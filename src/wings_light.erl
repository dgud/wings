%%
%%  wings_light.erl --
%%
%%     Implementation of lights.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_light).
-export([init/0, init/1, init_opengl/0,
         light_types/0,menu/3,command/2,is_any_light_selected/1,
	 any_enabled_lights/0,info/1,setup_light/2,
	 create/2,update_dynamic/2,update_matrix/2,update/1,
	 global_lights/1,
	 export/1,export_bc/1,export_camera_lights/0,
	 import/2,import/1,shape_materials/2,
	 light_pos/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-import(lists, [reverse/1,foldl/3,foldr/3,member/2,keydelete/3,sort/1]).

-define(DEF_X, 0.0).
-define(DEF_Y, 3.0).
-define(DEF_Z, 0.0).
-define(DEF_POS, {?DEF_X,?DEF_Y,?DEF_Z}).

%% Light record in We.
%%
%% The type field must be the first field, since it is used by
%% the light testing macros in wings.hrl.
-record(light,
	{type,					%Type. (DO NOT MOVE.)
	 diffuse={1.0,1.0,1.0,1.0},
	 ambient={0.0,0.0,0.0,1.0},
	 specular={1.0,1.0,1.0,1.0},
	 aim,					%Aim point for spot/infinite.
	 lin_att,				%Linear attenuation.
	 quad_att,				%Quadratic attenuation.
	 spot_angle,
	 spot_exp,				%Spot exponent.
	 prop=[]				%Extra properties.
	}).

init() ->
    init(false).

init(Recompile) ->
    Path = filename:join(wings_util:lib_dir(wings), "textures"),
    LTCmatFile = "areal_ltcmat.bin",
    {ok, LTCmat} = file:read_file(filename:join(Path, LTCmatFile)),
    AreaMatTxId = load_area_light_tab(LTCmat),
    DefEnvMap = "grandcanyon.png",
    EnvImgRec = wings_image:image_read([{filename, filename:join(Path, DefEnvMap)}]),
    EnvIds = case cl_setup(Recompile) of
                 {error, _} -> fake_envmap(Path, EnvImgRec);
                 CL -> make_envmap(CL, EnvImgRec)
             end,
    [?SET(Tag, Id) || {Tag,Id} <- [AreaMatTxId|EnvIds]],
    init_opengl(),
    wings_develop:gl_error_check({?MODULE,?FUNCTION_NAME}),
    ok.

init_opengl() ->
    %% Bind textures to units
    Ids = [{areamatrix_tex, ?AREA_LTC_MAT_UNIT},
           {brdf_tex, ?ENV_BRDF_MAP_UNIT},
           {env_diffuse_tex, ?ENV_DIFF_MAP_UNIT},
           {env_spec_tex, ?ENV_SPEC_MAP_UNIT}],
    SetupUnit = fun({Tag, Unit}) ->
                        case ?GET(Tag) of
                            undefined -> ignore;
                            ImId ->
                                TxId = wings_image:txid(ImId),
                                gl:activeTexture(?GL_TEXTURE0 + Unit),
                                is_integer(TxId) andalso gl:bindTexture(?GL_TEXTURE_2D, TxId),
                                gl:activeTexture(?GL_TEXTURE0)
                        end
                end,
    _ = [SetupUnit(Id) || Id <- Ids],
    ok.

command({move_light,Type}, St) ->
    wings_move:setup(Type, St);
command(color, St) ->
    color(St);
command({position_highlight,Data}, St) ->
    position_highlight(Data, St);
command({attenuation,Type}, St) ->
    attenuation(Type, St);
command(spot_angle, St) ->
    spot_angle(St);
command(spot_falloff, St) ->
    spot_falloff(St);
command(edit, St) ->
    edit(St);
command({edit,Id}, St) ->
    edit(Id, St);
command(delete, St) ->
    {save_state,delete(St)};
command({duplicate,Dir}, St) ->
    duplicate(Dir, St).

-spec is_any_light_selected(#st{}) -> boolean().

is_any_light_selected(St) ->
    MF = fun(_, We) -> ?IS_LIGHT(We) end,
    RF = fun erlang:'or'/2,
    wings_sel:dfold(MF, RF, false, St).

any_enabled_lights() ->
    wings_dl:fold(fun(#dlo{src_we=We}, Bool) ->
			  Bool orelse ?IS_ANY_LIGHT(We)
		  end, false).

-spec info(#we{}) -> iolist().

info(#we{name=Name,light=#light{type=Type}=L}=We) ->
    Info0 = io_lib:format(?__(1,"Light ~ts"), [Name]),
    case Type of
	ambient -> Info0;
	_ ->
	    Pos = light_pos(We),
	    Info = [Info0|io_lib:format(?__(2,": Pos ~s"),
					[wings_util:nice_vector(Pos)])],
	    [Info|info_1(Type, Pos, L)]
    end.

info_1(point, _, _) -> [];
info_1(Type, Pos, #light{aim=Aim,spot_angle=A}) ->
    Dir = e3d_vec:norm_sub(Aim, Pos),
    Info = io_lib:format(?__(1,". Aim ~s. Dir ~s"),
			 [wings_util:nice_vector(Aim),
			  wings_util:nice_vector(Dir)]),
    [Info|case Type of
	      spot -> io_lib:format(?__(2,". Angle ~s~c"),
				    [wings_util:nice_float(A),?DEGREE]);
	      _ -> []
	  end].

%%%
%%% Light Commands.
%%%

color(St0) ->
    {St,Flags} =
        wings_sel:mapfold(
          fun(_, #we{light=L}=We, []) when ?IS_LIGHT(We) ->
                  {R,G,B,A} = get_light_color(L),
                  {H,S,V} = wings_color:rgb_to_hsv(R, G, B),
                  ColorFun = fun({finish,C}, D) -> color(C, D, A);
                                (C, D) -> color(C, D, A)
                             end,
                  Flags = [{initial,[H,V,S]}],
                  {We#we{temp=ColorFun},Flags};
             (_, We, _) when ?IS_LIGHT(We) ->
                  wings_u:error_msg(?__(1,"Select only one light."));
             (_, _, A) -> A
           end, [], St0),
    Units = [{angle,{0.0,359.9999}},
             {percent,{0.0,1.0}},
             {percent,{0.0,1.0}}],
    DF = fun(#we{temp=General}) -> General end,
    wings_drag:general(DF, Units, Flags, St).

color([H,V,S], #dlo{src_we=#we{light=L0}=We0}=D, A) ->
    {R,G,B} = wings_color:hsv_to_rgb(H, S, V),
    Col = {R,G,B,A},
    L = update_color(L0, Col),
    We = We0#we{light=L},
    update(D#dlo{work=none,src_we=We}).

get_light_color(#light{type=ambient,ambient=Col}) -> Col;
get_light_color(#light{diffuse=Diff}) -> Diff.

update_color(#light{type=ambient}=L, Col) -> L#light{ambient=Col};
update_color(L, Col) -> L#light{diffuse=Col}.

position_highlight({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun position_highlight/2);
position_highlight(Center, St) ->
    {save_state,
     wings_sel:map(fun(_, We) when ?IS_LIGHT(We) ->
			   position_highlight_1(Center, We);
		      (_, We) -> We
		   end, St)}.

position_highlight_1(Center, #we{light=L0}=We) ->
    case L0 of
	#light{type=point} ->
	    move_light(Center, We);
	_ ->
	    L = L0#light{aim=Center},
	    We#we{light=L}
    end.

spot_angle(St) ->
    case selected_light(St) of
	#light{type=spot,spot_angle=SpotAngle} ->
	    SpotFun0 = fun([Angle|_], L) -> L#light{spot_angle=Angle} end,
            DF = fun(_) -> adjust_fun(SpotFun0) end,
	    Units = [{angle,{0.1,89.9}}],
	    Flags = [{initial,[SpotAngle]}],
	    wings_drag:general(DF, Units, Flags, St);
	_ ->
	    wings_u:error_msg(?__(1,"Not a spotlight."))
    end.

spot_falloff(St) ->
    case selected_light(St) of
	#light{type=spot,spot_exp=SpotExp} ->
	    SpotFun0 = fun([Exp|_], L) -> L#light{spot_exp=Exp} end,
	    DF = fun(_) -> adjust_fun(SpotFun0) end,
	    Units = [{number,{0.0,128.0}}],
	    Flags = [{initial,[SpotExp]}],
	    wings_drag:general(DF, Units, Flags, St);
	_ ->
	    wings_u:error_msg(?__(1,"Not a spotlight."))
    end.

attenuation(Type, St) ->
    case selected_light(St) of
	#light{type=Ltype}=L when Ltype =:= point; Ltype =:= spot ->
	    Initial = att_initial(Type, L),
	    DF = fun(_) -> adjust_fun(att_fun(Type)) end,
	    Units = [{dx,att_range(Type)}],
	    Flags = [{initial,[Initial]}],
	    wings_drag:general(DF, Units, Flags, St);
	_ ->
	    wings_u:error_msg(?__(1,"Not a point light or spotlight."))
    end.

att_initial(linear, #light{lin_att=LinAtt}) -> LinAtt;
att_initial(quadratic, #light{quad_att=QuadAtt}) -> QuadAtt.

att_fun(linear) -> fun([V|_], L) -> L#light{lin_att=V} end;
att_fun(quadratic) -> fun([V|_], L) -> L#light{quad_att=V} end.

att_range(linear) -> {0.0,1.0};
att_range(quadratic) -> {0.0,0.5}.

selected_light(St) ->
    MF = fun(_, #we{light=L}=We) when ?IS_LIGHT(We) ->
                 [L];
            (_, #we{}) ->
                 []
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St) of
        [Selected] ->
            Selected;
        [_|_] ->
            wings_u:error_msg(?__(1,"Select only one light."))
    end.

adjust_fun(AdjFun) ->
    fun({finish,Ds}, D) -> adjust_fun_1(AdjFun, Ds, D);
       (Ds, D) -> adjust_fun_1(AdjFun, Ds, D)
    end.

adjust_fun_1(AdjFun, Ds, #dlo{src_we=#we{light=L0}=We0}=D) ->
    L = AdjFun(Ds, L0),
    We = We0#we{light=L},
    update(D#dlo{work=none,src_we=We}).

%%
%% The Edit Properties command.
%%
edit(St) ->
    case wings_sel:selected_ids(St) of
        [Id] ->
            edit(Id, St);
        [_|_] ->
            wings_u:error_msg(?__(1,"Select only one light."))
    end.

edit(Id, St) ->
    Obj = wings_obj:get(Id, St),
    Prop = get_light(Obj, false, St),
    case Obj of
	#{light:=#light{type=ambient}} ->
	    {dialog,Qs,Fun} = edit_ambient_dialog(Obj, Prop, St),
	    wings_dialog:dialog(?__(2,"Ambient Light Properties"), Qs, Fun);
	#{light:=#light{}} ->
	    {dialog,Qs,Fun} = edit_dialog(Obj, Prop, St),
	    wings_dialog:dialog(?__(3,"Light Properties"), Qs, Fun)
    end.

edit_ambient_dialog(Obj, Prop0, St) ->
    #{name:=Name,light:=L0} = Obj,
    #light{ambient=Amb0} = L0,
    Qs0 = {vframe,
	   [{hframe,
	     [{label_column,
	       [{?__(1,"Ambient"),{color,Amb0}}]}],
	     [{title,?__(2,"Color")}]}|qs_specific(L0)]},
    Qs1 = wings_plugin:dialog({light_editor_setup,Name,Prop0}, [{"Wings 3D", Qs0}]),
    Qs = {vframe_dialog,
	  [{oframe, Qs1, 1, [{style, buttons}]}],
	  [{buttons, [ok, cancel]}, {key, result}]},
    Fun = fun([Amb|Res]) ->
		  {ok,Prop} = plugin_results(Name, Prop0, Res),
		  L = L0#light{ambient=Amb,prop=Prop},
                  wings_obj:put(Obj#{light:=L}, St)
	  end,
    {dialog,Qs,Fun}.

edit_dialog(Obj, Prop0, St) ->
    #{name:=Name,light:=L0} = Obj,
    #light{diffuse=Diff0,specular=Spec0} = L0,
    Qs0 = {vframe,
	   [{hframe,
	     [{label_column,
	       [{?__(1,"Diffuse"),{color,Diff0}},
		{?__(3,"Specular"),{color,Spec0}}]}],
	     [{title,?__(4,"Colors")}]}|qs_specific(L0)]},
    Qs1 = wings_plugin:dialog({light_editor_setup,Name,Prop0}, [{"Wings 3D", Qs0}]),
    Qs = {vframe_dialog,
 	  [{oframe, Qs1, 1, [{style, buttons}]}],
	  [{buttons, [ok, cancel]}, {key, result}]},
    Fun = fun([Diff,Spec|More0]) ->
		  L1 = L0#light{diffuse=Diff,specular=Spec},
		  {L2,More} = edit_specific(More0, L1),
		  case plugin_results(Name, Prop0, More) of
		      {ok,Prop} ->
                          L = L2#light{prop=Prop},
                          wings_obj:put(Obj#{light:=L}, St)
		  end
	  end,
    {dialog,Qs,Fun}.

plugin_results(Name, Prop0, Res0) ->
    case wings_plugin:dialog_result({light_editor_result,Name,Prop0}, Res0) of
	{Prop,[{result, ok}]} ->
	    {ok,keydelete(opengl, 1, Prop)};
	{_,Res} ->
	  io:format(?__(1,
			"Light editor plugin(s) left garbage:~n    ~P~n"), 
		    [Res,20]),
	    wings_u:error_msg(?__(2,"Plugin(s) left garbage"))
    end.

qs_specific(#light{type=spot,spot_angle=Angle,spot_exp=SpotExp}=L) ->
    Spot = [{vframe,
		[{label_column,
		    [{?__(1, "Angle"), {slider, {text, Angle, [{range, {0.0, 89.9}}]}}},
		     {?__(2, "Falloff"), {slider, {text, SpotExp, [{range, {0.0, 128.0}}]}}}]
		}],
	     [{title,?__(3,"Spot Parameters")}]}],
    qs_att(L, Spot);
qs_specific(#light{type=point}=L) -> qs_att(L, []);
qs_specific(#light{type=area}=L) -> qs_att(L, []);
qs_specific(_) -> [].

qs_att(#light{lin_att=Lin,quad_att=Quad}, Tail) ->
    [{vframe,
	[{label_column,
	    [{?__(1,"Linear"),{slider,{text,Lin,[{range,{0.0,1.0}}]}}},
	     {?__(2,"Quadratic"),{slider,{text,Quad,[{range,{0.0,0.5}}]}}}]
	}],
      [{title,?__(3,"Attenuation")}]}|Tail].
    
edit_specific([LinAtt,QuadAtt,Angle,SpotExp|More], #light{type=spot}=L) ->
    {L#light{spot_angle=Angle,spot_exp=SpotExp,lin_att=LinAtt,quad_att=QuadAtt},More};
edit_specific([LinAtt,QuadAtt|More], #light{type=point}=L) ->
    {L#light{lin_att=LinAtt,quad_att=QuadAtt},More};
edit_specific([LinAtt,QuadAtt|More], #light{type=area}=L) ->
    {L#light{lin_att=LinAtt,quad_att=QuadAtt},More};
edit_specific(More, L) -> {L,More}.

%%%
%%% The Delete command.
%%%

delete(St) ->
    wings_sel:map_update_sel(
      fun(_, _) ->
              {#we{},gb_sets:empty()}
      end, St).

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, St0) ->
    CF = fun(Items, We) ->
                 Empty = gb_sets:empty(),
                 New = [{We,Items,copy}],
                 {We,Empty,New}
         end,
    St = wings_sel:clone(CF, St0),
    case Dir of
	none -> St;
	_ -> wings_move:setup(Dir, St)
    end.

%%%
%%% Creating lights.
%%%

create(Type, #st{onext=Oid}=St) ->
    Prefix = atom_to_list(Type),
    Name = Prefix++integer_to_list(Oid),
    import([{Name,[{opengl,[{type,Type}]}]}], St).

%%%
%%% Updating, drawing and rendering lights.
%%%
update_dynamic(#dlo{src_we=We0}=D, Vtab0) ->
    Vtab = array:from_orddict(sort(Vtab0)),
    We = We0#we{vp=Vtab},
    List = update_1(We, D),
    D#dlo{work=List,src_we=We}.

update_matrix(#dlo{src_we=We0}=D, Matrix) ->
    We = wings_we:transform_vs(Matrix, We0),
    List = update_1(We, D),
    D#dlo{work=List,sel=none,transparent=We}.

update(#dlo{work=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(#dlo{sel=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(D) -> D.

update_1(#we{light=#light{type=Type}}=We, #dlo{src_sel=SrcSel}) ->
    SelColor = case SrcSel of
		   none -> {0.0,0.0,1.0,1.0};
		   _ -> {R,G,B} = wings_pref:get_value(selected_color),
                        {R,G,B,1.0}
	       end,
    update_fun(Type, SelColor, We).

update_fun(infinite, SelColor, #we{light=#light{aim=Aim}}=We) ->
    LightPos = light_pos(We),
    LightCol = get_light_col(We),
    Vec = e3d_vec:norm_sub(Aim, LightPos),
    Data = [e3d_vec:mul(Vec, 0.2),e3d_vec:mul(Vec, 0.6)],
    {Len, Tris,_,_,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
		gl:lineWidth(1.5),
		gl:pushMatrix(),
		{X,Y,Z} = LightPos,
		gl:translatef(X, Y, Z),
		wings_shaders:set_uloc(light_color, LightCol, RS),
                gl:drawArrays(?GL_TRIANGLES, 2, Len*3),
                wings_shaders:set_uloc(light_color, SelColor, RS),
                gl:drawArrays(?GL_LINES, 0, 2),
		gl:popMatrix(),
                RS
	end,
    wings_vbo:new(D, Data++Tris);
update_fun(point, SelColor, We) ->
    LightPos = light_pos(We),
    LightCol = get_light_col(We),
    Data0 = [{1.0,0.0,0.0},
	     {0.0,1.0,0.0},
	     {0.0,0.0,1.0},
	     {0.71,0.71,0.0},
	     {0.71,0.0,0.71},
	     {0.0,0.71,0.71}],
    N = length(Data0) * 4,
    Data = lines(Data0),
    {Len, Tris,_,_,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
		gl:lineWidth(1.0),
		wings_shaders:set_uloc(light_color, LightCol, RS),
		gl:pushMatrix(),
		{X,Y,Z} = LightPos,
		gl:translatef(X, Y, Z),
                gl:drawArrays(?GL_TRIANGLES, N, Len*3),
		wings_shaders:set_uloc(light_color, SelColor, RS),
		gl:drawArrays(?GL_LINES, 0, N),
		gl:popMatrix(),
                RS
	end,
    wings_vbo:new(D, Data++Tris);
update_fun(spot, SelColor, #we{light=#light{aim=Aim,spot_angle=Angle}}=We) ->
    Top = light_pos(We),
    LightCol = get_light_col(We),
    SpotDir0 = e3d_vec:norm_sub(Aim, Top),
    SpotDir = case e3d_vec:is_zero(SpotDir0) of
		  false -> SpotDir0;
		  true -> {0.0,1.0,0.0}
	      end,
    Rad = Angle*math:pi()/180,
    R = math:sin(Rad),
    H = math:cos(Rad),
    Translate = e3d_vec:mul(SpotDir, H),
    Rot = e3d_mat:rotate_s_to_t({0.0,0.0,1.0}, e3d_vec:neg(SpotDir)),
    {Len, Tris,_,_,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
                gl:lineWidth(1.0),
		wings_shaders:set_uloc(light_color, LightCol, RS),
                gl:pushMatrix(),
                {Tx,Ty,Tz} = Top,
                gl:translatef(Tx, Ty, Tz),
                gl:drawArrays(?GL_TRIANGLES, 0, Len*3),
		wings_shaders:set_uloc(light_color, SelColor, RS),
                {Dx,Dy,Dz} = Translate,
                gl:translatef(Dx, Dy, Dz),
                gl:multMatrixd(Rot),
                Obj = glu:newQuadric(),
                glu:quadricDrawStyle(Obj, ?GLU_LINE),
                glu:cylinder(Obj, R, 0.08, H, 12, 1),
                glu:deleteQuadric(Obj),
                gl:popMatrix(),
                RS
        end,
    wings_vbo:new(D, Tris);
update_fun(ambient, _, _) ->
    fun(RS) -> RS end.

lines([Vec|Vecs]) ->
    [e3d_vec:mul(Vec, 0.2),
     e3d_vec:mul(Vec, 0.6),
     e3d_vec:mul(Vec, -0.2),
     e3d_vec:mul(Vec, -0.6)|lines(Vecs)];
lines([]) -> [].

get_light_col(#we{light=#light{diffuse=Diff}}) ->
    Diff.

%%%
%%% Exporting lights.
%%%

%% For exporters.
export(St) ->
    export(St, false).

%% For saving in .wings files.
export_bc(St) ->
    export(St, true).

export(St, BackwardsCompatible) ->
    F = fun(#{light:=_}=Obj, A) ->
                [get_light(Obj, BackwardsCompatible, St)|A];
           (_, A) ->
                A
        end,
    L = wings_obj:fold(F, [], St),
    reverse(L).

export_camera_lights() ->
    Amb = {?__(1,"Ambient"), camera_ambient()},
    Ls = case wings_pref:get_value(number_of_lights) of
	     1 ->
		 [{?__(2,"Infinite"),camera_infinite_1_0()}];
	     2 ->
		 [{?__(3,"Infinite1"),camera_infinite_2_0()},
		  {?__(4,"Infinite2"),camera_infinite_2_1()}]
	 end,
    #view{origin=Aim} = wings_view:current(),
    CameraPos = wings_view:eye_point(),
    GL = fun({Name,Li = #light{aim=Diff}}) ->
		 LPos = e3d_vec:add(CameraPos,Diff),
		 We = #we{name = Name,
			  vp = array:from_orddict([{1, LPos}]),
			  light = Li#light{aim=Aim}},
		 get_light(We, false)
	 end,
    [GL(Light) || Light <- [Amb|Ls]].

get_light(#{id:=Id,name:=Name,perm:=P,light:=Light}, BC, St) ->
    F = fun(We) -> get_light_1(Light, We, BC) end,
    Ps0 = wings_obj:with_we(F, Id, St),
    Ps = export_perm(P, Ps0),
    case BC of
        true -> {Name,Ps};
        false -> Ps
    end.

get_light(#we{name=Name,perm=P,light=Light}=We, BC) ->
    Ps0 = get_light_1(Light, We, BC),
    Ps = export_perm(P, Ps0),
    {Name,Ps}.

get_light_1(#light{type=ambient,ambient=Amb,prop=Prop}, #we{pst=Pst}=We, _) ->
    P = light_pos(We),
    OpenGL = [{type,ambient},{ambient,Amb},{position,P},{pst,Pst}],
    [{opengl,OpenGL}|Prop];
get_light_1(L, #we{pst=Pst}=We, BC) ->
    #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
	   aim=Aim,spot_angle=Angle,spot_exp=SpotExp,
	   lin_att=LinAtt,quad_att=QuadAtt,prop=Prop} = L,
    P = light_pos(We),
    Common = [{type,Type},{position,P},{aim_point,Aim},
	      {diffuse,Diff},{ambient,Amb},{specular,Spec},{pst,Pst}],
    OpenGL0 = case Type of
		  spot ->
		      [{cone_angle,Angle},{spot_exponent,SpotExp}|Common];
		  _ ->
		      Common
	     end,
    OpenGL1 = if
		 Type =:= point; Type =:= spot; Type =:= area ->
		     [{linear_attenuation,LinAtt},
		      {quadratic_attenuation,QuadAtt}|OpenGL0];
		 true -> OpenGL0
	     end,
    OpenGL = case Type of
		 area -> [{mesh,export_mesh(We, BC)}|OpenGL1];
		 _ -> OpenGL1
	     end,
    [{opengl,OpenGL}|Prop].

export_perm({_,_}, Ps) ->
    [{visible,false},{locked,false}|Ps];
export_perm(P, Ps) when is_integer(P) ->
    [{visible,P < 2},{locked,(P band 1) =/= 0}|Ps].

%% This is the classic definition of e3d_face{}, as it was defined
%% before 1.1.9.
-record(classic_e3d_face,
	{vs=[],				        %List of vertex indices.
	 vc=[],					%Vertex color indices.
	 tx=[],				        %List of texture indices.
	 ns=[],				        %List of normal indices.
	 mat=[],				%Materials for face.
	 vis=-1}).				%Visible edges (as in 3DS).

export_mesh(We, BC) ->
    #e3d_mesh{fs=Fs0} = Mesh = wings_export:make_mesh(We, []),
    Fs = case BC of
	     false ->
		 Fs0;
	     true ->
		 [export_fix_face(F) || F <- Fs0]
	 end,
    Mesh#e3d_mesh{fs=Fs}.

export_fix_face(#e3d_face{vs=Vs,mat=Mat}) ->
    %% Fix the face record so that it looks like the classic
    %% definition of #e3d_face{} (before 1.1.9).
    FaceRec = #classic_e3d_face{vs=Vs,mat=Mat},

    %% Patch the record type.
    setelement(1, FaceRec, e3d_face).

%%%
%%% Importing lights.
%%%

import(Lights, St) ->
    foldl(fun import_fun/2, St, Lights).

import_fun({Name,Ps}, St) ->
    wings_obj:new(Name, import(Ps), St).

import(Ps) ->
    Visible = proplists:get_value(visible, Ps, []),
    Locked = proplists:get_value(locked, Ps, []),
    Prop1 = proplists:delete(visible, Ps),
    Prop0 = proplists:delete(locked, Prop1),

    OpenGL = proplists:get_value(opengl, Prop0, []),
    Type = proplists:get_value(type, OpenGL, point),
    Pos = proplists:get_value(position, OpenGL, ?DEF_POS),
    Diff = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Amb = import_ambient(Type, OpenGL),
    Spec = proplists:get_value(specular, OpenGL, {1.0,1.0,1.0,1.0}),
    Aim = proplists:get_value(aim_point, OpenGL, {0.0,0.0,0.0}),
    LinAtt = proplists:get_value(linear_attenuation, OpenGL, 0.0),
    QuadAtt = proplists:get_value(quadratic_attenuation, OpenGL, 0.0),
    Angle = proplists:get_value(cone_angle, OpenGL, 30.0),
    SpotExp = proplists:get_value(spot_exponent, OpenGL, 0.0),
    
    Prop = proplists:delete(opengl, Prop0),
    Light = #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
		   aim=Aim,lin_att=LinAtt,quad_att=QuadAtt,
		   spot_angle=Angle,spot_exp=SpotExp,prop=Prop},
    We=import_we(Light, OpenGL, Pos),
    We#we{perm=import_perm(Visible, Locked)}.

import_ambient(ambient, OpenGL) ->
    proplists:get_value(ambient, OpenGL, {0.1,0.1,0.1,1.0});
import_ambient(_, OpenGL) ->
    proplists:get_value(ambient, OpenGL, {0.0,0.0,0.0,1.0}).

import_we(#light{type=area}=Light, OpenGL, {X,Y,Z}) ->
    Mesh = 
	case proplists:lookup(mesh, OpenGL) of
	    none ->
		#e3d_mesh{type=polygon,
			  fs=[#e3d_face{vs=[0,1,2,3],
					mat=[default]}],
			  vs=[{X+1.0,Y,Z+1.0},{X-1.0,Y,Z+1.0},
			      {X-1.0,Y,Z-1.0},{X+1.0,Y,Z-1.0}]};
	    {mesh,M} -> import_fix_mesh(M)
	end,
    We = wings_import:import_mesh(material, Mesh),
    Pst = proplists:get_value(pst, OpenGL, gb_trees:empty()),
    We#we{light=Light,pst=Pst};
import_we(#light{}=Light, OpenGL, {X,Y,Z}) ->
    %% We used to put all vertices at the same position, but with
    %% then rewritten pick handling we need a vertex array for picking.
    %% The cube will be slightly larger than the sphere that is shown
    %% for the light. The position of the light will be the centroid
    %% of the cube.
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    S = 0.07,
    Vs = [{X-S,Y-S,Z+S},{X-S,Y+S,Z+S},{X+S,Y+S,Z+S},{X+S,Y-S,Z+S},
	  {X-S,Y-S,Z-S},{X-S,Y+S,Z-S},{X+S,Y+S,Z-S},{X+S,Y-S,Z-S}],
    We = wings_we:build(Fs, Vs),
    Pst = proplists:get_value(pst, OpenGL, gb_trees:empty()),
    We#we{light=Light,pst=Pst}.
    
import_perm([]=_Visible,[]=_Locked) ->  % it will match when a new light is added to project  
    import_perm(true,false);
import_perm(false,false) ->
    ?PERM_HIDDEN_BIT;
import_perm(true,false) ->
    0;
import_perm(true,true) ->
    ?PERM_LOCKED_BIT;
import_perm(false,true) ->
    ?PERM_HIDDEN_BIT bor ?PERM_LOCKED_BIT.

import_fix_mesh(#e3d_mesh{fs=Fs0}=Mesh0) ->
    Fs = [import_fix_face(F) || F <- Fs0],
    Mesh1 = Mesh0#e3d_mesh{fs=Fs},
    Mesh = e3d_mesh:clean_faces(Mesh1),
    e3d_mesh:transform(Mesh).

import_fix_face(FaceRec) when is_tuple(FaceRec) ->
    %% Different versions of Wings can have #e3d_face{}
    %% records of different size (for example, in Wings 1.1.9
    %% a new 'sg' field was added to #e3d_face{}). We know
    %% that the fields we are interested in are in the same
    %% place, so we can retrieve them using element/2.
    %%
    e3d_face = element(1, FaceRec),		%Crash on unknown record type.
    Vs = element(#e3d_face.vs, FaceRec),
    Mat = element(#e3d_face.mat, FaceRec),
    #e3d_face{vs=Vs,mat=Mat}.

%%%
%%% Setting up lights.
%%%

global_lights(Lights0) ->
    Lights = lists:map(fun scene_lights_fun/1, Lights0),
    IsAL = fun(#{light:=#light{type=Type}}) -> Type =:= ambient end,
    lists:partition(IsAL, Lights).

camera_ambient() ->
    #light{type = ambient, 
	   aim = {0.0,0.0,0.0},
	   ambient = {0.1,0.1,0.1,1.0}}.
camera_infinite_1_0() ->
    #light{type = infinite, 
	   diffuse  = {0.7,0.7,0.7,1},
	   specular = {0.2,0.2,0.2,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {0.110,0.0,0.994}
	  }.
camera_infinite_2_0() ->
    #light{type = infinite, 
	   diffuse  = {1,1,1,1},
	   specular = {0.3,0.3,0.3,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {0.71,0.71,0.0}
	  }.
camera_infinite_2_1() ->
    #light{type = infinite, 
	   diffuse  = {0.5,0.5,0.5,0.5},
	   specular = {0.3,0.3,0.3,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {-0.71,-0.71,0.0}
	  }.

scene_lights_fun(#dlo{transparent=#we{light=L}=We}) ->
    %% This happens when dragging a light in Body selection mode.
    %% (Not area light.)
    prepare_light(L, We, none);
scene_lights_fun(#dlo{drag=Drag,src_we=We0}=D) ->
    %% Area lights handled here in all selection modes +
    %% other lights in vertex/edge/face modes.
    We = case We0 of
	     #we{light=#light{type=area}} ->
		 %% For an area light it looks better in vertex/edge/face
		 %% modes to emulate with the static non-splitted shape
		 %% during drag. It would be more correct if the area light
		 %% updating would use the resulting #we{}, but it does not
		 %% exist until the drag is done.
		 wings_draw:original_we(D);
	     _ ->
		 %% Non-area lights drag the whole shape so they can use
		 %% the dynamic part of the splitted shape
		 %% (which is the whole shape).
		 We0
	 end,
    M = case Drag of
	    {matrix,_Tr,_M0,M1} -> M1;
 	    _ -> none
 	end,
    prepare_light(We#we.light, We, M).

prepare_light(#light{type=ambient}=L, _We, _M) ->
    #{light=>L};
prepare_light(#light{type=infinite,aim=Aim}=L, We, _M) ->
    {X,Y,Z} = e3d_vec:norm_sub(light_pos(We), Aim),
    #{light=>L, pos=>{X,Y,Z,0.0}};
prepare_light(#light{type=point}=L, We, _M) ->
    {X,Y,Z} = light_pos(We),
    #{light=>L, pos=>{X,Y,Z,1.0}};
prepare_light(#light{type=spot,aim=Aim}=L, We, _M) ->
    Pos = {X,Y,Z} = light_pos(We),
    Dir = e3d_vec:norm_sub(Aim, Pos),
    #{light=>L, pos=>{X,Y,Z,1.0}, dir=>Dir};
prepare_light(#light{type=area}=L, We, M) ->
    case arealight_props(We) of
        {area, Corners} -> #{light=>L, points=>[mul_point(M,P)||P<-Corners]};
        {point, C} ->
            {X,Y,Z} = mul_point(M, C),
            #{light=>L#light{type=point}, pos=>{X,Y,Z,1.0}};
        {spot, Dir0, C} ->
            {X,Y,Z} = mul_point(M, C),
            Dir = mul_point(M, Dir0),
            #{light=>L#light{type=spot}, pos=>{X,Y,Z,1.0}, dir=>Dir}
    end.

setup_light(#{light:=#light{type=ambient,ambient=Amb}}, RS0) ->
    RS = wings_shaders:use_prog(ambient_light, RS0),
    wings_shaders:set_uloc(light_diffuse, Amb, RS);

setup_light(#{light:=#light{type=infinite, diffuse=Diff, specular=Spec},
              pos:=Pos}, RS0) ->
    RS1 = wings_shaders:use_prog(infinite_light, RS0),
    RS2 = wings_shaders:set_uloc(ws_lightpos, Pos, RS1),
    RS3 = wings_shaders:set_uloc(light_diffuse, Diff, RS2),
    wings_shaders:set_uloc(light_specular, Spec, RS3);

setup_light(#{light:=#light{type=point, diffuse=Diff,specular=Spec,
                            lin_att=Lin,quad_att=Quad},
              pos:=Pos}, RS0) ->
    RS1 = wings_shaders:use_prog(point_light, RS0),
    RS2 = wings_shaders:set_uloc(ws_lightpos, Pos, RS1),
    RS3 = wings_shaders:set_uloc(light_diffuse, Diff, RS2),
    RS4 = wings_shaders:set_uloc(light_specular, Spec, RS3),
    wings_shaders:set_uloc(light_att, {0.8, Lin, Quad}, RS4);

setup_light(#{light:=#light{type=spot, diffuse=Diff,specular=Spec,
                            lin_att=Lin,quad_att=Quad,
                            spot_angle=Angle,spot_exp=Exp},
              pos:=Pos, dir:=Dir}, RS0) ->
    RS1 = wings_shaders:use_prog(spot_light, RS0),
    RS2 = wings_shaders:set_uloc(ws_lightpos, Pos, RS1),
    RS3 = wings_shaders:set_uloc(light_diffuse, Diff, RS2),
    RS4 = wings_shaders:set_uloc(light_att, {0.8, Lin, Quad}, RS3),
    RS5 = wings_shaders:set_uloc(light_dir, Dir, RS4),
    RS6 = wings_shaders:set_uloc(light_angle, math:cos(Angle*math:pi()/180.0), RS5),
    RS7 = wings_shaders:set_uloc(light_exp, Exp, RS6),
    wings_shaders:set_uloc(light_specular, Spec, RS7);

setup_light(#{light:=#light{type=area, diffuse=Diff, specular=Spec,
                            lin_att=Lin,quad_att=Quad},
              points:=Points}, RS0) ->
    RS1 = wings_shaders:use_prog(area_light, RS0),
    RS2 = wings_shaders:set_uloc(light_diffuse, Diff, RS1),
    RS3 = wings_shaders:set_uloc(light_specular, Spec, RS2),
    RS4 = wings_shaders:set_uloc(light_att, {0.8, Lin, Quad}, RS3),
    wings_shaders:set_uloc(light_points, Points, RS4).

light_pos(We) ->
    wings_vertex:center(We).

arealight_props(#we{light=#light{type=area}}=We) ->
    case wings_we:visible(We) of
        [Face] ->
            Vs0 = wings_face:vertex_positions(Face, We),
            case lists:reverse(Vs0) of
                [_,_,_,_] = Vs -> {area, Vs};
                [A,B,C]   -> {area, [A,B,C,C]};
                [A,B,C,D,_] -> {area, [A,B,C,D]}; %% Could do better here
                _ ->
                    N = wings_face:normal(Face, We),
                    C = wings_face:center(Face, We),
                    {spot, N, C}
            end;
        Fs ->
            C = wings_vertex:center(We),
            Ns = [wings_face:normal(F, We) || F <- Fs],
            N = e3d_vec:average(Ns),
            case e3d_vec:len(N) > 0.5 of
                true -> {spot, e3d_vec:norm(N), C};
                false -> {point, C}
            end
    end.

move_light(Pos, #we{vp=Vtab0}=We) ->
    Vtab = array:sparse_map(fun(_, _) -> Pos end, Vtab0),
    We#we{vp=Vtab}.

shape_materials(#we{id=Id, light=#light{diffuse=Front}}, #st{mat=Mtab}=St) ->
    St#st{mat=gb_trees:insert({'_area_light_',Id},[Front],Mtab)}.

mul_point(none, Pos) -> Pos;
mul_point({1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0, Tx,Ty,Tz}, {X,Y,Z}) ->
    {X+Tx,Y+Ty,Z+Tz};
mul_point(M, P) -> e3d_mat:mul_point(M, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_area_light_tab(LTCmat) ->
    64*64*4*4 = byte_size(LTCmat),
    Opts = [{wrap, {clamp,clamp}}, {filter, {linear, linear}}],
    ImId = wings_image:new_hidden(area_mat,
                                  #e3d_image{type=r32g32b32a32f, bytes_pp=16,
                                             width=64,height=64,
                                             image=LTCmat,
                                             extra=Opts
                                            }),
    ?CHECK_ERROR(),
    {areamatrix_tex, ImId}.

fake_envmap(Path, EnvImgRec) ->
    ErrorStr = ?__(1, "Could not initialize OpenCL: env lighting limited ~n"),
    io:format(ErrorStr,[]),
    wings_status:message(geom, ErrorStr),
    %% Poor mans version with blured images
    SpecBG = wings_image:e3d_to_wxImage(EnvImgRec),
    wxImage:rescale(SpecBG, 512, 256, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    tone_map(SpecBG),
    SBG0 = wings_image:wxImage_to_e3d(SpecBG),
    SpecBG1 = wxImage:copy(SpecBG),
    MMs = make_mipmaps(SpecBG1, 1, 256, 128),
    Opts = [{wrap, {repeat,clamp}}, {filter, {mipmap, linear}}, {mipmaps, MMs}],
    SBG = SBG0#e3d_image{name="Fake Spec", extra=Opts},
    SpecId = wings_image:new_hidden(env_spec_tex, SBG),

    wxImage:rescale(SpecBG, 64, 32, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    DiffBG = wxImage:blur(SpecBG, 10),
    blur_edges(DiffBG),
    DBG0 = wings_image:wxImage_to_e3d(DiffBG),
    DBG = DBG0#e3d_image{name="Fake diffuse", extra=[{wrap, {repeat,clamp}},
                                                     {filter, {linear,linear}}]},
    wxImage:destroy(SpecBG),
    wxImage:destroy(DiffBG),
    {ok, BrdfBin0} = file:read_file(filename:join(Path,"brdf_tab.bin")),
    128*128*2 = byte_size(BrdfBin0),
    BrdfBin = << << R,G,0 >> || << R,G >> <= BrdfBin0 >>,
    OptsB = [{wrap, {clamp,clamp}}, {filter, {linear, linear}}],
    Brdf = #e3d_image{width=128,height=128,image=BrdfBin,extra=OptsB},
    %% wings_image:debug_display(brdf, Brdf),
    %% wings_image:debug_display(spec, SBG),
    %% wings_image:debug_display(diff, DBG),
    [{env_spec_tex,    SpecId},
     {env_diffuse_tex, wings_image:new_hidden(env_diffuse_tex, DBG)},
     {brdf_tex,        wings_image:new_hidden(brdf_tex, Brdf)}].


tone_map(Image) ->
    RGB0 = wxImage:getData(Image),
    RGB = << << (pixel_tonemap(R/256,G/256,B/256)):24 >> || <<R:8,G:8,B:8>> <= RGB0 >>,
    wxImage:setData(Image, RGB).

pixel_tonemap(R0,G0,B0) ->
    Lum = (1.0+(R0 * 0.2126 + G0 * 0.72152 + B0 * 0.0722)),
    R = min(255, trunc(Lum * R0 * 255.0)),
    G = min(255, trunc(Lum * G0 * 255.0)),
    B = min(255, trunc(Lum * B0 * 255.0)),
    (R bsl 16) bor (G bsl 8) bor B.

blur_edges(Image) ->
    RGB0 = wxImage:getData(Image),
    RowSz = wxImage:getWidth(Image)*3,
    BlobSz = (wxImage:getHeight(Image)-2)*RowSz,
    <<First0:RowSz/binary, Center:BlobSz/binary, Last0:RowSz/binary>> = RGB0,
    First = blur_row(First0),
    Last = blur_row(Last0),
    RGB1 = <<First:RowSz/binary, Center:BlobSz/binary, Last:RowSz/binary>>,
    RGB = << << (blur_edge(Row, RowSz))/binary >> || <<Row:RowSz/binary>> <= RGB1 >>,
    wxImage:setData(Image, RGB).

-define(A(C0,C1,C2), (round((C0+C1+C2)/3))).

blur_row(Bin) ->
    List = binary_to_list(Bin),
    {R0,G0,B0} = average(List, 0,0,0,0),
    blur_pixel(List, R0,G0,B0, <<>>).

average([R,G,B|Rest], R0,G0,B0,N) ->
    average(Rest, R0+R,G0+G,B0+B,N+1);
average([], R0,G0,B0,N) ->
    {R0 div N, G0 div N, B0 div N}.

blur_pixel([R,G,B|Rest], R0,G0,B0, Bin) ->
    Acc = <<Bin/binary, ((R+R0) div 2):8, ((G+G0) div 2):8, ((B+B0) div 2):8>>,
    blur_pixel(Rest, R0,G0,B0, Acc);
blur_pixel([], _R0,_G0,_B0, Bin) ->
    Bin.

blur_edge(Row0, Bytes) ->
    Skip = Bytes-18,
    <<R0:8,G0:8,B0:8, R1:8,G1:8,B1:8, R2:8,G2:8,B2:8,
      Bin:Skip/bytes,
      R7:8,G7:8,B7:8, R8:8,G8:8,B8:8, R9:8,G9:8,B9:8>> = Row0,
    R00 = ?A(R0,R1,R9),  G00=?A(G0,G1,G9),  B00=?A(B0,B1,B9),
    R90 = ?A(R0,R8,R9),  G90=?A(G0,G8,G9),  B90=?A(B0,B8,B9),
    R10 = ?A(R00,R1,R2), G10=?A(G00,G1,G2), B10=?A(B00,B1,B2),
    R80 = ?A(R90,R8,R7), G80=?A(G90,G8,G7), B80=?A(B90,B8,B7),
    R01 = ?A(R00,R10,R90),  G01=?A(G00,G10,G90),  B01=?A(B00,B10,B90),
    R91 = ?A(R00,R80,R90),  G91=?A(G00,G80,G90),  B91=?A(B00,B80,B90),
    <<R01:8,G01:8,B01:8,
      R10:8,G10:8,B10:8,
      R2:8,G2:8,B2:8,
      Bin:Skip/bytes,
      R7:8,G7:8,B7:8,
      R80:8,G80:8,B80:8,
      R91:8,G91:8,B91:8
    >>.


make_mipmaps(Img0, Level, W, H) when Level < 6 ->
    wxImage:rescale(Img0, W, H),
    Img = wxImage:blur(Img0, 4),
    wxImage:destroy(Img0),
    Bin = wxImage:getData(Img),
    %% wings_image:debug_display(1000-Level,
    %%                           #e3d_image{width=W, height=H, image=Bin, order=upper_left,
    %%                                      name="Fake Spec: " ++ integer_to_list(Level)}),
    [{Bin, W, H, Level} | make_mipmaps(Img, Level+1, W div 2, H div 2)];
make_mipmaps(Img, _, _, _) ->
    wxImage:destroy(Img),
    [].

make_envmap(CL, EnvImgRec0) ->
    wings_pb:start(?__(1, "Building envmaps")),
    EnvImgRec = e3d_image:convert(EnvImgRec0, r8g8b8a8, 1, lower_left),
    wings_pb:update(0.1),
    W = 512, H = 256,  %% Sizes for result images
    OrigImg = wings_cl:image(EnvImgRec, CL),
    Buff0   = wings_cl:buff(W*512*4*4, [read_write], CL),
    Buff1   = wings_cl:buff(W*512*4*4, [read_write], CL),
    BrdfId = make_brdf(Buff0, 512, 512, CL),
    wings_pb:update(0.5),
    DiffId = make_diffuse(OrigImg, Buff0, Buff1, W, H, CL),
    wings_pb:update(0.9),
    SpecId = make_spec(OrigImg, Buff0, Buff1, W, H, CL),
    wings_pb:done(),
    cl:release_mem_object(OrigImg),
    cl:release_mem_object(Buff0),
    cl:release_mem_object(Buff1),
    [DiffId,SpecId,BrdfId].

make_brdf(Buff, W, H, CL) ->
    Fill = wings_cl:fill(Buff, <<0:(32*2)>>, W*H*4*2, CL),
    CC   = wings_cl:cast(schlick_brdf, [Buff, W, H], [W,H], [Fill], CL),
    Read = wings_cl:read(Buff, W*H*4*2, [CC], CL),
    {ok, BrdfData} = cl:wait(Read),
    Img = << << (round(X*255)), (round(Y*255)), 0 >>
             || <<X:32/float-native, Y:32/float-native>> <= BrdfData >>,
    %% wings_image:debug_display(brdf,#e3d_image{width=W, height=H, image=Img, name="BRDF"}),
    Opts = [{wrap, {clamp,clamp}}, {filter, {linear, linear}}],
    ImId = wings_image:new_hidden(brdf_tex, #e3d_image{width=W,height=H,image=Img,extra=Opts}),
    {brdf_tex, ImId}.

make_diffuse(OrigImg, Buff0, Buff1, W, H, CL) ->
    Fill0 = wings_cl:fill(Buff0, <<0:(32*4)>>, W*H*4*4, CL),
    Fill1 = wings_cl:fill(Buff1, <<0:(32*4)>>, W*H*4*4, CL),
    {B0,B1,Pre} = cl_multipass(make_diffuse, [OrigImg, W, H], Buff0, Buff1, 0, 10,
                               [W,H], [Fill0, Fill1], CL),
    CC   = wings_cl:cast(color_convert, [B0,B1,W,H], [W,H], Pre, CL),
    Read = wings_cl:read(B1, W*H*4*4, [CC], CL),
    {ok, DiffData} = cl:wait(Read),
    Img = << << (round(R*255)), (round(G*255)), (round(B*255)) >> ||
              <<R:32/float-native, G:32/float-native, B:32/float-native, _:32>> <= DiffData >>,
    %% wings_image:debug_display(1000+W,#e3d_image{width=W, height=H, image=Img, name="Diffuse"}),
    Opts = [{wrap, {repeat,clamp}}, {filter, {linear, linear}}],
    ImId = wings_image:new_hidden(env_diffuse_tex, #e3d_image{width=W,height=H,image=Img,extra=Opts}),
    {env_diffuse_tex, ImId}.

make_spec(OrigImg, Buff0, Buff1, W0, H0, CL) ->
    NoMipMaps = trunc(math:log2(min(W0,H0))),
    [{Img,W0,H0,0}|MMs] = make_spec(0, NoMipMaps, OrigImg, Buff0, Buff1, W0, H0, CL),
    Opts = [{wrap, {repeat,clamp}}, {filter, {mipmap, linear}}, {mipmaps, MMs}],
    ImId = wings_image:new_hidden(env_spec_tex, #e3d_image{width=W0,height=H0,image=Img,extra=Opts}),
    {env_spec_tex, ImId}.

make_spec(Level, Max, OrigImg, Buff0, Buff1, W, H, CL) when Level =< Max ->
    Step = Level/Max,
    Fill0 = wings_cl:fill(Buff0, <<0:(32*4)>>, W*H*4*4, CL),
    Fill1 = wings_cl:fill(Buff1, <<0:(32*4)>>, W*H*4*4, CL),
    {B0,B1,Pre}  = cl_multipass(make_specular, [OrigImg, W, H, Step],
                                Buff0, Buff1, 0, 10, [W,H], [Fill0, Fill1], CL),
    CC   = wings_cl:cast(color_convert, [B0,B1,W,H], [W,H], Pre, CL),
    Read = wings_cl:read(B1, W*H*4*4, [CC], CL),
    {ok, SpecData} = cl:wait(Read),
    Img = << << (round(R*255)), (round(G*255)), (round(B*255)) >> ||
              <<R:32/float-native, G:32/float-native, B:32/float-native, _:32>> <= SpecData >>,
    %% io:format("~p: ~p ~p  ~.3f~n", [Level, W, H, Step]),
    %% Level < 3 andalso
    %%     wings_image:debug_display(900-Level, #e3d_image{width=W, height=H, image=Img,
    %%                                         name="Spec: " ++ integer_to_list(Level)}),
    [{Img,W,H,Level} | make_spec(Level+1, Max, OrigImg, Buff0, Buff1, W div 2, H div 2, CL)];
make_spec(_Level, _Max, _OrigImg, _B0, _B1, _W, _H, _CL) ->
    [].

cl_multipass(Kernel, Args, Buff0, Buff1, N, Tot, No, Wait, CL) when N < Tot ->
    Next = wings_cl:cast(Kernel, Args ++ [Buff0, Buff1, N, Tot], No, Wait, CL),
    cl_multipass(Kernel, Args, Buff1, Buff0, N+1, Tot, No, [Next], CL);
cl_multipass(_Kernel, _Args, Buff0, Buff1, _N, _Tot, _No, Wait, _CL) ->
    {Buff0, Buff1, Wait}.

cl_setup(Recompile) ->
    case ?GET(opencl) of
	undefined ->
            case wings_cl:is_available() of
                true ->
                    try cl_setup_1()
                    catch _:Reason ->
                            io:format("CL setup error: ~p ~p~n",
                                      [Reason, erlang:get_stacktrace()]),
                            {error, no_openCL}
                    end;
                false -> {error, no_openCL}
            end;
	CL0 when Recompile ->
            try
                CL = wings_cl:compile("img_lib.cl", CL0),
                ?SET(opencl, CL),
                CL
            catch _:Reason ->
                    io:format("CL compile error: ~p ~p~n",
                              [Reason, erlang:get_stacktrace()]),
                    CL0
            end;
        CL ->
            CL
    end.

cl_setup_1() ->
    CL0 = wings_cl:setup(),
    case wings_cl:have_image_support(CL0) of
        true  ->
            CL = wings_cl:compile("img_lib.cl", CL0),
            ?SET(opencl, CL),
            CL;
        false ->
            ?SET(opencl, CL0),
            {error, no_openCL_image}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

light_types() ->
    [{?__(1,"Infinite"),infinite,
      ?__(2,"Create a far-away, directional light (like the sun)")},
     {?__(3,"Point"),point,
      ?__(4,"Create a light that radiates light in every direction")},
     {?__(5,"Spot"),spot,
      ?__(6,"Create a spotlight")},
     {?__(7,"Ambient"),ambient,
      ?__(8,"Create an ambient light source")},
     {?__(9,"Area"),area,
      ?__(10,"Create an area that radiates light")}].

menu(X, Y, St) ->
    SpotOnly = {iff,[spot]},
    NotAmb = {iff,[spot,infinite,point,area]},
    One = one_light,
    Dir = wings_menu_util:directions(St#st{selmode=body}),
    Menu0 = [{?__(2,"Move"),{move_light,Dir}},
	     {NotAmb,separator},
	     {NotAmb,{?__(3,"Position Highlight"),
		      {'VALUE',{position_highlight,{'ASK',{[point],[]}}}},
		      ?__(4,"Position the aim point or location of light")}},
	     {NotAmb,{?__(5,"Color"),color,
		      ?__(6,"Interactively adjust hue, value, and saturation")}},
	     {NotAmb,
	      {?__(7,"Attenuation"),
	       {attenuation,
		[{?__(8,"Linear"),linear,
		  ?__(9,"Interactively adjust how much light weakens as it travels away from its source (linear factor)")},
		 {?__(10,"Quadratic"),quadratic,
		  ?__(11,"Interactively adjust how much light weakens as it travels away from its source (quadratic factor)")}]}}},
	     {SpotOnly,separator},
	     {SpotOnly,{?__(12,"Spot Angle"),spot_angle,
			?__(13,"Interactivly adjust the angle of the spotlight cone")}},
	     {SpotOnly,{?__(14,"Spot Falloff"),spot_falloff,
			?__(15,"Interactivly adjust how much light weakens farther away from the center of the spotlight cone")}},
	     {One,separator},
	     {One,{?__(16,"Edit Properties..."),edit,
		   ?__(17,"Edit light properties")}}|body_menu(Dir, St)],
    Menu = filter_menu(Menu0, St),
    wings_menu:popup_menu(X, Y, light, Menu).

body_menu(Dir, #st{selmode=body}) ->
    [separator,
     {?STR(menu,18,"Duplicate"),{duplicate,Dir},
      ?STR(menu,19,"Duplicate and move selected lights")},
     {?STR(menu,20,"Delete"),delete,
      ?STR(menu,21,"Delete seleced lights")}];
body_menu(_, _) -> [].

filter_menu(Menu, St) ->
    MF = fun(_, #we{light=#light{type=Type}}) -> Type;
            (_, #we{}) -> not_light
         end,
    RF = fun(Type, []) -> Type;
            (Type, Type) -> Type;
            (_, _) -> mixed
         end,
    T = wings_sel:dfold(MF, RF, [], St),
    foldr(fun({one_light,_}, A) when T =:= mixed -> A;
             ({one_light,Entry}, A) -> [Entry|A];
             ({{iff,[_|_]=Types},Entry}, A) ->
                  case member(T, Types) of
                      true -> [Entry|A];
                      false -> A
                  end;
             (Entry, A) -> [Entry|A]
          end, [], Menu).
