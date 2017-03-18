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
-export([light_types/0,menu/3,command/2,is_any_light_selected/1,
	 any_enabled_lights/0,info/1,setup_light/2,
	 create/2,update_dynamic/2,update_matrix/2,update/1,
	 global_lights/1,
	 export/1,export_bc/1,export_camera_lights/0,
	 import/2,import/1,shape_materials/2,
	 light_pos/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d.hrl").

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
edit(#st{sel=[{Id,_}]}=St) ->
    edit(Id, St);
edit(_) -> wings_u:error_msg(?__(1,"Select only one light.")).

edit(Id, #st{shapes=Shs}=St) ->
    We = #we{light=#light{type=Type}} = gb_trees:get(Id, Shs),
    {Name,Prop} = get_light(We, false),
    case Type of
	ambient ->
	    {dialog,Qs,Fun} = edit_ambient_dialog(Name, Prop, We, Shs, St),
	    wings_dialog:dialog(?__(2,"Ambient Light Properties"), Qs, Fun);
	_ ->
	    {dialog,Qs,Fun} = edit_dialog(Name, Prop, We, Shs, St),
	    wings_dialog:dialog(?__(3,"Light Properties"), Qs, Fun)
    end.

edit_ambient_dialog(Name, Prop0, 
		    We0=#we{id=Id,light=#light{ambient=Amb0}=L0}, Shs, St) ->
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
		  We = We0#we{light=L},
		  St#st{shapes=gb_trees:update(Id, We, Shs)}
	  end,
    {dialog,Qs,Fun}.

edit_dialog(Name, Prop0, #we{id=Id,light=L0}=We0, Shs, St) ->
    #light{diffuse=Diff0,ambient=Amb0,specular=Spec0} = L0,
    Qs0 = {vframe,
	   [{hframe,
	     [{label_column,
	       [{?__(1,"Diffuse"),{color,Diff0}},
		{?__(2,"Ambient"),{color,Amb0}},
		{?__(3,"Specular"),{color,Spec0}}]}],
	     [{title,?__(4,"Colors")}]}|qs_specific(L0)]},
    Qs1 = wings_plugin:dialog({light_editor_setup,Name,Prop0}, [{"Wings 3D", Qs0}]),
    Qs = {vframe_dialog,
 	  [{oframe, Qs1, 1, [{style, buttons}]}],
	  [{buttons, [ok, cancel]}, {key, result}]},
    Fun = fun([Diff,Amb,Spec|More0]) ->
		  L1 = L0#light{diffuse=Diff,ambient=Amb,specular=Spec},
		  {L,More} = edit_specific(More0, L1),
		  case plugin_results(Name, Prop0, More) of
		      {ok,Prop} ->
			  We = We0#we{light=L#light{prop=Prop}},
			  St#st{shapes=gb_trees:update(Id, We, Shs)}
			  %%{again,Prop} -> edit_dialog(Name, Prop, We0, Shs, St)
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
    D#dlo{work=List,transparent=We}.

update(#dlo{work=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(#dlo{sel=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(D) -> D.

update_1(#we{light=#light{type=Type}}=We, #dlo{src_sel=SrcSel}) ->
    SelColor = case SrcSel of
		   none -> {0.0,0.0,1.0};
		   _ -> wings_pref:get_value(selected_color)
	       end,
    update_fun(Type, SelColor, We).

update_fun(infinite, SelColor, #we{light=#light{aim=Aim}}=We) ->
    LightPos = light_pos(We),
    LightCol = get_light_col(We),
    Vec = e3d_vec:norm_sub(Aim, LightPos),
    Data = [e3d_vec:mul(Vec, 0.2),e3d_vec:mul(Vec, 0.6)],
    {Len, Tris,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
		gl:lineWidth(1.5),
		gl:pushMatrix(),
		{X,Y,Z} = LightPos,
		gl:translatef(X, Y, Z),
		gl:color4fv(LightCol),
                gl:drawArrays(?GL_TRIANGLES, 2, Len*3),
		gl:color3fv(SelColor),
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
    {Len, Tris,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
		gl:lineWidth(1.0),
		gl:color4fv(LightCol),
		gl:pushMatrix(),
		{X,Y,Z} = LightPos,
		gl:translatef(X, Y, Z),
                gl:drawArrays(?GL_TRIANGLES, N, Len*3),
		gl:color3fv(SelColor),
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
    {Len, Tris,_} = wings_shapes:tri_sphere(#{subd=>3, scale=>0.08}),
    D = fun(RS) ->
                gl:lineWidth(1.0),
                gl:color4fv(LightCol),
                gl:pushMatrix(),
                {Tx,Ty,Tz} = Top,
                gl:translatef(Tx, Ty, Tz),
                gl:drawArrays(?GL_TRIANGLES, 0, Len*3),
                gl:color3fv(SelColor),
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

export(#st{shapes=Shs}, BackwardsCompatible) ->
    L = foldl(fun(We, A) when ?IS_ANY_LIGHT(We) ->
		      [get_light(We, BackwardsCompatible)|A];
		 (_, A) -> A
	      end, [], gb_trees:values(Shs)),
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

get_light(#we{name=Name,perm=P}=We, BC) ->
    Ps0 = get_light_1(We, BC),
    Ps = export_perm(P, Ps0),
    {Name,Ps}.

get_light_1(#we{light=#light{type=ambient,ambient=Amb,prop=Prop},pst=Pst}=We, _) ->
    P = light_pos(We),
    OpenGL = [{type,ambient},{ambient,Amb},{position,P},{pst,Pst}],
    [{opengl,OpenGL}|Prop];
get_light_1(#we{light=L,pst=Pst}=We, BC) ->
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
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.0,0.0,0.0,1.0}),
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

prepare_light(#light{type=ambient,ambient=Amb}=L, _We, _M) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb),
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
    Apde = arealight_posdirexp(We),
    prepare_arealight(L, Apde, M).

prepare_arealight(#light{type=area}=L, {Pos0,{0.0,0.0,0.0},_}, M) ->
    {X,Y,Z} = case M of
                  none -> Pos0;
                  _ -> mul_point(M, Pos0)
              end,
    #{light=>L, pos=>{X,Y,Z,1.0}};
prepare_arealight(#light{type=area}=L, {Pos0,Dir0,Exp}, M) ->
    {{X,Y,Z},Dir} =
	case M of
	    none -> {Pos0,Dir0};
	    _ ->
		Pos = mul_point(M, Pos0),
		Aim = mul_point(M, e3d_vec:add(Dir0, Pos0)),
		{Pos,e3d_vec:sub(Aim, Pos)}
	end,
    #{light=>L, pos=>{X,Y,Z,1.0}, dir=>Dir, exp=>Exp}.

setup_light(#{light:=#light{type=ambient,ambient=Amb}}, RS) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb),
    wings_shaders:use_prog(ambient_light, RS);
setup_light(#{light:=#light{type=infinite}=L, pos:=Pos}, RS) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, Pos),
    setup_color(?GL_LIGHT0, L),
    wings_shaders:use_prog(infinite_light, RS);
setup_light(#{light:=#light{type=point}=L,pos:=Pos}, RS) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, Pos),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 180.0),
    setup_color(?GL_LIGHT0, L),
    setup_attenuation(?GL_LIGHT0, L),
    wings_shaders:use_prog(point_light, RS);
setup_light(#{light:=#light{type=spot,spot_angle=Angle,spot_exp=Exp}=L,
              pos:=Pos, dir:=Dir}, RS) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, Pos),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, Angle),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_EXPONENT, Exp),
    gl:lightfv(?GL_LIGHT0, ?GL_SPOT_DIRECTION, Dir),
    setup_color(?GL_LIGHT0, L),
    setup_attenuation(?GL_LIGHT0, L),
    wings_shaders:use_prog(spot_light, RS);
setup_light(#{light:=#light{type=area}=L, pos:=Pos, dir:=Dir, exp:=Exp}, RS) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, Pos),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 90.0),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_EXPONENT, Exp),
    gl:lightfv(?GL_LIGHT0, ?GL_SPOT_DIRECTION, Dir),
    setup_color(?GL_LIGHT0, L),
    setup_attenuation(?GL_LIGHT0, L),
    wings_shaders:use_prog(spot_light, RS);
setup_light(#{light:=#light{type=area}=L, pos:=Pos}, RS) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, Pos),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 180.0),
    setup_color(?GL_LIGHT0, L),
    setup_attenuation(?GL_LIGHT0, L),
    wings_shaders:use_prog(point_light, RS).

setup_color(Lnum, #light{diffuse=Diff,ambient=Amb,specular=Spec}) ->
    gl:lightfv(Lnum, ?GL_DIFFUSE, Diff),
    gl:lightfv(Lnum, ?GL_AMBIENT, Amb),
    gl:lightfv(Lnum, ?GL_SPECULAR, Spec).

setup_attenuation(Lnum, #light{lin_att=Lin,quad_att=Quad}) ->
    gl:lightf(Lnum, ?GL_LINEAR_ATTENUATION, Lin),
    gl:lightf(Lnum, ?GL_QUADRATIC_ATTENUATION, Quad).

light_pos(#we{light=#light{type=area}}=We) ->
    {Pos,_,_} = arealight_posdirexp(We),
    Pos;
light_pos(We) ->
    wings_vertex:center(We).

arealight_posdirexp(#we{light=#light{type=area}}=We) ->
    Faces = wings_we:visible(We),
    ANCs = 
	[begin 
	     {wings_face:area(Face, We),
	      wings_face:normal(Face, We),
	      wings_face:center(Face, We)}
	 end || Face <- Faces, wings_face:good_normal(Face, We)],
    Area = foldl(fun ({A,_,_}, Acc) -> A+Acc end, 0.0, ANCs),
    AreaNormal = 
	foldl(fun ({A,N,_}, Acc) -> 
		      e3d_vec:add(e3d_vec:mul(N, A), Acc) end, 
	      {0.0,0.0,0.0}, ANCs),
    Center = e3d_vec:average([C || {_,_,C} <- ANCs]),
    Normal = e3d_vec:norm(AreaNormal),
    AreaNormalLen = e3d_vec:len(AreaNormal),
    if AreaNormalLen >= (0.9 * Area) -> 
	    %% 90 percent of all light in one direction; spotlight with falloff
	    {Center,Normal,1.0};
       true ->
	    Rear = foldl(fun ({_,N,_}, Acc) ->
				 A = e3d_vec:dot(Normal, N),
				 if A < 0.0 -> Acc-A; true -> Acc end
			 end, 0.0, ANCs),
	    if Rear > (0.1 * Area) ->
		    %% At least 10 percent rear light; pointlight
		    {Center,{0.0,0.0,0.0},0.0};
	       true ->
		    %% Front and side light, but no rear; hemispherical spot
		    {Center,Normal,0.0}
	    end
    end.

move_light(Pos, #we{vp=Vtab0}=We) ->
    Vtab = array:sparse_map(fun(_, _) -> Pos end, Vtab0),
    We#we{vp=Vtab}.

shape_materials(#light{diffuse={_,_,_,Af}=Front}, St) ->
    BlackF = {0.0,0.0,0.0,Af},
    Default = 
	sort([{opengl,sort([{diffuse,BlackF},{ambient,BlackF},{specular,BlackF},
			    {emission,Front},{shininess,0.0}])},
	      {maps,[]}]),
    wings_material:update_materials([{default,Default}], St).

mul_point({1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0, Tx,Ty,Tz}, {X,Y,Z}) ->
    {X+Tx,Y+Ty,Z+Tz};
mul_point(M, P) -> e3d_mat:mul_point(M, P).
