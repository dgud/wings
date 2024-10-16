%%
%%  auv_texture.erl --
%%
%%     Render and capture a texture.
%%
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(auv_texture).
-export([get_texture/2,draw_options/1,delete_preview_vbo/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-define(ERROR, error_msg(?LINE)).
-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include("auv.hrl").

-import(lists, [foreach/2,reverse/1,sort/1,foldl/3,member/2]).
-import(auv_segment, [map_vertex/2]).

-define(OPT_BG, [auv_background, {type_sel,color},{undefined,ignore},{1.0,1.0,1.0,0.0}]).
-define(OPT_EDGES, [auv_edges, all_edges,{0.0,0.0,0.0},1.0,false]).
-define(OPT_FACES, [texture]).
-define(PREVIEW_SIZE, 256).
-define(SHADER_PRW_NAME, "shdr_preview").
-define(SHADER_PRW_SIZE, 512).
-define(SHADER_PRW_VBO, shdr_vbo).

-record(opt, {texsz = {512,512},   %% Texture size
	      no_renderers = 4,
	      renderers = [{auv_background, ?OPT_BG},
			   {auv_edges, [?OPT_EDGES]}]
	     }).

-record(sh, {id=ignore,
	     name="Unnamed",   %% Shader menu entry
	     file="",
	     vs = "",          %% Vertex shader
	     fs = "",          %% Fragment shader
	     tex_units = 1,    %% No of texture units used
	     reqs = [],        %% Requirements: normals and/or binormals
	     args = [],        %% Arguments
	     def  = [],        %% Gui Strings
	     preview = true    %% Shows or not the preview dialog for the shader
	    }).

-record(sh_conf, {texsz,      % More shader options
		  fbo_r,      % Fbo read buffer
		  fbo_w,      % Fbo write buffer
		  fbo_d,      % Clean up fbo
		  prog,       % Shader Id
		  ts}).       % Shader data

-record(ts,         % What              Type
	{charts,    % #chart{}          (list)
         uv,        % UV positions       (binary)
	 pos,       % Real 3D position  (binary)
 	 n,         % Normal            (binary) Optional
	 bi,        % BiNormal          (binary) Optional
	 bb,        % BoundingBox 3D pos
         vc,        % Vertex colors     (binary)
         vbo        % Vbo               (binary)
	}).

-record(chart, 
	{id,        % Chart ID
	 fs,        % Faces see [{Face,#fs{}}]
	 oes=[],    % Outer vertices [[va,vb,face],[vc,vb,face]..],
	 bb_uv,     % Uv Bounding Box {{minX,minY,0},{maxX,maxY,0}}
	 mode       % Previous mode material/vertex-colored
	}).

-record(fs,
	{vs,        % triangulated vertex id in uv window [[Id1,Id2,Id3]]
	 vse,       % face vertex id's untriangulated for edge drawings
	 id}).      % Face Id

%% Menu

draw_options(#st{bb=Uvs}=AuvSt0) ->
    #uvstate{st=GeomSt0,matname=MatName0,bg_img=TexImg} = Uvs,
    BkpImg = wings_image:info(TexImg),
    prw_img_id(new),

    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = max(min(8192, MaxTxs0), 256),
    Shaders = shaders(),
    Prefs = get_valid_prefs(Shaders),
    TexSz = proplists:get_value(texsz, Prefs, 512),
    Qs = [{hframe,[{menu, gen_tx_sizes(MaxTxs, []),TexSz,
		    [{key,texsz}]}],[{title,?__(1,"Size")}]},
	  {vframe, render_passes(Prefs, Shaders), [{title,?__(2,"Render")}]}
	 ],
    Fun = fun({dialog_preview,Options}) ->
                  Opt = list_to_prefs(Options),
                  NewImg = ?SLOW(get_texture(AuvSt0, {Opt,Shaders})),
                  case MatName0 of
                      none ->
                          ok = wings_image:update(TexImg, NewImg);
                      _ ->
                          TexName = case get_mat_texture(MatName0, GeomSt0) of
                                        false -> atom_to_list(MatName0);
                                        OldId  ->
                                            OldImg = wings_image:info(OldId),
                                            OldName = OldImg#e3d_image.name,
                                            case string:prefix("auvBG",  OldName) of
                                                nomatch -> OldName;
                                                _ ->  atom_to_list(MatName0)
                                            end
                                    end,
                          catch wings_material:update_image(MatName0, diffuse, NewImg#e3d_image{name=TexName}, GeomSt0)
                  end,
                  {preview,GeomSt0,GeomSt0};
             (cancel) ->
                  case MatName0 of
                      none ->
                          ok = wings_image:update(TexImg, BkpImg);
                      _ ->
                          catch wings_material:update_image(MatName0, diffuse, BkpImg, GeomSt0)
                  end,
                  wings_wm:later({new_state,AuvSt0}),
                  prw_img_id(delete),
                  GeomSt0;
             (Options) ->
                  Opt = list_to_prefs(Options),
                  prw_img_id(delete),
                  set_pref([{tx_prefs,pref_to_list(Opt)}]),
                  {auv,{draw_options,{Opt,Shaders}}}
          end,
    wings_dialog:dialog(?__(3,"Draw Options"), {preview, Qs}, Fun).

get_mat_texture(MatName, #st{mat=Materials}) ->
    get_mat_texture(MatName, Materials);
get_mat_texture(MatName, Materials) ->
    case gb_trees:lookup(MatName, Materials) of
        none -> false;
        {value,Mat} ->
            Maps = proplists:get_value(maps, Mat, []),
            proplists:get_value(diffuse, Maps, false)
    end.

%% the goal is to remove invalid images references from the previous
%% shader settings stored in preferences which may not be present in 
%% the current project. That avoid crashes on preview dialg
get_valid_prefs(Shaders) ->
    Prefs = get_pref(tx_prefs, pref_to_list(#opt{})),
    validate_prefs(Prefs,Shaders,[]).
    
validate_prefs([], _, Acc) -> Acc;
validate_prefs([{texsz,_}=Val|Prefs], Sh, Acc) ->
    validate_prefs(Prefs, Sh, [Val|Acc]);
validate_prefs([{{auv_pass,_}=Slot,PassId}=Pass,{{auv_opt,_}=Op,OptVal}=Opt0|Prefs], Sh, Acc) ->
    PassOpt = 
        case PassId of
            {shader,Id} when OptVal /= [] -> 
                case lists:keysearch(Id,#sh.id,Sh) of
                    {value,#sh{args=Args}} ->
                        [{shader,_}|Opts] = OptVal,
                        case valid_opt(reverse(Args),Opts,true) of
                            true -> [Pass,Opt0];
                            false -> [{Slot,ignore},{Op,[]}]
                        end;
                    _ -> [{Slot,ignore},{Op,[]}]
                end;
            _ -> [Pass,Opt0]
        end,
    validate_prefs(Prefs, Sh, Acc++PassOpt);
validate_prefs([Val|Prefs], Sh, Acc) ->
    validate_prefs(Prefs, Sh, [Val|Acc]).

valid_opt([], _, Acc) -> Acc;
valid_opt([{uniform,{image,_},_,_,_}|As], [{_,Id}|Opts], Acc) ->
    ValidImg = wings_image:txid(Id) /= none,
    valid_opt(As,Opts,Acc and ValidImg);
valid_opt([{uniform,_,_,_,_}|As], [_|Opts], Acc) ->
    valid_opt(As,Opts,Acc);
valid_opt([_|As], Opts, Acc) ->
    valid_opt(As,Opts,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menu handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_passes(Prefs, Shaders) ->
    NoOfPasses = 7,
    Menu = renderers(Shaders),
    Background =
	{hframe,
	 [{menu,[{?__(1,"Background"), auv_background}],auv_background,
	   [{key,{auv_pass,0}}, enable_opt()]},
	  {value, get_def(Prefs,auv_opt,0), [{key,{auv_opt,0}}]},
	  {button,?__(2,"Options"),keep,
	   [{key, {opt_butt, 0}}, option_hook(0,background(),[])]}],
	 []},
    Other = [{hframe,
	      [{menu,Menu,default_menu(Id, Prefs),
		[{key,{auv_pass,Id}}, enable_opt()]},
	       {value,get_def(Prefs, auv_opt, Id),[{key,{auv_opt,Id}}]},
	       {button,?__(3,"Options"),keep,
		[{key, {opt_butt, Id}}, option_hook(Id, Menu, Shaders)]}
	      ],
	      []} || Id <- lists:seq(1, NoOfPasses)],
    [Background|Other].

default_menu(Pass, Prefs) ->
    case get_def(Prefs, auv_pass, Pass) of
	ignore -> default_menu(Pass);
	Val -> Val
    end.
	    
default_menu(1) -> auv_edges; 
default_menu(_) -> ignore.

get_def(List, What, Id) ->
    case proplists:get_value({What,Id}, List) of
	undefined when What =:= auv_pass -> ignore;
	undefined when What =:= auv_opt  -> [];
	Val -> Val
    end.

background() ->
    [{?__(1,"Background"), auv_background}].
renderers(Shaders) ->
    Menu0 = [{"*"++Name++"*",{shader,Id}} ||
		#sh{name=Name,id=Id} <- Shaders],
    [{?__(1,"None"), ignore},
     {?__(2,"Draw Edges"),auv_edges},
     {?__(3,"Draw Faces"),auv_faces}|Menu0].

option_dialog(Id, Fields, Renderers, Shaders) ->
    try
        Name = wings_dialog:get_value({auv_pass, Id}, Fields),
        Opts =
            case wings_dialog:get_value({auv_opt, Id}, Fields) of
                [] ->  %% it happens first time the menu item is filled
                    %% we need the Vals field to be filled in the next dialog,
                    %% then we need to build it at this point
                    case Name of
                        Name when is_atom(Name) -> NameId = Name;
                        {_,NameId} -> NameId
                    end,
                    if NameId =/= auv_edges ->
                        {value,#sh{def=Opts0}} = lists:keysearch(NameId,#sh.id,Shaders),
                        [{shader,NameId}|lists:reverse(Opts0)];
                    true ->
                        []
                    end;
                Opts0 ->
                    Opts0
            end,
        {StrName, Name} = renderer(Name,Renderers),
        SetValue = fun(Res) ->
                       %% Change the value in the parent dialog
                       wings_dialog:set_value({auv_opt, Id}, [Name|Res], Fields),
                       %% removing the temporary VBO data
                       delete_preview_vbo()
                   end,
        SphereData = create_sphere_data(),
        wings_dialog:dialog(StrName,options(Name,Opts,Shaders,SphereData),SetValue)
    catch _:Crash:ST ->
	    io:format("EXIT: ~p ~p~n",[Crash, ST])
    end.

%% removing the temporary VBO data used by preview
delete_preview_vbo() ->
    case wings_pref:get_value(?SHADER_PRW_VBO) of
        undefined ->  ignore;
        Vbo ->
            wings_pref:delete_value(?SHADER_PRW_VBO),
            wings_vbo:delete(Vbo),
            ignore
    end.

%% function required to ensure a config file with image on shader can be correctly
%% load by file name or by name (image in Wings3D), otherwise we use the default
%% auvBG in order to avoid GLSL issues by not receiving an image.
%% The Filename parameter comes from .auv config file
load_image(ImgName, Filename) when is_list(Filename) ->
    Is = wings_image:images(),
    ImgsId = [{Name,TexId} || {TexId, #e3d_image{name=Name}} <- Is],
    case filelib:is_file(Filename) of
        true ->
            %% lets check if the file was not loaded before (using the same var name)
            case lists:keyfind(ImgName,1,ImgsId) of
                {_,Value} -> Value;
                _ ->
                    %% we try to load the file set in the .auv configuration file
                    case wpa:image_read([{filename,Filename},
                                         {alignment,1}]) of
                        #e3d_image{}=Image ->
                            {ImgName,wings_image:new_temp(ImgName, Image)};
                        _ -> image_bg(ImgsId)
                    end
                end;
        _ ->
            %% Filename is an image name at Wings3D
            case lists:keyfind(Filename,1,ImgsId) of
                {_,Value} -> Value;
                _ ->
                    %% check for the image name already load
                    case lists:keyfind(ImgName,1,ImgsId) of
                        {_,Value} -> Value;
                        _ -> image_bg(ImgsId)
                    end
            end
    end;
%% after the first call, in the further ones the Filename will be
%% the image info {name,id}
load_image(_, Filename) -> Filename.

image_bg(ImgsId) ->
    AuvBG =
        case lists:keyfind("auvBG",1,ImgsId) of
            {_,Value} -> Value;
            _ -> wings_image:new("auvBG",wpc_autouv:bg_image())
        end,
    {"auvBG",AuvBG}.

options(auv_background, [auv_background, {type_sel,Type},{Image,_},Color],_,_) ->
    Enable = fun(_, What, Fields) ->
		     IsImage = image == What,
		     wings_dialog:enable(image_sel, IsImage, Fields),
		     wings_dialog:enable(col_sel,  not IsImage, Fields)
	     end,
    [{hradio,[{?__(1,"Image"),image},{?__(2,"Color"),color}],
      Type,[{key,type_sel},{hook, Enable}]},
     {hframe,[{label,?__(1,"Image")},image_selector(Image,[])],
      [{key, image_sel}]},
     {hframe,[{label,?__(2,"Color")},{color,fix(Color,wings_gl:have_fbo())}],
      [{key, col_sel}]}];
options(auv_background, _Bad,Sh,SphereData) ->
    options(auv_background, ?OPT_BG,Sh,SphereData);

options(auv_edges,[auv_edges, Type,Color,Size,UseVtxColors],_,_) ->
    [{vradio,[{?__(3,"Draw All Edges"),all_edges},
	      {?__(4,"Draw Border Edges"), border_edges}],
      Type, []},
     {hframe,[{label,?__(5,"Edge Color:")},{color,Color}]},
     {hframe,[{label,?__(6,"Edge Width:")},{text,Size,[{range,{0.0,100.0}}]}]},
     {?__(8,"Use vertex colors (on border edges)"),UseVtxColors}
    ];
options(auv_edges,_,Sh,SphereData) ->
    options(auv_edges,?OPT_EDGES,Sh,SphereData);

options({shader,Id}=Opt,Vals,Sh,SphereData) ->
    {value,Shader} = lists:keysearch(Id,#sh.id,Sh),
    options_1(Opt,Vals,Sh,SphereData,Shader);

options(Command,Vals,_,_) ->
    io:format("~p: ~p~n",[Command, Vals]),
    exit(unknown_default).

options_1(Opt, Vals0, _, _, #sh{preview=false}=Shader) ->
    FrmShader =
        case Vals0 of
            [Opt|Vals] ->
                shader_options(Shader,[],Vals);
            _ ->
                shader_options(Shader,[],[])
        end,
    [{vframe, FrmShader}];

options_1(Opt, Vals0, Sh, {Ts,SphereMesh}, Shader) ->
    Preview =
        fun(GLCanvas, _Fields) ->
            wings_light:init_opengl(),
            %% we create the sphere data for preview once
            case wings_pref:get_value(?SHADER_PRW_VBO) of
                undefined ->
                    Vbo = setup_sphere_vbo(SphereMesh),
                    wings_pref:set_value(?SHADER_PRW_VBO,Vbo);
                Vbo0 -> Vbo = Vbo0
            end,
            tex_preview(GLCanvas,Vbo)
        end,

    Refresh =
        fun(Key, Value, Fields) ->
            GLCanvas = wings_dialog:get_widget(preview, Fields),
            case wxWindow:isShown(GLCanvas) of
                false -> ok;
                true ->
                    %% update the Vals list with the content of Fields plus
                    %% the current field changed (Key) which is not updated
                    %% in Fields
                    Vals = update_values(Key,Value,Vals0,Fields),
                    %% creating the texture preview image
                    wings_gl:setCurrent(GLCanvas, ?GET(gl_context)),
                    PrwImg = get_texture_preview({Opt,Vals},Sh,Ts),
                    %% updating the image
                    prw_img_id(PrwImg),
                    case os:type() of
                        {_, darwin} -> %% workaround wxWidgets 3.0.4 and mojave
                            Preview(GLCanvas, Fields),
                            wxGLCanvas:swapBuffers(GLCanvas);
                        _ ->
                            wxWindow:refresh(GLCanvas)
                    end
            end
        end,

    FrmShader =
        case Vals0 of
            [Opt|Vals] ->
                shader_options(Shader,[{hook,Refresh}],Vals);
            _ ->
                shader_options(Shader,[{hook,Refresh}],[])
        end,
    [{hframe, [
        {vframe, [{custom_gl,?PREVIEW_SIZE,?PREVIEW_SIZE,Preview,
                   [{key, preview}, {proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]}],
                 [{title, "Preview"}]},
        {vframe, FrmShader, [{title, "Parameters"}]}]
    }].

shader_options(#sh{args=Args,def=Defs,file=File}, OptDef, Vals) ->
    case shader_menu(Args,OptDef,reverse(Vals),[]) of
	{failed,_} ->
	    case shader_menu(Args,OptDef,Defs,[]) of
		{failed,What} ->
		    io:format("AUV: Bad default value ~p in ~p~n",
			      [What, File]),
		    [];
		Menues -> Menues
	    end;
	Menues ->
	    Menues
    end.
shader_menu([{uniform,color,_,_,Label}|As],OptDef,[Col={_,_,_,_}|Vs],Acc) ->
    Menu = {hframe, [{label,Label},{color,Col,OptDef}]},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{uniform,float,_,_,Label}|As],OptDef,[Def|Vs],Acc)
  when is_number(Def) ->
    Menu = {hframe,[{label,Label},
		    {text,Def,[{range,{'-infinity',infinity}}]++OptDef}]},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{uniform,bool,_,_,Label}|As],OptDef,[Def|Vs],Acc)
  when is_boolean(Def) ->
    Menu = {Label, Def, OptDef},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{uniform,{slider,From,To},_,_,Label}|As],OptDef,[Def|Vs],Acc)
  when is_number(Def) ->
    Menu = {hframe,[{label,Label},
		    {slider,{text,Def,[{range,{From,To}},{width,5}]++OptDef}}]},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{uniform,{image,_},_,_Def,Label}|As],OptDef,[Def|Vs],Acc) ->
    Image = case Def of {Im,_} -> Im; _ -> Def end,
    Menu = {hframe,[{label,Label},image_selector(Image,OptDef)]},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{uniform,menu,_,_,Labels}|As],OptDef,[Def|Vs],Acc) ->
    Menu = {menu,Labels,Def,[]++OptDef},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{auv,{auv_send_texture,Label,Def0}}|As],OptDef,Vs0,Acc) ->
    case Vs0 of
	[{auv_send_texture, Def}|Vs] -> ok;
	[Def|Vs] -> ok;
	[] -> Def = Def0, Vs = []
    end,
    Menu = {Label, Def, [{key,auv_send_texture}]},
    shader_menu(As,OptDef,Vs,[Menu|Acc]);
shader_menu([{auv,_Skip}|As],OptDef,Vs,Acc) ->
    shader_menu(As,OptDef,Vs,Acc);
shader_menu([What|_],_,[Def|_],_) ->
    {failed,{What,value,Def}};
shader_menu([What|_],_,[],_) ->
    {failed,What};
shader_menu([],_,_,Acc) ->
    Acc.

image_selector(Default,OptDef) ->
    Is = wings_image:images(),
    Menu = [{Name,{Name,TexId}} || {TexId, #e3d_image{name=Name}} <- Is],
    case lists:keysearch(Default,1,Menu) of 
	{value,{_,What}} -> Def = What;
	_ -> case Menu of 
		 [{_,Def}|_] -> Def;
		 _ -> Def = void
	     end
    end,
    {menu,Menu,Def,OptDef}.

enable_opt() ->
    {hook,
     fun({_W, Id}, Pass, Fields) ->
	     Disable = Pass =:= ignore orelse Pass =:= auv_faces,
	     wings_dialog:enable({opt_butt, Id},  not Disable, Fields),
	     case wings_dialog:get_value({auv_opt, Id}, Fields) of
		 [Pass|_] ->
		     ok;
		 _ -> %% Changed type reset options
		     wings_dialog:set_value({auv_opt, Id}, [], Fields)
	     end
     end}.

option_hook(Id,Renderers,Shaders) ->
    Me = wings_wm:this(),
    {hook, fun(_Key, button_pressed, Fields) ->
                   %% Open dialog from the autouv process
                   Dlg = fun() ->
                                 option_dialog(Id, Fields, Renderers, Shaders),
                                 wings_wm:psend(send_once, dialog_blanket, preview),
                                 keep
                         end,
                   wings_wm:psend(Me, {callback, Dlg}),
                   ok
           end}.

renderer(Id,[Renderer={_,Id}|_R]) ->  Renderer;
renderer(Id,[_|R]) ->  renderer(Id,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture preview handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_values(Key, Val, Vals, Fields) ->
    update_values(1,Key,Val,Vals,Fields,[]).

update_values(_, _, _, [], _, Acc) -> lists:reverse(Acc);
update_values(Key, Key, Value, [_|Vals], Fields, Acc) ->
    update_values(Key+1,Key,Value,Vals,Fields,[Value|Acc]);
update_values(Key, CKey, CValue, [H|Vals], Fields, Acc) ->
    try wings_dialog:get_value(Key,Fields) of
        Value ->
            update_values(Key+1,CKey,CValue,Vals,Fields,[Value|Acc])
    catch
        _:_ ->
            update_values(Key+1,CKey,CValue,Vals,Fields,[H|Acc])
    end.

get_texture_preview(Render,Shaders,Ts) ->
    Options = #opt{texsz = {?SHADER_PRW_SIZE,?SHADER_PRW_SIZE},no_renderers=2,renderers=[Render]},
    Compiled = compile_shaders([Render],Shaders),
    Passes = get_passes([Render],{Shaders,Compiled}),
    Reqs = get_requirements(Shaders),
    Res  = render_image_preview(Ts, Passes, Options, Reqs),
    delete_shaders(Compiled),
    Res.

render_image_preview(Geom0, Passes, #opt{texsz={TexW,TexH}}, Reqs) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    UsingFbo = setup_fbo(TexW,TexH),
    {W0,H0} = if not UsingFbo ->
                  wings_wm:top_size();
              true ->
                  gl:blendEquationSeparate(?GL_FUNC_ADD, ?GL_MAX),
                  {TexW,TexH}
              end,
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    set_viewport({0,0,W,H}, 1),
    Geom = make_vbo(Geom0, Reqs),
    try
        Do = fun(Pass) ->
                if not UsingFbo ->
                    Pass(Geom,undefined);
                true ->
                    fill_bg_tex(UsingFbo),
                    Pass(Geom,UsingFbo)
                end
             end,
        Dl = fun() -> foreach(Do, Passes) end,
        ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, UsingFbo,[]),
        ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
        if not UsingFbo ->
            #e3d_image{image=ImageBin,width=TexW,height=TexH};
        true ->
            #e3d_image{image=ImageBin,width=TexW,height=TexH,
                       type=r8g8b8a8,bytes_pp=4}
        end
    catch _:What:Where ->
        exit({What,Where})
    after
        case UsingFbo of
            false -> ignore;
            #sh_conf{fbo_d=DeleteMe} -> DeleteMe()
        end,
        wings_vbo:delete(Geom#ts.vbo),
        gl:readBuffer(?GL_BACK),
        gl:popAttrib(),
        gl:blendEquationSeparate(?GL_FUNC_ADD, ?GL_FUNC_ADD),
        gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
        ?ERROR
    end.

create_sphere_data() ->
    {We, SphereData} = setup_sphere_mesh(),
    Shs = gb_trees:enter(We#we.id, We, gb_trees:empty()),
    Maps =
        case wings_pref:get_value(?SHADER_PRW_NAME) of
            undefined -> [];
            {TxId,_} -> {maps,[{diffuse,TxId}]}
        end,
    Mat = [{opengl,[{diffuse, {1.0,1.0,1.0,1.0}},
                    {emission,{0.0,0.0,0.0,1.0}},
                    {metallic, 0.0},
                    {roughness, 1.0},
                    {vertex_colors,ignore}]}] ++ Maps,
    Mtab = gb_trees:enter(list_to_atom(?SHADER_PRW_NAME),Mat,gb_trees:empty()),
    FakeSt0 = #st{sel=[],shapes=Shs,mat=Mtab},
    Uvs = #uvstate{st=wpa:sel_set(face, [], FakeSt0),
                   id      = We#we.id,
                   mode    = object,
                   matname = none},
    FakeSt = FakeSt0#st{selmode=body,sel=[],shapes=gb_trees:empty(),bb=Uvs,
                        repeatable=ignore,ask_args=none,drag_args=none},
    Ts = setup(rebuild_charts(We, FakeSt),[normal]),
    {Ts, SphereData}.

prw_img_id(new) ->
    case wings_pref:get_value(?SHADER_PRW_NAME) of
        undefined ->
            Image = bg_image_prw(),
            Id = wings_image:new_hidden(?SHADER_PRW_NAME,Image),
            TxId = wings_image:txid(Id),
            wings_pref:set_value(?SHADER_PRW_NAME,{TxId,Id}),
            TxId;
        {TxId,_} ->
            TxId
    end;
prw_img_id(#e3d_image{}=Image) ->
    TxId =
        case wings_pref:get_value(?SHADER_PRW_NAME) of
            undefined ->
                prw_img_id(new);
            {TxId0,Id} ->
                wings_pref:set_value(?SHADER_PRW_NAME,{TxId0,Id}),
                TxId0
        end,
    init_texture(Image, TxId);
prw_img_id(delete) ->
    case wings_pref:get_value(?SHADER_PRW_NAME) of
        undefined ->
            ignore;
        {_,Id} ->
            wings_image:delete(Id),
            wings_pref:delete_value(?SHADER_PRW_NAME)
    end.

bg_image_prw() ->
    White = <<255,255,255,255>>,
    Width = Height = ?SHADER_PRW_SIZE,
    Row = fill_bg(Width,White),
    Pixels = fill_bg(Height,Row),
    #e3d_image{width=Width,height=Height,image=Pixels,
               order=lower_left,bytes_pp=4,type=r8g8b8a8,name=?SHADER_PRW_NAME}.

fill_bg(Width, White) ->
    fill_bg(Width,White,<<>>).

fill_bg(0, _, Acc) -> Acc;
fill_bg(Width, White, Acc) -> fill_bg(Width-1,White,<<White/binary,Acc/binary>>).

init_texture(#e3d_image{width=W,height=H,image=Bits,extra=Opts}, TxId) ->
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    FT = {Format,Type} = {?GL_RGBA, ?GL_UNSIGNED_BYTE},
    Ft = case wings_pref:get_value(filter_texture, false) of
             true -> linear;
             false -> nearest
         end,
    {MinFilter,MagFilter} = proplists:get_value(filter, Opts, {mipmap, Ft}),
    MMs = proplists:get_value(mipmaps, Opts, []),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    case MinFilter of
        mipmap ->
            case lists:reverse(lists:keysort(4, MMs)) of
                [{_,_,_,Max}|_] ->
                    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAX_LEVEL, Max-1);
                [] ->
                    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, ?GL_TRUE)
            end,
            gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR_MIPMAP_LINEAR),
            gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, filter(MagFilter));
        _ ->
            gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, filter(MagFilter)),
            gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, filter(MinFilter))
    end,
    IntFormat = internal_format(FT),
    gl:texImage2D(?GL_TEXTURE_2D, 0, IntFormat, W, H, 0, Format, Type, Bits),
    [gl:texImage2D(?GL_TEXTURE_2D, Levl, IntFormat, MMW, MMH, 0, Format, Type, MM)
     || {MM, MMW, MMH, Levl} <- MMs],
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    ?CHECK_ERROR(),
    TxId.

filter(mipmap) -> ?GL_LINEAR_MIPMAP_LINEAR;
filter(linear) -> ?GL_LINEAR;
filter(nearest) -> ?GL_NEAREST.

%% internal_format({?GL_BGR,?GL_UNSIGNED_BYTE}) -> ?GL_RGB;
%% internal_format({?GL_BGRA,?GL_UNSIGNED_BYTE}) -> ?GL_RGBA;
%% internal_format({?GL_RGBA,?GL_FLOAT}) -> ?GL_RGBA32F;
%% internal_format({?GL_RGB,?GL_FLOAT}) -> ?GL_RGB32F;
internal_format({Format,?GL_UNSIGNED_BYTE}) -> Format.

rebuild_charts(We, St = #st{bb=UVS=#uvstate{st=Old,mode=Mode}}) ->
    {Faces,FvUvMap} = auv_segment:fv_to_uv_map(Mode,We),
    {Charts1,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We),
    Charts2 = auv_segment:cut_model(Charts1, Cuts, We),
    Charts = update_uv_tab(Charts2, FvUvMap),
    St#st{sel=[],bb=UVS#uvstate{mode=update_mode(Faces,We),st=Old#st{sel=[]}},shapes=Charts}.

update_mode(Faces0, #we{fs=Ftab}) ->
    Fs = gb_sets:from_list(Faces0),
    case gb_sets:size(Fs) == gb_trees:size(Ftab) of
        true -> object;
        false -> Fs
    end.

update_uv_tab(Cs, FvUvMap) ->
    update_uv_tab_1(Cs, FvUvMap, []).

update_uv_tab_1([#we{id=Id,name=#ch{vmap=Vmap}}=We0|Cs], FvUvMap, Acc) ->
    Fs = wings_we:visible(We0),
    UVs0 = wings_face:fold_faces(
        fun(F, V, _, _, A) ->
            OrigV = auv_segment:map_vertex(V, Vmap),
            [{V,[F|OrigV]}|A]
        end, [], Fs, We0),
    case update_uv_tab_2(sort(UVs0), FvUvMap, 0.0, []) of
        error ->
            %% No UV coordinate for at least some vertices (probably
            %% all) in the chart. Throw away this chart.
            update_uv_tab_1(Cs, FvUvMap, Acc);
        UVs1 ->
            UVs = array:from_orddict(UVs1),
            We = We0#we{vp=UVs},
            update_uv_tab_1(Cs, FvUvMap, [{Id,We}|Acc])
    end;
update_uv_tab_1([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

update_uv_tab_2([{V,_}|T], FvUvMap, Z, [{V,_}|_]=Acc) ->
    update_uv_tab_2(T, FvUvMap, Z, Acc);
update_uv_tab_2([{V,Key}|T], FvUvMap, Z, Acc) ->
    case gb_trees:get(Key, FvUvMap) of
        {X,Y} ->
            Pos = {X,Y,Z},
            update_uv_tab_2(T, FvUvMap, Z, [{V,Pos}|Acc]);
        _ ->
            %% No UV-coordinate for this vertex. Abandon the entire chart.
            error
    end;
update_uv_tab_2([], _, _, Acc) ->
    lists:reverse(Acc).

setup_sphere_mesh() ->
    #{size:=Len, tris:= Tris, ns:=Normals, uvs:=UVs, tgs:=Tgs} =
        wings_shapes:tri_sphere(#{subd=>4, ccw=>false, normals=>true, tgs=>true,
                                  uvs=>true, scale=>0.45}),
    Idx = length(Tris) div 3,
    Fs = [#e3d_face{vs=[I*3,I*3+1,I*3+2],
                    tx=[I*3,I*3+1,I*3+2],
                    ns=[I*3,I*3+1,I*3+2]} || I <- lists:seq(0,Idx-1)],
    Mesh = #e3d_mesh{vs=Tris,tx=UVs,ns=Normals,fs=Fs},
    #we{vp=Vtab0} = We = wings_import:import_mesh(material,Mesh),
    %% rotating the sphere slight above and right to better visualization
    %% of the blending area of a triplanar shader
    M0 = e3d_mat:rotate(30.0,{0.0,1.0,0.0}),
    M1 = e3d_mat:rotate(-30.0,{1.0,0.0,0.0}),
    M = e3d_mat:mul(M0,M1),
    Vtab =
        array:sparse_foldl(fun(V, Value0, Acc)->
                              Value = e3d_mat:mul_point(M,Value0),
                              array:set(V,Value,Acc)
                           end,Vtab0,Vtab0),
    Data = {Len, zip(Tris, Normals, UVs, Tgs)},
    {We#we{id=1,vp=Vtab,mat=list_to_atom(?SHADER_PRW_NAME)},Data}.

setup_sphere_vbo({Len,Data}) ->
    Layout = [vertex, normal, uv, tangent],
    D = fun(#{preview := PreviewMat} = RS0) ->
                RS1 = wings_shaders:use_prog(1, RS0),
                RS2 = lists:foldl(fun apply_material/2, RS1, PreviewMat),
                gl:drawArrays(?GL_TRIANGLES, 0, Len*3),
                wings_shaders:use_prog(0, RS2)
        end,
    wings_vbo:new(D, Data, Layout).

apply_material({{tex, Type}=TexType, TexId}, Rs0) ->
    case wings_shaders:set_state(TexType, TexId, Rs0) of
        {false, Rs0} ->
            Rs0;
        {true, Rs1} when TexId =:= none ->
            wings_shaders:set_uloc(texture_var(Type), enable(false), Rs1);
        {true, Rs1} ->
            gl:activeTexture(?GL_TEXTURE0 + tex_unit(Type)),
            gl:bindTexture(?GL_TEXTURE_2D, TexId),
            gl:activeTexture(?GL_TEXTURE0),
            wings_shaders:set_uloc(texture_var(Type), enable(true), Rs1)
    end;
apply_material({Type, Value}, Rs0)
    when Type =:= diffuse; Type =:= emission; Type =:= metallic; Type =:= roughness ->
    wings_shaders:set_uloc(Type, Value, Rs0);
apply_material({_Type,_}, Rs0) ->
    io:format("~p:~p: unsupported type ~p~n",[?MODULE,?LINE,_Type]),
    Rs0.

enable(true)  -> 1;
enable(false) -> 0.

texture_var(diffuse) -> 'UseDiffuseMap';
texture_var(normal) ->  'UseNormalMap';
texture_var(pbr_orm) -> 'UsePBRMap'; %% red = occlusion green = roughness blue = metallic
texture_var(emission) -> 'UseEmissionMap'.

tex_unit(diffuse) -> ?DIFFUSE_MAP_UNIT;
tex_unit(normal) -> ?NORMAL_MAP_UNIT;
tex_unit(pbr_orm) -> ?PBR_MAP_UNIT; %% red = occlusion green = roughness blue = metallic
tex_unit(emission) -> ?EMISSION_MAP_UNIT.

tex_preview(Canvas, Vbo) ->
    TxId = prw_img_id(new),
    Maps = [{{tex,diffuse}, TxId}],
    {W,H} = wxWindow:getSize(Canvas),
    Scale = wxWindow:getContentScaleFactor(Canvas),

    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:viewport(0, 0, round(W*Scale), round(H*Scale)),
    {BR,BG,BB, _} = wxWindow:getBackgroundColour(wxWindow:getParent(Canvas)),
    BGC = fun(Col) -> (Col-15) / 255 end,
    gl:clearColor(BGC(BR),BGC(BG),BGC(BB),1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    Fov = 45.0, Aspect = W/H,
    MatP = e3d_transform:perspective(Fov, Aspect, 0.01, 256.0),
    gl:multMatrixd(e3d_transform:matrix(MatP)),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    Dist = (0.5/min(1.0,Aspect)) / math:tan(Fov/2*math:pi()/180),
    Eye = {0.0,0.0,Dist}, Up = {0.0,1.0,0.0},
    MatMV = e3d_transform:lookat(Eye, {0.0,0.0,0.0}, Up),
    gl:multMatrixd(e3d_transform:matrix(MatMV)),
    gl:shadeModel(?GL_SMOOTH),
    Diff  = {diffuse, {1.0,1.0,1.0,1.0}},
    Emis  = {emission, {0.0,0.0,0.0,1.0}},
    Metal = {metallic, 0.0},
    Rough = {roughness, 1.0},
    Material = [Diff, Emis, Metal, Rough | Maps],
    gl:enable(?GL_BLEND),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:color4ub(255, 255, 255, 255),
    RS = #{ws_eyepoint=>Eye, view_from_world=>MatMV, preview=>Material},
    wings_dl:call(Vbo, RS),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_FLAT),
    gl:popAttrib(),
    case os:type() of
        {_, darwin} ->
            %% Known problem during redraws before window is shown
            %% only reset error check
            _ = gl:getError();
        _ ->
            wings_develop:gl_error_check("Rendering texture viewer")
    end.

zip(Vs, Ns, UVs, Tgs) ->
    zip_0(Vs, Ns, UVs, Tgs, []).

zip_0([V|Vs], [N|Ns], [UV|UVs], [T|Ts], Acc) ->
    zip_0(Vs, Ns, UVs,Ts, [V,N,UV,T|Acc]);
zip_0([], [], [], [],Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture creation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_texture(St = #st{bb=#uvstate{}}, {Options,Shaders}) ->
    Compiled = compile_shaders(Options#opt.renderers,Shaders),
    Passes = get_passes(Options#opt.renderers,{Shaders,Compiled}),
    Reqs = get_requirements(Shaders),
    Ts   = setup(St,Reqs),
    Res  = render_image(Ts, Passes, Options, Reqs),
    delete_shaders(Compiled),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture Rendering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_image(Geom0, Passes, #opt{texsz={TexW,TexH}}, Reqs) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),

    Current = wings_wm:viewport(),
    Scale = wings_wm:win_scale(),
    UsingFbo = setup_fbo(TexW,TexH),
    {W0,H0} = if not UsingFbo ->
                      wings_wm:top_size();
		 true ->
                      gl:blendEquationSeparate(?GL_FUNC_ADD, ?GL_MAX),
                      {TexW,TexH}
	      end,
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
%%    io:format("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}, 1),
    Geom = make_vbo(Geom0, Reqs),
    try
        Do = fun(Pass) ->
                     if not UsingFbo ->
                             Pass(Geom,undefined);
                        true ->
                             fill_bg_tex(UsingFbo),
                             Pass(Geom,UsingFbo)
                     end
             end,
        Dl = fun() -> foreach(Do, Passes) end,
	ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, UsingFbo,[]),
	ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
	if not UsingFbo ->
		#e3d_image{image=ImageBin,width=TexW,height=TexH};
	   true ->
		#e3d_image{image=ImageBin,width=TexW,height=TexH,
			   type=r8g8b8a8,bytes_pp=4}
	end
    catch _:What:Where ->
	    exit({What,Where})
    after
	case UsingFbo of
            false -> ignore;
            #sh_conf{fbo_d=DeleteMe} -> DeleteMe()
        end,
        wings_vbo:delete(Geom#ts.vbo),
        set_viewport(Current, Scale),
        gl:readBuffer(?GL_BACK),
        gl:popAttrib(),
        gl:blendEquationSeparate(?GL_FUNC_ADD, ?GL_FUNC_ADD),
        gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
        ?ERROR
    end.

make_vbo(#ts{uv=UV}=Ts, Reqs) ->
    {Layout, Bin} = make_bin_normal(Ts, Reqs, [{vertex2d, 0, 0}], UV),
    Ts#ts{vbo=wings_vbo:new(dynamic, Bin, {predefined, Layout})}.

make_bin_normal(#ts{n=Ns}=Ts, Reqs, Layout, Bin) ->
    case lists:member(normal, Reqs) of
	true  -> make_bin_color(Ts, Reqs, [{normal, 0, byte_size(Bin)}|Layout],
                                <<Bin/binary, Ns/binary>>);
	false -> make_bin_color(Ts, Reqs, Layout, Bin)
    end.

make_bin_color(#ts{vc=Vc}=Ts, Reqs, Layout, Bin) ->
    make_bin_pos3d(Ts, Reqs, [{color, 0, byte_size(Bin)}|Layout], <<Bin/binary, Vc/binary>>).

make_bin_pos3d(#ts{pos=Pos}=Ts, Reqs, Layout, Bin) ->
    case have_shaders() of
        true  -> make_bin_binormal(Ts, Reqs, [{{tex,1,3}, 0, byte_size(Bin)}|Layout],
                                   <<Bin/binary, Pos/binary>>);
	false -> make_bin_binormal(Ts, Reqs, Layout, Bin)
    end.

make_bin_binormal(#ts{bi=BiNs}, Reqs, Layout, Bin) ->
    case lists:member(binormal, Reqs) of
	true -> {lists:reverse([{{tex,2,4}, 0, byte_size(Bin)}|Layout]),
                 <<Bin/binary, BiNs/binary>>};
	false -> {lists:reverse(Layout), Bin}
    end.

%%%%%%%%% FBO stuff

setup_fbo(W,H) ->
    case wings_gl:setup_fbo({W,H}, [{color,[]}, {color,[]}, {depth,[]}]) of
	not_supported -> false;
	List ->
	    [Col1,Col2] = [Col || {color, Col} <- List],
	    #sh_conf{texsz={W,H},fbo_r=Col2,fbo_w=Col1,
		     fbo_d=fun() -> wings_gl:delete_fbo(List) end}
    end.
	
error_msg(Line) ->
    case wings_gl:error_string(gl:getError()) of
	no_error -> ok;
	Err -> io:format("~p:~p: ~p ~n",[?MODULE, Line,Err]), error
    end.

draw_texture_square() ->
    {U,V} = {1,0},
    VertexUvQ = << (0.0+U):?F32,(0.0+V):?F32, (0.0+U):?F32,(0.0+V):?F32,
                   (1.0+U):?F32,(0.0+V):?F32, (1.0+U):?F32,(0.0+V):?F32,
                   (1.0+U):?F32,(1.0+V):?F32, (1.0+U):?F32,(1.0+V):?F32,
                   (0.0+U):?F32,(1.0+V):?F32, (0.0+U):?F32,(1.0+V):?F32>>,
    wings_vbo:draw(fun(_) -> gl:drawArrays(?GL_QUADS, 0, 4) end, VertexUvQ, [vertex2d, uv]).

fill_bg_tex(#sh_conf{fbo_w=Prev}) ->
    gl:drawBuffer(?GL_COLOR_ATTACHMENT1_EXT),
    gl:bindTexture(?GL_TEXTURE_2D, Prev),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:enable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND),
    draw_texture_square(),
    gl:disable(?GL_TEXTURE_2D),
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
    ok.
	    
get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, DL, UsingFbo, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1.0, 1.0, 1.0, 1.0),
    gl:shadeModel(?GL_SMOOTH),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    texture_view(Wc, Wd, Hc, Hd),
    DL(),
    gl:flush(),
    {Sz,Type} = 
	case UsingFbo of
	    false -> 
		gl:readBuffer(?GL_BACK),
		{3,?GL_RGB};
	    _ -> 
		gl:readBuffer(?GL_COLOR_ATTACHMENT0_EXT),
		{4,?GL_RGBA}
	end,
    Mem = wings_io:get_buffer(W*H*Sz, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0,0,W,H,Type,?GL_UNSIGNED_BYTE,Mem),
    ImageBin = wings_io:get_bin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, DL, UsingFbo, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Dl, UsingFbo, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Dl, UsingFbo, ImageAcc);
get_texture(_,_,_,_,_,_,_,ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    Ortho = e3d_transform:ortho(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD, -1.0, 1.0),
    gl:loadMatrixd(e3d_transform:matrix(Ortho)),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL).
    
merge_texture_cols(List, Wd, Wd, _W, _RowC, Acc) ->
    {list_to_binary(reverse(Acc)), List};
merge_texture_cols([H|R], Wc, Wd, W, RowC, Acc) ->
    SkipBytes = RowC*W,
    <<_:SkipBytes/binary, Row:W/binary,_/binary>> = H,
    merge_texture_cols(R, Wc + 1, Wd, W, RowC, [Row|Acc]).

merge_texture_rows(_ImageBins, H, H, _W, _Wd,Acc, Last) ->
    {list_to_binary(reverse(Acc)), Last};
merge_texture_rows(ImageBins, RowC, H, W, Wd, Acc, _) ->
    {Row, Rest} = merge_texture_cols(ImageBins, 0, Wd, W, RowC, []),
    merge_texture_rows(ImageBins, RowC + 1, H,W,Wd, [Row|Acc], Rest).

merge_texture([Bin],1,1,_,_,[]) ->   Bin;  %% No merge needed.
merge_texture(Bins, 1,_,_,_,[]) ->   list_to_binary(Bins);  %% No merge needed.
merge_texture([],_,_,_,_,Acc) -> 
    list_to_binary(reverse(Acc));
merge_texture(ImageBins,Wd,Hd,W,H,Acc) ->    
    {Col, Bins} = merge_texture_rows(ImageBins, 0, H, W, Wd, [], ImageBins),
    merge_texture(Bins,Wd,Hd,W,H,[Col|Acc]).

calc_texsize(Vp, Tex) ->
    calc_texsize(Vp, Tex, Tex).

calc_texsize(Vp, Tex, Orig) when Tex =< Vp ->
    {Tex,Orig div Tex};
calc_texsize(Vp, Tex, Orig) ->
    calc_texsize(Vp, Tex div 2, Orig).

get_pref(Key, Def) ->
    wpa:pref_get(autouv, Key, Def).
set_pref(KeyVals) ->
    wpa:pref_set(autouv, KeyVals).

pref_to_list(#opt{texsz={TexSz,_TexSz}, no_renderers=NoR, 
		  renderers=[{auv_background,Bg}|Rs]}) ->
    [{texsz, TexSz},{{auv_pass,0},auv_background},{{auv_opt,0},Bg}|
     r2list(Rs,1,NoR)].

r2list([{Type, Opts}|Rest], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, Type},{{auv_opt,Id}, Opts}|r2list(Rest,Id+1,Max)];
r2list([], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, ignore},{{auv_opt,Id},[]}|r2list([],Id+1,Max)];
r2list([], _Id, _Max)  -> [].

list_to_prefs([{texsz, TexSz}|Rest]) ->
    LR = listOfRenders(Rest,[]),
    #opt{texsz={TexSz,TexSz},no_renderers=length(LR),renderers=LR}.

listOfRenders([{{auv_pass,_},ignore},_|Rest],Acc) ->
    listOfRenders(Rest,[{ignore,[]}|Acc]);
listOfRenders([{{auv_pass,_},Type},{{auv_opt,_},Opts}|Rest],Acc) ->
    listOfRenders(Rest,[{Type,Opts}|Acc]);
listOfRenders([],Acc) -> reverse(Acc).
    
gen_tx_sizes(Sz, Acc) when Sz < 128 -> Acc;
gen_tx_sizes(Sz, Acc) ->
    Bytes = Sz*Sz*3,
    Mb = 1024*1024,
    SzStr = if
		Bytes < 1024*1024 ->
		    io_lib:format("(~pKb)",[Bytes div 1024]);
		true ->
		    io_lib:format("(~pMb)",[Bytes div Mb])
	    end,
    Str0 = io_lib:format("~px~p ", [Sz,Sz]),
    Str = lists:flatten([Str0|SzStr]),
    gen_tx_sizes(Sz div 2, [{Str,Sz}|Acc]).

set_viewport({X,Y,W,H}=Viewport, Scale) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, round(W*Scale), round(H*Scale)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data setup 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup(#st{shapes=ShUV,selmode=SModeUV0,sel=SelUV0,bb=Uvs}=St, Reqs) ->
    #uvstate{id=RId,matname=MatName,st=#st{shapes=Sh0}} = Uvs,
    We = gb_trees:get(RId,Sh0),
    {Charts,{_Cnt,UVpos,Vpos,Ns,Ts,BB,Vc}} =
        case wpc_autouv:get_textureset_info(We) of
            {?MULTIPLE,[_,[_|_]]} ->
                Get_mat_face = fun(#we{id=Id}=WeUV) ->
                        FsMat = wings_facemat:all(WeUV),
                        case [F || {F,Mat} <- FsMat, Mat==MatName] of
                            [] -> [];
                            Fs -> {Id,Fs}
                        end
                    end,
                SelForTile = lists:flatten([Get_mat_face(WeUV) || WeUV <- gb_trees:values(ShUV)]),
                case SelForTile of
                    [_|_] ->
                        if (SelUV0==[]) ->
                            SModeUV = body,
                            SelUV = [{Id,gb_sets:singleton(0)} || {Id,_} <- SelForTile];
                        true ->
                            SelUV1 = [Sel || {IdSel,_}=Sel <- SelUV0, proplists:is_defined(IdSel,SelForTile)],
                            case SelUV1 of
                                [] ->
                                    SModeUV = face,
                                    SelUV = [{Id,gb_sets:from_list(Fs)} || {Id,Fs} <- SelForTile];
                                _ ->
                                    SModeUV = SModeUV0,
                                    SelUV = SelUV0
                            end
                        end,
                        setup_charts(St#st{selmode=SModeUV,sel=SelUV}, We, Reqs);
                    [] ->
                        setup_charts(St, We, Reqs)
                end;
            none ->
                setup_charts(St, We, Reqs)
        end,
    #ts{charts=Charts,
	uv = to_bin(UVpos,uv),
	pos= to_bin(Vpos,pos),
	n  = to_bin(Ns,pos),
	bi = build_tangents(Ts),
	bb = BB,
	vc = to_bin(Vc, vertex)}.

setup_charts(#st{shapes=Cs0,selmode=Mode,sel=Sel}, We, Reqs) ->
    Ns = case member(normal, Reqs) orelse member(binormal, Reqs) of
	     true -> setup_normals(We#we{mirror=none});
	     false -> []
	 end,
    Z = e3d_vec:zero(),
    TSA = array:new([{default, {Z,Z}}]),
    Start = {0,[],[],[],{TSA,[]},[],[]}, %% {UvPos,3dPos,Normal,Tangent,Vc}
    Shapes = if Sel =:= [] -> 
		     gb_trees:values(Cs0);
		Mode =:= body -> 
		     [gb_trees:get(Id,Cs0) || {Id,_} <- Sel];
		true -> 
		     gb_trees:values(Cs0)
	     end,
    Setup = fun(Ch,Acc) ->
		    setup_chart(Ch, Ns, Reqs, We, Acc)
	    end,
    lists:mapfoldl(Setup, Start, Shapes).

setup_chart(#we{id=Id}=Ch, Ns, Reqs, WWe, State0) ->
    OEs0 = outer_verts(Ch),
    BB = [],
    {Fs,{OEs,UvBB,State}} = create_faces(Ch, WWe, Ns, Reqs, {OEs0,BB,State0}),
    {#chart{id=Id,fs=Fs,oes=OEs,bb_uv=UvBB},State}.

create_faces(#we{vp=Vtab,name=#ch{vmap=Vmap}}=We,
	     #we{vp=Vt3d}=RealWe, NTab, Reqs, State) ->
    Fs = wings_we:visible(We),
    C=fun(Face,{OEs,UvBB,{Cnt,UVpos,Vpos,Ns,Ts0,PosBB,Vc}}) ->
	      Vs0 = wings_face:vertices_ccw(Face,We),
	      UVcoords = [array:get(V, Vtab) || V <- Vs0],
	      Coords   = [array:get(map_vertex(V,Vmap),Vt3d) 
			  || V <- Vs0],
	      Normals = if
			    NTab=:= [] ->
				[];
			    true ->
				fix_normals(Vs0, Vmap,
					    wings_va:face_attr([vertex|uv],
							       Face, RealWe),
					    gb_trees:get(Face,NTab))
			end,
	      OldVc  = fix_vc(Vs0, Face, RealWe, Vmap),
	      Len = length(Vs0),
	      FaceVs = lists:seq(0, Len-1),
	      Vs = case Len of
		       3 -> FaceVs;
		       Len -> triangulate(FaceVs,UVcoords)
		   end,
%	      io:format("Face ~p Normals ~p~n", [Face,Normals]),
	      Ts = calc_tang_vecs(Vs,Vs0,Coords,UVcoords,Normals,Ts0,Reqs),
	      Indx = fun(I) -> [V+Cnt || V <- I] end,
	      {#fs{vs=Indx(Vs),vse=Indx(FaceVs),id=Face},
	       {map_oes(OEs, Vs0, Cnt+Len-1, Face),
		e3d_vec:bounding_box(UvBB ++ UVcoords),
		{Cnt+Len,
		 UVcoords ++ UVpos,
		 Coords   ++ Vpos,
		 Normals  ++ Ns,
		 Ts,
		 e3d_vec:bounding_box(PosBB ++ Coords),
		 OldVc   ++ Vc}}}
      end,
    lists:mapfoldl(C, State, Fs).

triangulate(FaceVs,Vcoords) ->
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = e3d_mesh:triangulate_face(E3dface, Vcoords),
    lists:append([FVs || #e3d_face{vs=FVs} <- T3dfaces]).

map_oes([[A,B,Face]|OEs], Vs0, Cnt, Face) ->
    MA = find_index(A, Vs0, Cnt),
    MB = find_index(B, Vs0, Cnt),
    [[MA,MB,Face]|map_oes(OEs, Vs0, Cnt, Face)];
map_oes([Other|OEs], Vs0, Cnt, Face) ->
    [Other|map_oes(OEs, Vs0, Cnt, Face)];
map_oes([], _, _, _) -> [].

find_index(Val, [Val|_], Pos) -> Pos;
find_index(Val, [_|R], Pos) -> find_index(Val, R, Pos-1).

fix_normals(Vs,Vmap,VsI,Ns0) -> %% can be different order 
    fix_normals(Vs,n_zip(VsI,Ns0),Vmap).  
fix_normals([V|R],Ns,Vmap) ->
    [find(map_vertex(V,Vmap),Ns)|fix_normals(R,Ns,Vmap)];
fix_normals([],_,_) -> [].

%%%% Tangent calc

calc_tang_vecs(_,_,_,_,[],Ts,_) -> Ts;
calc_tang_vecs(Vs,VsIds,Coords,UVCoords,Normals,Ts0,Reqs) ->
    %% io:format("Vs ~p ~n",[Vs]),
    %% io:format("Coords ~p ~n",[Coords]),
    %% io:format("UV ~p ~n",[UVCoords]),
    %% io:format("Normals ~p ~n",[Normals]),
    case lists:member(binormal,Reqs) of
	true ->
	    N = e3d_vec:norm(e3d_vec:average(Normals)),
	    calc_tang_vecs_1([V+1||V <- Vs], VsIds, Coords, UVCoords, N, Ts0);
	false ->
	    Ts0
    end.

calc_tang_vecs_1([Vi1,Vi2,Vi3|Vis], VsIds, Coords, UVCoords, N, {Ts, F2V}) ->
    {X1,Y1,Z1} = e3d_vec:sub(lists:nth(Vi2,Coords), lists:nth(Vi1,Coords)),
    {X2,Y2,Z2} = e3d_vec:sub(lists:nth(Vi3,Coords), lists:nth(Vi1,Coords)),
    {U1,V1,_} = lists:nth(Vi1, UVCoords),
    {U2,V2,_} = lists:nth(Vi2, UVCoords),
    {U3,V3,_} = lists:nth(Vi3, UVCoords),
    S1 = U2-U1,
    S2 = U3-U1,
    T1 = V2-V1,
    T2 = V3-V1,
    Vs = [lists:nth(Vi1, VsIds),lists:nth(Vi2,VsIds),lists:nth(Vi3,VsIds)],
    try
	F = 1.0 / (S1*T2 - S2*T1),
	Tangent = {F*(T2*X1-T1*X2), F*(T2*Y1-T1*Y2), F*(T2*Z1-T1*Z2)},
	BiTangent = {F*(S1*X2-S2*X1), F*(S1*Y2-S2*Y1), F*(S1*Z2-S2*Z1)},
	H = case e3d_vec:dot(e3d_vec:cross(N, Tangent), BiTangent) < 0.0 of
		true  -> 1;
		false -> -1
	    end,
	Tss0 = {add_tangent(Vs, Tangent, BiTangent, Ts), [{N,H,Vs}|F2V]},
	calc_tang_vecs_1(Vis, VsIds, Coords, UVCoords, N, Tss0)
    catch _:badarith ->
	    Tss1 = {Ts, [{N,0,Vs}|F2V]},
	    calc_tang_vecs_1(Vis, VsIds, Coords, UVCoords, N, Tss1)
    end;
calc_tang_vecs_1([], _, _, _, _, Tss) ->
    Tss.

add_tangent([V|Vs], Tangent, BiTangent, Ts) ->
    {T0, B0} = array:get(V,Ts),
    T1 = e3d_vec:add(T0, Tangent),
    T2 = e3d_vec:add(B0, BiTangent),
    add_tangent(Vs, Tangent, BiTangent, array:set(V, {T1, T2}, Ts));
add_tangent([], _, _, Ts) -> Ts.

build_tangents({VsTs0, RevF2V}) ->
    VsTs = array:map(fun(_V, {T, BT}) -> {e3d_vec:norm(T), e3d_vec:norm(BT)} end, VsTs0),
    build_tangents(lists:reverse(RevF2V), VsTs, <<>>).

build_tangents([{N, H, Face}|Fs], Ts, Bin0) ->
    Bin = add_tangents1(Face, Ts, H, N, undefined, Bin0),
    build_tangents(Fs, Ts, Bin);
build_tangents([], _, Bin) -> Bin.

add_tangents1([V|Vs], Ts, H0, N, Prev, Bin0) ->
    case array:get(V, Ts) of
	{{+0.0, +0.0, +0.0}, BiT} ->
	    {Tan = {X,Y,Z}, H} = get_tangent(Prev, BiT, H0, N),
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H:?F32>>,
	    add_tangents1(Vs, Ts, H, N, Tan, Bin);
	{Tan = {X,Y,Z}, _} when H0 /= 0 ->
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H0:?F32>>,
	    add_tangents1(Vs, Ts, H0, N, Tan, Bin);
	{Tan = {X,Y,Z}, BiT} ->
	    H = case e3d_vec:dot(e3d_vec:cross(N, Tan), BiT) < 0.0 of
		    true  -> 1;
		    false -> -1
		end,
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H:?F32>>,
	    add_tangents1(Vs, Ts, H, N, Tan, Bin)
    end;
add_tangents1([], _, _, _, _, Bin) -> Bin.

get_tangent(undefined, {+0.0,+0.0,+0.0}, H0, N) ->
    H = if H0 =:= 0 -> -1; true -> H0 end,
    {cross_axis(N), H};
get_tangent(undefined, BiT, 0, N) ->
    T = e3d_vec:cross(BiT, N),
    H = case e3d_vec:dot(e3d_vec:cross(N, T), BiT) < 0.0 of
	    true  -> 1;
	    false -> -1
	end,
    {T, H};
get_tangent(undefined, BiT, H, N) ->
    {e3d_vec:cross(BiT, N), H};
get_tangent(Prev, _, H, _) ->
    {Prev, H}.

cross_axis(N = {NX,NY,NZ}) ->
    try
	V2 = case abs(NX) > abs(NY) of
		 true ->
		     ILen = 1.0 / math:sqrt(NX*NX+NZ*NZ),
		     {-NZ*ILen, 0.0, NX * ILen};
		 false ->
		     ILen = 1.0 / math:sqrt(NY*NY+NZ*NZ),
		     {0.0, NZ*ILen, -NY * ILen}
	     end,
	e3d_vec:cross(N, V2)
    catch _:badarith ->
	    {1.0, 0.0,0.0}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fix_vc(Vs, Face, We, Vmap) ->
    try 
	Uvc = wings_va:face_attr([vertex|color], Face, We),
	fix_vc1(Vs, Uvc, Vmap, [])
    catch error:_ ->
	    fix_vc1(Vs, [], Vmap, [])
    end.

fix_vc1([V|Vs], Uvc, Vmap, Acc) ->
    Val = case find(map_vertex(V, Vmap), Uvc) of
	      {_,_,_}=Color ->  Color;
	      _ -> {1.0,1.0,1.0}
	  end,
    fix_vc1(Vs, Uvc, Vmap, [Val|Acc]);
fix_vc1([], _, _, Acc) -> reverse(Acc).

find(V, [[V|Info]|_R]) -> Info;
find(V, [_|R]) -> find(V,R);
find(_, []) -> none.

n_zip([[V|_]|R1],[N|R2]) ->
    [[V|N]|n_zip(R1,R2)];
n_zip([],[]) -> [].
    

fix(OK = {_,_,_}, false) -> OK;
fix(OK = {_,_,_,_}, true) -> OK;
fix({R,G,B,_}, false) -> {R,G,B};
fix({R,G,B}, true) -> {R,G,B,1.0}.
    
setup_normals(We = #we{fs=Ftab}) ->
    FN0	= [{Face,wings_face:normal(Face, We)} || Face <- gb_trees:keys(Ftab)],
    Ns = wings_we:normals(FN0, We, none),
    gb_trees:from_orddict(sort(Ns)).

outer_verts(We = #we{es=Etab}) ->
    Fs = wings_we:visible(We),
    Outer = auv_util:outer_edges(Fs,We,false),    
    Verts = fun({Edge,Face}) -> 
		    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
		    [Va,Vb,Face]
	    end,
    lists:map(Verts, Outer).

%% Start with 64 bytes so that binary will be reference counted 
%% and not on the process heap spent hours debugging this.. :-(
to_bin(List, uv) -> to_bin3to2(List,[<<0:512>>]);
to_bin(List, pos) -> to_bin3(List,[<<0:512>>]);
to_bin(List, vertex) -> to_bin3(List,[<<0:512>>]).  %% Vertex colors

to_bin3([{A,B,C}|R],Acc) -> 
    to_bin3(R,[<<A:32/native-float,B:32/native-float,C:32/native-float>>|Acc]);
to_bin3([],Acc) -> list_to_binary(Acc).

to_bin3to2([{A,B,_}|R],Acc) -> 
    to_bin3to2(R,[<<A:32/native-float,B:32/native-float>>|Acc]);
to_bin3to2([],Acc) -> list_to_binary(Acc).

%% Workaround for ATI: gl:'end' doesn't bite for line loop/strip...
vs_lines([A|R=[B|_]],Last) ->
    [A,B|vs_lines(R,Last)];
vs_lines([B],Last) ->
    [B,Last].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Builtin Shader Passes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_passes(Passes,Shaders) ->
    lists:foldr(fun(Pass,Acc) ->
			case pass(Pass,Shaders) of
			    Fun when is_function(Fun) ->
				[Fun|Acc];
			    _ ->
				Acc
			end
		end, [], Passes).

pass({ignore,_},_) ->  ignore;
pass({auv_background,[auv_background, {type_sel,color},_,Color]},_) ->
    fun(_Geom,_) ->
	    {R,G,B,A} = fix(Color,true),
	    gl:clearColor(R,G,B,A),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT)
    end;
pass({auv_background,[auv_background, {type_sel,image},{_Name,Id},Color]},_) ->
    fun(_Geom,_) ->
	    {R,G,B,A} = fix(Color,true),
	    gl:clearColor(R,G,B,A),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    case wings_image:txid(Id) of
		none -> ignore;
		TxId ->
 		    gl:enable(?GL_TEXTURE_2D),
		    gl:bindTexture(?GL_TEXTURE_2D, TxId),
		    draw_texture_square(),
		    gl:disable(?GL_TEXTURE_2D)
	    end
    end;
pass({auv_background, _},Sh) ->
    pass({auv_background, ?OPT_BG},Sh);

pass({auv_edges, [auv_edges,all_edges,Color,Width,_UseMat]},_) ->
    R=fun(#chart{fs=Fs}) ->
	      Draw = fun(#fs{vse=Vs}) ->
			     Patched = vs_lines(Vs,hd(Vs)),
			     wings_gl:drawElements(?GL_LINES,length(Patched),
                                                   ?GL_UNSIGNED_INT,Patched)
                     end,
	      foreach(Draw,Fs)
      end,
    fun(#ts{vbo={call,_,{vbo,Vbo}}, charts=Charts},_) ->
	    gl:disable(?GL_DEPTH_TEST),
	    gl:color3fv(Color),
	    gl:lineWidth(float(Width)),
            gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
            gl:vertexPointer(2, ?GL_FLOAT, 0, 0),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    foreach(R, Charts),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, [auv_edges,border_edges,Color,Width,UseMat]},_) ->
    R= fun(#chart{oes=Es0}) ->
	       Es = foldl(fun([A,B,_],Acc) -> [A,B|Acc] end, [], Es0),
	       wings_gl:drawElements(?GL_LINES,length(Es),?GL_UNSIGNED_INT,Es)
       end,
    fun(#ts{vbo={call,_,{vbo,Vbo}}, charts=Charts},_) ->
	    gl:color3fv(Color),
	    gl:lineWidth(float(Width)),
            gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
            gl:vertexPointer(2, ?GL_FLOAT, 0, 0),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:disable(?GL_DEPTH_TEST),
	    case UseMat of
		true ->
		    gl:enableClientState(?GL_COLOR_ARRAY),
		    foreach(R, Charts),
		    gl:disableClientState(?GL_COLOR_ARRAY);
		_ ->
		    foreach(R, Charts)
	    end,
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, _},Sh) ->
    pass({auv_edges, ?OPT_EDGES},Sh);

pass({auv_faces,[_]},_) ->
    fun(#ts{vbo={call,EnableVbo,{vbo,_Vbo}}, charts=Charts}, _) ->
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    R = fun(#fs{vs=Vs}) ->
			wings_gl:drawElements(?GL_TRIANGLES,length(Vs),?GL_UNSIGNED_INT,Vs)
		end,
	    All = fun(_) -> foreach(fun(#chart{fs=Fs}) -> foreach(R, Fs) end, Charts) end,
            EnableVbo(All, #{})
    end;
pass({auv_faces, _},Sh) ->
    pass({auv_faces,?OPT_FACES},Sh);

pass({{shader,Id}, Opts},{Sh,Compiled}) ->
    shader_pass(lists:keysearch(Id,#sh.id,Sh),
		lists:keysearch(Id,1,Compiled),Opts);
pass({_R, _O},_) ->
    io:format("AUV: ~p:~p: Unknown Render Pass (~p) or options (~p) ~n",
	      [?MODULE,?LINE,_R,_O]),
    ignore.

shader_pass(Shader={value,#sh{id=Id, def=Def}},Prog,[]) when Def /= [] ->
    shader_pass(Shader, Prog, [{shader,Id}|reverse(Def)]);
shader_pass(_,false,_) ->
    io:format("AUV: No shader program found skipped ~p~n", [?LINE]),
    ignore;
shader_pass(false,_,_) ->
    io:format("AUV: Not shader found skipped ~p~n", [?LINE]),
    ignore;
shader_pass({value,#sh{id=Id, args=Args, tex_units=TexUnits}},
	    {value,{_,Prog}}, [{shader,Id}|Opts]) ->
    fun(Ts = #ts{vbo={call,EnableVbo,_}, charts=Charts}, Config) ->
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    wings_gl:use_prog(Prog),
	    try
		Conf = Config#sh_conf{prog=Prog,ts=Ts},
		PerChartUniF = shader_uniforms(reverse(Args),Opts,Conf),
		case send_texture(reverse(Args),Opts) of
		    true ->
			gl:enable(?GL_TEXTURE_2D),
			gl:disable(?GL_BLEND),
			draw_texture_square();
		    _ ->
                        DC = fun(Chart = #chart{fs=Fas}) ->
                                     PerFaceUniF = sh_uniforms(PerChartUniF,Chart,Conf),
                                     R = fun(Face = #fs{vs=Vss}) ->
                                                 sh_uniforms(PerFaceUniF,Face,Conf),
                                                 wings_gl:drawElements(?GL_TRIANGLES,length(Vss),
                                                                       ?GL_UNSIGNED_INT,Vss)
                                         end,
                                     foreach(R,Fas)
                             end,
                        EnableVbo(fun(_) -> foreach(DC, Charts) end, #{})
                end
            catch throw:What ->
                    io:format("AUV: ERROR ~s ~n",[What]);
		_:What:Stack ->
		    io:format("AUV: Internal ERROR ~p:~n~p ~n",[What,Stack])
	    after
                wings_gl:use_prog(0),
                gl:disable(?GL_TEXTURE_2D),
                gl:disableClientState(?GL_VERTEX_ARRAY),
                gl:disableClientState(?GL_NORMAL_ARRAY),
                gl:clientActiveTexture(?GL_TEXTURE2),
                gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
                gl:clientActiveTexture(?GL_TEXTURE1),
                gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
                gl:clientActiveTexture(?GL_TEXTURE0),
                disable_tex_units(TexUnits),
                gl:activeTexture(?GL_TEXTURE0)
	    end
    end.

send_texture([{auv,{auv_send_texture,_, _Def0}}|_],
	     [{auv_send_texture,Def}|_]) ->
    Def;
send_texture([{auv,{auv_send_texture,_, _Def0}}|_],
	     [Def|_]) ->
    Def;
send_texture([{auv,_}|Next], Opts) ->
    send_texture(Next,Opts);
send_texture([_|Next], [_|Opts]) ->
    send_texture(Next,Opts);
send_texture([],_) -> false.


shader_uniforms([A|As], [O|Opts]=Opts0, Conf) ->
    Res = shader_uniform(A,O,Conf),
    ?ERROR == error andalso io:format("~p~n", [A]),
    case Res of
        ok -> shader_uniforms(As,Opts,Conf);
        no_opt -> shader_uniforms(As,Opts0,Conf);
        {keep, Keep} -> [Keep|shader_uniforms(As,Opts0,Conf)]
    end;
shader_uniforms([], [], _) ->
    [].

shader_uniform({uniform,color,Name,_,_}, Val, Conf) ->
    wings_gl:set_uloc(Conf#sh_conf.prog, Name,Val),
    ok;
shader_uniform({uniform,float,Name,_,_},Val,Conf) ->
    wings_gl:set_uloc(Conf#sh_conf.prog, Name, Val),
    ok;
shader_uniform({uniform,menu,Name,_,_}, Vals,Conf)
  when is_list(Vals) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    foldl(fun(Val,Cnt) when is_integer(Val) ->
                gl:uniform1i(Loc+Cnt,Val),Cnt+1;
             (Val,Cnt) ->
                gl:uniform1f(Loc+Cnt,Val),Cnt+1
          end,0,Vals),
    ok;
shader_uniform({uniform,menu,Name,_,_},Vals,Conf)
    when is_integer(Vals) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:uniform1i(Loc,Vals),
    ok;
shader_uniform({uniform,bool,Name,_,_},Val,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    BoolF = if Val -> 1.0; true -> 0.0 end,
    gl:uniform1f(Loc, BoolF),
    ok;
shader_uniform({uniform,{slider,_,_},Name,_,_},Val,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
	if is_integer(Val) ->
			gl:uniform1i(Loc,Val);
		true ->
			gl:uniform1f(Loc,Val)
	end,
    ok;
shader_uniform({uniform,{image,Unit},Name,_,_},{_,Id},Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:activeTexture(?GL_TEXTURE0 + Unit),
    TxId = wings_image:txid(Id),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:uniform1i(Loc, Unit),
    ok;
shader_uniform({auv,{auv_bg,Unit}},_Opts,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog, "auv_bg"),
    gl:activeTexture(?GL_TEXTURE0),
    gl:bindTexture(?GL_TEXTURE_2D, Conf#sh_conf.fbo_r),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    gl:uniform1i(Loc, Unit),
    no_opt;
shader_uniform({auv,auv_texsz},_Opts,Conf = #sh_conf{texsz={W,H}}) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,"auv_texsz"),
    gl:uniform2f(Loc,float(W),float(H)),
    no_opt;
shader_uniform({auv,{auv_send_texture,_,_}},_Val,_Conf) ->
    ok;
shader_uniform({auv,auv_bbpos3d},_Opts,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,"auv_bbpos3d"),
    [{MinX,MinY,MinZ},{MaxX,MaxY,MaxZ}] = (Conf#sh_conf.ts)#ts.bb,
    gl:uniform3f(Loc,MinX,MinY,MinZ),
    gl:uniform3f(Loc+1,MaxX,MaxY,MaxZ),
    no_opt;
shader_uniform({auv,What},_Opts,_Conf) ->
    {keep, What}.

%% Per Face or per Chart uniforms
sh_uniforms([auv_bbpos2d|R],Chart=#chart{bb_uv=BB},Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,"auv_bbpos2d"),
    [{MinX,MinY,_},{MaxX,MaxY,_}] = BB,
    gl:uniform2f(Loc,MinX,MinY),
    gl:uniform2f(Loc+1,MaxX,MaxY),
    sh_uniforms(R,Chart,Conf);
sh_uniforms([_|R],Chart,Conf) ->
    sh_uniforms(R,Chart,Conf);
sh_uniforms([],_,_) ->
    [].

disable_tex_units(Unit) when Unit > 0 ->
    gl:activeTexture(?GL_TEXTURE0 + Unit-1),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    disable_tex_units(Unit-1);
disable_tex_units(_) -> ok.

delete_shaders(List) ->
    foreach(fun({_,Prog}) -> gl:deleteProgram(Prog) end, List).

get_requirements(Shaders) ->
    lists:foldl(fun(#sh{reqs=List},Acc) ->
			List ++ Acc
		end, [], Shaders).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Shader loading/handling 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shaders() ->
    case have_shaders() of
	true -> load_shaders_cfg();
	false -> []
    end.

have_shaders() ->
    wings_gl:support_shaders() andalso wings_gl:is_ext('GL_EXT_framebuffer_object').

load_shaders_cfg() ->
    Path  = filename:dirname(code:which(?MODULE)),
    Files = filelib:wildcard("wpc_*.auv", Path),
    lists:keysort(#sh.name, load_configs(Files,Path, [])).

load_configs([Name|Fs], Path, Acc) ->
    File = filename:join(Path,Name),
    case file:consult(File) of
	{ok,Info} -> 
	    Id = list_to_atom(filename:basename(Name,".auv")++"_auv"),
	    Sh = #sh{file=File,id=Id},
	    load_configs(Fs,Path,parse_sh_info(Info,Sh,1,Acc));
	Other ->
	    io:format("AUV: Couldn't load ~p ~n",[File]),
	    io:format("     Error: ~p ~n",[Other]),
	    load_configs(Fs,Path,Acc)
    end;
load_configs([],_Path,Acc) ->
    Acc.

parse_sh_info([{name,Name}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{name=Name},NI,Acc);
parse_sh_info([{vertex_shader,Name}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{vs=Name},NI, Acc);
parse_sh_info([{fragment_shader,Name}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{fs=Name},NI, Acc);
parse_sh_info([{requires,List}|Opts],Sh,NI,Acc) when is_list(List) ->
    parse_sh_info(Opts, Sh#sh{reqs=List},NI, Acc);
parse_sh_info([{preview,Opt}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{preview=(Opt=/=no)},NI, Acc);
parse_sh_info([{auv,auv_bg}|Opts],Sh,NI,Acc) ->
    What = {auv,{auv_bg,0}},
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args]},NI,Acc);
parse_sh_info([What={auv,auv_texsz}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args]},NI,Acc);
parse_sh_info([What={auv,auv_bbpos2d}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args]},NI,Acc);
parse_sh_info([What={auv,auv_bbpos3d}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args]},NI,Acc);
parse_sh_info([What={auv,{auv_send_texture,L,Def}}|Opts],Sh,NI,Acc) 
  when is_list(L) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI,Acc);
parse_sh_info([What={uniform,color,_,Def={_,_,_,_},_}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI,Acc);
parse_sh_info([What={uniform,float,_,Def,_}|Opts],Sh,NI,Acc)
  when is_number(Def) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI,Acc);
parse_sh_info([What={uniform,bool,_,Def,_}|Opts],Sh,NI,Acc) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI,Acc);
parse_sh_info([{uniform,image,Id,Def0,Str}|Opts],Sh,NI,Acc) ->
    %% default value must to be an image valid. It can be a file name or an internal
    %% image name. Otherwise, it will be created an background image will be used
    Def = load_image(Id, Def0),
    What = {uniform,{image,NI},Id,Def,Str},
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI+1,Acc);
parse_sh_info([What={uniform,{slider,F,T},_,Def,_}|Opts],Sh,NI,Acc) 
  when is_number(F),is_number(T),is_number(Def) ->
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],def=[Def|Sh#sh.def]},
		  NI,Acc);
parse_sh_info([What={uniform,menu,_,DefKey,List}|Opts],Sh,NI,Acc) ->
    case lists:keysearch(DefKey,1, List) of
	{value, {_,Def}} -> 
	    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args],
				      def=[Def|Sh#sh.def]},NI,Acc);
	false -> 
	    io:format("AUV: ~p Bad default value ignored menu ~p ~n",
		      [Sh#sh.file,What]),
	    parse_sh_info(Opts,Sh,NI,Acc)
    end;
parse_sh_info([_Error|Opts],Sh,NI,Acc) ->
    io:format("AUV: In ~p Unknown shader opt ignored ~p",
	      [Sh#sh.file,_Error]),
    parse_sh_info(Opts,Sh,NI,Acc);
parse_sh_info([],Sh,NI,Acc) ->
    %% Verify shader here
    [Sh#sh{tex_units=NI}|Acc].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shader compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_shaders(Passes, Available) ->
    foldl(fun({{shader,Id},_},Acc) -> 
		  case lists:keysearch(Id,1,Acc) of
		      {value, _} -> Acc; %  Already compiled
		      false ->
			  compile_shader(Id,lists:keysearch(Id,#sh.id,Available),Acc)
		  end;
	     (_NormalPass,Acc) -> 
		  Acc
	  end, [], Passes).
		   
compile_shader(Id, {value,#sh{name=Name,vs=VsF,fs=FsF}}, Acc) ->
    try
	Vs = wings_gl:compile(vertex, read_file(VsF)),
        Fs = wings_gl:compile(fragment, read_file(FsF)),
        Prog = wings_gl:link_prog([Vs,Fs]),
	%% io:format("AUV: Shader ´~s´ ok~n", [Name]),
	[{Id,Prog}|Acc]
    catch throw:What ->
	    io:format("AUV: Error ~p ~s ~n",[Name, What]),
	    Acc;
	_:Err:Stack ->
	    io:format("AUV: Internal Error ~s in~n ~p~n",[Err,Stack]),
	    Acc
    end;
compile_shader(Id, false, Acc) ->
    io:format("AUV: Error did not find shader ~p ~n",[Id]),
    Acc.

read_file(Name) ->
    Path = filename:dirname(code:which(?MODULE)),
    File = filename:join(Path,Name),
    case file:read_file(File) of
	{ok, Bin} -> Bin;
	_ -> throw("Couldn't read file: " ++ File)
    end.
