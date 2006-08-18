%%
%%  auv_texture.erl --
%%
%%     Render and capture a texture.
%%
%%  Copyright (c) 2002-2006 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: auv_texture.erl,v 1.37 2006/08/14 08:45:19 dgud Exp $
%%

-module(auv_texture).
-export([get_texture/1, get_texture/2, draw_options/0]).

-compile(export_all).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-define(ERROR, error(?LINE)).
-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2, reverse/1, sort/1, foldl/3, min/1,max/1]).
-import(auv_segment, [map_vertex/2]).

-define(OPT_BG, [{type_sel,color},{undefined,ignore},{1.0,1.0,1.0}]).
-define(OPT_EDGES, [all_edges,{0.0,0.0,0.0}, 1.0, false]).
-define(OPT_FACES, [texture]).
-define(OPT_LIGHT, [64]).

-record(opt, {texsz = {512,512},   %% Texture size
	      no_renderers = 4,
	      renderers = [{auv_background, ?OPT_BG},
			   {auv_edges, [all_edges]}]
	     }).

-record(sh, {id=ignore,
	     name="Unnamed",   %% Shader menu entry
	     file="",
	     vs = "",          %% Vertex shader
	     fs = "",          %% Fragment shader
	     tex_units = 1,    %% No of texture units used
	     reqs = [],        %% Requirements: normals and/or binormals
	     args = [],        %% Arguments
	     def  = []         %% Gui Strings
	    }).

-record(sh_conf, {texsz,      % More shader options
		  fbo_r,      % Fbo read buffer
		  fbo_w,      % Fbo write buffer
		  fbo_d,      % Clean up fbo
		  prog,       % Shader Id
		  ts}).       % Shader data

-record(ts,         % What              Type
	{charts,    % #chart{}          (list)
         uv,        % UV postions       (binary)
	 pos,       % Real 3D position  (binary)
 	 n,         % Normal            (binary) Optional
	 bi,        % BiNormal          (binary) Optional
	 bb,        % BoundingBox 3D pos
	 uvc,       % Previous uv or vertex color (binary)
	 uvc_mode   % material (uv) or vertex
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
	 mat,       % material 
	 id}).      % Face Id

%% Menu

draw_options() ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = max([min([4096,MaxTxs0]),256]),
    Prefs = get_pref(tx_prefs, pref_to_list(#opt{})),
    TexSz = proplists:get_value(texsz, Prefs, 512),
    Shaders = shaders(),
    Qs = [{hframe,[{menu,gen_tx_sizes(MaxTxs,[]),TexSz,
		    [{key,texsz}]}],
	   [{title,?__(1,"Size")}]},
	  {vframe, render_passes(Prefs,Shaders), [{title,?__(2,"Render")}]},
	  {hframe, [{button,?__(4,"New Pass"),done,[{key,add_shader}]},
		    {button,?__(5,"Delete Unused Pass"),done,[{key,del_shader}]}]}
	 ],
    
    wings_ask:dialog(?__(3,"Draw Options"), Qs,
		     fun(Options) ->
			     {{changed,New},Opt} = list_to_prefs(Options),
			     set_pref([{tx_prefs, pref_to_list(Opt)}]),
			     if New -> 
				     {auv,{draw_options,restart}};
				true  ->
				     {auv,{draw_options,{Opt,Shaders}}}
			     end
		     end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menu handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_passes(Prefs,Shaders) ->
    %% Ugly count of shaders
    NoOfPasses = lists:max([length([ok||{{auv_pass,_},_}<- Prefs])-1,1]), 
    Menu = renderers(Shaders),
    Background = 
	{hframe, 
	 [{label, integer_to_list(0) ++ ": "},
	  {menu,[{?__(1,"Background"), auv_background}],auv_background,
	   [{key,{auv_pass,0}},layout]}, 
	  {value, get_def(Prefs,auv_opt,0), store_opt(0)},
	  {button,?__(2,"Options"),keep,[option_hook(0,background(),[]),
					 drop_flags(0)]}],[]},
    Other = [{hframe, 
	      [{label, integer_to_list(Id) ++ ": "},
	       {menu,Menu,default_menu(Id,Prefs),
		[{key,{auv_pass,Id}},layout,pass_hook(Id)]}, 
	       {value, get_def(Prefs,auv_opt,Id), store_opt(Id)},
	       {button,?__(3,"Options"),keep,
		[option_hook(Id,Menu,Shaders),
		 drop_flags(Id)]}],
	      []} 
	     || Id <- lists:seq(1,NoOfPasses)],
    [Background|Other].

default_menu(Pass,Prefs) -> 
    case get_def(Prefs, auv_pass, Pass) of
	ignore -> default_menu(Pass);
	Val -> Val
    end.
	    
default_menu(1) -> auv_edges; 
default_menu(_) -> ignore.

get_def(List, What, Id) ->
    case proplists:get_value({What,Id}, List) of
	undefined when What == auv_pass -> ignore;
	undefined when What == auv_opt  -> [];
	Val -> Val
    end.

background() ->
    [{?__(1,"Background"), auv_background}].
renderers(Shaders) ->
    Menu0 = [{"*"++Name++"*", {shader,Id}} || 
		#sh{name=Name,id=Id} <- Shaders],
    Menu1 = Menu0,
%%     Menu1 = case Shaders of 
%% 		[] -> Menu0;
%% 		_ -> [{?__(4, "Lights"), auv_light}|Menu0]
%% 	    end,
    [{?__(1,"None"), ignore},
     {?__(2,"Draw Edges"),auv_edges},
     {?__(3,"Draw Faces"),auv_faces}| Menu1
    ].

options(auv_background, [{type_sel,Type},{Image,_},Color],_) ->
    [{hradio,[{?__(1,"Image"),image},{?__(2,"Color"),color}],
      Type,[{key,type_sel},layout]},
     {hframe,[{label,?__(1,"Image")},image_selector(Image)],
      [is_enabled(image)]},
     {hframe,[{label,?__(2,"Color")},{color,fix(Color,have_fbo())}],
      [is_enabled(color)]}];
options(auv_background, _Bad,Sh) ->  
    options(auv_background, ?OPT_BG,Sh);
options(auv_edges,[Type,Color,Size,UseMat],_) ->
    [{vradio,[{?__(3,"Draw All Edges"),all_edges},
	      {?__(4,"Draw Border Edges"), border_edges}], 
      Type, []},
     {hframe,[{label,?__(5,"Edge Color:")},{color, Color}]},
     {hframe,[{label,?__(6,"Edge Width:")},{text,Size,[{range,{0.0,100.0}}]}]},
     {?__(7,"Use face material (on border edges)"), UseMat}
    ];
options(auv_edges,_,Sh) -> options(auv_edges,?OPT_EDGES,Sh);
options(auv_faces,[Type],_) ->
    [{vradio,[{?__(8,"Use Material Colors"),materials},
	      {?__(9,"Use (previous) Texture/Vertex colors"), texture}],
      Type, []}];
options(auv_faces,_,Sh) -> options(auv_faces,?OPT_FACES,Sh);
options(auv_light,[Size],_Sh) -> 
    [{hframe,[{label,?__(10,"No Lights:")},{text,Size,[{range,{4,100}}]}]}];
options(auv_light,_,Sh) -> options(auv_light,?OPT_LIGHT,Sh);
options({shader,Id},Vals,Sh) ->
    {value,Shader} = lists:keysearch(Id,#sh.id,Sh),
    shader_options(Shader,Vals);
options(Command,Vals,_) ->
    io:format("~p: ~p~n",[Command, Vals]),
    exit(unknown_default).

shader_options(#sh{args=Args,def=Defs,file=File}, Vals) ->
    case shader_menu(Args,reverse(Vals),[]) of
	{failed,_} -> 
	    case shader_menu(Args,Defs,[]) of
		{failed,What} -> 
		    io:format("AUV: Bad default value ~p in ~p~n",
			      [What, File]),
		    [];
		Menues -> Menues
	    end;
	Menues -> Menues
    end.
shader_menu([{uniform,color,_,_,Label}|As],[Col={_,_,_,_}|Vs],Acc) ->
    Menu = {hframe, [{label,Label},{color,Col}]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{uniform,float,_,_,Label}|As],[Def|Vs],Acc) 
  when is_number(Def) ->
    Menu = {hframe,[{label,Label},
		    {text,Def,[{range,{'-infinity',infinity}}]}]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{uniform,bool,_,_,Label}|As],[Def|Vs],Acc) 
  when is_boolean(Def) ->
    Menu = {Label, Def},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{uniform,{slider,From,To},_,_,Label}|As],[Def|Vs],Acc) 
  when is_number(Def) ->
    Menu = {hframe,[{label,Label},
		    {slider,{text,Def,[{range,{From,To}},{width,5}]}}]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{uniform,{image,_},_,_Def,Label}|As],[Def|Vs],Acc) ->
    Image = case Def of {Im,_} -> Im; _ -> Def end,
    Menu = {hframe,[{label,Label},image_selector(Image)]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{uniform,menu,_,_,Labels}|As],[Def|Vs],Acc) ->
    Menu = {menu,Labels,Def,[]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{auv,{auv_send_texture,Label,_}}|As],[Def0|Vs],Acc) ->
    case Def0 of
	{auv_send_texture, Def} -> Def;
	Def -> Def
    end,
    Menu = {Label, Def, [{key,auv_send_texture}]},
    shader_menu(As,Vs,[Menu|Acc]);
shader_menu([{auv,_Skip}|As],Vs,Acc) ->
    shader_menu(As,Vs,Acc);
shader_menu([What|_],[Def|_],_) ->
    {failed,{What,value,Def}};
shader_menu([What|_],[],_) ->
    {failed,What};
shader_menu([],_,Acc) -> 
    Acc.

image_selector(Default) ->
    Is = wings_image:images(),
    Menu = [{Name,{Name,TexId}} || {TexId, #e3d_image{name=Name}} <- Is],
    case lists:keysearch(Default,1,Menu) of 
	{value,{_,What}} -> Def = What;
	_ -> case Menu of 
		 [{_,Def}|_] -> Def;
		 _ -> Def = void
	     end
    end,
    {menu,Menu,Def,[layout]}.

is_enabled(Type) ->
    {hook, fun(is_minimized, {_Var,_I,Store}) -> 
		   case gb_trees:get(type_sel, Store) of
		       Type ->   false;
		       _Other -> true
		   end;
	      (_,_) -> void
	   end}.

option_hook(Id,Renderers,Shaders) ->
    {hook, fun(is_disabled,{_Var,_I,Sto}) ->
		   gb_trees:get({auv_pass,Id}, Sto) == ignore;
	      (is_minimized, _) ->
		   false;
	      (update,{_Var,_I,_B,Sto}) ->
		   Name = gb_trees:get({auv_pass,Id},Sto),
		   render_option_dialog(Id,renderer(Name,Renderers),
					Shaders,Sto);
	      (_,_) -> void
	   end}.

pass_hook(Id) ->
    {hook, fun(update,{Var,_I,B,Sto0}) ->
		   VarE = {auv_opt,Id},
		   Sto1 = gb_trees:enter(VarE,[],Sto0),
		   {store, gb_trees:enter(Var,B,Sto1)};
	      (_,_) -> void
	   end}.

renderer(Id,[Renderer={_,Id}|_R]) ->  Renderer;
renderer(Id,[_|R]) ->  renderer(Id,R).

render_option_dialog(Id,{StrName,Name},Shaders,Sto) ->
    Fun = render_option_fun(wings_wm:this()),
    Opt = gb_trees:get({auv_opt,Id},Sto),
    wings_ask:dialog(StrName,options(Name,Opt,Shaders),Fun).

render_option_fun(Parent) ->
    fun(What) -> wpa:drop(Parent, {render_opt, What}) end.

store_opt(Id) ->
    [{key,{auv_opt,Id}},
     {hook, fun(update,{Var,_I,_B={render_opt,Opt},Sto}) -> 
		    {store, gb_trees:enter(Var,Opt,Sto)};
	       (_,_) -> void
	    end}].
drop_flags(Id) ->
    {drop_flags, [{index,-1}|store_opt(Id)]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture creation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_texture(St) ->    
    Ops = list_to_prefs(get_pref(tx_prefs, list_to_prefs(#opt{}))),
    get_pref(St, Ops).

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

render_image(Geom = #ts{uv=UVpos,pos=Pos,n=Ns,bi=BiNs,uvc=Uvc,uvc_mode=Mode}, 
	     Passes,#opt{texsz={TexW,TexH}},Reqs) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    
    Current = wings_wm:viewport(),
    UsingFbo = setup_fbo(TexW,TexH),
    {W0,H0} = if not UsingFbo -> wings_wm:top_size();
		 true -> {TexW,TexH}
	      end,
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
%%    io:format("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    %% Load Pointers
    gl:vertexPointer(2, ?GL_FLOAT, 0, UVpos),
    case lists:member(normal, Reqs) of
	true -> gl:normalPointer(?GL_FLOAT, 0, Ns);
	false -> ignore
    end,
    case Mode of
	vertex -> gl:colorPointer(3,?GL_FLOAT,0,Uvc);
	_Other -> (catch gl:clientActiveTexture(?GL_TEXTURE0)),
		  gl:texCoordPointer(2,?GL_FLOAT,0,Uvc)
    end,
    case have_shaders() of
	false -> ignore;
	true  -> 
 	    gl:clientActiveTexture(?GL_TEXTURE1),
 	    gl:texCoordPointer(3,?GL_FLOAT,0,Pos),
 	    gl:clientActiveTexture(?GL_TEXTURE0)
    end,
    case lists:member(binormal, Reqs) of
	true -> 	    
	    gl:clientActiveTexture(?GL_TEXTURE2),
	    gl:texCoordPointer(3,?GL_FLOAT,0,BiNs),
	    gl:clientActiveTexture(?GL_TEXTURE0);
	false ->
	    ignore
    end,

    try 
        Dl = fun() ->
		     foreach(fun(Pass) -> 
				     if not UsingFbo -> 
					     Pass(Geom,undefined);
					true ->
					     fill_bg_tex(UsingFbo),
					     Pass(Geom,UsingFbo)
				     end
			     end,
			     Passes) 
	     end,
	ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, UsingFbo,[]),
	ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
	if not UsingFbo -> 
		#e3d_image{image=ImageBin,width=TexW,height=TexH};
	   true -> 
		#e3d_image{image=ImageBin,width=TexW,height=TexH,
			   type=r8g8b8a8,bytes_pp=4}
	end
    catch _:What ->
	    Where = erlang:get_stacktrace(),
	    exit({What,Where})
    after 
	case UsingFbo of 
	  false -> ignore;
	  #sh_conf{fbo_d=DeleteMe} -> DeleteMe()
      end,
      set_viewport(Current),
      gl:readBuffer(?GL_BACK),
      gl:popAttrib(),
      gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
      ?ERROR    
    end.

%%%%%%%%% FBO stuff
have_fbo() -> wings_gl:is_ext('GL_EXT_framebuffer_object').

setup_fbo(W,H) ->
    case have_fbo() of
	false -> false;
	true -> setup_fbo2(W,H) 
    end.

setup_fbo2(W,H) ->
    [FB] = gl:genFramebuffersEXT(1),
    [Col1,Col2] = gl:genTextures(2),
    [Depth] = gl:genRenderbuffersEXT(1),
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, FB),
    %% Init color texture
    gl:bindTexture(?GL_TEXTURE_2D, Col1),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA8, W, H, 0,
		  ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),
    gl:framebufferTexture2DEXT(?GL_FRAMEBUFFER_EXT,
			       ?GL_COLOR_ATTACHMENT0_EXT,
			       ?GL_TEXTURE_2D, Col1, 0),
    
    gl:bindTexture(?GL_TEXTURE_2D, Col2),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA8, W, H, 0,
		  ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),
    gl:framebufferTexture2DEXT(?GL_FRAMEBUFFER_EXT,
			       ?GL_COLOR_ATTACHMENT1_EXT,
			       ?GL_TEXTURE_2D, Col2, 0),
    
    %% Init depth texture
    gl:bindRenderbufferEXT(?GL_RENDERBUFFER_EXT, Depth),
    gl:renderbufferStorageEXT(?GL_RENDERBUFFER_EXT,
			      ?GL_DEPTH_COMPONENT24, W, H),
    gl:framebufferRenderbufferEXT(?GL_FRAMEBUFFER_EXT,
				  ?GL_DEPTH_ATTACHMENT_EXT,
				  ?GL_RENDERBUFFER_EXT, Depth),
    Delete = 
	fun() ->
		gl:framebufferTexture2DEXT(?GL_FRAMEBUFFER_EXT,
					   ?GL_COLOR_ATTACHMENT0_EXT,
					   ?GL_TEXTURE_2D,0,0),
		gl:deleteTextures(2,[Col1,Col2]),
		gl:framebufferRenderbufferEXT(?GL_FRAMEBUFFER_EXT,
					      ?GL_DEPTH_ATTACHMENT_EXT,
					      ?GL_RENDERBUFFER_EXT, 0),
		gl:deleteRenderbuffersEXT(1,[Depth]),
		gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
		gl:deleteFramebuffersEXT(1,[FB])
	end,
    case check_fbo_status(FB) of
	false -> 		    
	    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0), 
	    gl:deleteFramebuffersEXT(1,[FB]),
	    false;
	_ ->	    
	    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, FB),
	    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
	    #sh_conf{texsz={W,H},fbo_r=Col2,fbo_w=Col1,
		     fbo_d=Delete}
    end.
	
error(Line) ->
    case wings_gl:error_string(gl:getError()) of 
	no_error -> ok; 
	Err -> io:format("~p: ~p ~n",[Line,Err])
    end.

draw_texture_square() ->
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0), gl:vertex2f(0,0),
    gl:texCoord2f(1,0), gl:vertex2f(1,0),
    gl:texCoord2f(1,1), gl:vertex2f(1,1),
    gl:texCoord2f(0,1), gl:vertex2f(0,1),
    gl:'end'().


fill_bg_tex(#sh_conf{fbo_w=Prev}) ->
    gl:drawBuffer(?GL_COLOR_ATTACHMENT1_EXT),
    gl:bindTexture(?GL_TEXTURE_2D, Prev),
    gl:enable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND),
    draw_texture_square(),
    gl:disable(?GL_TEXTURE_2D),
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
    ok.
	    
get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, DL, UsingFbo, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
%%    gl:shadeModel(?GL_SMOOTH),
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
    Mem = sdl_util:alloc(W*H*Sz, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0,0,W,H,Type,?GL_UNSIGNED_BYTE,Mem),
    ImageBin = sdl_util:getBin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, DL, UsingFbo, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Dl, UsingFbo, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Dl, UsingFbo, ImageAcc);
get_texture(_,_,_,_,_,_,_,ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD),
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

check_fbo_status(FB) ->
    case gl:checkFramebufferStatusEXT(?GL_FRAMEBUFFER_EXT) of
	?GL_FRAMEBUFFER_COMPLETE_EXT ->
	    FB;
	?GL_FRAMEBUFFER_UNSUPPORTED_EXT ->
	    io:format("GL_FRAMEBUFFER_UNSUPPORTED_EXT~n",[]),
	    false;
	?GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT    ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT~n",[]),
	    false
    end.

get_pref(Key, Def) ->
    wpa:pref_get(autouv, Key, Def).
set_pref(KeyVals) ->
    wpa:pref_set(autouv, KeyVals).

pref_to_list(#opt{texsz={TexSz,_TexSz}, no_renderers=NoR, 
		  renderers=[{auv_background,Bg}|Rs]}) ->
    [{texsz, TexSz},{{auv_pass,0},auv_background},{{auv_opt,0},Bg}|
     r2list(Rs,1,NoR)].

r2list([{Type, Opts}|Rest], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, Type},{{auv_opt,Id},Opts}|r2list(Rest,Id+1,Max)];
r2list([], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, ignore},{{auv_opt,Id},[]}|r2list([],Id+1,Max)];
r2list([], _Id, _Max)  -> [].

list_to_prefs([{texsz, TexSz}|Rest]) ->    
    case reverse(Rest) of
	[{del_shader, true},{add_shader,_}|ROpts] -> 
	    LR = listOfRenders(reverse(ROpts),delete,[]),
	    Changed = true;
	[{del_shader, false},{add_shader, true}|ROpts] -> 
	    LR = listOfRenders(reverse(ROpts),keep,[]) ++ [{ignore, none}],
	    Changed = true;
	[{del_shader, false},{add_shader, false}|ROpts] ->
	    LR = listOfRenders(reverse(ROpts),keep,[]),
	    Changed = false
    end,
    {{changed, Changed},
     #opt{texsz={TexSz,TexSz},no_renderers=length(LR),renderers=LR}}.

listOfRenders([{{auv_pass,_},ignore},_|Rest],delete,Acc) ->
    listOfRenders(Rest,delete,Acc);
listOfRenders([{{auv_pass,_},ignore},_|Rest],keep,Acc) ->
    listOfRenders(Rest,keep,[{ignore,[]}|Acc]);
listOfRenders([{{auv_pass,_},Type},{{auv_opt,_},Opts}|Rest],Op,Acc) ->
    listOfRenders(Rest,Op,[{Type,Opts}|Acc]);
listOfRenders([],_,Acc) -> reverse(Acc).
    
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

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data setup 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup(St = #st{bb=#uvstate{id=RId, st=#st{mat=Mat,shapes=Sh0},
			   orig_st=#st{mat=OrigMat,shapes=OrigSh}}},
      Reqs) ->
    We   = gb_trees:get(RId,Sh0),
    Orig = gb_trees:get(RId,OrigSh),
    Mats = merge_mats(gb_trees:to_list(OrigMat),Mat),
    {Charts,{_Cnt,UVpos,Vpos,Ns,Ts,BB,Uvc}} = setup_charts(St,We,Orig,Mats,Reqs),
    #ts{charts=Charts,
	uv =to_bin(UVpos,uv),
	pos=to_bin(Vpos,pos),
	n  =to_bin(Ns,pos),
	bi =to_bin(Ts,pos),
	bb = BB,
	uvc=to_bin(Uvc,Orig#we.mode), 
	uvc_mode=Orig#we.mode}.

setup_charts(#st{shapes=Cs0,selmode=Mode,sel=Sel},We,OrigWe,Mats,Reqs) ->
    Ns = case lists:member(normal,Reqs) orelse lists:member(binormal,Reqs) of
	     true -> setup_normals(We);
	     false -> []
	 end,
    Start = {0,[],[],[],[],[],[]}, %% {UvPos,3dPos,Normal,Tangent,Uvc}
    Mat = fun(Face) -> get_material(Face,We,OrigWe,Mats) end,
    Shapes = if Sel =:= [] -> 
		     gb_trees:values(Cs0);
		Mode =:= body -> 
		     [gb_trees:get(Id,Cs0) || {Id,_} <- Sel];
		true -> 
		     gb_trees:values(Cs0)
	     end,
    Setup = fun(Ch,Acc) -> setup_chart(Ch,Mat,Ns,Reqs,We,OrigWe,Acc) end,
    lists:mapfoldl(Setup, Start, Shapes).

setup_chart(Uv = #we{id=Id},Mat,Ns,Reqs,WWe,OWe,State0) ->
    OEs0 = outer_verts(Uv), 
    BB = [],
    {Fs,{OEs,UvBB,State}}  = 
	create_faces(Uv,WWe,OWe,Ns,Mat,Reqs,{OEs0,BB,State0}),
    {#chart{id=Id,fs=Fs,oes=OEs,bb_uv=UvBB},State}.

create_faces(We = #we{vp=Vtab,name=#ch{vmap=Vmap}},
	     RWe = #we{vp=Vt3d},OWe=#we{mode=OldMode},
	     NTab,GetMat,Reqs,State) ->
    Fs = wings_we:visible(We),
    C=fun(Face,{OEs,UvBB,{Cnt,UVpos,Vpos,Ns,Ts,PosBB,Uvc}}) ->
	      Vs0 = wings_face:vertices_ccw(Face,We),
	      UVcoords = [gb_trees:get(V, Vtab) || V <- Vs0],
	      Coords   = [gb_trees:get(map_vertex(V,Vmap),Vt3d) 
			  || V <- Vs0],
	      Normals = if
			    NTab=:= [] -> [];
			    true -> fix_normals(Vs0,Vmap,
						wings_face:vinfo_ccw(Face,RWe),
						gb_trees:get(Face,NTab))
			end,
	      OldUvc  = fix_uvc(Vs0,Face,OWe,Vmap,OldMode),
	      Len = length(Vs0),
	      FaceVs = lists:seq(0, Len-1),
	      Vs = case Len of
		       3 -> FaceVs;
		       Len -> triangulate(FaceVs,UVcoords)
		   end,
%	      io:format("Face ~p Normals ~p~n", [Face,Normals]),
	      Tangents = fix_tang_vecs(Vs,Coords,UVcoords,Normals,Reqs), 
	      Indx = fun(I) -> [V+Cnt || V <- I] end,
	      Mat  = GetMat(Face),
	      {#fs{vs=Indx(Vs),vse=Indx(FaceVs),mat=Mat,id=Face},
	       {map_oes(OEs,Vs0,Cnt+Len-1,Face,Mat),
		e3d_vec:bounding_box(UvBB ++ UVcoords),
		{Cnt+Len,
		 UVcoords ++ UVpos,
		 Coords   ++ Vpos,
		 Normals  ++ Ns,
		 Tangents ++ Ts,
		 e3d_vec:bounding_box(PosBB ++ Coords),
		 OldUvc   ++ Uvc}}}
      end,
    lists:mapfoldl(C, State, Fs).

triangulate(FaceVs,Vcoords) ->
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = e3d_mesh:triangulate_face(E3dface, Vcoords),
    lists:append([FVs || #e3d_face{vs=FVs} <- T3dfaces]).

map_oes([[A,B,Face]|OEs],Vs0,Cnt,Face,Mat) ->
    MA = member(A,Vs0,Cnt),
    MB = member(B,Vs0,Cnt),
    [[MA,MB,Mat]|map_oes(OEs,Vs0,Cnt,Face,Mat)];
map_oes([Other|OEs],Vs0,Cnt,Face,Mat) ->
    [Other|map_oes(OEs,Vs0,Cnt,Face,Mat)];
map_oes([],_,_,_,_) -> [].

member(Val,[Val|_],Pos) -> Pos;
member(Val,[_|R],Pos) -> member(Val,R,Pos-1).

fix_normals(Vs,Vmap,VsI,Ns0) -> %% can be different order 
    fix_normals(Vs,n_zip(VsI,Ns0),Vmap).  
fix_normals([V|R],Ns,Vmap) ->
    [find(map_vertex(V,Vmap),Ns)|fix_normals(R,Ns,Vmap)];
fix_normals([],_,_) -> [].

fix_tang_vecs(_,_,_,[],_) -> [];
fix_tang_vecs(_Vs,Coords,_UVCoords,Normals,Reqs) ->
    case lists:member(binormal,Reqs) of
	true -> % Not tested enough, but I think it's better 
% 	    All = fix_tang_vecs1(Vs,Coords,UVCoords,Normals,[]),
% 	    R1 = fix_tang_vecs2(sort(All),[]),
	    R2 = binormals(Normals,e3d_vec:normal(Coords)),
%	    io:format("~p~n~p~n~n",[R1,R2]),
	    R2;
	false ->
	    []
    end.

binormals(Normals,FaceNormal) ->
    Q = e3d_q:rotate_s_to_t({0.0,0.0,1.0},FaceNormal),
    R = fun(N) -> 
		D = e3d_q:rotate_s_to_t(FaceNormal,N),
%		io:format("Z-N ~p~nN-n ~p~n",[Q,D]),
		R = e3d_q:mul(D,Q),
		Bi = e3d_q:vec_rotate({0.0,1.0,0.0},R),
%		io:format("~p ~p~n",[R,Bi]),
		e3d_vec:norm(e3d_vec:cross(Bi,N))
	end,
    [R(N) || N <- Normals].

%%% This code should be inside wings somewhere its copied 
%%% from wpc_opengl.erl
fix_tang_vecs1([A,B,C|Vs],Coords,UVCoords,Normals,Acc) ->
    {UV1x,UV1y,_} = lists:nth(A+1,UVCoords),
    {UV2x,UV2y,_} = lists:nth(B+1,UVCoords),
    {UV3x,UV3y,_} = lists:nth(C+1,UVCoords),
    V1 = lists:nth(A+1,Coords),N1 = lists:nth(A+1,Coords),
    V2 = lists:nth(B+1,Coords),N2 = lists:nth(B+1,Coords),
    V3 = lists:nth(C+1,Coords),N3 = lists:nth(C+1,Coords),
    TBN1 = calcTS(V3,V1,V2,UV3x,UV3y,UV1x,UV1y,UV2x,UV2y,N1),
    TBN2 = calcTS(V1,V2,V3,UV1x,UV1y,UV2x,UV2y,UV3x,UV3y,N2),
    TBN3 = calcTS(V2,V3,V1,UV2x,UV2y,UV3x,UV3y,UV1x,UV1y,N3),
    fix_tang_vecs1(Vs,Coords,UVCoords,Normals,
		   [{A,TBN1},{B,TBN2},{C,TBN3}|Acc]);
fix_tang_vecs1([],_Coords,_UVCoords,_Normals,Acc) -> 
    Acc.
   
calcTS(V1,V2,V3,S1,T1,S2,T2,S3,T3,N) ->
    Side1 = e3d_vec:sub(V1,V2),
    Side2 = e3d_vec:sub(V3,V2),
    DT1 = T1-T2,
    DT2 = T3-T2,
    Stan1 = e3d_vec:norm(e3d_vec:sub(e3d_vec:mul(Side1,DT2),
				     e3d_vec:mul(Side2,DT1))),	    
    DS1 = S1-S2,
    DS2 = S3-S2,
    Ttan1 = e3d_vec:norm(e3d_vec:sub(e3d_vec:mul(Side1,DS2),
				     e3d_vec:mul(Side2,DS1))),
    %% OK we have the 2 tangents but cross them again with normal
    %% so they are orthogonal
    Stan  = e3d_vec:norm(e3d_vec:cross(Ttan1,N)),
    Ttan  = e3d_vec:norm(e3d_vec:cross(Stan1,N)),
    check_coordsys(Stan,Ttan,N).

fix_tang_vecs2([{_,{S1,_,_}},{_,{S2,_,_}},{_,{S3,_,_}}],[]) ->
    [S1,S2,S3];
fix_tang_vecs2(List=[{Id,{_,_,N}}|_], Acc) ->
    {Stans,R} = get_same(Id, List,[]),
    T = e3d_vec:cross(e3d_vec:average(Stans),N),
    fix_tang_vecs2(R,[e3d_vec:norm(T)|Acc]);
fix_tang_vecs2([],Acc) ->
    lists:reverse(Acc).

get_same(Id, [{Id,{S,_,_}}|R],Acc) ->
    get_same(Id,R,[S|Acc]);
get_same(_,R,Acc) -> {Acc,R}.

check_coordsys(Stan,Ttan,N) ->
    Check = e3d_vec:dot(e3d_vec:cross(Stan,Ttan),N),
    if 
	Check < 0.0 -> {e3d_vec:mul(Stan,-1.0),e3d_vec:mul(Ttan,-1.0),N};
	true ->        {Stan,Ttan,N}
    end.

fix_uvc(Vs,Face,OWe,Vmap,Mode) ->
    try 
	Uvc = wings_face:vinfo_ccw(Face,OWe),
	fix_uvc1(Vs,Uvc,Vmap,Mode,[])
    catch error:_ ->
	    fix_uvc1(Vs,[],Vmap,Mode,[])
    end.
fix_uvc1([V|Vs],Uvc,Vmap,Mode, Acc) ->
    Val = case find(map_vertex(V,Vmap),Uvc) of
	      Color = {_,_,_} when Mode == vertex ->  Color;
	      _ when Mode == vertex -> {1.0,1.0,1.0};
	      Uv = {_,_} -> Uv;
	      _ -> {0.0,0.0}
	  end,
    fix_uvc1(Vs,Uvc,Vmap,Mode,[Val|Acc]);
fix_uvc1([],_,_,_,Acc) -> reverse(Acc).

find(V, [[V|Info]|_R]) -> Info;
find(V, [_|R]) -> find(V,R);
find(_, []) -> none.

n_zip([[V|_]|R1],[[_|N]|R2]) -> 
    [[V|N]|n_zip(R1,R2)];
n_zip([],[]) -> [].
    

fix(OK = {_,_,_}, false) -> OK;
fix(OK = {_,_,_,_}, true) -> OK;
fix({R,G,B,_}, false) -> {R,G,B};
fix({R,G,B}, true) -> {R,G,B,1.0}.
    
setup_normals(We = #we{fs=Ftab}) ->
    FN0	= [{Face,wings_face:normal(Face, We)} || Face <- gb_trees:keys(Ftab)],
    Ns = wings_we:normals(FN0, We),  %% gb_tree of {Face, [VInfo|Normal]}    
    gb_trees:from_orddict(sort(Ns)).

get_material(Face, We, OrigWe, Materials) ->
    Mat1 = wings_facemat:face(Face,We),
    Mat = try
	      case reverse(atom_to_list(Mat1)) of
		  "vua_" ++ _ -> 
		      try wings_facemat:face(Face,OrigWe)
		      catch _:_ -> Mat1
		      end;
		  _ ->
		      Mat1
	      end
	  catch _:_ ->
		  Mat1
	  end,
    gb_trees:get(Mat, Materials).

outer_verts(We = #we{es=Etab}) ->
    Fs = wings_we:visible(We),
    Outer = auv_util:outer_edges(Fs,We,false),    
    Verts = fun({Edge,Face}) -> 
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    [Va,Vb,Face]
	    end,
    lists:map(Verts, Outer).

%% Start with 64 bytes so that binary will be reference counted 
%% and not on the process heap spent hours debugging this.. :-(
to_bin(List, uv) -> to_bin3to2(List,[<<0:512>>]);
to_bin(List, pos) -> to_bin3(List,[<<0:512>>]);
to_bin(List, vertex) -> to_bin3(List,[<<0:512>>]);  %% Vertex colors
to_bin(List, material) -> to_bin2(List,[<<0:512>>]).  %% UV coords.

to_bin3([{A,B,C}|R],Acc) -> 
    to_bin3(R,[<<A:32/native-float,B:32/native-float,C:32/native-float>>|Acc]);
to_bin3([],Acc) -> list_to_binary(Acc).
to_bin2([{A,B}|R],Acc) -> 
    to_bin2(R,[<<A:32/native-float,B:32/native-float>>|Acc]);
to_bin2([],Acc) -> list_to_binary(Acc).
to_bin3to2([{A,B,_}|R],Acc) -> 
    to_bin3to2(R,[<<A:32/native-float,B:32/native-float>>|Acc]);
to_bin3to2([],Acc) -> list_to_binary(Acc).

%
% Workaround for ATI gl:'end' doesn't bite for line loop/strip..
vs_lines([A|R=[B|_]],Last) ->
    [A,B|vs_lines(R,Last)];
vs_lines([B],Last) ->
    [B,Last].

merge_mats([This={MatName,_Def}|R], Mats) ->
    case gb_trees:is_defined(MatName,Mats) of
	true ->
	    merge_mats(R,Mats);
	false ->
	    merge_mats(R,gb_trees:add(This, Mats))
    end;
merge_mats([],Mats) -> Mats.
	     
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
pass({auv_background,[{type_sel,color},_,Color]},_) ->  
    fun(_Geom,_) ->
	    {R,G,B,A} = fix(Color,true),
	    gl:clearColor(R,G,B,A),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT)
    end;
pass({auv_background,[{type_sel,image},{_Name,Id},Color]},_) ->
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
pass({auv_edges, [all_edges,Color,Width,_UseMat]},_) ->
    R=fun(#chart{fs=Fs}) ->
	      Draw = fun(#fs{vse=Vs}) ->
			     Patched = vs_lines(Vs,hd(Vs)),
			     gl:drawElements(?GL_LINES,length(Patched),
					     ?GL_UNSIGNED_INT,Patched)
%%                     Doesn't work
%% 			     gl:drawElements(?GL_LINE_LOOP,length(Vs),
%% 					     ?GL_UNSIGNED_INT,Vs) 
			     end,
	      foreach(Draw,Fs)
      end,
    fun(#ts{charts=Charts},_) ->  
	    gl:disable(?GL_DEPTH_TEST),	    
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    foreach(R, Charts),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, [border_edges,Color,Width,UseMat]},_) -> 
    R= fun(#chart{oes=Es}) when UseMat ->
	       Draw = fun([A,B,Mat]) ->
			      gl:color4fv(get_diffuse(Mat)),
			      gl:drawElements(?GL_LINES,2,?GL_UNSIGNED_INT,[A,B])
		      end,
	       foreach(Draw,Es);
	  (#chart{oes=Es0}) ->
	       Es = foldl(fun([A,B,_],Acc) -> [A,B|Acc] end, [], Es0),
	       gl:drawElements(?GL_LINES,length(Es),?GL_UNSIGNED_INT,Es)
       end,
    fun(#ts{charts=Charts},_) ->  
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:disable(?GL_DEPTH_TEST),
	    foreach(R, Charts),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, _},Sh) ->
    pass({auv_edges, ?OPT_EDGES},Sh);

pass({auv_faces, [Type]},_) ->
    fun(#ts{charts=Charts,uvc_mode=Mode},_) ->  
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),	   
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:clientActiveTexture(?GL_TEXTURE0),
	    R = case Type of
		    materials ->			
			fun(#fs{vs=Vs,mat=Mat}) ->
				gl:color4fv(get_diffuse(Mat)),
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end;
		    _ when Mode == vertex -> 
			gl:enableClientState(?GL_COLOR_ARRAY),
			fun(#fs{vs=Vs}) ->
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end;
		    _ -> 
			gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
			fun(#fs{vs=Vs,mat=Mat}) ->
				set_diffuse_tx(Mat),
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end
		end,
	    erase({?MODULE,use_tx}),
	    gl:disable(?GL_TEXTURE_2D),
	    foreach(fun(#chart{fs=Fs}) -> foreach(R,Fs) end,Charts),
	    gl:disable(?GL_TEXTURE_2D),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:disableClientState(?GL_COLOR_ARRAY),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY)
    end;
pass({auv_faces, _},Sh) ->
    pass({auv_faces,?OPT_FACES},Sh);
pass({auv_light, _},_Sh) ->
    Circle = fun(N, Y, R) ->
		     Delta = math:pi()*2 / N,
		     [{R*math:cos(I*Delta), Y, R*math:sin(I*Delta)} 
		      || I <- lists:seq(0, N-1)]
	     end,
    Sphere = fun(Ns,Nl) ->
		     Delta = math:pi() / Nl,
		     PosAndRads= [{math:cos(I*Delta), math:sin(I*Delta)} 
				  || I <- lists:seq(1, Nl-1)],
		     Circles = [Circle(Ns, Pos, Rad) || {Pos, Rad} <- PosAndRads],
		     lists:flatten(Circles)
	     end,
    %Lights = Sphere(8,4) ++ [{0.0,1.0,0.0}, {0.0,-1.0,0.0}],
    Ligths = [{1.0,0.0,0.0}],
    
    VsS = wings_gl:compile(vertex, read_file("light1.vs")),
    LP1 = wings_gl:link_prog([VsS]),
    io:format("Light Prog ~p ~n",[LP1]),

    fun(Ts = #ts{bb=BB=[Min,Max]},_) ->
	    Center = e3d_vec:average(BB),
	    Dist   = e3d_vec:dist(Min,Max),
	    PM = get_lightProjM(Dist),
	    render_lights(Ligths, PM, 10.0+Dist, Center,LP1,Ts)
    end;

pass({{shader,Id}, Opts},{Sh,Compiled}) ->
    shader_pass(lists:keysearch(Id,#sh.id,Sh),
		lists:keysearch(Id,1,Compiled),Opts);
pass({_R, _O},_) ->    
    io:format("AUV: ~p:~p: Unknown Render Pass (~p) or options (~p) ~n",
	      [?MODULE,?LINE,_R,_O]),
    ignore.

shader_pass(Shader={value,#sh{def=Def}},Prog,[]) when Def /= [] ->    
    shader_pass(Shader, Prog, reverse(Def));
shader_pass(_,false,_) ->    
    io:format("AUV: No shader program found skipped ~p~n", [?LINE]),
    ignore;
shader_pass(false,_,_) ->    
    io:format("AUV: Not shader found skipped ~p~n", [?LINE]),
    ignore;
shader_pass({value,#sh{args=Args,tex_units=TexUnits,reqs=Reqs}},
	    {value,{_,Prog}},Opts) ->
    fun(Ts = #ts{charts=Charts},Config) ->
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:useProgram(Prog),
	    try 
		Conf = Config#sh_conf{prog=Prog,ts=Ts},
		PerChartUniF = shader_uniforms(reverse(Args),Opts,Conf),
		case lists:keysearch(auv_send_texture,1,Opts) of 
		    {value,{auv_send_texture,true}} -> 
			draw_texture_square();
		    _ -> 
			gl:enableClientState(?GL_VERTEX_ARRAY),
			gl:clientActiveTexture(?GL_TEXTURE1),
			gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
			case lists:member(binormal,Reqs) of
			    true -> 
				gl:clientActiveTexture(?GL_TEXTURE2),
				gl:enableClientState(?GL_TEXTURE_COORD_ARRAY);
			    false -> ignore
			end,
			case lists:member(normal,Reqs) of
			    true ->
				gl:enableClientState(?GL_NORMAL_ARRAY);
			    false -> ignore
			end,
			foreach(fun(Chart = #chart{fs=Fas}) -> 
					PerFaceUniF = sh_uniforms(PerChartUniF,Chart,Conf),
					R = fun(Face = #fs{vs=Vss}) ->			    
						    sh_uniforms(PerFaceUniF,Face,Conf),
						    gl:drawElements(?GL_TRIANGLES,length(Vss),
								    ?GL_UNSIGNED_INT,Vss)
					    end,
					foreach(R,Fas) 
				end,
				Charts)
		end
	    catch throw:What ->
		    io:format("AUV: ERROR ~s ~n",[What]);
		_:What ->
		    Stack = erlang:get_stacktrace(),
		    io:format("AUV: Internal ERROR ~p:~n~p ~n",[What,Stack])
	    after
	      gl:useProgram(0),
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

shader_uniforms([{uniform,color,Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:uniform4fv(Loc,1,[Val]),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,float,Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:uniform1f(Loc,Val),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,menu,Name,_,_}|As],[Vals|Opts],Conf) 
  when is_list(Vals) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    foldl(fun(Val,Cnt) -> gl:uniform1f(Loc+Cnt,Val),Cnt+1 end,0,Vals),
    shader_uniforms(As,Opts,Conf);

shader_uniforms([{uniform,bool,Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    BoolF = if Val -> 1.0; true -> 0.0 end,
    gl:uniform1f(Loc, BoolF),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,{slider,_,_},Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:uniform1f(Loc,Val),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,{image,Unit},Name,_,_}|As],[{_,Id}|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:activeTexture(?GL_TEXTURE0 + Unit),
    TxId = wings_image:txid(Id),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:uniform1i(Loc, Unit),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{auv,{auv_noise,Unit}}|Rest],Opts,Conf) ->
    case wings_image:pnoiseid() of
	0 -> throw("No noise texture available");
	TxId -> 
	    Loc = wings_gl:uloc(Conf#sh_conf.prog, "auv_noise"),
	    gl:activeTexture(?GL_TEXTURE0 + Unit),
	    gl:bindTexture(?GL_TEXTURE_3D, TxId),
	    gl:uniform1i(Loc, Unit),
            shader_uniforms(Rest,Opts,Conf)
    end;
shader_uniforms([{auv,{auv_bg,Unit}}|Rest],Opts,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog, "auv_bg"),
    gl:activeTexture(?GL_TEXTURE0),
    gl:bindTexture(?GL_TEXTURE_2D, Conf#sh_conf.fbo_r),
    gl:uniform1i(Loc, Unit),
    shader_uniforms(Rest,Opts,Conf);
shader_uniforms([{auv,auv_texsz}|As],Opts,Conf = #sh_conf{texsz={W,H}}) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,"auv_texsz"),
    gl:uniform2f(Loc,W,H),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{auv,{auv_send_texture,_,_}}|As],[_Val|Opts],Conf) ->
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{auv,auv_bbpos3d}|R],Opts,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,"auv_bbpos3d"),
    [{MinX,MinY,MinZ},{MaxX,MaxY,MaxZ}] = (Conf#sh_conf.ts)#ts.bb,
    gl:uniform3f(Loc,MinX,MinY,MinZ),
    gl:uniform3f(Loc+1,MaxX,MaxY,MaxZ),
    shader_uniforms(R,Opts,Conf);
shader_uniforms([{auv,What}|As],Opts,Conf) ->
    [What|shader_uniforms(As,Opts,Conf)];
shader_uniforms([],[],_) ->
    [].

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

%%%%%%%%%%%% Ligth shader funcs

get_lightProjM(Dist) ->
    %% Calc ProjMatrix
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    Sz     = Dist*math:tan(45*math:pi()/180/2),
    gl:ortho(-Sz, Sz, -Sz, Sz, 1.0, Dist*2+10),
    PM = gl:getDoublev(?GL_PROJECTION_MATRIX),
    gl:popMatrix(),
    PM.

id(Id) ->
    <<R:8,G:8,B:8,A:8>> = <<Id:32/big>>,
    V = {A/255,B/255,G/255,(255-R)/255},
    io:format("~p ~n",[V]),
    V.

render_lights([{Lx,Ly,Lz}| Ligths], PM, Dist, C={Cx,Cy,Cz}, LP1,Ts = #ts{charts=Charts}) ->
    io:format("~p ~p ~p ~n", [C, Dist, {Lx,Ly,Lz}]),
    gl:matrixMode(?GL_PROJECTION),  gl:pushMatrix(),  gl:loadMatrixd(PM),
    gl:matrixMode(?GL_MODELVIEW),   gl:pushMatrix(),  gl:loadIdentity(),
    glu:lookAt(Lx*Dist,Ly*Dist,Lz*Dist,Cx,Cy,Cz,0.0,1.0,0.0),
    MV = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    gl:clear(?GL_DEPTH_BUFFER_BIT bor ?GL_COLOR_BUFFER_BIT),
%%    gl:enable(?GL_CULL_FACE),
    gl:enable(?GL_DEPTH_TEST), 
    gl:disable(?GL_BLEND), 
    gl:disable(?GL_ALPHA_TEST), 
    gl:disable(?GL_TEXTURE_2D), 
    gl:shadeModel(?GL_FLAT),
    gl:cullFace(?GL_BACK),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_COLOR_ARRAY),
    gl:clientActiveTexture(?GL_TEXTURE1),
    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),

    gl:useProgram(LP1),    
    IdPos = wings_gl:uloc(LP1,"id"),
    
    R = fun(#fs{vs=Vs,id=Id}) ->
		%% gl:color4ubv(id(Id)),
		gl:uniform4fv(IdPos, 1, id(Id)),
		gl:drawElements(?GL_TRIANGLES,length(Vs),
				?GL_UNSIGNED_INT,Vs)
	end,
    foreach(fun(#chart{fs=Fs}) -> foreach(R,Fs) end,Charts),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    
%%     %% Pass 2
%%     gl:useProgram(Vert2d),
%%     MVP = e3d_mat:mul(MM,PM),
%%     foreach(fun(#chart{fs=Fs}) -> foreach(R,Fs) end,Charts),
    
%%     %% Hmm kan vi verkligen använda DEPTH_BUFFERN HÄR utan 
%%     %% att swappa render buffers?
    render_lights(Ligths, PM, Dist, C, LP1, Ts);
render_lights([],_,_,_,LP1,_) -> 
    gl:deleteProgram(LP1),
    ok.

%%%%%%%%%%%% Ligth shader funcs

%%%%%%%%%%%%%%%% 
%% Materials
get_diffuse(Mat) ->
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

set_diffuse_tx(Mat) ->
    case get({?MODULE,use_tx}) of
	Mat -> ok;
	_ ->
	    Maps = proplists:get_value(maps, Mat),
	    Ogl = proplists:get_value(opengl,Mat),
	    gl:color4fv(proplists:get_value(diffuse,Ogl)),
	    case proplists:get_value(diffuse, Maps, none) of
		none ->
		    gl:disable(?GL_TEXTURE_2D),
		    put({?MODULE,use_tx},Mat),
		    false;
		Diff0 ->
		    gl:enable(?GL_TEXTURE_2D),
		    Diff = wings_image:txid(Diff0),
		    gl:bindTexture(?GL_TEXTURE_2D,Diff),
		    put({?MODULE,use_tx},Mat),
		    true
	    end
    end.

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
parse_sh_info([{auv,auv_noise}|Opts],Sh,NI,Acc) ->
    What = {auv,{auv_noise,1}},
    parse_sh_info(Opts, Sh#sh{args=[What|Sh#sh.args]},NI+1,Acc);
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
parse_sh_info([{uniform,image,Id,Def,Str}|Opts],Sh,NI,Acc) ->
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
	io:format("AUV: ~s ok~n", [Name]),
	[{Id,Prog}|Acc]
    catch throw:What ->
	    io:format("AUV: Error ~s ~n",[What]),
	    Acc;
	_:Err ->
	    Stack = erlang:get_stacktrace(),
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
