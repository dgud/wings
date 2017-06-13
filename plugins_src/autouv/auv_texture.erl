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
-export([get_texture/2,draw_options/0]).

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

draw_options() ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = max(min(8192, MaxTxs0), 256),
    Prefs = get_pref(tx_prefs, pref_to_list(#opt{})),
    TexSz = proplists:get_value(texsz, Prefs, 512),
    Shaders = shaders(),
    Qs = [{hframe,[{menu, gen_tx_sizes(MaxTxs, []),TexSz,
		    [{key,texsz}]}],[{title,?__(1,"Size")}]},
	  {vframe, render_passes(Prefs, Shaders), [{title,?__(2,"Render")}]}
	 ],

    wings_dialog:dialog(?__(3,"Draw Options"), Qs,
			fun(Options) ->
				Opt = list_to_prefs(Options),
				set_pref([{tx_prefs,pref_to_list(Opt)}]),
				{auv,{draw_options,{Opt,Shaders}}}
			end).

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
	Opts = wings_dialog:get_value({auv_opt, Id}, Fields),
	{StrName, Name} = renderer(Name,Renderers),
	SetValue = fun(Res) ->
			   %% Change the value in the parent dialog
			   wings_dialog:set_value({auv_opt, Id}, [Name|Res], Fields),
			   ignore
		   end,
	wings_dialog:dialog(StrName,options(Name,Opts,Shaders),SetValue)
    catch _:Crash ->
	    io:format("EXIT: ~p ~p~n",[Crash, erlang:get_stacktrace()])
    end.

options(auv_background, [auv_background, {type_sel,Type},{Image,_},Color],_) ->
    Enable = fun(_, What, Fields) ->
		     IsImage = image == What,
		     wings_dialog:enable(image_sel, IsImage, Fields),
		     wings_dialog:enable(col_sel,  not IsImage, Fields)
	     end,
    [{hradio,[{?__(1,"Image"),image},{?__(2,"Color"),color}],
      Type,[{key,type_sel},{hook, Enable}]},
     {hframe,[{label,?__(1,"Image")},image_selector(Image)],
      [{key, image_sel}]},
     {hframe,[{label,?__(2,"Color")},{color,fix(Color,wings_gl:have_fbo())}],
      [{key, col_sel}]}];
options(auv_background, _Bad,Sh) ->
    options(auv_background, ?OPT_BG,Sh);

options(auv_edges,[auv_edges, Type,Color,Size,UseVtxColors],_) ->
    [{vradio,[{?__(3,"Draw All Edges"),all_edges},
	      {?__(4,"Draw Border Edges"), border_edges}],
      Type, []},
     {hframe,[{label,?__(5,"Edge Color:")},{color,Color}]},
     {hframe,[{label,?__(6,"Edge Width:")},{text,Size,[{range,{0.0,100.0}}]}]},
     {?__(8,"Use vertex colors (on border edges)"),UseVtxColors}
    ];
options(auv_edges,_,Sh) -> options(auv_edges,?OPT_EDGES,Sh);

options({shader,Id}, Vals0, Sh) ->
    {value,Shader} = lists:keysearch(Id,#sh.id,Sh),
    case Vals0 of
	[{shader,Id}|Vals] ->
	    shader_options(Shader,Vals);
	_ ->
	    shader_options(Shader,[])
    end;

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
	Menues ->
	    Menues
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
shader_menu([{auv,{auv_send_texture,Label,Def0}}|As],Vs0,Acc) ->
    case Vs0 of
	[{auv_send_texture, Def}|Vs] -> ok;
	[Def|Vs] -> ok;
	[] -> Def = Def0, Vs = []
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
    {hook,
     fun(_Key, button_pressed, Fields) ->
	     Env = wx:get_env(),
	     spawn(fun() ->
			   %% Neeed open dialog in dialog from another process
			   wx:set_env(Env),
			   option_dialog(Id, Fields, Renderers, Shaders)
		   end)
     end
    }.

renderer(Id,[Renderer={_,Id}|_R]) ->  Renderer;
renderer(Id,[_|R]) ->  renderer(Id,R).

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
    set_viewport({0,0,W,H}),
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
    catch _:What ->
	    Where = erlang:get_stacktrace(),
	    exit({What,Where})
    after
	case UsingFbo of
            false -> ignore;
            #sh_conf{fbo_d=DeleteMe} -> DeleteMe()
        end,
        wings_vbo:delete(Geom#ts.vbo),
        set_viewport(Current),
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
	Err -> io:format("~p: ~p ~n",[Line,Err])
    end.

draw_texture_square() ->
    VertexUvQ = << 0.0:?F32,0.0:?F32, 0.0:?F32,0.0:?F32,
                   1.0:?F32,0.0:?F32, 1.0:?F32,0.0:?F32,
                   1.0:?F32,1.0:?F32, 1.0:?F32,1.0:?F32,
                   0.0:?F32,1.0:?F32, 0.0:?F32,1.0:?F32>>,
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

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data setup 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup(#st{bb=#uvstate{id=RId,st=#st{shapes=Sh0}}}=St, Reqs) ->
    We = gb_trees:get(RId,Sh0),
    {Charts,{_Cnt,UVpos,Vpos,Ns,Ts,BB,Vc}} = setup_charts(St, We, Reqs),
    #ts{charts=Charts,
	uv = to_bin(UVpos,uv),
	pos= to_bin(Vpos,pos),
	n  = to_bin(Ns,pos),
	bi = build_tangents(Ts),
	bb = BB,
	vc = to_bin(Vc, vertex)}.

setup_charts(#st{shapes=Cs0,selmode=Mode,sel=Sel}, We, Reqs) ->
    Ns = case member(normal, Reqs) orelse member(binormal, Reqs) of
	     true -> setup_normals(We);
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
	{{0.0, 0.0, 0.0}, BiT} ->
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

get_tangent(undefined, {0.0,0.0,0.0}, H0, N) ->
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
	    gl:lineWidth(Width),
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
	    gl:lineWidth(Width),
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
	    All = fun() -> foreach(fun(#chart{fs=Fs}) -> foreach(R, Fs) end, Charts) end,
            EnableVbo(All)
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
		_:What ->
		    Stack = erlang:get_stacktrace(),
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


shader_uniforms([{uniform,color,Name,_,_}|As],[Val|Opts],Conf) ->
    wings_gl:set_uloc(Conf#sh_conf.prog,Name,Val),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,float,Name,_,_}|As],[Val|Opts],Conf) ->
    wings_gl:set_uloc(Conf#sh_conf.prog,Name, Val),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,menu,Name,_,_}|As],[Vals|Opts],Conf) 
  when is_list(Vals) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    foldl(fun(Val,Cnt) when is_integer(Val) ->
                gl:uniform1i(Loc+Cnt,Val),Cnt+1;
             (Val,Cnt) ->
                gl:uniform1f(Loc+Cnt,Val),Cnt+1
          end,0,Vals),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,menu,Name,_,_}|As],[Vals|Opts],Conf)
    when is_integer(Vals) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:uniform1i(Loc,Vals),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,bool,Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    BoolF = if Val -> 1.0; true -> 0.0 end,
    gl:uniform1f(Loc, BoolF),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,{slider,_,_},Name,_,_}|As],[Val|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
	if is_integer(Val) ->
			gl:uniform1i(Loc,Val);
		true ->
			gl:uniform1f(Loc,Val)
	end,
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{uniform,{image,Unit},Name,_,_}|As],[{_,Id}|Opts],Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog,Name),
    gl:activeTexture(?GL_TEXTURE0 + Unit),
    TxId = wings_image:txid(Id),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:uniform1i(Loc, Unit),
    shader_uniforms(As,Opts,Conf);
shader_uniforms([{auv,{auv_bg,Unit}}|Rest],Opts,Conf) ->
    Loc = wings_gl:uloc(Conf#sh_conf.prog, "auv_bg"),
    gl:activeTexture(?GL_TEXTURE0),
    gl:bindTexture(?GL_TEXTURE_2D, Conf#sh_conf.fbo_r),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
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
	io:format("AUV: Shader ´~s´ ok~n", [Name]),
	[{Id,Prog}|Acc]
    catch throw:What ->
	    io:format("AUV: Error ~p ~s ~n",[Name, What]),
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
