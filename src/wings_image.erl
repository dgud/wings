%%
%%  wings_image.erl --
%%
%%     This module manages images.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_image).
-export([from_file/1,new/2,new_temp/2,new_hidden/2, create/1,
	 rename/2,info/1,images/0,
	 screenshot/2,screenshot/1,viewport_screenshot/1,
	 txid/1,bumpid/1, combid/1,
	 is_normalmap/1,
	 next_id/0,delete_older/1,delete_from/1,delete/1,
         filter_images/1,
	 update/2,update_filename/2,find_image/2,
	 window/1, debug_display/2]).
-export([image_formats/0,image_read/1,image_write/1,
	 e3d_to_wxImage/1, wxImage_to_e3d/1]).
-export([maybe_exceds_opengl_caps/1]).

-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-import(lists, [reverse/1]).

start_link() ->
    Env = wings_io:get_process_option(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Env], [{spawn_opt, [{fullsweep_after,0}]}]).

%%%
%%% Interface against plug-ins.
%%%
image_formats() ->
    wings_plugin:call_ui({image,formats,[]}).

image_read(Ps) ->
    CurrDir  = wings_pref:get_value(current_directory),
    OptDir   = proplists:get_value(opt_dir,  Ps, undefined),
    AbsFile  = fix_filename(proplists:get_value(filename, Ps)),
    FileName = filename:basename(AbsFile),
    Dirs     = filename:split(filename:dirname(AbsFile)),
    case wings_plugin:call_ui({image,read,Ps}) of
	{error, enoent} ->
	    try_relative_paths(Dirs,FileName,OptDir,CurrDir,Ps);
	Ok = #e3d_image{} ->
	    Ok#e3d_image{filename=AbsFile};
	Other ->
	    Other
    end.

image_write(Ps) ->
    case catch wings_plugin:call_ui({image,write,Ps}) of
	{'EXIT',Reason} ->
	    {error,{none,?MODULE,{crash,Reason}}};
	Result -> Result
    end.

e3d_to_wxImage(I) ->
    e3d_to_wxImage_1(I).

%%%
%%% Client API.
%%%

from_file(Filename) ->
    Props = [{filename,Filename},{alignment,1}],
    case image_read(Props) of
	#e3d_image{}=Image ->
	    Name = filename:basename(Filename),
	    req({new,Image#e3d_image{name=Name},false, false});
	{error,_}=Error -> Error
    end.

new(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},false, false}).

new_temp(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},true, false}).

new_hidden(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},true, true}).

debug_display(Id, Img) ->
    Display = fun(_) -> wings_image_viewer:new({image, Id}, Img), keep end,
    wings ! {external, Display}.

create(St) ->
    create_image(),
    St.

rename(Id, NewName) ->
    req({rename,Id,NewName}).

txid(Id) ->
    req({txid,Id}, false).

bumpid(Id) ->
    req({bumpid,Id}, false).

combid(Id) ->
    req({combid,Id}, false).

is_normalmap(Id) ->
    req({is_normalmap,Id},false).

info(Id) ->
    req({info,Id}, false).

images() ->
    req(images, false).

filter_images(Bool) ->
    req({filter_images, Bool}, false).

next_id() ->
    req(next_id, false).

delete_older(Id) ->
    req({delete_older,Id}).

delete_from(Id) ->
    req({delete_from,Id}).

delete(Id) ->
    req({delete,Id}).

update(Id, Image) ->
    req({update,Id,Image}).

update_filename(Id, Filename) ->
    gen_server:cast(?MODULE, {update_filename,Id,Filename}),
    wings_wm:notify(image_change).

find_image(Dir, Filename) ->
    req({find_image,Dir,Filename}, false).

req(Req) ->
    req(Req, true).

req(Req, Notify) ->
    Reply = gen_server:call(?MODULE, Req, infinity),
    Running = get(wings_not_running) == undefined,
    case Notify andalso Running of
        false -> ok;
        true -> wings_wm:notify(image_change)
    end,
    Reply.

screenshot(Ask, _) when is_atom(Ask) ->
    ViewPortOnly = wings_pref:get_value(screenshot_viewport_only, false),
    SaveView = wings_pref:get_value(screenshot_save_current_view, false),
    Qs = [{?__(2,"Capture viewport only"),ViewPortOnly},
          {?__(3,"Add current view to Saved Views"),SaveView},
          {hframe,[{label,?__(4,"Name")},
		   {text,?__(1,"Screenshot"),[]}]}],
    wings_dialog:dialog(Ask, ?__(1,"Screenshot"), [{vframe,Qs}],
			fun(Res) -> {tools,{screenshot,Res}} end);
screenshot([ViewPortOnly,SaveView,Name], St) ->
    wings_pref:set_value(screenshot_viewport_only, ViewPortOnly),
    wings_pref:set_value(screenshot_save_current_view, SaveView),
    wings_wm:send_after_redraw(geom,{action,{tools,{screenshot,[ViewPortOnly,Name]}}}),
    case SaveView of
      true -> wings_view:command({views,{save,[Name]}},St);
      false -> St
    end;
screenshot([ViewPortOnly,Name], St) ->
    case ViewPortOnly of
      true -> viewport_screenshot(Name);
      false -> screenshot(Name)
    end,
    St.

screenshot(Name) ->
    {W,H} = wings_wm:top_size(),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_FRONT),
    Mem = wings_io:get_buffer(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = wings_io:get_bin(Mem),
    Image = #e3d_image{image=ImageBin,width=W,height=H},
    Id = new_temp(Name, Image),
    window(Id).

viewport_screenshot(Name) ->
%% screenshot of just the viewport scene
    {X,Y,W,H} = wings_wm:viewport(),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_FRONT),
    Mem = wings_io:get_buffer(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(X, Y, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = wings_io:get_bin(Mem),
    Image = #e3d_image{image=ImageBin,width=W,height=H},
    Id = new_temp(Name, Image),
    window(Id).

%%%
%%% Server implementation.
%%%
%%% Reason for using a server: Convenient encapsulation of images.
%%% We'll get an entire process dictionary to use for texture ids.
%%%

-record(ist,
	{next=0,				% Next image ID.
	 images				        % All #img{}'s (gb_trees).
	}).

-record(img, {e3d :: #e3d_image{},               % The image
              hidden :: boolean(),               % is_hidden?
              partof=[] :: list()                % Used in a combined textures
             }).

init([Env]) ->
    process_flag(trap_exit, true),
    wings_io:set_process_option(Env),
    {ok, #ist{images=gb_trees:empty()}}.

handle_call({new,#e3d_image{name=Name0}=Im0,false,Hide}, _From, #ist{next=Id,images=Images0}=S) ->
    try texture_format(Im0) of
        _ ->
            Name = make_unique(Name0, Images0),
            Im = maybe_convert(Im0#e3d_image{name=Name}),
            Images = gb_trees:insert(Id, #img{e3d=Im, hidden=Hide}, Images0),
            case make_texture(Id, Im) of
                {error,_GlErr}=Err ->
                    {reply, Err, S};
                _ ->
                    {reply, Id, S#ist{next=Id+1,images=Images}}
            end
    catch _:_ -> %% texture_format is used to check validity
            {reply, {error, unknown_image_format}, S}
    end;
handle_call({new,#e3d_image{name=Name}=Im,true,Hide}, From, #ist{images=Images}=S0) ->
    Exist = fun({_, #img{e3d=#e3d_image{name=N}}}) when N =:= Name -> true;
	       (_) -> false
	    end,
    case lists:filter(Exist, gb_trees:to_list(Images)) of
	[] ->
	    handle_call({new,Im,false,Hide}, From, S0);
	[{Id,_}] ->
	    {reply, ok, S} = handle_call({delete,Id}, From, S0),
	    handle_call({new,Im,false,Hide}, From, S)
    end;
handle_call({rename,Id,Name0}, _From, #ist{images=Images0}=S) ->
    Name = make_unique(Name0, gb_trees:delete(Id, Images0)),
    #img{e3d=E3d}= Im0 = gb_trees:get(Id, Images0),
    Im = Im0#img{e3d=E3d#e3d_image{name=Name}},
    Images = gb_trees:update(Id, Im, Images0),
    {reply, Id,S#ist{images=Images}};
handle_call({txid,Id}, _From, S) ->
    case get(Id) of
        undefined -> {reply, none, S};
        TxId -> {reply, TxId, S}
    end;
handle_call({bumpid,Id}, _From, S) ->
    case get({Id,normal}) of
        undefined -> {reply, create_normal(Id,undefined,S), S};
        TxId -> {reply, TxId, S}
    end;
handle_call({combid,Id}, _From, S0) ->
    case get(Id) of
        undefined ->
            {Reply, S} = create_combined(Id,S0),
            {reply, Reply, S};
        TxId ->
            {reply, TxId, S0}
    end;
handle_call({is_normalmap,Id}, _From, S) ->
    case {get({Id,normal}),get(Id)} of
        {undefined,undefined} -> {reply, none, S};
        {undefined, ImId} -> {reply, create_normal(Id,ImId,S), S};
        {TxId,_} -> {reply, TxId, S}
    end;
handle_call({info,Id}, _From, #ist{images=Images}=S) ->
    case gb_trees:lookup(Id, Images) of
	{value,#img{e3d=E3D}} -> {reply, E3D, S};
	none -> {reply, none,S}
    end;
handle_call(images, _From, #ist{images=Images}=S) ->
    {reply, [{Id, Img} || {Id, #img{e3d=#e3d_image{}=Img, hidden=false}} <- gb_trees:to_list(Images)],S};

handle_call({filter_images, Bool}, _From, #ist{images=Images}=S) ->
    Ft = case Bool of
             true -> ?GL_LINEAR;
             false -> ?GL_NEAREST
         end,
    Filter = fun(Id) ->
                     TxId = get(Id),
                     gl:bindTexture(?GL_TEXTURE_2D, TxId),
                     gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, Ft),
                     gl:bindTexture(?GL_TEXTURE_2D, 0)
             end,
    [Filter(NotHidden) || {NotHidden, #img{hidden=false}} <- gb_trees:to_list(Images)],
    {reply, ok, S};

handle_call(next_id, _From, #ist{next=Id}=S) ->
    {reply,Id,S};
handle_call({delete,Id}, _From, #ist{images=Images0}=S) ->
    delete_normal(Id),
    #img{partof=Combs} = gb_trees:get(Id, Images0),
    Ids = [erase(Comb)|| Comb <- [Id|Combs]],
    wings_gl:deleteTextures([Tex || Tex <- Ids, is_integer(Tex)]),
    Images = gb_trees:delete(Id, Images0),
    {reply, ok, S#ist{images=remove_all_combs(Combs, Images)}};
handle_call({delete_older,Id}, _From, #ist{images=Images0}=S) ->
    Images1 = delete_older_1(gb_trees:to_list(Images0), Id),
    Images = gb_trees:from_orddict(Images1),
    {reply, ok, S#ist{images=Images}};
handle_call({delete_from,Id}, _From, #ist{images=Images0}=S) ->
    Images1 = delete_from_1(gb_trees:to_list(Images0), Id, []),
    Images = gb_trees:from_orddict(Images1),
    {reply, ok, S#ist{images=Images}};
handle_call({update,Id,Image}, _From, S) ->
    {reply, ok, do_update(Id, Image, S)};
handle_call({find_image, Dir, File}, _From, #ist{images=Ims}=S) ->
    AbsName = filename:join(Dir, File),
    Find = fun(Fn) -> Fn == AbsName end,
    Found = [Id || {Id, #img{e3d=#e3d_image{filename=FN}}} <-
                       gb_trees:to_list(Ims), Find(FN)],
    case Found of
        [] -> {reply, false, S};
        [Id|_] -> {reply, {true, Id}, S}
    end;
handle_call(Req, _From, S) ->
    io:format("~w: Bad request: ~w~n", [?MODULE, Req]),
    {reply, error, S}.

handle_cast({update_filename,Id,NewName}, #ist{images=Images0}=S) ->
    #img{e3d=E3d} = Im0 = gb_trees:get(Id, Images0),
    Im = Im0#img{e3d=E3d#e3d_image{filename=NewName}},
    Images = gb_trees:update(Id, Im, Images0),
    {noreply, S#ist{images=Images}}.

%%%%%%%%%%%%%%%%% Internal Functions %%%%%%%%%%%%%%%

make_texture(Id, Image) ->
    case init_texture_0(Image) of
	{error,_}=Error ->
	    Error;
	TxId ->
	    put(Id, TxId),
	    TxId
    end.

init_texture_0(Image) ->
    case get(wings_not_running) of
        true -> 0;
        _ ->
            [TxId] = gl:genTextures(1),
            case init_texture_1(Image, TxId) of
                {error,_}=Error ->
                    wings_gl:deleteTextures([TxId]),
                    Error;
                Other ->
                    Other
            end
    end.

init_texture_1(Image0, TxId) ->
    case maybe_scale(Image0) of
        {error,_}=Error ->
            Error;
        Image ->
            init_texture_2(Image, TxId)
    end.

init_texture_2(#e3d_image{width=W,height=H,image=Bits,extra=Opts}=Image, TxId) ->
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    FT = {Format,Type} = texture_format(Image),
    Ft = case wings_pref:get_value(filter_texture, false) of
             true -> linear;
             false -> nearest
         end,
    {MinFilter,MagFilter} = proplists:get_value(filter, Opts, {mipmap, Ft}),
    {WrapS,WrapT} = proplists:get_value(wrap, Opts, {repeat, repeat}),
    MMs = proplists:get_value(mipmaps, Opts, []),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, wrap(WrapS)),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, wrap(WrapT)),
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

do_update(Id, In = #e3d_image{width=W,height=H,type=Type,name=NewName},
	  #ist{images=Images0}=S) ->
    #img{e3d=Im0, partof=Combs} = Image = gb_trees:get(Id, Images0),

    %% Cleanup combined textures and recreate (later) on the fly (can be optimized)
    Ids = [erase(Comb)|| Comb <- Combs],
    wings_gl:deleteTextures([Tex || Tex <- Ids, is_integer(Tex)]),
    Images1 = remove_all_combs(Combs, Images0),

    #e3d_image{filename=File,name=OldName} = Im0,
    Name = if is_list(NewName), length(NewName) > 2 -> NewName;
	      true -> OldName
	   end,
    Im   = maybe_convert(In#e3d_image{filename=File, name=Name}),
    TxId = get(Id),
    Images = gb_trees:update(Id, Image#img{e3d=Im, partof=[]}, Images1),
    Size = {Im0#e3d_image.width, Im0#e3d_image.height, Im0#e3d_image.type},
    case Size of
	{W,H,Type} ->
            {Format, TexType} = texture_format(Im),
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texSubImage2D(?GL_TEXTURE_2D, 0, 0, 0, W, H, Format, TexType, Im#e3d_image.image);
	_ ->
	    init_texture_1(Im, TxId)
    end,
    case get({Id,normal}) of
	undefined ->
	    S#ist{images=Images};
	Bid ->
	    create_normal(Id, Bid, S#ist{images=Images}),
	    S#ist{images=Images}
    end.

create_combined(CIds, #ist{images=Images0}=S) ->
    [O,R,M] = [gb_trees:lookup(Id, Images0) || Id <- CIds],
    Img0 = setup_combined(M, b, undefined),
    Img1 = setup_combined(R, g, Img0),
    Img2 = setup_combined(O, r, Img1),
    case Img2 of
        undefined ->
            {none, S};
        #e3d_image{} ->
            [TxId] = gl:genTextures(1),
            init_texture_2(Img2, TxId),
            put(CIds, TxId),
            {TxId, S#ist{images=add_combined(CIds, Images0)}}
    end.

setup_combined(none, _, Acc) -> Acc;
setup_combined({value, #img{e3d=Ch0}}, Where, undefined) ->
    Ch1 = e3d_image:convert(maybe_scale(Ch0),g8,1,lower_left),
    e3d_image:expand_channel(Where, Ch1);
setup_combined({value, #img{e3d=Ch0}}, Where, #e3d_image{width=W,height=H}=Img) ->
    Ch1 = e3d_image:convert(resize_image(Ch0, W, H),g8,1,lower_left),
    e3d_image:replace_channel(Where, Ch1, Img).

add_combined(CIds, Images) ->
    %% Store texture Key belong to each image
    Update = fun(none, Tree) -> Tree;
                (Id, Tree) ->
                     #img{partof=Old} = Im = gb_trees:get(Id, Tree),
                     gb_trees:update(Id, Im#img{partof=[CIds|Old]}, Tree)
             end,
    lists:foldl(Update, Images, CIds).

remove_all_combs(CIdsList, Images) ->
    lists:foldl(fun(CIds, Tree) -> remove_combined(CIds, Tree) end, Images, CIdsList).

remove_combined(CIds, Images) ->
    Update = fun(none, Tree) -> Tree;
                (Id, Tree) ->
                     #img{partof=Old} = Im = gb_trees:get(Id, Tree),
                     gb_trees:update(Id, Im#img{partof=lists:delete(CIds,Old)}, Tree)
             end,
    lists:foldl(Update, Images, CIds).

create_normal(Id, NormalId, #ist{images=Images0}) ->
    delete_normal(Id),  %% update case..
    case gb_trees:lookup(Id, Images0) of
	{value, #img{e3d=E3D0}} ->
            Image0 = e3d_image:convert(maybe_scale(E3D0),r8g8b8,1,lower_left),
	    TxId = case get(Id) of
                       NormalId ->
                           Image = make_normal_mipmaps(Image0),
                           init_texture_2(Image, NormalId);
                       _ ->
                           %% Scale ?? 4 is used in the only example I've seen.
                           Image = height2normal(Image0, #{scale=>4.0}, true),
                           [NId] = gl:genTextures(1),
                           init_texture_2(Image, NId)
                   end,
	    put({Id,normal}, TxId),
	    TxId;
	_ ->
	    none
    end.

height2normal(Img, Opts, Mipmaps) ->
    CL = ?GET(opencl),
    case wings_cl:is_kernel(height2normal, CL) of
        true  ->
            try height2normal_cl(CL, Img, Opts, Mipmaps)
            catch _:{badmatch,{error,_}} ->
                    %% Fallback (probably alloc memory)
                    ?dbg("Failed to allocate gfx memory ~n",[]),
                    e3d_image:height2normal(Img, Opts, false)
            end;
        false ->
            e3d_image:height2normal(Img, Opts, Mipmaps)
    end.

height2normal_cl(CL, #e3d_image{width=W,height=H}=Img, Opts, Mipmaps) ->
    Scale = maps:get(scale, Opts, 4.0),
    InvX  = case maps:get(inv_x, Opts, false) of true -> -Scale; false -> Scale end,
    InvY  = case maps:get(inv_y, Opts, false) of true -> -Scale; false -> Scale end,
    CLImg = wings_cl:image(e3d_image:convert(Img, g8, 1), CL),
    ResImg = Img#e3d_image{type=r8g8b8a8, bytes_pp=4, image= <<>>},
    Normal = wings_cl:buff(W*H*4*4, CL),
    NRGB  = wings_cl:image(ResImg, CL),
    W0    = wings_cl:cast(height2normal, [CLImg,W,H,InvX,InvY,Normal], [W,H], [], CL),
    W1 = wings_cl:cast(normal_to_rgba, [Normal, W, H, NRGB], [W,H], [W0], CL),
    W2 = wings_cl:read_img(NRGB, W, H, 4, [W1], CL),
    {ok, NormalRGB} = cl:wait(W2),
    cl:release_mem_object(CLImg),
    MMs = case Mipmaps of
              %% false -> [];
              true -> make_normal_mm(Normal, W div 2, H div 2, NRGB, CL)
          end,
    cl:release_mem_object(NRGB),
    cl:release_mem_object(Normal),
    ResImg#e3d_image{image=NormalRGB, extra=[{mipmaps,MMs}]}.


make_normal_mipmaps(#e3d_image{width=W, height=H} = NormalMap0) ->
    CL = ?GET(opencl),
    case wings_cl:is_kernel(mm_normalmap, CL) of
        true ->
            try
                %% OpenCL 1.2 wants r8g8b8a8
                NormalMap = e3d_image:convert(NormalMap0, r8g8b8a8, 1),
                CLImg = wings_cl:image(NormalMap, CL),
                Buff0 = wings_cl:buff(W*H*4*4, CL),
                Temp = #e3d_image{width=W div 2, height=H div 2, type=r8g8b8a8, bytes_pp=4, image= <<>>},
                NRGB  = wings_cl:image(Temp, CL),
                Init = wings_cl:cast(rgba_to_normal, [CLImg,W,H,Buff0], [W,H], [], CL),
                cl:wait(Init),
                cl:release_mem_object(CLImg),
                MMs = make_normal_mm(Buff0, W div 2, H div 2, NRGB, CL),
                cl:release_mem_object(NRGB),
                cl:release_mem_object(Buff0),
                NormalMap#e3d_image{extra=[{mipmaps,MMs}]}
            catch _:{badmatch, {error,_}} ->
                    ?dbg("Failed to allocate gfx memory ~n",[]),
                    NormalMap0;
                  _:Reason ->
                    ?dbg("CL calc crashed ~P ~P~n",[Reason, 30, erlang:get_stacktrace(),20]),
                    NormalMap0
            end;
        false ->
            NormalMap = e3d_image:convert(NormalMap0, r8g8b8, 1),
            MipMapFun = fun() -> e3d_image:buildNormalMipmaps(NormalMap) end,
            MipMaps = worker_process(MipMapFun),
            NormalMap#e3d_image{extra=[{mipmaps,MipMaps}]}
    end.

make_normal_mm(In, W, H, Img, CL) ->
    Out = wings_cl:buff(W*H*4*4, CL),
    Res = make_normal_mm(In, 1, W, H, CL, Out, Img),
    cl:release_mem_object(Out),
    Res.

make_normal_mm(In, Level, W, H, CL, Out, Img) when W > 1, H > 1 ->
    W0 = wings_cl:cast(mm_normalmap, [In, W, H, Out], [W,H], [], CL),
    W1 = wings_cl:cast(normal_to_rgba, [Out, W, H, Img], [W,H], [W0], CL),
    Read = wings_cl:read_img(Img, W, H, 4, [W1], CL),
    {ok, Bin} = cl:wait(Read),
    [{Bin, W, H, Level} | make_normal_mm(Out, Level+1, W div 2, H div 2, CL, In, Img)];
make_normal_mm(_In, _Level, _W, _H, _CL, _Out, _Img) ->
    [].

maybe_scale(#e3d_image{width=W0,height=H0}=Image) ->
%%  case wings_gl:is_ext({2,0}, 'GL_ARB_texture_non_power_of_two') of
%%  Aarg ATI doesn't support ARB_NPOT textures, though it report GL_VER >= 2.0
    case maybe_exceds_opengl_caps(Image) of
        {error,_}=Error ->
            Error;
        Image1 ->
            #e3d_image{width=W1,height=H1}=Image1,
            GL_ARB = case {W1,H1} of
                         {W0,H0} -> wings_gl:is_ext('GL_ARB_texture_non_power_of_two');
                         {_,_} -> false
                     end,
            case GL_ARB of
                true -> Image;
                false ->
                    case {nearest_power_two(W1),nearest_power_two(H1)} of
                        {W1,H1} -> Image1;
                        {W,H} -> resize_image(Image1, W, H)
                    end
            end
    end.

maybe_exceds_opengl_caps(#e3d_image{width=W0,height=H0}=Image) ->
    MaxSize = hd(gl:getIntegerv(?GL_MAX_TEXTURE_SIZE)),
    case need_resize_image(W0, H0, MaxSize) of
        true ->
            ScaleFactor = case W0 > H0 of
                true ->
                    MaxSize/W0;
                false ->
                    MaxSize/H0
            end,
            W = trunc(W0*ScaleFactor),
            H = trunc(H0*ScaleFactor),
            resize_image(Image, W, H);
        false ->
            Image
    end.

resize_image(#e3d_image{width=W0,height=H0}=Image, W0, H0) ->
    Image;
resize_image(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
                        image=Bits0}=Image, W, H) ->
    Out = wings_io:get_buffer(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
    {Format, ?GL_UNSIGNED_BYTE} = texture_format(Image),
    GlErr =glu:scaleImage(Format, W0, H0, ?GL_UNSIGNED_BYTE,
        Bits0, W, H, ?GL_UNSIGNED_BYTE, Out),
    case GlErr of
        0 ->
            Bits = wings_io:get_bin(Out),
            Image#e3d_image{width=W,height=H,bytes_pp=BytesPerPixel,image=Bits};
        _ ->
            {error,GlErr}
    end.

need_resize_image(W, H, Max) when W > Max; H > Max ->
    true;
need_resize_image(_, _, _) ->
    false.

maybe_convert(#e3d_image{type=Type0,order=Order}=Im) ->
    case {img_type(Type0),Order} of
	{Type0,lower_left} -> Im;
	{Type,_} -> e3d_image:convert(Im, Type, 1, lower_left)
    end.

img_type(b8g8r8) -> r8g8b8;
img_type(b8g8r8a8) -> r8g8b8a8;
img_type(Type) -> Type.

nearest_power_two(N) when (N band -N) =:= N -> N;
nearest_power_two(N) -> nearest_power_two(N, 1).

nearest_power_two(N, B) when B > N -> B bsr 1;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

texture_format(#e3d_image{type=r8g8b8}) -> {?GL_RGB, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=r8g8b8a8}) -> {?GL_RGBA, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=b8g8r8}) -> {?GL_BGR, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=b8g8r8a8}) -> {?GL_BGRA, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=g8}) -> {?GL_LUMINANCE, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=a8}) -> {?GL_ALPHA, ?GL_UNSIGNED_BYTE};
texture_format(#e3d_image{type=r32g32b32a32f}) -> {?GL_RGBA, ?GL_FLOAT};
texture_format(#e3d_image{type=r32g32b32f}) -> {?GL_RGB, ?GL_FLOAT}.

%% Long ago we tried to use compression, but we no longer do since compression
%% lowers the quality too much, especially for bump/normal maps.
internal_format({?GL_BGR,?GL_UNSIGNED_BYTE}) -> ?GL_RGB;
internal_format({?GL_BGRA,?GL_UNSIGNED_BYTE}) -> ?GL_RGBA;
internal_format({?GL_RGBA,?GL_FLOAT}) -> ?GL_RGBA32F;
internal_format({?GL_RGB,?GL_FLOAT}) -> ?GL_RGB32F;
internal_format({Format,?GL_UNSIGNED_BYTE}) -> Format.

filter(mipmap) -> ?GL_LINEAR_MIPMAP_LINEAR;
filter(linear) -> ?GL_LINEAR;
filter(nearest) -> ?GL_NEAREST.

wrap(mirror) -> ?GL_MIRRORED_REPEAT;
wrap(repeat) -> ?GL_REPEAT;
wrap(clamp) -> ?GL_CLAMP_TO_EDGE.

delete_older_1([{_,#img{hidden=true}}=Im|T], Limit) ->
    [Im|delete_older_1(T, Limit)];
delete_older_1([{Id,#img{partof=Combs}}|T], Limit) when Id < Limit ->
    delete_normal(Id),
    Ids = [erase(Comb)|| Comb <- [Id|Combs]],
    wings_gl:deleteTextures([Tex || Tex <- Ids, is_integer(Tex)]),
    delete_older_1(T, Limit);
delete_older_1(Images, _) -> Images.

delete_from_1([{_,#img{hidden=true}}=Im|T], Limit, Acc) ->
    delete_from_1(T, Limit, [Im|Acc]);
delete_from_1([{Id,_}=Im|T], Limit, Acc) when Id < Limit ->
    delete_from_1(T, Limit, [Im|Acc]);
delete_from_1([{Id,#img{partof=Combs}}|T], Limit, Acc) ->
    delete_normal(Id),
    Ids = [erase(Comb)|| Comb <- [Id|Combs]],
    wings_gl:deleteTextures([Tex || Tex <- Ids, is_integer(Tex)]),
    delete_from_1(T, Limit, Acc);
delete_from_1([], _, Acc) -> reverse(Acc).

delete_normal(Id) ->
    TxId = get(Id),
    case erase({Id,normal}) of
	undefined -> ok;
	TxId -> ok;
	Bid ->  wings_gl:deleteTextures([Bid])
    end.

make_unique(Name, Images0) ->
    Images = [Old || #img{e3d=#e3d_image{name=Old}} <- gb_trees:values(Images0)],
    wings_util:unique_name(Name, Images).

%%%
%%% Window for image.
%%%

window(Id) ->
    Name = {image,Id},
    case wings_wm:is_window(Name) of
	true ->
	    wings_wm:raise(Name);
	false ->
	    wings_image_viewer:new(Name, info(Id)),
	    keep
    end.

%%%
%%% Creating images with pre-defined patterns.
%%%

create_image() ->
    Def = wings_pref:get_value(current_directory),
    Ps = [{extensions,image_formats()},{multiple,false}],
    Flags = [{props, Ps}],
    Qs = [{label_column,
           [{?__(0,"Import"), {button, {text, Def, Flags}}},
            separator,
            {?__(1,"Width"), {text, 256,[{range,{8,1024}}]}},
            {?__(2,"Height"),{text, 256,[{range,{8,1024}}]}}
           ]},
	  {vradio,
	   [{?__(4,"Grid"),grid},
	    {?__(5,"Checkerboard"),checkerboard},
	    {?__(6,"Vertical Bars"),vbars},
	    {?__(7,"Horizontal Bars"),hbars},
	    {?__(8,"White"),white},
	    {?__(9,"Black"),black}],
	   grid, [{title, ?__(3,"Pattern")}]}],
    wings_dialog:dialog(?__(10,"Create Image"), Qs,
			fun([File, W,H,Pattern]) ->
                                case filelib:is_regular(File) of
                                    true  ->
                                        from_file(File);
                                    false ->
                                        create_image_1(Pattern, W, H)
                                end,
				ignore
			end).

create_image_1(Pattern, W, H) ->
    Pixels = pattern(Pattern, W, H),
    case {byte_size(Pixels),3*W*H} of
	{S,S} ->				%Assertion.
	    Im = #e3d_image{width=W,height=H,image=Pixels,order=upper_left},
	    new(atom_to_list(Pattern), Im)
    end.

pattern(grid, W, H) ->
    grid(W, H);
pattern(checkerboard, W, H) ->
    checkerboard(W, H);
pattern(vbars, W, H) ->
    vertical_bars(W, H);
pattern(hbars, W, H) ->
    horizontal_bars(W, H);
pattern(white, W, H) ->
    all_white(W, H);
pattern(black, W, H) ->
    all_black(W, H).

%% Generate a grid image.
grid(Width, Height) ->
    White = [255,255,255],
    Black = [0,0,0],
    WhiteRow = pattern_repeat(Width, White),
    BlackLine = pattern_repeat(14, Black),
    Mixed0 = pattern_repeat(14*((Width+15) div 16), [White,BlackLine|White]),
    Mixed = truncate(Mixed0, 3*Width),
    MixedRows = pattern_repeat(14, Mixed),
    R = [WhiteRow,MixedRows|WhiteRow],
    All = pattern_repeat((Height+15) div 16, R),
    truncate(All, 3*Width*Height).

%% Generate a checkerboard image of 4x4 squares 
%% with given side length in pixels.
checkerboard(Width, Height) ->
    FourWhite = pattern_repeat(3*4, 255),
    FourBlack = pattern_repeat(3*4, 0),
    RoundedW = (Width+7) div 8,
    RowSize = 3*Width,
    R1 = truncate(pattern_repeat(RoundedW, [FourBlack|FourWhite]), RowSize),
    R2 = truncate(pattern_repeat(RoundedW, [FourWhite|FourBlack]), RowSize),
    R8 = [pattern_repeat(4, [R1])|pattern_repeat(4, [R2])],
    truncate(pattern_repeat(RoundedW, R8), 3*Width*Height).

%% Generate a vertical bars image of 4 pixels width of given size.
vertical_bars(Width, Height) ->
    W4 = pattern_repeat(3*4, 255),
    B4 = pattern_repeat(3*4, 0),
    Row = truncate(pattern_repeat((Width+7) div 8, [B4|W4]), 3*Width),
    list_to_binary(pattern_repeat(Height, Row)).

%% Generate a horizontal bars image of 4 pixels height.
horizontal_bars(Width, Height) ->
    WhiteRow = pattern_repeat(3*Width, 255),
    BlackRow = pattern_repeat(3*Width, 0),
    WR4 = pattern_repeat(4, WhiteRow),
    BR4 = pattern_repeat(4, BlackRow),
    truncate(pattern_repeat((Height+7) div 8, [BR4|WR4]), 3*Width*Height).

%% Generate an all white image with given size.
all_white(Width, Height) ->
    solid(Width, Height, 255).

%% Generate an all white image with given size.
all_black(Width, Height) ->
    solid(Width, Height, 0).

solid(Width, Height, Channel) ->
    list_to_binary(pattern_repeat(3*Width*Height, Channel)).

pattern_repeat(0, _) -> [];
pattern_repeat(1, D) -> [D];
pattern_repeat(N, D) ->
    B = pattern_repeat(N div 2, D),
    case N rem 2 of
	0 -> [B|B];
	1 -> [D,B|B]
    end.

truncate(B0, Sz) ->
    <<B:Sz/binary,_/binary>> = list_to_binary(B0),
    B.

%% Run a computation in a worker process with a generous heap size
%% and the default generational garbage collector. Before R12B,
%% heap fragments would allow the heap to be over-committed, but in
%% R12B there will be garbage collection as soon as the heap space
%% is exhausted.
worker_process(WorkFun) ->
    ResultRef = make_ref(),
    {Pid,Ref} = spawn_opt(fun() -> exit({ResultRef,WorkFun()}) end,
			  [monitor,{min_heap_size,20000}]),
    receive
	{'DOWN',Ref,process,Pid,{ResultRef,Res}} ->
	    Res;
	{'DOWN',Ref,process,Pid,Reason} ->
	    exit(Reason)
    end.

e3d_to_wxImage_1(I = #e3d_image{bytes_pp=4, width=W, height=H}) ->
    #e3d_image{image=RGB} = e3d_image:convert(I, r8g8b8, 1, upper_left),
    Wx = wxImage:new(W,H,RGB),
    #e3d_image{image=Alpha} = e3d_image:convert(I, a8, 1, upper_left),
    wxImage:setAlpha(Wx, Alpha),
    Wx;
e3d_to_wxImage_1(I = #e3d_image{bytes_pp=3, width=W, height=H}) ->
    #e3d_image{image=RGB} = e3d_image:convert(I, r8g8b8, 1, upper_left),
    wxImage:new(W,H,RGB);
e3d_to_wxImage_1(I = #e3d_image{bytes_pp=1, width=W, height=H}) ->
    #e3d_image{image=RGB} = e3d_image:convert(I, r8g8b8, 1, upper_left),
    wxImage:new(W,H,RGB);
e3d_to_wxImage_1(I = #e3d_image{type=r32g32b32f, width=W, height=H}) ->
    #e3d_image{image=RGBf} = e3d_image:convert(I, r32g32b32f, 1, upper_left),
    RGB8 = << <<(max(1.0, abs(C))*255)>> || <<C:32/float>> <= RGBf >>,
    wxImage:new(W,H,RGB8);
e3d_to_wxImage_1(I = #e3d_image{type=r32g32b32a32f, width=W, height=H}) ->
    #e3d_image{image=RGBf} = e3d_image:convert(I, r32g32b32a32f, 1, upper_left),
    RGB8 = << <<(max(1.0, abs(R))*255),(max(1.0, abs(G))*255),(max(1.0, abs(B))*255)>>
              || <<R:32/float,G:32/float,B:32/float,_:32/float>> <= RGBf >>,
    Wx = wxImage:new(W,H,RGB8),
    Alpha = << <<(max(1.0, abs(A))*255)>> || <<_:12/binary,A:32/float>> <= RGBf >>,
    wxImage:setAlpha(Wx, Alpha),
    Wx.

wxImage_to_e3d(Wx) ->
    wxImage_to_e3d_1(Wx).

wxImage_to_e3d_1(Wx) ->
    wxImage = wx:getObjectType(Wx), %% Assert
    E3d0 = #e3d_image{image=wxImage:getData(Wx),
		      width=wxImage:getWidth(Wx),
		      height=wxImage:getHeight(Wx),
		      order = upper_left
		     },
    case wxImage:hasAlpha(Wx) of
	true ->
            e3d_image:add_alpha(E3d0, wxImage:getAlpha(Wx));
	false ->
            case wxImage:hasMask(Wx) of
                true ->
                    wxImage:initAlpha(Wx),
                    e3d_image:add_alpha(E3d0, wxImage:getAlpha(Wx));
                false ->
                    E3d0
            end
    end.

fix_filename([_,Char|Path]=Dir)
  when Char =:= $:; Char =:= $\\ ->
    case os:type() of
        {win32, _} ->
            Dir;
        _ -> %% Win32 Path on non window system
            lists:map(fun($\\) -> $/; (C) -> C end, Path)
    end;
fix_filename(Dir) ->
    Dir.

try_relative_paths(Dirs,FileName,undefined,CurrDir,Ps) ->
    try_relative_paths(CurrDir,Dirs,FileName,Ps);
try_relative_paths(Dirs,FileName,OptDir,CurrDir,Ps) ->
    case try_relative_paths(OptDir,Dirs,FileName,Ps) of
	{error, enoent} ->
	    try_relative_paths(Dirs,FileName,undefined,CurrDir,Ps);
	Other ->
	    Other
    end.

try_relative_paths(Start, Rel, File, Ps0) ->
    Abs = case Rel of
	      [] -> filename:join(Start,File);
	      _  -> filename:join(filename:join(Start,filename:join(Rel)),File)
	  end,
    Ps = lists:keyreplace(filename,1,Ps0,{filename,Abs}),
    case wings_plugin:call_ui({image,read,Ps}) of
	Err = {error, enoent} ->
	    case Rel of
		[] -> Err;
		[_|Rest] -> try_relative_paths(Start,Rest,File,Ps0)
	    end;
	Ok = #e3d_image{} ->
	    Ok#e3d_image{filename=Abs};
	Other ->
	    Other
    end.
