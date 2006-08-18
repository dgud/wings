%%
%%  wings_image.erl --
%%
%%     This module manages images.
%%
%%  Copyright (c) 2003-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_image.erl,v 1.53 2006/01/25 21:15:12 dgud Exp $
%%

-module(wings_image).
-export([init/0,init_opengl/0,
	 from_file/1,new/2,new_temp/2,create/1,
	 rename/2,txid/1,info/1,images/0,screenshot/0,
	 bumpid/1, default/1,
	 is_normalmap/1, normal_cubemapid/0, pnoiseid/0,
	 next_id/0,delete_older/1,delete_from/1,delete/1,
	 update/2,update_filename/2,draw_preview/5,
	 window/1]).
-export([image_formats/0,image_read/1,image_write/1]).

-export([loop/1]).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [reverse/1,foreach/2,flatten/1]).

-define(DEFAULT, '$Default Img').

init() ->
    spawn_opt(fun server/0, [link,{fullsweep_after,0}]).

init_opengl() ->
    req(init_opengl).

%%%
%%% Interface against plug-ins.
%%%
image_formats() ->
    wings_plugin:call_ui({image,formats,[]}).

image_read(Ps) ->
    wings_plugin:call_ui({image,read,Ps}).

image_write(Ps) ->
    case catch wings_plugin:call_ui({image,write,Ps}) of
	{'EXIT',Reason} ->
	    {error,{none,?MODULE,{crash,Reason}}};
	Result -> Result
    end.


%%%
%%% Client API.
%%%

from_file(Filename) ->
    Props = [{filename,Filename},{alignment,1}],
    case image_read(Props) of
	#e3d_image{}=Image ->
	    Name = filename:rootname(filename:basename(Filename)),
	    req({new,Image#e3d_image{filename=Filename,name=Name},false});
	{error,_}=Error -> Error
    end.

new(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},false}).

new_temp(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},true}).

create(St) ->
    create_image(),
    St.

rename(Id, NewName) ->
    req({rename,Id,NewName}).

txid(Id) ->
    req({txid,Id}, false).

screenshot() ->
    {W,H} = wings_wm:top_size(),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_FRONT),
    Mem = sdl_util:alloc(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = sdl_util:getBin(Mem),
    Image = #e3d_image{image=ImageBin,width=W,height=H},
    Id = new_temp(?__(1,"<<Screenshot>>"), Image),
    wings_image:window(Id).

is_normalmap(Id) ->
    req({is_normalmap,Id},false).

bumpid(Id) ->
    req({bumpid,Id}, false).

normal_cubemapid() ->
    req(normalCM, false).

pnoiseid() ->
    req(pnoise, false).

default(Type) ->
    req({default,Type}, false).

info(Id) ->
    req({info,Id}, false).

images() ->
    req(images, false).

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
    req({update_filename,Id,Filename}).

draw_preview(X, Y, W, H, Id) ->
    req({draw_preview,X,Y,W,H,Id}, false).

req(Req) ->
    req(Req, true).

req(Req, Notify) ->
    Self = self(),
    Ref = make_ref(),
    wings_image ! {Self,Ref,Req},
    receive
	{Ref,Answer} ->
	    case Notify of
		false -> ok;
		true -> wings_wm:notify(image_change)
	    end,
	    Answer
    end.

%%%
%%% Server implementation.
%%%
%%% Reason for using a server: Convenient encapsulation of images.
%%% We'll get an entire process dictionary to use for texture ids.
%%% 

-record(ist,
	{next=0,				%Next image ID.
	 images					%All images (gb_trees).
	}).

server() ->
    register(wings_image, self()),
    loop(#ist{images=gb_trees:empty()}).

loop(S0) ->
    receive
	{Client,Ref,Req} ->
	    case handle(Req, S0) of
		#ist{}=S ->
		    Client ! {Ref,ok};
		{Resp,S} ->
		    Client ! {Ref,Resp}
	    end,
	    loop(S);
	reload ->
	    ?MODULE:loop(S0);
	Other ->
	    exit({bad_message_to_wings_image,Other})
    end.

handle(init_opengl, #ist{images=Images}=S) ->
    erase(), %% Forget all textures!
    foreach(fun({Id,Image}) ->
		    make_texture(Id, Image)
	    end, gb_trees:to_list(Images)),
    init_background_tx(),
    S;
handle({new,#e3d_image{name=Name0}=Im0,false}, #ist{next=Id,images=Images0}=S) ->
    Name = make_unique(Name0, Images0),
    Im = maybe_convert(Im0#e3d_image{name=Name}),
    Images = gb_trees:insert(Id, Im, Images0),
    make_texture(Id, Im),
    {Id,S#ist{next=Id+1,images=Images}};
handle({new,#e3d_image{name=Name}=Im,true}, #ist{images=Images}=S0) ->
    Prev = [Id || {Id,#e3d_image{name=N}} <- gb_trees:to_list(Images),
		 N =:= Name],
    case Prev of
	[] ->
	    handle({new,Im,false}, S0);
	[Id] ->
	    S = handle({delete,Id}, S0),
	    handle({new,Im,false}, S)
    end;
handle({rename,Id,Name0}, #ist{images=Images0}=S) ->
    Name = make_unique(Name0, gb_trees:delete(Id, Images0)),
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{name=Name},
    Images = gb_trees:update(Id, Im, Images0),
    {Id,S#ist{images=Images}};
handle({txid,Id}, S) ->
    {case get(Id) of
	 undefined -> none;
	 TxId -> TxId
     end,S};
handle({bumpid,Id}, S) ->
    {case get({Id,bump}) of
	 undefined -> 
	     create_bump(Id,undefined,S);
	 TxId -> 
	     TxId
     end,S};
handle({is_normalmap,Id},S) ->
    {case {get({Id,bump}),get(Id)} of
	 {undefined,undefined} -> none;
	 {undefined, ImId} -> create_bump(Id,ImId,S);
	 {TxId,_} -> TxId
     end,S};

handle(normalCM, S) ->
    {case get(normalCM) of
	 undefined -> 
	     create_normal_cube_map();
	 TxId -> 
	     TxId
     end,S};
handle(pnoise, S) ->
    {case get(pnoise) of
	 undefined -> 
	     create_pnoise();
	 TxId -> 
	     TxId
     end,S};
handle({default,all},S) ->
    All = [diffuse, bump, gloss],
    Ids = [{Type,element(1, handle({default,Type},S))} || Type <- All],
    {Ids, S};
handle({default,Type},S) ->
    {case get({?DEFAULT, Type}) of
	 undefined -> 
	     create_default(Type);
	 TxId -> 
	     TxId
     end,S};
handle({info,Id}, #ist{images=Images}=S) ->
    case gb_trees:lookup(Id, Images) of
	{value,E3D} -> {E3D,S};
	none -> {none,S}
    end;
handle(images, #ist{images=Images}=S) ->
    {gb_trees:to_list(Images),S};
handle(next_id, #ist{next=Id}=S) ->
    {Id,S};
handle({delete,Id}, S) ->
    delete(Id, S);
handle({delete_older,Id}, S) ->
    delete_older(Id, S);
handle({delete_from,Id}, S) ->
    delete_from(Id, S);
handle({update,Id,Image}, S) ->
    do_update(Id, Image, S);
handle({update_filename,Id,NewName}, #ist{images=Images0}=S) ->
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{filename=NewName},
    Images = gb_trees:update(Id, Im, Images0),
    S#ist{images=Images};
handle({draw_preview,X,Y,W,H,Id}, S) ->
    {case get(Id) of
	 undefined -> error;
	 TxId -> draw_image(X, Y, W, H, TxId)
     end,S}.

create_bump(Id, BumpId, #ist{images=Images0}) ->
    delete_bump(Id),  %% update case..
    case gb_trees:lookup(Id, Images0) of
	{value, E3D} -> 
	    gl:pushAttrib(?GL_TEXTURE_BIT),
	    case get(Id) of
		BumpId ->
		    gl:bindTexture(?GL_TEXTURE_2D, BumpId),
		    Image = case e3d_image:convert(maybe_scale(E3D),r8g8b8,1,lower_left) of
				E3D -> E3D;
				New = #e3d_image{width=W,height=H,image=Bits} ->
				    gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGB,W,H,0,?GL_RGB,
						  ?GL_UNSIGNED_BYTE,Bits),
				    New
			    end,
		    MipMaps = e3d_image:buildNormalMipmaps(Image),
		    TxId = BumpId;
		_ ->
		    %% Scale ?? 4 is used in the only example I've seen.
		    Img = e3d_image:convert(maybe_scale(E3D), r8g8b8, 1, lower_left),
		    {#e3d_image{width=W,height=H,image=Bits},MipMaps} 
			= e3d_image:height2normal(Img, 4, true),
		    [TxId] = gl:genTextures(1),
		    gl:bindTexture(?GL_TEXTURE_2D, TxId),
		    gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGB,W,H,0,?GL_RGB,
				  ?GL_UNSIGNED_BYTE,Bits)
	    end,
	    put({Id,bump}, TxId),
	    update_mipmaps(TxId,MipMaps),
	    gl:popAttrib(),
	    TxId;
	_ ->
	    none
    end.

create_default(bump) ->
    Img = #e3d_image{width=1,height=1,        % Default direction
		     image= <<127,127,255>>}, % Normal points to +Z
    make_texture({?DEFAULT,bump},Img);
create_default(Type) when Type == diffuse; Type == gloss ->
    Img = #e3d_image{width=1,height=1,        % Default diffuse/spec
		     image= <<255,255,255>>}, % White and full specular.
    make_texture({?DEFAULT,Type},Img);
create_default(Type) -> 
    io:format(?__(1,"~p Don't know about the type ~p; ignoring\n"),
	      [?MODULE,Type]),
    none.

create_normal_cube_map() ->
    case wings_gl:is_ext('GL_ARB_texture_cube_map') of
	true ->	
	    [CubeMap] = gl:genTextures(1),
	    gl:bindTexture(?GL_TEXTURE_CUBE_MAP, CubeMap),
	    make_normalize_vector_cubemap(32),
	    put(normalCM,CubeMap),
	    CubeMap;
	false ->
	    none
    end.

create_pnoise() ->
    try 
	[NoiseMap] = gl:genTextures(1),
	gl:bindTexture(?GL_TEXTURE_3D, NoiseMap),
	Map = pnoise:s_map3d(128),

	Assert = size(Map),
	Assert = 128*128*128*4,

	gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
	case wings_gl:is_ext({1,4},'GL_SGIS_generate_mipmap') of
	    true -> 
		gl:texParameteri(?GL_TEXTURE_3D, ?GL_GENERATE_MIPMAP, 
				 ?GL_TRUE),
		gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_MIN_FILTER, 
				 ?GL_NEAREST_MIPMAP_LINEAR);
	    false ->
		gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_MIN_FILTER, 
				 ?GL_LINEAR)
	end,
	gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_WRAP_S, 
			 ?GL_MIRRORED_REPEAT),
	gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_WRAP_T, 
			 ?GL_MIRRORED_REPEAT),
	gl:texParameteri(?GL_TEXTURE_3D, ?GL_TEXTURE_WRAP_R, 
			 ?GL_MIRRORED_REPEAT),
	gl:texImage3D(?GL_TEXTURE_3D, 0, ?GL_RGBA, 128, 128, 128, 0, 
		      ?GL_RGBA, ?GL_UNSIGNED_BYTE, Map),
        put(pnoise,NoiseMap),
	NoiseMap
    catch _:_ -> 
	    none
    end.


update_mipmaps(TxId, MipMaps) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR_MIPMAP_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    Load = fun({Level,MW,MH, Bin}) ->
		   gl:texImage2D(?GL_TEXTURE_2D,Level,?GL_RGB,MW,MH,0,
				 ?GL_RGB, ?GL_UNSIGNED_BYTE, Bin)
	   end,
    [Load(MM) || MM <- MipMaps].


maybe_convert(#e3d_image{type=Type0,order=Order}=Im) ->
    case {img_type(Type0),Order} of
	{Type0,lower_left} -> Im;
	{Type,_} -> e3d_image:convert(Im, Type, 1, lower_left)
    end.

img_type(b8g8r8) -> r8g8b8;
img_type(b8g8r8a8) -> r8g8b8a8;
img_type(Type) -> Type.

init_background_tx() ->
    White = [255,255,255],
    Grey = [204,204,204],
    EightWhite = pattern_repeat(8, White),
    EightGrey = pattern_repeat(8, Grey),
    B0 = [pattern_repeat(8, [EightGrey|EightWhite])|
	  pattern_repeat(8, [EightWhite|EightGrey])],
    B = list_to_binary(B0),
    Im = #e3d_image{width=16,height=16,image=B},
    put(background, init_texture(Im)).

make_texture(Id, Image) ->
    TxId = init_texture(Image),
    put(Id, TxId),
    TxId.

init_texture(Image) ->
    [TxId] = gl:genTextures(1),
    init_texture(Image, TxId).

init_texture(Image0, TxId) ->
    Image = maybe_scale(Image0),
    #e3d_image{width=W,height=H,image=Bits} = Image,
    gl:pushAttrib(?GL_TEXTURE_BIT),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    case wings_gl:is_ext({1,4},'GL_SGIS_generate_mipmap') of
	true -> 
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, ?GL_TRUE),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, 
			     ?GL_LINEAR_MIPMAP_LINEAR);
	false ->
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR)
    end,
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    Format = texture_format(Image),
    gl:texImage2D(?GL_TEXTURE_2D, 0, internal_format(Format),
		  W, H, 0, Format, ?GL_UNSIGNED_BYTE, Bits),
    gl:popAttrib(),
    TxId.

maybe_scale(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
		       image=Bits0}=Image) ->
%%    case wings_gl:is_ext({2,0}, 'GL_ARB_texture_non_power_of_two') of
%%  Aarg ATI doesn't support ARB_NPOT textures, though it report GL_VER >= 2.0
    case wings_gl:is_ext('GL_ARB_texture_non_power_of_two') of
	true -> Image;
	false ->
	    case {nearest_power_two(W0),nearest_power_two(H0)} of
		{W0,H0} -> Image;
		{W,H} ->
		    Out = sdl_util:alloc(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
		    Format = texture_format(Image),
		    glu:scaleImage(Format, W0, H0, ?GL_UNSIGNED_BYTE,
				   Bits0, W, H, ?GL_UNSIGNED_BYTE, Out),
		    Bits = sdl_util:getBin(Out),
		    Image#e3d_image{width=W,height=H,image=Bits}
	    end
    end.

nearest_power_two(N) when (N band -N) =:= N -> N;
nearest_power_two(N) -> nearest_power_two(N, 1).

nearest_power_two(N, B) when B > N -> B bsr 1;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

texture_format(#e3d_image{type=r8g8b8}) -> ?GL_RGB;
texture_format(#e3d_image{type=r8g8b8a8}) -> ?GL_RGBA;
texture_format(#e3d_image{type=b8g8r8}) -> ?GL_BGR;
texture_format(#e3d_image{type=b8g8r8a8}) -> ?GL_BGRA;
texture_format(#e3d_image{type=g8}) -> ?GL_LUMINANCE;
texture_format(#e3d_image{type=a8}) -> ?GL_ALPHA.

internal_format(Type) ->
    %% Compression lowers the quality, especially when bump/normal maps
    %% are compressed. Disabled for now.
%     Compress = wings_util:is_gl_ext({1,3}, 'GL_ARB_texture_compression'),
    internal_format(Type, false).

internal_format(?GL_BGR, false) -> ?GL_RGB;
internal_format(?GL_BGRA, false) -> ?GL_RGBA;
internal_format(Else, false) -> Else;
internal_format(?GL_BGR, true) -> ?GL_COMPRESSED_RGB;
internal_format(?GL_BGRA, true) -> ?GL_COMPRESSED_RGBA;
internal_format(?GL_ALPHA, true) -> ?GL_COMPRESSED_ALPHA;
internal_format(?GL_LUMINANCE, true) -> ?GL_COMPRESSED_LUMINANCE;
internal_format(?GL_LUMINANCE_ALPHA, true) -> ?GL_COMPRESSED_LUMINANCE_ALPHA;
internal_format(?GL_INTENSITY, true) -> ?GL_COMPRESSED_INTENSITY;
internal_format(?GL_RGB, true) ->  ?GL_COMPRESSED_RGB;
internal_format(?GL_RGBA, true) -> ?GL_COMPRESSED_RGBA;  
internal_format(Else, _) -> Else.

delete(Id, #ist{images=Images0}=S) ->
    delete_bump(Id),
    gl:deleteTextures(1, [erase(Id)]),
    Images = gb_trees:delete(Id, Images0),
    S#ist{images=Images}.

delete_older(Id, #ist{images=Images0}=S) ->
    Images1 = delete_older_1(gb_trees:to_list(Images0), Id),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_older_1([{Id,_}|T], Limit) when Id < Limit ->
    delete_bump(Id),
    gl:deleteTextures(1, [erase(Id)]),
    delete_older_1(T, Limit);
delete_older_1(Images, _) -> Images.

delete_from(Id, #ist{images=Images0}=S) ->
    Images1 = delete_from_1(gb_trees:to_list(Images0), Id, []),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_from_1([{Id,_}=Im|T], Limit, Acc) when Id < Limit ->
    delete_from_1(T, Limit, [Im|Acc]);
delete_from_1([{Id,_}|T], Limit, Acc) ->
    delete_bump(Id),
    gl:deleteTextures(1, [erase(Id)]),
    delete_from_1(T, Limit, Acc);
delete_from_1([], _, Acc) -> reverse(Acc).

delete_bump(Id) ->
    TxId = get(Id), 
    case erase({Id,bump}) of
	undefined -> ok;
	TxId -> ok;
	Bid ->  gl:deleteTextures(1, [Bid])
    end.

do_update(Id, In = #e3d_image{width=W,height=H,type=Type,name=NewName}, 
	  #ist{images=Images0}=S) ->
    Im0 = #e3d_image{filename=File,name=OldName} = gb_trees:get(Id, Images0),
    Name = if is_list(NewName), length(NewName) > 2 -> NewName;
	      true -> OldName
	   end,	       
    Im   = maybe_convert(In#e3d_image{filename=File, name=Name}),
    TxId = get(Id),
    Images = gb_trees:update(Id, Im, Images0),
    Size = {Im0#e3d_image.width, Im0#e3d_image.height, Im0#e3d_image.type},
    case Size of
	{W,H,Type} ->
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texSubImage2D(?GL_TEXTURE_2D, 0, 0, 0,
			     W, H, texture_format(Im), 
			     ?GL_UNSIGNED_BYTE, Im#e3d_image.image);
	_ ->	    
	    init_texture(Im, TxId)
    end,
    case get({Id,bump}) of
	undefined ->     
	    S#ist{images=Images};
	Bid -> 
	    create_bump(Id, Bid, S#ist{images=Images}),
	    S#ist{images=Images}
    end.

make_unique(Name, Images0) ->
    Images = [N || #e3d_image{name=N} <- gb_trees:values(Images0)],
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
	    {Size,Title} = window_params(Id),
	    Pos = {10,50,highest},
	    Op = {seq,push,window_fun(Id)},
	    Props = window_props(),
	    wings_wm:toplevel(Name, Title, Pos, Size,
			      [resizable,closable,{properties,Props}], Op),
	    wings_wm:send(Name, {action,{viewer,100}})
    end.

window_params(Id) ->
    #e3d_image{width=W0,height=H0,name=Name,bytes_pp=BytesPerPixel} = info(Id),
    Title = flatten(io_lib:format(?__(1,"Image: ~s [~wx~wx~w]"),
				  [Name,W0,H0,8*BytesPerPixel])),
    {DeskW,DeskH} = wings_wm:win_size(desktop),
    W = if
	    W0 < 250 -> 250;
	    W0+50 < DeskW -> W0+2;
	    true -> DeskW - 50
	end,
    H = if
	    H0+70 < DeskH -> H0+2;
	    true -> DeskH - 70
	end,
    {{W,H},Title}.

window_props() ->
    View = #view{origin={0.0,0.0,0.0},
		 distance=0.65,
		 azimuth=0.0,
		 elevation=0.0,
		 pan_x=0.0,
		 pan_y=0.0,
		 fov=90.0,
		 hither=0.001,
		 yon=100.0},
    [{current_view,View},
     {orthogonal_view,true},
     {allow_rotation,false},
     {hide_sel_in_camera_moves,false}].

window_fun(Id) ->
    fun(Ev) ->
	    event(Ev, Id)
    end.

event(redraw, Id) ->
    redraw(Id),
    keep;
event(close, _) ->
    delete;
event(got_focus, _) ->
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [],?__(1,"Show menu")),
    Message = wings_msg:join([Msg2,Msg3]),
    wings_wm:message(Message),
    wings_wm:dirty();
event({action,{viewer,Cmd}}, Id) ->
    command(Cmd, Id);
event(Ev, Id) ->
    case wings_camera:event(Ev, #st{shapes=gb_trees:empty()},fun() -> redraw(Id) end) of
	next -> event_1(Ev, Id);
	Other -> Other
    end.

event_1(Ev, _) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    Menu = [{"12%",12},
		    {"25%",25},
		    {"50%",50},
		    separator,
		    {"100%",100,?__(1,"Show in natural size")},
		    separator,
		    {"200%",200},
		    {"400%",400},
		    {"800%",800}
		   ],
	    wings_menu:popup_menu(X, Y, viewer, Menu)
    end.

command(Percent, Id) when is_integer(Percent) ->
    View = wings_view:current(),
    #e3d_image{width=Iw,height=Ih} = info(Id),
    {_,H} = wings_wm:win_size(),
    Dist = 100/2*H/Ih/Percent,
    PanX = -(Iw/Ih/2),
    PanY = -0.5,
    wings_view:set_current(View#view{distance=Dist,pan_x=PanX,pan_y=PanY}),
    wings_wm:dirty(),
    keep.
    
redraw(Id) ->
    case info(Id) of
	none ->
	    wings_wm:later(close),
	    keep;
	Im -> redraw_1(Id, Im)
    end.

redraw_1(Id, #e3d_image{width=Iw,height=Ih}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_wm:clear_background(),

    wings_view:load_matrices(false),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:disable(?GL_DEPTH_TEST),
    draw_background(0, 0, Iw/Ih, 1),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    draw_image(Iw/Ih, 1, txid(Id)),
    gl:bindTexture(?GL_TEXTURE_2D, 0),

    %% Draw window border.
    wings_io:ortho_setup(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    {W,H} = wings_wm:win_size(),
    gl:rectf(0.5, 0.5, W-0.5, H-0.5),
    #view{distance=Dist} = wings_view:current(),

    gl:popAttrib(),

    %% Info line.
    Percent = 100/2*H/Ih/Dist,
    wings_io:info(io_lib:format("~.2f%", [Percent])).

draw_background(X, Y, W, H) ->
    {Wwin,Hwin} = wings_wm:win_size(),
    Ua = 0,
    Va = 0,
    Ub = Wwin div 16,
    Vb = Hwin div 16,
    gl:bindTexture(?GL_TEXTURE_2D, txid(background)),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(Ua, Va),
    gl:vertex2f(X, Y),
    gl:texCoord2f(Ua, Vb),
    gl:vertex2f(X, Y+H),
    gl:texCoord2f(Ub, Vb),
    gl:vertex2f(X+W, Y+H),
    gl:texCoord2f(Ub, Va),
    gl:vertex2f(X+W, Y),
    gl:'end'().

draw_image(W, H, TxId) ->
    Ua = 0, Ub = 1,
    Va = 0, Vb = 1,
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2i(Ua, Va),
    gl:vertex2f(0, 0),
    gl:texCoord2i(Ua, Vb),
    gl:vertex2f(0, H),
    gl:texCoord2i(Ub, Vb),
    gl:vertex2f(W, H),
    gl:texCoord2i(Ub, Va),
    gl:vertex2f(W, 0),
    gl:'end'().

draw_image(X, Y, W, H, TxId) ->
    Ua = 0, Ub = 1,
    Va = 1, Vb = 0,
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2i(Ua, Va),
    gl:vertex2i(X, Y),
    gl:texCoord2i(Ua, Vb),
    gl:vertex2i(X, Y+H),
    gl:texCoord2i(Ub, Vb),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2i(Ub, Va),
    gl:vertex2i(X+W, Y),
    gl:'end'().

%%%
%%% Creating images with pre-defined patterns.
%%%

create_image() ->
    Qs = [{?__(1,"Width"),256,[{range,{8,1024}}]},
	  {?__(2,"Height"),256,[{range,{8,1024}}]},
	  {?__(3,"Pattern"),
	   {menu,[{?__(4,"Grid"),grid},
		  {?__(5,"Checkerboard"),checkerboard},
		  {?__(6,"Vertical Bars"),vbars},
		  {?__(7,"Horizontal Bars"),hbars},
		  {?__(8,"White"),white},
		  {?__(9,"Black"),black}],
	    grid}}],
    wings_ask:ask(?__(10,"Create Image"), Qs,
		  fun([W,H,Pattern]) ->
			  create_image_1(Pattern, W, H),
			  ignore
		  end).

create_image_1(Pattern, W, H) ->
    Pixels = pattern(Pattern, W, H),
    case {size(Pixels),3*W*H} of
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

%% Creating Normal-Cubemap

%% Initialize a cube map texture object that generates RGB values
%% that when expanded to a [-1,1] range in the texture-unit
%% form a normalized vector matching the per-pixel vector used to
%% access the cube map.

make_normalize_vector_cubemap(Size) ->
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_WRAP_R, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),

    Sides = [?GL_TEXTURE_CUBE_MAP_POSITIVE_X, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
	     ?GL_TEXTURE_CUBE_MAP_POSITIVE_Y, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
	     ?GL_TEXTURE_CUBE_MAP_POSITIVE_Z, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z],
    lists:foreach(fun(Side) -> make_cube_map(Side,Size-1,Size-1,Size, []) end, Sides).

make_cube_map(Side,-1,-1,Size,Acc) ->
    Image = list_to_binary(Acc),
    gl:texImage2D(Side, 0, ?GL_RGB8, Size, Size, 0, 
		  ?GL_RGB, ?GL_UNSIGNED_BYTE, Image);
make_cube_map(Side,-1,Y,Size,Acc) ->
    make_cube_map(Side,Size-1,Y-1,Size,Acc);
make_cube_map(Side,X,Y,Size,Acc) ->
    Vec = get_cube_vec(Side,Size,X,Y),
    make_cube_map(Side,X-1,Y,Size,[Vec|Acc]).

%% Given a cube map face index, cube map size, and integer 2D face position,
%% return the cooresponding normalized vector.
get_cube_vec(Side, Size, X, Y) ->
    S = (X + 0.5) / Size,
    T = (Y + 0.5) / Size,    
    SC = S*2.0 - 1.0,    
    TC = T*2.0 - 1.0,
    Vec = 
	case Side of
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_X -> {1.0,  -TC, -SC};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_X -> {-1.0, -TC,  SC};
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_Y -> {SC,   1.0,  TC};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y -> {SC,  -1.0, -TC};
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_Z -> {SC,   -TC, 1.0};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z -> {-SC,  -TC,-1.0}
	end,
    {RX,RY,RZ} = e3d_vec:norm(Vec),
    [round(128+127*RX),round(128+127*RY),round(128+127*RZ)].
