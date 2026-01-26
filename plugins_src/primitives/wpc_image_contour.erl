
%%
%%  wpc_image_contour.erl --
%%
%%     Create a contoured 3D shape from an image.
%%
%%  Copyright (c) 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_image_contour).
-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").


init() -> true.


menu({shape},Menu) ->
    submenu(Menu);
menu({shape,image_contour},Menu) ->
    Menu ++ [
        {"Monochrome Mask...",image_mask,[option]},
        {"Transparent Image...",image_alpha,[option]}
    ];
menu(_,Menu) ->
    Menu.

submenu([{_,{image_contour,_}}|_]=Menu) ->
    Menu;
submenu([Item|Menu]) when element(2,Item) =:= image_plane ->
    [Item|submenu_1()] ++ Menu;
submenu([Item|Menu]) ->
    [Item|submenu(Menu)];
submenu([]) ->
    submenu_1().

submenu_1() ->
    [{"Image Contour",{image_contour,[]}}].

%%

command({shape,{image_contour,{image_mask,Ask}}},_St) when is_atom(Ask) ->
    make_mesh(image_mask,Ask);
command({shape,{image_contour,{image_alpha,Ask}}},_St) when is_atom(Ask) ->
    make_mesh(image_alpha,Ask);
command({shape,{image_contour,{image_mask,{Filename,Ask}}}},St) when is_atom(Ask) ->
    make_mesh(image_mask,Filename,Ask,St);
command({shape,{image_contour,{image_alpha,{Filename,Ask}}}},St) when is_atom(Ask) ->
    make_mesh(image_alpha,Filename,Ask,St);
command(_, _) -> next.

make_mesh(Type,Ask) ->
    Ps = case Type of
        image_mask -> [{extensions,exts_all()}];
        image_alpha -> [{extensions,exts_alpha()}]
    end,
    wpa:import_filename(Ps, fun(N) -> {shape,{image_contour,{Type,{N,Ask}}}} end).

make_mesh(Type,Filename,Ask,St) when is_atom(Ask) ->
    case wpa:image_read([{filename,Filename}]) of
        #e3d_image{}=Img ->
            make_mesh(Type,Filename,Img,Ask,St);
        {error,Error} ->
            wpa:error_msg(?__(1,"Failed to load \"~ts\": ~s\n"),
            [Filename,file:format_error(Error)])
    end.

make_mesh(Type,Filename,Img,Ask,St) when is_atom(Ask) ->
	Dlg = [{vframe,
        lists:append([
            map_type_frame(Type),
            opt_frame(Type =:= image_mask)
        ])}],
    ImageId = load_image(Type,Filename,Img),
    wings_dialog:dialog(Ask, ?__(2,"Image Contour"), {preview, Dlg},
        fun
            ({dialog_preview,Params}) ->
                St1 = make_mesh_1(Type,Filename,ImageId,Img,Params,St),
                {preview,St,St1};
            (cancel) ->
                unload_image(St, ImageId),
                St;
            (Params) ->
                St1 = make_mesh_1(Type,Filename,ImageId,Img,Params,St),
                {commit,St,St1}
        end).

map_type_frame(image_mask) ->
    [];
map_type_frame(_Type) ->
    List=[
        {?__(10,"Map Type:"),
            {menu,[
                {?__(1,"Diffuse"),diffuse},
                {?__(5,"Bump (HeightMap)"),bump},
                {?__(6,"Bump (NormalMap)"),normal},
                {?__(8,"Emission"),emission}
            ],emission,[{key,map_type}]}}
    ],
    [{label_column,List}].

opt_frame(HasInvert) ->
    OptsList0 = trace_2d:opts(),
    OptsList = case HasInvert of
        true -> OptsList0;
        false -> [Opt || Opt <- OptsList0, Opt =/= invert]
    end,
    {Checkboxes,OptsList1} = lists:partition(
        fun(Atom) -> is_boolean(trace_2d:opt_default(Atom)) end,
        OptsList),
    List = lists:map(fun (Atom) ->
        Default = trace_2d:opt_default(Atom),
        {opt_string(Atom), {text,Default,[{key,Atom}]}}
    end, OptsList1),
    Checkboxes1 = [
        {opt_string(Atom),trace_2d:opt_default(Atom),[{key,Atom}]}
    || Atom <- Checkboxes],
    [{label_column,List},{hframe,Checkboxes1}].

opt_string(budget) -> ?__(1,"Points Budget");
opt_string(invert) -> ?__(2,"Invert");
opt_string(denoise) -> ?__(3,"Denoise");
opt_string(clamp) -> ?__(4,"Clamp");
opt_string(smooth_ini) -> ?__(5,"Smooth Initial Factor");
opt_string(smooth_att) -> ?__(6,"Smooth Attenuation");
opt_string(smooth_count) -> ?__(7,"Smooth Point Count");
opt_string(smooth_disp) -> ?__(8,"Smooth Point Displacement");
opt_string(smooth_dispc) -> ?__(9,"Smooth Point Disp. Correction").

make_mesh_1(image_mask, _Filename,ImageId,Img, Opts,St) when is_list(Opts) ->
    make_mesh_image_mask(_Filename,ImageId,Img, Opts,St);
make_mesh_1(image_alpha, _Filename,ImageId,Img, Opts,St) when is_list(Opts) ->
    make_mesh_image_alpha(_Filename,ImageId,Img, Opts,St).

make_mesh_image_mask(_Filename,_,Img, Opts,St) ->
    case from_image(Img, Opts) of
        {_, []} ->
            St;
        {Vco_0, Paths} ->
            Vco_1 = array:to_list(Vco_0),
            Rescale = get_scale(Vco_1),
            Vco = [{X,-Y,Z+0.15} || {X,Y,Z} <- Vco_1] ++
                  [{X,-Y,Z-0.15} || {X,Y,Z} <- Vco_1],
            Tx = make_uv_list(Img,Vco_1),
            Offset = array:size(Vco_0),
            Faces = lists:append([make_mesh_2(P,Offset,Vco) || P <- Paths]),

            Center = e3d_vec:average(e3d_vec:bounding_box(Vco)),
            Center_1 = e3d_vec:sub(e3d_vec:zero(),Center),
            Vco_2 = [ e3d_vec:mul(e3d_vec:add(V,Center_1),Rescale) || V <- Vco],

            Mesh = #e3d_mesh{type=polygon,vs=Vco_2,tx=Tx,fs=Faces},
            Obj=#e3d_object{obj=Mesh},
            wings_import:import(#e3d_file{objs=[Obj]}, St)
    end.

make_mesh_image_alpha(_Filename,{image,ImageId},Img0, Opts,St) ->
    MapType = proplists:get_value(map_type, Opts, emission),
    {EmissionColor,DiffuseColor} = mat_colors(MapType),
    Img = alpha_to_mask(Img0),
    case from_image(Img, Opts) of
        {_, []} ->
            St;
        {Vco_0, Paths} ->
            Vco_1 = array:to_list(Vco_0),
            Rescale = get_scale(Vco_1),
            Vco = [{X,-Y,Z+0.15} || {X,Y,Z} <- Vco_1] ++
                  [{X,-Y,Z-0.15} || {X,Y,Z} <- Vco_1],
            Tx = make_uv_list(Img,Vco_1),
            Offset = array:size(Vco_0),
            Faces0 = lists:append([make_mesh_2(P,Offset,Vco) || P <- Paths]),
            Center = e3d_vec:average(e3d_vec:bounding_box(Vco)),
            Center_1 = e3d_vec:sub(e3d_vec:zero(),Center),
            Vco_2 = [ e3d_vec:mul(e3d_vec:add(V,Center_1),Rescale) || V <- Vco],

            Faces = [F#e3d_face{mat=[image_mat]} || F <- Faces0],
            Mesh = #e3d_mesh{type=polygon,vs=Vco_2,tx=Tx,fs=Faces},

            OpenGL = [
                {emission,EmissionColor},
                {diffuse,DiffuseColor},
                {metallic,0.0},
                {roughness,1.0}
            ],
            Mat = [{image_mat,[{opengl,OpenGL},{maps,[{MapType,ImageId}]}]}],
            Obj=#e3d_object{obj=Mesh,mat=Mat},
            wings_import:import(#e3d_file{objs=[Obj]}, St)
    end.

mat_colors(Type) ->
    Col1 = wings_color:white(),
    Col2 = {0.0,0.0,0.0,1.0},
    case Type of
        emission -> {Col1, Col2};
        _ -> {Col2, Col1}
    end.

    

load_image(image_mask, _Filename, _Img) ->
    none;
load_image(_, Filename, Img) ->
    Name = filename:rootname(filename:basename(Filename)),
    {image, wings_image:new(Name,Img)}.

unload_image(_St, none) ->
    ok;
unload_image(St, {image,Id}) ->
    case gb_sets:is_member(Id, wings_material:used_images(St)) of
        false ->
            wings_image:delete(Id);
        _ ->
            ok
    end.



make_uv_list(#e3d_image{width=W0,height=H0}=_Img,Vco_1) ->
    UV_W = 1.0 / float(W0),
    UV_H = 1.0 / float(H0),
    [{X*UV_W,-Y*UV_H} || {X,Y,_} <- Vco_1].

from_image(Img, Opts) ->
    try
        trace_2d:from_image(Img, Opts)
    catch
        _:_ -> {array:new(), []}
    end.


alpha_to_mask(Img0) ->
    Img1=e3d_image:convert(Img0, a8, 1, upper_left),
    Img1#e3d_image{type=g8}.


make_mesh_2([Main|Holes], Offset, Vco) ->
    TopFaces = e3d__tri_quad:quadrangulate_face_with_holes(
        #e3d_face{vs=Main},
        [#e3d_face{vs=H} || H <- Holes],
        Vco),
    BotFaces0 = e3d__tri_quad:quadrangulate_face_with_holes(
        #e3d_face{vs=[Vs+Offset || Vs <- Main]},
        [#e3d_face{vs=[Vs+Offset || Vs <- H]} || H <- Holes],
        Vco),
    BotFaces = [
        F#e3d_face{vs=lists:reverse(V)}
        || F=#e3d_face{vs=V} <- BotFaces0],

    Sides =
        make_mesh_sides(Main, Offset) ++
        lists:append([ make_mesh_sides(H, Offset) || H <- Holes]),
    add_uv(TopFaces ++ BotFaces ++ Sides, Offset).

add_uv(List, Offset) ->
    [add_uv_1(F, Offset) || F <- List].
add_uv_1(#e3d_face{vs=Vs}=F, Offset) ->
    F#e3d_face{tx=[V rem Offset || V <- Vs]}.

make_mesh_sides([V|_]=List, Offset) ->
    make_mesh_sides(List, V, Offset, []).
make_mesh_sides([V], First, Offset, OL) ->
    [#e3d_face{vs=[V,First,First+Offset,V+Offset]}|OL];
make_mesh_sides([V1|[V2|_]=List], First, Offset, OL) ->
    OL1 = [#e3d_face{vs=[V1,V2,V2+Offset,V1+Offset]}|OL],
    make_mesh_sides(List, First, Offset, OL1).


get_scale(Vco) ->
    {X1,Y1,Z1} = lists:foldl(fun ({X,Y,Z}, {MX,MY,MZ}) ->
        {get_scale_1(X,MX),get_scale_1(Y,MY),get_scale_1(Z,MZ)}
    end, {{100000.0,0.0},{100000.0,0.0},{100000.0,0.0}}, Vco),
    X2 = get_scale_2(X1),
    Y2 = get_scale_2(Y1),
    Z2 = get_scale_2(Z1),
    Max = max(X2,max(Y2,Z2)),
    if
        Max < 1.0 ->
            2.0;
        true ->
            2.0 / Max
    end.

get_scale_1(X, {Min,Max}) ->
    {min(Min,X),max(Max,X)}.

get_scale_2({Min,Max}) ->
    Max-Min.

exts_all() ->
    wpa:image_formats().

%% file extensions with transparency support
exts_alpha() ->
    List = exts_all(),
    lists:foldl(
        fun (RemExt, Acc) -> proplists:delete(RemExt, Acc) end, List,
        [".bmp",".jpg"]).




