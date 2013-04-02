%%
%%  wpc_image.erl --
%%
%%     Image plane plug-in
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL,1).
-include("wings.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").


-import(lists, [reverse/1]).

-define(DEF_NAME, "image").
-define(IP_HELPER_SIZE, 128).

init() ->
    true.

menu({shape}, Menu) ->
    insert_before_image(Menu);
menu(_, Menu) -> Menu.

insert_before_image([{_,image,_}=Image|Rest]) ->
    [Image,image_menu(),separator|Rest];
insert_before_image([H|T]) ->
    [H|insert_before_image(T)];
insert_before_image([]) ->
    [image_menu()].

image_menu() ->
    {?__(1,"Image Plane..."),image_plane,?__(2,"Create a plane containing an image"),[option]}.

command({shape,{image_plane,Ask}}, _St) when is_atom(Ask) ->
    make_image(Ask);
command({shape,{image_plane,{FileName,Ask}}}, St) when is_atom(Ask) ->
    make_image(FileName,Ask,St);
command({shape,{image_plane,{params,{ImData,Arg}}}}, St) ->
    make_image_0(ImData,Arg,St);
command(_, _) -> next.

make_image(Ask) ->
    Ps = [{extensions,wpa:image_formats()}],
    wpa:import_filename(Ps, fun(N) -> {shape,{image_plane,{N,Ask}}} end).

make_image(Name,Ask,St) when is_atom(Ask) ->
    Props = [{filename,Name}],
    case wpa:image_read(Props) of
    #e3d_image{}=Image ->
        make_image(Name,Image,Ask,St);
    {error,Error} ->
        wpa:error_msg(?__(1,"Failed to load \"~s\": ~s\n"),
              [Name,file:format_error(Error)])
    end.

make_image(FileName, Image0, false=Ask, _St) ->
    case load_img_plane(FileName,Image0) of
    {error,_} -> ok;
    ImData ->
        Id_Img_Helper = load_ip_helper(),
        Qs = image_dialog(FileName, Id_Img_Helper),
        wings_ask:dialog(Ask, ?__(2,"Image Plane"), [{vframe,Qs}],
        fun(Res) ->
            {shape,{image_plane,{params,{ImData,Res}}}}
        end)
    end;

make_image(FileName, Image0, Ask, St) when is_atom(Ask) ->
    case load_img_plane(FileName,Image0) of
    {error,_} -> ok;
    {Id_Img_Plane,_,_}=ImData ->
        Id_Img_Helper = load_ip_helper(),
        wings_ask:dialog(?__(2,"Image Plane"), {preview,image_dialog(FileName, Id_Img_Helper)},
          fun
            ({dialog_preview,Res}) ->
                St1 = make_image_0(ImData,Res,St),
                {preview,St1,St1};
            (cancel) ->
                unload_img_plane(Id_Img_Plane,St),
                wings_image:delete(Id_Img_Helper),
                St;
            (Res) ->
                St1 = make_image_0(ImData,Res,St),
                {commit,St1,St1}
          end)
    end.

image_dialog(FileName, Tx_helper) ->
    Owner = wings_wm:this(),
    FName = filename:rootname(filename:basename(FileName)),
    Disable_Hook = fun(Event, Params) ->
        case Event of
        is_disabled ->
            {Var,_,Store}=Params,
            case Var of
            rotation ->
                gb_trees:get(alignment,Store)=:=view;
            img_name ->
                gb_trees:get(usename,Store)=:=for_mat_obj;
            _ ->
                false
            end;
        _ ->
            void
        end
    end,
    Draw_Helper = fun(X, Y, W, H, _) ->
        draw_img_helper(X, Y, W, H, Tx_helper),
        keep
    end,
    [
            {hframe,[
                {vradio,[{?__(3,"Front"),front},{?__(5,"Right"),right},{?__(7,"Top"),top},
                         {?__(4,"Back"),back},{?__(6,"Left"),left},{?__(8,"Bottom"),bottom},{?__(9,"View"),view}],front,
                    [{title,?__(10,"Aligned with...")},{info, ?__(11,"Location in accord with placement helper (image on the right)")},{key,alignment}]},
                {custom,?IP_HELPER_SIZE+?CHAR_HEIGHT+2,?IP_HELPER_SIZE+2,Draw_Helper}
            ]},
            {hradio,[{?__(12,"Material "),for_mat},{?__(13,"Material and Object "),for_mat_obj},{?__(14,"None "),for_none}],for_none,
                [{title,?__(15,"Use image name for...")},{info, ?__(16,"To allow the image name be used for the image plane material and/or object")},{key,usename}]},
            {hframe,[
                {label,?__(17,"Name")},
                {text,?DEF_NAME,[{info, ?__(18,"Name for the image plane object")},{width,25},{hook,Disable_Hook},{key,img_name}]}
            ]},
            {value,FName,[{key,fname}]},
            {value,Owner,[{key,owner}]},
            {hframe,[
                {hframe,[
                    {label,?__(19,"Offset")},
                    {text,0.0,[{info, ?__(20,"Offset from origin (positive values only)")},{range,{0.0,infinity}},{key,offset},{width,7}]},
                    {label," "}]},
                {hframe,[
                    {label,?__(21,"Rotation")}]},
                    {text,0.0,[{info, ?__(22,"Rotation around the origin (positive is counterclockwise)")},{hook,Disable_Hook},{key,rotation},{width,7}]}
            ]},
            separator,
            {?__(23,"Lock after create"),false,[{info, ?__(24,"Lock image plane object")},{key,locked}]},  
            {?__(25,"Transparent back face"),false,[{info, ?__(26,"Assign transparent material to back face")},{key,transp}]}  
        ].

make_image_0({ImageId, {MaxU,MaxV}, {AspX,AspY}}, Arg, #st{mat=Mat0}=St) ->
    ArgDict = dict:from_list(Arg),
    Alignment = dict:fetch(alignment,ArgDict),
    UseName = dict:fetch(usename,ArgDict),
    ImgName = dict:fetch(img_name,ArgDict),
    FName = dict:fetch(fname,ArgDict),
    Offset = dict:fetch(offset,ArgDict),
    Rotation = dict:fetch(rotation,ArgDict),
    Owner = dict:fetch(owner,ArgDict),
    Lock = dict:fetch(locked,ArgDict),
    Transparent = dict:fetch(transp,ArgDict),

    MatName = if UseName == for_none -> ?DEF_NAME;
        true -> FName end,
    MatId = list_to_atom(MatName),
    Mt = if Transparent==true -> [transparency_ip];
        true -> [] end,
    Mi = [MatId],
    Fs = [#e3d_face{vs=[0,3,2,1],mat=Mt},
      #e3d_face{vs=[2,3,7,6],tx=[6,2,3,7],mat=Mi},
      #e3d_face{vs=[0,4,7,3],mat=Mt},
      #e3d_face{vs=[1,2,6,5],mat=Mt},
      #e3d_face{vs=[4,5,6,7],mat=Mt},
      if Transparent==true -> #e3d_face{vs=[0,1,5,4],mat=Mt};
      true -> #e3d_face{vs=[0,1,5,4],tx=[4,0,1,5],mat=Mi} end ],

    UVs = [{0.0,MaxV},{MaxU,MaxV},{0.0,0.0},{MaxU,0.0},
           {0.0,0.0},{MaxU,0.0},{0.0,MaxV},{MaxU,MaxV}],
    HardEdges = [{0,3},{2,3},{1,2},{0,1},{3,7},{6,7},
                 {2,6},{0,4},{4,7},{4,5},{5,6},{1,5}],
    D = 1.0e-3/2,
    Vs0 = [{-D,-AspY,AspX},{-D,AspY,AspX},{D,AspY,AspX},{D,-AspY,AspX},
          {-D,-AspY,-AspX},{-D,AspY,-AspX},{D,AspY,-AspX},{D,-AspY,-AspX}],

    Vs=do_new_place(Alignment, if Alignment=:=view -> Owner;
                                true -> Rotation end, Offset, Vs0),
    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UVs,he=HardEdges},
    Obj = #e3d_object{obj=Mesh},
    White = wings_color:white(),
    Black = wings_color:white(),
    WhiteT = if Transparent==true -> {1.0,1.0,1.0,0.999};
        true -> White end,
    M = [{MatId,
            [{opengl,[{emission,White},
                  {diffuse,WhiteT},
                  {specular,Black},
                  {ambient,Black}]},
             {maps,[{diffuse,ImageId}]}]}],

    Mat = if Transparent==true -> lists:flatten(lists:append(M,get_transp_mat(Mat0)));
        true -> M end,

    ImgName0 = case UseName of
        for_mat_obj -> FName;
        _ ->
            if ImgName==?DEF_NAME -> object_name(ImgName, St);
            true -> ImgName end
    end,

    File = #e3d_file{objs=[Obj#e3d_object{name=ImgName0,mat=Mat}]},
    #st{shapes=Shapes}=St0=wings_import:import(File, St),
    St0#st{shapes=lock_image(Lock,ImgName0,Shapes)}.

object_name(Prefix, #st{onext=Oid}) ->
    Prefix++integer_to_list(Oid).

lock_image(false,_,Shapes) -> Shapes;
lock_image(true,Name,Shapes) ->
    gb_trees:map(fun(_,#we{name=Name0}=We) ->
        if Name==Name0 ->
            We#we{perm=1};
        true -> We end
    end, Shapes).

get_transp_mat(Mat) ->
    case gb_trees:lookup(transparency_ip, Mat) of
    none ->
        M = gb_trees:get(default, Mat),
        OpenGL0 = proplists:get_value(opengl, M, []),
        OpenGL1 = lists:foldr(fun({vertex_colors,_}, Acc) ->
                [{vertex_colors,ignore} | Acc];
            ({diffuse,{R,G,B,_}}, Acc) ->
                [{diffuse,{R,G,B,0.0}} | Acc];
            (Prop, Acc) ->
                [Prop | Acc]
        end, [],OpenGL0),
        [{transparency_ip, lists:foldr(fun({opengl,_}, Acc) ->
                [{opengl,OpenGL1} | Acc];
            (Prop, Acc) ->
                [Prop | Acc]
        end, [],M)}];
    {_,M} -> 
        [{transparency_ip,M}]
    end.

do_new_place(view, Owner, Offset, VsPos) ->
    #view{azimuth=Az,elevation=El} = wings_wm:get_prop(Owner,current_view),
    Mv0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    Mv = e3d_mat:mul(Mv0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    Vnormal=e3d_vec:norm(e3d_mat:mul_point(Mv, {0.0,0.0,1.0})),
    
    Me = e3d_mat:rotate(-El, {0.0,0.0,1.0}),
   % this will make the mapped face point to origin 
    Mr = e3d_mat:mul(Me,e3d_mat:rotate(180.0, {0.0,1.0,0.0})),
    Ma = e3d_mat:rotate(-Az+90, {0.0,1.0,0.0}),

    {Cx,Cy,Cz}=e3d_vec:mul(Vnormal,-Offset),
    Mt = e3d_mat:translate(Cx,Cy,Cz),
    do_new_place_2(VsPos,Mr,Ma,Mt);
do_new_place(front, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,1.0,0.0}, -90.0, Rotation, e3d_vec:mul({0.0,0.0,1.0},-Offset), VsPos);
do_new_place(back, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,1.0,0.0}, +90.0, Rotation, e3d_vec:mul({0.0,0.0,1.0},Offset), VsPos);
do_new_place(right, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,1.0,0.0}, 0.0, Rotation, e3d_vec:mul({1.0,0.0,0.0},-Offset), VsPos);
do_new_place(left, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,1.0,0.0}, 180.0, Rotation, e3d_vec:mul({1.0,0.0,0.0},Offset), VsPos);
do_new_place(top, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,0.0,1.0}, +90.0, Rotation, e3d_vec:mul({0.0,1.0,0.0},-Offset), VsPos);
do_new_place(bottom, Rotation, Offset, VsPos) ->
    do_new_place_1({0.0,0.0,1.0}, -90.0, Rotation, e3d_vec:mul({0.0,1.0,0.0},Offset), VsPos).

do_new_place_1(Axis, Angle, Rotation, {Cx,Cy,Cz}, VsPos) ->
    Mr = e3d_mat:rotate(Angle,Axis),
    Mt = e3d_mat:translate(Cx,Cy,Cz),
    Mra = e3d_mat:rotate(Rotation,Axis),
    do_new_place_2(VsPos,Mr,Mt,Mra).

do_new_place_2(VsPos,M1,M2,M3) ->
    M0 = e3d_mat:mul(M2,M1),
    M = e3d_mat:mul(M3,M0),
    lists:foldr(fun(Pos0, Acc) ->
          Pos = e3d_mat:mul_point(M,Pos0),
          [Pos|Acc]
      end, [], VsPos).

draw_img_helper(X, Y, W, H, none) ->
    draw_img_helper_1(X, Y, W, H);
draw_img_helper(X, Y, W, H, Id_Img_Helper) ->
    draw_img_helper_1(X, Y, W, H),
    Dx=(W-?IP_HELPER_SIZE) div 2,
    Dy=(H-?IP_HELPER_SIZE) div 2,
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_image:draw_preview(X+Dx+1,Y+Dy+1,?IP_HELPER_SIZE,?IP_HELPER_SIZE,Id_Img_Helper),
    gl:disable(?GL_TEXTURE_2D).

draw_img_helper_1(X, Y, W, H) ->
    Y1=Y+?CHAR_HEIGHT -2,
    H1=H-?CHAR_HEIGHT -1,
    wings_io:set_color(color4_highlight()),
    wings_io:border_only(X,Y1,W,H1).

load_ip_helper() ->
    Name=wings_util:lib_dir(wings)++"/textures/ip_helper.png",
    Props = [{filename,Name}],
    case wpa:image_read(Props) of
      #e3d_image{}=Image ->
        case wings_image:new_hidden("ip_helper",Image) of
          {error,_} -> none;
          Id -> Id
        end;
      _ -> none
    end.

load_img_plane(FileName, #e3d_image{type=Type,width=W,height=H}=Image0) ->
    %% Convert to the format that wings_image wants before padding (faster).
    Name = filename:rootname(filename:basename(FileName)),
    case wings_image:maybe_exceds_opengl_caps(Image0) of
    {error,GlErr} ->
        wpa:error_msg(?__(1,"The image cannot be loaded as a texture.~nFile: \"~s\"~n GLU Error: ~p - ~s~n"),
              [FileName,GlErr, glu:errorString(GlErr)]);
    Image0i ->
        Image1 = e3d_image:convert(Image0i, img_type(Type), 1, lower_left),
        #e3d_image{width=W0,height=H0} = Image1,
        Image = case pad_image(Image1) of
            Image1 ->
                %% The image was not padded, therefore it is safe
                %% to keep the image as external.
                Image0;
            Image2 ->
                %% The image has been padded. We must make sure that
                %% the new image is saved - so force it to be internal.
                Image2#e3d_image{filename=none}
            end,
        MaxU = W0/W,
        MaxV = H0/H,
        {wings_image:new(Name,Image),{MaxU,MaxV},ratio(W0, H0)}
    end.

unload_img_plane(Id, St) ->
    %% remove the image loaded for preview if it was not in use before dialog be opened
    Used = wings_material:used_images(St),
    case gb_sets:is_member(Id, Used) of
      false -> wings_image:delete(Id);
      _ -> ok
    end.

color4_highlight() ->
    wings_color:mix(?BEVEL_HIGHLIGHT_MIX, {1,1,1}, wings_pref:get_value(dialog_color)).

ratio(D, D) -> {1.0,1.0};
ratio(W, H) when W < H -> {1.0,H/W};
ratio(W, H) -> {W/H,1.0}.
    
pad_image(#e3d_image{width=W0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(W0) of
    W0 ->
        pad_image_1(Image);
    W ->
        Pad = zeroes(PP*(W-W0)),
        Pixels = pad_rows(Pixels0, PP*W0, Pad, []),
        pad_image_1(Image#e3d_image{width=W,image=Pixels})
    end.

pad_image_1(#e3d_image{width=W,height=H0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(H0) of
    H0 ->
        pad_image_2(Image);
    H ->
        Pad = zeroes(PP*W*(H-H0)),
        Pixels = [Pixels0|Pad],
        pad_image_2(Image#e3d_image{height=H,image=Pixels})
    end.

pad_image_2(#e3d_image{image=Pixels}=Image) when is_list(Pixels) ->
    Image#e3d_image{image=list_to_binary(Pixels)};
pad_image_2(Image) -> Image.

pad_rows(Bin, W, Pad, Acc) ->
    case Bin of
    <<>> -> reverse(Acc);
    <<Row:W/binary,T/binary>> ->
        pad_rows(T, W, Pad, [[Row|Pad]|Acc])
    end.

zeroes(0) -> [];
zeroes(1) -> [0];
zeroes(N) when N rem 2 =:= 0 ->
    Z = zeroes(N div 2),
    [Z|Z];
zeroes(N) ->
    Z = zeroes(N div 2),
    [0,Z|Z].

nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

img_type(b8g8r8) -> r8g8b8;
img_type(b8g8r8a8) -> r8g8b8a8;
img_type(Type) -> Type.
