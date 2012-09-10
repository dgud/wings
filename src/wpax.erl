%%
%%  wpax.erl --
%%
%%     Wings Plugin API Extended.
%%
%%  Copyright (c) 2012 Micheus L Vieira
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpax).
-export([scr2d_to_pnt3d/4,get_proj_matrix_inv/0,orient_vec/1,orient_len/1]).
-export([extract_alpha/1,prepare_img2cart/1]).
-export([face2edges/2,face2edges/4,face2edges_col/5,face2vertice/4,grayscale_image/2]).
-export([load_texture/1,release_texture/1,texture_format/1,internal_format/1]).

-define(NEED_OPENGL, 1).

-include("wpax.hrl").
-include("e3d_image.hrl").
-include_lib("wings.hrl").

%% It computes the 3d position for a point by projecting 2d screen   
%% coordenate in the plane defined by the vertice (V) and its normal (Vn)
%% using the current space transformation. (used by Sculpt Brush code)
scr2d_to_pnt3d(X0,Y0,V,Vn) ->
    {W,H} = wings_wm:win_size(),
    Wc=trunc((W+1)/2),
    Hc=trunc((H+1)/2),
    X=adjust_offset((X0-Wc)/Wc),
    Y=adjust_offset(((H-Y0+1)-Hc)/Hc),
    
    PMi = get_proj_matrix_inv(),
    
    {Xa,Ya,Za,Sa}=e3d_mat:mul(PMi,{X,Y,-1.0,1.0}),
    PosA=e3d_vec:mul({Xa,Ya,Za},1.0/Sa),
    {Xb,Yb,Zb,Sb}=e3d_mat:mul(PMi,{X,Y,0.0,1.0}),
    PosB=e3d_vec:mul({Xb,Yb,Zb},1.0/Sb),
    
    Dir=e3d_vec:norm(e3d_vec:sub(PosB,PosA)),
    
    case e3d_vec:dot(Dir,Vn) of
    0.0 ->
    	io:format("screen2d_to_point3d got 0.0",[]),
        Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn),
        e3d_vec:add(PosB, e3d_vec:mul(Vn, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn) / Dot,
        e3d_vec:add(PosB, e3d_vec:mul(Dir, Intersection))
    end.

adjust_offset(N) when N < 0.0 -> N+1.0; 
adjust_offset(N) -> N. 

%% computes the inverse projection matrix for the current space transformation.
get_proj_matrix_inv() ->
    ModelMatrix = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
    ProjMatrix = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
    e3d_mat:invert(e3d_mat:mul(ProjMatrix, ModelMatrix)).

%% build the orientation vector
orient_vec(#seg_inf{va=Vla,vb=none,vc=Vlc}) ->
    orient_vec_1(Vla,Vlc);
orient_vec(#seg_inf{va=_,vb=Vlb,vc=Vlc}) ->
    orient_vec_1(Vlb,Vlc).
orient_vec_1(#vl{v=Va},#vl{v=Vb}) ->
    e3d_vec:norm_sub(Vb,Va).

%% get the length of the orientation vector
orient_len(#seg_inf{va=Vla,vb=none,vc=Vlc}) ->
    orient_len_1(Vla,Vlc);
orient_len(#seg_inf{va=Vla,vb=Vlb,vc=none}) ->
    orient_len_1(Vla,Vlb);
orient_len(#seg_inf{va=_,vb=Vlb,vc=Vlc}) ->
    orient_len_1(Vlb,Vlc).
orient_len_1(#vl{v=Va},#vl{v=Vb}) ->
    e3d_vec:len(e3d_vec:sub(Vb,Va)).

%% 
prepare_img2cart({W0,H0,Img0}) ->
    H0h=H0 div 2,
    W0h=W0 div 2,
    {H,Img1}= if ((H0 rem 2)==0) -> {H0+1,add_row_zero(Img0,W0)};
    true -> {H0,Img0}
    end,
    {W,ImgLst}= if ((W0 rem 2)==0) -> {W0+1,add_col_zero(Img1,W0)};
    true -> {W0,Img1}
    end,
    ZeroOfs=W*(H div 2)+(W div 2)+1,
    {W,H,W0h,H0h,ZeroOfs,ImgLst}.

add_row_zero(Img,W) ->
    Nl=list_to_binary(lists:duplicate(W, 0)),
    <<Img/binary,Nl/binary>>.

add_col_zero(Img,W) ->
    add_col_zero_0(W,Img,<<>>).

add_col_zero_0(_,<<>>,Acc) -> Acc;
add_col_zero_0(W,Bin,Acc) ->
    <<R:W/binary,T/binary>> =Bin,
    Zero=list_to_binary([0]),
    add_col_zero_0(W,T,<<Acc/binary,R/binary,Zero/binary>>).

face2edges(Faces, #we{es=Etab,vp=Vtab}=We) ->
    Extract = fun(Face, _V, Edge, _E, Acc0) ->
		to_edges_raw_1(Edge, Etab, Vtab, Acc0, Face, Edge, not_done)
	end,
    wings_face:fold_faces(Extract, [], Faces, We).

face2edges(Faces, Ftab, Etab, Vtab) ->
    to_edges_raw(Faces, Ftab, Etab, Vtab, []).

to_edges_raw([Face|Faces], Ftab, Etab, Vtab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_edges_raw_1(Edge, Etab, Vtab, Acc0, Face, Edge, not_done),
    to_edges_raw(Faces, Ftab, Etab, Vtab, Acc);
to_edges_raw([], _, _, _, Acc) -> Acc.

to_edges_raw_1(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
to_edges_raw_1(Edge, Etab, Vtab, Acc, Face, LastEdge, _) ->
    case array:get(Edge, Etab) of
	#edge{vs=Va0,ve=Vb0,lf=Face,ltsu=NextEdge} ->
        VsPair=[array:get(Va0, Vtab),array:get(Vb0, Vtab)],
	    to_edges_raw_1(NextEdge, Etab, Vtab, VsPair++Acc, Face, LastEdge, done);
	#edge{vs=Va0,ve=Vb0,rf=Face,rtsu=NextEdge} ->
        VsPair=[array:get(Va0, Vtab),array:get(Vb0, Vtab)],
	    to_edges_raw_1(NextEdge, Etab, Vtab, VsPair++Acc, Face, LastEdge, done)
    end.

face2edges_col(Faces, VsDyn, Ftab, Etab, Vtab) ->
    to_edges_raw(Faces, VsDyn, Ftab, Etab, Vtab, []).

to_edges_raw([Face|Faces], VsDyn, Ftab, Etab, Vtab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_edges_raw_1(Edge, VsDyn, Etab, Vtab, Acc0, Face, Edge, not_done),
    to_edges_raw(Faces, VsDyn, Ftab, Etab, Vtab, Acc);
to_edges_raw([],_ , _, _, _, Acc) -> Acc.

to_edges_raw_1(LastEdge, _, _, _, Acc, _, LastEdge, done) -> Acc;
to_edges_raw_1(Edge, VsDyn, Etab, Vtab, Acc, Face, LastEdge, _) ->
    case array:get(Edge, Etab) of
	#edge{vs=Va0,ve=Vb0,lf=Face,ltsu=NextEdge} ->
		Cola=get_vs_color(Va0, VsDyn),
		Colb=get_vs_color(Vb0, VsDyn),
        VsPair=[{array:get(Va0, Vtab),Cola, array:get(Vb0, Vtab),Colb}],
	    to_edges_raw_1(NextEdge, VsDyn, Etab, Vtab, VsPair++Acc, Face, LastEdge, done);
	#edge{vs=Va0,ve=Vb0,rf=Face,rtsu=NextEdge} ->
		Cola=get_vs_color(Va0, VsDyn),
		Colb=get_vs_color(Vb0, VsDyn),
        VsPair=[{array:get(Va0, Vtab),Cola, array:get(Vb0, Vtab),Colb}],
	    to_edges_raw_1(NextEdge, VsDyn, Etab, Vtab, VsPair++Acc, Face, LastEdge, done)
    end.

get_vs_color(V, VsDyn) ->
    case lists:keysearch(V, 1, VsDyn) of
        false -> {0.0,0.0,0.0};
        {_, {_,Value}} ->
        	{1.0*Value,1.0*Value,1.0*Value}
    end.

face2vertice(Faces, Ftab, Etab, Vtab) ->
    to_vertice_raw(Faces, Ftab, Etab, Vtab, []).

to_vertice_raw([Face|Faces], Ftab, Etab, Vtab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_vertice_raw_1(Edge, Etab, Vtab, Acc0, Face, Edge, not_done),
    to_vertice_raw(Faces, Ftab, Etab, Vtab, Acc);
to_vertice_raw([], _, _, _, Acc) -> Acc.

to_vertice_raw_1(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
to_vertice_raw_1(Edge, Etab, Vtab, Acc, Face, LastEdge, _) ->
    case array:get(Edge, Etab) of
	#edge{vs=Va0,ve=Vb0,lf=Face,ltsu=NextEdge} ->
        VsPair=[Va0,Vb0],
	    to_vertice_raw_1(NextEdge, Etab, Vtab, VsPair++Acc, Face, LastEdge, done);
	#edge{vs=Va0,ve=Vb0,rf=Face,rtsu=NextEdge} ->
        VsPair=[Va0,Vb0],
	    to_vertice_raw_1(NextEdge, Etab, Vtab, VsPair++Acc, Face, LastEdge, done)
    end.

grayscale_image(#e3d_image{width=W,height=H,type=Type,image=Pixels}=Image,UseAlpha)->
    Size=W*H,
    Gsi=extract_gray_value(Type,UseAlpha,Pixels),
    case byte_size(Gsi) of
    Size -> 
        {true,Image#e3d_image{type=g8,bytes_pp=e3d_image:bytes_pp(g8),image=Gsi}};
    _ ->
        {false,Image}
	end.

extract_gray_value(Type,_,Pixels) when Type =:= g8; Type =:= a8 ->
    Pixels;
extract_gray_value(Type,_,Pixels) when Type =:= r8g8b8; Type =:= b8g8r8 ->
    extract_gray(Pixels,<<>>);
extract_gray_value(Type,UseAlpha,Pixels) when Type =:= r8g8b8a8; Type =:= b8g8r8a8 ->
    extract_alpha(Pixels,UseAlpha,<<>>);
extract_gray_value(_,_,_) -> <<>>.

extract_gray(<<>>, Acc) -> Acc;
extract_gray(<<C1:1/binary,C1:1/binary,C1:1/binary,T/binary>>, Acc) ->
    extract_gray(T,<<Acc/binary,C1/binary>>);
extract_gray(<<_:1/binary,_:1/binary,_:1/binary,T/binary>>, Acc) ->
    extract_gray(T,Acc).

extract_alpha(Img0) ->
	extract_alpha_0(Img0,<<>>).
extract_alpha_0(<<_R:1/binary,_G:1/binary,_B:1/binary,A:1/binary>>,Acc) ->
	<<Acc/binary,A/binary>>;
extract_alpha_0(<<_R:1/binary,_G:1/binary,_B:1/binary,A:1/binary,T/binary>>,Acc) ->
	extract_alpha_0(T,<<Acc/binary,A/binary>>).

extract_alpha(<<>>,_,Acc) -> Acc;
extract_alpha(<<_:1/binary,_:1/binary,_:1/binary,A:1/binary,T/binary>>,true,Acc) ->
    extract_alpha(T,true,<<Acc/binary,A/binary>>);
extract_alpha(<<C1:1/binary,C1:1/binary,C1:1/binary,_A:1/binary,T/binary>>,false,Acc) ->
    extract_alpha(T,false,<<Acc/binary,C1/binary>>);
extract_alpha(<<_:1/binary,_:1/binary,_:1/binary,_:1/binary,T/binary>>,UseAlpha,Acc) ->
    extract_alpha(T,UseAlpha,<<Acc>>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This will load the texture "silently" 
%%
load_texture(Image) ->
    case init_texture(Image) of
	{error,_}=Error ->
        io:format("Error loading texture ~p\n",[Error]),
	    Error;
	TxId ->
	    TxId
    end.
%%
%% These functions below came from wings_image.erl
%% 
init_texture(Image) ->
    case get(wings_not_running) of
        true -> 0;
        _ ->
            [TxId] = gl:genTextures(1),
            case init_texture(Image, TxId) of
                {error,_}=Error ->
                    gl:deleteTextures(1, [TxId]),
                    Error;
                Other ->
                    Other
            end
    end.

init_texture(Image0, TxId) ->
    case maybe_scale(Image0) of
        {error,_}=Error ->
            Error;
        Image ->
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
            TxId
    end.

maybe_scale(#e3d_image{width=W0,height=H0}=Image) ->
%%  case wings_gl:is_ext({2,0}, 'GL_ARB_texture_non_power_of_two') of
%%  Aarg ATI doesn't support ARB_NPOT textures, though it report GL_VER >= 2.0
    case maybe_exceds_opengl_caps(Image) of
        {error,_}=Error ->
            Error;
        Image1 ->
            #e3d_image{width=W1,height=H1}=Image1,
            case {W1,H1} of
                {W0,H0} ->
                    GL_ARB = wings_gl:is_ext('GL_ARB_texture_non_power_of_two');
                {_,_} -> GL_ARB = false
            end,
            case GL_ARB of
                true ->
                    Image;
                false ->
                    case {nearest_power_two(W1),nearest_power_two(H1)} of
                        {W1,H1} ->
                            Image1;
                        {W,H} ->
                            resize_image(Image1, W, H)
                    end
            end
    end.

maybe_exceds_opengl_caps(#e3d_image{width=W0,height=H0}=Image) ->
    MaxSize = lists:last(gl:getIntegerv(?GL_MAX_TEXTURE_SIZE)),
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

resize_image(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
  image=Bits0}=Image, W, H) ->
    Out = wings_io:get_buffer(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
    Format = texture_format(Image),
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

release_texture(Id) ->
    wings_gl:deleteTextures([Id]).

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

%% Long ago we tried to use compression, but we no longer do since compression
%% lowers the quality too much, especially for bump/normal maps.
internal_format(?GL_BGR) -> ?GL_RGB;
internal_format(?GL_BGRA) -> ?GL_RGBA;
internal_format(Else) -> Else.

