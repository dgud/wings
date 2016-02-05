%%%-------------------------------------------------------------------
%%% File    : wpc_snap.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Snap texture (image) to model
%%%
%%% Created : 28 May 2003 by Dan Gudmundsson
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id$

-module(wpc_snap).

-define(NEED_OPENGL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").

-define(HUGE, 1.0E307).

-export([init/0,menu/2,command/2,active/0]).

-record(s, {reply,name,w,h,sx=1.0,sy=1.0,tx=0.0,ty=0.0,r=0.0}).

init() ->
    true.

menu({tools}, Menu) ->
    Menu ++
	[separator,
	 case active() of
	     false ->
		 {?__(1,"Snap Image"),snap_image_mode,
		  ?__(2,"Start snap mode for \"snapping\" UV coordinates onto an image")};
	     true ->
		 {?__(3,"Exit Snap Mode"),exit_snap_mode,
		  ?__(4,"Exit snap mode")}
	 end];
menu({face}, Menu) ->
    case active() of
	false ->
	    Menu;
	true ->
	    [{?__(5,"Snap Image"),snap_image,
	      ?__(6,"Put background image on selected faces by assigning "
	      "UV coordinates to them")}|
	     snap_menu()] ++ Menu
    end;
menu({Type}, Menu) when Type == vertex; Type == edge;
			Type == body; Type == shape ->
    case active() of
	false -> Menu;
	true -> snap_menu() ++ Menu
    end;
menu(_, Menu) -> Menu.

snap_menu() ->
    ScaleMenu = [{?__(1,"Horizontal"),x,?__(2,"Scale the background image horizontally")},
		 {?__(3,"Vertical"),y,?__(4,"Scale the background image vertically")},
		 {?__(5,"Free"),free,?__(6,"Scale the background image freely")},
		 {?__(7,"Uniform"),uniform,?__(8,"Scale the background image uniformly")},
		 {?__(32,"Proportional"),{auv_snap_prop,proportional_scale()},?__(33,"Make image scale proportional")}],
    MoveMenu = [{?__(1,"Horizontal"),x,?__(10,"Move the background image horizontally")},
		{?__(3,"Vertical"),y, ?__(12,"Move the background image vertically")},
		{?__(5,"Free"),free,  ?__(14,"Move the background image freely")},
		separator,
		{?__(30,"Center"),center, ?__(31,"Center the background image to the viewport")},
		{?__(26,"Center X"),center_x, ?__(27,"Center the background image horizontally")},
		{?__(28,"Center Y"),center_y, ?__(29,"Center the background image vertically")}],

    [{?__(15,"Scale Snap Image"),{auv_snap_scale,ScaleMenu},?__(16,"Scale the background image")},
     {?__(17,"Move Snap Image"),{auv_snap_move,MoveMenu},?__(18,"Move the background image")},
     {?__(21,"Fit Snap Image"),{auv_snap_fit,
         [{?__(22,"Both"),both},
          {?__(1,"Horizontal"),x},
          {?__(3,"Vertical"),y}]},
      ?__(25,"Fit image to the dimensions of the viewport")},
     {?__(34,"Rotate Snap Image"),auv_snap_rotate,?__(35,"Rotate the background image")},
     {?__(19,"Exit Snap Mode"),exit_snap_mode,?__(20,"Exit the snap mode")},
     separator].

proportional_scale() ->
    [{?__(1,"...to Current X"),proportional_x,
      ?__(2,"Scale image's Y value to be proportional to it's current X value")},
     {?__(3,"...to Current Y"),proportional_y,
      ?__(4,"Scale image's X value to be proportional to it's current Y value")},
     {?__(5,"Actual Size"),actual_size,
      ?__(6,"Reset image to its actual size")}].

command({face,snap_image}, St) ->
    snap(St);
command({_,{auv_snap_scale,Op}}, St) ->
    scale(Op,St);
command({_,{auv_snap_move,Op}}, St) ->
    move(Op,St);
command({_,{auv_snap_fit,Op}}, St) ->
    fit(Op,St);
command({tools,snap_image_mode}, St) ->
    select_image(St);
command({_,auv_snap_rotate}, St) ->
    rotate(St);
command({_,exit_snap_mode}, St) ->
    cancel(St);
command(_, _) -> next.

active() ->
    get(?MODULE) =/= undefined.

cancel(St) ->
    wings:unregister_postdraw_hook(geom, ?MODULE),
    erase(?MODULE),
    St.

snap(St0) ->
    #s{reply=Image,name=Name} = get(?MODULE),
    St = set_materials({Image,Name}, St0),
    insert_uvs(St).

select_image(_St) ->
    Images = find_images(),
    case Images of
	[] ->
	    wpa:error_msg(?__(1,"No images present, import an image first."));
	[{_, Def}|_] ->
	    Qs = [{vradio, Images, Def}],
	    Select = fun([Reply]) ->
			     TId = wings_image:txid(Reply),
			     Draw = fun(St) -> draw_image(TId,St) end,
			     wings:register_postdraw_hook(geom, ?MODULE,Draw),
			     _I = #e3d_image{width=W, height=H, name =Name} =
				 wings_image:info(Reply),
			     %%			     io:format("~p~n", [_I]),
			     put(?MODULE,#s{reply=Reply,name=Name,w=W,h=H}),
			     ignore
		     end,
	    wings_dialog:dialog(?__(2,"Choose Image to snap"),Qs,Select)
    end.

find_images() ->
    case wings_image:images() of
	[] -> [];
	Imgs ->
	    [{Name, Id} || {Id,#e3d_image{name=Name}} <- Imgs]
    end.

scale({auv_snap_prop,Proportional}, St) ->
    State = #s{sx=Ix,sy=Iy} = get(?MODULE),
    case Proportional of
      proportional_x -> put(?MODULE, State#s{sy=Ix});
      proportional_y -> put(?MODULE, State#s{sx=Iy});
      actual_size -> put(?MODULE, State#s{sx=1.0,sy=1.0})
    end,
    St;
scale(Op, St) ->
    #s{sx=Ix,sy=Iy} = get(?MODULE),
    ScaleFun = fun({finish,_}, Dlo) -> Dlo;
		  ([X,Y], Dlo) ->
		       State = #s{sx=SX,sy=SY} = get(?MODULE),
		       case Op of
			   x ->    put(?MODULE, State#s{sx=X});
			   y ->    put(?MODULE, State#s{sy=Y});
			   free -> put(?MODULE, State#s{sx=X,sy=Y});
			   uniform ->
			       Diff = X-SX,
			       DY   = SY+Diff,
			       put(?MODULE, State#s{sx=X,sy=DY})
		       end,
		       Dlo
	       end,
    Tvs   = {general, [{find_a_id(St), ScaleFun}]},
    Units = [{dx, {0.0,?HUGE}},{dy, {0.0,?HUGE}}],
    Flags = [{initial, [Ix,Iy]}],
    wings_drag:setup(Tvs,Units,Flags,St).

move(Op, St) when Op =:= center_x; Op =:= center_y; Op =:= center ->
    S = get(?MODULE),
    case Op of
      center_x -> put(?MODULE, S#s{tx=0.0});
      center_y -> put(?MODULE, S#s{ty=0.0});
      center -> put(?MODULE, S#s{tx=0.0,ty=0.0})
    end,
    St;
move(Op, St) ->
    #s{tx=Ix,ty=Iy} = get(?MODULE),
    MoveFun = fun({finish,_}, Dlo) -> Dlo;
		 ([X,Y], Dlo) ->
		      State = get(?MODULE),
		      case Op of
			  x ->    put(?MODULE, State#s{tx=-X});
			  y ->    put(?MODULE, State#s{ty=-Y});
			  free -> put(?MODULE, State#s{tx=-X,ty=-Y})
		      end,
		      Dlo
	      end,

    Tvs   = {general, [{find_a_id(St), MoveFun}]},
    Units = [{dx, {-?HUGE,?HUGE}},{dy,{-?HUGE,?HUGE}}],
    Flags = [{initial, [-Ix,-Iy]}],
    wings_drag:setup(Tvs,Units,Flags,St).

rotate(St) ->
    #s{r=R0}=get(?MODULE),
    RotateFun = fun({finish,_}, Dlo) -> Dlo;
		 ([R], Dlo) ->
		      State = get(?MODULE),
			  put(?MODULE, State#s{r=R}),
		      Dlo
	      end,

    Tvs   = {general, [{find_a_id(St), RotateFun}]},
    Units = [{rx, {-360.0,360.0}}],
    Flags = [{initial, [R0]}],
    wings_drag:setup(Tvs,Units,Flags,St).

fit(Op, St) ->
    #s{w=IW,h=IH}=S = get(?MODULE),
    {_,_,W,H} = wings_wm:viewport(),
    {X,Y} = scale(W,H,IW,IH),
    case Op of
      x -> put(?MODULE, S#s{sx=1.0/X,tx=0.0});
      y -> put(?MODULE, S#s{sy=1.0/Y,ty=0.0});
      both -> put(?MODULE, S#s{sx=1.0/X,sy=1.0/Y,tx=0.0,ty=0.0})
    end,
    St.

find_a_id(#st{shapes=Shs}) ->
    Ida = [Id || #we{id=Id,perm=Perm} <- gb_trees:values(Shs),
        ?IS_VISIBLE(Perm)],
    Id = case length(Ida) of
    0 -> wpa:error_msg(?__(1,"Visible object required."));
    _ -> lists:min(Ida)
    end,
    Id.

draw_image(Image,_St) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0.0, 1.0, 0.0, 1.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_ALPHA_TEST),

    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Image),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

    gl:'begin'(?GL_QUADS),
    gl:color4f(1.0, 1.0, 1.0, 0.55),   %%Semitransparant
    {_,_,W,H} = wings_wm:viewport(),
    {Xs,Ys,Xe,Ye} = {0.0,0.0,1.0,1.0},

    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty,r=Rot} = get(?MODULE),
    {X,Y} = scale(W,H,IW,IH),

    Center = 0.5,
    Size   = 1.0,
    Xrange = (X*Size)/2,
    Yrange = (Y*Size)/2,

    plot_uv({Tx/2,Ty/2},{-Sx*Xrange,-Sy*Yrange},Center,Rot),
    gl:vertex2f(Xs,Ys),
    plot_uv({Tx/2,Ty/2},{Sx*Xrange,-Sy*Yrange},Center,Rot),
    gl:vertex2f(Xe,Ys),
    plot_uv({Tx/2,Ty/2},{Sx*Xrange,Sy*Yrange},Center,Rot),
    gl:vertex2f(Xe,Ye),
    plot_uv({Tx/2,Ty/2},{-Sx*Xrange,Sy*Yrange},Center,Rot),
    gl:vertex2f(Xs,Ye),

    gl:'end'(),
    gl:popAttrib().

plot_uv({Tx,Ty},{OffX0,OffY0},Center,Degree) ->
    {OffX,OffY}=rotate(Degree, {OffX0,OffY0}),
    {TxX,TyY}=rotate(Degree, {Tx,Ty}),
    gl:texCoord2f(TxX+(Center+OffX),TyY+(Center+OffY)).

rotate(Dgree0, {X,Y}) ->
    Dgree=(math:pi()/180.0*-Dgree0),
    Cosf=math:cos(Dgree),
    Sinf=math:sin(Dgree),
    {X*Cosf-Y*Sinf, Y*Cosf+X*Sinf}.

calc_uv_fun() ->
    %% First do all the view-dependent calculations that are
    %% common for all vertices.
    {MM,PM,{_,_,W,H}=Viewport} = wings_u:get_matrices(0, original),
    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty,r=Rot} = get(?MODULE),
    {Xs,Ys} = scale(W, H, IW, IH),

    %% In the fun, do the calculations that are specific
    %% for each vertex.
    fun({X,Y,Z}) ->
	    {S,T,_} = wings_gl:project(X, Y, Z, MM, PM, Viewport),
	    Center = 0.5,
	    {XA0,YA0}={Tx/2+(S/W*Xs*Sx+Center-Sx*Xs/2),Ty/2+(T/H*Sy*Ys+Center-Sy*Ys/2)},
	    {XA,YA}=rotate(Rot,{XA0-Center,YA0-Center}),
	    {Center+XA,Center+YA}
    end.

scale(W, H, IW, IH) ->
    if
	W == H ->
	    if
		IW == IH ->
		    {1,1};
		IW > IH ->
		    {1.0,IW/IH};
		true ->
		    {IH/IW, 1.0}
	    end;
	W > H ->
 	    if IW == IH ->
		    {W/H, 1.0};
 	       IH > IW ->
 		    {W/H*IH/IW, 1.0};
	       true ->
		    {W/H,IW/IH}
 	    end;
	true ->
 	    if IW == IH ->
		    {1.0, H/W};
 	       IW > IH ->
 		    {1.0, IW/IH*H/W};
	       true ->
		    {IH/IW,H/W}
 	    end
    end.

insert_uvs(St0) ->
    CalcUV = calc_uv_fun(),
    wings_sel:map(fun(Faces, We) ->
			  insert_we_uvs(Faces, CalcUV, We)
		  end, St0).

insert_we_uvs(Faces, CalcUV, We) ->
    VFace = wings_face:fold_faces(
	      fun(Face, V, _, _, A) ->
		      [{V,Face}|A]
	      end, [], Faces, We),
    VFaces = wings_util:rel2fam(VFace),
    insert_we_uvs_1(VFaces, CalcUV, We).

insert_we_uvs_1([{V,Faces}|T], CalcUV, We0) ->
    UV = CalcUV(wings_vertex:pos(V, We0)),
    We = wings_va:set_vtx_face_uvs(V, Faces, UV, We0),
    insert_we_uvs_1(T, CalcUV, We);
insert_we_uvs_1([], _, We) -> We.

set_materials(Image,St0) ->
    Fix = fun(Items,We0,NewMats0) ->
		  Set = fun(Face,_,_,_,{We1,NMats0}) ->
				FaceM = wings_facemat:face(Face, We0),
				case lists:keysearch(FaceM,1,element(1,NMats0)) of
				    false ->
					{MatName,NMats} = dup_mat(FaceM, NMats0,Image),
					{wings_facemat:assign(MatName,[Face],We1),NMats};
				    {value, {_Old,MatName}}->
					{wings_facemat:assign(MatName,[Face],We1),NMats0}
				end
			end,
		  wings_face:fold_faces(Set, {We0,NewMats0}, Items, We0)
	  end,
    {St1, {_,NewMats}} = wings_sel:mapfold(Fix, {[],St0}, St0),
    St1#st{mat=NewMats#st.mat}.

dup_mat(MatName,{Used,St0},{Image,Name}) ->
    Mat0 = gb_trees:get(MatName, St0#st.mat),
    Maps0 = proplists:get_value(maps, Mat0),
    case proplists:get_value(diffuse, Maps0) of
	Image ->
	    %% It already has the texture; no need to create new material
	    {MatName,{[{MatName,MatName}|Used],St0}};
	Else ->
	    NewMatName = list_to_atom(atom_to_list(MatName) ++ "_" ++ Name),
	    Maps = case Else of
		       undefined -> [{diffuse,Image}|Maps0];
		       _ -> lists:keyreplace(diffuse,1,Maps0,{diffuse,Image})
		   end,

	    %% Make sure that vertex colors will not override the texture.
	    %% If the Vertex Color attribute in the material is 'Set',
	    %% change it to 'Multiply'.
	    OpenGL0 = proplists:get_value(opengl, Mat0),
	    VtxColors = case proplists:get_value(vertex_colors, OpenGL0) of
			    set -> multiply;
			    Other -> Other
			end,
	    OpenGL1 = proplists:delete(vertex_colors, OpenGL0),
	    OpenGL = [{vertex_colors,VtxColors}|OpenGL1],

	    Mat1 = proplists:delete(opengl, Mat0),
	    Mat2 = proplists:delete(maps, Mat1),
	    Mat = {NewMatName,[{opengl,OpenGL},{maps,Maps}|Mat2]},
	    case wings_material:add_materials([Mat], St0) of
		{St,[]} ->
		    {NewMatName, {[{MatName,NewMatName}|Used],St}};
		{St,[{NewMatName,ChangedMatName}]} ->
		    {ChangedMatName, {[{MatName,ChangedMatName}|Used],St}}
	    end
    end.

