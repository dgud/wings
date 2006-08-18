%%%-------------------------------------------------------------------
%%% File    : wpc_snap.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Snap texture (image) to model
%%%
%%% Created : 28 May 2003 by Dan Gudmundsson
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_snap.erl,v 1.10 2005/09/23 19:38:00 giniu Exp $

-module(wpc_snap).

-define(NEED_OPENGL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").

-define(HUGE, 1.0E307).

-export([init/0,menu/2,command/2,active/0]).

-record(s, {reply,name,w,h,sx=1.0,sy=1.0,tx=0.0,ty=0.0}).

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
		 {?__(7,"Uniform"),uniform,?__(8,"Scale the background image uniformly")}],
    MoveMenu = [{?__(9,"Horizontal"),x,?__(10,"Move the background image horizontally")},
		{?__(11,"Vertical"),y, ?__(12,"Move the background image vertically")},
		{?__(13,"Free"),free,  ?__(14,"Move the background image freely")}], 

    [{?__(15,"Scale Snap Image"),{auv_snap_scale,ScaleMenu},?__(16,"Scale the background image")},
     {?__(17,"Move Snap Image"),{auv_snap_move,MoveMenu},?__(18,"Move the background image")},
     {?__(19,"Exit Snap Mode"),exit_snap_mode,?__(20,"Exit the snap mode")},
     separator].

command({face,snap_image}, St) ->
    snap(St);
command({_,{auv_snap_scale,Op}}, St) ->
    scale(Op,St);
command({_,{auv_snap_move,Op}}, St) ->
    move(Op,St);
command({tools,snap_image_mode}, St) ->
    select_image(St);
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
	    wpa:error(?__(1,"No images present, import an image first."));
	_ ->
	    Qs = [{vframe, Images}],
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
	    wings_ask:dialog(?__(2,"Choose Image to snap"),Qs,Select)
    end.

find_images() ->
    case wings_image:images() of 
	[] -> [];
	Imgs = [{Def,_}|_] -> find_images_1(Imgs, Def, 0)
    end.

find_images_1([{Id,#e3d_image{name=Name}}|Tail], Def, Key) ->
    [{key_alt,{Key,Def},Name,Id}|find_images_1(Tail, Def, Key-1)];
find_images_1([], _Def, _Key) -> [].
    
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

find_a_id(#st{shapes=Shs}) ->
    {Id, _} = gb_trees:smallest(Shs),
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
    {Xs,Ys,Xe,Ye} = {0,0,1,1},
    
    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty} = get(?MODULE),
    {X,Y} = scale(W,H,IW,IH),

    Center = 0.5,
    Size   = 1.0,
    Xrange = (X*Size)/2,
    Yrange = (Y*Size)/2,

    gl:texCoord2f(Tx/2+(Center-Sx*Xrange),Ty/2+(Center-Sy*Yrange)),
    gl:vertex2f(Xs,Ys),
    gl:texCoord2f(Tx/2+(Center+Sx*Xrange),Ty/2+(Center-Sy*Yrange)),
    gl:vertex2f(Xe,Ys),
    gl:texCoord2f(Tx/2+(Center+Sx*Xrange),Ty/2+(Center+Sy*Yrange)),
    gl:vertex2f(Xe,Ye),
    gl:texCoord2f(Tx/2+(Center-Sx*Xrange),Ty/2+(Center+Sy*Yrange)),
    gl:vertex2f(Xs,Ye),

    gl:'end'(),
    gl:popAttrib().
    
calc_uv(_V = {X,Y,Z}) ->
    {MM,PM,Viewport = {_,_,W,H}} = wings_u:get_matrices(0, original),
    {S,T, _} = glu:project(X,Y,Z,MM,PM,Viewport),
    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty} = get(?MODULE),
    {Xs,Ys} = scale(W,H,IW,IH),
    Center = 0.5,
    Res = {Tx/2+(S/W*Xs*Sx+Center-Sx*Xs/2),Ty/2+(T/H*Sy*Ys+Center-Sy*Ys/2)},
%%    io:format("~p st ~.3f,~.3f XsYs ~.2f ~.2f Res ~p~n", [_V, S/W,T/H,Xs,Ys,Res]),
    Res.

scale(W,H,IW,IH) ->
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

update_uv_fun(Vs) ->
    fun(Face, _V, Edge, Rec0, E0) ->
	    case Rec0 of
		#edge{vs=V,lf=Face} ->
		    Rec  = gb_trees:get(Edge, E0),
		    Vpos = gb_trees:get(V, Vs),
		    UV = calc_uv(Vpos),
		    gb_trees:update(Edge, Rec#edge{a=UV},E0);
		#edge{ve=V,rf=Face} ->
		    Rec  = gb_trees:get(Edge, E0),
		    Vpos = gb_trees:get(V, Vs),
		    UV = calc_uv(Vpos),
		    gb_trees:update(Edge, Rec#edge{b=UV},E0)
	    end
    end.

insert_uvs(St0) ->
    wings_sel:map(fun(Items, We=#we{vp=Vs,es=Etab0}) ->  
			  AddUv = update_uv_fun(Vs),
			  Etab = wings_face:fold_faces(AddUv, Etab0, Items, We),
			  We#we{es=Etab}
		  end, St0).

set_materials(Image,St0) ->     
    Fix = fun(Items,We0,NewMats0) ->
		  case We0#we.mode of
		      vertex -> 
			  wpa:error(?__(1,"Can't put on image when object has vertex colors"));
		      _ -> 
			  continue
		  end,
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
	    %% It already has the texture no need to create new material
	    {MatName,{[{MatName,MatName}|Used],St0}};
	Else ->
	    NewMatName = list_to_atom(atom_to_list(MatName) ++ "_" ++ Name),
	    Maps = case Else of
		       undefined -> [{diffuse,Image}|Maps0];
		       _ -> lists:keyreplace(diffuse,1,Maps0,{diffuse,Image})
		   end,
	    Mat   = {NewMatName,lists:keyreplace(maps,1,Mat0, {maps,Maps})},
	    case wings_material:add_materials([Mat], St0) of
		{St,[]} ->
		    {NewMatName, {[{MatName,NewMatName}|Used],St}};
		{St,[{NewMatName,ChangedMatName}]} ->
		    {ChangedMatName, {[{MatName,ChangedMatName}|Used],St}}
	    end
    end.
    
