%%%-------------------------------------------------------------------
%%% File    : wpc_snap_win.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Snap texture (image) to model
%%%
%%% Created : 28 May 2003 by Dan Gudmundsson
%%% Snap Image migration to window mode: 30 Jan 2017 by Micheus
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id$

-module(wpc_snap_win).

-define(NEED_OPENGL, 1).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-define(HUGE, 1.0E307).
-define(WIN_NAME, {plugin,snap}).
-define(LOCAL_MODULE, ?MODULE).

-export([init/0,menu/2,command/2,active/0,win_data/1,win_name/0]).
-export([window/1,window/5]).

-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2,
	 handle_info/2, code_change/3, terminate/2]).

-record(s, {active=false,reply,name,w,h,opacity=0.5,sx=1.0,sy=1.0,tx=0.0,ty=0.0,r=0.0,tiled=1}).

init() -> true.

menu({window}, Menu) ->
    Menu ++ [snap_win_menu()];
menu(_, Menu) -> Menu.

snap_win_menu() ->
    {title(), snap_win,
     ?__(2,"Shows the snap image window")}.

proportional_scale() ->
    [{?__(1,"...to Current X"),{auv_snap_prop,proportional_x},
      ?__(2,"Scale image's Y value to be proportional to it's current X value")},
     {?__(3,"...to Current Y"),{auv_snap_prop,proportional_y},
      ?__(4,"Scale image's X value to be proportional to it's current Y value")},
     {?__(5,"Actual Size"),{auv_snap_prop,actual_size},
      ?__(6,"Reset image to its actual size")}].

snap_menu(scale) ->
    [{?__(1,"Horizontal"),{auv_snap_scale,x},?__(2,"Scale the background image horizontally")},
     {?__(3,"Vertical"),{auv_snap_scale,y},?__(4,"Scale the background image vertically")},
     {?__(5,"Free"),{auv_snap_scale,free},?__(6,"Scale the background image freely")},
     {?__(7,"Uniform"),{auv_snap_scale,uniform},?__(8,"Scale the background image uniformly")},
     {?__(32,"Proportional"),{auv_snap_scale,proportional_scale()},?__(33,"Make image scale proportional")}];
snap_menu(move) ->
    [{?__(1,"Horizontal"),{auv_snap_move,x},?__(10,"Move the background image horizontally")},
     {?__(3,"Vertical"),{auv_snap_move,y}, ?__(12,"Move the background image vertically")},
     {?__(5,"Free"),{auv_snap_move,free},  ?__(14,"Move the background image freely")},
     separator,
     {?__(30,"Center"),{auv_snap_move,center}, ?__(31,"Center the background image to the viewport")},
     {?__(26,"Center X"),{auv_snap_move,center_x}, ?__(27,"Center the background image horizontally")},
     {?__(28,"Center Y"),{auv_snap_move,center_y}, ?__(29,"Center the background image vertically")}];
snap_menu(fit) ->
    [{?__(22,"Both"),{auv_snap_fit,both}},
     {?__(1,"Horizontal"),{auv_snap_fit,x}},
     {?__(3,"Vertical"),{auv_snap_fit,y}}].


command({window,snap_win}, St) ->
    window(St),
    keep;
command({snap_image,start}, St) ->
    start(St);
command({snap_image,{start,TxInfo}}, St) ->
    start(TxInfo, St);
command({snap_image,cancel}, St) ->
    cancel(St);
command({snap_image,{opacity,Opacity}}, St) ->
    opacity(Opacity, St);
command({snap_image,{tiled,Active}}, St) ->
    tiled(Active, St);
command({snap_image,apply}, St) ->
    snap(St);
command({_,{auv_snap_scale,Op}}, St) ->
    scale(Op,St);
command({_,{auv_snap_move,Op}}, St) ->
    move(Op,St);
command({_,{auv_snap_fit,Op}}, St) ->
    fit(Op,St);
command({snap_image,{rotate,Rotate}}, St) ->
    rotate(Rotate, St);
command(_Op, _) ->
    next.

active() ->
    case get(?MODULE) of
	undefined -> false;
	#s{active=Active} -> Active
    end.

start(St0) ->
    #s{reply=ImgId} = get(?MODULE),
    TId = wings_image:txid(ImgId),
    Draw = fun(St) -> draw_image(TId, St) end,
    wings:register_postdraw_hook(geom, ?MODULE, Draw),
    St0.

start(#{reply:=ImgId,name:=Name,w:=W,h:=H}, St) ->
    State =
	case get(?MODULE) of
	    undefined -> #s{active=true,reply=ImgId,name=Name,w=W,h=H};
	    State0 -> State0#s{active=true,reply=ImgId,name=Name,w=W,h=H}
	end,
    put(?MODULE, State),
    start(St).

cancel() ->
    case get(?MODULE) of
	undefined -> ignore;
	State -> put(?MODULE, State#s{active=false})
    end,
    wings:unregister_postdraw_hook(geom, ?MODULE).

cancel(St) ->
    cancel(),
    St.

snap(St0) ->
    #s{reply=Image,name=Name} = get(?MODULE),
    St = set_materials({Image,Name}, St0),
    insert_uvs(St).

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
    ScaleFun = fun([X,Y]) ->
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
                       ok
	       end,
    Units = [{dx, {0.0,?HUGE}},{dy, {0.0,?HUGE}}],
    Flags = [{initial, [Ix,Iy]}],
    wings_drag:drag_only(ScaleFun, Units, Flags, St).

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
    MoveFun = fun([X,Y]) ->
		      State = get(?MODULE),
		      case Op of
			  x ->    put(?MODULE, State#s{tx=-X});
			  y ->    put(?MODULE, State#s{ty=-Y});
			  free -> put(?MODULE, State#s{tx=-X,ty=-Y})
		      end,
		      ok
	      end,
    Units = [{dx,{-?HUGE,?HUGE}},{dy,{-?HUGE,?HUGE}}],
    Flags = [{initial,[-Ix,-Iy]}],
    wings_drag:drag_only(MoveFun, Units, Flags, St).

rotate(Rotate, St) ->
    State=get(?MODULE),
    put(?MODULE, State#s{r=Rotate}),
    St.

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

opacity(Opacity, St) ->
    State=get(?MODULE),
    wings_pref:set_value(snap_opacity, Opacity),
    put(?MODULE, State#s{opacity=Opacity}),
    St.

tiled(Active, St) ->
    State=get(?MODULE),
    wings_pref:set_value(snap_tiled, Active),
    put(?MODULE, State#s{tiled=Active}),
    St.

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

    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty,r=Rot,opacity=Opa,tiled=Tiled} = get(?MODULE),
    if Tiled =:= 0 ->
	gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_BORDER),
	gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_BORDER);
    true ->
	gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
	gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT)
    end,

    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

    gl:color4f(1.0, 1.0, 1.0, Opa),   %%Semitransparant
    {_,_,W,H} = wings_wm:viewport(),
    {Xs,Ys,Xe,Ye} = {0.0,0.0,1.0,1.0},

    #s{w=IW,h=IH,sx=Sx,sy=Sy,tx=Tx,ty=Ty,r=Rot} = get(?MODULE),
    {X,Y} = scale(W,H,IW,IH),

    Center = 0.5,
    Size   = 1.0,
    Xrange = (X*Size)/2,
    Yrange = (Y*Size)/2,

    Vs = [{Xs,Ys}, {Xe,Ys}, {Xe,Ye}, {Xs,Ye}],
    UVs = [plot_uv({Tx/2,Ty/2},{-Sx*Xrange,-Sy*Yrange},Center,Rot),
	   plot_uv({Tx/2,Ty/2},{Sx*Xrange,-Sy*Yrange},Center,Rot),
	   plot_uv({Tx/2,Ty/2},{Sx*Xrange,Sy*Yrange},Center,Rot),
	   plot_uv({Tx/2,Ty/2},{-Sx*Xrange,Sy*Yrange},Center,Rot)],
    List = zip(UVs, Vs),
    wings_vbo:draw(fun(_) -> gl:drawArrays(?GL_QUADS, 0, 4) end, List, [uv, vertex2d]),
    gl:popAttrib().

zip([V|Vs], [UV|UVs]) ->
    [V,UV|zip(Vs,UVs)];
zip([], []) -> [].

plot_uv({Tx,Ty},{OffX0,OffY0},Center,Degree) ->
    {OffX,OffY}=rotate_uv(Degree, {OffX0,OffY0}),
    {TxX,TyY}=rotate_uv(Degree, {Tx,Ty}),
    {TxX+(Center+OffX),TyY+(Center+OffY)}.

rotate_uv(Dgree0, {X,Y}) ->
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
            {XA,YA}=rotate_uv(Rot,{XA0-Center,YA0-Center}),
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


%% win_data/1 function allows many plugin windows to be saved.
%% it returns: {Name, {Horiz alignment, Custom_data}}
%% horiz alignment should be either "left" or "right"
%% custom data is used to store windows properties and custom data - it should be parsed in window/5
win_data(?WIN_NAME=Name) ->
    {Name, {right,[]}}.

win_name() ->
    ?WIN_NAME.

title() ->
    ?__(1,"Snap Image").

window(St) ->
    case wings_wm:is_window(?WIN_NAME) of
	true ->
	    wings_wm:raise(?WIN_NAME),
	    keep;
	false ->
	    wpc_snap_win:window(?WIN_NAME, {5,150}, {-1,-1}, [], St),
	    keep
    end.

window(Name, Pos, Size, Ps0, St) ->
    State = get_state(Name),
    {Frame,Ps} = wings_frame:make_win(title(), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Name, State]),
    wings_wm:toplevel(Name, Window, Ps, {push,change_state(Window, St)}),
    keep.

%%%%%%%% Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event({note,image_change}=Ev, Window, _St) ->
    wx_object:cast(Window, Ev),
    keep;
forward_event({action,{wpc_snap_win,_}}=Ev, Window, _St) ->
    wx_object:cast(Window, Ev),
    keep;
forward_event({apply, ReturnSt, Fun}, Window, St0) ->
    %% Apply ops from window in wings process
    case ReturnSt of
	true ->
	    St = Fun(St0),
	    {replace, change_state(Window, St)};
	false ->
	    Fun(St0)
    end;
forward_event(_, _Window, _St) ->
    keep.

get_state(?WIN_NAME) ->
    {wings_pref:get_value(snap_opacity, 0.5),wings_pref:get_value(snap_tiled, 1)}.

snap_label(image) ->
    ?__(1, "Select Image");
snap_label(activate) ->
    ?__(2, "Activate");
snap_label(deactivate) ->
    ?__(3, "Deactivate");
snap_label(snap) ->
    ?__(4, "Snap Image");
snap_label(opacity) ->
    ?__(5, "Opacity");
snap_label(rotate) ->
    ?__(6, "Rotate");
snap_label(move) ->
    ?__(7, "Move");
snap_label(scale) ->
    ?__(8, "Scale");
snap_label(fit) ->
    ?__(9, "Fit");
snap_label(tiled) ->
    ?__(10, "Tiled Image").

mod_choices() ->
    [snap_label(move) ++ " ...",
     snap_label(scale) ++ " ...",
     snap_label(fit) ++ " ..."].

snap_tooltip(image) ->
    ?__(1, "Select the image to be used for snap");
snap_tooltip(activate) ->
    ?__(2, "Start snap mode for \"snapping\" UV coordinates onto an image");
snap_tooltip(deactivate) ->
    ?__(3, "Exit snap mode");
snap_tooltip(snap) ->
    ?__(4, "Put background image on selected faces by assigning "
        "UV coordinates to them");
snap_tooltip(opacity) ->
    ?__(5, "Defines how translucent the image is on screen");
snap_tooltip(rotate) ->
    ?__(6, "Rotate the background image");
snap_tooltip(move) ->
    ?__(7, "Move the background image");
snap_tooltip(scale) ->
    ?__(8, "Scale the background image");
snap_tooltip(fit) ->
    ?__(9, "Fit image to the dimensions of the viewport");
snap_tooltip(tiled) ->
    ?__(10, "Draws the image repeatedly ").

append_value(Op=opacity, Val) ->
    snap_label(Op) ++ " (" ++ to_str(Val) ++ "%)";
append_value(Op=rotate, Val) ->
    snap_label(Op) ++ " (" ++ to_str(Val) ++ "°)".

to_str(Val) ->
    wings_util:nice_float(Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {me, name, shown, state, cols, ctrls}).

init([Frame, Name, {OpaVal,Tiled}=State]) ->
    #{bg:=BG, text:=_FG} = Cols = wings_frame:get_colors(),
    Panel = wxPanel:new(Frame, [{style, ?wxNO_BORDER}, {size,{200,300}}]),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    wxWindow:setBackgroundColour(Panel, BG),
    Main = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(Panel, Main),

    %% first level: Images, Button and Panel
    ImgLst = wxComboBox:new(Panel, ?wxID_ANY, [{style,?wxCB_SORT bor ?wxTE_PROCESS_ENTER},{pos, {2,2}}]),
    wxWindow:setToolTip(ImgLst, wxToolTip:new(snap_tooltip(image))),
    setup_image_list(ImgLst),
    BAct = wxToggleButton:new(Panel, ?wxID_ANY, snap_label(activate)),
    wxWindow:setToolTip(BAct, wxToolTip:new(snap_tooltip(activate))),
    CtrlBox = wxPanel:new(Panel, [{style, ?wxNO_BORDER}]),
    wxWindow:setBackgroundColour(CtrlBox, BG),
    %% layout settings for the first level controls
    Szr1 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Szr1, ImgLst, [{flag, ?wxEXPAND}]),
    wxSizer:add(Szr1, BAct, [{flag, ?wxEXPAND}]),
    wxSizer:add(Szr1, CtrlBox, [{proportion, 1}, {border, 2}, {flag, ?wxEXPAND}]),

    %% second level (Panel content): Snap Image, Opacity, Rotation
    BSnap = wxButton:new(CtrlBox, ?wxID_ANY, [{label,snap_label(snap)}]),
    wxWindow:setToolTip(BSnap, wxToolTip:new(snap_tooltip(snap))),
    TileChk = wxCheckBox:new(CtrlBox, ?wxID_ANY, snap_label(tiled)),
    wxCheckBox:setValue(TileChk,Tiled=:=1),
    wxWindow:setToolTip(TileChk, wxToolTip:new(snap_tooltip(tiled))),
    OpaLbl = wxStaticText:new(CtrlBox, ?wxID_ANY, append_value(opacity,OpaVal*100.0)),
    Opa = wxSlider:new(CtrlBox, ?wxID_ANY, round(OpaVal*100.0), 0, 100),
    wxWindow:setToolTip(Opa, wxToolTip:new(snap_tooltip(opacity))),
    RotLbl = wxStaticText:new(CtrlBox, ?wxID_ANY, append_value(rotate,0.0)),
    Rot = wxSlider:new(CtrlBox, ?wxID_ANY, 0, -360, 360),
    wxWindow:setToolTip(Rot, wxToolTip:new(snap_tooltip(rotate))),
    ModLbx = wxListBox:new(CtrlBox, ?wxID_ANY, [{style, ?wxLB_SINGLE}, {choices, mod_choices()}]),
    wxWindow:setBackgroundColour(ModLbx, BG),
    %% layout settings for the second level controls
    Szr2 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Szr2, BSnap, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Szr2, 3),
    wxSizer:add(Szr2, TileChk, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Szr2, 3),
    wxSizer:add(Szr2, OpaLbl, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Szr2, Opa, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Szr2, RotLbl, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Szr2, Rot, [{proportion, 0}, {border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Szr2, 3),
    wxSizer:add(Szr2, ModLbx, [{proportion, 1}, {border, 2}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(CtrlBox, Szr2),

    wxSizer:add(Main, Szr1, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:fit(Main, Panel),

    wxToggleButton:enable(BAct, [{enable,false}]),
    wxPanel:enable(CtrlBox, [{enable,false}]),

    wxComboBox:connect(ImgLst, command_combobox_selected),
    %% needed to manage the fake option 'Select image'
    wxComboBox:connect(ImgLst, command_text_updated),
    wxComboBox:connect(ImgLst, command_text_enter),

    wxToggleButton:connect(BAct, command_togglebutton_clicked),
    wxButton:connect(BSnap, command_button_clicked),
    wxListBox:connect(ModLbx, left_up),
    wxListBox:connect(ModLbx, motion),
    wxListBox:connect(ModLbx, leave_window),
    wxCheckBox:connect(TileChk, command_checkbox_clicked),
    wxSlider:connect(Opa,command_slider_updated),
    wxSlider:connect(Rot,command_slider_updated),

    %% this will allow us to set the window as focused
    wxWindow:connect(ImgLst, enter_window, [{userData, {win, Panel}}]),
    wxWindow:connect(BAct, enter_window, [{userData, {win, Panel}}, {skip, true}]),
    wxWindow:connect(BSnap, enter_window, [{userData, {win, Panel}}, {skip, true}]),
    wxWindow:connect(Opa, enter_window, [{userData, {win, Panel}}]),
    wxWindow:connect(Rot, enter_window, [{userData, {win, Panel}}]),
    wxWindow:connect(ModLbx, enter_window, [{userData, {win, Panel}}]),

    ?GET(top_frame) =:= Frame orelse wxSizer:setSizeHints(Main, Frame),
    Entries = [],
    {Panel, #state{me=Panel, name=Name, shown=Entries, cols=Cols, state=State,
		   ctrls=#{imglst=>ImgLst,actbtn=>BAct,ctrlbox=>CtrlBox,sldopa=>Opa,
			   sldrot=>Rot,opalbl=>OpaLbl,rotlbl=>RotLbl,tilechk=>TileChk}}}.

setup_image_list(Ctrl) ->
    Images = find_images(),
    SelStr =
	case wxComboBox:getSelection(Ctrl) of
	    ?wxNOT_FOUND -> ignore;
	    Idx -> wxComboBox:getString(Ctrl,Idx)
	end,
    wxComboBox:clear(Ctrl),
    lists:foldl(fun(Choice,N) ->
                        setup_choices(Choice, Ctrl, -1, N)
		end, 0, Images),

    %% In case the image list be updated by an image addition or
    %% exclusion - check if the old is still there
    if SelStr =/= ignore ->
            wxComboBox:setStringSelection(Ctrl, SelStr);
       true ->
            wxComboBox:setValue(Ctrl, snap_label(image)),
            false
    end.

setup_choices({Str, Tag}, Ctrl, Def, N) ->
    wxComboBox:append(Ctrl, Str, Tag),
    Def =:= Tag andalso wxComboBox:setSelection(Ctrl, N),
    N + 1.

reset_choice_to_empty(Ctrl) ->
    Str = snap_label(image),
    wxComboBox:setValue(Ctrl, Str),
    wxComboBox:setSelection(Ctrl, 0, length(Str)).

%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event=#wxCommand{type=command_text_enter, commandInt=Op}, obj=Obj}, State) ->
    %% test for invalid text input when pressed Enter
    case Op of
	-1 -> reset_choice_to_empty(Obj);
	_ -> ignore
    end,
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=Name}, obj=Obj}, State) ->
    if Name =:= [] ->
            reset_choice_to_empty(Obj);
       true -> ok
    end,
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_combobox_selected, cmdString=Name, commandInt=Op}, obj=Obj},
	     #state{ctrls=#{actbtn:=BAct}}=State) ->
    ImgId = wxComboBox:getClientData(Obj,Op),
    #e3d_image{width=W, height=H, name=Name} = wings_image:info(ImgId),
    case wxToggleButton:getValue(BAct) of
	true ->
	    Snap = #{reply=>ImgId,name=>Name,w=>W,h=>H},
	    wings_wm:psend(geom, {action,{snap_image,{start,Snap}}});
	_ ->
	    wxToggleButton:enable(BAct, [{enable,true}])
    end,
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_togglebutton_clicked}, obj=Btn},
	     #state{ctrls=#{imglst:=ImgLst,ctrlbox:=CtrlBox}}=State) ->
    Toggled = wxToggleButton:getValue(Btn),
    wxPanel:enable(CtrlBox, [{enable,Toggled}]),
    Toggle =
	case Toggled of
	    true -> deactivate;
	    _ -> activate
	end,
    wxToggleButton:setLabel(Btn, snap_label(Toggle)),
    wxToggleButton:setToolTip(Btn, snap_tooltip(Toggle)),
    case Toggled of
	true ->
	    Op = wxComboBox:getSelection(ImgLst),
	    ImgId = wxComboBox:getClientData(ImgLst,Op),
	    #e3d_image{width=W, height=H, name=Name} = wings_image:info(ImgId),
            Snap = #{reply=>ImgId,name=>Name,w=>W,h=>H},
	    wings_wm:psend(geom, {action,{snap_image,{start,Snap}}});
	_ ->
	    wings_wm:psend(geom, {action,{snap_image,cancel}})
    end,
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_button_clicked}}, State) ->
    wings_wm:psend(geom, {action,{snap_image,apply}}),
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_checkbox_clicked, commandInt=Val}, obj=_Obj}, State) ->
    wings_wm:psend(geom, {action,{snap_image,{tiled,Val}}}),
    {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_slider_updated, commandInt=Val}, obj=Obj},
	     #state{ctrls=#{sldopa:=Opa,sldrot:=Rot,opalbl:=OpaLbl,rotlbl:=RotLbl}}=State) ->
    case Obj of
        Opa ->
	    wxStaticText:setLabel(OpaLbl,append_value(opacity,Val*1.0)),
	    wings_wm:psend(geom, {action,{snap_image,{opacity,Val/100.0}}});
        Rot ->
	    wxStaticText:setLabel(RotLbl,append_value(rotate,Val/2.0)),
	    wings_wm:psend(geom, {action,{snap_image,{rotate,Val/2.0}}}); %% precision of 0.5 degree
        _ -> ignore
    end,
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=motion, x=X, y=Y}, obj=LBox}, State) ->
    Sel = wxListBox:hitTest(LBox, {X,Y}),
    Sel0 =
    	case wxListBox:getSelections(LBox) of
	    {_,[Idx]} -> Idx;
	    _ -> ignore
	end,
    MKind =
	case Sel of
	    0 -> move;
	    1 -> scale;
	    2 -> fit;
	    _ -> -1
	end,
    if (Sel >= 0) and (Sel =/= Sel0) ->
            wxListBox:setSelection(LBox, Sel),
            wxListBox:setToolTip(LBox, snap_tooltip(MKind)),
            wings_status:message(wings_wm:this(),snap_tooltip(MKind));
       true -> ok
    end,
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=leave_window}, obj=LBox}, State) ->
    case wxListBox:getSelections(LBox) of
	{_,[Sel]} -> wxListBox:deselect(LBox,Sel);
	_ -> ignore
    end,
    wxListBox:setToolTip(LBox, ""),
    wings_status:message(wings_wm:this(),""),
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=left_up, x=X, y=Y}, obj=LBox}, State) ->
    MPos = wxWindow:clientToScreen(LBox, {X,Y}),
    case wxListBox:getSelections(LBox) of
	{_,[Sel]} ->
	    MKind =
		case Sel of
		    0 -> move;
		    1 -> scale;
		    2 -> fit
		end,
	    wxListBox:deselect(LBox,Sel),
	    Menus = snap_menu(MKind),
	    Cmd = fun(_) -> wings_menu:popup_menu(LBox, MPos, ?MODULE, Menus) end,
	    wings_wm:psend(?WIN_NAME, {apply, false, Cmd});
	_ ->
	    ignore
    end,
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, State) ->
    wings_frame ! Ev,
    {noreply, State};
handle_event(#wx{} = _Ev, State) ->
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({note,image_change}, #state{ctrls=#{imglst:=ImgLst,actbtn:=BAct}}=State) ->
    Active = setup_image_list(ImgLst),
    %% Active is true if current image is still in the image list
    %% also, when a new project is started of the image deleted the UI will be updated
    wxToggleButton:setValue(BAct, Active),
    if not Active ->
            wxToggleButton:enable(BAct, [{enable,false}]),
            Cmd = #wxCommand{type=command_togglebutton_clicked, cmdString=[], commandInt=0, extraLong=0},
            Evt = #wx{id=?wxID_ANY, obj=BAct, event=Cmd, userData=[]},
            handle_event(Evt, State),
            wings_wm:psend(geom, {action,{snap_image,cancel}});
       true -> ignore
    end,
    {noreply, State};
handle_cast({action,_}=Cmd, State) ->
    %% fowarding the context menu options from window to module process
    wings_wm:psend(geom, Cmd),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{name=Name}) ->
    erase(?MODULE),
    wings_wm:psend(geom, {action,{snap_image,cancel}}),
    wings ! {wm, {delete, Name}},
    normal.
