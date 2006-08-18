%%
%%  wings_render.erl --
%%
%%     Render all objects and helpers (such as axes) in the scene.
%%     Used for the Geometry and AutoUV windows.
%%
%%  Copyright (c) 2001-2006 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_render.erl,v 1.15 2006/07/27 02:22:24 antoneos Exp $
%%

-module(wings_render).
-export([init/0, render/1,polygonOffset/1,
	 enable_lighting/0, disable_lighting/0]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

init() ->
    case wings_gl:support_shaders() of
	true ->
	    try 
		Sh = wings_gl:compile(vertex, light_shader_src()),
		Prog = wings_gl:link_prog([Sh]),
 		gl:useProgram(Prog),
		wings_pref:set_default(hl_lightpos,  {3.0,10.0,1.0}),
		wings_pref:set_default(hl_skycol,    {0.95,0.95,0.90}),
		wings_pref:set_default(hl_groundcol, {0.026,0.024,0.021}),
		wings_gl:set_uloc(Prog, "LightPosition", 
				  wings_pref:get_value(hl_lightpos)),
		wings_gl:set_uloc(Prog, "SkyColor",
				  wings_pref:get_value(hl_skycol)),
		wings_gl:set_uloc(Prog, "GroundColor",
				  wings_pref:get_value(hl_groundcol)),
		?CHECK_ERROR(),
 		gl:useProgram(0),
		put(light_shader, Prog),
		io:format("Using GPU shaders.\n"),
		ok
	    catch _:_Err -> ok
	    end;
	false ->
	    ok
    end.

%% render(St)
%%  Render the entire contents of a Geometry or AutoUV window,
%%  including groundplane and axes. Use the contents of the display
%%  lists maintained by wings_dl. All display lists must
%%  already exist; no display lists are created by this function.

render(#st{selmode=Mode}=St) ->
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_TEXTURE_BIT bor ?GL_POLYGON_BIT bor
		  ?GL_LINE_BIT bor ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    wings_view:load_matrices(true),
    ground_and_axes(),
    show_saved_bb(St),
    render_objects(Mode),
    axis_letters(),
    gl:lineWidth(1),
    wings_io:ortho_setup(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    {W,H} = wings_wm:win_size(),
    gl:rectf(W-0.5, 0.5, 0.5, H-0.5),
    gl:popAttrib().


%%%
polygonOffset(M) ->
    case get(polygon_offset) of
	undefined ->
	    F = wings_pref:get_value(polygon_offset_f,1.0),
	    R = wings_pref:get_value(polygon_offset_r,1.0),
	    put(polygon_offset, {F,R}),
	    gl:polygonOffset(M*F, M*R);
	{F,R} -> 
	    gl:polygonOffset(M*F, M*R)
    end.
%%%
%%% Internal functions follow.
%%%

render_objects(Mode) ->
    Dls = wings_dl:display_lists(),
    case wings_wm:get_prop(workmode) of
	false ->
	    render_smooth_objects(Dls, Mode, false),
	    render_smooth_objects(Dls, Mode, true);
	true ->
	    render_work_objects(Dls, Mode)
    end.

render_smooth_objects([D|Dls], Mode, RenderTrans) ->
    render_object(D, Mode, false, RenderTrans),
    render_smooth_objects(Dls, Mode, RenderTrans);
render_smooth_objects([], _, _) -> ok.

render_work_objects([D|Dls], Mode) ->
    render_object(D, Mode, true, false),
    render_work_objects(Dls, Mode);
render_work_objects([], _) -> ok.
    
render_object(#dlo{drag={matrix,_,_,Matrix}}=D, Mode, Work, RT) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_1(D, Mode, Work, RT),
    gl:popMatrix();
render_object(D, Mode, Work, RT) ->
    render_object_1(D, Mode, Work, RT).

render_object_1(#dlo{mirror=none}=D, Mode, Work, RenderTrans) ->
    render_object_2(D, Mode, Work, RenderTrans);
render_object_1(#dlo{mirror=Matrix}=D, Mode, Work, RenderTrans) ->
    render_object_2(D, Mode, Work, RenderTrans),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_2(D, Mode, Work, RenderTrans),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW).

render_object_2(#dlo{src_we=We}=D, _, _, false) when ?IS_LIGHT(We) ->
    wings_light:render(D);
render_object_2(#dlo{src_we=We}, _, _, true) when ?IS_LIGHT(We) ->
    ok;
render_object_2(D, Mode, true, _) ->
    render_plain(D, Mode);
render_object_2(#dlo{transparent=true}=D, _, false, false) ->
    gl:disable(?GL_CULL_FACE),
    render_smooth(D, false),
    gl:enable(?GL_CULL_FACE);
render_object_2(#dlo{transparent=true}=D, _, false, true) ->
    render_smooth(D, true);
render_object_2(#dlo{transparent=false}=D, _, false, RenderTrans) ->
    render_smooth(D, RenderTrans).

render_plain(#dlo{work=Faces,edges=Edges,open=Open,
		  src_we=We,proxy_data=none}=D, SelMode) ->
    %% Draw faces for winged-edge-objects.
    Wire = wire(We),
    case Wire of
	false ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    polygonOffset(2),
	    gl:shadeModel(?GL_SMOOTH),
	    enable_lighting(),
	    
	    case Open of
		false ->
		    wings_dl:call(Faces);
		true ->
		    gl:disable(?GL_CULL_FACE),
		    wings_dl:call(Faces),
		    gl:enable(?GL_CULL_FACE)
	    end,
	    disable_lighting(),
	    gl:shadeModel(?GL_FLAT);
	true -> ok
    end,

    %% Draw edges.
    case Wire orelse wings_pref:get_value(show_edges) of
	false -> ok;
	true ->
	    case {SelMode,wings_pref:get_value(edge_color)} of
		{body,{0.0,0.0,0.0}} ->
		    gl:color3f(0.3, 0.3, 0.3);
		{_,EdgeColor} ->
		    gl:color3fv(EdgeColor)
	    end,
	    gl:lineWidth(edge_width(SelMode)),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    polygonOffset(1),
	    case Wire andalso wings_wm:get_prop(show_wire_backfaces) of
		true ->
		    gl:disable(?GL_CULL_FACE),
		    wings_dl:call(Edges),
		    gl:enable(?GL_CULL_FACE);
		false ->
		    wings_dl:call(Edges)
	    end
    end,
    render_plain_rest(D, Wire, SelMode);
render_plain(#dlo{src_we=We}=D, SelMode) ->
    Wire = wire(We),
    wings_proxy:draw(D, Wire),
    render_plain_rest(D, Wire, SelMode).

render_plain_rest(D, Wire, SelMode) ->
    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    draw_hilite(D),
    case Wire of
	true ->
	    gl:disable(?GL_CULL_FACE),
	    draw_orig_sel(D),
	    draw_sel(D),
	    gl:enable(?GL_CULL_FACE);
	false ->
	    draw_orig_sel(D),
	    draw_sel(D)
    end,
    draw_vertices(D, SelMode),
    draw_hard_edges(D, SelMode),
    draw_normals(D).

render_smooth(#dlo{work=Work,edges=Edges,smooth=Smooth,transparent=Trans,
		   src_we=We,proxy_data=Pd,open=Open}=D,
	      RenderTrans) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    enable_lighting(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2, 2),

    case Trans of
	false -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE);
	true -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE)
    end,

    case RenderTrans of
	true ->
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:depthMask(?GL_FALSE);
	false ->
	    gl:disable(?GL_BLEND),
	    gl:depthMask(?GL_TRUE)
    end,

    case Open of
	false -> ok;
	true -> gl:disable(?GL_CULL_FACE)
    end,

    case {Smooth,RenderTrans} of
	{none,false} ->
	    if
		Pd =:= none ->
		    wings_dl:call(Work);
		true ->
		    wings_proxy:draw(D, wire(We))
	    end;
	{[Op,_],false} -> wings_dl:call(Op);
	{[_,Tr],true} -> wings_dl:call(Tr);
	{_,_} -> ok
    end,
    gl:enable(?GL_CULL_FACE),

    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:depthMask(?GL_TRUE),
    disable_lighting(),
    gl:shadeModel(?GL_FLAT),
    case wire(We) of
	true when Pd =:= none ->
	    gl:color3fv(wings_pref:get_value(edge_color)),
	    gl:lineWidth(1),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1, 1),
	    wings_dl:call(Edges);
	true ->
	    wings_proxy:draw_smooth_edges(D);
	false -> ok
    end,
    draw_hilite(D),
    draw_orig_sel(D),
    draw_sel(D).

wire(#we{id=Id}) ->
    W = wings_wm:get_prop(wireframed_objects),
    gb_sets:is_member(Id, W).

draw_sel(#dlo{sel=SelDlist,src_sel={edge,_}}) ->
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    sel_color(),
    wings_dl:call(SelDlist);
draw_sel(#dlo{sel=SelDlist,src_sel={vertex,_}}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    sel_color(),
    wings_dl:call(SelDlist);
draw_sel(#dlo{orig_sel=OrigSel,sel=SelDlist}) ->
    sel_color(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1, 1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case OrigSel =/= none orelse wings_pref:get_value(selection_style) =:= solid of
	true ->					%Solid selection style.
	    wings_dl:call(SelDlist);
	false ->				%Stippled selection style.
	    gl:enable(?GL_POLYGON_STIPPLE),
	    wings_dl:call(SelDlist),
	    gl:disable(?GL_POLYGON_STIPPLE)
    end.

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_vertices(#dlo{src_we=#we{perm=P},vs=VsDlist}, vertex) when ?IS_SELECTABLE(P) ->
    wings_dl:call(VsDlist);
draw_vertices(_, _) -> ok.

draw_hilite(#dlo{hilite=DL}) -> wings_dl:call(DL).

draw_orig_sel(#dlo{orig_sel=none}) -> ok;
draw_orig_sel(#dlo{orig_sel=Dlist,orig_mode=Mode}) ->
    draw_orig_sel_1(Mode, Dlist).

draw_orig_sel_1(vertex, DlistSel) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)*2),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    wings_dl:call(DlistSel),
    gl:disable(?GL_BLEND);
draw_orig_sel_1(edge, DlistSel) ->
    gl:lineWidth(wings_pref:get_value(selected_edge_width)*2),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    wings_dl:call(DlistSel),
    gl:disable(?GL_BLEND);
draw_orig_sel_1(_, DlistSel) ->
    sel_color(),
    gl:enable(?GL_POLYGON_STIPPLE),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1, 1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_dl:call(DlistSel),
    gl:disable(?GL_POLYGON_STIPPLE).

draw_hard_edges(#dlo{hard=none}, _) -> ok;
draw_hard_edges(#dlo{hard=Hard}, SelMode) ->
    gl:lineWidth(edge_width(SelMode)),
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    wings_dl:call(Hard).

draw_normals(#dlo{normals=none}) -> ok;
draw_normals(#dlo{normals=Ns}) ->
    gl:color3f(0, 0, 1),
    gl:lineWidth(2),
    wings_dl:call(Ns).

edge_width(edge) -> wings_pref:get_value(edge_width);
edge_width(_) -> 1.

ground_and_axes() ->
    Axes = wings_wm:get_prop(show_axes),
    ?CHECK_ERROR(),
    groundplane(Axes),
    ?CHECK_ERROR(),
    case wings_pref:get_value(constrain_axes) of
	true -> Yon = ?GROUND_GRID_SIZE * 10.0;
	false -> #view{yon=Yon} = wings_view:current()
    end,
    case Axes of
	true ->
	    axis(1, Yon, get_pref(x_color), get_pref(neg_x_color)),
	    axis(2, Yon, get_pref(y_color), get_pref(neg_y_color)),
	    axis(3, Yon, get_pref(z_color), get_pref(neg_z_color));
	false -> ok
    end.

get_pref(Key) ->
    wings_pref:get_value(Key).

axis(I, Yon, Pos, Neg) ->
    A0 = {0.0,0.0,0.0},
    A = setelement(I, A0, Yon),
    B = setelement(I, A0, -Yon),
    gl:'begin'(?GL_LINES),
    gl:color3fv(Pos),
    gl:vertex3fv(A0),
    gl:vertex3fv(A),
    gl:color3fv(Neg),
    gl:vertex3fv(A0),
    gl:vertex3fv(B),
    gl:'end'().

dummy_axis_letter() ->
    %% Attempt to work around a crash occurring with Matrox cards.
    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
    %% Since this is a workaround, we will do a real fetching
    %% of the viewport (rather than wings_wm:viewport/0).
    [X,Y,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Viewport = {X,Y,W,H},
    dummy_axis_letter(MM, PM, Viewport).

dummy_axis_letter(_, _, {_,_,W,H}) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    wings_io:set_color(wings_pref:get_value(background_color)),
    axis_text(10, 90, axisx),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).

axis_letters() ->
    case wings_pref:get_value(show_axis_letters) andalso
	wings_wm:get_prop(show_axes) of
	false ->
	    case wings_pref:get_value(dummy_axis_letter) of
		false -> ok;
		true -> dummy_axis_letter()
	    end;
	true ->
	    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
	    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
	    ViewPort = wings_wm:viewport(),
	    Start = {0.0,0.0,0.0},
	    Origin = proj(Start, MM, PM),
	    Info = {Start,Origin,MM,PM,ViewPort},

	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    {_,_,W,H} = ViewPort,
	    glu:ortho2D(0, W, 0, H),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),

	    case wings_pref:get_value(constrain_axes) of
		true -> Yon = ?GROUND_GRID_SIZE * 11.0;
		false -> #view{yon=Yon} = wings_view:current()
	    end,
	    axis_letter_1(1, Yon, axisx, x_color, Info),
	    axis_letter_1(2, Yon, axisy, y_color, Info),
 	    axis_letter_1(3, Yon, axisz, z_color, Info)
    end.

axis_letter_1(I, Yon, Char, Color0, {Start,{Ox,Oy,_,Ow},MM,PM,Viewport}) ->
    Color = wings_pref:get_value(Color0),
    wings_io:set_color(Color),
    End = setelement(I, Start, Yon),
    {Px,Py,_,Pw} = proj(End, MM, PM),
    NegPw = -Pw,
    if
	NegPw < Px, Px < Pw, NegPw < Py, Py < Pw ->
	    show_letter(Px, Py, Pw, Char, Viewport);
	true ->
	    clip(Ox, Oy, Ow, Px, Py, Pw, Char, Viewport)
    end.

clip(Ox, Oy, Ow, Px, Py, Pw, Char, Viewport) ->
    AxisRay = line(Ox, Oy, Px, Py),
    NegOw = -Ow,
    Lines = [line(NegOw, NegOw, Ow, NegOw),line(NegOw, Ow, Ow, Ow),
	     line(NegOw, NegOw, NegOw, Ow),line(Ow, NegOw, Ow, Ow)],
    case clip_1(AxisRay, Lines, {Ow,Pw}) of
	none -> ok;
	{X,Y,W} -> show_letter(X, Y, W, Char, Viewport)
    end.

clip_1({O1,D1}=Axis, [{O2,D2}|Lines], {Ow,_}=W) ->
    E = 1.0E-6,
    case {pdot(D1, D2),pdot(D2, D1)} of
	{Z1,Z2} when abs(Z1) < E; abs(Z2) < E ->
	    clip_1(Axis, Lines, W);
	{Div1,Div2} ->
	    S = pdot(sub(O2, O1), D2)/Div1,
	    T = pdot(sub(O1, O2), D1)/Div2,
	    if
		S < 0.0; T < 0.0; T > 1.0 ->
		    clip_1(Axis, Lines, W);
		true ->
		    {X,Y} = add_prod(O1, D1, S),
		    {X,Y,Ow}
	    end
    end;
clip_1(_, [], _W) -> none.

show_letter(X0, Y0, W, Char, {_,_,Vw,Vh}) ->
    X = trunc((0.5*X0/W+0.5)*(Vw-20) + 10),
    Y = trunc((0.5*Y0/W+0.5)*(Vh-16) + 7),
    axis_text(X, Y, Char).

axis_text(X, Y, C) ->
    gl:rasterPos2i(X, Y),
    wings_text:char(C).

proj({X0,Y0,Z0}, MM, PM) ->
    e3d_mat:mul(PM, e3d_mat:mul(MM, {X0,Y0,Z0,1.0})).

line(Ox, Oy, Px, Py) -> {{Ox,Oy},{Px-Ox,Py-Oy}}.

pdot({X1,Y1}, {X2,Y2}) when is_float(X1), is_float(Y1) ->
    Y1*X2-X1*Y2.

sub({X1,Y1}, {X2,Y2}) ->
    {X1-X2,Y1-Y2}.

add_prod({X1,Y1}, {X2,Y2}, S) when is_float(S) ->
    {S*X2+X1,S*Y2+Y1}.

groundplane(Axes) ->
    case (wings_wm:get_prop(show_groundplane) orelse
	  (wings_pref:get_value(force_show_along_grid) andalso
	   (wings_view:current())#view.along_axis =/= none)) of
	true -> groundplane_1(Axes);
	false -> ok
    end.

groundplane_1(Axes) ->
    #view{along_axis=Along} = wings_view:current(),
    gl:color3fv(wings_pref:get_value(grid_color)),
    gl:lineWidth(1),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    case Along of
	x -> gl:rotatef(90, 0, 1, 0);
	z -> ok;
	_ -> gl:rotatef(90, 1, 0, 0)
    end,
    gl:'begin'(?GL_LINES),
    Sz = ?GROUND_GRID_SIZE * 10,
    groundplane_2(-Sz, Sz, Sz, Axes),
    gl:'end'(),
    gl:popMatrix(),
    ?CHECK_ERROR().

groundplane_2(X, Last, _Sz, _Axes) when X > Last -> ok;
groundplane_2(X, Last, Sz, true) when X == 0 ->
    groundplane_2(?GROUND_GRID_SIZE, Last, Sz, true);
groundplane_2(X, Last, Sz, Axes) ->
    gl:vertex2f(X, -Sz),
    gl:vertex2f(X, Sz),
    gl:vertex2f(-Sz, X),
    gl:vertex2f(Sz, X),
    groundplane_2(X+?GROUND_GRID_SIZE, Last, Sz, Axes).

show_saved_bb(#st{bb=[{X1,Y1,Z1},{X2,Y2,Z2}]}) ->
    case wings_pref:get_value(show_bb) of
	false -> ok;
	true ->
	    gl:enable(?GL_LINE_STIPPLE),
	    gl:lineStipple(4, 2#1110111011101110),
	    gl:color3f(0, 0, 1),
	    gl:'begin'(?GL_LINE_STRIP),
	    gl:vertex3f(X1, Y1, Z1),
	    gl:vertex3f(X2, Y1, Z1),
	    gl:vertex3f(X2, Y2, Z1),
	    gl:vertex3f(X1, Y2, Z1),
	    gl:vertex3f(X1, Y1, Z1),
	    gl:vertex3f(X1, Y1, Z2),
	    gl:vertex3f(X2, Y1, Z2),
	    gl:vertex3f(X2, Y2, Z2),
	    gl:vertex3f(X1, Y2, Z2),
	    gl:vertex3f(X1, Y1, Z2),
	    gl:'end'(),
	    gl:'begin'(?GL_LINES),
	    gl:vertex3f(X1, Y2, Z1),
	    gl:vertex3f(X1, Y2, Z2),
	    gl:vertex3f(X2, Y2, Z1),
	    gl:vertex3f(X2, Y2, Z2),
	    gl:vertex3f(X2, Y1, Z1),
	    gl:vertex3f(X2, Y1, Z2),
	    gl:'end'(),
	    gl:disable(?GL_LINE_STIPPLE)
    end;
show_saved_bb(_) -> ok.

enable_lighting() ->
    Prog = get(light_shader),
    UseProg = 
	(Prog /= undefined) andalso (not wings_pref:get_value(scene_lights)) 
        andalso 2 == wings_pref:get_value(number_of_lights),
    case UseProg of
	false -> 
	    gl:enable(?GL_LIGHTING);
	true ->
	    gl:color4ub(255,255,255,255), %% Reset color needed by crappy drivers.
	    %% We put it here and not in apply_material because we can't use some
	    %% optimizations (i.e. reuse display lists) when drawing selected objects
	    gl:useProgram(Prog)
    end.


disable_lighting() ->
    gl:disable(?GL_LIGHTING),
    case get(light_shader) /= undefined of
 	true -> gl:useProgram(0);
	false -> ok
    end.


light_shader_src() ->
    <<"							       
       uniform vec3 LightPosition;				       
       uniform vec3 SkyColor;					       
       uniform vec3 GroundColor;				       
       							       
       void main() 						       
       {							       
           vec3 ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);    
           vec3 tnorm      = normalize(gl_NormalMatrix * gl_Normal);  
           vec3 lightVec   = normalize(LightPosition - ecPosition);   
           float costheta  = dot(tnorm, lightVec);
           float a         = 0.5 + 0.5 * costheta;
                             // ATI needs this for vcolors to work
           vec4 color      = gl_FrontMaterial.diffuse * gl_Color; 
           gl_FrontColor   = color * vec4(mix(GroundColor, SkyColor, a), 1.0);
           gl_TexCoord[0]  = gl_MultiTexCoord0;
           gl_Position     = ftransform();
       }							       
       ">>.
