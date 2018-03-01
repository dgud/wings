%%
%%  wings_render.erl --
%%
%%     Render all objects and helpers (such as axes) in the scene.
%%     Used for the Geometry and AutoUV windows.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_render).
-export([init/0,
	 render/1,polygonOffset/1,
	 enable_lighting/1,disable_lighting/0]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

init() ->
    wings_shaders:init(),
    wings_pref:set_default(multisample, true),
    init_polygon_stipple().

%% render(St)
%%  Render the entire contents of a Geometry or AutoUV window,
%%  including groundplane and axes. Use the contents of the display
%%  lists maintained by wings_dl. All display lists must
%%  already exist; no display lists are created by this function.

render(#st{selmode=Mode}=St) ->
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_TEXTURE_BIT bor ?GL_POLYGON_BIT bor
		  ?GL_LINE_BIT bor ?GL_COLOR_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    case wings_pref:get_value(multisample) of
	true -> gl:enable(?GL_MULTISAMPLE);
	_ -> gl:disable(?GL_MULTISAMPLE)
    end,
    {PM,MM,SceneLights} = wings_view:load_matrices(true),
    Yon = ground_and_axes(PM,MM),
    mini_axis_icon(MM),
    show_saved_bb(St),
    show_bb_center(St),
    user_clipping_planes(on),
    render_objects(Mode, PM, SceneLights),
    user_clipping_planes(off),
    axis_letters(PM,MM,Yon),
    show_camera_image_plane(),
    gl:popAttrib(),
    call_post_hook(St),
    wings_develop:gl_error_check("Rendering scene").

call_post_hook(St) ->
    case wings_wm:lookup_prop(postdraw_hook) of
        none -> ok;
        {value,{_Id,Fun}} -> Fun(St)
    end.

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

%% According Game Programming Gems chap 4.1 (By Eric Lengyel)
%% http://www.terathon.com/books/code/Listing9.1.txt
%% http://terathon.com/gdc07_lengyel.pdf
%% Minimum Eps for 24b depth buffer (skip calc and use min value)
non_polygon_offset(Offset, TPM) ->
    ProjMat = e3d_transform:matrix(TPM),
    Eps = 1.0 - Offset*5.0e-7,
    E33 = element(11, ProjMat),  %% Modify entry {3,3}
    gl:loadMatrixd(setelement(11, ProjMat, E33*Eps)).

%%%
%%% Internal functions follow.
%%%

init_polygon_stipple() ->
    P = <<16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77,
	 16#DD,16#DD,16#DD,16#DD,16#77,16#77,16#77,16#77>>,
    gl:polygonStipple(P).

setup_scene_lights(false, _) ->
    false;
setup_scene_lights(true, Lights) ->
    {Amb0, SL} = wings_light:global_lights(Lights),
    case Amb0 of
        [] ->
            gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.0,0.0,0.0,1.0}),
            wings_shaders:use_prog(ambient_light),
            SL;
        [Amb|RestAmb] ->
            wings_light:setup_light(Amb),
            RestAmb ++ SL
    end.

render_objects(Mode, PM, UseSceneLights) ->
    Dls = wings_dl:display_lists(),
    {Open,Closed,Lights} = split_objects(Dls, [], [], []),
    render_lights(Lights),
    SL = setup_scene_lights(UseSceneLights, Lights),
    case wings_wm:get_prop(workmode) of
	false ->
            case UseSceneLights of
                true -> render_smooth_objects(Open, Closed, ambient); %% amb pass
                false -> ignore
            end,
	    render_smooth_objects(Open, Closed, SL),
            render_wire(Dls, Mode, true),
            render_sel_highlight(Dls, Mode, true, PM);
	true ->
            case UseSceneLights of
                true -> render_work_objects(Open, Closed, ambient); %% amb pass
                false -> ignore
            end,
            render_work_objects(Open, Closed, SL),
            render_wire(Dls, Mode, false),
            render_sel_highlight(Dls, Mode, false, PM)
    end.

render_lights(Lights) ->
    wings_shaders:use_prog(1),
    render_work_objects_0(Lights, false),
    gl:color4ub(255, 255, 255, 255).

render_work_objects(Open, Closed, SceneLights) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    polygonOffset(2.0),
    gl:shadeModel(?GL_SMOOTH),
    enable_lighting(SceneLights),
    render_work_objects_0(Closed, SceneLights),
    wings_pref:get_value(show_backfaces) andalso gl:disable(?GL_CULL_FACE),
    render_work_objects_0(Open, SceneLights),
    gl:enable(?GL_CULL_FACE),
    disable_lighting(),
    gl:shadeModel(?GL_FLAT),
    ok.

render_work_objects_0([D|Dls], SceneLights) ->
    render_object(D, true, false, SceneLights),
    render_work_objects_0(Dls, SceneLights);
render_work_objects_0([], _) -> ok.

render_smooth_objects(Open, Closed, SceneLights) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    enable_lighting(SceneLights),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    polygonOffset(2.0),
    gl:enable(?GL_CULL_FACE),
    render_smooth_objects_0(Closed, false, SceneLights),
    wings_pref:get_value(show_backfaces) andalso gl:disable(?GL_CULL_FACE),
    render_smooth_objects_0(Open, false, SceneLights),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_CULL_FACE),
    render_smooth_objects_0(Closed, true, SceneLights),
    wings_pref:get_value(show_backfaces) andalso gl:disable(?GL_CULL_FACE),
    render_smooth_objects_0(Open, true, SceneLights),
    gl:enable(?GL_CULL_FACE),
    gl:disable(?GL_BLEND),
    gl:depthMask(?GL_TRUE).

render_smooth_objects_0([D|Dls], RenderTrans, SceneLights) ->
    render_object(D, false, RenderTrans, SceneLights),
    render_smooth_objects_0(Dls, RenderTrans, SceneLights);
render_smooth_objects_0([], _, _) -> ok.

render_object(#dlo{drag={matrix,_,_,Matrix}}=D, Work, RT, SceneLights) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_1(D, Work, RT, SceneLights),
    gl:popMatrix();
render_object(D, Work, RT, SceneLights) ->
    render_object_1(D, Work, RT, SceneLights).

render_object_1(#dlo{mirror=none}=D, Work, RenderTrans, SceneLights) ->
    render_object_2(D, Work, RenderTrans, SceneLights);
render_object_1(#dlo{mirror=Matrix}=D, Work, RenderTrans, SceneLights) ->
    render_object_2(D, Work, RenderTrans, SceneLights),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_2(D, Work, RenderTrans, SceneLights),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW).

render_object_2(D, true, _, SceneLights) ->
    render_plain(D, SceneLights);
render_object_2(#dlo{transparent=true}=D, false, false, SceneLights) ->
    render_smooth(D, false, SceneLights);
render_object_2(D, false, RenderTrans, SceneLights) ->
    render_smooth(D, RenderTrans, SceneLights).

render_plain(#dlo{work=Faces,src_we=We,proxy=false}, SceneLights) ->
    case wire(We) of
	false -> render_lighted(Faces, SceneLights);
        true -> ok
    end;
render_plain(#dlo{proxy_data=PD, drag=Drag}, SceneLights) ->
    polygonOffset(3.0),
    Faces = wings_proxy:flat_dl(PD),
    Key = case Drag =:= none of
              true  -> proxy_static_opacity;
              false -> proxy_moving_opacity
          end,
    if SceneLights =:= false ->
            Blend = wings_gl:is_ext('GL_ARB_imaging') andalso wings_pref:get_value(Key),
            case Blend of
                false ->
                    render_lighted(Faces, SceneLights);
                1.0 ->
                    render_lighted(Faces, SceneLights);
                _ ->
                    gl:enable(?GL_BLEND),
		    gl:blendFunc(?GL_CONSTANT_ALPHA, ?GL_ONE_MINUS_CONSTANT_ALPHA),
		    gl:blendColor(0.0, 0.0, 0.0, Blend),
                    render_lighted(Faces, SceneLights),
                    gl:disable(?GL_BLEND)
            end;
       true ->
            render_lighted(Faces, SceneLights)
    end.

render_smooth(#dlo{work=Work,smooth=Smooth0,transparent=Trans0,
		   proxy=Proxy,proxy_data=PD},
	      RenderTrans, SceneLights) ->
    {Smooth, _Trans} =
        case Proxy of
            true -> wings_proxy:smooth_dl(PD);
            false -> {Smooth0,Trans0}
        end,

    Draw = case {Smooth,RenderTrans} of
               {none,false}   -> Work;
               {[Op,_],false} -> Op;
               {[_,Tr],true}  -> Tr;
               {_,_} -> none
           end,
    render_lighted(Draw, SceneLights).

render_lighted(Faces, false) ->
    wings_dl:call(Faces);
render_lighted(Faces, ambient) ->
    wings_dl:call(Faces);
render_lighted(Faces, Lights) ->
    Do = fun(Light) ->
                 wings_light:setup_light(Light),
                 wings_dl:call(Faces)
         end,
    _ = [Do(Light) || Light <- Lights],
    ok.

enable_lighting(false) ->
    Lighting = wings_pref:get_value(number_of_lights),
    gl:color4ub(255, 255, 255, 255), %% Needed when vertex colors are not set
    wings_shaders:use_prog(Lighting);
enable_lighting(ambient) ->
    ok;
enable_lighting(_) ->
    gl:color4ub(255, 255, 255, 255),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_ONE, ?GL_ONE).

disable_lighting() ->
    wings_gl:use_prog(0),
    gl:disable(?GL_BLEND),
    gl:depthMask(?GL_TRUE).

render_wire(Dls, SelMode, false) ->
    WOs = wings_wm:get_prop(wireframed_objects),
    Show = wings_pref:get_value(show_edges),
    case split_wires(Dls, WOs, Show, [], [], []) of
        {[],[],[]} -> ok;
        {Ws,Os,PWs} ->
            case {SelMode,wings_pref:get_value(edge_color)} of
		{body,{0.0,0.0,0.0}} ->
		    gl:color3f(0.3, 0.3, 0.3);
		{_,EdgeColor} ->
		    gl:color3fv(EdgeColor)
	    end,
            case wings_pref:get_value(aa_edges) of
		true ->
		    gl:enable(?GL_LINE_SMOOTH),
		    gl:enable(?GL_BLEND),
		    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
		    gl:hint(?GL_LINE_SMOOTH_HINT, ?GL_NICEST),
		    ok;
		false ->
		    ok
	    end,
	    gl:lineWidth(edge_width(SelMode)),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
            gl:depthFunc(?GL_LEQUAL),
            wings_wm:get_prop(show_wire_backfaces)
                andalso gl:disable(?GL_CULL_FACE),
            _ = [render_wire_object(D, cage) || D <- Ws],
            gl:disable(?GL_CULL_FACE),
            _ = [render_wire_object(D, cage) || D <- Os],
            gl:enable(?GL_CULL_FACE),
            gl:color3fv(wings_pref:get_value(edge_color)),
            gl:lineWidth(1.0),
            _ = [render_wire_object(D, cage) || D <- PWs],
            gl:enable(?GL_CULL_FACE),
            gl:depthFunc(?GL_LESS)
    end;
render_wire(Dls, _, true) ->
    gl:enable(?GL_CULL_FACE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:depthMask(?GL_TRUE),
    disable_lighting(),
    gl:shadeModel(?GL_FLAT),
    gl:depthFunc(?GL_LEQUAL),
    WOs = wings_wm:get_prop(wireframed_objects),
    {Ws,[],PWs} = split_wires(Dls, WOs, false, [], [], []),
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    _ = [render_wire_object(D, cage) || D <- Ws],
    Style = wings_pref:get_value(proxy_shaded_edge_style),
    _ = [render_wire_object(D, Style) || D <- PWs],
    ok.

render_wire_object(#dlo{drag={matrix,_,_,Matrix}}=D, PStyle) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_wire_object_1(D, PStyle),
    gl:popMatrix();
render_wire_object(D, PStyle) ->
    render_wire_object_1(D, PStyle).

render_wire_object_1(#dlo{mirror=none, edges=Edges, proxy=false}, _) ->
    wings_dl:call(Edges);
render_wire_object_1(#dlo{mirror=Matrix, edges=Edges, proxy=false}, _) ->
    wings_dl:call(Edges),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    wings_dl:call(Edges),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW);
render_wire_object_1(#dlo{mirror=none}=D, PStyle) ->
    wings_proxy:draw_smooth_edges(D, PStyle);
render_wire_object_1(#dlo{mirror=Matrix}=D, PStyle) ->
    wings_proxy:draw_smooth_edges(D, PStyle),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    wings_proxy:draw_smooth_edges(D, PStyle),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW).

split_objects([#dlo{src_we=We}=D|Dls], Open, Closed, Lights) when ?IS_ANY_LIGHT(We) ->
    split_objects(Dls, Open, Closed, [D|Lights]);
split_objects([#dlo{open=true}=D|Dls], Open, Closed, Lights) ->
    split_objects(Dls, [D|Open], Closed, Lights);
split_objects([#dlo{open=false}=D|Dls], Open, Closed, Lights) ->
    split_objects(Dls, Open, [D|Closed], Lights);
split_objects([], Open, Closed, Lights) ->
    {Open, Closed, Lights}.

split_wires([#dlo{src_we=#we{id=Id}, proxy=Proxy}=D|Dls], WOs, Show, Wires, Others,Proxis) ->
    case gb_sets:is_member(Id, WOs) of
        true when Proxy -> split_wires(Dls, WOs, Show, Wires, Others, [D|Proxis]);
        true -> split_wires(Dls, WOs, Show, [D|Wires], Others,Proxis);
        false when Proxy -> split_wires(Dls, WOs, Show, Wires, Others,Proxis);
        false when Show -> split_wires(Dls, WOs, Show, Wires, [D|Others], Proxis);
        false -> split_wires(Dls, WOs, Show, Wires, Others,Proxis)
    end;
split_wires([], _, _, Ws, Os, Ps) ->
    {Ws, Os, Ps}.

render_sel_highlight(Dls, SelMode, false, PM) ->
    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    non_polygon_offset(1.0, PM),
    gl:matrixMode(?GL_MODELVIEW),
    gl:depthFunc(?GL_LEQUAL),
    _ = [render_sel(D, SelMode, false) || D <- Dls],
    _ = [render_sel(D, SelMode, true) || D <- Dls],
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:depthFunc(?GL_LESS),
    gl:matrixMode(?GL_MODELVIEW),
    %% arbitrary placement in the grand scheme of things
    _ = [wings_plugin:draw(plain,D,SelMode) || D <- Dls],
    ok;
render_sel_highlight(Dls, SelMode, true, _PM) ->
    _ = [render_sel(D, SelMode, true) || D <- Dls],
    gl:depthFunc(?GL_LESS),
    %% arbitrary placement in the grand scheme of things
    _ = [wings_plugin:draw(smooth,D,none) || D <- Dls],
    ok.

render_sel(#dlo{drag={matrix,_,_,Matrix}}=D, Mode, Sel) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_sel_1(D, Mode, Sel),
    gl:popMatrix();
render_sel(D, Mode, Sel) ->
    render_sel_1(D, Mode, Sel).

render_sel_1(#dlo{mirror=none}=D, Mode, Sel) ->
    render_sel_2(D, Mode, Sel);
render_sel_1(#dlo{mirror=Matrix}=D, Mode, Sel) ->
    render_sel_2(D, Mode, Sel),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_sel_2(D, Mode, Sel),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW).

render_sel_2(#dlo{}=D, SelMode, false) ->
    draw_hard_edges(D, SelMode),
    draw_vertices(D, SelMode),
    draw_normals(D);
render_sel_2(#dlo{src_we=We}=D, _SelMode, true) ->
    case wire(We) of
	true ->
	    gl:disable(?GL_CULL_FACE),
	    draw_orig_sel(D),
	    draw_sel(D),
	    gl:enable(?GL_CULL_FACE);
	false ->
            draw_orig_sel(D),
	    draw_sel(D)
    end,
    draw_hilite(D).

wire(#we{id=Id}) ->
    W = wings_wm:get_prop(wireframed_objects),
    gb_sets:is_member(Id, W).

draw_sel(#dlo{sel=none}) -> ok;
draw_sel(#dlo{sel=SelDlist,src_sel={edge,_}}) ->
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    sel_color(),
    wings_dl:call(SelDlist);
draw_sel(#dlo{sel=SelDlist,src_sel={vertex,_}}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    sel_color(),
    wings_dl:call(SelDlist);
draw_sel(#dlo{open=Open,sel=SelDlist}) ->
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    polygonOffset(1.0),
    %Stippled selection style.
    gl:enable(?GL_POLYGON_STIPPLE),
    draw_face_sel(Open, SelDlist),
    gl:disable(?GL_POLYGON_STIPPLE),
    gl:disable(?GL_POLYGON_OFFSET_FILL).

draw_face_sel(true, SelDlist) ->
    case wings_pref:get_value(show_backfaces) of
        true ->
            gl:disable(?GL_CULL_FACE),
            wings_dl:call(SelDlist),
            gl:enable(?GL_CULL_FACE);
        _ ->
            wings_dl:call(SelDlist)
    end;
draw_face_sel(false, SelDlist) ->
    wings_dl:call(SelDlist).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_vertices(#dlo{src_we=#we{perm=P},vs=VsDlist}, vertex) when ?IS_SELECTABLE(P) ->
    {R,G,B} = wings_pref:get_value(vertex_color),
	Size = wings_pref:get_value(vertex_size),
    gl:pointSize(Size),
    gl:color3f(R,G,B),
    wings_dl:call(VsDlist);
draw_vertices(_, _) -> ok.

draw_hilite(#dlo{hilite=none}) -> ok;
draw_hilite(#dlo{hilite={_Mode,DL}}) ->
    wings_dl:call(DL).

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
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    polygonOffset(1.0),
    wings_dl:call(DlistSel),
    gl:disable(?GL_POLYGON_OFFSET_FILL).

draw_hard_edges(#dlo{hard=none}, _) -> ok;
draw_hard_edges(#dlo{hard=Hard}, SelMode) ->
    gl:lineWidth(hard_edge_width(SelMode)),
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    wings_dl:call(Hard).

draw_normals(#dlo{normals=none}) -> ok;
draw_normals(#dlo{normals=Ns}) ->
    gl:color3f(0.0, 0.0, 1.0),
    gl:lineWidth(wings_pref:get_value(normal_vector_width)),
    wings_dl:call(Ns).

edge_width(edge) -> wings_pref:get_value(edge_width);
edge_width(_) -> 1.

hard_edge_width(edge) -> wings_pref:get_value(hard_edge_width);
hard_edge_width(_) -> max(wings_pref:get_value(hard_edge_width) - 1, 1).

ground_and_axes(PM,MM) ->
    Axes = wings_wm:get_prop(show_axes),
    GridSize = groundplane(Axes, PM, MM),
    Yon = case wings_pref:get_value(constrain_axes) of
	      true  -> GridSize;
	      false -> (wings_view:current())#view.yon
	  end,
    Key = case Axes of
	      true ->
		  axis_data([{1,x_color,neg_x_color},
			     {2,y_color,neg_y_color},
			     {3,z_color,neg_z_color}],
			    Yon);
	      false ->
		  none
	  end,
    Update = fun(Data) ->
		     D = fun() ->
				 gl:drawArrays(?GL_LINES, 0, 3*4)
			 end,
		     wings_vbo:new(D, Data, [color,vertex])
	     end,
    wings_dl:draw(axes, Key, Update),
    Yon.

get_pref(Key) ->
    wings_pref:get_value(Key).

axis_data([{I,PosKey,NegKey}|T], Yon) ->
    Pos = get_pref(PosKey),
    Neg = get_pref(NegKey),
    A0 = {0.0,0.0,0.0},
    A = setelement(I, A0, Yon),
    B = setelement(I, A0, -Yon),
    [Pos,A0,Pos,A,Neg,A0,Neg,B|axis_data(T, Yon)];
axis_data([], _) -> [].

axis_letters(TPM, TMM, Yon0) ->
    case wings_wm:get_prop(show_axes) of
	false ->
	    ok;
	true ->
	    PM = e3d_transform:matrix(TPM),
	    MM = e3d_transform:matrix(TMM),
	    ViewPort = wings_wm:viewport(),
	    Start = {0.0,0.0,0.0},
	    Origin = proj(Start, MM, PM),
	    Info = {Start,Origin,MM,PM,ViewPort},

	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    {_,_,W,H} = ViewPort,
	    glu:ortho2D(0.0, W, H, 0.0),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    Yon = Yon0 + ?GROUND_GRID_SIZE,
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
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

show_camera_image_plane() ->
    case wings_wm:get_prop(show_cam_imageplane) of
	false ->
	    ok;
	true ->
	    {WinW,WinH} = wings_wm:win_size(),
	    CamW = wings_pref:get_value(negative_width),
	    CamH = wings_pref:get_value(negative_height),
	    AspRatio = CamW/CamH,
	    %% it's used only the height as reference because view horizon changes only for height variation
	    {W,H} = {round(WinH*AspRatio)/2, WinH/2},

	    X1 = (WinW/2.0)-W+5.0,
	    Y1 = (WinH/2.0)-H+5.0,
	    X2 = (WinW/2.0)+W-5.0,
	    Y2 = (WinH/2.0)+H-5.0,

	    Quads = [{X1,Y1,0.0},{X2,Y1,0.0},  			% top
		     {WinW*1.0,0.0,0.0},{0.0,0.0,0.0},
		     {X1,Y1,0.0},{0.0,0.0,0.0},			% left
		     {0.0,WinH*1.0,0.0},{X1,Y2,0.0},
		     {X1,Y2,0.0},{0.0,WinH*1.0,0.0},		% bottom
		     {WinW*1.0,WinH*1.0,0.0},{X2*1.0,Y2,0.0},
		     {X2*1.0,Y2,0.0},{WinW*1.0,WinH*1.0,0.0},	% right
		     {WinW*1.0,0.0,0.0},{X2,Y1,0.0}],
	    Poly = << <<X:?F32,Y:?F32,Z:?F32>> || {X,Y,Z} <- Quads >>,

	    Update = fun draw_camera_image_plane/1,
	    wings_dl:draw(draw_cam_imageplane, {Poly,{X1,Y1,X2,Y2}}, Update)
    end.

draw_camera_image_plane({Poly,{X1,Y1,X2,Y2}}) ->
    NoVs = byte_size(Poly) div 12,
    Draw =
    fun() ->
	wings_io:ortho_setup(),
	gl:enable(?GL_BLEND),
	gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	gl:color4f(0.0, 0.4, 0.8, 0.5),
	gl:drawArrays(?GL_QUADS, 0, NoVs),
	gl:color3f(0.0, 0.4, 0.8),
	gl:lineWidth(2.0),
	gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	gl:rectf(X2, Y1, X1, Y2),
	gl:flush()
    end,
    wings_vbo:new(Draw, Poly).

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
    Y = Vh - trunc((0.5*Y0/W+0.5)*(Vh-16) + 7),
    axis_text(X, Y, Char).

axis_text(X, Y, C) ->
    wings_io:unclipped_text(X, Y, [C]).

proj({X0,Y0,Z0}, MM, PM) ->
    e3d_mat:mul(PM, e3d_mat:mul(MM, {X0,Y0,Z0,1.0})).

line(Ox, Oy, Px, Py) -> {{Ox,Oy},{Px-Ox,Py-Oy}}.

pdot({X1,Y1}, {X2,Y2}) when is_float(X1), is_float(Y1) ->
    Y1*X2-X1*Y2.

sub({X1,Y1}, {X2,Y2}) ->
    {X1-X2,Y1-Y2}.

add_prod({X1,Y1}, {X2,Y2}, S) when is_float(S) ->
    {S*X2+X1,S*Y2+Y1}.

calc_grid_size(PM,MM) ->
    Viewport = {_,_,W,H} =  wings_wm:viewport(),
    W1=max(W,H)/2.0,
    {S,T,U} = wings_gl:unProject(W1, 0.0, 0.0,
				 e3d_transform:matrix(MM),
				 e3d_transform:matrix(PM),
				 Viewport),
    ?GROUND_GRID_SIZE*max(round(max(max(abs(S),abs(T)),abs(U))),10.0).

groundplane(Axes, PM, MM) ->
    GridSize = calc_grid_size(PM, MM),
    Show = wings_wm:get_prop(show_groundplane) orelse
	  (wings_pref:get_value(force_show_along_grid) andalso
	   (wings_view:current())#view.along_axis =/= none),
    Key = case Show of
	      true ->
		  #view{along_axis=Along} = wings_view:current(),
		  Color = wings_pref:get_value(grid_color),
		  {Along,GridSize,Axes,Color};
	      false ->
		  none
	  end,
    wings_dl:draw(groundplane, Key, fun update_groundplane/1),
    float(GridSize).

update_groundplane({Along,Sz,Axes,Color}) ->
    Data = groundplane_2(-Sz, Sz, Sz, Axes),
    N = length(Data),
    Draw = fun() ->
		   gl:color3fv(Color),
		   gl:lineWidth(1.0),
		   gl:matrixMode(?GL_MODELVIEW),
		   gl:pushMatrix(),
		   case Along of
		       x -> gl:rotatef(90.0, 0.0, 1.0, 0.0);
		       z -> ok;
		       _ -> gl:rotatef(90.0, 1.0, 0.0, 0.0)
		   end,
		   gl:drawArrays(?GL_LINES, 0, N),
		   gl:popMatrix(),
		   ?CHECK_ERROR()
	   end,
    wings_vbo:new(Draw, Data).

groundplane_2(X, Last, _Sz, _Axes) when X > Last ->
    [];
groundplane_2(X, Last, Sz, true) when X == 0->
    %% Skip ground plane where the axes go.
    groundplane_2(?GROUND_GRID_SIZE, Last, Sz, true);
groundplane_2(X, Last, Sz, Axes) ->
    NegSz = -Sz,
    [{X,NegSz,0},{X,Sz,0},{NegSz,X,0},{Sz,X,0}|
     groundplane_2(X+?GROUND_GRID_SIZE, Last, Sz, Axes)].

show_saved_bb(St) ->
    Key = get_saved_bb_key(St),
    Update = fun update_saved_bb/1,
    wings_dl:draw(saved_bb, Key, Update).

get_saved_bb_key(#st{bb=BB}) ->
    case {wings_pref:get_value(show_bb),BB} of
	{true,[A,B]} ->
	    Color = wings_pref:get_value(active_vector_color),
	    {A,B,Color};
	{_,_} ->
	    none
    end.

update_saved_bb({{X1,Y1,Z1},{X2,Y2,Z2},Color}) ->
    %% 10 vertices in a line strip.
    Data = [{X1,Y1,Z1},
	    {X2,Y1,Z1},
	    {X2,Y2,Z1},
	    {X1,Y2,Z1},
	    {X1,Y1,Z1},
	    {X1,Y1,Z2},
	    {X2,Y1,Z2},
	    {X2,Y2,Z2},
	    {X1,Y2,Z2},
	    {X1,Y1,Z2},
	    %% 6 vertices / 3 lines
	    {X1,Y2,Z1},
	    {X1,Y2,Z2},
	    {X2,Y2,Z1},
	    {X2,Y2,Z2},
	    {X2,Y1,Z1},
	    {X2,Y1,Z2}],
    D = fun() ->
		gl:enable(?GL_LINE_STIPPLE),
		gl:lineStipple(4, 2#1110111011101110),
		gl:color3fv(Color),
		gl:drawArrays(?GL_LINE_STRIP, 0, 10),
		gl:drawArrays(?GL_LINES, 10, 6),
		gl:disable(?GL_LINE_STIPPLE)
	end,
    wings_vbo:new(D, Data).

show_bb_center(St) ->
    Key = get_bb_center_key(St),
    Update = fun update_bb_center/1,
    wings_dl:draw(saved_bb_center, Key, Update).

get_bb_center_key(#st{bb=BB}) ->
    case {wings_pref:get_value(show_bb_center),BB} of
	{true,[_,_]} ->
	    Center = e3d_vec:average(BB),
	    Color = wings_pref:get_value(active_vector_color),
	    {Center,Color};
	{_,_} ->
	    none
    end.

update_bb_center({{Cx,Cy,Cz}=Center,Color}) ->
    Data = [Center,
	    {Cx,Cy+0.2,Cz},
	    {Cx,Cy-0.2,Cz},
	    {Cx+0.2,Cy,Cz},
	    {Cx-0.2,Cy,Cz},
	    {Cx,Cy,Cz+0.2},
	    {Cx,Cy,Cz-0.2}],
    D = fun() ->
		gl:color3fv(Color),
		gl:pointSize(8.0),
		gl:drawArrays(?GL_POINTS, 0, 1),
		gl:drawArrays(?GL_LINES, 1, 6)
	end,
    wings_vbo:new(D, Data).

mini_axis_icon(MM) ->
    case mini_axis_icon_key() of
	none -> ok;
	Key -> draw_mini_axis_icon(Key, MM)
    end.

draw_mini_axis_icon(Key, MM) ->
    {W,H} = wings_wm:win_size(),
    Ratio = W/H,
    Matrix0 = e3d_transform:matrix(MM),
    Matrix1 = setelement(15, Matrix0, 0.0),
    Matrix2 = setelement(14, Matrix1, -1.0+0.11),
    Matrix  = setelement(13, Matrix2, 0.11-Ratio),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:ortho(-Ratio, Ratio, -1.0, 1.0, 0.00001, 10000000.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadMatrixd(Matrix),

    Update = fun update_mini_axis_icon/1,
    wings_dl:draw(mini_axis_icon, Key, Update),

    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:popAttrib().

mini_axis_icon_key() ->
    case wings_pref:get_value(mini_axis) of
	false ->
	    none;
	true ->
	    #view{along_axis=Along} = wings_view:current(),
	    X = wings_pref:get_value(x_color),
	    Y = wings_pref:get_value(y_color),
	    Z = wings_pref:get_value(z_color),
	    {Along,{X,Y,Z}}
    end.

update_mini_axis_icon({Along,{X,Y,Z}}) ->
    Arrows = mini_axis_arrows(Along, X, Y, Z),
    Data = [X,{0.0,0.0,0.0},			%X Axis
	    X,{0.1,0.0,0.0},
	    Y,{0.0,0.0,0.0},			%Y Axis
	    Y,{0.0,0.1,0.0},
	    Z,{0.0,0.0,0.0},			%Z Axis
	    Z,{0.0,0.0,0.1}|Arrows],
    N = case Along of
	    none -> 3*2 + 3*4;
	    _ -> 3*2 + 2*4
	end,
    D = fun() ->
		gl:drawArrays(?GL_LINES, 0, N)
	end,
    wings_vbo:new(D, Data, [color,vertex]).

mini_axis_arrows(Along, X, Y, Z) ->
    PA = 0.08,
    PB = 0.01,
    case Along of
	none ->
	    %% X Arrows
	    [X,{PA,0.0,-PB},
	     X,{0.1,0.0,0.0},
	     X,{PA,0.0,PB},
	     X,{0.1,0.0,0.0},
	     %% Y Arrows
	     Y,{-PB,PA,0.0},
	     Y,{0.0,0.1,0.0},
	     Y,{PB,PA,0.0},
	     Y,{0.0,0.1,0.0},
	     %% Z Arrows
	     Z,{-PB,0.0,PA},
	     Z,{0.0,0.0,0.1},
	     Z,{PB,0.0,PA},
	     Z,{0.0,0.0,0.1}];
	x ->
	    %% Y Arrows
	    [Y,{0.0,PA,-PB},
	     Y,{0.0,0.1,0.0},
	     Y,{0.0,PA,PB},
	     Y,{0.0,0.1,0.0},
	     %% Z Arrows
	     Z,{0.0,-PB,PA},
	     Z,{0.0,0.0,0.1},
	     Z,{0.0,PB,PA},
	     Z,{0.0,0.0,0.1}];
	y ->
	    %% X Arrows
	    [X,{PA,0.0,-PB},
	     X,{0.1,0.0,0.0},
	     X,{PA,0.0,PB},
	     X,{0.1,0.0,0.0},
	     %% Z Arrows
	     Z,{-PB,0.0,PA},
	     Z,{0.0,0.0,0.1},
	     Z,{PB,0.0,PA},
	     Z,{0.0,0.0,0.1}];
	z ->
	    %% X Arrows
	    [X,{PA,-PB,0.0},
	     X,{0.1,0.0,0.0},
	     X,{PA,PB,0.0},
	     X,{0.1,0.0,0.0},
	     %% Y Arrows
	     Y,{-PB,PA,0.0},
	     Y,{0.0,0.1,0.0},
	     Y,{PB,PA,0.0},
	     Y,{0.0,0.1,0.0}]
    end.

user_clipping_planes(on) ->
    case wings_wm:get_prop(clip_plane) of
	true ->
	    {_Position,Direction = {X,Y,Z}} = wings_pref:get_value(last_axis),
	    Expand = fun({X1,Y1,Z1}) -> [X1,Y1,Z1,0.0] end,
	    draw_clip_disk(Direction, Expand),
	    gl:clipPlane(?GL_CLIP_PLANE0, {X,Y,Z,0.0}),
	    gl:enable(?GL_CLIP_PLANE0),
	    gl:disable(?GL_CULL_FACE);
	false ->
	    ok
    end;
user_clipping_planes(off) ->
    gl:disable(?GL_CLIP_PLANE0),
    gl:enable(?GL_CULL_FACE).

draw_clip_disk(Direction, Expand) ->
    NZ = e3d_vec:norm(Direction),
    NX = e3d_vec:norm(e3d_vec:cross(NZ,{0.0,0.0,1.0})),
    NY = e3d_vec:cross(NX,NZ),
    Nx = Expand(NX),
    Ny = Expand(NY),
    Nz = Expand(NZ),
    Nw = [0.0,0.0,0.0,1.0],
    M  = list_to_tuple(lists:append([Nx,Ny,Nz,Nw])),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_SILHOUETTE),
    gl:pushMatrix(),
    gl:multMatrixd(M),
    gl:color3fv(wings_pref:get_value(clip_plane_color)),
    glu:disk(Obj, 0.0, wings_pref:get_value(clip_plane_size), 35, 1),
    gl:popMatrix(),
    glu:deleteQuadric(Obj).
