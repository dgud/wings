%%
%%  wpc_ambocc.erl --
%%
%%     Plug-in for Generating Ambient Occlusion via OpenGL
%%
%%     Uses hardware-accelerated OpenGL calls, instead of ray-tracing, to
%%     calculate and set a per-vertex ambient-occlusion factor. A HemiCube is
%%     used to sample the environment's visibility. The results are stored in
%%     the vertex-colors and can also be baked to a texture through autouv.
%%
%%  CopyRight (c) 2008-2009 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_ambocc).

-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").

init() ->
    true.

menu({tools}, Menu) ->
    Menu ++ [separator,{"Ambient Occlusion",ambient_occlusion,
			"Add Ambient-Occlusion vertex colors via OpenGL"}];
menu(_, Menu) -> Menu.

command({tools,ambient_occlusion}, St) ->
    ambient_occlusion(St);
command(_Cmd, _) -> next.

ambient_occlusion(St) ->
    StartTime = now(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_gl(),
    DispList = make_disp_list(St),
    #st{shapes=Shapes} = St,
    ProcessObject = fun(_,We) -> process_obj(We,DispList) end,
    Shapes2 = ?SLOW(gb_trees:map(ProcessObject, Shapes)),
    St2 = St#st{shapes=Shapes2},
    gl:deleteLists(DispList,1),
    gl:popAttrib(),
    EndTime = now(),
    Seconds = timer:now_diff(EndTime,StartTime)/1.0e6,
    VidCard = gl:getString(?GL_RENDERER),
    io:fwrite("OpenGL AmbOcc time: ~.1fs (~s)\n", [Seconds,VidCard]),
    St3 = create_ambient_light(St2),
    St3.

process_obj(We, _) when ?IS_NOT_VISIBLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_NOT_SELECTABLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_ANY_LIGHT(We) ->
    case We#we.name =/= "Ambient" of
	true -> We#we{perm=[]};
	false -> We
    end;
process_obj(We0, DispList) ->
    #we{es=Etab,vp=Vtab,name=Name} = We0,
    io:fwrite("Processing: ~s\n", [Name]),
    GetColor = fun(Key,_Val) ->
	Eye = wings_vertex:pos(Key,We0) ,
	LookAt = wings_vertex:normal(Key,We0),
	get_ao_color(Eye,LookAt,DispList)
    end,
    VertexColors = array:sparse_map(GetColor, Vtab),
    SetColor = fun(Edge, #edge{vs=Va,ve=Vb}, W) ->
	Color1 = array:get(Va, VertexColors),
	Color2 = array:get(Vb, VertexColors),
        wings_va:set_edge_color(Edge, Color1, Color2, W)
    end,
    array:sparse_foldl(SetColor, We0, Etab).

make_disp_list(St) ->
    #st{shapes=Shapes} = St,
    GetAllFaces = fun(_Key,Val) ->
	Perm = Val#we.perm,
	case ?IS_ANY_LIGHT(Val) or ?IS_NOT_VISIBLE(Perm) or ?IS_NOT_SELECTABLE(Perm) of
	    true ->
		[];
	    false  ->
		Fs = gb_trees:to_list(Val#we.fs),
		[wings_face:vertex_positions(Face, Val) || {Face,_} <- Fs]
	end
    end,
    AddPolygons = fun(RawFs2) ->
	ProcessVert = fun(Vert) ->
	    {X,Y,Z} = Vert,
	    gl:vertex3f(X,Y,Z)
	end,
	ProcessFace = fun(Face) ->
	    gl:'begin'(?GL_POLYGON),
	    lists:foreach(ProcessVert, Face),
	    gl:'end'()
	end,
	lists:foreach(ProcessFace, RawFs2)
    end,
    RawFs = gb_trees:map(GetAllFaces, Shapes),
    AllRawFs = lists:append(gb_trees:values(RawFs)),
    DispList = gl:genLists(1),
    gl:newList(DispList, ?GL_COMPILE),
    AddPolygons(AllRawFs),
    gl:endList(),
    DispList.

get_ao_factor(Buffer) ->
    Data = binary_to_list(Buffer),
    NumWhitePixels = length([Val || Val <- Data, Val==255]),
    Samples = 0.75*length(Data),
    Misses = -0.25*length(Data) + NumWhitePixels,
    Factor = Misses/Samples,
    Factor.

read_frame() ->
    Hemirez = 64, % Must be even and/or power-of-two
    W = H = Hemirez,
    Buffer = wings_io:get_buffer(W*H, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0,0, W,H, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    get_ao_factor(ImageBin).

setup_gl() ->
    gl:clearColor(1,1,1,0),  % Sky Color
    gl:color4f(0,0,0,1),     % Obj Color
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING).

get_ao_color(Eye, Lookat, DispList) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    render_hemicube(Eye, Lookat, DispList),
    Factor = read_frame(),
    {Factor,Factor,Factor}.

get_up_right({0.0,0.0,1.0}) ->
    Up = {0.0,+1.0,0.0},
    Right = {1.0,0.0,0.0},
    {Up,Right};
get_up_right({0.0,0.0,-1.0}) ->
    Up = {0.0,-1.0,0.0},
    Right = {1.0,0.0,0.0},
    {Up,Right};
get_up_right(Lookat) ->
    Up1 = {0.0,0.0,1.0},
    Right = e3d_vec:cross(Up1, Lookat),
    Up = e3d_vec:cross(Lookat, Right),
    {Up,Right}.

render_hemicube(Eye, Lookat, DispList) ->
    {Up,Right} = get_up_right(Lookat),
    Hemirez = 64, % Must be even and/or power-of-two
    P1 = trunc(Hemirez * 0.00),
    P2 = trunc(Hemirez * 0.25),
    P3 = trunc(Hemirez * 0.50),
    P4 = trunc(Hemirez * 0.75),
    W = P3, H = P2,
    EpL = e3d_vec:add(Eye, Lookat),
    EpR = e3d_vec:add(Eye, Right),
    EmR = e3d_vec:sub(Eye, Right),
    EmU = e3d_vec:sub(Eye, Up),
    EpU = e3d_vec:add(Eye, Up),
    LookatN = e3d_vec:neg(Lookat),
    render_view(Eye, EpL, Up,      [-1,1,-1,1], [P2,P2,W,W], DispList), % Center
    render_view(Eye, EpR, Up,      [ 0,1,-1,1], [P1,P2,H,W], DispList), % Right
    render_view(Eye, EmR, Up,      [-1,0,-1,1], [P4,P2,H,W], DispList), % Left
    render_view(Eye, EmU, Lookat,  [-1,1, 0,1], [P2,P1,W,H], DispList), % Down
    render_view(Eye, EpU, LookatN, [-1,1,-1,0], [P2,P4,W,H], DispList). % Up

render_view(Eye, Lookat, Up, Frustum, Viewport, DispList) ->
    Near = 0.01,
    Far = 100.0,
    {Ex,Ey,Ez} = Eye,
    {Dx,Dy,Dz} = Lookat,
    {Ux,Uy,Uz} = Up,
    [L,R,B,T] = [I*Near || I <- Frustum],
    [X,Y,W,H] = Viewport,
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:frustum(L,R, B,T, Near,Far),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    glu:lookAt(Ex,Ey,Ez, Dx,Dy,Dz, Ux,Uy,Uz),
    gl:viewport(X,Y, W,H),
    gl:callList(DispList).

create_ambient_light(St) ->
    wings_pref:set_value(scene_lights, true),
    SceneLights = wings_light:export(St),
    case proplists:is_defined("Ambient", SceneLights) of
	true ->
	    St;
	false ->
	    White = {1.0,1.0,1.0,1.0},
	    Lights = [{"Ambient",[{opengl,[{type,ambient},{ambient,White}]}]}],
	    wings_light:import(Lights,St)
    end.
