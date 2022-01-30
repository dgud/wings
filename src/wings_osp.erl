%%--------------------------------------------------------------------
%%
%%  wings_osp.erl --
%%
%%     Ray-tracing renderer with ospray
%%
%%  Copyright (c) 2015 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(wings_osp).
-behaviour(gen_server).

-compile([export_all, nowarn_export_all]).
-include("wings.hrl").

%% API
-export([render/1, render/2, menu/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

menu() ->
    {"OSPray...", wings_osp}.

-spec render(#st{}) -> 'keep'.
render(St) ->
    %% Should display options and recurse but for now
    render(St, []).

-spec render(#st{}, [any()]) -> 'keep'.
render(#st{shapes=Sh, mat=Materials} = St, Opts) ->
    %% Should work on objects but wings_draw_setup:we/3 needs we{} for now
    Pid = start_link(),
    Wes = gb_trees:values(Sh),
    Subdiv = proplists:get_value(subdivisions, Opts, 0),
    Stash = #{opts=>Opts, subdiv=>Subdiv, materials=>Materials, st=>St, pid=>Pid},
    
    ok = gen_server:call(Pid, cancel_prev_render),
    send({camera, wpa:camera_info([pos_dir_up])}, Stash),
    send({materials, Materials}, Stash),
    lists:foldl(fun prepare_mesh/2, Stash, Wes),
    send(render, Stash),
    keep.

prepare_mesh(We, Stash) when ?IS_VISIBLE(We#we.perm), ?IS_ANY_LIGHT(We) ->
    %% case IsLight of
    %%     true when not ?IS_POINT_LIGHT(We) ->
    %%         %% Don't subdiv area, ambient and infinite lights
    %%         {0, pbr_light:lookup_id(Name,Ls)};
    %%     true -> %% Subdiv the point lights to be round
    %%         {3, pbr_light:lookup_id(Name,Ls)};
    %%         false ->
    %%         {proplists:get_value(subdivisions, Opts, 1),false}
    %% end,
    Stash;
prepare_mesh(#we{id=Id}=We, #{st:=St, subdiv:=SubDiv} = Stash) when ?IS_VISIBLE(We#we.perm) ->
    Options = [{smooth, true}, {subdiv, SubDiv}],
    Vab = wings_draw_setup:we(We, Options, St),
    %% I know something about this implementation :-)
    try Vab of
	#vab{data=Bin, sn_data=Ns, face_vs=Vs, face_vc=Vc, face_uv=Uv, mat_map=MatMap} ->
            send({mesh, #{id=>Id, bin=>Bin, vs=>Vs, ns=>Ns, vc=>Vc, uv=>Uv, mm=>MatMap}}, Stash),
            Stash
    catch _:badmatch ->
	    erlang:error({?MODULE, vab_internal_format_changed})
    end;
%% {byte_size(Bin) div 96, Bin, array:from_list(lists:append(MatList))}.
prepare_mesh(_We, Stash) ->
    %% ?dbg("Perm = ~p ~p~n", [_We#we.perm, ?IS_VISIBLE(_We#we.perm)]),
    Stash.

start_link() ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid;
        Error -> error(Error)
    end.

init([]) ->
    %% process_flag(trap_exit, false),
    add_priv_path(),   %% Must be done before osp:init/1 otherwise we can't load the nif
    Dev = osp:init(),
    no_error = osp:deviceGetLastErrorCode(Dev),
    Sz = {800,600},
    {ok, reset(#{dev=>Dev, sz=>Sz})}.

handle_call(cancel_prev_render, _From, State) ->
    case maps:get(render, State, false) of
        false ->
            {reply, ok, State};
        #{future:=Future} ->
            osp:cancel(Future),
            {reply, ok, reset(State)}
    end.

handle_cast({mesh, Request}, #{meshes:=Ms, materials:=Mats0} = State) ->
    %% ?dbg("~P~n", [Request,20]),
    {Mesh, Mats} = make_geom(Request, Mats0),
    no_error = osp:deviceGetLastErrorCode(maps:get(dev, State)),
    {noreply, State#{meshes:=[Mesh|Ms], materials:=Mats}};
handle_cast({camera, [{Pos,Dir,Up}]}, #{sz:={W,H}} = State) ->
    Camera = osp:newCamera(perspective),
    osp:setFloat(Camera, "aspect", W / H),
    osp:setParam(Camera, "position", vec3f, Pos),
    osp:setParam(Camera, "direction", vec3f, Dir),
    osp:setParam(Camera, "up", vec3f, Up),
    osp:commit(Camera),
    {noreply, State#{camera=>Camera}};
handle_cast({materials, Materials0}, State) ->
    Materials = prepare_materials(gb_trees:to_list(Materials0)),
    {noreply, State#{materials => Materials}};
handle_cast(render, State0) ->
    ?dbg("Start render: ~p~n",[time()]),
    try {noreply, render_start(State0)}
    catch _:Reason:ST ->
            ?dbg("Crash: ~P ~P~n",[Reason,20, ST,20]),
            {stop, normal}
    end;
handle_cast(What, State) ->
    ?dbg("unexpected msg: ~p~n",[What]),
    {noreply, State}.

handle_info({osp, Future, task_finished}, #{render:=#{future:=Future}} = State0) ->
    State = render_done(State0),
    {noreply, State};
handle_info(_Info, State) ->
    ?dbg("unexpected info: ~p ~P~n",[_Info, State, 20]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reset(State) ->
    maps:remove(render, State#{meshes=>[], materials=>[]}).

render_start(#{meshes := []} = State) ->
    State;
render_start(#{meshes := Ms, camera := Camera, sz:= {W,H}, dev := Dev} = State) ->
    no_error = osp:deviceGetLastErrorCode(Dev),
    Meshes = [Geom || #{geom:=Geom} <- Ms],
    ?dbg("Meshes: ~w ~n", [length(Ms)]),
    Group = osp:newGroup(),
    ModelList = osp:newCopiedData(Meshes, geometric_model, length(Meshes)),
    osp:setParam(Group, "geometry", geometric_model, ModelList),
    osp:commit(Group),
    Instance = osp:newInstance(Group),
    osp:commit(Instance),
    World = osp:newWorld(),
    osp:setParam(World, "instance", instance, osp:newCopiedData([Instance], instance, 1)),

    Light = osp:newLight("sunSky"),
    osp:commit(Light),
    osp:setParam(World, "light", light, osp:newCopiedData([Light], light, 1)),
    osp:commit(World),

    ?dbg("~p~n", [osp:deviceGetLastErrorCode(Dev)]),
    Renderer = osp:newRenderer("pathtracer"), %% choose path tracing renderer
    osp:setParam(Renderer, "backgroundColor", vec4f, {1,1,1,1.0}),
    MaterialsData = setup_materials(maps:get(materials, State)),
    osp:setObject(Renderer, "material", MaterialsData),
    osp:commit(Renderer),

    Framebuffer = osp:newFrameBuffer(W,H, fb_srgba, [fb_color, fb_accum]),
    osp:resetAccumulation(Framebuffer),
    ?dbg("~p~n", [osp:deviceGetLastErrorCode(Dev)]),
    Future = osp:renderFrame(Framebuffer, Renderer, Camera, World),
    RenderOpts = #{da=>5, done=>5, future=>Future, fb=>Framebuffer, render=>Renderer, cam=>Camera, world=>World},
    osp:subscribe(Future),
    State#{render => RenderOpts}.

render_done(#{render:=#{da:=Da}=Render} = State) when Da > 0 ->
    #{fb:=Framebuffer, render:=Renderer, cam:=Camera, world:=World} = Render,
    Future = osp:renderFrame(Framebuffer, Renderer, Camera, World),
    osp:subscribe(Future),
    State#{render:=Render#{da:=Da-1, future:=Future}};
render_done(#{render:=#{done:=Done}=Render, sz:={W,H}=Sz} = State) when Done > 0 ->
    #{fb:=Framebuffer, render:=Renderer, cam:=Camera, world:=World} = Render,
    %% access framebuffer and display content
    Image = osp:readFrameBuffer(Framebuffer, W,H, fb_srgba, fb_color),
    display("Wings temp", Sz, Image),
    Future = osp:renderFrame(Framebuffer, Renderer, Camera, World),
    osp:subscribe(Future),
    State#{render:=Render#{da:=10, done:=Done-1, future:=Future}};
render_done(#{render:=Render, sz:={W,H}=Sz} = State) ->
    #{fb:=Framebuffer} = Render,
    %% access framebuffer and display content
    Image = osp:readFrameBuffer(Framebuffer, W,H, fb_srgba, fb_color),
    display("Wings Last", Sz, Image),
    reset(State).

make_geom(#{id:=Id, bin:=Data, ns:=NsBin, vs:=Vs, vc:=Vc, uv:=Uv, mm:=MM} = _Input, Mats0) ->
    {Stride, 0} = Vs,
    %% Asserts
    N = byte_size(Data) div Stride,
    0 = byte_size(Data) rem Stride,
    %% ?dbg("id:~p ~p ~p ~p~n",[Id, Vs, Vc, Uv]),
    Mesh = osp:newGeometry("mesh"),
    {MatIds, UseVc, Mats} = make_mesh_materials(MM, Mats0),

    add_data(Mesh, Data, "vertex.position", vec3f, N, Vs),
    add_data(Mesh, Data, "vertex.texcoord", vec3f, N, Uv),
    add_vc_data(Mesh, Data, N, Vc, UseVc),
    add_data(Mesh, NsBin, "vertex.normal", vec3f, N),
    Index = lists:seq(0, N-1),
    IndexD  = osp:newCopiedData(Index, vec3ui, N div 3),  %% Triangles
    osp:setObject(Mesh, "index", IndexD),
    osp:commit(Mesh),
    Geom = osp:newGeometricModel(Mesh),
    add_mat(Geom, MatIds),
    osp:commit(Geom),
    {#{id=>Id, geom=>Geom}, Mats}.

%% Material handling

prepare_materials(Materials0) ->
    Mats = [prepare_mat(Mat) || Mat <- Materials0],
    Maps = maps:from_list(Mats),
    Maps#{'_next_id'=>0}.

prepare_mat({Name, Attrs0}) ->
    Maps = proplists:get_value(maps, Attrs0),
    Attrs1 = proplists:get_value(opengl, Attrs0),
    {value, {_, VC}, Attrs} = lists:keytake(vertex_colors, 1, Attrs1),
    {Name, maps:from_list([{maps,Maps}, {use_vc, VC /= ignore}|Attrs])}.

make_mesh_materials([{Name,_,_,_}], Mats0) ->
    %% Single material per mesh
    get_mat_id(Name, Mats0);
make_mesh_materials(MMS, Mats0) ->
    Sorted = lists:keysort(3, MMS),
    make_mesh_materials(Sorted, false, Mats0, 0, []).

make_mesh_materials([{Name,_,Start,N}|MMs],UseVc0,Mats0, Start, Acc) ->
    {Id, UseVc, Mats} = get_mat_id(Name,Mats0),
    MatIndex = make_mat_index(Id, N),
    make_mesh_materials(MMs, UseVc orelse UseVc0, Mats, Start+N, [MatIndex|Acc]);
make_mesh_materials([], UseVc, Mats, N, Acc) ->
    Index = iolist_to_binary(lists:reverse(Acc)),
    Data = osp:newCopiedData(Index, uint, N),
    {Data, UseVc, Mats}.

make_mat_index(Id, N) ->
    iolist_to_binary(lists:duplicate(N, <<Id:32/unsigned-native>>)).

get_mat_id(Name, #{'_next_id':= Next}=Mats0) ->
    #{use_vc:=UseVC} = Mat0 = maps:get(Name, Mats0),
    case maps:get(id, Mat0, undefined) of
        undefined ->
            Mat = Mat0#{id=>Next},
            {Next, UseVC, Mats0#{'_next_id':=Next+1, Name=>Mat}};
        Id ->
            {Id, UseVC, Mats0}
    end.

setup_materials(MatMap) ->
    Mats0 = maps:values(MatMap),
    MatRefs = lists:foldl(fun create_mat/2, [], Mats0),
    MatRefList = [Mat || {_, Mat} <- lists:keysort(1, MatRefs)],
    osp:newCopiedData(MatRefList, material, length(MatRefList)).

create_mat(#{id:=Id, diffuse:=Base, roughness:=Roughness, metallic:=Metallic, emission:=E}, Acc) ->
    Mat = osp:newMaterial("principled"),
    {R,G,B,A} = Base,
    {0.0, 0.0, 0.0, _} = E,
    osp:setParam(Mat, "baseColor", vec3f, {R,G,B}),
    osp:setParam(Mat, "metallic",  float, Metallic),
    osp:setParam(Mat, "roughness", float, Roughness),
    osp:setParam(Mat, "opacity",   float, A),
    osp:commit(Mat),
    [{Id, Mat}|Acc];
create_mat(_, Acc) ->
    Acc.

add_data(Obj, Data, Id, Type, N) ->
    add_data(Obj, Data, Id, Type, N, {0, 0}).
add_data(_Obj, _Data0, _Id, _Type, _N, none) ->
    ok;
add_data(Obj, Data0, Id, Type, N, {Stride, Start}) ->
    Data = case Start of
               0 -> Data0;
               _ ->
                   <<_:Start/binary, Data1/binary>> = Data0,
                   Data1
           end,
    Copied  = osp:newCopiedData(Data, Type, N, Stride),
    osp:commit(Copied),
    osp:setObject(Obj, Id, Copied).

add_mat(Geom, Id) when is_integer(Id) ->
    osp:setParam(Geom,  "material", uint, Id);
add_mat(Geom, IdsData) when is_reference(IdsData) ->
    osp:setObject(Geom, "material", IdsData).

add_vc_data(_Mesh, _Data, _N, none, _) ->
    ok;
add_vc_data(_Mesh, _Data, _N, _, false) ->
    ok;
add_vc_data(Mesh, Data, N, Vc, true) ->
    VcBin = rgb_to_rgba(Data, Vc),
    N = byte_size(VcBin) div 16,
    0 = byte_size(VcBin) rem 16,
    add_data(Mesh, VcBin, "vertex.color", vec4f, N).

rgb_to_rgba(Data0, {Stride, Start}) ->
    <<_:Start/binary, Data/binary>> = Data0,
    Skip = Stride - 12,
    rgb_to_rgba(Data,Skip, <<>>).

rgb_to_rgba(Data, SkipBytes, Acc) ->
    case Data of
        <<Cols:12/binary, _:SkipBytes/binary, Rest/binary>> ->
            rgb_to_rgba(Rest, SkipBytes, <<Acc/binary, Cols:12/binary, 1.0:32/float-native>>);
        <<Cols:12/binary, _/binary>> ->
            <<Acc/binary, Cols:12/binary, 1.0:32/float-native>>;
        <<>> ->
            Acc
    end.
%%

send(Cmd, #{pid := Pid}) ->
    gen_server:cast(Pid, Cmd).

add_priv_path() ->
    case os:type() of
        {win32, _} ->
            P0 = os_env:get("PATH"),
            OSPPath = filename:nativename(code:priv_dir(ospraye)),
            case string:find(P0, OSPPath) of
                nomatch ->
                    P1 = P0 ++ ";" ++ OSPPath,
                    io:format("Adding osp path: ~s~n", [P1]),
                    os_env:set("PATH", P1);
                _Where ->
                    io:format("osp path found: ~s~n", [_Where]),
                    ok
            end;
        _ ->
            P0 = os_env:get("LD_LIBRARY_PATH"),
            OSPPath = code:priv_dir(ospraye),
            case string:find(P0, OSPPath) of
                nomatch ->
                    P1 = P0 ++ ":" ++ OSPPath,
                    io:format("Adding osp path: ~s~n", [P1]),
                    os_env:set("PATH", P1);
                _ ->
                    ok
            end
    end.

display(Title, {IW,IH}=_Size, RGBABin0) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, [{size, {IW+50, IH+50}}]),
    Panel = wxPanel:new(Frame),
    wxWindow:setBackgroundColour(Panel, {200, 180, 180}),
    Szr = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addStretchSpacer(Szr),
    %% Sigh wxWidgets splits rgb and alpha
    RowSz = IW*4,
    RGBABin = flip_image(RGBABin0, RowSz, []),
    RGB = << <<RGB:24>> || <<RGB:24,_:8>> <= RGBABin >>,
    Alpha = << <<A:8>> || <<_:24, A:8>> <= RGBABin >>,
    Image = wxImage:new(IW,IH, RGB, Alpha),
    BMImage = wxBitmap:new(Image),
    SBM = wxStaticBitmap:new(Panel, ?wxID_ANY, BMImage),
    wxBitmap:destroy(BMImage),
    wxImage:destroy(Image),
    wxSizer:add(Szr, SBM, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(Szr),
    wxPanel:setSizer(Panel, Szr),
    wxFrame:show(Frame),
    #{frame => Frame, panel => Panel}.

flip_image(<<>>, _, Acc) ->
    iolist_to_binary(Acc);
flip_image(Bin, Size, Acc) ->
    <<Row:Size/binary, Rest/binary>> = Bin,
    flip_image(Rest, Size, [Row|Acc]).
