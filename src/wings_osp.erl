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

    send({camera, wpa:camera_info([pos_dir_up])}, Stash),
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
	#vab{data=Bin, sn_data=Ns, face_vs=Vs, face_vc=Vc, face_uv=Uv, mat_map=_MatMap} ->
            send({mesh, #{id=>Id, bin=>Bin, vs=>Vs, ns=>Ns, vc=>Vc, uv=>Uv}}, Stash),
            %% Change Normals
	    %Data = swap_normals(Vs, Ns, <<>>),
	    %MM = lists:reverse(lists:keysort(3, MatMap)),
	    %Mats = fix_matmap(MM, LightId, Mtab, []), %% This should be a tree?
            Stash
    catch _:badmatch ->
	    erlang:error({?MODULE, vab_internal_format_changed})
    end;
%% {byte_size(Bin) div 96, Bin, array:from_list(lists:append(MatList))}.
prepare_mesh(_We, Stash) ->
    ?dbg("Perm = ~p ~p~n", [_We#we.perm, ?IS_VISIBLE(_We#we.perm)]),
    Stash.

swap_normals(<<Vs:12/binary, _:12/binary, Uv:8/binary, NextVs/binary>>, 
	     <<Ns:12/binary, NextNs/binary>>, Acc) ->
    swap_normals(NextVs, NextNs, <<Acc/binary, Vs/binary, Ns/binary, Uv/binary>>);
swap_normals(<<>>,<<>>, Acc) -> Acc.

fix_matmap([_MI={Name, _, _Start, Count}|Mats], false, Mtab, Acc0) ->
    Mat = gb_trees:get(Name, Mtab),
    Acc = append_color(Count div 3, Mat, Acc0),
    fix_matmap(Mats, false, Mtab, Acc);
fix_matmap([_MI={_, _, _Start, Count}|Mats], LightId, Mtab, Acc0) ->
    Acc = append_color(Count div 3, LightId, Acc0),
    fix_matmap(Mats, LightId, Mtab, Acc);
fix_matmap([], _, _, Acc) -> Acc.

append_color(N, Diff, Acc) when N > 0 ->
    append_color(N-1, Diff, [Diff|Acc]);
append_color(_, _, Acc) -> Acc.

start_link() ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid;
        Error ->
            error(Error)
    end.

init([]) ->
    %% process_flag(trap_exit, false),
    add_priv_path(),   %% Must be done before osp:init/1 otherwise we can't load the nif
    Dev = osp:init(),
    no_error = osp:deviceGetLastErrorCode(Dev),
    Temp = osp:newMaterial("obj"),  %% Fixme
    osp:commit(Temp),

    Sz = {800,600},
    {ok, #{dev=>Dev, mat=>Temp, meshes => [], sz => Sz}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({mesh, Request}, #{meshes:=Ms, mat:=Mat} = State) ->
    %% ?dbg("~P~n", [Request,20]),
    Mesh = make_geom(Request, Mat),
    {noreply, State#{meshes:=[Mesh|Ms]}};
handle_cast({camera, [{Pos,Dir,Up}]}, #{sz:={W,H}} = State) ->
    Camera = osp:newCamera(perspective),
    osp:setFloat(Camera, "aspect", W / H),
    osp:setParam(Camera, "position", vec3f, Pos),
    osp:setParam(Camera, "direction", vec3f, Dir),
    osp:setParam(Camera, "up", vec3f, Up),
    osp:commit(Camera),
    {noreply, State#{camera=>Camera}};
handle_cast(render, State0) ->
    ?dbg("Start render: ~p~n",[time()]),
    try State = render_start(State0),
         {noreply, State#{meshes:=[]}}
    catch _:Reason:ST ->
            ?dbg("Crash: ~P ~P~n",[Reason,20, ST,20]),
            {stop, normal}
    end;
handle_cast(What, State) ->
    ?dbg("unexpected msg: ~p~n",[What]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

render_start(#{meshes := Ms, camera := Camera, sz:= {W,H} = Sz, dev := Dev} = State) ->
    no_error = osp:deviceGetLastErrorCode(Dev),
    Group = osp:newGroup(),
    Meshes = [Geom || #{geom:=Geom} <- Ms],
    ?dbg("Meshes: ~P ~n", [Ms, 20]),
    ModelList = osp:newCopiedData(Meshes, geometric_model, length(Meshes)),
    osp:setParam(Group, "geometry", geometric_model, ModelList),
    osp:commit(Group),

    %% put the group into an instance (give the group a world transform)
    Instance = osp:newInstance(Group),
    osp:commit(Instance),

    %% put the instance in the world
    World = osp:newWorld(),
    osp:setParam(World, "instance", instance, osp:newCopiedData([Instance], instance, 1)),

    %% create and setup light for Ambient Occlusion
    Light = osp:newLight("sunSky"),
    osp:commit(Light),
    osp:setParam(World, "light", light, osp:newCopiedData([Light], light, 1)),
    osp:commit(World),

    ?dbg("~p~n", [osp:deviceGetLastErrorCode(Dev)]),
    io:format("World bounds ~p~n",[osp:getBounds(World)]),

    %% create renderer
    Renderer = osp:newRenderer("pathtracer"), %% choose path tracing renderer
    %% complete setup of renderer
    osp:setParam(Renderer, "backgroundColor", vec4f, {1,0,0,1.0}),
    osp:commit(Renderer),

    %% create and setup framebuffer
    Framebuffer = osp:newFrameBuffer(W,H, fb_srgba, [fb_color, fb_accum]),
    osp:resetAccumulation(Framebuffer),

    ?dbg("~p~n", [osp:deviceGetLastErrorCode(Dev)]),
    Render = fun() ->
                     Future = osp:renderFrame(Framebuffer, Renderer, Camera, World),
                     osp:wait(Future)
             end,
    [Render() || _ <- lists:seq(1,10)],

    %% access framebuffer and write its content as PPM file
    FirstImage = osp:readFrameBuffer(Framebuffer, W,H, fb_srgba, fb_color),
    display("Wings N=10", Sz, FirstImage),

    io:format("done\n"),
    State.

make_geom(#{id:=Id, bin:=Data, ns:=Ns, vs:={Stride,VsStart}} = _Input, Mat) ->
    N = byte_size(Data) div Stride,
    %% Asserts
    0 = byte_size(Data) rem Stride,
    0 = VsStart,
    Mesh = osp:newGeometry("mesh"),
    VsD  = osp:newCopiedData(Data, vec3f, N, Stride),
    osp:commit(VsD),
    osp:setObject(Mesh, "vertex.position", VsD),
    ND  = osp:newCopiedData(Ns, vec3f, N),
    osp:commit(ND),
    osp:setObject(Mesh, "vertex.normal", ND),
    Index = lists:seq(0, N-1),
    IndexD  = osp:newCopiedData(Index, vec3ui, N div 3),  %% Triangles
    osp:setObject(Mesh, "index", IndexD),
    osp:commit(Mesh),
    Geom = osp:newGeometricModel(Mesh),
    osp:setObject(Geom, "material", Mat),
    osp:commit(Geom),
    #{id=>Id, geom=>Geom}. %% , orig=>Input}.

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
