%%% @author  <dgud@erlang.org>
%%% @copyright (C) 2009, 
%%% @doc
%%%    An erlang study version of the paper:
%%%    "Parallel View-Dependent Tesselation of Catmull-Clark Subdivision Surfaces"
%%%       by Anjul Patney, Mohamed S. Ebeida, John D. Owens
%%%    www.idav.ucdavis.edu/func/return_pdf?pub_id=964
%%%
%%% The intention is to create an OpenCL version later.
%%% @end
%%% Created : 22 Nov 2009 by  <dgud@erlang.org>

-module(wings_cc).
-export([setup/1, addp_subdiv/1]).

-include("wings.hrl").
-define(EPS, 0.0005).

-record(v, {pos,                     % position
	    n = {0.0,0.0,0.0},       % normal
	    vc}).                    % Valence

%%-record(flags, {ap, a, vs, es, f3, f4}).  Bitfield

%% The different working sets

-record(base, {v,   %% array of #v{}           nv	       
	       f,   %% array of v0,v1,v2,v3    nf
	       e,   %% array of v0,v1,f1,f2    ne
	       sd   %% array of active
	      }).

-record(mesh, {v,   %% array of #v{}           2*nv
	       f,   %% array of v0,v1,v2,v3    2*nf
	       e    %% array of v0,v1,f1,f2    2*ne
	      }).

-record(temp, {f,   %% array of #flags{}       max(nf,nfe,nv)
	       f3,  %% scanned f3 faces        nf
	       f4,  %% scanned f4 faces        nf
	       e    %% Edges scanned           ne
	      }).

%% @spec(#we) -> #base{}
setup(#we{fs=Ftab, holes=Holes, mirror=Mirror} = We) ->
    Exclude = case Mirror of
		  none -> Holes;
		  _ -> ordsets:add_element(Mirror, Holes)
	      end,
    Faces = ordsets:subtract(gb_trees:keys(Ftab), Exclude),
    Acc = {{array:new(), array:new()}, array:new()}, 
    setup(Faces, array:new(), Acc, We).

setup([F|Fs], Ftab0, Acc0, We) when F >= 0 ->
    {Vs, Acc} = wings_face:fold(fun(V,Eid,E,Acc) ->  %% CCW
					setup(V,Eid,E,Acc,We)
				end, {[], Acc0}, F, We),
    Ftab = array:set(F, list_to_tuple(lists:reverse(Vs)), Ftab0),
    setup(Fs, Ftab, Acc, We);
setup([_|Fs], Ftab, Acc, We) ->
    setup(Fs, Ftab, Acc, We);
setup([], Ftab, {{Vtab, VC}, Etab}, _) ->
    tag_potentially_active(VC, #base{v=Vtab, e=Etab, f=Ftab}).

setup(V, Eid, E, {Vs, {Vtab0, Etab0}}, We) ->
    Vtab = setup_vertex(V, Vtab0, We),
    Etab = setup_edge(Eid, E, Etab0),
    {[V|Vs], {Vtab, Etab}}.

setup_vertex(V, Tabs = {Vtab, Vc}, We = #we{vp=Vpos}) ->
    case array:get(V, Vtab) of
	undefined ->
	    Count = fun(E,_,_,[C|Es]) -> [C+1,E|Es] end,
	    [N|Es] = wings_vertex:fold(Count, [0], V, We),
	    VP = #v{pos = array:get(V,Vpos),
		    vc  = N},
	    {array:set(V, VP, Vtab), array:set(V, Es, Vc)};
	_ ->
	    Tabs
    end.

setup_edge(Eid, #edge{vs=V1,ve=V2,lf=F1,rf=F2}, Etab) ->
    case array:get(Eid, Etab) of
	undefined ->
	    array:set(Eid, {V1,V2,F1,F2}, Etab);
	_ ->
	    Etab
    end.

tag_potentially_active(V2E, Base = #base{e=Es}) ->
    PA0 = tag([], 1, Es, V2E),
    PA  = array:from_orddict(array:sparse_to_orddict(PA0), 0),
    Base#base{sd=PA}.

tag(Vs = [_|_], Tag, Es, PA0) ->
    Update = fun(V,Curr=[PA|Next]) ->
		     Edges = array:get(V, PA),
		     if
			 is_integer(Edges) ->
			     %% Already updated
			     Curr;
			 true ->			     
			     [array:set(V,Tag,PA)|other(Edges,V,Es,Next)]
		     end
	     end,
    [PA|Rest] = lists:foldl(Update, [PA0], Vs),
    tag(Rest, Tag bxor 1, Es, PA);
tag([], _Tag, Es, PA) ->
    try
	array:sparse_foldl(fun(Id, VE=[_|_], _) -> throw({restart, Id, VE});
			      (_,_,A) -> A
			   end, PA, PA)
    catch
	{restart,Id,_VE} ->
	    %% Not all vertices are connected
	    tag([Id], 1, Es, PA)
    end.
    
other([E|Es], V, Etab, Acc) ->
    case array:get(E, Etab) of
	{V,Other,_,_} ->
	    other(Es, V, Etab, [Other|Acc]);
	{Other,V,_,_} ->
	    other(Es, V, Etab, [Other|Acc]);
	undefined ->
	    other(Es, V, Etab, Acc)
    end;
other([], _, _, Acc) ->
    Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addp_subdiv(#base{sd=Flags, f=Fs, e=Es, v=Vs}) ->
    try
	addp_subdiv_1(Fs, Es, Vs, Flags, 5)
    catch
	{done,Res} ->
	    io:format("~p ~n", [Res])
    end.

addp_subdiv_1(Fs0, Es0, Vs0, Flags0, Max) when Max > 0 ->
    case test_subd(Flags0, Vs0, Fs0) of
	{false,_} ->
	    throw({done, Max});
	{true, Flags1} ->
	    Flags2 = update_faces(Fs0, Flags1),  %% Parallel
	    Flags3 = update_edges(Es0, Flags2),  %% Parallel
	    %% debug_flags(Flags, ?LINE),
	    {SFs,SEd} = scan_flags(Flags3, 0, 0),
	    {Flags4,_,Vs1,_,Fs1} = 
		gen_face_points(Fs0, Flags3, Vs0, Vs0, SFs),
	    {Flags, _,Vs2,_,_,Fs2,_,Es1} = 
		gen_edge_points(Es0, Flags4, Vs0, Vs1,SFs, Fs0, Fs1, SEd),
	    %%io:format("~p ~n",[array:to_orddict(Vs1)]),
	    io:format("~p ~n",[array:to_orddict(Fs2)])	    
    end,
    ok.

test_subd(Fl, Vtab, Fs) ->
    Test = fun(FId, Vs, Acc) -> test_subd(FId, Vs, Vtab, Acc) end,
    array:sparse_foldl(Test, {false, Fl}, Fs).

test_subd(_, {IdV1,IdV2,IdV3,IdV4}, Vtab, P = {_, Fl}) ->
    #v{pos=V1} = array:get(IdV1,Vtab),
    #v{pos=V2} = array:get(IdV2,Vtab),
    case e3d_vec:dist_sqr(V1,V2) > ?EPS of
	true -> SubdF = true;
	false ->
	    #v{pos=V3} = array:get(IdV3,Vtab),
	    case e3d_vec:dist_sqr(V2,V3) > ?EPS of
		true -> SubdF = true;
		false ->
		    #v{pos=V4} = array:get(IdV4,Vtab),
		    SubdF = e3d_vec:dist_sqr(V3,V4) > ?EPS orelse
			e3d_vec:dist_sqr(V4,V1) > ?EPS
	    end
    end,
    if SubdF ->
	    {true, tag_active_v(
		     IdV4,tag_active_v(
			    IdV3,tag_active_v(
				   IdV2, tag_active_v(IdV1, Fl))))};
       true ->
	    P
    end.

tag_active_v(V, Fl) ->
    Flag = case (array:get(V,Fl) band 2#01) > 0 of
	       true ->  2#111;
	       false -> 2#100
	   end,
    array:set(V, Flag, Fl).
      
update_faces(Fs, Fl) ->
    array:sparse_foldl(fun update_faces/3, Fl, Fs).

update_faces(Face, {V1,V2,V3,V4}, Fl) ->
    Active = (array:get(V1,Fl) band 2#10) + (array:get(V2,Fl) band 2#10) +
	(array:get(V3,Fl) band 2#10) + (array:get(V4,Fl) band 2#10),
    FaceInfo = array:get(Face, Fl),
    case Active of
	4 -> array:set(Face, FaceInfo bor 2#10000000, Fl);
	2 -> array:set(Face, FaceInfo bor 2#01000000, Fl)
    end.

update_edges(Es, Fl) ->
    array:sparse_foldl(fun update_edges/3, Fl, Es).

update_edges(Edge, {V1,V2,F1,F2}, Fl) ->
    Active = ((array:get(V1,Fl) band 2#10) > 1) orelse
	     ((array:get(V2,Fl) band 2#10) > 1),
    case Active of
	true ->
	    array:set(Edge, array:get(Edge, Fl) bor 2#00001000, Fl);
	false ->
	    NoF3 = (array:get(F1, Fl) band 2#01000000) bsr 6 +
		(array:get(F2, Fl) band 2#01000000) bsr 6,
	    case NoF3 of
		1 -> 
		    array:set(Edge, array:get(Edge, Fl) bor 2#00010000, Fl);
		2 ->
		    array:set(Edge, array:get(Edge, Fl) bor 2#00100000, Fl);
		_ ->
		    Fl
	    end
    end.
   
scan_flags(Flags,StartFs,StartEds) ->
    Acc = {[StartFs],[StartEds]},
    {SFs,SEs} = array:sparse_foldl(fun scan_flag/3, Acc, Flags),
    {array:from_list(lists:reverse(SFs)),array:from_list(lists:reverse(SEs))}.

scan_flag(_Ix, Flag, {SFs=[NFs|_],Es0=[NEs|_]}) ->
    Es = if
	     (Flag band 2#00001000) > 1 -> [NEs + 4|Es0];
	     (Flag band 2#00110000) > 1 -> [NEs + 2|Es0];
	     true -> [NEs + 1|Es0]
	 end,
    Fs = if
	     (Flag band 2#0100000) > 1 -> [NFs + 3 | SFs]; 
	     (Flag band 2#1000000) > 1 -> [NFs + 4 | SFs];
	     true -> [NFs + 4 | SFs]
	 end,
    {Fs, Es}.
	    
%%%  Step 2 build vertices/edges and faces

gen_face_points(Fs, Flags, InVs, OutVs, SFs) ->
    array:sparse_foldl(fun gen_face_point/3, {Flags, InVs, OutVs, SFs, Fs}, Fs).

gen_face_point(Face, {V0,V1,V2,V3}, {Flags, InVs, OutVs0, SFs, Fs0}) ->
    Subdiv = array:get(Face, Flags) band 2#11000000,
    case Subdiv > 1 of
	true ->
	    Center = e3d_vec:average((array:get(V0, InVs))#v.pos, 
				     (array:get(V1, InVs))#v.pos, 
				     (array:get(V2, InVs))#v.pos, 
				     (array:get(V2, InVs))#v.pos), 
	    OutVs1 = add_center([V0,V1,V2,V3], Center, OutVs0), %% atomicly 
	    Vid = array:size(InVs) + Face,
	    NewFid = array:get(Face, SFs),
	    case Subdiv >= 2#01000000 of
		true -> %% standard 4 faces
		    New = #v{pos=Center, vc=4},
		    OutVs = array:set(Vid, New, OutVs1),
		    Fs1 = array:set(NewFid+0, {V0, -1, Vid, -1}, Fs0),
		    Fs2 = array:set(NewFid+1, {V1, -1, Vid, -1}, Fs1),
		    Fs3 = array:set(NewFid+2, {V2, -1, Vid, -1}, Fs2),
		    Fs  = array:set(NewFid+3, {V3, -1, Vid, -1}, Fs3);
		false ->
		    %% standard 3 faces
		    New = #v{pos=Center, vc=3},
		    OutVs = array:set(Vid, New, OutVs1),
		    %% Hmm
		    Fs1 = array:set(NewFid+0, {V0, -1, Vid, V3}, Fs0),
		    Fs2 = array:set(NewFid+1, {V1, -1, Vid, V3}, Fs1),
		    Fs  = array:set(NewFid+2, {V2, -1, Vid, V3}, Fs2),
		    
		    %%[array:set(V, Vs=0, Flags) || V <- [V1,V2,V3,V4]], %% atomicly 
		    ok
	    end;
	false ->
	    %% array:set(Face, {V0,V1,V2,V3}, Fs0)
	    Fs = Fs0,
	    OutVs = OutVs0
    end,
    {Flags, InVs, OutVs, SFs, Fs}.

gen_edge_points(Es, Flags, InVs, OutVs, FsNo, InFs, OutFs, EdsNo) ->
    array:sparse_foldl(fun gen_edge_point/3, {Flags,InVs,OutVs,FsNo,InFs,OutFs,EdsNo,Es}, Es).

gen_edge_point(Edge, {V0,V1,F1,F2}, {Flags,InVs,OutVs0,FsNo,InFs,Fs0,EdsNo,Es0}) ->
    Subdiv = array:get(Edge, Flags) band 2#00001000,
    case Subdiv > 1 of
	true ->
	    DMid   = e3d_vec:add((array:get(V0, InVs))#v.pos, 
				 (array:get(V1, InVs))#v.pos), 
	    OutVs1 = add_center([V0,V1], DMid, OutVs0),
	    %% Edge Split position
	    %% if Edge is hard Epoint = Mid,
	    %% else
	    VStart = array:size(InVs),
	    EP0 = e3d_vec:add([(array:get(VStart+F1, OutVs0))#v.pos,
			      (array:get(VStart+F2, OutVs0))#v.pos,
			      DMid]),
	    EP = e3d_vec:divide(EP0, 4),
	    FId = array:size(FsNo) + VStart,	 
	    VId = FId + Edge,
	    OutVs = array:set(VId, #v{pos=EP, vc=4}, OutVs1), 
	    %% Complete faces 
	    %% BUGBUG handle f3 faces...
	    SF1 = array:get(F1, FsNo),
	    {F11,F12} = find_new_faces(V0,V1,array:get(F1, InFs),SF1),
	    SF2 = array:get(F2, FsNo),
	    {F21,F22} = find_new_faces(V0,V1,array:get(F2, InFs),SF2),	   
	    io:format("Up ~p ~p~n ~p => ~p ~p~n ~p => ~p~p~n",
		      [Edge, {V0,V1}, 
		       F1, {F11,F12}, array:get(F1, InFs),
		       F2, {F21,F22}, array:get(F2, InFs)]),
	    Fs1 = update_face(F11,1,VId, Fs0),
	    Fs2 = update_face(F12,3,VId, Fs1),
	    Fs3 = update_face(F21,1,VId, Fs2),
	    Fs4 = update_face(F22,3,VId, Fs3),
	    %% New Edges
	    SEdId = array:get(Edge,EdsNo),
	    Es1 = array:set(SEdId+0, {V0,VId,F11,F22}, Es0),
	    Es2 = array:set(SEdId+0, {VId,V1,F21,F12}, Es1),
	    Es3 = array:set(SEdId+0, {VId,VStart+F1,F11,F12}, Es2),
	    Es4 = array:set(SEdId+0, {VId,VStart+F2,F21,F22}, Es3),

	    {Flags,InVs,OutVs,FsNo,InFs,Fs4,EdsNo,Es4};
	false ->
	    %% Hmm is this correct
	    {Flags,InVs,OutVs0,FsNo,InFs,Fs0,EdsNo,Es0}
    end.

%% The order is important to get ccw winding
find_new_faces(V0,V1,{V0,V1,_,_},Sid) -> {Sid+0,Sid+1};
find_new_faces(V0,V1,{V1,V0,_,_},Sid) -> {Sid+0,Sid+1};
find_new_faces(V0,V1,{_,V0,V1,_},Sid) -> {Sid+1,Sid+2};
find_new_faces(V0,V1,{_,V1,V0,_},Sid) -> {Sid+1,Sid+2};
find_new_faces(V0,V1,{_,_,V0,V1},Sid) -> {Sid+2,Sid+3};
find_new_faces(V0,V1,{_,_,V1,V0},Sid) -> {Sid+2,Sid+3};
find_new_faces(V0,V1,{V0,_,_,V1},Sid) -> {Sid+3,Sid+0};
find_new_faces(V0,V1,{V1,_,_,V0},Sid) -> {Sid+3,Sid+0}. 

update_face(Face,1,VId, Fs) ->
    try 
	Vs = {_,-1,_,_} = array:get(Face,Fs),
	array:set(Face, setelement(2,Vs,VId), Fs)
    catch _:Reason ->
	    io:format("Fs: ~p~n",[array:to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,1,VId,array:get(Face, Fs)])
    end;
update_face(Face,3,VId, Fs) ->
    try
	Vs = {_,_,_,-1} = array:get(Face,Fs),
	array:set(Face, setelement(4,Vs,VId), Fs)
    catch _:Reason ->
	    io:format("Fs: ~p~n",[array:to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,3,VId,array:get(Face, Fs)])
    end.


add_center([V|Vs], Center, OutVs) ->
    Vx = #v{pos=Pos} = array:get(V,OutVs),
    add_center(Vs, Center, array:set(V, 
				     Vx#v{pos = e3d_vec:add(Pos,Center)}, 
				     OutVs));
add_center([],_,OutVs) ->
    OutVs.

%%%%%%%%

debug_flags(Flags, Line) ->
    io:format("Flags: ~p ~n",[Line]),
    [io:format("~3.w ~8.2.0B ~n",[Id, Flag]) || {Id,Flag} <- array:to_orddict(Flags)].

