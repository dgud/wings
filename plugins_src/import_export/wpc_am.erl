% A:M exporter
%
% Howard Trickey
%
% Does not work well with recent versions of A:M (e.g. 11).
% No longer included in Wings. The source code is kept
% here just in case.
%
% $Id: wpc_am.erl,v 1.7 2004/12/14 07:51:26 bjorng Exp $
%
-module(wpc_am).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").
-import(lists,[reverse/1,last/1,foldl/3,nthtail/2,sublist/2,
		seq/2,foreach/2]).

% Element in Path of edges
-record(pel,
	{vs,	% id of start vert
	 ve,	% id of dest vert
	 e,	% id of edge
	 dir	% forward or backward
	}).

% State used during spline assignment
-record(sst,
	{nextid,	% next unassigned Spline id
	 we,		% the we shape being converted
	 eunmapped,	% gb_set of unmapped We edge ids
	 emap,		% gb_tree: we edge id -> spline id
	 smap,		% gb_tree: spline id -> pel list
	 closed,	% gb_set: spline ids of closed loop splines
	 options	% property list, from export option dialog
	}).

% State used during cp assignment
-record(cst,
	{nextcp,	% next unassigned A:M CP number
	 cl,		% gb_set: spline ids of closed loop splines
	 vmap,		% gb_tree: wings vertex id -> vtx record
	 splines,	% gb_tree: spline id -> spline record (accumulator)
	 cpp,		% gb_tree: A:M CP id -> {X,Y,Z} if root, or next CP if in stack
	 cpstk,		% gb_tree: wings vertex id -> CP id stack (last is root CP)
	 smang		% smooth angle (0 means all peaked)
	}).

% Info needed about one (A:M) CP
-record(cp,
	{id,		% A:M cp index
	 v,		% we vertex index
	 flags		% see following defines
	}).
-define(CP_SMOOTH, 1).
-define(CP_CLOSED, 4).
-define(CP_HOOK, 16).

% Info needed about one (A:M) spline
-record(spline,
	{ncps,		% length of cps list
	 cps,		% list of cp records
	 closed,	% true if closed, false if not
	 pels		% pel list
	}).

% Info needed about one (A:M) patch
-record(patch,
	{flags=0,	% see following defines
	 cps		% list of cp records
	}).
-define(P_IS5POINT, 1).

% Info needed about an A:M model
-record(mdl,
	{name,		% model name
	 splines,	% gb_tree: Spline Id -> spline record
	 patches5,	% list of 5pt patches
	 cppos		% gb_tree: CP Id -> pos {X,Y,Z} (if root) or RootCPId
	}).

init() ->
    %% Intentinally disabled.
    false.

menu({file,export}, Menu0) ->
	Menu0 ++ [{"Hash A:M (.mdl)...", mdl, [option]}];
menu(_, Menu) -> Menu.

% First time, gets called with Arg=true if invoked with option box
% and Arg=false if invoked without.
% That causes a second call where Arg is list of options.
command({file,{export,{mdl,Arg}}}, St) ->
    if
	is_atom(Arg) ->
	    wpa:dialog(Arg, "Hash A:M Export Options", dialog_qs(),
		       fun(Res) -> {file,{export,{mdl,Res}}} end);
	true ->
	    set_pref(Arg),
	    Prop = [{ext,".mdl"},{ext_desc,"Hash A:M Model File"}],
	    wpa:export_filename(Prop, St, fun(Name) ->
						  ?SLOW(export(Name, Arg, St))
					  end)
    end;
command(_, _) -> next.

dialog_qs() ->
    [{vradio,[{"A:M V9 or V9.5 format",v9},
	      {"A:M V6 -- V8.5 format",v6}],
      get_pref(mdl_ver, v9),
      [{key,mdl_ver},{title,"Mdl Format"}]},
     {vradio,[{"yes",true},
	      {"no",false}],
      get_pref(use_5pt, true),
      [{key,use_5pt},{title,"Generate 5-point patches?"}]},
     {vradio,[{"yes",true},
	      {"no",false}],
      get_pref(avoid_ipatch, true),
      [{key,avoid_ipatch},{title,"Avoid internal patches?"}]},
     {vradio,[{"smooth when > smooth-angle",true},
	      {"all peaked",false}],
      get_pref(smooth,true),
      [{key,smooth},{title,"Control point smoothness"}]},
     {vframe,
      [{label_column,
	[{"Smooth angle",
	  {slider,{text,get_pref(smooth_angle,135),
		   [{range,{0,180}},{key,smooth_angle}]}}}]}]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export(Name, Options, #st{shapes=Shapes0}) ->
	Shapes1 = gb_trees:values(Shapes0),
	Mdl = to_mdl(wings_we:merge(Shapes1), Options),
	MdlVer = proplists:get_value(mdl_ver, Options),
	write_file(Name, MdlVer, Mdl).

write_file(Name, Ver, Mdl) ->
	case file:open(Name, [write]) of
	{ok, F} ->
		io:put_chars(F, "[MODELFILE]\r\n"),
		write_filehead(F, Ver),
		io:put_chars(F, "[OBJECTS]\r\n"),
		write_model(F, Mdl, Ver),
		io:put_chars(F, "[ENDOBJECTS]\r\n"),
		write_fileinfo(F, Ver),
		io:put_chars(F, "[ENDMODELFILE]\r\n"),
		file:close(F);
	{error,Reason} -> {error,file:format_error(Reason)}
	end.

write_filehead(F, v6) ->
	io:put_chars(F, "ProductVersion=6.1\r\n");
write_filehead(F, v9) ->
	io:put_chars(F, "ProductVersion=9.03\r\n").

write_fileinfo(_F, v6) ->
	ok;
write_fileinfo(F, v9) ->
	io:put_chars(F, "[FileInfo]\r\n"),
	io:put_chars(F, "CreatedBy=wings3d\r\nOrganization=\r\nUrl=\r\nEmail=\r\n"),
	io:put_chars(F, "[EndFileInfo]\r\n").

write_model(F, #mdl{splines=Splines,patches5=P5,cppos=Cppos}, Ver) ->
	io:put_chars(F, "[MODEL]\r\n"),
	write_mesh(F, Splines, Cppos),
	write_patches(F, P5, Ver),
	io:put_chars(F, "[ENDMODEL]\r\n").

write_mesh(F, Splines, Cppos) ->
	io:put_chars(F, "[MESH]\r\nVersion=2\r\n"),
	io:format(F, "Splines=~p\r\n", [gb_trees:size(Splines)]),
	foreach(fun (S) -> write_spline(F, S, Cppos) end, gb_trees:values(Splines)),
	io:put_chars(F, "[ENDMESH]\r\n").

write_spline(F, #spline{ncps=Ncps, cps=Cps}, Cppos) ->
	io:format(F, "CPs=~w\r\n", [Ncps]),
	foreach(fun (C) -> write_cp(F, C, Cppos) end, Cps).

write_cp(F, #cp{id=Id, flags=Fl}, Cppos) ->
	case gb_trees:get(Id, Cppos) of
	{X, Y, Z} ->
		io:format(F, "~w 0 ~w\r\n", [Fl, Id]),
		io:format(F, "~f ~f ~f\r\n", [X, Y, Z]);
	I ->
		io:format(F, "~w 1 ~w\r\n", [Fl, Id]),
		io:format(F, "~w\r\n", [I])
	end,
	io:put_chars(F, "0\r\n0\r\n").	% bias lines

write_patches(F, P5, Ver) ->
	N = length(P5),
	if
	N == 0 -> done;
	true ->
		io:format(F, "[PATCHES]\r\nVersion=~w\r\n",
			[case Ver of v6 -> 2; _ -> 3 end]),
		io:put_chars(F, "Count=0\r\n"),	% let A:M compute regular patches
		io:format(F, "~w\r\n", [N]),
		foreach(fun (P) -> write_patch(F, P, Ver) end, P5),
		io:put_chars(F, "[ENDPATCHES]\r\n")
	end.

write_patch(F, #patch{cps=Cps,flags=Flags}, v6) ->
	io:format(F, "~w\r\n", [Flags]),
	foreach(fun (#cp{id=CpId}) -> io:format(F, "~w ", [CpId]) end, Cps),
	io:put_chars(F, "\r\n");
write_patch(F, #patch{cps=Cps,flags=Flags}, v9) ->
	io:format(F, "~w\r\n", [Flags]),
	foreach(fun (#cp{id=CpId}) -> io:format(F, "~w ", [CpId]) end, Cps),
	% For now, use index 0 for normals index
	foreach(fun (_Cp) -> io:put_chars(F, "0 ") end, Cps),
	% For now, use index 0 for attribute index
	io:put_chars(F, "0\r\n").

to_mdl(#we{name=Name}=We, Options) ->
	Use5pt = proplists:get_value(use_5pt, Options),
	AvoidIpatch = proplists:get_value(avoid_ipatch, Options),
	We1 = quad(We, if Use5pt -> 6; true -> 5 end),
	Sst0 = #sst{nextid=0, we=We1,
		eunmapped=gb_sets:from_list(gb_trees:keys(We1#we.es)),
		emap=gb_trees:empty(),
		smap=gb_trees:empty(),
		closed=gb_sets:empty(),
		options=Options},
	FP = get_face_patches(We1),
	FP5 = if
		Use5pt -> lists:filter(fun (Pels) -> length(Pels) == 5 end, FP);
		true -> []
		end,
	PIP = if
		AvoidIpatch ->
			P = get_patches(We1),
			Pset = gb_sets:from_list(P),
			FPset = gb_sets:from_list(FP),
			gb_sets:to_list(gb_sets:difference(Pset, FPset));
		true -> []
		end,
	Sst1 = assign_pips(Sst0, PIP),
	Sst2 = assign(Sst1),
	Mdl1 = sst_to_mdl(Sst2),
	Mdl2 = add_5pt_patches(Mdl1, Sst2, FP5),
	Mdl2#mdl{name=Name}.

get_patches(#we{vp=Vp}=We) ->
	Vvisited = gb_sets:empty(),
	Vl = gb_trees:keys(Vp),
	patches(Vl, Vvisited, We, []).

patches([], _, _, Acc) -> Acc;
patches([V|Rest], Vvisited, We, Acc) ->
	case gb_sets:is_member(V, Vvisited) of
	true ->
		patches(Rest, Vvisited, We, Acc);
	false ->
		Acc1 = patches_from(V, We, Acc),
		Vvisited1 = gb_sets:insert(V, Vvisited),
		patches(Rest, Vvisited1, We, Acc1)
	end.

% patches are list of pels, length 3 or 4, forming cycles.
% canonical rep: starts from lowest-numbered vertex in cycle
% and goes in direction towards whichever of its neighbors
% is the lower-numbered one.
% The patches from V are added to input Acc.
patches_from(V, We, Acc) ->
	Q = out_pels(V, -1, We),
	psearch(Q, V, V, [], 0, We, Acc).

psearch([], _, _, _, _, _, Acc) -> Acc;
psearch([#pel{ve=V}=Pel | Prest], Vfrom, Vstart, Path, N, We, Acc) ->
	N1 = N+1,
	Acc1 = if
		V < Vstart ->
			Acc;	% won't lead to canonical cycle
		V == Vstart  ->
			if
			N1 >= 3 ->
				% canonical if second vert < Vfrom
				#pel{ve=V2} = last(Path),
				if
				V2 < Vfrom -> [reverse([Pel | Path]) | Acc];
				true -> Acc
				end;
			true -> Acc
			end;
		true ->
			if
			N1 == 4 -> Acc;
			true ->
				Q = out_pels(V, Vfrom, We),
				psearch(Q, V, Vstart, [Pel | Path], N1, We, Acc)
			end
		end,
	psearch(Prest, Vfrom, Vstart, Path, N, We, Acc1).

% Return list of pels that lead out of vertex with id V,
% but not including those that lead to vertex with id Vexclude
out_pels(V, Vexclude, We) ->
	wings_vertex:fold(
		fun(Edge, _Face, E, Acc) ->
			Vother = case E of
				#edge{vs=V,ve=Vx} -> Vx;
				#edge{vs=Vx,ve=V} -> Vx
				end,
			case Vother of
			Vexclude -> Acc;
			_ -> [pel_for(E, Edge, V) | Acc]
			end
		    end,
		[],
		V,
		We).

% Get patches (canonical - see above) according to
% faces of We.
% Exclude faces that have the _hole_ material,
% as they should be treated like "internal patches",
% and made not to render.
get_face_patches(We) ->
    Ms = wings_material:get_all(We),
    foldl(fun({F,Mat}, Acc) -> face_patch(F, Mat, We, Acc) end, [], Ms).

face_patch(_, '_hole_', _, Acc) -> Acc;
face_patch(Face, _, We, Acc) ->
    Pels = wings_face:fold(
	     fun(V, Edge, Erec, Acc0) ->
		     [pel_for(Erec, Edge, V) | Acc0] end,
	     [], Face, We),
    Pels1 = reverse(Pels),
    Pels2 = canon(Pels1),
    %% io:format("face patch: ~p~n", [Pels2]),
    [Pels2 | Acc].

pel_for(E, Edge, Vs) ->
	case E of
	#edge{vs=Vs,ve=Vother} ->
		#pel{vs=Vs,ve=Vother,e=Edge,dir=forward};
	#edge{ve=Vs,vs=Vother} ->
		#pel{vs=Vs,ve=Vother,e=Edge,dir=backward}
	end.

pel_for(E, #we{es=Es}) ->
	#edge{vs=Vs,ve=Ve} = gb_trees:get(E, Es),
	#pel{vs=Vs,ve=Ve,e=E,dir=forward}.

canon(Pels) ->
	{_MinV, MinI, _} = foldl(fun(#pel{vs=V},{MV,MI,I}) ->
				if
				V < MV -> {V,I,I+1};
				true -> {MV,MI,I+1}
				end end, {10000000,-1,0}, Pels),
	Pels1 = if
		MinI == 0 -> Pels;
		true -> nthtail(MinI, Pels) ++ sublist(Pels, MinI)
		end,
	#pel{ve=V2} = hd(Pels1),
	#pel{vs=VNm1} = last(Pels1),
	if
	V2 < VNm1 -> Pels1;
	true -> reverse_pels(Pels1)
	end.

reverse_pels(Pels) -> reverse_pels(Pels, []).

reverse_pels([], Acc) -> Acc;
reverse_pels([Pel | Rest], Acc) ->
	reverse_pels(Rest, [reverse_pel(Pel) | Acc]).

reverse_pel(#pel{vs=Vs,ve=Ve,e=E,dir=forward}) ->
	#pel{vs=Ve,ve=Vs,e=E,dir=backward};
reverse_pel(#pel{vs=Vs,ve=Ve,e=E,dir=backward}) ->
	#pel{vs=Ve,ve=Vs,e=E,dir=forward}.

% Assign edges in potential-internal-patch cycles,
% trying for complete closed splines for each.
assign_pips(Sst, []) -> Sst;
assign_pips(#sst{eunmapped=Eu}=Sst, [P|Prest]) ->
	Sst1 = case all_unassigned(P, Eu) of
		true ->
			#sst{nextid=I,emap=Emap,smap=Smap,closed=Cl}=Sst,
			Smap1 = gb_trees:insert(I, P, Smap),
			{Emap1, Eu1} = foldl(fun (#pel{e=E}, {Em,Eum}) ->
				{gb_trees:insert(E, I, Em), gb_sets:delete(E, Eum)} end,
				{Emap,Eu}, P),
			Cl1 = gb_sets:insert(I, Cl),
			Sst#sst{nextid=I+1, eunmapped=Eu1, emap=Emap1,
					smap=Smap1, closed=Cl1};
		_ ->
			% io:format("couldn't assign pip patch ~p~n", [P]),
			Sst
		end,
	assign_pips(Sst1, Prest).

all_unassigned([], _Eu) -> true;
all_unassigned([#pel{e=E}|Rest], Eu) ->
	gb_sets:is_member(E, Eu) andalso all_unassigned(Rest, Eu).

% Assign rest of unassigned edges to splines,
% being careful not to form a cycle of length 3 or 4
% because the remaining such paths SHOULD form patches in A:M.
% Returns sst with smap all filled in.
assign(#sst{nextid=I0,eunmapped=Eu,we=We,emap=Emap,smap=Smap}=Sst) ->
	GE = gb_sets:fold(fun (E, L) -> [bestsucc(E, We) | L] end, [], Eu),
	GEsorted = lists:sort(GE),
	% io:format("gesorted=~p~n",[GEsorted]),
	% to start, map all unmapped edges to singleton pels
	{Emap1, Smap1, I1} = gb_sets:fold(fun (E, {Em,Sm,I}) ->
				{gb_trees:insert(E, I, Em),
				 gb_trees:insert(I, [pel_for(E,We)], Sm),
				 I+1} end,
				{Emap, Smap, I0}, Eu),
	Sst1 = Sst#sst{nextid=I1, eunmapped=gb_sets:empty(), emap=Emap1, smap=Smap1},
	pathmerge(GEsorted, Sst1).

% Return {goodness,best_succ_pel,E} : the goodness ( smaller == better) and best successor
% pel to edge E.  "best" means closer to straight.
bestsucc(E, #we{es=Emap,vp=Vpmap}=We) ->
	#edge{vs=Vs,ve=Ve} = gb_trees:get(E, Emap),
	Vspos = gb_trees:get(Vs, Vpmap),
	Vepos = gb_trees:get(Ve, Vpmap),
	Pels = out_pels(Ve, Vs, We),
	foldl(fun (#pel{ve=Vn}=Pel, {G,_GPel,E0}=Acc) ->
		Vnpos = gb_trees:get(Vn, Vpmap),
		X = straightness(Vspos, Vepos, Vnpos),
		if
		X < G -> {X, Pel, E0};
		true -> Acc
		end end,
		{1000.0, #pel{}, E},
		Pels).

% Return 0 if P1->P2->P3 is a straight line,
% else greater number as angle increases (max: 2).
straightness(P1,P2,P3) ->
	Vec1 = e3d_vec:norm(e3d_vec:sub(P2,P1)),
	Vec2 = e3d_vec:norm(e3d_vec:sub(P3,P2)),
	Dot = e3d_vec:dot(Vec1,Vec2),
	1.0 - Dot.

% First arg is list of {goodness,best_succ_pel,E} (as returned by bestsucc).
% One at a time, try continuing the spline assigned to id E with its bestsucc.
pathmerge([], Sst) -> Sst;
pathmerge([{_,Psucc,E}|Rest], Sst) ->
	pathmerge(Rest, trymerge(E,Psucc,Sst)).

% Try continuing the spline assigned to id E with Psucc.
% Return possibly-modified Sst.
trymerge(E,#pel{e=Esucc}=Psucc,#sst{emap=Emap,closed=Cl}=Sst) ->
	Se = gb_trees:get(E, Emap),
	Sesucc = gb_trees:get(Esucc, Emap),
	if
	Se == Sesucc ->
		Sst;
	true ->
		case gb_sets:is_member(Se,Cl) orelse gb_sets:is_member(Sesucc,Cl) of
		true ->
			Sst;
		_ ->
			tryextend(E, Psucc, Se, Sesucc, Sst)
		end
	end.

% We know Sa and Sb are different spline ids, and neither are closed.
% And, we have that Psucc (in Sb) is pel that is best successor of edge E (in Sa).
% See if we can pat Sb on end of Sa.
% If Sb in turn has an end that can attach to the beginning of Sa,
% make the spline closed iff (a) it doesn't form a cycle of length <= 4
% and (b) that continuation was the best one.
tryextend(E, #pel{e=Esucc}, Sa, Sb, #sst{smap=Smap}=Sst) ->
	Sap = gb_trees:get(Sa, Smap),
	Sbp = gb_trees:get(Sb, Smap),
	#pel{vs=Vafirst,e=Eafirst} = hd(Sap),
	#pel{ve=Valast,e=Ealast} = last(Sap),
	#pel{vs=Vbfirst,e=Ebfirst} = hd(Sbp),
	#pel{ve=Vblast,e=Eblast} = last(Sbp),
	% io:format("tryextend~n Sap=~p~n Sbp=~p~n",[Sap,Sbp]),
	case {E, Esucc, Vafirst, Valast, Vbfirst, Vblast, nocross(Sap, Sbp)} of
	{Ealast, Ebfirst, _, Vx, Vx, _, true} ->
		extend(Sa, Sb, Sap, Sbp, Sst);
	{Ealast, Eblast, _, Vx, _, Vx, true} ->
		extend(Sa, Sb, Sap, reverse_pels(Sbp), Sst);
	{Eafirst, Ebfirst, Vx, _, Vx, _, true} ->
		extend(Sa, Sb, reverse_pels(Sap), Sbp, Sst);
	{Eafirst, Eblast, Vx, _, _, Vx, true} ->
		extend(Sa, Sb, reverse_pels(Sap), reverse_pels(Sbp), Sst);
	_ ->
		Sst
	end.

% pel Sap's end joins with Sbp's beginning (and "bestsucc" holds).
% Check to make sure, if loops back, that loop back is allowed.
% If it is allowed, or no loopback, merge the splines (Sa and Sb are spline ids).
extend(Sa, Sb, Sap, Sbp, #sst{smap=Smap,emap=Emap,closed=Cl}=Sst) ->
	#pel{vs=Vafirst} = hd(Sap),
	#pel{ve=Vblast} = last(Sbp),
	{Loopback,Badloopback} =
		if
		Vblast == Vafirst ->
			{true, (length(Sap) + length(Sbp)) =< 4};
		true ->
			{false, false}
		end,
	case Badloopback of
	true -> Sst;
	_ ->
		% splice Sb spline onto end of Sa,
		% and map Sb to empty pel list to mark "unused"
		MergedPels = Sap ++ Sbp,
		% io:format(" merged: ~p~n", [MergedPels]),
		Smap1 = gb_trees:enter(Sa, MergedPels, Smap),
		Smap2 = gb_trees:enter(Sb, [], Smap1),
		Emap1 = foldl(fun (#pel{e=E0}, Em) -> gb_trees:enter(E0, Sa, Em) end,
			Emap, Sbp),
		Cl1 = if Loopback -> gb_sets:add(Sa, Cl); true -> Cl end,
		Sst#sst{smap=Smap2,emap=Emap1,closed=Cl1}
	end.

% Return true if Pel lists S1 and S2 have no common vertices
% except those that are at either end of BOTH lists
nocross(S1, S2) ->
	Vcommon = gb_sets:intersection(pelvs(S1), pelvs(S2)),
	case gb_sets:to_list(Vcommon) of
	[] -> true;
	[V] ->
		atend(V, S1) andalso atend(V, S2);
	[V1,V2] ->
		atend(V1, S1) andalso atend(V1, S2)
		andalso atend(V2, S1) andalso atend(V2, S2);
	_ -> false
	end.

% Return true if V is at one of the ends of pel list arg (assumed nonempty)
atend(V, [#pel{vs=Vs,ve=Ve}]) ->
	V == Vs orelse V == Ve;
atend(V, [#pel{vs=Vs} | Rest]) ->
	if
	Vs == V -> true;
	true ->
		#pel{ve=Vend} = last(Rest),
		Vend == V
	end.

% Return gb_set containing WE vert ids touched by pels in argument list.
pelvs([]) -> gb_sets:empty();
pelvs([#pel{vs=Vs} | Rest]) ->
	foldl(fun (#pel{ve=V}, S) -> gb_sets:add(V, S) end,
		gb_sets:singleton(Vs), Rest).

% Return mdl
sst_to_mdl(#sst{smap=Smap,closed=Cl,we=#we{vp=Vmap},options=Options}) ->
	Smang = case proplists:get_value(smooth, Options) of
		true -> proplists:get_value(smooth_angle, Options);
		_ -> 0
		end,
	SL = gb_trees:to_list(Smap),
	% CPs need to start with index 1
	Cst = #cst{nextcp = 1, cl=Cl, vmap=Vmap, splines=gb_trees:empty(),
			cpp=gb_trees:empty(), cpstk=gb_trees:empty(),
			smang=Smang},
	stsp(SL, Cst).

% Do the work of sst_to_mdl, one spline at a time
% 1st arg: list of {SplineID,Pels_for_spline} left to convert
% 2nd arg: cst record.
% Returns: mdl record
stsp([], #cst{splines=Splines,cpp=Cpp}) ->
	#mdl{splines=Splines,cppos=Cpp};
stsp([{_Sid,[]} | Rest], Cst) ->
	stsp(Rest, Cst);
stsp([{Sid,Pels} | Rest], #cst{nextcp=Nextcp,cl=Cl,splines=Splines}=Cst) ->
	IsCl = gb_sets:is_member(Sid, Cl),
	Prevpel =
		if
		IsCl -> last(Pels);
		true -> nil
		end,
	{Cps, Cst1} = pels_to_cps(Pels, Prevpel, IsCl, Cst, []),
	Ncps = Cst1#cst.nextcp - Nextcp,
	Spline = #spline{ncps=Ncps,cps=Cps,closed=IsCl,pels=Pels},
	Cst2 = Cst1#cst{splines=gb_trees:insert(Sid, Spline, Splines)},
	stsp(Rest, Cst2).

% Convert one list of pels to corresponding list of cps
% 1st arg: list of pels left to convert
% 2nd arg: previous pel, nil if none (but pay attention to wraparound)
% 3rd arg: true if spline is closed, else false
% 4th arg: cst record
% 5th arg: list of cps so far (accumulated, reversed)
% Returns: {list of cps, modified cst}
pels_to_cps([#pel{vs=Vs,ve=Ve}=Pel], Prevpel, false, #cst{nextcp=Nextcp}=Cst, Acc) ->
	% not closed -> need cps for both ends of last edge
	Smflag = smoothcheck(Prevpel, Pel, Cst),
	CP1 = #cp{id=Nextcp,v=Vs,flags=Smflag},
	CP2 = #cp{id=Nextcp+1,v=Ve,flags=Smflag},
	Cst1 = add_to_cpmap(CP1, Cst),
	Cst2 = add_to_cpmap(CP2, Cst1),
	{reverse([CP2, CP1 | Acc]), Cst2#cst{nextcp=Nextcp+2}};
pels_to_cps([#pel{vs=Vs}=Pel], Prevpel, true, #cst{nextcp=Nextcp}=Cst, Acc) ->
	% closed -> only need cp for start of last edge
	Smflag = smoothcheck(Prevpel, Pel, Cst),
	CP = #cp{id=Nextcp,v=Vs,flags=?CP_CLOSED bor Smflag},
	Cst1 = add_to_cpmap(CP, Cst),
	{reverse([CP | Acc]), Cst1#cst{nextcp=Nextcp+1}};
pels_to_cps([#pel{vs=Vs}=Pel | Rest], Prevpel, IsCl, #cst{nextcp=Nextcp}=Cst, Acc) ->
	Smflag = smoothcheck(Prevpel, Pel, Cst),
	CP = #cp{id=Nextcp,v=Vs,flags=Smflag},
	Cst1 = add_to_cpmap(CP, Cst),
	pels_to_cps(Rest, Pel, IsCl, Cst1#cst{nextcp=Nextcp+1}, [CP | Acc]).

add_to_cpmap(#cp{id=Id,v=V}, #cst{vmap=Vmap, cpp=Cpp, cpstk=Cpstk}=Cst) ->
	case gb_trees:lookup(V, Cpstk) of
	none ->
		Pos = gb_trees:get(V, Vmap),
		Cst#cst{cpp=gb_trees:insert(Id, Pos, Cpp),
			cpstk=gb_trees:insert(V, [Id], Cpstk)};
	{value, [Stktop | _]=Stk} ->
		Cst#cst{cpp=gb_trees:insert(Id, Stktop, Cpp),
			cpstk=gb_trees:enter(V, [Id | Stk], Cpstk)}
	end.

% See if CP at beginning of Pel should be "smooth"
% according to options and angle at that CP.
% Prevpel is the preceding pel, or nil if there is none.
% Return ?CP_SMOOTH if it is to be smooth, else peaked.
smoothcheck(nil, _Pel, _Cst) -> 0;
smoothcheck(#pel{vs=V1},#pel{vs=V2,ve=V3},#cst{smang=Smang,vmap=Vmap}) ->
	if
	Smang == 0 -> ?CP_SMOOTH;
	true ->
		V1pos = gb_trees:get(V1, Vmap),
		V2pos = gb_trees:get(V2, Vmap),
		V3pos = gb_trees:get(V3, Vmap),
		Ang = angle(V1pos, V2pos, V3pos),
		if
		Ang >= Smang -> ?CP_SMOOTH;
		true -> 0
		end
	end.

% Return angle in degrees, 0 to 180, that P1->P2->P3 makes
angle(P1, P2, P3) ->
	Vec1 = e3d_vec:norm(e3d_vec:sub(P1,P2)),
	Vec2 = e3d_vec:norm(e3d_vec:sub(P3,P2)),
	Dot = e3d_vec:dot(Vec1,Vec2),
	if
	Dot =< -1.0 -> 180.0;
	Dot >= 1.0 -> 0.0;
	true -> math:acos(Dot)*180.0/math:pi()
	end.

% Quadrangulate all faces of We that have >= TessN vertices
% and do not have the _hole_ material assigned
quad(We, TessN) ->
	Tfaces = tfaces(We, TessN),
	wpa:quadrangulate(Tfaces, We).

tfaces(#we{fs=Ftab}=We, TessN) ->
	It = gb_trees:iterator(Ftab),
	tfaces_loop(It, We, TessN, []).

tfaces_loop(It, We, TessN, Acc) ->
    case gb_trees:next(It) of
	{Fid, _, It1} ->
	    Mat = wings_material:get(Fid, We),
	    case (Mat == '_hole_') orelse (wings_face:vertices(Fid, We) < TessN) of
		true -> tfaces_loop(It1, We, TessN, Acc);
		_ -> tfaces_loop(It1, We, TessN, [Fid | Acc])
	    end;
	none ->
	    Acc
    end.

% FP5 is list of pels representing 5pt patches.
% Return Mdl modified with patches5 = corresponding list of #patch records.
add_5pt_patches(#mdl{splines=Splines}=Mdl, Sst, FP5) ->
	Patches = mp5(Sst, Splines, FP5, []),
	Mdl#mdl{patches5=Patches}.

mp5(_Sst, _Splines, [], Acc) -> Acc;
mp5(Sst, Splines, [Pels | Rest], Acc) ->
	P = #patch{cps=patchcps(Pels, Sst, Splines),flags=?P_IS5POINT},
	mp5(Sst, Splines, Rest, [P | Acc]).

% Pels is list of pels.  Return corresponding patch record.
patchcps(Pels, Sst, Splines) -> pcps(Pels, Sst, Splines, []).

pcps([], _Sst, _Splines, Acc) -> reverse(Acc);
pcps([#pel{vs=Vs,e=E}|Rest], #sst{emap=Emap}=Sst, Splines, Acc) ->
	Sid = gb_trees:get(E, Emap),
	S = gb_trees:get(Sid, Splines),
	Cp = cp_for_v(S, Vs),
	pcps(Rest, Sst, Splines, [Cp | Acc]).

% Return CP record cooresponding to V
cp_for_v(#spline{cps=Cps}, V) -> cfv(Cps, V).

% Assumes V is has corresponding CP somewhere in list (will bomb if not)
cfv([#cp{v=V}=Cp|_], V) -> Cp;
cfv([_|Rest], V) -> cfv(Rest, V).
