%%
%%  wpc_ai.erl --
%%
%%     Adobe Illustrator (.ai) import by Howard Trickey
%%     Some updates by Richard Jones
%%  For now:
%%     - Only Illustrator version 8 or less files parsed (v9 -> pdf)
%%     - Ignore line width, fill, clip mask, text
%%
%%
%%     $Id$
%%

-module(wpc_ai).
-export([init/0,menu/2,command/2,tryimport/2]).	% tryimport is for debugging

%% exported to wpc_ps wpc_svg_path
-export([findpolyareas/1, polyareas_to_faces/1, subdivide_pas/2]).

-import(lists, [reverse/1,splitwith/2,member/2,
		sublist/2,nthtail/2,map/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(SCALEFAC, 0.01).		% amount to scale AI coords by

-record(cedge, {vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs
-record(polyarea,
	{boundary,			%%list of cedges (CCW oriented, closed)
	 islands=[]}).			%%list of lists of cedges (CW, closed)

-record(path,
	{ops=[],			%list of pathops
	 close=false}).		%true or false

-record(pathop,
	{opkind,			%pmoveto, plineto, or pcurveto
	 x1=0.0,
	 y1=0.0,
	 x2=0.0,
	 y2=0.0,
	 x3=0.0,
	 y3=0.0}).

-record(pstate,
	{curpath=#path{},		%current path
	 objects=[]}).          %object list (paths)

init() -> true.

menu({file,import}, Menu) ->
    Menu ++ [{"Adobe Illustrator (.ai)...",ai,[option]}];
menu(_, Menu) -> Menu.

command({file,{import,{ai,Ask}}}, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(wpc_ai, bisections, 0),
    wpa:ask(Ask, ?__(1,"AI Import Options"),
	    [{?__(2,"Number of edge bisections"), DefBisect}],
	    fun(Res) -> {file,{import,ai,Res}} end);
command({file,{import,ai,[Nsub]}}, St) ->
    Props = [{ext,".ai"},{ext_desc,?__(3,"Adobe Illustrator File")}],
    wpa:import(Props, fun(F) -> make_ai(F, Nsub) end, St);

command(_, _) ->
    next.

make_ai(Name, Nsubsteps) ->
    case catch tryimport(Name, Nsubsteps) of
	{ok, E3dFile} ->
	    wpa:pref_set(wpc_ai, bisections, Nsubsteps),
	    {ok, E3dFile};
	{error,Reason} ->
	    {error, ?__(1,"AI import failed")++": " ++ Reason};
	_ ->
	    {error, ?__(2,"AI import internal error")}
    end.

tryimport(Name, Nsubsteps) ->
    case file:read_file(Name) of
	{ok,<<"%!PS-Adobe",Rest/binary>>} ->
		Objs = tokenize_bin(Rest),
		Closedpaths = [ P || P <- Objs, P#path.close == true ],
		Cntrs = getcontours(Closedpaths),
		Pas = findpolyareas(Cntrs),
		Pas1 = subdivide_pas(Pas,Nsubsteps),
		{Vs0,Fs,HEs} = polyareas_to_faces(Pas1),
		Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
		Vec = e3d_vec:sub(e3d_vec:zero(),Center),
		Vs = reverse(center_object(Vec,Vs0)),
		Efs = [ #e3d_face{vs=X} || X <- Fs],
		Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,he=HEs},
		Obj = #e3d_object{name=Name,obj=Mesh},
		{ok, #e3d_file{objs=[Obj]}};
    {ok,_} ->
		{error,?__(1,"Not an Adobe Illustrator File (Version 8 or earlier)")};
	    {error,Reason} ->
		{error,file:format_error(Reason)}
	end.

center_object(Vec,Vs) ->
    lists:foldl(fun(V,Acc) ->
       {X,Y,Z} = e3d_vec:add(V,Vec),
	   [{X,Y,Z}|Acc]
    end,[],Vs).


tokenize_bin(Bin) ->
	Chars = afterendsetup(Bin),
	Toks = tokenize(Chars, []),
	Objs = parsetokens(Toks),
	Objs.

% skip until after %%EndSetup line, as we currently use nothing before that,
% then convert rest of binary to list of characters
afterendsetup(<<"%%EndSetup",Rest/binary>>) ->
	binary_to_list(Rest);
afterendsetup(<<_,Rest/binary>>) ->
	afterendsetup(Rest);
afterendsetup(_) -> [].

% tokenize first list (characters from file) into list of tokens
% (accumulated reversed in second list, reversed at end).
% a token is {tnum,Val}, {tname,Val}, {tlitname, Val}, or {tstring}

tokenize([], Toks) ->
	reverse(Toks);
tokenize([C|T], Toks) when C == $\s; C == $\t; C == $\r; C == $\n;
			C == $); C == $> ->	% these 2 are "shouldn't happens"
	tokenize(T, Toks);
tokenize("%" ++ T, Toks) ->
	tokenize(skipline(T), Toks);
tokenize("/" ++ T, Toks) ->
	{Name,TT} = splitwith(fun isnttokbreak/1, T),
	tokenize(TT, [{tlitname,Name}|Toks]);
tokenize("(" ++ T, Toks) ->
	tokenize(skipstring(T), [{tstring}|Toks]);
tokenize("<" ++ T, Toks) ->
	tokenize(skiphexstring(T), [{tstring}|Toks]);
tokenize([C|T], Toks) when C == $[; C == $]; C == ${; C == $} ->
	tokenize(T, [{tname,[C]}|Toks]);
tokenize([C|_] = Arg, Toks) when C >= $0, C =< $9; C==$- ->
	{Tok,TT} = parsenum(Arg),
	tokenize(TT, [Tok|Toks]);
tokenize(Arg, Toks) ->
	{Name,TT} = splitwith(fun isnttokbreak/1, Arg),
	tokenize(TT, [{tname,Name}|Toks]).

% note: this list of chars be exactly those matched explicitly
% by the non-default cases of tokenize, else get infinite loop
isnttokbreak(C) -> not(member(C, " \t\r\n()<>[]{}/%")).

% AI numbers are either ints or floats
% no radix notation for ints, no scientific notation for floats
parsenum([C|Rest]=L) ->
	case re:run(L, "^((\\+|\\-?)([0-9]+\\.[0-9]*)|(\\.[0-9]+))",[{capture,first}]) of
	    {match,[{0,Length}]} ->
		    Fstr = sublist(L, Length),
		    F = list_to_float(Fstr),
		{{tnum,F}, nthtail(Length, L)};
	    nomatch ->
		  case re:run(L, "^(\\+|-)?[0-9]+", [{capture,first}]) of
		      {match, [{0, Length}]} ->
		          Istr = sublist(L, Length),
		          I = list_to_integer(Istr),
		          {{tnum,float(I)}, nthtail(Length, L)};
		      nomatch ->
		          {{tname,[C]}, Rest}
		  end
	end.

% skip past next end of line, return rest
skipline("\r\n" ++ T) -> T;
skipline("\r" ++ T) -> T;	% sometimes find files with only CRs!
skipline("\n" ++ T) -> T;
skipline([_|T]) -> skipline(T);
skipline([]) -> [].

% skip past next ")", but be careful about escaped ones
% return rest
skipstring([]) -> [];
skipstring("\\") -> [];
skipstring("\\" ++ [_|T]) -> skipstring(T);
skipstring(")" ++ T) -> T;
skipstring([_|T]) -> skipstring(T).

% skip past next ">", return rest
skiphexstring([]) -> [];
skiphexstring(">" ++ L) -> L;
skiphexstring([_|L]) -> skiphexstring(L).

% consume tokens, return list of objects.
% an object is either a path or a compoundpath.
parsetokens(Toks) ->
	#pstate{objects=Objs}=parse(Toks, #pstate{}),
	Objs.

parse([],#pstate{objects=Objs}=Pst) ->
	Pst#pstate{objects=reverse(Objs)};
parse([{tname,[_|_]=N}|T], Pst) ->
	parse(T,dorenderop(N,dopathop0(N,Pst)));
parse([{tnum,X1},{tnum,Y1},{tname,[_|_]=N}|T], Pst) ->
	parse(T,dopathop2(N,{X1,Y1},Pst));
parse([{tnum,X1},{tnum,Y1},{tnum,X2},{tnum,Y2},{tname,[_|_]=N}|T], Pst) ->
	parse(T,dopathop4(N,{X1,Y1,X2,Y2},Pst));
parse([{tnum,X1},{tnum,Y1},{tnum,X2},{tnum,Y2},{tnum,X3},{tnum,Y3},
		{tname,[_|_]=N}|T], Pst) ->
	parse(T,dopathop6(N,{X1,Y1,X2,Y2,X3,Y3},Pst));
parse([_|T], Pst) ->
	parse(T, Pst).

% check if C is a no-arg path operation, and if so, return a modified Pst,
% otherwise return original Pst
dopathop0([C],Pst) when C == $h; C == $H ->
	P = Pst#pstate.curpath,
	Pst#pstate{curpath=P#path{close=true}};
dopathop0(_,Pst) -> Pst.

dopathop2("m",{X1,Y1},Pst) ->
	finishpop(#pathop{opkind=pmoveto,x1=X1,y1=Y1},Pst);
dopathop2([C],{X1,Y1},Pst) when C==$l; C==$L ->
	finishpop(#pathop{opkind=plineto,x1=X1,y1=Y1},Pst);
dopathop2(_,_,Pst) -> Pst.

dopathop4([C],{X2,Y2,X3,Y3},Pst) when C==$v; C==$V ->
	% start point and next bezier control point coincide
	% need curpath to have an op, so can get previous point!
	Pop = #pathop{opkind=pcurveto,x2=X2,y2=Y2,x3=X3,y3=Y3},
	case Pst#pstate.curpath of
	    #path{ops=[#pathop{opkind=pmoveto,x1=X1,y1=Y1}|_]} ->
		finishpop(Pop#pathop{x1=X1,y1=Y1},Pst);
	    #path{ops=[#pathop{opkind=plineto,x1=X1,y1=Y1}|_]} ->
		finishpop(Pop#pathop{x1=X1,y1=Y1},Pst);
	    #path{ops=[#pathop{opkind=pcurveto,x3=X1,y3=Y1}|_]} ->
		finishpop(Pop#pathop{x1=X1,y1=Y1},Pst);
	    _ -> Pst
	end;
dopathop4([C],{X1,Y1,X2,Y2},Pst) when C==$y; C==$Y ->
	% end point and previous bezier control point coincide
	finishpop(#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X2,y3=Y2},Pst);
dopathop4(_,_,Pst) -> Pst.

dopathop6([C],{X1,Y1,X2,Y2,X3,Y3},Pst) when C==$c; C==$C ->
	finishpop(#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3},Pst);
dopathop6(_,_,Pst) -> Pst.

% finish job of dopathop[2,4,6] by putting arg pathop onto curpath's ops list
% and returning Pst with modified curpath
finishpop(#pathop{opkind=pmoveto}=Pop, #pstate{curpath=#path{ops=[]}}=Pst) ->
	Pst#pstate{curpath=#path{ops=[Pop]}};
finishpop(_, #pstate{curpath=#path{ops=[]}}=Pst) ->
	Pst;	% note: only pmoveto's can start path, so ignore others
finishpop(Pop, #pstate{curpath=#path{ops=Ops}=P}=Pst) ->
	Pst#pstate{curpath=P#path{ops=[Pop|Ops]}}.

% If Nam is a renderop, finish off curpath and put on objects list.
dorenderop([C],Pst) when C==$n; C==$f; C==$s; C==$b; C==$B ->
	finishrop(true,Pst);
dorenderop([C],Pst) when C==$N; C==$F; C==$S ->
	finishrop(false,Pst);
dorenderop([$B,C],Pst) when C==$b; C==$g; C==$m; C==$c; C==$B ->
	finishrop(false,Pst);
dorenderop(_,Pst) -> Pst.

finishrop(Close,#pstate{curpath=P,objects=Objs}=Pst) ->
	#path{close=Pclose,ops=Ops} = P,
	Newp = P#path{close=Close or Pclose,ops=reverse(Ops)},
	Pst#pstate{objects=[Newp|Objs],curpath=#path{}}.

getcontours(Ps) ->
	map(fun getcedges/1, Ps).

getcedges(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops]}) ->
	getcedges(Ops,{X,Y},{X,Y},[]);
getcedges(_) -> [].

getcedges([],{X,Y},{X,Y},Acc) ->
	Acc1 = map(fun (CE) -> scalece(CE,?SCALEFAC) end, Acc),
	reverse(Acc1);
getcedges([],Prev,{X,Y},Acc) ->		% prev != first, so close with line
	reverse([#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=plineto,x1=X,y1=Y}|Ops],Prev,First,Acc) ->
	getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X,y3=Y}|Ops],
		Prev,First,Acc) ->
	getcedges(Ops,{X,Y},First,
		[#cedge{vs=Prev,cp1={X1,Y1},cp2={X2,Y2},ve={X,Y}}|Acc]);
getcedges([_|_],_,_,_) ->
	[].	% funny path (probably moveto in middle), so return nothing

scalece(#cedge{vs={Xs,Ys},cp1=nil,cp2=nil,ve={Xe,Ye}},F) ->
	#cedge{vs={Xs*F,Ys*F},cp1=nil,cp2=nil,ve={Xe*F,Ye*F}};
scalece(#cedge{vs={Xs,Ys},cp1={X1,Y1},cp2={X2,Y2},ve={Xe,Ye}},F) ->
	#cedge{vs={Xs*F,Ys*F},cp1={X1*F,Y1*F},cp2={X2*F,Y2*F},ve={Xe*F,Ye*F}}.


%% Copied from old wpc_tt.erl
%%

polyareas_to_faces(Pas) ->
    VFpairs = map(fun pa2object/1, Pas),
    concatvfs(VFpairs).

concatvfs(Vfp) -> concatvfs(Vfp, 0, [], [], []).

concatvfs([{Vs,Fs,HardEdges}|Rest], Offset, Vsacc, Fsacc, Hdacc) ->
    Fs1 = offsetfaces(Fs, Offset),
    He1 = offsetfaces(HardEdges, Offset),
    Off1 = Offset + length(Vs),
    concatvfs(Rest, Off1, [Vs|Vsacc], Fsacc ++ Fs1, He1 ++ Hdacc);
concatvfs([], _Offset, Vsacc, Fsacc, Hdacc) ->
    He = build_hard_edges(Hdacc, []),
    {lists:flatten(reverse(Vsacc)),Fsacc, He}.

build_hard_edges([[First|_]=Loop|Rest], All) ->
    New = build_hard_edges(Loop, First, All),
    build_hard_edges(Rest, New);
build_hard_edges([], All) -> All.

build_hard_edges([A|[B|_]=Rest], First, All) ->
    build_hard_edges(Rest, First, [{A,B}|All]);
build_hard_edges([Last], First, All) ->
    [{Last, First}|All].

%% Subdivide (bisect each each) Nsubsteps times.
%% When bezier edges are subdivided, the inserted point goes
%% at the proper place on the curve.
subdivide_pas(Pas,0) -> Pas;
subdivide_pas(Pas,Nsubsteps) ->
    map(fun (Pa) -> subdivide_pa(Pa,Nsubsteps) end, Pas).

subdivide_pa(Pa, 0) ->
    Pa;
subdivide_pa(#polyarea{boundary=B,islands=Isls}, N) ->
    subdivide_pa(#polyarea{boundary=subdivide_contour(B),
			   islands=map(fun subdivide_contour/1, Isls)}, N-1).

subdivide_contour(Cntr) ->
    lists:flatten(map(fun (CE) -> subdivide_cedge(CE,0.5) end, Cntr)).

%% subdivide CE at parameter Alpha, returning two new CE's in list.
subdivide_cedge(#cedge{vs=Vs,cp1=nil,cp2=nil,ve=Ve},Alpha) ->
    Vm = lininterp(Alpha, Vs, Ve),
    [#cedge{vs=Vs,ve=Vm}, #cedge{vs=Vm,ve=Ve}];
subdivide_cedge(#cedge{vs=Vs,cp1=C1,cp2=C2,ve=Ve},Alpha) ->
    B0 = {Vs,C1,C2,Ve},
    B1 = bezstep(B0,1,Alpha),
    B2 = bezstep(B1,2,Alpha),
    B3 = bezstep(B2,3,Alpha),
    [#cedge{vs=element(1,B0),cp1=element(1,B1),cp2=element(1,B2),ve=element(1,B3)},
     #cedge{vs=element(1,B3),cp1=element(2,B2),cp2=element(3,B1),ve=element(4,B0)}].

bezstep(B,R,Alpha) ->
    list_to_tuple(bzss(B,0,3-R,Alpha)).

bzss(_B,I,Ilim,_Alpha) when I > Ilim -> [];
bzss(B,I,Ilim,Alpha) ->
    [lininterp(Alpha,element(I+1,B),element(I+2,B)) | bzss(B,I+1,Ilim,Alpha)].

lininterp(F,{X1,Y1},{X2,Y2}) -> {(1.0-F)*X1 + F*X2, (1.0-F)*Y1 + F*Y2}.

findpolyareas(Cconts) ->
    Areas = map(fun ccarea/1, Cconts),
    {Cc,_Ar} = orientccw(Cconts, Areas),
    Cct = list_to_tuple(Cc),
    N = size(Cct),
    Art = list_to_tuple(Areas),
    Lent = list_to_tuple(map(fun length/1,Cc)),
    Seqn = lists:seq(1,N),
    Cls = [ {{I,J},classifyverts(element(I,Cct),element(J,Cct))}
	    || I <- Seqn, J <- Seqn],
    Clsd = gb_trees:from_orddict(Cls),
    Cont = [ {{I,J},contains(I,J,Art,Lent,Clsd)}
	     || I <- Seqn, J <- Seqn],
    Contd = gb_trees:from_orddict(Cont),
    Assigned = gb_sets:empty(),
    getpas(1,N,Contd,Cct,{[],Assigned}).

getpas(I,N,Contd,Cct,{Pas,Ass}) when I > N ->
    case length(gb_sets:to_list(Ass)) of
	N ->
	    reverse(Pas);
	_ ->
	    %% not all assigned: loop again
	    getpas(1,N,Contd,Cct,{Pas,Ass})
    end;
getpas(I,N,Contd,Cct,{Pas,Ass}=Acc) ->
    case gb_sets:is_member(I,Ass) of
	true -> getpas(I+1,N,Contd,Cct,Acc);
	_ ->
	    case isboundary(I,N,Contd,Ass) of
		true ->
		    %% have a new polyarea with boundary = contour I
		    Ass1 = gb_sets:add(I,Ass),
		    {Isls,Ass2} = getisls(I,N,N,Contd,Ass1,Ass1,[]),
		    Cisls = lists:map(fun (K) -> revccont(element(K,Cct)) end, Isls),
		    Pa = #polyarea{boundary=element(I,Cct), islands=Cisls},
		    getpas(I+1,N,Contd,Cct,{[Pa|Pas],Ass2});
		_ -> getpas(I+1,N,Contd,Cct,Acc)
	    end
    end.

%% Return true if there is no unassigned J <= second arg, J /= I,
%% such that contour J contains contour I.
isboundary(_I,0,_Contd,_Ass) -> true;
isboundary(I,I,Contd,Ass) -> isboundary(I,I-1,Contd,Ass);
isboundary(I,J,Contd,Ass) ->
    case gb_sets:is_member(J,Ass) of
	true ->
	    isboundary(I,J-1,Contd,Ass);
	_ ->
	    case gb_trees:get({J,I},Contd) of
		true -> false;
		_ -> isboundary(I,J-1,Contd,Ass)
	    end
    end.

%% Find islands for contour I : i.e., unassigned contours directly inside it.
%% Only have to check J and less.
%% Ass, Isls are (assigned-so-far, islands-so-far).
%% Ass0 is assigned before we started adding islands.
%% Return {list of island indices, Assigned array with those indices added}
getisls(_I,0,_N,_Contd,_Ass0,Ass,Isls) -> {reverse(Isls),Ass};
getisls(I,J,N,Contd,Ass0,Ass,Isls) ->
    case gb_sets:is_member(J,Ass) of
	true ->
	    getisls(I,J-1,N,Contd,Ass0,Ass,Isls);
	_ ->
	    case directlycont(I,J,N,Contd,Ass0) of
		true ->
		    getisls(I,J-1,N,Contd,Ass0,gb_sets:add(J,Ass),[J|Isls]);
		_ ->
		    getisls(I,J-1,N,Contd,Ass0,Ass,Isls)
	    end
    end.

directlycont(I,J,N,Contd,Ass) ->
    gb_trees:get({I,J},Contd) andalso
	lists:foldl(fun (K,DC) ->
		      DC andalso
			   (K == J orelse gb_sets:is_member(K,Ass) orelse
			    not(gb_trees:get({K,J},Contd))) end,
	      true, lists:seq(1,N)).

ccarea(Ccont) ->
    0.5 * lists:foldl(fun (#cedge{vs={X1,Y1},ve={X2,Y2}},A) ->
			A + X1*Y2 - X2*Y1 end,
		0.0, Ccont).

%% Reverse contours if area is negative (meaning they were Clockwise),
%% and return revised Cconts and Areas.
orientccw(Cconts, Areas) -> orientccw(Cconts, Areas, [], []).

orientccw([], [], Cacc, Aacc) ->
    { reverse(Cacc), reverse(Aacc) };
orientccw([C|Ct], [A|At], Cacc, Aacc) ->
    if
	A >= 0.0 ->
	    orientccw(Ct, At, [C|Cacc], [A|Aacc]);
	true ->
	    orientccw(Ct, At, [revccont(C)|Cacc], [-A|Aacc])
    end.

revccont(C) -> reverse(map(fun revcedge/1, C)).

%% reverse a cedge
revcedge(#cedge{vs=Vs,cp1=Cp1,cp2=Cp2,ve=Ve}) ->
    #cedge{vs=Ve,cp1=Cp2,cp2=Cp1,ve=Vs}.

%% classify vertices of contour B with respect to contour A.
%% return {# inside A, # on A}.
classifyverts(A,B) -> lists:foldl(fun (#cedge{vs=Vb},Acc) -> cfv(A,Vb,Acc) end,
			    {0,0}, B).


%% Decide whether vertex P is inside or on (as a vertex) contour A,
%% and return modified pair.  Assumes A is CCW oriented.
%% CF Eric Haines ptinpoly.c in Graphics Gems IV
cfv(A,P,{Inside,On}) ->
    #cedge{vs=Va0} = lists:last(A),
    if
	Va0 == P ->
	    {Inside, On+1};
	true ->
	    Yflag0 = (element(2,Va0) > element(2,P)),
	    case vinside(A, Va0, P, false, Yflag0) of
		true -> {Inside+1, On};
		false -> {Inside, On};
		on -> {Inside, On+1}
	    end
    end.

vinside([], _V0, _P, Inside, _Yflag0) ->
    Inside;
vinside([#cedge{vs={X1,Y1}=V1}|Arest], {X0,Y0}, P={Xp,Yp}, Inside, Yflag0) ->
    if
	V1 == P ->
	    on;
	true ->
	    Yflag1 = (Y1 > Yp),
	    Inside1 =
		if
		    Yflag0 == Yflag1 -> Inside;
		    true ->
			Xflag0 = (X0 >= Xp),
			Xflag1 = (X1 >= Xp),
			if
			    Xflag0 == Xflag1 ->
				case Xflag0 of
				    true -> not(Inside);
				    _ -> Inside
				end;
			    true ->
				Z = X1 - (Y1-Yp)*(X0-X1)/(Y0-Y1),
				if
				    Z >= Xp -> not(Inside);
				    true -> Inside
				end
			end
		end,
	    vinside(Arest, V1, P, Inside1, Yflag1)
    end.

%% I, J are indices into tuple Cct of curved contours.
%% Clsd is gb_tree mapping {I,J} to [Inside,On,Outside].
%% Return true if contour I contains at least 55% of contour J's vertices.
%% (This low percentage is partly because we are dealing with polygonal approximations
%% to curves, sometimes, and the containment relation may seem worse than it actually is.)
%% Lengths (in Lent tuple) are used for calculating percentages.
%% Areas (in Art tuple) are used for tie-breaking.
%% Return false if contour I is different from contour J, and not contained in it.
%% Return same if I == J or all vertices on I are on J (duplicate contour).
contains(I,I,_,_,_) ->
    same;
contains(I,J,Art,Lent,Clsd) ->
    LenI = element(I,Lent),
    LenJ = element(J,Lent),
    {JinsideI,On} = gb_trees:get({I,J},Clsd),
    if
	JinsideI == 0 ->
	    false;
	On == LenJ, LenI == LenJ ->
	    same;
	true ->
	    if
		float(JinsideI) / float(LenJ) > 0.55 ->
		    {IinsideJ,_} = gb_trees:get({J,I},Clsd),
		    FIinJ = float(IinsideJ) / float(LenI),
		    if
			FIinJ > 0.55 ->
			    element(I,Art) >= element(J,Art);
			true ->
			    true
		    end;
		true ->
		    false
	    end
    end.

%% Return {Vs,Fs} where Vs is list of {X,Y,Z} for vertices 0, 1, ...
%% and Fs is list of lists, each sublist is a face (CCW ordering of
%% (zero-based) indices into Vs).
pa2object(#polyarea{boundary=B,islands=Isls}) ->
    Vslist = [cel2vec(B, 0.0) | map(fun (L) -> cel2vec(L, 0.0) end, Isls)],
    Vtop = lists:flatten(Vslist),
    Vbot = lists:map(fun ({X,Y,Z}) -> {X,Y,Z-0.2} end, Vtop),
    Vs = Vtop ++ Vbot,
    Nlist = [length(B) | map(fun (L) -> length(L) end, Isls)],
    Ntot = lists:sum(Nlist),
    Fs1 = [FBtop | Holestop] = faces(Nlist,0,top),
    Fs2 = [FBbot | Holesbot] = faces(Nlist,Ntot,bot),
    Fsides = sidefaces(Nlist, Ntot),
    FtopQ = e3d__tri_quad:quadrangulate_face_with_holes(FBtop, Holestop, Vs),
    FbotQ = e3d__tri_quad:quadrangulate_face_with_holes(FBbot, Holesbot, Vs),
    Ft = [ F#e3d_face.vs || F <- FtopQ ],
    Fb = [ F#e3d_face.vs || F <- FbotQ ],
    Fs = Ft ++ Fb ++ Fsides,
    {Vs,Fs, [ F#e3d_face.vs || F <- Fs1 ++ Fs2]}.

cel2vec(Cel, Z) -> map(fun (#cedge{vs={X,Y}}) -> {X,Y,Z} end, Cel).

faces(Nlist,Org,Kind) -> faces(Nlist,Org,Kind,[]).

faces([],_Org,_Kind,Acc) -> reverse(Acc);
faces([N|T],Org,Kind,Acc) ->
    FI = case Kind of
	     top -> #e3d_face{vs=lists:seq(Org, Org+N-1)};
	     bot -> #e3d_face{vs=lists:seq(Org+N-1, Org, -1)}
	 end,
    faces(T,Org+N,Kind,[FI|Acc]).

sidefaces(Nlist,Ntot) -> sidefaces(Nlist,0,Ntot,[]).

sidefaces([],_Org,_Ntot,Acc) -> lists:append(reverse(Acc));
sidefaces([N|T],Org,Ntot,Acc) ->
    End = Org+N-1,
    Fs = [ [I, Ntot+I, wrap(Ntot+I+1,Ntot+Org,Ntot+End), wrap(I+1,Org,End)]
	   || I <- lists:seq(Org, End) ],
    sidefaces(T,Org+N,Ntot,[Fs|Acc]).

%% I should be in range (Start, Start+1, ..., End).  Make it so.
wrap(I,Start,End) -> Start + ((I-Start) rem (End+1-Start)).

offsetfaces(Fl, Offset) ->
    map(fun (F) -> offsetface(F,Offset) end, Fl).

offsetface(F, Offset) ->
    map(fun (V) -> V+Offset end, F).

