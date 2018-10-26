%%
%%  wpc_ai.erl --
%%
%%     Adobe Illustrator (.ai) import by Howard Trickey
%%     Some updates by Richard Jones
%%  For now:
%%     - Only Illustrator version 8 or less files parsed (v9 -> pdf)
%%     - Ignore line width, fill, clip mask, text
%%
%%  To work, the wpc_tt plugin must also be loaded.
%%
%%     $Id$
%%

-module(wpc_ai).
-export([init/0,menu/2,command/2,tryimport/2]).	% tryimport is for debugging

-import(lists, [reverse/1,splitwith/2,member/2,
		sublist/2,nthtail/2,map/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(SCALEFAC, 0.01).		% amount to scale AI coords by

-record(cedge,% polyarea and cedge records must match definitions in wpc_tt.erl
	{vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs

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
		Pas = wpc_tt:findpolyareas(Cntrs),
		Pas1 = wpc_tt:subdivide_pas(Pas,Nsubsteps),
		{Vs0,Fs,HEs} = wpc_tt:polyareas_to_faces(Pas1),
		Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
		Vec = e3d_vec:sub(e3d_vec:zero(),Center),
		Vs = lists:reverse(center_object(Vec,Vs0)),
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
