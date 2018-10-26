%%
%%  wpc_ps.erl --
%%
%%    Adobe PostScript (*.ps/*.eps) import based on wpc_ai.erl by Howard Trickey
%%    To work, the wpc_tt plugin must also be loaded.
%%
%%  Copyright (c) 2009-2011 Richard Jones.
%%                2017 Micheus (add/fixed support to Adobe Illustrator, LibreOffice, Inkscape and scribus (partial)).
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_ps).
-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(SCALEFAC, 0.01).		% amount to scale PS coords by

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
         objects=[],                    %object list (paths)
         curobjs=[]}).                  %current object been processed

init() -> true.

menu({file,import}, Menu) ->
    Menu ++ [{"Adobe PostScript (.ps|.eps)...", ps, [option]}];
menu(_, Menu) -> Menu.

command({file,{import,{ps,Ask}}}, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(wpc_ps, ps_bisections, 0),
    wpa:ask(Ask, ?__(4,"PS/EPS Import Options"),
            [{?__(2,"Number of edge bisections"), DefBisect}],
            fun(Res) -> {file,{import, ps, Res}} end);
command({file,{import, ps, [Nsub]}}, St) ->
    Props = [{extensions,[{".ps",?__(5,"PostScript File")},
                          {".eps",?__(6,"Encapsulated PostScript File")}]}],
    wpa:import(Props, fun(F) -> make_ps(F, Nsub) end, St);

command(_, _) ->
    next.

make_ps(Name, Nsubsteps) ->
    case catch try_import_ps(Name, Nsubsteps) of
        {ok, E3dFile} ->
            wpa:pref_set(wpc_ps, ps_bisections, Nsubsteps),
            {ok, E3dFile};
        {error,Reason} ->
            {error, ?__(1,"PS import failed")++": " ++ Reason};
        E ->
            io:format("File Import Error Report:\n ~p\n",[E]),
            {error, ?__(2,"PS import internal error")}
    end.

try_import_ps(Name, Nsubsteps) ->
    case file:read_file(Name) of
        {ok,<<"%!PS-Adobe",Rest/binary>>} ->
            case tokenize_bin_ps(Rest) of
                {{error,no_token}, Creator} ->
                    {error, Creator ++ "\n"++?__(2,"File doesn't have a valid token structure")};
                {{error,unsuported}, Creator} ->
                    {error, Creator ++ "\n"++?__(3,"File creator unsupported")};
                {Objs0, Creator} ->
                    Objs = break_grouped_moveto(Objs0),
                    Closedpaths = [[P || P <- Obj, P#path.close == true, length(P#path.ops) > 2] || Obj <- Objs, Obj=/=[]],
                    case Closedpaths of
                        [] -> {error, Creator ++ "\n"++?__(4,"File mismatch or doesn't have valid paths")};
                        _ ->
                            Closedpaths0 = fix_inkscape(get({?MODULE,eps_creator}), Closedpaths),
                            Cntrs0 = getcontours(Closedpaths0),
                            Cntrs = reverse_def(Cntrs0),
                            %% giving some information to the user about possible absent objects
                            if length(Cntrs) =/= length(Objs) ->
                                io:format("~ts: ~ts\n",[Creator, ?__(5,"Some token structures were not valid in the file and were ignored")]);
                                true -> ok
                            end,
                            Pas = [wpc_tt:findpolyareas(Cntr) || Cntr <- Cntrs],
                            Pas0 = lists:flatten(Pas),
                            Pas1 = wpc_tt:subdivide_pas(Pas0,Nsubsteps),
                            {Vs0,Fs,HEs} = process_islands(Pas1),
                            Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
                            Vec = e3d_vec:sub(e3d_vec:zero(),Center),
                            Vs = lists:reverse(center_object(Vec,Vs0)),
                            Efs = [ #e3d_face{vs=X} || X <- Fs],
                            Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,he=HEs},
                            Obj = #e3d_object{name=Name,obj=Mesh},
                            {ok, #e3d_file{objs=[Obj]}}
                    end
            end;
        {ok,_} ->
            {error,?__(1,"Not an Adobe PostScript file")};
        {error,Reason} ->
            {error,file:format_error(Reason)}
    end.

%%% put the object definiton in reverse order to build taces with valid normals
reverse_def(Contours) ->
    [[lists:reverse(Cntr) || Cntr <- Cntrs] || Cntrs <- Contours, Cntrs=/=[[]]].

process_islands(Plas) ->
    Process = fun(Pla) -> wpc_tt:polyareas_to_faces([Pla]) end,
    Objs =
        lists:foldr(fun(Pla, Acc) ->
            %% it was noticed during the tests that some files may contain data that causes
            %% wpc_tt crashes with a key_exists error. We ignore that path definition and go on
            case catch Process(Pla) of
                {'EXIT',{{key_exists,_},_}} -> Acc;
                Res -> [Res|Acc]
            end
        end, [], Plas),
    fix_slands_vertices(Objs).

%%% fixes the vertex number relative to the entire object to be created.
fix_slands_vertices(Objs) ->
    fix_slands_vertices(Objs,{0,{[],[],[]}}).

fix_slands_vertices([], {_,Acc}) -> Acc;
fix_slands_vertices([{Vs,Fs0,HEs0}|Objs], {I,{AcVs,AcFs,AcHEs}}) ->
    Fs = [[V+I || V <- F] || F <- Fs0],
    HEs = [{Va+I,Vb+I} || {Va,Vb} <- HEs0],
    fix_slands_vertices(Objs, {I+length(Vs), {AcVs++Vs,AcFs++Fs,AcHEs++HEs}}).

%% some paths definitions can contain many 'pmoveto' operators that cannot be understand
%% for the code in wpc_tt module. Then, we break them in separated objects in order to
%% provide user with most objects as he/she expect to get.
break_grouped_moveto(Objs) ->
    [break_grouped_moveto_0(Paths,[]) || Paths <- Objs].
break_grouped_moveto_0([], Acc) -> lists:flatten(Acc);
break_grouped_moveto_0([#path{ops=Ops0}=Path0|Paths], Acc0) ->
    Acc =
        case break_grouped_moveto_1(lists:reverse(Ops0),[],[]) of
            [Ops0] -> Path0;
            NewOps -> [#path{ops=NOps,close=true} || NOps <- NewOps]
        end,
    break_grouped_moveto_0(Paths, [Acc|Acc0]).
break_grouped_moveto_1([], [], Acc) -> Acc;
break_grouped_moveto_1([], Ops0, Acc) -> [Ops0|Acc];
break_grouped_moveto_1([#pathop{opkind=pmoveto}=Path|Paths], Ops0, Acc) ->
    Ops = [Path|Ops0],
    break_grouped_moveto_1(Paths, [], [Ops|Acc]);
break_grouped_moveto_1([Path|Paths], Ops0, Acc) ->
    break_grouped_moveto_1(Paths, [Path|Ops0], Acc).


center_object(Vec,Vs) ->
    lists:foldl(fun(V,Acc) ->
        {X,Y,Z} = e3d_vec:add(V,Vec),
        [{X,Y,Z}|Acc]
    end,[],Vs).

tokenize_bin_ps(Bin) ->
    {Creator, Chars} = after_end_setup_ps(Bin, "Undefined"),
    Objs =
        case get_creator(Creator) of
            ID when ID =:= adobe; ID =:= libre_office; ID =:= inkscape; ID =:= scribus ->
                Toks = tokenize(Chars, []), % seems to be the same as what is needed for .ps
                case Toks of
                    [] -> {error,no_token};
                    _ ->
                        put({?MODULE,eps_creator}, ID),
                        case parse_tokens_ps(Toks) of
                            [] -> {error,no_token};
                            Tokens ->
                                set_var(eps_scale,{1.0,1.0}),
                                set_var(eps_translate,{0.0,0.0}),
                                %% lists:usort will remove duplicated items since some files may
                                %% contain the fill and strock data for the same path definition
                                lists:usort(Tokens)
                        end
                end;
            _ -> {error,unsuported}
        end,
    {Objs, Creator}.

get_creator(Creator0) ->
    Creator = string:to_upper(Creator0),
    io:format("EPS Creator: ~p\n",[Creator]),
    Idx = string:str(Creator,"ADOBE"),
    if Idx > 0 -> adobe;
        true ->
            Idx0 = string:str(Creator,"LIBREOFFICE"),
            if Idx0 > 0 -> libre_office;
                true ->
                    Idx2 = string:str(Creator,"CAIRO"),
                    if Idx2 > 0 -> inkscape;
                        true ->
                            Idx3 = string:str(Creator,"SCRIBUS"),
                            if Idx3 > 0 -> scribus;
                                true -> unknow
                            end
                    end
            end
    end.

%% used to ensure the value is stored once - no overwrite if any other
%% scale and translate tags are found
set_var(Var, Value) when is_atom(Var) ->
    case get(Var) of
        undefined -> put({?MODULE,Var}, Value);
        _ -> ignore
    end.

%% skip until after %%Page: 1 1 line, as we currently use nothing before that,
%% then convert rest of binary to list of characters
after_end_setup_ps(<<"%%Page: 1 1",Rest1/binary>>, Creator) ->
    Rest0 = re:replace(Rest1, " \\.", " 0\\.", [global,{return,list}]),
    Rest = re:replace(Rest0, "\n\\.", "\n0\\.", [global,{return,list}]),
    {Creator, Rest};
after_end_setup_ps(<<"%%Creator:",Rest/binary>>, _) ->
    <<Line1:255/binary,_/binary>> = Rest,
    Line0 = binary_to_list(Line1),
    Idx = string:str(Line0, "%"),
    Line = string:sub_string(Line0, 1, Idx-1),
    Creator = string:strip(string:strip(string:strip(Line, right, $\n), right, $\r), both),
    after_end_setup_ps(Rest, string:to_upper(Creator));
after_end_setup_ps(<<_,Rest/binary>>, Creator) ->
    after_end_setup_ps(Rest, Creator);
after_end_setup_ps(_, Creator) -> {Creator, []}.

%% tokenize first list (characters from file) into list of tokens
%% (accumulated reversed in second list, reversed at end).
%% a token is {tnum,Val}, {tname,Val}, {tlitname, Val}, or {tstring}

tokenize([], Toks) ->
    lists:reverse(Toks);
tokenize([C|T], Toks) when C == $\s; C == $\t; C == $\r; C == $\n;
            C == $); C == $> ->	% these 2 are "shouldn't happens"
    tokenize(T, Toks);
tokenize("%" ++ T, Toks) ->
    tokenize(skipline(T), Toks);
tokenize("/" ++ T, Toks) ->
    {Name,TT} = lists:splitwith(fun isnttokbreak/1, T),
    tokenize(TT, [{tlitname,Name}|Toks]);
tokenize("(" ++ T, Toks) ->
    tokenize(skipstring(T), [{tstring}|Toks]);
tokenize("<" ++ T, Toks) ->
    tokenize(skiphexstring(T), [{tstring}|Toks]);
tokenize([C|T], Toks) when C == $[; C == $]; C == ${; C == $} ->
    tokenize(T, [{tname,[C]}|Toks]);
tokenize([C|_] = Arg, Toks) when C >= $0, C =< $9; C==$-; C==$. ->
    {Tok,TT} = parsenum(Arg),
    tokenize(TT, [Tok|Toks]);
tokenize(Arg, Toks) ->
    {Name,TT} = lists:splitwith(fun isnttokbreak/1, Arg),
    tokenize(TT, [{tname,Name}|Toks]).

%% note: this list of chars be exactly those matched explicitly
%% by the non-default cases of tokenize, else get infinite loop
isnttokbreak(C) -> not(lists:member(C, " \t\r\n()<>[]{}/%")).

%% PS numbers are either ints or floats
%% no radix notation for ints, no scientific notation for floats
parsenum([C|Rest]=L) ->
    case re:run(L, "^((\\+|\\-?)([0-9]+\\.[0-9]*)|(\\.[0-9]+))",[{capture,first}]) of
        {match,[{0,Length}]} ->
            Fstr0 = lists:sublist(L, Length),
            Fstr =
            case Fstr0 of
                [$.|_] -> [$0|Fstr0];
                _ -> Fstr0
            end,
            F = list_to_float(Fstr),
            {{tnum,F}, lists:nthtail(Length, L)};
        nomatch ->
            case re:run(L, "^(\\+|-)?[0-9]+", [{capture,first}]) of
                {match, [{0, Length}]} ->
                    Istr = lists:sublist(L, Length),
                    I = list_to_integer(Istr),
                    {{tnum,float(I)}, lists:nthtail(Length, L)};
                nomatch ->
                    {{tname,[C]}, Rest}
            end
    end.

%% skip past next end of line, return rest
skipline("\r\n" ++ T) -> T;
skipline("\r" ++ T) -> T;	% sometimes find files with only CRs!
skipline("\n" ++ T) -> T;
skipline([_|T]) -> skipline(T);
skipline([]) -> [].

%% skip past next ")", but be careful about escaped ones
%% return rest
skipstring([]) -> [];
skipstring("\\") -> [];
skipstring("\\" ++ [_|T]) -> skipstring(T);
skipstring(")" ++ T) -> T;
skipstring([_|T]) -> skipstring(T).

%% skip past next ">", return rest
skiphexstring([]) -> [];
skiphexstring(">" ++ L) -> L;
skiphexstring([_|L]) -> skiphexstring(L).

%% consume tokens, return list of objects.
%% an object is either a path or a compoundpath.
parse_tokens_ps(Toks) ->
    #pstate{objects=Objs,curobjs=CObjs} = _X = parse_ps(Toks, #pstate{}),
    case Objs of
        [] -> case CObjs of
                  [] -> CObjs;
                  _ -> [CObjs]
              end;
        _ -> Objs
    end.


%% PS Parse
parse_ps([],#pstate{objects=Objs}=Pst) -> % done
    Pst#pstate{objects=lists:reverse(Objs)};
parse_ps([{tname,[_|_]=N} | T ], Pst) ->
    parse_ps(T,ps_dorenderop(N,ps_dopathop0(N,Pst)));
parse_ps([{tnum,X1},{tnum,Y1},{tname,[_|_]=N}|T], Pst) ->
    parse_ps(T,ps_dopathop2(N,{X1,Y1},Pst));
parse_ps([{tnum,X1},{tnum,Y1},{tnum,W1},{tnum,H1},{tname,[_|_]=N}|T], Pst) ->
    parse_ps(T,ps_dopathop4(N,{X1,Y1,W1,H1},Pst));
parse_ps([{tnum,X1},{tnum,Y1},{tnum,X2},{tnum,Y2},{tnum,X3},{tnum,Y3},
        {tname,[_|_]=N}|T], Pst) ->
    parse_ps(T,ps_dopathop6(N,{X1,Y1,X2,Y2,X3,Y3},Pst));
parse_ps([_|T], Pst) ->
    parse_ps(T, Pst).


%% check if C is a no-arg path operation, and if so, return a modified Pst,
%% otherwise return original Pst
ps_dopathop0(CP,Pst) when CP=:="closepath"; CP=:="cp"; CP=:="h"; CP=:="cl"; CP=:="p"; CP=:="pc" ->
    P = Pst#pstate.curpath,
    Pst#pstate{curpath=P#path{close=true}};
%% we intercept the clip new path to remove the bounding box data - it causes a crash in wpc_tt module
ps_dopathop0(CP,Pst) when CP=:="clp"; CP=:="clp_npth"; CP=:="clip" ->
    Pst#pstate{curobjs=[]};
ps_dopathop0(_,Pst) -> Pst.

ps_dopathop2(MT,{X1,Y1}, Pst) when MT=:="moveto"; MT=:="mo"; MT=:="m" ->
    finishpop(#pathop{opkind=pmoveto,x1=X1,y1=Y1},Pst);
ps_dopathop2(LT,{X1,Y1},Pst) when LT=:="lineto"; LT=:="li"; LT=:="l" ->
    finishpop(#pathop{opkind=plineto,x1=X1,y1=Y1},Pst);
ps_dopathop2(MT,{X1,Y1}, Pst) when MT=:="scale"; MT=:="s" ->
    finishpop(#pathop{opkind=scale,x1=X1,y1=Y1},Pst);
ps_dopathop2(MT,{X1,Y1}, Pst) when MT=:="translate"; MT=:="t" ->
    finishpop(#pathop{opkind=translate,x1=X1,y1=Y1},Pst);
ps_dopathop2(_,_,Pst) -> Pst.

%% 're' means rectangle for Inkscape file. we translate it to regular operations and close the path
ps_dopathop4(RT,{X1,Y1,W1,H1},Pst) when RT=:="re" ->
    Pst0 = finishpop(#pathop{opkind=pmoveto,x1=X1,y1=Y1},Pst),
    Pst1 = finishpop(#pathop{opkind=plineto,x1=X1,y1=Y1+H1}, Pst0),
    Pst2 = finishpop(#pathop{opkind=plineto,x1=X1+W1,y1=Y1+H1}, Pst1),
    Pst3 = finishpop(#pathop{opkind=plineto,x1=X1+W1,y1=Y1}, Pst2),
    finishrop(true,Pst3);
ps_dopathop4(_,_,Pst) -> Pst.

ps_dopathop6(CT,{X1,Y1,X2,Y2,X3,Y3},Pst) when CT=:="curveto"; CT=:="cv"; CT=:="c"; CT=:="cu"; CT=:="ct" ->
    finishpop(#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3},Pst);
ps_dopathop6(_,_,Pst) -> Pst.



%% finish job of dopathop[2,6] by putting arg pathop onto curpath's ops list
%% and returning Pst with modified curpath
finishpop(#pathop{opkind=pmoveto}=Pop, #pstate{curpath=#path{ops=[]}}=Pst) ->
    Pst#pstate{curpath=#path{ops=[Pop]}};
finishpop(#pathop{opkind=pmoveto}=Pop, #pstate{curpath=#path{ops=[#pathop{opkind=pmoveto}]}=P}=Pst) ->
    %% note: only one pmoveto is accept by path, so ignore a second one found in Inkscape's files
    Pst#pstate{curpath=P#path{ops=[Pop]}};
finishpop(#pathop{opkind=scale,x1=XS,y1=YS}, Pst) ->
    set_var(eps_scale,{XS,YS}),
    Pst;
finishpop(#pathop{opkind=translate,x1=XT,y1=YT}, Pst) ->
    set_var(eps_translate,{XT,YT}),
    Pst;
finishpop(_, #pstate{curpath=#path{ops=[]}}=Pst) ->
    Pst;	% note: only pmoveto's can start path, so ignore others
finishpop(Pop, #pstate{curpath=#path{ops=Ops}=P}=Pst) ->
    Pst#pstate{curpath=P#path{ops=[Pop|Ops]}}.

ps_dorenderop(CP,Pst) when CP=:="closepath"; CP=:="cp"; CP=:="h"; CP=:="cl"; CP=:="p"; CP=:="pc" ->
    finishrop(true,Pst);
ps_dorenderop(NP,Pst) when NP=:="newpath"; NP=:="np"; NP=:="n" ->
    finishrop(false,Pst);
ps_dorenderop(NP,Pst) when NP=:="@"; NP=:="S" ->  % Stroke - sing a new object
%    finishrop(true,Pst);
    finishrop(obj,Pst);
ps_dorenderop(_,Pst) -> Pst.

%% If Nam is a renderop, finish off curpath and put on objects list.
finishrop(obj,#pstate{objects=Objs0,curobjs=CObjs}=Pst) ->
    Objs =
        case CObjs of
            [] -> Objs0;
            _ -> [CObjs|Objs0]
        end,
    Pst#pstate{objects=Objs,curobjs=[]};
finishrop(Close,#pstate{curpath=P,curobjs=Objs}=Pst) ->
    #path{close=Pclose,ops=Ops} = P,
    Newp = P#path{close=Close or Pclose,ops=lists:reverse(Ops)},
    Pst#pstate{curobjs=[Newp|Objs],curpath=#path{}}.

%%% fixes the inkscape issue that doesn't close the mesh in some situations
%%% in these cases we force a plineto command to the first pmoveto coordinate
fix_inkscape(inkscape, Objs) ->
    [[fix_inkscape(Path) || Path <- Paths] || Paths <- Objs];
fix_inkscape(_, Paths) -> Paths.

fix_inkscape(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops0]=Ops}=Path) ->
    [LastOp|_] = lists:reverse(Ops0),
    case LastOp of
        #pathop{opkind=plineto,x1=X,y1=Y} -> Path;
        #pathop{opkind=plineto} -> Path#path{ops=Ops++[#pathop{opkind=plineto,x1=X,y1=Y}]};
        #pathop{opkind=pcurveto,x3=X,y3=Y} -> Path;
        #pathop{opkind=pcurveto} -> Path#path{ops=Ops++[#pathop{opkind=plineto,x1=X,y1=Y}]};
        _ -> Path
    end.

getcontours(Ps) ->
    {XS,YS} = get({?MODULE,eps_scale}),
    S =
        case get({?MODULE,eps_creator}) of
            adobe -> {XS*?SCALEFAC,YS*-?SCALEFAC};
            libre_office -> {XS*?SCALEFAC,YS*-?SCALEFAC};
            inkscape -> {XS*?SCALEFAC,YS*-?SCALEFAC};
            _ -> {XS*?SCALEFAC,YS*?SCALEFAC}
        end,
    T = get({?MODULE,eps_translate}),
    Ps0 = [lists:map(fun getcedges/1, P) || P <- Ps],
    lists:map(fun(CEs) ->
                lists:foldl(fun(CE, Acc) ->
                                CE0 = [scalece(Cedge, S, T) || Cedge <- CE],
                                case CE0 of
                                    [] -> Acc;
                                    _ -> [CE0|Acc]
                                end
                            end,[],CEs)
            end, Ps0).

getcedges(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops]}) ->
    getcedges(Ops,{X,Y},{X,Y},[]);
getcedges(_) -> [].

getcedges([],{X,Y},{X,Y},Acc) -> Acc;
getcedges([],Prev,{X,Y},Acc) ->		% prev != first, so close with line
    lists:reverse([#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=plineto,x1=X,y1=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X,y3=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,cp1={X1,Y1},cp2={X2,Y2},ve={X,Y}}|Acc]);
getcedges([_|_],_,_,_) ->
    [].	% funny path (probably moveto in middle), so return nothing

scalece(#cedge{vs={Xs,Ys},cp1=nil,cp2=nil,ve={Xe,Ye}},{XS,YS},{XT,YT}) ->
    #cedge{vs={(Xs+XT)*XS,(Ys+YT)*YS},cp1=nil,cp2=nil,ve={(Xe+XT)*XS,(Ye+YT)*YS}};
scalece(#cedge{vs={Xs,Ys},cp1={X1,Y1},cp2={X2,Y2},ve={Xe,Ye}},{XS,YS},{XT,YT}) ->
    #cedge{vs={(Xs+XT)*XS,(Ys+YT)*YS},cp1={(X1+XT)*XS,(Y1+YT)*YS},cp2={(X2+XT)*XS,(Y2+YT)*YS},ve={(Xe+XT)*XS,(Ye+YT)*YS}}.
