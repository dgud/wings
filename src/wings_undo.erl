%%
%%  wings_undo.erl --
%%
%%     This module handles the undo stack.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_undo).
-export([init/1,save/2,undo_toggle/1,undo/1,redo/1,info/1]).

%% For the Develop menu.
-export([mem_stat_help/0]).

-include("wings.hrl").

-import(lists, [reverse/1,reverse/2]).

-type elem_num() :: wings_vertex:vertex_num()
                  | wings_edge:edge_num()
                  | wings_face:face_num().

%% Develop info.
-record(info,
	{change,
	 sz_diff,
	 own_size
	}).
	 
%% The essential part of the state record.
-record(est,
	{shapes=[] :: list(#we{}) | gb_trees:tree(),
	 selmode=face :: wings_sel:mode(),
	 sel=[] :: list(),
	 onext=1 :: elem_num(),
	 mat=wings_material:default(),
	 pst=gb_trees:empty() :: gb_trees:tree(),

	 %% For the Develop menu.
	 cmd,
	 info :: #info{} | undefined
	}).

init(St) ->
    St#st{undo=queue:new(),undone=[],next_is_undo=true}.
    
save(OldState, St0) ->
    St1 = discard_old_states(St0),
    St = push(St1, OldState),
    mem_stat(St#st{undone=[],next_is_undo=true}).

undo_toggle(#st{next_is_undo=true}=St) -> undo(St);
undo_toggle(St) -> redo(St).

undo(#st{undone=Undone}=St0) ->
    case pop(St0) of
	empty -> St0;
	St -> mem_stat(St#st{undone=[St0|Undone],next_is_undo=false})
    end.

redo(#st{undone=[StOld|Undone]}=St0) ->
    St1 = push(St0, St0),
    #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,
	mat=Mat,last_cmd=Cmd,pst=Pst} = StOld,
    St = St1#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,
		mat=Mat,last_cmd=Cmd,pst=Pst},
    mem_stat(St#st{undone=Undone,next_is_undo=true});
redo(St) -> St.

info(#st{undo=Undo,undone=Undone}) ->
    {queue:len(Undo),length(Undone)}.

%%
%% Low-level queue operations.
%%

push(St, OldState) ->
    Est = save_essential(OldState),
    push_1(St, Est).

push_1(#st{undo=Undo0}=St, #est{sel=Sel}=Est0) ->
    case queue:is_empty(Undo0) of
	true ->
	    Est = compress(#est{shapes=[]}, Est0),
	    St#st{undo=queue:in(Est, Undo0)};
	false ->
	    PrevEst = queue:get_r(Undo0),
	    Est = compress(PrevEst, Est0),
	    case compare_states(PrevEst, Est) of
		new ->
		    St#st{undo=queue:in(Est, Undo0)};
		new_sel ->
		    Undo1 = queue:drop_r(Undo0),
		    Undo = queue:in(PrevEst#est{sel=Sel}, Undo1),
		    St#st{undo=Undo};
		unchanged ->
		    St
	    end
    end.

save_essential(#st{last_cmd=Cmd,shapes=Sh,selmode=Mode,sel=Sel,
		   onext=Onext,mat=Mat,pst=Pst}) ->
    #est{cmd=Cmd,shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat,pst=Pst}.
    
compare_states(Old, New) ->
    #est{shapes=Osh,selmode=Omode,sel=Osel,onext=Oonext,mat=Omat,pst=Opst} = Old,
    #est{shapes=Nsh,selmode=Nmode,sel=Nsel,onext=Nonext,mat=Nmat,pst=Npst} = New,
    if
	Omode =/= Nmode, (Osel =/= [] orelse Nsel =/= []) -> new;
	Oonext =/= Nonext -> new;
	Omat =/= Nmat -> new;
	Osel =/= Nsel ->
	    if
		Osh =:= Nsh -> new_sel;
		true -> new
	    end;
	Osh =/= Nsh -> new;
	Opst =/= Npst -> new;
	true -> unchanged
    end.

compress(#est{shapes=OldShapes}, #est{shapes=NewShapes}=Est) ->
    Shapes = compress_1(OldShapes, gb_trees:values(NewShapes), []),
    Est#est{shapes=Shapes}.

compress_1([#we{id=OldId}|OldWes], [#we{id=NewId}|_]=NewWes, Acc)
  when OldId < NewId ->
    compress_1(OldWes, NewWes, Acc);
compress_1([#we{id=OldId}|OldWes], [#we{id=NewId}=NewWe|NewWes], Acc)
  when OldId > NewId ->
    We = NewWe#we{fs=undefined,vc=undefined},
    compress_1(OldWes, NewWes, [We|Acc]);
compress_1([#we{}=OldWe|OldWes], [#we{}=NewWe|NewWes], Acc) ->
    case NewWe#we{fs=undefined,vc=undefined} of
	OldWe -> compress_1(OldWes, NewWes, [OldWe|Acc]);
	We -> compress_1(OldWes, NewWes, [We|Acc])
    end;
compress_1(_, [We0|NewWes], Acc) ->
    We = We0#we{fs=undefined,vc=undefined},
    compress_1([], NewWes, [We|Acc]);
compress_1(_, [], Acc) -> reverse(Acc).

pop(#st{undo=Undo0}=St) ->
    case queue:out_r(Undo0) of
	{empty,_} -> empty;
	{{value,Est},Undo} ->
	    #est{shapes=Sh0,selmode=Mode,sel=Sel,
		 onext=Onext,mat=Mat,cmd=Cmd,pst=Pst} = Est,
	    Sh = uncompress(Sh0, []),
	    St#st{undo=Undo,shapes=Sh,selmode=Mode,sel=Sel,
		  onext=Onext,mat=Mat,last_cmd=Cmd,pst=Pst}
    end.

discard_old_states(#st{undo=Undo0}=St) ->
    case 1 + queue:len(Undo0) - wings_pref:get_value(num_undo_levels) of
	N when N > 0 ->
	    Undo = discard_old_states_1(N, Undo0),
	    St#st{undo=Undo};
	_ -> St
    end.

discard_old_states_1(0, St) -> St;
discard_old_states_1(N, Undo) ->
    discard_old_states_1(N-1, queue:drop(Undo)).

uncompress([#we{id=Id}=We0|Wes], Acc) ->
    We = wings_we:fast_rebuild(We0),
    uncompress(Wes, [{Id,We}|Acc]);
uncompress([], Acc) -> gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Statistics on how much memory the Undo states occupy.
%%%

mem_stat(St) ->
    case wings_pref:get_value(develop_undo_stat) of
	false -> St;
	true -> mem_stat_1(St)
    end.

mem_stat_1(#st{undo=Undo0}=St) ->
    Undo = queue:in_r(#est{}, Undo0),
    Est0 = save_essential(St),
    Est = compress(queue:get_r(Undo), Est0),
    Stat = mem_stat_2([Est|reverse(queue:to_list(Undo))]),
    print_stat(Stat),
    St#st{undo=queue:from_list(reverse(tl(Stat)))}.

mem_stat_2([#est{info=undefined}=S1|[S2|_]=Ss]) ->
    Total = erts_debug:size({S1#est{info=undefined},S2#est{info=undefined}}),
    OwnSz = erts_debug:size(S1#est{info=undefined}),
    OtherSz = erts_debug:size(S2#est{info=undefined}),
    Change = change_type(S2, S1),
    Diff = Total - OtherSz - erts_debug:size({a,b}),
    Info = #info{change=Change,sz_diff=Diff,own_size=OwnSz},
    [S1#est{info=Info}|mem_stat_2(Ss)];
mem_stat_2([#est{}=S1|[_|_]=Ss]) ->
    [S1|mem_stat_2(Ss)];
mem_stat_2([_]) -> [].

change_type(#est{shapes=Obj1,selmode=Mode1,sel=Sel1,onext=N1,mat=Mat1},
	    #est{shapes=Obj2,selmode=Mode2,sel=Sel2,onext=N2,mat=Mat2}) ->
    change_type_1(cmp(Mode1, Mode2), cmp(Sel1, Sel2),
		  cmp(Obj1, Obj2), cmp(Mat1, Mat2), cmp(N1, N2)).

cmp(T, T) -> [T];
cmp(T1, T2) -> {T1,T2}.

change_type_1([_], [_], [_], [_], {_,_}) ->
    "next changed";
change_type_1(Mode, Sel, Objs, Mat, _Next) ->
    string:join([S || S <- [mode_sel_change(Mode, Sel),
			    obj_change(Objs),
			    mat_change(Mat)],
		      S =/= []], "; ").

mode_sel_change({Old,New}, Sel) when is_atom(Old), is_atom(New) ->
    C = atom_to_list(Old) ++ " => " ++ atom_to_list(New),
    case Sel of
	[_] -> C;
	{_,_} -> C ++ " (selection converted)"
    end;
mode_sel_change([_], {_,_}) ->
    "selection changed";
mode_sel_change([_], [_]) -> "".

obj_change([_]) -> "";
obj_change({O1,O2}) ->
    {New,Del,Ch0} = obj_change_1(O1, O2, 0, 0, []),
    N = fmt_cnt(New, "added"),
    D = fmt_cnt(Del, "deleted"),
    Ch = we_changes(Ch0),
    string:join([S || S <- [N,D,Ch], S =/= []], "; ").

obj_change_1([Obj|T1], [Obj|T2], New, Del, Ch) ->
    obj_change_1(T1, T2, New, Del, Ch);
obj_change_1([#we{id=Id1}|T1], [#we{id=Id2}|_]=T2, New, Del, Ch) when Id1 < Id2 ->
    obj_change_1(T1, T2, New+1, Del, Ch);
obj_change_1([#we{id=Id1}|_]=T1, [#we{id=Id2}|T2], New, Del, Ch) when Id1 > Id2 ->
    obj_change_1(T1, T2, New, Del+1, Ch);
obj_change_1([#we{id=Id}=We1|T1], [#we{id=Id}=We2|T2], New, Del, Ch) ->
    obj_change_1(T1, T2, New, Del, [{We1,We2}|Ch]);
obj_change_1([_|T], [], New, Del, Ch) ->
    {New,Del+1+length(T),Ch};
obj_change_1([], [_|T], New, Del, Ch) ->
    {New+1+length(T),Del,Ch};
obj_change_1([], [], New, Del, Ch) ->
    {New,Del,Ch}.

fmt_cnt(0, _) -> "";
fmt_cnt(1, Action) -> "1 object " ++ Action;
fmt_cnt(N, Action) ->  integer_to_list(N) ++ " objects " ++ Action.

we_changes([]) -> "";
we_changes([{#we{}=We1,#we{}=We2}]) ->
    Empty = array:new(),
    case We1#we{vp=Empty} =:= We2#we{vp=Empty} of
	true ->
	    "vertices moved";
	false ->
	    Cs = we_change(2, We1, We2),
	    "changes in: " ++ non_empty_join(Cs, ", ")
    end;
we_changes(Changes) ->
    integer_to_list(length(Changes)) ++ " objects updated".

we_change(I, We1, We2) when I =< tuple_size(We1) ->
    case element(I, We1) =:= element(I, We2) of
	false ->
	    WeMap = list_to_tuple(record_info(fields, we)),
	    Desc = atom_to_list(element(I-1, WeMap)),
	    [Desc|we_change(I+1, We1, We2)];
	true ->
	    we_change(I+1, We1, We2)
    end;
we_change(_, _, _) -> [].

mat_change([_]) -> "";
mat_change({_,_}) -> "materials changed".

non_empty_join(List, Sep) ->
    string:join([S || S <- List, S =/= []], Sep).

-define(FORMAT, "~9s ~7s  ~20s  ~s\n").

print_stat(L) ->
    io:format(?FORMAT, ["Size","Cost","Command","Change"]),
    io:format(?FORMAT, ["----","----","-------","------"]),
    [do_print_stat(E) || E <- L],
    io:nl().

do_print_stat(#est{info=#info{change=Change,sz_diff=Diff,own_size=OwnSz},
		   cmd=Cmd}) ->
    io:format(?FORMAT, [integer_to_list(OwnSz),
			integer_to_list(Diff),
			format_cmd(Cmd),Change]).

format_cmd(Cmd) -> wings_util:stringify(Cmd).
    
mem_stat_help() ->
    S = ["Explanation of the columns for Undo stat:\n",
	 "\n",
	 "Size     Size in words for this undo state.\n",
	 "Cost     Relative difference in size (in words) compared to \n",
	 "         the next undo state.\n",
	 "Command  The name of the command that generated this undo state.\n",
	 "Change   Summary of the changes compared to the next undo state.\n",
	 "\n"],
    io:put_chars(S).
    

