%%
%%  wings_undo.erl --
%%
%%     This module handles the undo stack.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_undo).
-export([init/1,save/2,undo_toggle/1,undo/1,redo/1,info/1]).

-include("wings.hrl").

-import(lists, [reverse/1,reverse/2]).

%% The essential part of the state record.
-record(est,
	{shapes,
	 selmode,
	 sel,
	 onext,
	 mat
	}).

init(St) ->
    St#st{undo=queue:new(),undone=[],next_is_undo=true}.
    
save(OldState, St0) ->
    St1 = discard_old_states(St0),
    St = push(St1, OldState),
    St#st{undone=[],next_is_undo=true}.

undo_toggle(#st{next_is_undo=true}=St) -> undo(St);
undo_toggle(St) -> redo(St).

undo(#st{undone=Undone}=St0) ->
    case pop(St0) of
	empty -> St0;
	St -> St#st{undone=[St0|Undone],next_is_undo=false}
    end.

redo(#st{undone=[StOld|Undone]}=St0) ->
    St1 = push(St0, St0),
    #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat} = StOld,
    St = St1#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat},
    St#st{undone=Undone,next_is_undo=true};
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

save_essential(#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat}) ->
    #est{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat}.
    
compare_states(Old, New) ->
    #est{shapes=Osh,selmode=Omode,sel=Osel,onext=Oonext,mat=Omat} = Old,
    #est{shapes=Nsh,selmode=Nmode,sel=Nsel,onext=Nonext,mat=Nmat} = New,
    if
	Omode =/= Nmode -> new;
	Oonext =/= Nonext -> new;
	Omat =/= Nmat -> new;
	Osel =/= Nsel ->
	    if
		Osh =:= Nsh -> new_sel;
		true -> new
	    end;
	true -> new
    end.

compress(#est{shapes=OldShapes}, #est{shapes=NewShapes}=Est) ->
    Shapes = compress_1(OldShapes, gb_trees:values(NewShapes), []),
    Est#est{shapes=Shapes}.

compress_1([#we{id=OldId}|OldWes], [#we{id=NewId}|_]=NewWes, Acc) when OldId < NewId ->
    compress_1(OldWes, NewWes, Acc);
compress_1([#we{id=OldId}|OldWes], [#we{id=NewId}=NewWe|NewWes], Acc) when OldId > NewId ->
    We = NewWe#we{fs=undefined,vc=undefined},
    compress_1(OldWes, NewWes, [We|Acc]);
compress_1([#we{}=OldWe|OldWes], [#we{}=NewWe|NewWes], Acc) ->
    case NewWe#we{fs=undefined,vc=undefined} of
	OldWe -> compress_1(OldWes, NewWes, [OldWe|Acc]);
	We -> compress_1(OldWes, NewWes, [We|Acc])
    end;
compress_1(_, NewWes, Acc) ->
    reverse(Acc, NewWes).

pop(#st{undo=Undo0}=St) ->
    case queue:out_r(Undo0) of
	{empty,_} -> empty;
	{{value,Est},Undo} ->
	    #est{shapes=Sh0,selmode=Mode,sel=Sel,onext=Onext,mat=Mat} = Est,
	    Sh = uncompress(Sh0, []),
	    St#st{undo=Undo,shapes=Sh,selmode=Mode,sel=Sel,
		  onext=Onext,mat=Mat}
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

uncompress([#we{id=Id,next_id=Next}=We0|Wes], Acc) ->
    We = wings_we:rebuild(We0),
    uncompress(Wes, [{Id,We#we{next_id=Next}}|Acc]);
uncompress([], Acc) -> gb_trees:from_orddict(reverse(Acc)).
