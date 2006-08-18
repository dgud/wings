%%
%%  wings_undo.erl --
%%
%%     This module handles the undo stack.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_undo.erl,v 1.12 2004/03/30 15:56:03 bjorng Exp $
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
    St#st{top=[],bottom=[],undone=[],next_is_undo=true}.
    
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

info(#st{top=Top,bottom=Bot,undone=Undone}) ->
    {length(Top)+length(Bot),length(Undone)}.

%%
%% Low-level queue operations.
%%

push(St, OldState) ->
    Est = save_essential(OldState),
    push_1(St, Est).

push_1(#st{top=[],bottom=[_|_]=Bottom}=St, #est{}=Est) ->
    push_1(St#st{top=reverse(Bottom),bottom=[]}, Est);
push_1(#st{top=[],bottom=[]}=St, #est{}=Est0) ->
    Est = compress(#est{shapes=[]}, Est0),
    St#st{top=[Est]};
push_1(#st{top=[PrevEst|PrevTop]=Top}=St, #est{sel=Sel}=Est0) ->
    Est = compress(PrevEst, Est0),
    case compare_states(PrevEst, Est) of
	new ->
	    St#st{top=[Est|Top]};
	new_sel ->
	    St#st{top=[PrevEst#est{sel=Sel}|PrevTop]}
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

pop(#st{top=[Est|Top]}=St0) ->
    #est{shapes=Sh0,selmode=Mode,sel=Sel,onext=Onext,mat=Mat} = Est,
    Sh = uncompress(Sh0, []),
    St = St0#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext,mat=Mat},
    St#st{top=Top};
pop(#st{top=[],bottom=[_|_]=Bottom}=St) ->
    pop(St#st{top=reverse(Bottom),bottom=[]});
pop(_) -> empty.

discard_old_states(#st{top=Top,bottom=Bot}=St) ->
    case 1 + length(Top) + length(Bot) -
	wings_pref:get_value(num_undo_levels) of
	N when N > 0 -> discard_old_states_1(N, St);
	_ -> St
    end.

discard_old_states_1(0, St) -> St;
discard_old_states_1(N, #st{bottom=[_|Bottom]}=St) ->
    discard_old_states_1(N-1, St#st{bottom=Bottom});
discard_old_states_1(N, #st{bottom=[],top=[_|_]=Top}=St) ->
    discard_old_states_1(N, St#st{bottom=reverse(Top),top=[]}).

uncompress([#we{id=Id,next_id=Next}=We0|Wes], Acc) ->
    We = wings_we:rebuild(We0),
    uncompress(Wes, [{Id,We#we{next_id=Next}}|Acc]);
uncompress([], Acc) -> gb_trees:from_orddict(reverse(Acc)).
