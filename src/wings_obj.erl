%%
%%  wings_obj.erl --
%%
%%     Interface to objects.
%%
%%  Copyright (c) 2017 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_obj).

-export([new/3,
         delete/2,dfold/4,get/2,fold/3,map/2,num_objects/1,
         put/2,update/3,we_map/2,with_we/3,
         hide/2,unhide/2,lock/2,unlock/2,
         create_folder_system/1,recreate_folder_system/1, get_folder/1, set_folder/2]).

-export_type([obj/0]).

-include("wings.hrl").

-import(lists, [foldl/3,keydelete/3,keyfind/3,mapfoldl/3,
                member/2,reverse/1,reverse/2,sort/1]).

-type id() :: non_neg_integer().
-type perm() :: 0..3.

-type obj() :: #{'id' := id(),
                 'pid' := pid(),
                 'name' := string(),
                 'perm' := perm(),
                 'matrix' := term(),
                 'folder' => string(),
                 'light' => term()}.

-define(FOLDERS, wings_shape).
-define(NO_FLD, no_folder).

%%%
%%% API.
%%%

%% new(Name, We, St0) -> St.
%%  Create a new object having the given name,
%%  converting all unknown materials to default.

-spec new(Name, #we{}, #st{}) -> #st{} when
      Name :: string().

new(Name, #we{pst=Pst0}=We0, #st{shapes=Shapes0,onext=Oid,mat=Mat}=St) ->
    UsedMat = wings_facemat:used_materials(We0),
    UndefMat = [M || M <- UsedMat, not gb_trees:is_defined(M, Mat)],
    We1 = case UndefMat of
              [] ->
                  We0;
              [_|_] ->
                  FMs = [F || {F,M} <- wings_facemat:all(We0),
                              member(M, UndefMat)],
                  wings_facemat:assign(default, FMs, We0)
          end,
    Folder = case get_folder(Pst0) of
                 ?NO_FLD -> get_current_folder(St);
                 Folder0 -> Folder0
             end,
    Pst = gb_trees:enter(?FOLDERS, Folder, Pst0),
    We = We1#we{name=Name,id=Oid,pst=Pst},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.

-spec get(Id, #st{}) -> Obj when
      Id :: id(),
      Obj :: obj().

get(Id, #st{shapes=Shs}) ->
    obj_from_we(gb_trees:get(Id, Shs)).

-spec put(Obj, #st{}) -> #st{} when
      Obj :: obj().

put(#{id:=Id}=Obj, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We = we_from_obj(Obj, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.

-spec delete(Id, #st{}) -> #st{} when
      Id :: id().

delete(Id, #st{sel=Sel0,shapes=Shs0}=St) ->
    Shs = gb_trees:delete(Id, Shs0),
    Sel = keydelete(Id, 1, Sel0),
    St#st{sel=Sel,shapes=Shs}.

-spec dfold(Map, Reduce, Acc0, #st{}) -> Acc1 when
      Map :: fun((obj(), #we{}) -> Int),
      Reduce :: fun((Int, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

dfold(MF, RF, Acc0, #st{shapes=Shs0})
  when is_function(MF, 2), is_function(RF, 2) ->
    F = fun(We, A) ->
                B = MF(obj_from_we(We), We),
                RF(B, A)
        end,
    foldl(F, Acc0, gb_trees:values(Shs0)).

-spec fold(F, Acc0, St) -> Acc1 when
      F :: fun((obj(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      St :: #st{}.

fold(F, Acc0, #st{shapes=Shs}) when is_function(F, 2) ->
    foldl(fun(We, A) ->
                  F(obj_from_we(We), A)
          end, Acc0, gb_trees:values(Shs)).

-spec map(F, #st{}) -> #st{} when
      F :: fun((ObjIn) -> ObjOut),
      ObjIn :: obj(),
      ObjOut :: obj().

map(F, #st{shapes=Shs0}=St) when is_function(F, 1) ->
    Shs = gb_trees:map(fun(_, We) -> map_one(F, We) end, Shs0),
    St#st{shapes=Shs}.

-spec num_objects(#st{}) -> non_neg_integer().

num_objects(#st{shapes=Shs}) ->
    gb_trees:size(Shs).

%%%
%%% Functions that manipulate the #we{} records.
%%%

-spec with_we(F, Id, #st{}) -> Out when
      F :: fun((#we{}) -> Out),
      Out :: term(),
      Id :: id().

with_we(F, Id, #st{shapes=Shs}) when is_function(F, 1) ->
    F(gb_trees:get(Id, Shs)).

-spec we_map(F, #st{}) -> #st{} when
      F :: fun((ObjIn) -> ObjOut),
      ObjIn :: obj(),
      ObjOut :: obj().

we_map(F, #st{shapes=Shs0}=St) when is_function(F, 1) ->
    Shs = gb_trees:map(fun(_, We) -> F(We) end, Shs0),
    St#st{shapes=Shs}.

-spec update(Fun, Ids, #st{}) -> #st{} when
      Fun :: fun((#we{}) -> #we{}),
      Ids :: [id()].

update(F, Ids, #st{shapes=Shs0}=St) when is_function(F, 1) ->
    UF = fun(Id, S) ->
                 We0 = gb_trees:get(Id, S),
                 We = F(We0),
                 gb_trees:update(Id, We, S)
         end,
    Shs = foldl(UF, Shs0, Ids),
    St#st{shapes=Shs}.

%%%
%%% Hide, unhide, lock, unlock.
%%%

-spec hide(Ids, #st{}) -> #st{} when
      Ids :: [id()].

hide(Ids0, #st{selmode=Mode,shapes=Shs0,sel=Sel0}=St) ->
    Ids = sort(Ids0),
    {Shs,Sel} = hide_1(Ids, Sel0, Mode, [], Shs0),
    St#st{shapes=Shs,sel=Sel}.

-spec unhide(Ids, #st{}) -> #st{} when
      Ids :: [id()].

unhide(Ids0, #st{selmode=Mode,shapes=Shs0,sel=Sel0}=St) ->
    Ids = gb_sets:from_list(Ids0),
    Wes0 = gb_trees:values(Shs0),
    Wes1 = [We || #we{id=Id}=We <- Wes0, gb_sets:is_member(Id, Ids)],
    Sel = lists:sort(unhide_update_sel(Wes1, Mode, Sel0)),
    Wes = [unhide_we(We) || We <- Wes1],
    Shs = update_wes(Wes, Shs0),
    St#st{sel=Sel,shapes=Shs}.

-spec lock(Ids, #st{}) -> #st{} when
      Ids :: [id()].

lock(Ids0, #st{shapes=Shs0,sel=Sel0}=St) ->
    Ids = gb_sets:from_list(Ids0),
    Wes0 = gb_trees:values(Shs0),
    Wes1 = [We || #we{id=Id}=We <- Wes0, gb_sets:is_member(Id, Ids)],
    Wes = [lock_we(We) || We <- Wes1],
    Shs = update_wes(Wes, Shs0),
    Sel = [Item || {Id,_}=Item <- Sel0, not gb_sets:is_member(Id, Ids)],
    St#st{shapes=Shs,sel=Sel}.

-spec unlock(Ids, #st{}) -> #st{} when
      Ids :: [id()].

unlock(Ids0, #st{shapes=Shs0}=St) ->
    Ids = gb_sets:from_list(Ids0),
    Wes0 = gb_trees:values(Shs0),
    Wes1 = [We || #we{id=Id}=We <- Wes0, gb_sets:is_member(Id, Ids)],
    Wes = [unlock_we(We) || We <- Wes1],
    Shs = update_wes(Wes, Shs0),
    St#st{shapes=Shs}.

%%%
%%% Folder system.
%%%

-spec create_folder_system(#st{}) -> #st{}.

create_folder_system(#st{pst=Pst0}=St) ->
    case gb_trees:lookup(?FOLDERS, Pst0) of
        none ->
            Fs = [{?NO_FLD,{open,gb_sets:empty()}}],
            Pst = gb_trees:insert(?FOLDERS, {?NO_FLD,Fs}, Pst0),
            St#st{pst=Pst};
        {value,_} ->
            St
    end.

-spec recreate_folder_system(#st{}) -> #st{}.

recreate_folder_system(#st{pst=Pst0}=St) ->
    {Current,Fs0} = gb_trees:get(?FOLDERS, Pst0),
    InUse0 = orddict:fetch_keys(Fs0),
    F = fun(#{folder:=F}, A) -> [F|A] end,
    InUse1 = fold(F, InUse0, St),
    InUse = ordsets:from_list(InUse1),
    NewFolder = {open,gb_sets:empty()},
    Fs = [{Folder,NewFolder} || Folder <- InUse],
    Pst = gb_trees:update(?FOLDERS, {Current,Fs}, Pst0),
    St#st{pst=Pst}.

%%%
%%% Local functions.
%%%


map_one(F, We) ->
    Obj0 = obj_from_we(We),
    Obj = F(Obj0),
    we_from_obj(Obj, We).

we_from_obj(Obj, #we{name=Name0,perm=Perm0,pst=Pst0,light=Light0}=We) ->
    Folder0 = get_folder(Pst0),
    #{name:=Name,perm:=Perm,folder:=Folder} = Obj,
    Light = maps_get_def_none(light, Obj),
    case {Name,Perm,Folder,Light} of
        {Name0,Perm0,Folder0,Light0} ->
            We;
        {_,_,_,_} ->
            Pst = case Folder of
                      Folder0 ->
                          Pst0;
                      _ ->
                          gb_trees:enter(?FOLDERS, Folder, Pst0)
                  end,
            We#we{name=Name,perm=Perm,pst=Pst,light=Light}
    end.

maps_get_def_none(Key, Map) ->
    case Map of
        #{Key:=Val} -> Val;
        #{} -> none
    end.

obj_from_we(#we{id=Id,name=Name,perm=Perm,pst=Pst,light=Light}) ->
    Folder = get_folder(Pst),
    M = #{id => Id,
          pid => self(),
          name => Name,
          perm => Perm,
          folder => Folder,
          matrix => e3d_mat:identity()},
    case Light of
        none -> M;
        _ -> M#{light => Light}
    end.

get_folder(#we{pst=Pst}) ->
    get_folder(Pst);
get_folder(Pst) ->
    case gb_trees:lookup(?FOLDERS, Pst) of
        {value,[_|_]=Folder} -> Folder;
        _ -> ?NO_FLD
    end.

set_folder(Folder, #we{pst=Pst}=We) ->
    We#we{pst=set_folder(Folder, Pst)};
set_folder(?NO_FLD, Pst0) ->
    gb_trees:delete_any(?FOLDERS, Pst0);
set_folder(Folder, Pst0) ->
    gb_trees:enter(?FOLDERS, Folder, Pst0).

get_current_folder(#st{pst=Pst}) ->
    {Current,_} = gb_trees:get(?FOLDERS, Pst),
    Current.

hide_1([Id0|_]=Ids, [{Id1,_}=P|Sel], Mode, SelAcc, Shs) when Id0 > Id1 ->
    hide_1(Ids, Sel, Mode, [P|SelAcc], Shs);
hide_1([Id|Ids], [{Id,Items}|Sel], Mode, SelAcc, Shs0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = We0#we{perm={Mode,Items}},
    Shs = gb_trees:update(Id, We, Shs0),
    hide_1(Ids, Sel, Mode, SelAcc, Shs);
hide_1([Id|Ids], Sel, Mode, SelAcc, Shs0) ->
    case gb_trees:get(Id, Shs0) of
        #we{perm=P0}=We0 when is_integer(P0) ->
            We = We0#we{perm=P0 bor ?PERM_HIDDEN_BIT},
            Shs = gb_trees:update(Id, We, Shs0),
            hide_1(Ids, Sel, Mode, SelAcc, Shs);
        #we{} ->
            %% Already hidden, with saved selection.
            hide_1(Ids, Sel, Mode, SelAcc, Shs0)
    end;
hide_1([], Sel, _Mode, SelAcc, Shs) ->
    {Shs,reverse(SelAcc, Sel)}.

unhide_update_sel([#we{id=Id,perm={Mode,Items}}|T], Mode, Acc) ->
    unhide_update_sel(T, Mode, [{Id,Items}|Acc]);
unhide_update_sel([#we{id=Id,perm={_,_}=Sel}=We|T], Mode, Acc) ->
    Items = wings_sel_conv:mode(Mode, Sel, We),
    unhide_update_sel(T, Mode, [{Id,Items}|Acc]);
unhide_update_sel([_|T], Mode, Acc) ->
    unhide_update_sel(T, Mode, Acc);
unhide_update_sel([], _, Acc) -> reverse(Acc).

unhide_we(#we{perm=Perm0}=We) when is_integer(Perm0) ->
    Perm = Perm0 band (bnot ?PERM_HIDDEN_BIT),
    We#we{perm=Perm};
unhide_we(#we{}=We) ->
    We#we{perm=0}.

unlock_we(#we{perm=Perm0}=We) when ?IS_VISIBLE(Perm0) ->
    We#we{perm=0};
unlock_we(#we{}=We) ->
    We.

lock_we(#we{perm=Perm0}=We) when ?IS_VISIBLE(Perm0) ->
    We#we{perm=?PERM_LOCKED_BIT};
lock_we(#we{}=We) ->
    We.

update_wes(Wes, Shs0) ->
    case length(Wes) =:= gb_trees:size(Shs0) of
        false ->
            foldl(fun(#we{id=Id}=We, S) ->
                          gb_trees:update(Id, We, S)
                  end, Shs0, Wes);
        true ->
            Shs = [{Id,We} || #we{id=Id}=We <- Wes],
            gb_trees:from_orddict(Shs)
    end.
