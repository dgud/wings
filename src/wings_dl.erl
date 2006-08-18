%%
%%  wings_dl.erl --
%%
%%     Manage display lists for objects in Geometry and AutoUV windows
%%     (providing "garbage collection" of display lists).
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_dl.erl,v 1.3 2004/12/16 20:05:09 bjorng Exp $
%%

-module(wings_dl).

-export([init/0,delete_dlists/0,
	 update/2,map/2,fold/2,changed_materials/1,
	 display_lists/0,
	 call/1,mirror_matrix/1]).

%%% This module manages display lists for all objects in a Geometry
%%% or AutoUV window.
%%%
%%% The major data structure of this module is a list of #dlo{}
%%% records. There is one #dlo{} record for each visible object
%%% (hidden objects have no #dlo{} record in the list). The #dlo{}
%%% records holds several different type of display lists for
%%% the corresponding object (e.g. a work display list for faces
%%% in smooth mode), and also other needed information (e.g. the
%%% #we{} record for the object).
%%%
%%% The update/2 and map/2 functions are used for updating #dlo{}
%%% records. The update/2 function supports deletion of records
%%% (for objects that has been deleted or hidden) and addition of
%%% new records (for newly added objects), while the map/2 function
%%% only allows modification of already existing #dlo{} records.
%%%
%%% Both functions automatically deletes any display lists that
%%% are no longer referenced by any #dlo{} record ("garbage collection").
%%%
%%% The fold/2 function can be used for folding over all #dlo{}
%%% records to collect information. The callback fun called by fold/2
%%% is not allowed to do any changes to any #dlo{} entries.

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foreach/2,foldl/3]).

-record(du,
	{dl=[],					%Display list records.
	 mat=gb_trees:empty(),			%Materials.
	 used=[]				%Display lists in use.
	 }).

init() ->
    Dl = case get_dl_data() of
	     undefined -> [];
	     #du{dl=Dl0,used=Used} ->
		 ?CHECK_ERROR(),
		 foreach(fun(DL) -> gl:deleteLists(DL, 1) end, Used),
		 gl:getError(),			%Clear error.
		 clear_old_dl(Dl0)
	 end,
    put_dl_data(#du{dl=Dl}).

display_lists() ->
    #du{dl=Dls} = get_dl_data(),
    Dls.

%% changed_materials(St)
%%  Get a list of all materials that were changed since the last time
%%  the display lists were updated. (The list does not include materials
%%  that were deleted or added.)

changed_materials(#st{mat=NewMat}) ->
    case get_dl_data() of
	#du{mat=NewMat} -> [];
	#du{mat=OldMat}=Du ->
	    put_dl_data(Du#du{mat=NewMat}),
	    changed_materials_1(gb_trees:to_list(OldMat), NewMat, [])
    end.

%% update(CallbackFun, Data0)
%%  Update existing #dlo{} records and/or delete or add new #dlo{}
%%  records. Data0 is any Erlang term. The callback Fun will be
%%  called like this
%%      CallbackFun(#dlo{}, DataN)
%%  for each #dlo{} entry in the list kept by this module.
%%
%%  Allowed return values are:
%%    #dlo{}          Updated #dlo{} record; Data not updated.
%%    {#dlo{},Data}   Update #dlo{} record and Data.
%%    deleted         The object has been deleted; Data not updated.
%%    {deleted,Data}  The object has been deleted; new value of Data.
%%
%%  When the callback fun has been called once for each #dlo{} record,
%%  it will be called at least once more like this:
%%
%%    CallbackFun(eol, DataN)
%%
%%  Allowed return values are:
%%  #dlo{}          Add a #dlo{} record for a new object
%%  {#dlo{},Data}   Add a #dlo{} record for a new object; updated Data.
%%  eol             Finished (the callback fun will not be called again).

update(Fun, Data) ->
    #du{dl=Dlists} = get_dl_data(),
    update_1(Fun, Dlists, Data, [], []).

%% map(CallbackFun, Data0)
%%  Map over existing #dlo{} records and update them.
%%  Data0 is any Erlang term. The callback Fun will be
%%  called like this
%%      CallbackFun(#dlo{}, DataN)
%%  for each #dlo{} entry in the list kept by this module.
%%
%%  Allowed return values are:
%%    #dlo{}          Updated #dlo{} record; Data not updated.
%%    {#dlo{},Data}   Update #dlo{} record and Data.

map(Fun, Data) ->
    case get_dl_data() of
	undefined -> ok;
	#du{dl=Dlists} ->
	    map_1(Fun, Dlists, Data, [], [])
    end.

%% fold(CallbackFun, Acc0)
%%  Fold over the list of #dlo{} records. The callback will
%%  be called like this
%%    CallbackFun(#dlo{}, AccN)
%%  for each #dlo{} record. The callback fun must return
%%  the updated Acc value.

fold(Fun, Acc) ->
    #du{dl=Dlists} = get_dl_data(),
    foldl(Fun, Acc, Dlists).

%% call(DisplayListTerm)
%%  Call the OpenGL display list using gl:callList/1 for
%%  the display lists embedded in the display list term.

call(none) -> none;
call({call,Dl,_}) -> call(Dl);
call({call_in_this_win,Win,Dl}) ->
    case wings_wm:this() of
	Win -> call(Dl);
	_ -> ok
    end;
call([H|T]) -> call(H), call(T);
call([]) -> ok;
call(Dl) when is_integer(Dl) -> gl:callList(Dl).

%% mirror_matrix(Id)
%%  Return the mirror matrix for the object having id Id.

mirror_matrix(Id) -> fold(fun mirror_matrix/2, Id).

mirror_matrix(#dlo{mirror=Matrix,src_we=#we{id=Id}}, Id) -> Matrix;
mirror_matrix(_, Acc) -> Acc.

%%%
%%% Local functions.
%%%

delete_dlists() ->
    case erase(wings_wm:get_prop(display_lists)) of
	#du{used=Used} ->
	    foreach(fun(DL) -> gl:deleteLists(DL, 1) end, Used),
	    gl:getError();			%Clear error.
	_ ->
	    ok
    end.

clear_old_dl([#dlo{src_we=We,proxy_data=Pd0,ns=Ns}|T]) ->
    Pd = wings_proxy:clean(Pd0),
    [#dlo{src_we=We,mirror=none,proxy_data=Pd,ns=Ns}|clear_old_dl(T)];
clear_old_dl([]) -> [].

get_dl_data() ->
    case wings_wm:lookup_prop(display_lists) of
	none -> undefined;
	{value,DlName} -> get(DlName)
    end.

put_dl_data(Data) ->
    put(wings_wm:get_prop(display_lists), Data).

update_1(Fun, [D0|Dlists], Data0, Seen0, Acc) ->
    case Fun(D0, Data0) of
	#dlo{}=D ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, Dlists, Data0, Seen, [D|Acc]);
	deleted ->
	    update_1(Fun, Dlists, Data0, Seen0, Acc);
	{deleted,Data} ->
	    update_1(Fun, Dlists, Data, Seen0, Acc);
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, Dlists, Data, Seen, [D|Acc])
    end;
update_1(Fun, [], Data0, Seen0, Acc) ->
    case Fun(eol, Data0) of
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, [], Data, Seen, [D|Acc]);
	eol ->
	    update_last(Data0, Seen0, Acc)
    end.


changed_materials_1([{Name,Val}|T], New, Acc) ->
    case gb_trees:lookup(Name, New) of
	none -> changed_materials_1(T, New, Acc);
	{value,Val} -> changed_materials_1(T, New, Acc);
	{value,_} -> changed_materials_1(T, New, [Name|Acc])
    end;
changed_materials_1([], _, Acc) -> Acc.


map_1(Fun, [D0|Dlists], Data0, Seen0, Acc) ->
    case Fun(D0, Data0) of
	#dlo{}=D ->
	    Seen = update_seen(D, Seen0),
	    map_1(Fun, Dlists, Data0, Seen, [D|Acc]);
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    map_1(Fun, Dlists, Data, Seen, [D|Acc])
    end;
map_1(_Fun, [], Data, Seen, Acc) ->
    update_last(Data, Seen, Acc).

update_last(Data, Seen, Acc) ->
    #du{used=Used0} = Du = get_dl_data(),
    Used = ordsets:from_list(Seen),
    put_dl_data(Du#du{used=Used,dl=reverse(Acc)}),
    NotUsed = ordsets:subtract(Used0, Used),
    delete_lists(NotUsed),
    Data.

delete_lists([]) -> ok;
delete_lists([D1,D2|Dls]) when D1+1 =:= D2 ->
    gl:deleteLists(D1, 2),
    delete_lists(Dls);
delete_lists([Dl|Dls]) ->
    gl:deleteLists(Dl, 1),
    delete_lists(Dls).
    
update_seen(D, Seen) ->
    update_seen_0(size(D), D, Seen).

update_seen_0(0, _, Seen) -> Seen;
update_seen_0(I, D, Seen0) ->
    Seen = update_seen_1(element(I, D), Seen0),
    update_seen_0(I-1, D, Seen).
    
update_seen_1([H|T], Seen) ->
    update_seen_1(T, update_seen_1(H, Seen));
update_seen_1([], Seen) -> Seen;
update_seen_1(none, Seen) -> Seen;
update_seen_1({call,Dl1,Dl2}, Seen) ->
    update_seen_1(Dl1, update_seen_1(Dl2, Seen));
update_seen_1({matrix,_,Dl}, Seen) ->
    update_seen_1(Dl, Seen);
update_seen_1(Dl, Seen) when is_integer(Dl) ->
    [Dl|Seen];
update_seen_1(_, Seen) -> Seen.
