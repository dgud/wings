%%
%%  wings_dl.erl --
%%
%%     Manage Vertex Buffer Objects (and display lists for objects in
%%     Geometry and AutoUV windows (providing "garbage collection" of
%%     display lists).
%%
%%  Copyright (c) 2001-2015 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_dl).

-export([init/0,delete_dlists/0,
	 update/2,map/2,fold/2,changed_materials/1,
	 display_lists/0,
	 call/2,mirror_matrix/1,draw/4]).
-export_type([dl/0,real_dl/0,sel_dl/0]).

%%% This module manages Vertex Buffer Objects (VBOs, represented by
%%% #vab{} records) for all objects in a Geometry or AutoUV window.
%%%
%%% The major data structure of this module is a list of #dlo{}
%%% records. There is one #dlo{} record for each visible object
%%% (hidden objects have no #dlo{} record in the list). The #dlo{}
%%% record holds #vab{} records for rendering different aspects of the
%%% corresponding object, and also other needed information (e.g. the
%%% #we{} record for the object).
%%%
%%% The update/2 and map/2 functions are used for updating #dlo{}
%%% records. The update/2 function supports deletion of records
%%% (for objects that has been deleted or hidden) and addition of
%%% new records (for newly added objects), while the map/2 function
%%% only allows modification of already existing #dlo{} records.
%%%
%%% Both functions automatically deletes VBOs that are no longer
%%% referenced by any #dlo{} record ("garbage collection").
%%%
%%% The fold/2 function can be used for folding over all #dlo{}
%%% records to collect information. The callback fun called by fold/2
%%% is not allowed to do any changes to any #dlo{} entries.

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foreach/2,foldl/3]).

-type vbo() :: {'vbo',non_neg_integer()}.
-type draw_fun() :: fun((rs()) -> rs()).
-type real_dl() :: draw_fun()
                 | {'call',draw_fun()|'none',vab()|vbo()}
                 | {'call_in_this_win',_,draw_fun()}
                 | [real_dl()].
-type dl() :: 'none' | real_dl().
-type sel_dl() :: 'none' | vab() | vbo() | draw_fun()
                | {'call',draw_fun()|'none',vab()|vbo()}.

-type rs() :: map().

-record(du,
	{dl=[],					%Display list records.
	 mat=gb_trees:empty(),			%Materials.
	 used=[],				%Display lists in use.
	 extra=#{}
	 }).

init() ->
    Dl = case get_dl_data() of
	     undefined -> [];
	     #du{dl=Dl0,used=Used, extra=Extra} ->
		 ?CHECK_ERROR(),
                 All = [update_seen_1(DA, []) ||
                           {_, {_, DA}} <- maps:to_list(Extra)],
                 delete_buffers(ordsets:from_list(lists:append(All)), ?FUNCTION_NAME),
		 delete_buffers(Used, ?FUNCTION_NAME),
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

%% call(Term, RenderState) -> RenderState.
%%  Call OpenGL to render the geometry using the VBOs embedded in
%%  the term.

-spec call(dl(), rs()) -> rs().

call(none, State) -> State;
call({call,Dl,_}, State) ->
    %%    erlang:display({call, element(2, _Vbo)}),
    call(Dl, State);
call({call_in_this_win,Win,Dl}, State) ->
    case wings_wm:this() of
	Win -> call(Dl, State);
	_ -> State
    end;
call([H|T], State0) ->
    State = call(H, State0),
    call(T, State);
call([], State) ->
    State;
call(Draw, #{}=State) when is_function(Draw, 1) ->
    #{}=Draw(State).

%% mirror_matrix(Id)
%%  Return the mirror matrix for the object having id Id.

mirror_matrix(Id) -> fold(fun mirror_matrix/2, Id).

mirror_matrix(#dlo{mirror=Matrix,src_we=#we{id=Id}}, Id) -> Matrix;
mirror_matrix(_, Acc) -> Acc.


%% draw(Category, Key, Update, RS) -> ok.
%%  Draw a non-object graphic thing by calling:
%%
%%    call(Update(Key))
%%
%%  Typically, Update/1 should return the result from
%%  wings_vbo:new/[2,3]. The return value from Update/1 will be
%%  saved and reused if draw/3 is kalled with the same Key within
%%  a category.
%%
%%  The key 'none' is specially handled. It means that nothing
%%  should be drawn and that Update/1 will not be called.

draw(Category, Key, Update, RS)
  when is_atom(Category), is_function(Update, 1) ->
    case get_dl_data() of
	#du{extra=#{Category:={Key,Drawable}}} ->
	    call(Drawable, RS);
	#du{extra=#{Category:={_,OldDrawable}}=Extra0}=Du ->
	    NotUsed = ordsets:from_list(update_seen_1(OldDrawable, [])),
	    delete_buffers(NotUsed, ?FUNCTION_NAME),
	    case Key of
		none ->
		    Extra = maps:remove(Category, Extra0),
		    put_dl_data(Du#du{extra=Extra}),
                    RS;
		_ ->
		    Drawable = Update(Key),
		    Extra = Extra0#{Category:={Key,Drawable}},
		    put_dl_data(Du#du{extra=Extra}),
		    call(Drawable, RS)
	    end;
	#du{extra=Extra0}=Du ->
	    case Key of
		none ->
		    RS;
		_ ->
		    Drawable = Update(Key),
		    Extra = Extra0#{Category=>{Key,Drawable}},
		    put_dl_data(Du#du{extra=Extra}),
		    call(Drawable, RS)
	    end
    end.

%%%
%%% Local functions.
%%%

delete_dlists() ->
    case get_dl_data() of
	#du{used=Used} ->
	    delete_buffers(Used, ?FUNCTION_NAME),
            erase(wings_wm:get_dd()),
	    gl:getError();			%Clear error.
	_ ->
	    ok
    end.

clear_old_dl([#dlo{src_we=We,proxy_data=Pd0,ns=Ns}|T]) ->
    Pd = wings_proxy:invalidate(Pd0, vab),
    [#dlo{src_we=We,mirror=none,proxy_data=Pd,ns=Ns}|clear_old_dl(T)];
clear_old_dl([]) -> [].

get_dl_data() ->
    case wings_wm:get_dd() of
	none -> undefined;
	DlName -> get(DlName)
    end.

put_dl_data(Data) ->
    put(wings_wm:get_dd(), Data).

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
    case ordsets:subtract(Used0, Used) of
	[] ->
	    ok;
	[_|_]=NotUsed ->
	    delete_buffers(NotUsed, ?FUNCTION_NAME)
    end,
    Data.

update_seen(#dlo{plugins=Plugins}=D, Seen0) ->
    Seen = update_seen_1([V || {_,V} <- Plugins], Seen0),
    update_seen_0(tuple_size(D), D, Seen).

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
update_seen_1(#vab{id=Id,face_sn={vbo,_}=Vbo}, Seen) ->
    update_seen_1(Vbo, [Id|Seen]);
update_seen_1(#vab{id=Id}, Seen) ->
    [Id|Seen];
update_seen_1({vbo,Id}, Seen) ->
    [Id|Seen];
update_seen_1(Dl, Seen) when is_tuple(Dl), element(1, Dl) =:= sp ->
    %% Proxy DL's
    update_seen_0(tuple_size(Dl), Dl, Seen);
update_seen_1(_, Seen) -> Seen.


delete_buffers(NotUsed, _From) ->
    %% erlang:display({delete,{?MODULE, _From}, NotUsed}),
    gl:deleteBuffers(NotUsed).
