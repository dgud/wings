%%
%%  wpc_magnet_mask.erl --
%%
%%     Plugin for locking vertices against the influence of magnets.
%%     This plugin uses #dlo.plugin to store its drawlist and the #we.pst to
%%     store vextex data.  The vertex data is renumerbered when the objects are
%%     merged or saved.
%%
%%  Copyright (c) 2009-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_magnet_mask).

-export([init/0,menu/2,command/2]).
-export([update_dlist/3,draw/5,get_locked_vs/1,get_data/3,merge_we/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").

init() ->
    wings_pref:set_default(show_magnet_mask,true),
    wings_pref:set_default(magnet_mask_on,true),
    true.

menu({tools},Menu) ->
    Menu ++ tools_menu_entry();
menu({view,show},Menu) ->
    Menu ++ view_menu_entry();
menu(_,Menu) -> Menu.

tools_menu_entry() ->
    Mask = wings_pref:get_value(magnet_mask_on),
    [{?__(1,"Magnet Mask"),{magnet_mask,
      [{?__(2,"Lock"),mask,?__(6,"Lock selection against the influence of magnets")},
       {?__(3,"Unlock"),unmask,?__(7,"Unlock any locked elements in the selection")},
       separator,
       {?__(11,"Select"),select,?__(12,"Add locked elements to current selection")},
       {?__(9,"Deselect"),deselect,?__(10,"Subtract locked elements from current selection")},
       {?__(4,"Invert"),invert_masked,?__(8,"Invert the locked and unlocked elements")},
       separator,
       mask_on_off(Mask)]}}].

mask_on_off(true) ->
    {?__(1,"Switch Masking Off"),magnet_mask_on,?__(2,"Toggle masking On/Off")};
mask_on_off(false) ->
    {?__(3,"Switch Masking On"),magnet_mask_on,?__(2,"Toggle masking On/Off")}.

view_menu_entry() ->
    [{?__(1,"Show Magnet Mask"),show_magnet_mask,
      ?__(2,"Show the magnet mask"),crossmark(show_magnet_mask)}].

crossmark(Key) ->
    wings_menu_util:crossmark(Key).

command({tools,{magnet_mask,magnet_mask_on}},St) ->
    Bool = wings_pref:get_value(magnet_mask_on),
    wings_pref:set_value(magnet_mask_on,not Bool),
    St;
command({tools,{magnet_mask,Type}},St) ->
    {save_state,locking(Type,St)};
command({view,{show,show_magnet_mask}}, St) ->
    Bool = wings_pref:get_value(show_magnet_mask),
    wings_pref:set_value(show_magnet_mask, not Bool),
    St;
command(_,_) ->
    next.

%%%% Some temp selection
locking(Type, #st{sel=[]}=St0) when Type =:= mask; Type =:= unmask ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
      {add,_,TempSt} ->
          #st{shapes=Shs} = locking_1(Type, TempSt),
          St0#st{shapes=Shs,sel=[]};
      none -> locking_1(Type, St0)
    end;
locking(Type, St) ->
    locking_1(Type, St).

%%%% Lock or unlock selected vertices storing those locked in the pst
locking_1(mask, #st{selmode=Selmode}=St) ->
    wings_sel:map(fun
           (Sel, #we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Vertices = case Selmode =:= vertex of
                false -> gb_sets:from_list(convert_to_vs(Selmode,Sel,We));
                true -> Sel
              end,
              Locked = gb_sets:union(Lvs, Vertices),
              NewPst = set_locked_vs(Locked,Pst),
              We#we{pst=NewPst}
          end, St);
locking_1(unmask, #st{selmode=Selmode}=St) ->
    wings_sel:map(fun
           (Sel, #we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Vertices = case Selmode =:= vertex of
                false -> gb_sets:from_list(convert_to_vs(Selmode,Sel,We));
                true -> Sel
              end,
              Locked = gb_sets:difference(Lvs, Vertices),
              NewPst = set_locked_vs(Locked,Pst),
              We#we{pst=NewPst}
          end, St);

locking_1(invert_masked, #st{shapes=Shs0, sel=[]}=St) ->
    Shs1 = lists:map(fun
            (#we{id=Id,pst=Pst,perm=0}=We) ->
              Lvs = get_locked_vs(Pst),
              Diff = wings_sel:inverse_items(vertex, Lvs, We),
              NewPst = set_locked_vs(Diff,Pst),
              {Id,We#we{pst=NewPst}};
            (#we{id=Id}=We) -> {Id,We}
          end,gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    St#st{shapes=Shs};
locking_1(invert_masked, #st{}=St) ->
    wings_sel:map(fun
            (_,#we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Diff = wings_sel:inverse_items(vertex, Lvs, We),
              NewPst = set_locked_vs(Diff,Pst),
              We#we{pst=NewPst}
            end,St);

locking_1(deselect,#st{sel=[]}=St) -> St;
locking_1(select, #st{sel=[],selmode=body}=St) ->
    Sel = fun(_,#we{pst=Pst}) ->
            not gb_sets:is_empty(get_locked_vs(Pst))
          end,
    wings_sel:make(Sel,body,St);
locking_1(select, #st{sel=[],selmode=Selmode}=St) ->
    Sel = locked_vs_as_selection(Selmode),
    wings_sel:make(Sel, Selmode, St);
locking_1(Type, #st{sel=OrigSel,selmode=body}=St) ->
    Sel0 = fun(_,#we{pst=Pst}) ->
            not gb_sets:is_empty(get_locked_vs(Pst))
          end,
    #st{sel=Sel1} = wings_sel:make(Sel0,body,St),
    Sel = case Type of
      select -> union(OrigSel, Sel1);
      deselect -> subtract(OrigSel, Sel1)
    end,
    St#st{sel=Sel,sh=false};
locking_1(Type, #st{sel=OrigSel,selmode=Selmode}=St) ->
    LvsSel0 = locked_vs_as_selection(Selmode),
    #st{sel=LvsSel} = wings_sel:make(LvsSel0,Selmode,St),
    Sel = case Type of
      select -> union(OrigSel, LvsSel);
      deselect -> subtract(OrigSel, LvsSel)
    end,
    St#st{sel=Sel,sh=false}.

locked_vs_as_selection(Selmode) ->
    fun(Elem,#we{pst=Pst}=We) ->
        Vs = convert_to_vs(Selmode,[Elem],We),
        Lvs = get_locked_vs(Pst),
        is_in_locked_vs(Vs,Lvs)
    end.

is_in_locked_vs([V|Vs],Lvs) ->
    case gb_sets:is_member(V,Lvs) of
      true -> is_in_locked_vs(Vs,Lvs);
      false -> false
    end;
is_in_locked_vs([],_Lvs) ->
    true.

%% From wings_sel_cmd (selection groups code)
union(Sa, Sb) ->
    combine_sel(fun(Ss) -> gb_sets:union(Ss) end, Sa, Sb).

combine_sel(Combine, Sa, Sb) ->
    combine_sel(Combine, lists:merge(Sa, Sb)).
combine_sel(Combine, [{Id,Sa},{Id,Sb}|T]) ->
    S = Combine([Sa,Sb]),
    case gb_sets:is_empty(S) of
    true -> combine_sel(Combine, T);
    false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(Combine, [{Id,S0}|T]) ->
    S = Combine([S0]),
    case gb_sets:is_empty(S) of
    true -> combine_sel(Combine, T);
    false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(_Combine, []) -> [].

subtract([{Id1,_}=E1|Es1], [{Id2,_}|_]=Set2) when Id1 < Id2 ->
    [E1|subtract(Es1, Set2)];
subtract([{Id1,_}|_]=Set1, [{Id2,_}|Es2]) when Id1 > Id2 ->
    subtract(Set1, Es2);
subtract([{Id,E1}|Es1], [{Id,E2}|Es2]) ->	%E1 == E2
    E = gb_sets:subtract(E1, E2),
    case gb_sets:is_empty(E) of
    true -> subtract(Es1, Es2);
    false -> [{Id,E}|subtract(Es1, Es2)]
    end;
subtract([], _Es2) -> [];
subtract(Es1, []) -> Es1.
%%%%

convert_to_vs(vertex,Sel,_) -> Sel;
convert_to_vs(edge,Sel,We) ->
    wings_edge:to_vertices(Sel, We);
convert_to_vs(face,Sel,We) ->
    wings_face:to_vertices(Sel, We);
convert_to_vs(body,_,We) ->
    wings_we:visible_vs(We).

update_dlist({vs,LockedVs}, #dlo{plugins=Pdl,src_we=#we{vp=Vtab}=We}=D, _) ->
    Key = ?MODULE,
    Locked0 = gb_sets:to_list(LockedVs),
    Visible = wings_we:visible_vs(We),
    Locked = [LVs || LVs <- Locked0, lists:member(LVs, Visible)],
    case positions(Locked, Vtab, <<>>) of
	<<>> ->
	    D#dlo{plugins=[{Key,none}|Pdl]};
	Data ->
	    N = byte_size(Data) div 12,
	    Draw0 = fun(RS) ->
			    gl:drawArrays(?GL_POINTS, 0, N),
                            RS
		    end,
	    Draw = wings_vbo:new(Draw0, Data),
	    D#dlo{plugins=[{Key,Draw}|Pdl]}
    end.

positions([V|Vs], Vtab, Acc0) ->
    {X,Y,Z} = array:get(V, Vtab),
    Acc = <<Acc0/binary,X:?F32,Y:?F32,Z:?F32>>,
    positions(Vs, Vtab, Acc);
positions([], _, Acc) -> Acc.

get_locked_vs(Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
      none ->
          gb_sets:empty();
      {_,Data} ->
          gb_trees:get(vs,Data)
    end.

set_locked_vs(LockedVs,Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
      none ->
          Data = gb_trees:empty(),
          NewData = gb_trees:insert(vs,LockedVs,Data),
          gb_trees:insert(?MODULE,NewData,Pst);
      {_,Data} ->
          NewData = gb_trees:update(vs,LockedVs,Data),
          gb_trees:update(?MODULE,NewData,Pst)
    end.

get_data(update_dlist, Data, Acc) ->  % for draw lists
    case wings_pref:get_value(show_magnet_mask) of
      true -> get_data_2(Data,Acc);
      false -> Acc
    end;
get_data(save, Data, Acc) ->  % the 'save' causes vertices to be renumbered
    get_data_2(Data,Acc).

get_data_2(Data, Acc) ->
    LockedVs = gb_trees:get(vs,Data),
    {ok, [{plugin, {?MODULE, {vs, LockedVs}}}|Acc]}.

draw(plain, List, _D, Selmode, RS0) ->
    case wings_pref:get_value(show_magnet_mask) of
        true ->
            {R0,G0,B0,A} = wings_pref:get_value(masked_vertex_color),
            PtSize = wings_pref:get_value(masked_vertex_size),
            case wings_pref:get_value(magnet_mask_on) of
                true ->
                    Colour = gl:color4f(R0, G0, B0, A),
                    Size = PtSize;
                false ->
                    Colour = gl:color4f(1-R0, 1-G0, 1-B0, A),
                    Size = PtSize*0.8
            end,
            gl:pointSize(vert_display(Size,Selmode)),
            gl:enable(?GL_BLEND),
            gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
            Colour,
            RS = wings_dl:call(List, RS0),
            gl:disable(?GL_BLEND),
            RS;
        false-> RS0
    end;
draw(_,_,_,_, RS) -> RS.

vert_display(Size,vertex) ->
    VSize = wings_pref:get_value(selected_vertex_size),
    case VSize >= Size of
      true -> VSize + 2;
      false -> Size
    end;
vert_display(Size,_Selmode) -> Size.

% Called from wings_we:merge/1.
% When two or more shapes merge, plugins that use the Pst have
% the option to merge the pst data from those shapes like I'm doing here :)
merge_we([We]) -> We;
merge_we(Wes) -> merge_we_1(Wes,[]).

merge_we_1([#we{pst=Pst}|Wes], LockedVs) ->
    Locked = get_locked_vs(Pst),
    PluginData = gb_sets:to_list(Locked),
    merge_we_1(Wes, [PluginData|LockedVs]);
merge_we_1([],LockedVs) ->
    LVs = lists:merge(LockedVs),
    NewLvs = gb_sets:from_list(LVs),
    NewTree = gb_trees:empty(),
    gb_trees:insert(vs, NewLvs, NewTree).
