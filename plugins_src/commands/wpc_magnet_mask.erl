%%
%%  wpc_magnet_mask.erl --
%%
%%     Plugin for locking vertices against the influence of magnets.
%%     This plugin uses #dlo.plugin to store its drawlist and the #we.pst to
%%     store vextex data.  The vertex data is renumerbered when the objects are
%%     merged or saved.
%%
%%  Copyright (c) 2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_magnet_mask).

-export([init/0,menu/2,command/2]).
-export([update_dlist/3,draw/4,get_locked_vs/1,get_data/3,merge_we/1]).

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
    [{?__(1,"Show Magnet Mask"),show_magnet_mask,crossmark(show_magnet_mask)}].

crossmark(Key) ->
    Val = case wings_pref:get_value(Key) of
          undefined ->
            {_,Client} = wings_wm:this(),
            wings_wm:get_prop(Client, Key);
          Other -> Other
      end,
    case Val of
      false -> [];
      true -> [crossmark]
    end.

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
locking(Type, #st{sel=[]}=St0) when Type=/=select; Type=/=deselect ->
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
              Vertices = convert_to_vs(Selmode,We,Sel),
              Locked = gb_sets:union(Lvs, Vertices),
              NewPst = set_locked_vs(Locked,Pst),
              We#we{pst=NewPst}
          end, St);
locking_1(unmask, #st{selmode=Selmode}=St) ->
    wings_sel:map(fun
           (Sel, #we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Vertices = convert_to_vs(Selmode,We,Sel),
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
locking_1(deselect, #st{selmode=body}=St) -> St;
locking_1(deselect, #st{selmode=Selmode}=St) ->
    NewSel = wings_sel:fold(fun (Items,#we{pst=Pst,id=Id} = We,Acc) ->
               Lvs0 = get_locked_vs(Pst),
               LockedCurSelmode = convert_vs_to_selmode(Selmode,Lvs0,We),
               NewSel = gb_sets:subtract(Items,LockedCurSelmode),
               [{Id,NewSel} | Acc]
               end, [], St),
    St#st{sel=lists:sort(NewSel), sh=false};

locking_1(select, #st{selmode=body}=St) -> St;
locking_1(select, #st{sel=[],selmode=Selmode}=St) ->
    Sel = fun(V,#we{pst=Pst}) ->
        lists:member(V,gb_sets:to_list(get_locked_vs(Pst)))
    end,
    case Selmode =:= vertex of
      false ->
        wings_sel_conv:mode(Selmode,wings_sel:make(Sel, vertex, St));
      true ->
        wings_sel:make(Sel, vertex, St)
    end;
locking_1(select, #st{selmode=Selmode}=St) ->
    NewSel = wings_sel:fold(fun (Items,#we{pst=Pst,id=Id} = We,Acc) ->
               Lvs0 = get_locked_vs(Pst),
               LockedCurSelmode = convert_vs_to_selmode(Selmode,Lvs0,We),
               NewSel = gb_sets:union(Items,LockedCurSelmode),
               [{Id,NewSel} | Acc]
               end, [], St),
    St#st{sel=lists:sort(NewSel), sh=false}.

convert_vs_to_selmode(vertex,Vs,_) -> Vs;
convert_vs_to_selmode(edge,Vs,We) -> wings_edge:from_vs(Vs,We);
convert_vs_to_selmode(face,Vs,We) -> gb_sets:from_ordset(wings_face:from_vs(Vs,We)).


convert_to_vs(vertex,_,Sel) -> Sel;
convert_to_vs(edge,We,Sel) ->
    gb_sets:from_ordset(wings_edge:to_vertices(Sel, We));
convert_to_vs(face,We,Sel) ->
    gb_sets:from_ordset(wings_face:to_vertices(Sel, We));
convert_to_vs(body,We,_) ->
    gb_sets:from_list(wings_we:visible_vs(We)).

update_dlist({vs,LockedVs},#dlo{plugins=Pdl,src_we=#we{vp=Vtab}=We}=D, _) ->
    Key = ?MODULE,
    Locked0 = gb_sets:to_list(LockedVs),
    Visible = wings_we:visible_vs(We),
    Locked = [LVs || LVs <- Locked0, lists:member(LVs,Visible)],
    Pos = positions(Locked,Vtab,[]),
    case Pos of
      [] ->
        D#dlo{plugins=[{Key,none}|Pdl]};
      _ ->
        List = gl:genLists(1),
        gl:newList(List,?GL_COMPILE),
        gl:'begin'(?GL_POINTS),
        pump_vertices(Pos),
        gl:'end'(),
        gl:endList(),
        D#dlo{plugins=[{Key,{vs,List}}|Pdl]}
    end.

pump_vertices([A|Vs]) ->
    gl:vertex3fv(A),
    pump_vertices(Vs);
pump_vertices([]) -> ok.

positions([V|Locked],Vtab,Acc) ->
    Pos = array:get(V,Vtab),
    positions(Locked,Vtab,[Pos|Acc]);
positions([],_,Acc) -> Acc.

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

draw(plain, {vs,List}, _D, Selmode) ->
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
        wings_dl:call(List),
        gl:disable(?GL_BLEND);
      false-> ok
    end;
draw(_,_,_,_) -> ok.

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
