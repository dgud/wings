%%%% Plugin for locking vertices against the influence of magnets.

-module(wpc_magnet_mask).

-export([init/0,menu/2,command/2]).
-export([update_dlist/3,draw/4,get_locked_vs/1,get_data/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").

init() ->
    wings_pref:set_default(show_magnet_mask,true),
    true.

menu({tools},Menu) ->
    Menu ++ tools_menu_entry();
menu({select,by},Menu) ->
    Menu ++ select_menu_entry();
menu({view,show},Menu) ->
    Menu ++ view_menu_entry();
menu(_,Menu) -> Menu.

tools_menu_entry() ->
    [{?__(1,"Magnet Mask"),{magnet_mask,
      [{?__(2,"Lock"),mask},
       {?__(3,"Unlock"),unmask}]}}].

select_menu_entry() ->
    [{?__(1,"Magnet Mask"),magnet_mask}].

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


command({tools,{magnet_mask,Type}},St) ->
    locking(Type,St);
command({select,{by,magnet_mask}}, St) ->
    select_locked(St);
command({view,{show,show_magnet_mask}}, St) ->
    Bool = wings_pref:get_value(show_magnet_mask),
    wings_pref:set_value(show_magnet_mask, not Bool),
    St;
command(_,_) ->
    next.

%%%% Lock or unlock selected vertices storing those locked in the pst
locking(mask, #st{selmode=Selmode}=St) ->
    wings_sel:map(fun
           (Sel, #we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Vertices = convert_sel(Selmode,We,Sel),
              Locked = gb_sets:union(Lvs, Vertices),
              NewPst = set_locked_vs(Locked,Pst),
              We#we{pst=NewPst}
          end, St);
locking(unmask, #st{selmode=Selmode}=St) ->
    wings_sel:map(fun
           (Sel, #we{pst=Pst}=We) ->
              Lvs = get_locked_vs(Pst),
              Vertices = convert_sel(Selmode,We,Sel),
              Locked = gb_sets:difference(Lvs, Vertices),
              NewPst = set_locked_vs(Locked,Pst),
              We#we{pst=NewPst}
          end, St).

select_locked(St) ->
    Sel = fun(V,#we{pst=Pst}) ->
        lists:member(V,gb_sets:to_list(get_locked_vs(Pst)))
    end,
    {save_state,wings_sel:make(Sel, vertex, St)}.

convert_sel(vertex,_,Sel) -> Sel;
convert_sel(edge,We,Sel) ->
    gb_sets:from_ordset(wings_edge:to_vertices(Sel, We));
convert_sel(face,We,Sel) ->
    gb_sets:from_ordset(wings_face:to_vertices(Sel, We));
convert_sel(body,We,_) ->
    gb_sets:from_list(wings_we:visible_vs(We)).

update_dlist({vs,LockedVs},#dlo{plugins=Pdl,src_we=#we{vp=Vtab}=We}=D,_) ->
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
        wings_draw:pump_vertices(Pos),
        gl:'end'(),
        gl:endList(),
        D#dlo{plugins=[{Key,{vs,List}}|Pdl]}
    end.


positions([V|Locked],Vtab,Acc) ->
    Pos = gb_trees:get(V,Vtab),
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

get_data(update_dlist, Data, Acc) ->
    case wings_pref:get_value(show_magnet_mask) of
      true -> get_data_2(Data,Acc);
      false -> Acc
    end;
get_data(save, Data, Acc) ->
    get_data_2(Data,Acc).

get_data_2(Data, Acc) ->
    LockedVs = gb_trees:get(vs,Data),
    {ok, [{plugin, {?MODULE, {vs, LockedVs}}}|Acc]}.

draw(plain, {vs,List}, _D, _Selmode) ->
    case wings_pref:get_value(show_magnet_mask) of
      true ->
        gl:pointSize(wings_pref:get_value(masked_vertex_size)),
        gl:enable(?GL_BLEND),
        gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
        {R0,G0,B0,A} = wings_pref:get_value(masked_vertex_color),
        gl:color4f(R0, G0, B0, A),
        wings_dl:call(List),
		gl:disable(?GL_BLEND);
      false-> ok
    end;
draw(_,_,_,_) -> ok.
