%%
%%  wpc_shift.erl --
%%
%%     Plug-in for shifting vertices
%%
%%  Copyright (c) 2005-2011 Dave Rodgers
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%


-module(wpc_shift).

-export([init/0,menu/2,command/2]).
-import(lists, [foldl/3]).
-include_lib("wings/src/wings.hrl").

-define(HUGE, 1.0E307).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Exported functions
%%

init() ->
    true.

menu({Mode}, Menu) when Mode==vertex; Mode==edge; Mode==face; Mode==body ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_heading()|NewMenu];
parse([A = separator|Rest], NewMenu, false) ->
    parse(Rest, [A, menu_heading()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

menu_heading() ->
    {?__(1,"Shift"), {shift,fun adv_submenu/2}}.

command({Mode,{shift,Type}}, St) when Mode==vertex; Mode==edge; Mode==face; Mode==body ->
    shift_cmd(Type, St);
command(_,_) -> next.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Create the menus
%%

submenu_items(1) ->
    {{planar_shift}, {'ASK',{[{axis, ?__(1,"Pick axis")},
	                      {point,  ?__(2,"Pick point")}],[],[]}}};
submenu_items(2) ->
    {{spherical_shift}, {'ASK',{[{point, ?__(3,"Pick center")}],[],[]}}};

submenu_items(3) ->
    {{cylindrical_shift}, {'ASK',{[{axis, ?__(4,"Pick axis")},
                                   {point, ?__(5,"Pick point")}],[],[]}}}.
adv_submenu(help, _) ->
    {?__(1,"Planar Shift"),
     ?__(2,"Spherical Shift"),
     ?__(3,"Cylindrical Shift")};
adv_submenu(Button, NS) ->
    wings_menu:build_command(submenu_items(Button), NS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Respond to commands
%%

shift_cmd({Mode, {'ASK',Ask}}, St) ->
    wings:ask(Ask, St, fun (AskResult, St0) ->
                            shift_ask_callback({Mode, AskResult}, St0)
                       end);
%%% for repeat cmds
shift_cmd({Mode, Data}, St) ->
    shift_ask_callback({Mode, Data}, St).

shift_ask_callback({{Mode}, Data}, St) ->
    shift_verts(Mode, Data, St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Drag and iterate through the vertices
%%

shift_verts(Mode, Data, #st{selmode=body}=St) ->
    F = fun(Vs, We) ->
                Center = wings_vertex:center(Vs, We),
                object_vector(Data, Center)
        end,
    shift_verts_1(F, Mode, St);
shift_verts(Mode, Data, St) ->
    F = fun(_, _) -> Data end,
    shift_verts_1(F, Mode, St).

shift_verts_1(F, ShiftMode, #st{selmode=SelMode}=St) ->
    Shift = case ShiftMode of
                planar_shift -> fun shift_planar/3;
                cylindrical_shift -> fun shift_cylindrical/3;
                spherical_shift -> fun shift_spherical/3
            end,
    Follow = wings_va:any_uvs(St),
    DF = fun(Items, We) ->
                 Vs = convert_sel(SelMode, Items, We),
                 Data = F(Vs, We),
                 uv_tv(Follow, shift_verts_2(Shift, Data, Vs, We))
         end,
    wings_drag:fold(DF, [distance], uv_flags(Follow), St).

shift_verts_2(ShiftFun, Data, Vs, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Distance], A) ->
                  foldl(fun({V,Vpos}, VsAcc) ->
                                [{V,ShiftFun(Vpos, Distance, Data)}|VsAcc]
                        end, A, VsPos)
          end,
    shift_uv(Vs, VsPos, Fun, We).

%% The UV coordinates follow the shifted vertices (the texture keeps its
%% place on the surface).
shift_uv(Vs, VsPos, Fun, #we{lv=Lv0,rv=Rv0}=We) ->
    case wings_va:any_uvs(We) of
        false ->
            {Vs,Fun};
        true ->
            MoveData = wings_va:uv_move_data(Vs, We),
            Old = maps:from_list(VsPos),
            UvFun =
                fun(W, [Distance|_]) ->
                    case wings_pref:get_value(update_uv) of
                        false ->
                            W#we{lv=Lv0,rv=Rv0};
                        true ->
                            New = maps:from_list(Fun([Distance], [])),
                            DPFun =
                                fun(V) ->
                                    e3d_vec:sub(maps:get(V, New), maps:get(V, Old))
                                end,
                            wings_va:uv_move(MoveData, DPFun, W)
                    end
                end,
            {we,[UvFun],{Vs,Fun}}
    end.

%% The key [1] switches the update of the UV coordinates. The transformation
%% fun must accept the change of the mode.
uv_flags(true) -> [{mode,{fun uv_mode/2,uv}}];
uv_flags(false) -> [].

uv_mode(help, _) ->
    [?__(1,"[Shift] and/or [Ctrl] and/or [Alt] Constrain"),
     "  [1] ",uv_help()];
uv_mode({key,$1}, Mode) ->
    wings_pref:set_value(update_uv, not wings_pref:get_value(update_uv)),
    Mode;
uv_mode(_, _) ->
    none.

uv_help() ->
    case wings_pref:get_value(update_uv) of
        true -> ?__(1,"Turn Off UV");
        false -> ?__(2,"Turn On UV")
    end.

uv_tv(true, {Vs,Tr}) ->
    {Vs,uv_tr(Tr)};
uv_tv(true, {we,WeFuns,Tv}) ->
    {we,WeFuns,uv_tv(true, Tv)};
uv_tv(false, Tv) ->
    Tv.

uv_tr(Tr) ->
    fun(Move, Acc) when is_list(Move) -> Tr(Move, Acc);
       (new_mode_data, {uv,_}) -> uv_tr(Tr);
       (Key, Val) -> uv_tr(Tr(Key, Val))
    end.

-spec convert_sel(SelMode, Items, #we{}) -> Vertices when
      SelMode :: wings_sel:mode(),
      Items :: [wings_sel:item_id()],
      Vertices :: [wings_vertex:vertex_num()].

convert_sel(vertex, Vs, _We) ->
    gb_sets:to_list(Vs);
convert_sel(edge, Es, We) ->
    wings_vertex:from_edges(Es, We);
convert_sel(face, Fs, We) ->
    wings_vertex:from_faces(Fs, We);
convert_sel(body, _, We) ->
    wings_we:visible_vs(We).

object_vector({Axis,CenterPoint}, ObjCenter) ->
    Vector = e3d_vec:norm_sub(ObjCenter, CenterPoint),
	{object, Axis, Vector};
object_vector(CenterPoint, ObjCenter) ->
    Vector = e3d_vec:norm_sub(ObjCenter, CenterPoint),
	{object, Vector}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  The Main Functions.
%%
%%   The return value is the new position. {X,Y,Z}
%%

shift_planar(Pos, +0.0, _) -> Pos;
shift_planar(Pos, Dist, {Axis, Center}) ->
  V = e3d_vec:sub(Pos,Center),
  D = e3d_vec:dot(V,Axis),
  if
    D < -?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, -Dist)]);
    D > +?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, +Dist)]);
    true ->
      Pos
  end;
shift_planar(Pos, Dist, {object, Axis, V}) ->
  D = e3d_vec:dot(V,Axis),
  if
    D < -?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, -Dist)]);
    D > +?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, +Dist)]);
    true ->
      Pos
  end.

shift_cylindrical(Pos, +0.0, _) -> Pos;
shift_cylindrical(Pos, Dist, {Axis, Center}) ->
    V = e3d_vec:sub(Pos,Center),
    OffDist = e3d_vec:dot(V,Axis),
    OffVec = e3d_vec:sub(V,e3d_vec:mul(Axis, OffDist)),
    N = e3d_vec:norm(OffVec),
    e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]);
shift_cylindrical(Pos, Dist, {object, Axis, V}) ->
    OffDist = e3d_vec:dot(V,Axis),
    OffVec = e3d_vec:sub(V,e3d_vec:mul(Axis, OffDist)),
    N = e3d_vec:norm(OffVec),
    e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]).

shift_spherical(Pos, +0.0, _Center) -> Pos;
shift_spherical(Pos, Dist, {object, V}) ->
    N = e3d_vec:norm(V),
    e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]);
shift_spherical(Pos, Dist, Center) ->
    V = e3d_vec:sub(Pos,Center),
    N = e3d_vec:norm(V),
    e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]).
