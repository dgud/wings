%%
%%  wpc_intersect_vertex.erl --
%%
%%     Plug-in for moving vertice(s) to the intersection of a line and plane
%%
%%  Copyright (c) 2004-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%  Contributed by elrond79.
%%
%%     $Id$
%%
%%  2000-10-01:  Changed help text to incorporate suggestions by Puzzled Paul
%%  2000-09-21:  Normalized LD and PN (so dot product not <.001 if either is very
%%               short)
%%  2000-09-17:  Tried to make more compliant w/ wings API (Paul Molodowitch)

-module(wpc_intersect_vertex).

-export([init/0,menu/2,command/2,intersect_vertex/3]).

-include_lib("wpc_intersect.hrl").
-import(lists, [foldl/3,splitwith/2]).

init() ->
    true.


% If we can find the "flatten" menu item, stick it in after that; otherwise, put at end
menu({vertex}, Menu) ->
    {SplitMenu1, SplitMenu2} = splitwith(fun isNotFlattenMenuItem/1, Menu),
    if
	length(SplitMenu1) < length(SplitMenu2) ->
	    [FlattenMenuItem|MenuTail] = SplitMenu2,
	    SplitMenu1 ++ [FlattenMenuItem|menu_item()] ++ MenuTail;
	true ->
	    Menu ++ [separator] ++ menu_item()
    end;
menu(_,Menu) -> Menu.

isNotFlattenMenuItem({"Flatten",_}) ->
    false;
isNotFlattenMenuItem({_,{flatten,_}}) ->
    false;
isNotFlattenMenuItem(_) ->
    true.


command({vertex,{intersect,{Type,{'ASK',Ask}}}}, St) ->
    intersect({Type,{'ASK',Ask}}, St);
%% command for repeat drag args
command({vertex,{intersect,{Type,Data}}},St) ->
    intersect_ask_callback({Type,Data},St);

command(_,_) -> next.

%% creates the menu item for the vertex intersect command.

menu_item() ->
    [{?__(8,"Intersect"),{intersect,fun adv_submenu/2}}].

submenu_items(1) ->
    {stay_on_line,
     {'ASK',{[{axis,       ?__(1,"Pick direction of line - line will pass through selected vertex(es)")},
	      {axis_point, ?__(2,"Pick plane to intersect")}],[],[]}}};
submenu_items(2) ->
    {stay_on_plane,
     {'ASK',{[{axis,       ?__(3,"Pick plane normal - plane will pass through selected vertex(es)")},
	      {axis_point, ?__(4,"Pick line to intersect")}],[],[]}}};
submenu_items(3) ->
    {pick_all,
     {'ASK',{[{axis,       ?__(5,"Pick direction of line")},
	      {point,      ?__(6,"Pick point for line to pass through")},
	      {axis,       ?__(7,"Pick plane normal")},
	      {point,      ?__(8,"Pick point for plane to pass through")}],[],[]}}}.

adv_submenu(help, _) ->
    {?__(1,"Stay on line, move to intersection with plane"),
     ?__(2,"Stay on plane, move to line"),
     ?__(3,"Pick line and plane")};
adv_submenu(Button, NS) ->
    wings_menu:build_command(submenu_items(Button), NS).

intersect({stay_on_line, {'ASK',Ask}}, St0) ->
    wings:ask(Ask, St0, fun (AskResult, St) -> intersect_ask_callback({stay_on_line, AskResult}, St) end);
intersect({stay_on_plane, {'ASK',Ask}}, St0) ->
    wings:ask(Ask, St0, fun (AskResult, St) -> intersect_ask_callback({stay_on_plane, AskResult}, St) end);
intersect({pick_all, {'ASK',Ask}}, St0) ->
    wings:ask(Ask, St0, fun (AskResult, St) -> intersect_ask_callback({pick_all, AskResult}, St) end).

intersect_ask_callback({stay_on_line, {LineDir, PlaneNorm, PlanePoint}}, St) ->
    intersect(LineDir, selection, PlaneNorm, PlanePoint, St);
intersect_ask_callback({stay_on_plane, {PlaneNorm, LineDir, LinePoint}}, St) ->
    intersect(LineDir, LinePoint, PlaneNorm, selection, St);
intersect_ask_callback({pick_all, {LineDir, LinePoint, PlaneNorm, PlanePoint}}, St) ->
    intersect(LineDir, LinePoint, PlaneNorm, PlanePoint, St).


intersect(LineDir0, LinePoint, PlaneNorm0, PlanePoint, St) ->
    PlaneNorm = e3d_vec:norm(PlaneNorm0),
    LineDir = e3d_vec:norm(LineDir0),
    DotProd = e3d_vec:dot(LineDir, PlaneNorm),
    if
 	abs(DotProd) > 0.001 ->
	    IntersectData = #intersect_data{lineDir = LineDir, linePoint = LinePoint,
					    planeNorm = PlaneNorm, planePoint = PlanePoint,
					    lineDotPlane = DotProd},
	    {save_state,
	     wpa:sel_map(
	       fun(Vs, We) ->
		       intersect_body(Vs, We, IntersectData)
	       end, St)};
	true ->
 	    wpa:error_msg(?__(1,"Line and plane are nearly parallel:\n"
		      "can't find intersection.")),
	    keep
    end.

intersect_body(Vs, #we{vp=Vtab0}=We0, #intersect_data{} = IntersectData) when is_list(Vs) ->
    Vtab = foldl(fun(V, VTabAcc) ->
  			 intersect_vertex(V, VTabAcc, IntersectData)
  		 end, Vtab0, Vs),
    We = We0#we{vp=Vtab},
    wings_we:mirror_flatten(We0, We);
intersect_body(Vs, We, #intersect_data{} = IntersectData) ->
    intersect_body(gb_sets:to_list(Vs), We, IntersectData).


intersect_vertex(V, Tab, #intersect_data{linePoint = selection} = IntersectData0) ->
    IntersectData = IntersectData0#intersect_data{linePoint = array:get(V, Tab)},
    intersect_vertex(V, Tab, IntersectData);
intersect_vertex(V, Tab, #intersect_data{planePoint = selection} = IntersectData0) ->
    IntersectData = IntersectData0#intersect_data{planePoint = array:get(V, Tab)},
    intersect_vertex(V, Tab, IntersectData);
intersect_vertex(V, Tab, #intersect_data{lineDir = LD, linePoint = LP,
					  planeNorm = PN, planePoint = PP, lineDotPlane = LDdotPN}) ->
    %% The vector equation for the line is
    %%       LP + xLD
    %% for any scalar x; we need to find x st we have a point on the plane.
    %% For any point P on the plane, we know that P - PP is perpendicular
    %% to the plane normal, ie that
    %%       (P - PP).PN = 0
    %% So sticking in our line equation for P, we have
    %%       (LP + xLD - PP).PN = 0   ->   x = (PP - LP).PN
    %%                                         ------------
    %%                                            LD.PN
    
    X = e3d_vec:dot(e3d_vec:sub(PP, LP),PN)/LDdotPN,
    Intersection = e3d_vec:add(LP, e3d_vec:mul(LD, X)),
    array:set(V, Intersection, Tab).

