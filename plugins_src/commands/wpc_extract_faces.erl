%%
%%  wpc_extract_faces.erl --
%%
%%    Plugin for extracting individual faces.
%%
%%  Copyright (c) 2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_extract_faces).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.
menu({face},Menu) ->
    lists:reverse(parse(Menu, [], false));

menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_heading()|NewMenu];
parse([A = {_,{extract_region,_}}|Rest], NewMenu, false) ->
    parse(Rest, [menu_heading(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

menu_heading() ->
    F = menu_fun(),
    {?__(1,"Extract Faces"),{extract_faces,F}}.

menu_fun() ->
    fun
      (help,_Ns) ->
        {?__(1,"Extract along std. axis"),
         ?__(2,"Extract along selection's normal"),
         ?__(3,"Pick axis to extract along")};
      (1,_Ns) -> std_menu();
      (2,_Ns) -> {face,{extract_faces,normal}};
      (3,_Ns) ->
          HelpStr = ?__(4,"Pick axis along which to move the extracted faces"),
          Ask = {[{axis,HelpStr}],[]},
          {face,{extract_faces,{'ASK',Ask}}}
    end.

std_menu() ->
    H = ?__(1,"Extract each face and move it "),
    [{wings_s:dir(normal), normal, H ++ ?__(2,"along its normal")},
     {wings_s:dir(free), free, H ++ ?__(3,"freely in all directions")},
     {wings_s:dir(x),x, H ++ wings_s:dir_axis(x)},
     {wings_s:dir(y),y, H ++ wings_s:dir_axis(y)},
     {wings_s:dir(z),z, H ++ wings_s:dir_axis(z)},
     separator,
     {wings_util:cap(wings_s:dir(last_axis)),last_axis,
       H ++ ?__(4,"along the last axis")},
     {wings_util:cap(wings_s:dir(default_axis)),default_axis,
       H ++ ?__(5,"along the default axis")}].

command({face,{extract_faces,Dir}}, St) ->
    extract_faces(Dir,St);
command(_,_) -> next.

extract_faces(Axis, St0) ->
    St1 = wings_sel:map(fun(Faces, We) ->
        wings_extrude_face:faces(Faces, We)
    end, St0),
    St2 = wings_sel:fold(
    fun(Faces, We0, #st{sel=Sel0,onext=Oid}=S0) ->
        We = wings_dissolve:complement(Faces, We0),
        S = wings_shape:insert(We, extract, S0),
        Sel = [{Oid,Faces}|Sel0],
        S#st{sel=Sel}
    end, St0#st{sel=[]}, St1),
    Sel = St2#st.sel,
    St = wings_sel:set(Sel, St2),
    wings_move:setup(Axis,St).
