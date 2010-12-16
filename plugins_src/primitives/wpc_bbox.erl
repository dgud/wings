%%
%%  wpc_bbox.erl --
%%
%%    Plugin to create a bounding box numerically.
%%
%%  Copyright (c) 2009-2010 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_bbox).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() -> true.

menu(Mode, Menu) when Mode =:= {shape}; Mode =:= {tools,bbox} ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [bb_menu(), separator|NewMenu];
parse([A = {_,image,_}|Rest], NewMenu, false) ->
    parse(Rest, [bb_menu(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

bb_menu() ->
    [{?__(1,"Bounding Box..."),bbox,
      ?__(2,"Create Bounding Box")}].

command({shape, bbox}, St) ->
    create_bbox(St);
command({tools,{bbox,bbox}}, St) ->
    create_bbox(St);
command(_, _) -> next.

create_bbox(#st{bb=BB}=St) ->
    {{Bx,By,Bz},{Cx,Cy,Cz}} = case BB of
        none ->
            Zero = e3d_vec:zero(),
            {Zero,Zero};
        [B1,B2] ->
            {Bx0,By0,Bz0} = e3d_vec:sub(B1,B2),
            Center = e3d_vec:add(B2,e3d_vec:divide({Bx0,By0,Bz0},2)),
            {{abs(Bx0),abs(By0),abs(Bz0)},Center}
    end,
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Qs =
      [{hframe,[{label,[{bold,?__(2,"BB Dimensions:")}]},
                      {label,io_lib:format("{~s ~s ~s}",
                       [wings_util:nice_float(Bx),
                        wings_util:nice_float(By),
                        wings_util:nice_float(Bz)])}]},
             {hframe,[{label,X},{text,Bx},
                      {label,Y},{text,By},
                      {label,Z},{text,Bz}]},
            separator,
             {hframe,[{label,[{bold,?__(3,"BB Center:")}]},
                      {label,io_lib:format("{~s ~s ~s}",
                       [wings_util:nice_float(Cx),
                        wings_util:nice_float(Cy),
                        wings_util:nice_float(Cz)])}]},
             {hframe,[{label,X},{text,Cx},
                      {label,Y},{text,Cy},
                      {label,Z},{text,Cz}]}],
    wings_ask:dialog(?__(1,"Create Bounding Box"), Qs,
      fun(Res) -> save_bbox(Res, St) end).

save_bbox(Result, St) ->
    [{Bx,By,Bz}, Center] = result(Result),
    Bb = {abs(Bx),abs(By),abs(Bz)},
    HalfB = e3d_vec:divide(Bb,2),
    BBox = [e3d_vec:sub(Center,HalfB), e3d_vec:add(Center,HalfB)],
    St#st{bb=BBox}.

result([X,Y,Z|Result]) ->
    [{X,Y,Z}|result(Result)];
result([]) -> [].
