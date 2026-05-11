%%
%%  wpc_metadata_units.erl --
%%
%%     Plugin that implements units metadata
%%
%%  Copyright (c) 2026 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_metadata_units).
-export([init/0,menu/2,command/2]).
-export([metadata_names/1]).
-export([metadata_dialog/3]).
-export([metadata_update/4]).

-include_lib("wings/src/wings.hrl").

init() ->
    true.
menu(_,Menu) -> Menu.
command(_,_) ->
    next.


%%%
%%%

%% Units of measure
%%
units() ->
    [
        {none,?__(1,"None")},
        {m,?__(2, "Meters")},
        {dm,?__(3, "Decimeters")},
        {cm,?__(4, "Centimeters")},
        {mm,?__(5, "Millimeters")},
        {micron,?__(6, "Microns")},
        {yd,?__(7, "Yards")},
        {ft,?__(8, "Feet")},
        {in,?__(9, "Inches")}
    ].


%%%
%%%

metadata_names(_Scope) ->
    [{units,?__(1,"Units"),?MODULE}].

metadata_dialog(_Scope, _Name, MetadataUnits) when is_list(MetadataUnits) ->
    Units = proplists:get_value(units, MetadataUnits, none),
    {vframe,[
        {label,?__(12,"Units")},
        {menu,
            [{MenuStr, MenuUnit}
                || {MenuUnit,MenuStr} <- units()],
            Units,[{key,units}]}
    ]}.

metadata_update(_Scope, _Name, MetadataUnits, Args) when is_list(MetadataUnits) ->
    lists:foldl(
        fun (FieldName, Acc) ->
            update(FieldName, Acc, Args)
        end, MetadataUnits, [units]).

update(Name, Acc, Args) ->
    case proplists:get_value(Name, Args, 1) of
        none ->
            proplists:delete(Name, Acc);
        Val ->
            orddict:store(Name, Val, orddict:from_list(proplists:delete(Name, Acc)))
    end.


