%%
%%  wpc_metadata_dc.erl --
%%
%%     Plugin that implements Dublin Core metadata
%%
%%  Copyright (c) 2026 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_metadata_dc).
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

%% The dublin core elements
%%
elements() ->
    [
        {text,{title,
            ?__(1,"Title:"),
            ?__(2,"The name of the resource")}},
        {text,{creator,
            ?__(3,"Creator:"),
            ?__(4,"The person or entity that primarily created the resource.")}},
        {text,{subject,
            ?__(5,"Subject:"),
            ?__(6,"The topic or keywords of the resource.")}},
        {text,{description,
            ?__(7,"Description:"),
            ?__(8,"A description of the resource")}},
        {text,{publisher,
            ?__(9,"Publisher:"),
            ?__(10,"The publisher of the resource")}},
        {text,{contributor,
            ?__(11,"Other Contributors:"),
            ?__(12,"Other persons or entities that contributed towards the creation of the resource")}},
        {text,{date,
            ?__(13,"Date:"),
            ?__(14,"A date specifying the creation or availability of the resource")}},
        {text,{identifier,
            ?__(15,"Identifier:"),
            ?__(16,"An identifier (such as a URI, string or number) that distinguishes the resource.")}},
        {text,{format,
            ?__(17,"Format:"),
            ?__(18,"The data format and dimensions of the resource. Software and hardware related to the resource can be specified here.")}},
        {text,{source,
            ?__(19,"Source:"),
            ?__(20,"Identifiers to other resources that are a source for the current resource.")}},
        {text,{coverage,
            ?__(21,"Coverage:"),
            ?__(22,"The geographic or temporal coverage of the resource.")}},
        {text,{language,
            ?__(23,"Language:"),
            ?__(24,"The language of the resource.")}},
        {text,{relation,
            ?__(25,"Relation:"),
            ?__(26,"Identifiers to other resources with a relation to the current resource.")}},
        {text,{rights,
            ?__(27,"Rights:"),
            ?__(28,"Information on rights of the resource.")}},
        {text,{type,
            ?__(29,"Type:"),
            ?__(30,"A category of media of the resource.")}}
    ].


%%%
%%%

metadata_names(_Scope) ->
    [{dc,?__(1,"General"),?MODULE}].

metadata_dialog(_Scope, _Name, MetadataDC) when is_list(MetadataDC) ->
    List_0 = [V || {C,V} <- elements(), C =:= text],
    {vframe,[{label_column,[
        text_field(Name, FieldStr, FieldInfo, MetadataDC)
        || {Name,FieldStr,FieldInfo} <- List_0]}]}.

metadata_update(_Scope, _Name, MetadataDC, Args) when is_list(MetadataDC) ->
    List = [Name || {Name,_,_} <- [V || {C,V} <- elements(), C =:= text]],
    lists:foldl(
        fun (FieldName, Acc) ->
            update(FieldName, Acc, Args)
        end, MetadataDC, List).

%%%
%%%

text_field(Name, FieldName, FieldInfo, MetadataDC) when is_list(MetadataDC) ->
    Val = proplists:get_value(Name, MetadataDC, ""),
    {FieldName,{text,Val,[{key,Name},{width,50},{info,FieldInfo}]}}.


update(Name, Acc, Args) ->
    case proplists:get_value(Name, Args, 1) of
        "" ->
            proplists:delete(Name, Acc);
        Creator ->
            orddict:store(Name, Creator, orddict:from_list(proplists:delete(Name, Acc)))
    end.





