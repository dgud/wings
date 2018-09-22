%%
%%  collada_import.erl --
%%
%%     Collada import.
%%
%%  Copyright (c) 2016 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(collada_import).
-export([import/1]).
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/src/wings.hrl").
%% Local exports callbacks
-export([ignored/4, asset/3, lib_geom/4, mesh/4, source/4,
         param/4, vertices/4, make_polys/1, polys/4,
         lib_material/4, lib_images/4, effects/4,
         scene/3, lib_scenes/4, node/4, matrix/4,
         common_mat/4, common_newparam/4, surface/4,
         sampler2D/4, sloppy_color/3, chars/3,
         pop/1, replace/2, to_floats/1, make_float/1,
         make_float2/1, make_float3/1, make_float4/2,
         to_ints/1, to_tuple/2, to_tuple2/1, to_tuple3/1,
         to_tuple4/1, to_tuple5/1,
         pack_source/1, make_mesh/1, pick_source/2, pick_src_1/2,
         pick_mesh/2, pick_mesh_1/2, mesh_type/2,
         polygon_type/1, polygon_type_1/2,
         make_faces/2, sort_inputs/2, remove_duplicates/1,
         pick_faces/5, pick_tristrips/5, pick_polygons/4,
         pick_polygon/3, pick_verts/4, pick_vert/4,
         add_vert_info/3, rev_face/2, rev_face/1
        ]).

-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

-record(mat,
        {refs=#{},
         defs=#{},
	 images=#{}
        }).

-record(es, {vsn,
             state=[],
             up = y,
             materials=#mat{},
	     mesh=[],
	     scenes=[],
	     scene,
             val %% Temporary return value
            }).

%%-define(TEST, true).
-ifdef(TEST).
-export([test/0]).
-endif.

import(File) ->
    EF = {event_fun, fun top/3},
    ES = {event_state, #es{}},
    io:format("Import: ~p~n",[File]),
    case xmerl_sax_parser:file(File, [EF,ES]) of
        {ok, Es, <<>>} ->
            try
                E3dFile = make_file(Es),
                {ok, E3dFile#e3d_file{dir=filename:dirname(File)}}
            catch _:Reason ->
                    io:format("ERROR: ~P in ~p~n", [Reason, 20, erlang:get_stacktrace()]),
                    {error, ?__(1, "unknown/unhandled format, see log window")}
            end;
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("~s:~p: ERROR: ~p:~p~n", [File, Line, Error, Reason]),
            {error, ?__(1, "unknown/unhandled format, see log window")}
    end.


%%%%%%%%%%%%%%%%%%%%%%%% PARSER %%%%%%%%%%%%%%%%%%%%%%%%
top({startElement, _, "extra"=Ignore, _, _}=Ev, Loc, State) ->
    %% Ignore all extra fields for now
    parse(push({ignored, Ignore},State), Ev, Loc);
top({startElement, _, _, _, _}=Ev, Loc, State) ->
    parse(State, Ev, Loc);
top({endElement, _, _, _}=Ev, Loc, State) ->
    parse(State, Ev, Loc);
top({characters, _}=Ev, Loc, State) ->
    parse(State, Ev, Loc);
top(startDocument, _, State) -> State;
top(endDocument, _, State) -> State;
top({startPrefixMapping,_,_}, _, State) -> State;
top({endPrefixMapping,_}, _, State) -> State;
top({ignorableWhitespace, _}, _, State) -> State;
top({comment, _}, _, State) -> State;
top(Ev, Loc, State) ->
    unhandled(State, Ev, Loc),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%
parse(#es{state=[_|_]}=Es, Ev, Loc) ->
    invoke(Es, Ev, Loc);
parse(#es{state=[]}=Es, {startElement, _, "COLLADA", _, As0}, _) ->
    case attrs(As0) of
        #{version :=Vsn} -> Es#es{vsn=Vsn};
        _ -> Es
    end;
parse(#es{state=[]}=Es, {endElement, _, _, _}, _) ->
    Es;
parse(#es{state=[]}=Es, {startElement, _, "asset", _, _}, _) ->
    push(asset, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_materials", _, _}, _) ->
    push({lib_material, new}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_images", _, _}, _) ->
    push({lib_images, []}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_effects", _, _}, _) ->
    push({effects, new}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_geometries", _, _}, _) ->
    push({lib_geom, new}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_visual_scenes", _, _}, _) ->
    push({lib_scenes, new}, Es);
parse(#es{state=[]}=Es, {startElement, _, "scene", _, _}, _) ->
    push(scene, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_lights"=What, _, _}, _) ->
    push({ignored, What}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_cameras"=What, _, _}, _) ->
    push({ignored, What}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_animation"++_=What, _, _}, _) ->
    push({ignored, What}, Es);
parse(#es{state=[]}=Es, {startElement, _, "library_controllers"=What, _, _}, _) ->
    push({ignored, What}, Es);
parse(#es{state=[]}=Es, {startElement, _, What, _, _}, Loc) ->
    unhandled(Es, What, Loc),
    push({ignored, What}, Es);
parse(Es, Ev, Loc) ->
    unhandled(Es, Ev, Loc),
    Es.

%%%%%%%%%%%%%%%%%%%%%%%%
ignored(State, Es, {endElement, _, State,_}, _Loc) ->
    pop(Es);
ignored(_, Es, _, _Loc) ->
    Es.

%%%%%%%%%%%%%%%%%%%%%%%%
% We are only interested in top_level assets for up_axis
asset(Es, {startElement, _, "up_axis", _, _}, _) ->
    push(chars, Es);
asset(#es{val=DirStr}=Es, {endElement, _, "up_axis", _}, _) ->
    case DirStr of
        "X_UP" -> Es#es{up=x};
        "Y_UP" -> Es#es{up=y};
        "Z_UP" -> Es#es{up=z}
    end;
asset(Es, {endElement, _, "asset",_}, _Loc) ->
    pop(Es);
asset(Es, _, _) ->
    Es.

%%%%%%%%%%%%%%%%%%%%%%%%
lib_geom(new, Es, {startElement, _, "geometry", _, As0}, _) ->
    As = attrs(As0),
    replace(As, Es);
lib_geom(Data, Es, {startElement, _, "mesh", _, _}, _) ->
    push({mesh, Data#{polys=>[]}}, Es);
lib_geom(_Data, #es{val=Val}=Es, {endElement, _, "geometry",_}, _Loc) ->
    Mesh = make_mesh(Val),
    replace(new, Es#es{mesh=[Mesh|Es#es.mesh],val=undefined});
lib_geom(_Data, Es, {endElement, _, "library_geometries",_}, _) ->
    pop(Es).

%%%%%%%%%%%%%%%%%%%%%%%%
mesh(_Data, Es, {startElement, _, "source", _, As}, _) ->
    push({source, attrs(As)}, Es);
mesh(Data, #es{val=Values}=Es, {endElement, _, "source", _}, _) ->
    Id = maps:get(id, Values),
    replace(Data#{{source, Id}=>Values}, Es#es{val=undefined});
mesh(_Data, Es, {startElement, _, "vertices", _, As}, _) ->
    push({vertices, attrs(As)}, Es#es{val=[]});
mesh(Data, #es{val=Values}=Es, {endElement, _, "vertices", _}, _) ->
    replace(Data#{vertices=>Values}, Es#es{val=undefined});
mesh(_Data, Es, {startElement, _, "polylist", _, As}, _) ->
    push({polys, attrs(As, make_polys(polylist))}, Es);
mesh(_Data, Es, {startElement, _, "triangles", _, As}, _) ->
    push({polys, attrs(As, make_polys(triangles))}, Es);
mesh(_Data, Es, {startElement, _, "tristrips", _, As}, _) ->
    push({polys, attrs(As, make_polys(tristrips))}, Es);
mesh(_Data, Es, {startElement, _, "polygons", _, As}, _) ->
    push({polys, attrs(As, make_polys(polygons))}, Es);
mesh(_Data, Es, {startElement, _, "lines"=What, _, _}, _) ->
    push({ignored, What}, Es);
mesh(Data, Es, {endElement, _, "mesh",_}, _Loc) ->
    pop(Es#es{val=Data});
mesh(#{polys:=Ps, vertices:=Vs}=Data, #es{val=Vals} = Es, {endElement, _, _, _}, _) ->
    Faces = make_faces(Vals, Vs),
    replace(Data#{polys=>[Faces|Ps]}, Es#es{val=undefined}).

%%%%%%%%%%%%%%%%%%%%%%%%
source(_Data, Es, {startElement, _, "technique_common", _, _As}, _) -> Es;
source(_Data, Es, {endElement, _, "technique_common", _}, _) ->  Es;
source(_Data, Es, {startElement, _, "accessor", _, As}, _) ->
    push({param, attrs(As, #{offset=>0, stride=>1})}, Es#es{val=[]});
source(Data, #es{val=Values}=Es, {endElement, _, "accessor", _}, _) ->
    replace(Data#{accessor=>Values}, Es#es{val=undefined});
source(Data0, Es, {endElement, _, "source", _}=Ev, Loc) ->
    Data = pack_source(Data0),
    invoke(pop(Es#es{val=Data}), Ev, Loc);
source(Data, Es, {startElement, _, "float_array", _, _As}, _) ->
    %% push(chars, replace(attrs(As, Data#{type=>float}), Es));
    push(chars, replace(Data#{type=>float}, Es));
source(Data, Es, {startElement, _, "int_array", _, _As}, _) ->
    %% push(chars, replace(attrs(As, Data#{type=>int}), Es));
    push(chars, replace(Data#{type=>int}, Es));
source(Data, Es, {startElement, _, "bool_array", _, _As}, _) ->
    %%push(chars, replace(attrs(As, Data#{type=>bool}), Es));
    push(chars, replace(Data#{type=>bool}, Es));
source(Data, Es, {startElement, _, "Name_array", _, _As}, _) ->
    %%push(chars, replace(attrs(As, Data#{type=>name}), Es));
    push(chars, replace(Data#{type=>name}, Es));
source(Data, Es, {startElement, _, "IDREF_array", _, _As}, _) ->
    %%push(chars, replace(attrs(As, Data#{type=>idref}), Es));
    push(chars, replace(Data#{type=>idref}, Es));
source(#{type:=Type}=Data, #es{val=List}=Es, {endElement, _, _, _}, _) ->
    Vals = case Type of
	       float -> to_floats(List);
	       int -> [to_ints(Entry) || Entry <- List];
               name -> List
	   end,
    replace(Data#{array=>Vals}, Es#es{val=undefined}).

param(Data, #es{val=Vs}=Es, {startElement, _, "param", _, As}, _) ->
    replace(Data, Es#es{val=[attrs(As)|Vs]});
param(_Data, Es, {endElement, _, "param", _}, _) ->
    Es;
param(Data, #es{val=Vs}=Es, {endElement, _, _, _}=Ev, Loc) ->
    invoke(pop(Es#es{val=Data#{params=>lists:reverse(Vs)}}), Ev, Loc).

vertices(Data, Es, {startElement, _, "input", _, As0}, _) ->
    replace(Data, Es#es{val=[attrs(As0)|Es#es.val]});
vertices(_Data, Es, {endElement, _, "input", _}, _) -> Es;
vertices(Data, #es{val=Vals}=Es, {endElement, _, "vertices", _}=Ev, Loc) ->
    invoke(pop(Es#es{val=Data#{input=>lists:reverse(Vals)}}), Ev, Loc).

make_polys(Type) ->
    #{type=>Type, input=>[], p=>[]}.

polys(#{input:=In}=Data, Es, {startElement, _, "input", _, As}, _) ->
    replace(Data#{input=>[attrs(As)|In]}, Es);
polys(_Data, Es, {endElement, _, "input", _}, _) ->
    Es;
polys(_Data, Es, {startElement, _, "vcount", _, _}, _) ->
    push(chars, Es);
polys(Data, #es{val=List}=Es, {endElement, _, "vcount", _}, _) ->
    replace(Data#{vcount=>to_ints(List)}, Es#es{val=undefined});
polys(_Data, Es, {startElement, _, "p", _, _}, _) ->
    push(chars, Es);
polys(#{p:=P}=Data, #es{val=List}=Es, {endElement, _, "p", _}, _) ->
    replace(Data#{p:=[to_ints(List)|P]}, Es#es{val=undefined});
polys(#{input:=In,p:=P}=Data0, Es, {endElement, _, _, _}=Ev, Loc) ->
    Data = Data0#{input:=lists:reverse(In), p:=lists:reverse(P)},
    invoke(pop(Es#es{val=Data}), Ev, Loc).

%%%%%%%%%%%%%%%%%%%%%%%%
lib_material(new, Es, {startElement, _, "material", _, As0}, _) ->
    As = attrs(As0),
    replace(As, Es);
lib_material(#{id:=Id}=Attrs, #es{materials=Mat0}=Es, {endElement, _, "material",_}, _Loc) ->
    MatRef = (Mat0#mat.refs)#{Id=>Attrs},
    replace(new, Es#es{materials=Mat0#mat{refs=MatRef}});
lib_material(Attrs, Es, {startElement, _, "instance_effect", _, As0}, _) ->
    case attrs(As0) of
        #{url:=Id} -> replace(Attrs#{instance_effect=>Id}, Es)
    end;
lib_material(_, Es, {endElement, _, "instance_effect",_}, _Loc) ->
    Es;
lib_material(_, Es, {endElement, _, "library_materials",_}, _Loc) ->
    pop(Es).

%%%%%%%%%%%%%%%%%%%%%%%%
lib_images(Data, Es, {startElement, _, "image", _, As}, _) ->
    replace([attrs(As)|Data], Es);
lib_images(_Data, Es, {startElement, _, "init_from", _, _As}, _) ->
    push(chars, Es);
lib_images([Head|Data], #es{val=Val}=Es, {endElement, _, "init_from", _}, _) ->
    replace([Head#{file=>Val}|Data], Es#es{val=undefined});
lib_images(_, Es, {endElement, _, "image", _}, _) ->
    Es;
lib_images(Data, #es{materials=Mat}=Es, {endElement, _, "library_images",_}, _Loc) ->
    Images = maps:from_list([{Id, Image} || #{id:=Id} = Image <- Data]),
    pop(Es#es{materials=Mat#mat{images=Images}}).

%%%%%%%%%%%%%%%%%%%%%%%%
effects(new, Es, {startElement, _, "effect", _, As0}, _) ->
    As = attrs(As0),
    replace(As, Es);
effects(#{id:=Id}=Attrs, #es{materials=Mat0}=Es, {endElement, _, "effect", _}, _) ->
    MatDef = (Mat0#mat.defs)#{Id=>Attrs},
    replace(new, Es#es{materials=Mat0#mat{defs=MatDef}});
effects(_Data, Es, {startElement, _, "profile_COMMON", _,_}, _) ->
    Es;
effects(Data, Es, {startElement, _, "technique", _,_}, _) ->
    push({common_mat, Data}, Es);
effects(_Data, Es, {endElement, _, "technique", _}, _) ->
    replace(Es#es.val, Es#es{val=undefined});
effects(_Data, Es, {startElement, _, "newparam", _, As0}, _) ->
    push({common_newparam, attrs(As0)}, Es);
effects(Data, #es{val=Val}=Es, {endElement, _, "newparam", _}, _) ->
    #{sid:=Sid}=Val,
    replace(Data#{Sid=>Val}, Es#es{val=undefined});

effects(new, Es, {endElement, _, "library_effects",_}, _Loc) ->
    pop(Es);
effects(_Data, Es, {endElement, _, _, _}, _) ->
    Es.

%%%%%%%%%%%%%%%%%%%%%%%%
scene(#es{scenes=Ss}=Es, {startElement, _, "instance_visual_scene", _, As}, _) ->
    #{url:=[$#|SceneId]} = attrs(As),
    {_, Scene} = lists:keyfind(SceneId,1,Ss),
    Es#es{scene=Scene};
scene(Es, {endElement, _, "instance_visual_scene", _}, _) ->
    Es;
scene(Es, {endElement, _, "scene", _}, _) ->
    pop(Es).

lib_scenes(new, Es, {startElement, _, "visual_scene", _, As}, _) ->
    replace(attrs(As, #{nodes=>[]}), Es);
lib_scenes(_, Es, {startElement, _, "node", _, As}, _) ->
    push({node, attrs(As, #{matrix=>[], geom=>[], mats=>#{}})}, Es);
lib_scenes(#{nodes:=Ns}=Data, #es{val=Val}=Es, {endElement, _, "node", _}, _) ->
    case maps:get(geom, Val, []) of
	[] -> Es;
	_ -> replace(Data#{nodes:=[Val|Ns]}, Es#es{val=undefined})
    end;
lib_scenes(#{id:=Id}=Data, #es{scenes=Ss}=Es, {endElement, _, "visual_scene", _}, _) ->
    replace(new, Es#es{scenes=[{Id, Data}|Ss]});
lib_scenes(new, Es, {endElement, _, "library_visual_scenes", _}, _) ->
    pop(Es).

node(_Data, Es, {startElement, _, "technique_common", _, _As}, _) -> Es;
node(_Data, Es, {endElement, _, "technique_common", _}, _) ->  Es;
node(_Data, Es, {startElement, _, "bind_material", _, _As}, _) -> Es;
node(_Data, Es, {endElement, _, "bind_material", _}, _) ->  Es;
node(#{matrix:=M}, Es, {startElement, _, "translate", _, _}, _) ->
    push(chars, push({matrix, {translate, M}}, Es));
node(#{matrix:=M}, Es, {startElement, _, "rotate", _, _As}, _) ->
    push(chars, push({matrix, {rotate, M}}, Es));
node(#{matrix:=M}, Es, {startElement, _, "scale", _, _As}, _) ->
    push(chars, push({matrix, {scale, M}}, Es));
node(Data, #es{val=Val}=Es, end_matrix, _) ->
    replace(Data#{matrix:=lists:reverse(Val)}, Es#es{val=undefined});
node(#{geom:=Urls}=Data, Es, {startElement, _, "instance_geometry", _, As}, _) ->
    #{url:=Url} = attrs(As),
    replace(Data#{geom=>[Url|Urls]}, Es);
node(#{mats:=Mats}=Data, Es, {startElement, _, "instance_material", _, As}, _) ->
    #{symbol:=Key, target:=Target} = attrs(As),
    replace(Data#{mats:=Mats#{Key=>Target}}, Es);
node(Data, Es, {endElement, _, "node", _}=Ev, Loc) ->
    invoke(pop(Es#es{val=Data}), Ev, Loc);
node(_, Es, {endElement, _, _, _}, _) ->
    Es;
node(_, Es, {startElement, _, What, _, _}, _) ->
    %% Ignore camera and lights for now
    %% io:format("~p:~p:~p: ignore ~p~n",[?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    push({ignored, What}, Es).

matrix({Type,Data}, #es{val=Val}=Es, {endElement, _, _, _}, Loc) ->
    Floats = to_floats(Val),
    invoke(pop(Es#es{val=[{Type, Floats}|Data]}), end_matrix, Loc).

%%%%%%%%%%%%%%%%%%%%%%%%
common_mat(Data, Es, {startElement, _, TechOrColor, _, _As0}, _) ->
    case maps:is_key(type, Data) of
        false -> replace(Data#{type=>TechOrColor}, Es);
        true  -> push(sloppy_color, Es)
    end;
common_mat(#{type:=Type}=Data, #es{val=Val}=Es, {endElement, _, What, _}, _) ->
    case What of
        Type -> pop(Es#es{val=Data});
        _ -> replace(Data#{list_to_atom(What)=>Val}, Es#es{val=undefined})
    end.

common_newparam(Data, Es, {startElement, _, "surface", _, As}, _) ->
    push({surface, attrs(As, Data)}, Es);
common_newparam(Data, Es, {startElement, _, "sampler2D", _, As}, _) ->
    push({sampler2D, attrs(As, Data)}, Es);
common_newparam(_Data, Es, {endElement, _, "newparam", _}=Ev, Loc) ->
    invoke(pop(Es), Ev, Loc);
common_newparam(_Data, Es, {endElement, _, _, _}, _) ->
    Es.

surface(_Data, Es, {startElement, _, _, _, _}, _) ->
    push(chars, Es);
surface(Data, Es, {endElement, _, "surface", _}, _) ->
    pop(Es#es{val=Data});
surface(Data, Es, {endElement, _, What, _}, _) ->
    replace(Data#{What=>Es#es.val}, Es).

sampler2D(_Data, Es, {startElement, _, _, _, _}, _) ->
    push(chars, Es);
sampler2D(Data, Es, {endElement, _, "sampler2D", _}, _) ->
    pop(Es#es{val=Data});
sampler2D(Data, Es, {endElement, _, What, _}, _) ->
    replace(Data#{What=>Es#es.val}, Es).

%%%%%%%%%%%%%%%%%%%%%%%%
sloppy_color(Es, {startElement, _, "color", _, _}, _) ->
    push(chars,Es);
sloppy_color(Es, {startElement, _, "float", _, _}, _) ->
    push(chars,Es);
sloppy_color(Es, {startElement, _, "texture", _, As}, _) ->
    Es#es{val=attrs(As)};
sloppy_color(Es, {endElement, _, "texture", _}, _) ->
    pop(Es);
sloppy_color(#es{val=List}=Es, {endElement, _, What, _}, _) ->
    case lists:member(What, ["color", "float"]) of
        true ->
            Col = case to_floats(List) of
                      [Val] -> Val;
                      [R,G,B] -> {R,G,B,1.0};
                      [R,G,B,A] -> {R,G,B,A}
                  end,
            pop(Es#es{val=Col});
        false ->
            Es
    end.

chars(Es, {characters, List}, _) ->
    pop(Es#es{val=List});
chars(#es{state=[_,{source, #{type:=Prev}=Data}|_]}=Es, {endElement, _, Type, _}=Ev, Where) ->
    case Prev of
        float when Type =:= "float_array" -> ?MODULE:source(Data, pop(Es#es{val=[]}), Ev, Where);
        int   when Type =:= "int_array"   -> ?MODULE:source(Data, pop(Es#es{val=[]}), Ev, Where);
        bool  when Type =:= "bool_array"  -> ?MODULE:source(Data, pop(Es#es{val=[]}), Ev, Where);
        name  when Type =:= "Name_array"  -> ?MODULE:source(Data, pop(Es#es{val=[]}), Ev, Where);
        idref when Type =:= "IDREF_array" -> ?MODULE:source(Data, pop(Es#es{val=[]}), Ev, Where);
        _ ->
            unhandled(Es, Ev, Where),
            Es
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unhandled(#es{state=[]}, Ev, {_,File,Line}) ->
    io:format("~s:~p ignored:   ~P~n", [File, Line, strip_chars(Ev), 20]);
unhandled(#es{state=State0}, Ev, {_,File,Line}) ->
    State = [fun({S,_}) -> S;(S) -> S end(St) || St <- State0],
    io:format("~s:~p ~p ignored:~n\t ~P~n", [File, Line, State, strip_chars(Ev), 20]).

strip_chars({characters, List}=What) ->
    try lists:split(50, List) of
	{First, _} -> {characters, First++"...."}
    catch _:_ -> What end;
strip_chars(Other) -> Other.

attrs(As) ->
    attrs(As, #{}).

attrs(As, Map0) ->
    Add = fun({_, _, Name, Value}, Map) ->
		  Map#{list_to_atom(Name)=>attr_value(Value)}
	  end,
    lists:foldl(Add, Map0, As).

attr_value(Val) ->
    try list_to_integer(Val)
    catch _:_ -> Val end.


invoke(#es{state=[Top|_]}=Es, Ev, Loc) ->
    try
	case Top of
	    {State, Data} ->
		?MODULE:State(Data, Es, Ev, Loc);
	    State when is_atom(State) ->
		?MODULE:State(Es, Ev, Loc)
	end
    catch
        error:function_clause=Reason ->
	    case erlang:get_stacktrace() of
		[{?MODULE,Func, _,_}|_] when Func =:= Top ->
		    unhandled(Es, Ev, Loc),
		    Es;
		[{?MODULE,Func, _,_}|_] when Func =:= element(1,Top) ->
		    unhandled(Es, Ev, Loc),
		    Es;
		_ ->
		    io:format("~p:~n ~P~n", [Reason, erlang:get_stacktrace(), 20]),
		    io:format("Last: ~P~n~P~n",[Ev,5,Es,10]),
		    throw({fatal_error, parser_error})
	    end;
        error:Reason ->
	    io:format("~p:~n ~P~n", [Reason, erlang:get_stacktrace(), 20]),
	    io:format("Last: ~P~n~P~n",[Ev,5,Es,10]),
	    throw({fatal_error, parser_error})
    end.

push(State,#es{state=Stack}=Es) -> Es#es{state=[State|Stack]}.
pop(#es{state=[_|Stack]}=Es) -> Es#es{state=Stack}.
replace(Data,#es{state=[{State,_Data}|Stack]}=Es) -> Es#es{state=[{State,Data}|Stack]}.

to_floats(List) ->
    [make_float(Str) || Str <- string:tokens(List, " \n\t")].

make_float(Str) ->
    try list_to_float(Str)
    catch _:_ -> make_float2(Str)
    end.
make_float2(Str) ->
    try float(list_to_integer(Str))
    catch _:_ -> make_float3(Str)
    end.

make_float3(Str0) ->
    Str = lists:flatten(string:tokens(Str0, "\t\s\n")),
    try list_to_float(Str)
    catch _:_ -> make_float4(Str, Str0) end.

make_float4(Str, Orig) ->
    WithDot = case string:tokens(Str, "e") of
		  [Pre,Post] -> Pre ++ ".0e" ++ Post;
		  [Other] -> Other ++ ".0";
		  Other -> Other
	      end,
    try list_to_float(WithDot)
    catch _:badarg ->
	    io:format("Bad float: ~p ~p ~p~n",[Orig, Str, WithDot]),
	    throw({fatal_error, {bad_float, Orig}})
    end.

to_ints(List) ->
    [list_to_integer(Str) || Str <- string:tokens(List, " ")].

to_tuple(1, List) -> List;

to_tuple(2, List) -> to_tuple2(List);
to_tuple(3, List) -> to_tuple3(List);
to_tuple(4, List) -> to_tuple4(List);
to_tuple(5, List) -> to_tuple5(List).

to_tuple2([A,B|Rest]) -> [{A,B}|to_tuple2(Rest)];
to_tuple2([]) -> [].

to_tuple3([A,B,C|Rest]) -> [{A,B,C}|to_tuple3(Rest)];
to_tuple3([]) -> [].

to_tuple4([A,B,C,D|Rest]) -> [{A,B,C,D}|to_tuple4(Rest)];
to_tuple4([]) -> [].

to_tuple5([A,B,C,D,E|Rest]) -> [{A,B,C,D,E}|to_tuple5(Rest)];
to_tuple5([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%
pack_source(#{accessor:=#{offset:=0, stride:=Stride}, array:=List}=Source) ->
    Source#{array:=to_tuple(Stride, List)}.

make_mesh(#{id:=Id, polys:=Polys}=Geom) ->
    %% io:format("~p~n",[Geom]),
    {Type,Mesh0} = lists:foldl(fun pick_mesh/2, {undefined, #e3d_mesh{}}, Polys),
    Mesh1 = pick_source(Mesh0#e3d_mesh{type=Type}, Geom),
    {Id,Mesh1}.

pick_source(#e3d_mesh{vs=Vs, vc=VC, tx=Tx, ns=Ns}=M, Geom) ->
    M#e3d_mesh{vs=pick_src_1(Vs,Geom), vc=pick_src_1(VC, Geom),
	       tx=pick_src_1(Tx,Geom), ns=pick_src_1(Ns, Geom)}.

pick_src_1([], _Geom) -> [];
pick_src_1([$#|Src], Geom) ->
    #{array:=Data} = maps:get({source, Src}, Geom),
    Data.

pick_mesh(#{input:=In, p:=P, type:=Type}, {MT, #e3d_mesh{fs=MFs}=Mesh0}) ->
    Mesh = pick_mesh_1(In, Mesh0),
    {mesh_type(MT, Type), Mesh#e3d_mesh{fs=P++MFs}}.

pick_mesh_1([#{semantic:="POSITION", source:=Src}|In], #e3d_mesh{vs=OldSrc}=M) ->
    case OldSrc of
	[] -> pick_mesh_1(In, M#e3d_mesh{vs=Src});
	Src -> pick_mesh_1(In, M)
    end;
pick_mesh_1([#{semantic:="NORMAL", source:=Src}|In], #e3d_mesh{ns=OldSrc}=M) ->
    case OldSrc of
	[] -> pick_mesh_1(In, M#e3d_mesh{ns=Src});
	Src -> pick_mesh_1(In, M)
    end;
pick_mesh_1([#{semantic:="TEXCOORD", source:=Src}|In], #e3d_mesh{tx=OldSrc}=M) ->
    case OldSrc of
	[] -> pick_mesh_1(In, M#e3d_mesh{tx=Src});
	Src -> pick_mesh_1(In, M)
    end;
pick_mesh_1([#{semantic:="COLOR", source:=Src}|In], #e3d_mesh{vc=OldSrc}=M) ->
    case OldSrc of
	[] -> pick_mesh_1(In, M#e3d_mesh{vc=Src});
	Src -> pick_mesh_1(In, M)
    end;
pick_mesh_1([_|In], Mesh) ->
    pick_mesh_1(In, Mesh);
pick_mesh_1([], Mesh) -> Mesh.

mesh_type(undefined, Type) -> Type;
mesh_type(Type, Type) -> Type;
mesh_type(_, _) -> polygon.

polygon_type([#e3d_face{vs=Vs}|Fs]) ->
    polygon_type_1(Fs, length(Vs)).

polygon_type_1([#e3d_face{vs=Vs}|Fs], Sz)
  when length(Vs) =:= Sz ->
    polygon_type_1(Fs, Sz);
polygon_type_1([_|_], _) -> polygon;
polygon_type_1([], 3) -> triangle;
polygon_type_1([], 4) -> quad;
polygon_type_1(_, _) -> polygon_type.

make_faces(#{type:=polylist, input:=In0, count:=FC, vcount:=VC, p:=Ps}=Data, #{input:=In1}) ->
    In = sort_inputs(In1, In0),
    Mat = [maps:get(material, Data, default)],
    Fs = pick_faces(VC, In, hd(Ps), Mat, []),
    FC = length(Fs), %% assert
    Data#{p:=Fs, type:=polygon_type(Fs), input:=[I||{_,I}<-In]};
make_faces(#{type:=triangles, input:=In0, count:=FC, p:=Ps}=Data, #{input:=In1}) ->
    In = sort_inputs(In1, In0),
    Mat = [maps:get(material, Data, default)],
    Fs = pick_faces(3, In, hd(Ps), Mat, []),
    FC = length(Fs), %% assert
    Data#{p:=Fs, type:=triangle, input:=[I||{_,I}<-In]};
make_faces(#{type:=tristrips, input:=In0, p:=Ps}=Data, #{input:=In1}) ->
    In = sort_inputs(In1, In0),
    Mat = [maps:get(material, Data, default)],
    Fs = pick_tristrips(In, true, Ps, Mat, []),
    Data#{p:=Fs, type:=triangle, input:=[I||{_,I}<-In]};
make_faces(#{type:=polygons, count:=FC, input:=In0, p:=Ps}=Data, #{input:=In1}) ->
    In = sort_inputs(In1, In0),
    Mat = [maps:get(material, Data, default)],
    Fs = pick_polygons(Ps, In, Mat, []),
    FC = length(Fs),
    Data#{p:=Fs, type:=polygon_type(Fs), input:=[I||{_,I}<-In]}.

sort_inputs(GIn0, PIn0) ->
    PIn1 = [{Sem, Map} || #{semantic:=Sem}=Map <- PIn0],
    In = case lists:keytake("VERTEX", 1, PIn1) of
	     false -> [{maps:get(offset,Map,0),Map} || Map <- GIn0];
	     {value, {_, #{offset:=Offset}}, PIn2} ->
		 PIn = [Map || {_,Map} <- PIn2],
		 GIn = [Map#{offset=>Offset} || Map <- GIn0],
		 [{maps:get(offset,Map,0),Map} || Map <- GIn++PIn]
	 end,
    remove_duplicates(lists:sort(In)).

%% Wings can only handle one set of UV's coords vertex colors and so on
remove_duplicates([{_, #{semantic:=Sem}}=First|In]) ->
    Rest = lists:map(fun({OS, #{semantic:=S}=M}) when S =:= Sem ->
			     {OS, M#{semantic:="IGNORE_"++S}};
			(M) -> M
		     end, In),
    [First | remove_duplicates(Rest)];
remove_duplicates([]) -> [].

%%
pick_faces(VC, _, [], _, Fs) ->
    true = (VC =:= []) orelse is_number(VC), %% assert
    lists:reverse(Fs);
pick_faces([VC|N], In, Ps0, Mat, Fs) ->
    {Face, Ps} = pick_verts(VC, In, Ps0, #e3d_face{mat=Mat}),
    pick_faces(N, In, Ps, Mat, [rev_face(Face)|Fs]);
pick_faces(VC, In, Ps0, Mat, Fs) when is_integer(VC) ->
    %% io:format("~p ~p~n", [VC, length(Ps0)]),
    {Face, Ps} = pick_verts(VC, In, Ps0, #e3d_face{mat=Mat}),
    pick_faces(VC, In, Ps, Mat, [rev_face(Face)|Fs]).

%%
pick_tristrips(In, Rev, [Ps|Next], Mat, Fs) ->
    {FI0, Ps1} = pick_vert(0, In, Ps, #e3d_face{mat=Mat}),
    case pick_verts(2, In, Ps1, FI0) of
        {Face, []} ->
            pick_tristrips(In, true, Next, Mat, [rev_face(Rev,Face)|Fs]);
        {Face, _} ->
            pick_tristrips(In, not Rev, [Ps1|Next], Mat, [rev_face(Rev,Face)|Fs])
    end;
pick_tristrips(_In, _, [], _, Fs) ->
    Fs.

%%
pick_polygons([Poly|Ps], In, Mat, Fs) ->
    Face = pick_polygon(In, Poly, #e3d_face{mat=Mat}),
    pick_polygons(Ps, In, Mat, [Face|Fs]);
pick_polygons([], _In, _Mat, Fs) -> Fs.

pick_polygon(_In, [], Face) ->
    rev_face(Face);
pick_polygon(In, Ps0, Face0) ->
    {Face, Ps} = pick_vert(0, In, Ps0, Face0),
    pick_polygon(In, Ps, Face).

%%
pick_verts(VC, In, Ps0, FI0) when VC > 0 ->
    {FI, Ps} = pick_vert(0, In, Ps0, FI0),
    pick_verts(VC-1, In, Ps, FI);
pick_verts(0, _, Ps, Face) ->
    {Face, Ps}.

pick_vert(Offset, [{Offset,What}|In], [P|_]=Ps, FI0) ->
    FI = add_vert_info(What, P, FI0),
    pick_vert(Offset, In, Ps, FI);
pick_vert(_, [], [_|Ps], FI) -> {FI, Ps};
pick_vert(Offset, [{Next,_}|_]=In, [_P|Ps], FI) when Offset < Next ->
    pick_vert(Offset+1, In, Ps, FI).

add_vert_info(#{semantic:="POSITION"}, Indx, F=#e3d_face{vs=Is}) ->
    F#e3d_face{vs=[Indx|Is]};
add_vert_info(#{semantic:="NORMAL"}, Indx, F=#e3d_face{ns=Is}) ->
    F#e3d_face{ns=[Indx|Is]};
add_vert_info(#{semantic:="COLOR"}, Indx, F=#e3d_face{vc=Is}) ->
    F#e3d_face{vc=[Indx|Is]};
add_vert_info(#{semantic:="TEXCOORD"}, Indx, F=#e3d_face{tx=Is}) ->
    F#e3d_face{tx=[Indx|Is]};
add_vert_info(_In, _Indx, F) ->
    %% Ignore stuff we don't know about
    %% io:format("Ignore: ~p~n", [maps:get(semantic, _In)]),
    F.

rev_face(false, Face) -> Face;
rev_face(true, Face) -> rev_face(Face).

rev_face(#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns,mat=Mat}) ->
    #e3d_face{vs=lists:reverse(Vs),vc=lists:reverse(Vc),
              tx=lists:reverse(Tx),ns=lists:reverse(Ns),
	      mat=Mat}.

make_materials(#mat{refs=Refs, defs=Defs, images=Imgs}=_Mat) ->
    %% io:format("Scene: ~p ~n", [_Scene]),
    [{Id,make_material(Ref, Defs, Imgs)} || #{id:=Id}=Ref <- maps:values(Refs)].

make_material(#{id:=Id, instance_effect:=[$#|Effect]}=Ref, Defs, Imgs) ->
    Name = maps:get(name, Ref, Id),
    Def  = maps:get(Effect, Defs),
    Shininess = case prop_get(shininess, Def, 0.0) of
		    Shin when Shin > 1.0 -> %% Assume OpenGL where 128 is max
			Shin / 128;
		    Shin -> Shin
		end,
    DefList = [{diffuse,prop_get(diffuse, Def, {1.0, 1.0, 1.0, 1.0})},
	       {ambient,prop_get(ambient, Def, {1.0, 1.0, 1.0, 1.0})},
	       {specular,prop_get(specular, Def, {0.0,0.0,0.0,0.0})},
	       {emission,prop_get(emission, Def, {0.0,0.0,0.0,0.0})},
	       {shininess, Shininess},
	       {vertex_colors,multiply}],
    Textures = case maps:get(diffuse, Def, false) of
		   #{texture:=ImageRef} ->
		       make_maps(ImageRef, Def, Imgs);
		   _ -> []
	       end,
    %% io:format("Name: ~p Maps ~p~n", [Name, Textures]),
    {atom_name(Name), [{opengl, DefList}|Textures]}.

prop_get(What, Def, Default) ->
    case maps:get(What, Def, undefined) of
	{_,_,_,_}=Col -> Col;
	Float when is_float(Float) -> Float;
	_Other ->
	    %% io:format("Ignored color ~p:~n",[_Other]),
	    Default
    end.

make_maps(ImageRef, Def, Images) ->
    case make_maps_1(ImageRef, Def, Images) of
	undefined -> [];
	File -> [{maps, [{diffuse, File}]}]
    end.

make_maps_1(ImageRef, Def, Imgs) ->
    case maps:get(ImageRef, Def, undefined) of
	undefined ->
	    case maps:get(ImageRef, Imgs, undefined) of
		#{file:=File} -> File;
		_Fail -> undefined
	    end;
	#{"source":=Source} ->
	    make_maps_1(Source, Def, Imgs);
	#{"init_from":=FileRef} ->
	    make_maps_1(FileRef, Def, Imgs)
    end.

make_file(#es{materials=Mat, scene=#{nodes:=Nodes}, mesh=Mesh0}) ->
    MatList0 = make_materials(Mat),
    MatMap = maps:from_list(MatList0),
    MeshMap = maps:from_list(Mesh0),
    MeshL = build_meshes(Nodes, MeshMap, MatMap, []),
    #e3d_file{objs=MeshL, mat=maps:values(MatMap)}.

build_meshes([#{geom:=[Gs], mats:=Mats0, matrix:=MatrixInfo}=Node|Ns],
	     MeshMap, MatMap, Acc) ->
    [$#|GsId] = Gs,
    NodeId = maps:get(name, Node, "unknown"),
    Name = maps:get(name, Node, NodeId),
    #{GsId:=#e3d_mesh{fs=Fs0}=Mesh} = MeshMap,
    Mats = maps:map(fun(_K,[$#|Ref]) ->
			    {MId,_} = maps:get(Ref, MatMap, {default, ignore}),
			    MId
		    end, Mats0),
    Translate = fun(#e3d_face{mat=[default]}=F) ->
			F;
		   (#e3d_face{mat=[MId]}=F) ->
			F#e3d_face{mat=[maps:get(MId, Mats)]}
		end,
    Fs = [Translate(F) || F <- Fs0],
    Matrix = lists:foldl(fun make_matrix/2, e3d_mat:identity(), MatrixInfo),
    Obj = #e3d_object{name=Name, obj=Mesh#e3d_mesh{fs=Fs, matrix=Matrix}},
    build_meshes(Ns, MeshMap, MatMap, [Obj|Acc]);
build_meshes([], _, _, Acc) -> Acc.

make_matrix({rotate, [_,_,_,0.0]}, M) ->
    M;
make_matrix({rotate, [X,Y,Z,Rad]}, M) ->
    Deg = Rad*180.0/math:pi(),
    e3d_mat:mul(e3d_mat:rotate(Deg, {X,Y,Z}), M);
make_matrix({translate, [0.0,0.0,0.0]}, M) ->
    M;
make_matrix({translate, [X,Y,Z]}, M) ->
    e3d_mat:mul(e3d_mat:translate(X,Y,Z), M);
make_matrix({scale, [1.0,1.0,1.0]}, M) ->
    M;
make_matrix({scale, [X,Y,Z]}, M) ->
    e3d_mat:mul(e3d_mat:scale(X,Y,Z), M).


-ifdef(TEST).
test() ->
    Dir = case os:type() of
	      {unix, darwin} -> "/Users/dgud/Dropbox/src/Collada/examples";
	      {win32, _} -> "c:/Users/familjen/Dropbox/src/Collada/examples";
	      _ -> "foo"
	  end,
    Files = [
	     "wings-box-2mat.dae"
	     ,"AsXML.xml"
             ,"COLLADA.dae"
             ,"COLLADA_triangulate.dae"
             ,"Cinema4D.dae"
             ,"ConcavePolygon.dae"
	     ,"anims_with_full_rotations_between_keys.DAE"
	     ,"cameras.dae"
             ,"cube_UTF16LE.dae"
             ,"cube_UTF8BOM.dae"
             ,"cube_emptyTags.dae"
             ,"cube_triangulate.dae"
             ,"cube_tristrips.dae"
             ,"cube_with_2UVs.DAE"
             ,"cube_xmlspecialchars.dae"
             ,"duck.dae"
             ,"duck_triangulate.dae"
             ,"earthCylindrical.DAE"
             ,"kwxport_test_vcolors.dae"
             ,"library_animation_clips.dae"
             ,"lights.dae"
             ,"regr01.dae"
             ,"sphere.dae"
             ,"sphere_triangulate.dae"
             ,"teapot_instancenodes.DAE"
             ,"teapots.DAE"
	     ,"wings.dae"
	    ],
    Imports = [import(filename:join(Dir, File)) || File <- Files],
    %% [io:format("~P~n",[Scene,20]) || Scene <- Imports],
    io:format("~p~n",[hd(Imports)%%    , 20]),
		      ]),
    ok.
-endif.

%%%%
%%%%   Provisory convertion from UTF16/32 - it make possible load a file
%%%%   which materials name/id uses unicode
%%%%   reported in: http://www.wings3d.com/forum/showthread.php?tid=2321
atom_name(Name) ->
    try list_to_atom(Name) of
        Atom -> Atom
    catch
        error:_ ->
            Name0 = unicode:characters_to_binary(Name),
            binary_to_atom(Name0,latin1)
    end.
