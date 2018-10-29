%%
%%  wings_util.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%% Note: To keep the call graph clean, wings_util MUST NOT call
%%       other wings_* modules (except wings_pref).

-module(wings_util).
-export([share/1,share/3,make_vector/1,
	 rel2fam/1,
	 format/2,
	 key_format/2,
	 cap/1,upper/1,stringify/1,quote/1,
	 add_vpos/2,update_vpos/2,
	 gb_trees_smallest_key/1,gb_trees_largest_key/1,
	 array_keys/1,array_smallest_key/1,array_greatest_key/1,
	 array_is_empty/1,array_entries/1,
	 mapsfind/3,
	 wxequal/2, wxset_pid/2, min_wx/1,
	 nice_float/1,nice_vector/1,nice_abs_vector/1,
	 unique_name/2,
	 is_name_masked/2,
	 lib_dir/1,
	 tc/3, profile_start/1, profile_stop/1,
	 limit/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-import(lists, [foldl/3,reverse/1,member/2,last/1]).

share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.

share({X,X,X}) -> {X,X,X};
share({X,X,Z}) -> {X,X,Z};
share({X,Y,Y}) -> {X,Y,Y};
share({X,Y,X}) -> {X,Y,X};
%%
share({X,X,X,X}) -> {X,X,X,X};
%%
share({X,X,X,A}) -> {X,X,X,A};
share({X,X,Z,X}) -> {X,X,Z,X};
share({X,Y,X,X}) -> {X,Y,X,X};
share({X,Y,Y,Y}) -> {X,Y,Y,Y};
%%
share({X,X,Y,Y}) -> {X,X,Y,Y};
share({X,Y,X,Y}) -> {X,Y,X,Y};
share({X,Y,Y,X}) -> {X,Y,Y,X};
%%
share({X,X,Z,A}) -> {X,X,Z,A};
share({X,Y,X,A}) -> {X,Y,X,A};
share({X,Y,Z,X}) -> {X,Y,Z,X};
share({X,Y,Y,A}) -> {X,Y,Y,A};
share({X,Y,Z,Y}) -> {X,Y,Z,Y};
share({X,Y,Z,Z}) -> {X,Y,Z,Z};
%%
share(Other) -> Other.

make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(free) -> free;
make_vector(normal) -> normal;
make_vector(intrude) -> normal;
make_vector({region,normal}) -> normal;
make_vector(Axis) when Axis == last_axis; Axis == default_axis ->
    {_,Vec} = wings_pref:get_value(Axis),
    Vec.


key_format(Key, Msg) ->
    [Key,160,Msg].

%% Like io_lib:format/2, but with very restricted format characters.
%% BUT allows arguments to ~s to be lists containing Unicode characters.
%% Now allows ~ts for translation of Asian glyphs.
%%
%% Format directives allowed: ~s ~p

format(Format, Args) ->
    format_1(Format, Args, []).

rel2fam(Rel) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(Rel))).

quote(Str) when is_list(Str) ->
    [$",Str,$"].

stringify({Atom,Other}) when is_atom(Atom) ->
    translation_string(Atom) ++
    case stringify(Other) of
        [] -> [];
        Str -> "|" ++ Str
    end;
stringify(Atom) when is_atom(Atom) ->
    translation_string(Atom);
stringify(Int) when is_integer(Int) ->
    integer_to_list(Int);
stringify(_Other) -> [].

cap(Str) when is_atom(Str) -> cap(atom_to_list(Str));
cap(Str) -> cap(Str, true).

cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], _Any) ->
    [$\s|cap(T, true)];
cap([H|T], _Any) ->
    [H|cap(T, false)];
cap([], _Flag) -> [].
    
upper(Str) when is_atom(Str) -> upper(atom_to_list(Str));
upper([Lower|T]) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|upper(T)];
upper([H|T]) ->
    [H|upper(T)];
upper([]) -> [].

add_vpos(Vs, #we{vp=Vtab}) -> add_vpos(Vs, Vtab);
add_vpos(Vs, Vtab) ->
    foldl(fun(V, A) ->
		  [{V,array:get(V, Vtab)}|A]
	  end, [], Vs).

update_vpos(Vs, #we{vp=Vtab}) -> update_vpos(Vs, Vtab);
update_vpos(Vs, Vtab) ->
    foldl(fun({V,_}, A) ->
		  [{V,array:get(V, Vtab)}|A];
	     ({V,_,Dist,Inf}, A) ->
		  [{V,array:get(V, Vtab),Dist,Inf}|A]
	  end, [], reverse(Vs)).

gb_trees_smallest_key(Tree) ->
    {Key,_Val} = gb_trees:smallest(Tree),
    Key.

gb_trees_largest_key(Tree) ->
    {Key,_Val} = gb_trees:largest(Tree),
    Key.

array_keys(Array) ->
    array:sparse_foldr(fun(I, _, A) -> [I|A] end, [], Array).

array_smallest_key(Array) ->
    try
	array:sparse_foldl(fun(I, _, _) -> throw(I) end, [], Array),
	error(empty_array)
    catch
	throw:I when is_integer(I) ->
	    I
    end.

array_greatest_key(Array) ->
    try
	array:sparse_foldr(fun(I, _, _) -> throw(I) end, [], Array),
	error(empty_array)
    catch
	throw:I when is_integer(I) ->
	    I
    end.

array_is_empty(Array) ->
    try
	array:sparse_foldr(fun(_, _, _) -> throw(false) end, [], Array),
	throw(true)
    catch
	throw:Empty ->
	    Empty
    end.

%% array_entries(Array) -> NumberOfEntries
%%  Return the number of non-default entries in the array.
%%
array_entries(Array) ->
    array:sparse_foldl(fun(_, _, N) -> N + 1 end, 0, Array).

-spec nice_abs_vector(e3d_vec:vector()) -> iolist().

nice_abs_vector({X,Y,Z}) ->
    nice_vector({abs(X),abs(Y),abs(Z)}).

-spec nice_vector(e3d_vec:vector()) -> iolist().

nice_vector({X,Y,Z}) ->
    ["<",
     wings_util:nice_float(X),"  ",
     wings_util:nice_float(Y),"  ",
     wings_util:nice_float(Z),
     ">"].

nice_float(F) when is_float(F) ->
    simplify_float(lists:flatten(io_lib:format("~f", [F]))).

simplify_float(F) ->
    reverse(simplify_float_1(reverse(F))).

simplify_float_1("0."++_=F) -> F;
simplify_float_1("0"++F) -> simplify_float_1(F);
simplify_float_1(F) -> F.

%%
%% Finds the first map containing Key:=Value
%%
-spec mapsfind(term(), term(), list()) -> map()|false.
mapsfind(Value, Key, [H|T]) ->
    case H of
	#{Key:=Value} -> H;
	_ -> mapsfind(Value, Key, T)
    end;
mapsfind(_, _, []) -> false.


%% Missing wx funcs
-record(wx_ref, {ref, type, state}).
wxequal(#wx_ref{ref=Ref1}, #wx_ref{ref=Ref2}) -> Ref1 =:= Ref2.
wxset_pid(#wx_ref{}=R, Pid) when is_pid(Pid) ->
    R#wx_ref{state=Pid}.

min_wx({_,_}=Ver) ->
    case get(wx_version) of
        undefined ->
            case filename:basename(code:lib_dir(wx)) of
                [$w,$x,$-,Major,_,Minor|_] ->
                    Version = {Major-$0, Minor-$0},
                    put(wx_version, Version),
                    Version >= Ver;
                _Vsn -> %% erlang src build? assume 19.2 or later
                    true
            end;
        Version ->
            Version >= Ver
    end.

%%
%% Create a unique name by appending digits.
%%

unique_name(Name, Names) ->
    case member(Name, Names) of
	false -> Name;
	true -> unique_name_1(reverse(Name), Names)
    end.

unique_name_1([C|Cs], Names) when $0 =< C, C =< $9, Cs /= [] ->
    unique_name_1(Cs, Names);
unique_name_1(Name, Names0) ->
    Base0 = [First|_] = reverse(Name),
    Names = [N || N <- Names0, hd(N) =:= First],
    Base = case member($\s, Base0) andalso last(Base0) =/= $\s of
	       true -> Base0 ++ " ";
	       false -> Base0
	   end,
    unique_name_2(Base, 2, gb_sets:from_list(Names)).

unique_name_2(Base, I, Names) ->
    Name = Base ++ integer_to_list(I),
    case gb_sets:is_member(Name, Names) of
	true -> unique_name_2(Base, I+1, Names);
	false -> Name
    end.

tc(Fun,Mod,Line) ->
    Before = os:timestamp(),
    R = Fun(),
    After = os:timestamp(),
    io:format("~p:~p: Time: ~p\n", [Mod, Line, timer:now_diff(After,Before)]),
    R.

profile_start(fprof) ->
    fprof:trace(start),
    ok;
profile_start(eprof) ->
    eprof:start(),
    profiling = eprof:start_profiling([whereis(wings), self(), whereis(wings_image)]),
    ok.

profile_stop(fprof) ->
    fprof:trace(stop),
    spawn_link(fun() ->
                       File = "fprof.analysis",
                       fprof:profile(),
                       fprof:analyse([{dest, File}, {cols, 120}]),
                       io:format("Analysis in: ~p~n", [filename:absname(File)]),
                       eprof:stop()
               end),
    ok;
profile_stop(eprof) ->
    eprof:stop_profiling(),
    spawn_link(fun() ->
                       File = "eprof.analysis",
                       eprof:log(File),
                       eprof:analyze(),
                       io:format("Analysis in: ~p~n", [filename:absname(File)])
               end).

limit(Val, {'-infinity',infinity}) -> Val;
limit(Val, {Min,infinity}) when Val < Min -> Min;
limit(Val, {'-infinity',Max}) when Val > Max -> Max;
limit(Val, {Min,Max}) when Min < Max, Val < Min -> Min;
limit(Val, {Min,Max}) when Min < Max, Val > Max -> Max;
limit(Val, {Min,Max}) when Min < Max -> Val.

%%
%% Check if name match with the mask.
%%

is_name_masked(Name,Mask) ->
    Mask0=string:to_upper(Mask),
    Name0=string:to_upper(Name),
    is_name_masked_0(Name0,Mask0,get_mask_pos(Mask0)).

is_name_masked_0(_Name,"*",_MaskPos) -> true;
is_name_masked_0([],_Mask,_MaskPos) -> false;
is_name_masked_0(_Name,[],_MaskPos) -> false;
is_name_masked_0(Name,Name,[]) -> true;
is_name_masked_0(_Name,_Mask,[]) -> false;
is_name_masked_0(Name,Mask,[H|[]]=_MaskPos) ->  % *text with only one wildcard
    Mlen=string:len(Mask),
    case H of
    1 ->     % *text value
        Mask0=string:sub_string(Mask,H+1),
        MPos=string:rstr(Name,Mask0),
        (string:len(Name)-MPos+1)=:=(Mlen-1);
    Mlen ->  % text value*
        Mask0=string:sub_string(Mask,1,Mlen-1),
        string:str(Name,Mask0)=:=1;
    _ ->     % text *value
        Mask0=string:sub_string(Mask,1,H-1),
        Mask1=string:sub_string(Mask,H+1),
        MPos=string:rstr(Name,Mask1),
        Mlen0=string:len(Mask1),
        (string:str(Name,Mask0)=:=1) and ((string:len(Name)-MPos)=:=(Mlen0-1))
    end;
is_name_masked_0(Name,Mask,[H|[H0|_T0]=_T]=_MaskPos) ->  % *text with more than one wildcard
    case H of
    1 ->     % *text value
        Mask0=string:sub_string(Mask,H+1,H0-1),
        Mask1=string:sub_string(Mask,H0),
        case string:str(Name,Mask0) of
        0 -> false;
        NPos ->
            Name0=string:sub_string(Name,NPos+H0-H-1),
            is_name_masked_0(Name0,Mask1,get_mask_pos(Mask1))
        end;
    _ ->     % te*xt value*
        Mask0=string:sub_string(Mask,1,H-1),
        Mask1=string:sub_string(Mask,H),
        case string:str(Name,Mask0) of
        1 ->
            Name0=string:sub_string(Name,H),
            is_name_masked_0(Name0,Mask1,get_mask_pos(Mask1));
        _ -> false
        end
    end.

%%%
%%% Local functions.
%%%

format_1("~s"++F, [S0|Args], Acc) ->
    S = if
	    is_atom(S0) -> atom_to_list(S0);
	    is_list(S0) -> S0;
	    true -> 
		io:format("Bad string formatter for ~p in ~p~n", 
			  [S0, lists:flatten(reverse(Acc) ++ F)]),
		error(badarg)
	end,
    format_1(F, Args, [S|Acc]);
format_1("~p"++F, [S0|Args], Acc) ->
    S = format_p(S0),
    format_1(F, Args, [S|Acc]);
format_1("~~"++F, Args, Acc) ->
    format_1(F, Args, [$~|Acc]);
format_1("~ts"++F, Args, Acc) ->
    format_1("~s"++F, Args, Acc);
format_1([C|F], Args, Acc) when C =/= $~ ->
    format_1(F, Args, [C|Acc]);
format_1([], [], Acc) -> reverse(Acc).

format_p(Str) when is_list(Str)  ->
    [$",Str,$"];
format_p(Str) ->
    io_lib:format("~p", [Str]).

lib_dir(wings) ->
    case code:lib_dir(wings) of
	{error,bad_name} ->
	    ["wings.beam","ebin"|Rev] = lists:reverse(filename:split(code:which(wings))),
	    filename:join(lists:reverse(Rev));
	Dir -> 
	    Dir
    end;
lib_dir(Lib) ->
    code:lib_dir(Lib).

get_mask_pos([]) -> [];
get_mask_pos(Mask) ->
    get_mask_pos_1(Mask,0,[]).

get_mask_pos_1([],_,Acc) -> Acc;
get_mask_pos_1(Mask,Offset,Acc) ->
    case string:str(Mask,"*") of
    0 -> Acc;
    Pos ->
        Pos0=Pos+Offset,
        get_mask_pos_1(string:sub_string(Mask,Pos+1),Pos0,Acc++[Pos0])
    end.


% % % % % % % % % % % % % % % % % % %
%% Strings for Translating Hotkeys %%
% % % % % % (not elegant) % % % % % %

%%%% Selection Modes
translation_string(vertex) -> ?__(1,"Vertex");
translation_string(edge)   -> ?__(2,"Edge");
translation_string(face)   -> ?__(3,"Face");
translation_string(body)   -> ?__(4,"Body");
translation_string(light)  -> ?__(5,"Light");

%%%% Menu Headers
translation_string(file)   -> ?__(6,"File");
translation_string(edit)   -> ?__(7,"Edit");
translation_string(view)   -> ?__(8,"View");
translation_string(select) -> ?__(9,"Select");
translation_string(tools)  -> ?__(10,"Tools");
translation_string(window) -> ?__(11,"Window");
translation_string(help)   -> ?__(12,"Help");

%%%% File Menu
translation_string(new)             -> ?__(13,"New");
translation_string(open)            -> ?__(14,"Open");
translation_string(merge)           -> ?__(15,"Merge");
translation_string(save)            -> ?__(16,"Save");
translation_string(save_as)         -> ?__(17,"Save As");
translation_string(save_selected)   -> ?__(18,"Save Selected");
translation_string(save_incr)       -> ?__(19,"Save Incrementally");
translation_string(revert)          -> ?__(20,"Revert");
translation_string(import)          -> ?__(21,"Import");
translation_string(export)          -> ?__(22,"Export");
translation_string(export_selected) -> ?__(23,"Export Selected");
translation_string(import_image)    -> ?__(24,"Import Image");
translation_string(render)          -> ?__(25,"Render");
translation_string(install_plugin)  -> ?__(26,"Install Plugin");
translation_string(quit)            -> ?__(27,"Exit");

%%%% Import Export Formats
%% these probably don't need translating

%%%% Edit Menu
translation_string(undo_toggle)     -> ?__(28,"Undo/Redo");
translation_string(undo)            -> ?__(29,"Undo");
translation_string(redo)            -> ?__(30,"Redo");
translation_string(repeat)          -> ?__(31,"Repeat");
translation_string(repeat_args)     -> ?__(32,"Repeat Args");
translation_string(repeat_drag)     -> ?__(33,"Repeat Drag");
translation_string(purge_undo)      -> ?__(34,"Purge Undo");
translation_string(preferences)     -> ?__(35,"Preferences");
translation_string(prefs)           -> ?__(36,"Prefs");
translation_string(plugin_manager)  -> ?__(37,"Plugin Manager");

%%%% View Menu
translation_string(show_groundplane)    -> ?__(38,"Show Ground Plane");
translation_string(show_axes)           -> ?__(39,"Show Axes");
translation_string(show_info_text)      -> ?__(40,"Show Info Text");
translation_string(workmode)            -> ?__(41,"Workmode");
translation_string(wireframe)           -> ?__(42,"Wireframe");
translation_string(shade)               -> ?__(43,"Shade");
translation_string(toggle_wirefrfame)   -> ?__(44,"Toggle Wireframe");
translation_string(show_edges)          -> ?__(45,"Show Edges");
translation_string(show_wire_backfaces) -> ?__(46,"Show Wireframe Backfaces");
translation_string(smooth_proxy)        -> ?__(47,"Toggle Smooth Proxy");
translation_string(quick_preview)       -> ?__(48,"Quick Smoothed Preview");
translation_string(show_bb)             -> ?__(49,"Show Bounding Box");
translation_string(clip_plane)          -> ?__(50,"Clipping Plane");
translation_string(show_normals)        -> ?__(51,"Show Normals");
translation_string(show_colors)         -> ?__(52,"Show Colors");
translation_string(show_materials)      -> ?__(53,"Show Materials");
translation_string(show_textures)       -> ?__(54,"Show Textures");
translation_string(scene_lights)        -> ?__(55,"Scene Lights");
translation_string(toggle_lights)       -> ?__(56,"Toggle Lights");
translation_string(orthogonal_view)     -> ?__(57,"Orthographic View");
translation_string(reset)               -> ?__(58,"Reset View");
translation_string(aim)                 -> ?__(59,"Aim");
translation_string(highlight_aim)       -> ?__(60,"Highlight Aim");
translation_string(frame)               -> ?__(61,"Frame");
translation_string(frame_mode)          -> ?__(62,"Frame Mode");
translation_string(align_to_selection)  -> ?__(63,"Align to Selection");
translation_string(next)                -> ?__(64,"Next");
translation_string(pref)                -> ?__(65,"Prev");
translation_string(current)             -> ?__(66,"Current");
translation_string(jump)                -> ?__(67,"Jump");
translation_string(along)               -> ?__(68,"Along");
translation_string(rename)              -> ?__(69,"Rename");
translation_string(delete_all)          -> ?__(70,"Delete All");
translation_string(camera_settings)     -> ?__(71,"Camera Settings");
translation_string(auto_rotate)         -> ?__(72,"Auto Rotate");

%%%% Select Menu
translation_string(deselect)            -> ?__(73,"Deselect");
translation_string(more)                -> ?__(74,"More");
translation_string(less)                -> ?__(75,"Less");
translation_string(similar)             -> ?__(76,"Similar");
translation_string(edge_loop)           -> ?__(77,"Edge Loop");
translation_string(edge_loop_to_region) -> ?__(78,"Edge Loop to Region");
translation_string(edge_ring)           -> ?__(79,"Edge Ring");
translation_string(prev_edge_loop)      -> ?__(80,"Previous Edge Loop");
translation_string(next_edge_loop)      -> ?__(81,"Next Edge Loop");
translation_string(edge_link_incr)      -> ?__(82,"Edge Link Increase");
translation_string(edge_link_decr)      -> ?__(83,"Edge Link Decrease");
translation_string(edge_ring_incr)      -> ?__(84,"Edge Ring Increase");
translation_string(edge_ring_decr)      -> ?__(85,"Edge Ring Decrease");
translation_string(adjacent)            -> ?__(86,"Adjacent");
translation_string(inverse)             -> ?__(87,"Inverse");
translation_string(hide_selected)       -> ?__(88,"Hide Selected");
translation_string(hide_unselected)     -> ?__(89,"Hide Unselected");
translation_string(lock_unselected)     -> ?__(90,"Lock Unselected");
translation_string(show_all)            -> ?__(91,"Show All");
translation_string(store_selection)     -> ?__(92,"Store Selection");
translation_string(recall_selection)    -> ?__(93,"Recall Selection");
translation_string(new_group)           -> ?__(94,"New Group");
translation_string(oriented_faces)      -> ?__(95,"Similar Normals");
translation_string(similar_area)        -> ?__(96,"Similar Area");
translation_string(delete_group)        -> ?__(97,"Delete Group");
translation_string(add_to_group)        -> ?__(98,"Add to Group");
translation_string(subtract_from_group) -> ?__(99,"Subtract from Group");
translation_string(select_group)        -> ?__(100,"Select Group");
translation_string(union_group)         -> ?__(101,"Union Group");
translation_string(subtract_group)      -> ?__(102,"Subtract Group");
translation_string(intersect_group)     -> ?__(103,"Intersect Group");

%%%% Select By Submenu
translation_string(by)                      -> ?__(104,"By");
translation_string(hard_edges)              -> ?__(105,"Hard Edges");
translation_string(isolated_vertices)       -> ?__(106,"Isolated Vertices");
translation_string(nonplanar_faces)         -> ?__(107,"Non-Planar Faces");
translation_string(vertices_with)           -> ?__(108,"Vertices With");
translation_string(faces_with)              -> ?__(109,"Faces With");
translation_string(non_quad)                -> ?__(110,"Non Quadrangle");
translation_string(odd)                     -> ?__(111,"Odd");
translation_string(even)                    -> ?__(112,"Even");
translation_string(random)                  -> ?__(113,"Random");
translation_string(short_edges)             -> ?__(114,"Short Edges");
translation_string(sharp_edges)             -> ?__(115,"Sharp Edges");
translation_string(vertex_path)             -> ?__(116,"Vertex Path");
translation_string(fewest_edges_path)       -> ?__(117,"Fewest Edges Path");
translation_string(dijkstra_shortest_path)  -> ?__(118,"Shortest Path (Dijkstra)");
translation_string(astar_shortest_path)     -> ?__(119,"Shortest Path (A-Star)");
translation_string(material_edges)          -> ?__(120,"Material Edges");



%%%% Tools Menu
translation_string(align)            -> ?__(121,"Align");
translation_string(save_bb)          -> ?__(122,"Save Bounding Box");
translation_string(scale_to_bb)      -> ?__(123,"Scale to Bounding Box");
translation_string(scale_to_bb_prop) -> ?__(124,"Scale to Bounding Box Proportionally");
translation_string(move_to_bb)       -> ?__(125,"Move to Bounding Box");
translation_string(virtual_mirror)   -> ?__(126,"Virtual Mirror");
translation_string(create)           -> ?__(127,"Create");
translation_string(break)            -> ?__(128,"Break");
translation_string(freeze)           -> ?__(129,"Freeze");
translation_string(screenshot)       -> ?__(130,"Screenshot");
translation_string(area_volume_info) -> ?__(131,"Area Volume Info");
translation_string(scene_size_info)  -> ?__(132,"Memory Usage");
translation_string(put_on_ground)    -> ?__(133,"Put on Ground");
translation_string(unitize)          -> ?__(134,"Unitize");
translation_string(tweak)            -> ?__(135,"Tweak");
translation_string(snap_image_mode)  -> ?__(136,"Snap Image");

%%%% Window Menu
translation_string(outliner)         -> ?__(137,"Outliner");
translation_string(object)           -> ?__(138,"Geometry Graph");
translation_string(palette)          -> ?__(139,"Palette");
translation_string(console)          -> ?__(140,"Console");
translation_string(geom_viewer)      -> ?__(141,"New Geomerty Window");
translation_string(uv_editor_window) -> ?__(142,"UV Editor Window");

%%%% Help Menu
translation_string(getting_started)      -> ?__(143,"Getting Started");
translation_string(one_or_two)           -> ?__(144,"Mouse Buttons");
translation_string(international)        -> ?__(145,"International Keyboards");
translation_string(hotkeys)              -> ?__(146,"Defined Hotkeys");
translation_string(defining_hotkeys)     -> ?__(147,"Defining Hotkeys");
%%translation_string(advanced_menus)       -> ?__(148,"Advanced Menus");
translation_string(default_commands)     -> ?__(149,"Default Commands");
translation_string(performance_tips)     -> ?__(150,"Performance Tips");
translation_string(opengl_info)          -> ?__(151,"OpenGL Info");
translation_string(about)                -> ?__(152,"About");

%%%% Primitives Menu
translation_string(shapes)            -> ?__(153,"Shapes");
translation_string(tetrahedron)       -> ?__(154,"Tetrahedron");
translation_string(octahedron)        -> ?__(155,"Octahedron");
translation_string(octotoad)          -> ?__(156,"Octotoad");
translation_string(dodecahedron)      -> ?__(157,"Dodecahedron");
translation_string(icosahedron)       -> ?__(158,"Icosahedron");
translation_string(cube)              -> ?__(159,"Cube");
translation_string(cylinder)          -> ?__(160,"Cylinder");
translation_string(cone)              -> ?__(161,"Cone");
translation_string(sphere)            -> ?__(162,"Sphere");
translation_string(torus)             -> ?__(163,"Torus");
translation_string(grid)              -> ?__(164,"Grid");
translation_string(material)          -> ?__(165,"Material");
translation_string(image)             -> ?__(166,"Image");
translation_string(image_plane)       -> ?__(167,"Image Plane");
translation_string(text)              -> ?__(168,"Text");
%%%% More
translation_string(gear)              -> ?__(169,"Gear");
translation_string(tube)              -> ?__(170,"Tube");
translation_string(geodome)           -> ?__(171,"Geo Dome");
translation_string(knot)              -> ?__(172,"Torus Knot");
translation_string(ncube)             -> ?__(173,"N-Cube");
translation_string(ngon)              -> ?__(174,"N-Gon");
translation_string(regularplane)      -> ?__(175,"Regular Plane");
translation_string(lumpyplane)        -> ?__(176,"Lumpy Plane");
translation_string(wavyplane)         -> ?__(177,"Wavy Plane");
translation_string(sombreroplane)     -> ?__(178,"Sombrero Plane");
translation_string(spiral)            -> ?__(179,"Spiral");
translation_string(spring)            -> ?__(180,"Spring");
translation_string(uvtorus)           -> ?__(181,"UV Torus");
translation_string(lutorus)           -> ?__(182,"Lumpy Torus");
translation_string(sptorus)           -> ?__(183,"Spiral Torus");

%%%% Common Axis Descriptors
translation_string(normal)       -> ?__(184,"Normal");
translation_string(free)         -> ?__(185,"Free");
translation_string(center)       -> ?__(186,"Center");
translation_string(central_axis) -> ?__(187,"Central Axis");
translation_string(x)            -> ?__(188,"X");
translation_string(y)            -> ?__(189,"Y");
translation_string(z)            -> ?__(190,"Z");
translation_string(neg_x)        -> ?__(191,"-X");
translation_string(neg_y)        -> ?__(192,"-Y");
translation_string(neg_z)        -> ?__(193,"-Z");
translation_string(last_axis)    -> ?__(194,"Last Axis");
translation_string(default_axis) -> ?__(195,"Default Axis");
translation_string(true)         -> ?__(196,"True");
translation_string(false)        -> ?__(197,"False");
translation_string(all)          -> ?__(198,"All");
translation_string(pick_all)     -> ?__(199,"Pick All");
translation_string(radial_x)     -> ?__(200,"Radial X");
translation_string(radial_y)     -> ?__(201,"Radial Y");
translation_string(radial_z)     -> ?__(202,"Radial Z");
translation_string('ASK')        -> ?__(203,"ASK");
translation_string(lmb)          -> ?__(204,"LMB");
translation_string(mmb)          -> ?__(205,"MMB");
translation_string(rmb)          -> ?__(206,"RMB");
translation_string(relative)     -> ?__(207,"Relative");
translation_string(absolute)     -> ?__(208,"Absolute");

%%%% Common Commands
translation_string(move)           -> ?__(209,"Move");
translation_string(rotate)         -> ?__(210,"Rotate");
translation_string(scale)          -> ?__(211,"Scale");
translation_string(uniform)        -> ?__(212,"Uniform");
translation_string(extrude)        -> ?__(213,"Extrude");
translation_string(extract)        -> ?__(214,"Extract");
translation_string(dissolve)       -> ?__(215,"Dissolve");
translation_string(collapse)       -> ?__(216,"Collapse");
translation_string(delete)         -> ?__(217,"Delete");
translation_string(flatten)        -> ?__(218,"Flatten");
translation_string(smooth)         -> ?__(219,"Smooth");
translation_string(tighten)        -> ?__(220,"Tighten");
translation_string(bevel)          -> ?__(221,"Bevel");
translation_string(weld)           -> ?__(222,"Weld");
translation_string(vertex_color)   -> ?__(223,"Vertex Color");
translation_string(move_planar)    -> ?__(290,"Move Planar");

%%%% Absolute Commands
translation_string(snap)           -> ?__(224,"Snap");
translation_string(nsnap)          -> ?__(225,"Snap|Numeric Entry");
translation_string(ctscale)        -> ?__(226,"Scale to Target Size");
translation_string(cscale)         -> ?__(227,"Scale|Center");

%%%% Intersect
translation_string(intersect)      -> ?__(228,"Intersect");
translation_string(stay_on_line)   -> ?__(229,"Stay on Line");
translation_string(stay_on_plane)  -> ?__(230,"Stay on Plane");
translation_string(arc_intersect)  -> ?__(289,"Rotate to Target");

%%%% Vertex Menu
translation_string(connecting_edges) -> ?__(231,"Connecting Edges");
translation_string(bend)             -> ?__(232,"Bend");
translation_string(shift)            -> ?__(233,"Shift");

%%%% Vertex|Deform
translation_string(deform)          -> ?__(234,"Deform");
translation_string(crumple)         -> ?__(235,"Crumple");
translation_string(taper)           -> ?__(236,"Taper");
translation_string(twist)           -> ?__(237,"Twist");
translation_string(torque)          -> ?__(238,"Torque");
translation_string(inflate)         -> ?__(239,"Inflate");
translation_string(cylindrilize)    -> ?__(240,"Inflate Cylindrically");
translation_string(shear)           -> ?__(241,"Shear");

%%%% Edge Menu
translation_string(slide)              -> ?__(242,"Slide");
translation_string(clean_dissolve)     -> ?__(243,"Clean Dissolve");
translation_string(cut)                -> ?__(244,"Cut");
translation_string(connect)            -> ?__(245,"Connect");
translation_string(hardness)           -> ?__(246,"Hardness");
translation_string(soft)               -> ?__(247,"Soft");
translation_string(hard)               -> ?__(248,"Hard");
translation_string(circularise)        -> ?__(249,"Circularise");
translation_string(circularise_center) -> ?__(250,"Circularise|Center");
translation_string(loop_cut)           -> ?__(251,"Loop Cut");
translation_string(turn_ccw)           -> ?__(252,"Turn CCW");
translation_string(turn_cw)            -> ?__(253,"Turn CW");
translation_string(turn_optimized)     -> ?__(254,"Turn Optimized");

%%%% Face Menu
translation_string(bridge)             -> ?__(255,"Bridge");
translation_string(bump)               -> ?__(256,"Bump");
translation_string(extrude_region)     -> ?__(257,"Extrude Region");
translation_string(extract_region)     -> ?__(258,"Extract Region");
translation_string(sweep_extrude)      -> ?__(259,"Sweep Extrude");
translation_string(sweep_region)       -> ?__(260,"Sweep Region");
translation_string(sweep_extract)      -> ?__(261,"Sweep Extract");
translation_string(inset)              -> ?__(262,"Inset");
translation_string(intrude)            -> ?__(263,"Intrude");
translation_string(lift)               -> ?__(264,"Lift");
translation_string(put_on)             -> ?__(265,"Put On");
translation_string(clone_on)           -> ?__(266,"Clone On");
translation_string(mirror)             -> ?__(267,"Mirror");
translation_string(mirror_separate)    -> ?__(268,"Mirror Separate");
translation_string(tesselate)          -> ?__(269,"Tesselate");
translation_string(trianglulate)       -> ?__(270,"Triangulate");
translation_string(quadrangulate)      -> ?__(271,"Quadrangulate");
translation_string(untriangulate)      -> ?__(272,"Untriangulate");
translation_string(hide)               -> ?__(273,"Hide");
translation_string(wpc_autouv)         -> ?__(274,"AutoUV");
translation_string(segment)            -> ?__(275,"Segment");
translation_string(segment_old)        -> ?__(276,"Segment Old");
translation_string(force_seg)          -> ?__(277,"Force Segment");

%%%% Object Menu
translation_string(flip)                 -> ?__(278,"Flip");
translation_string(invert)               -> ?__(279,"Invert");
translation_string(doo_sabin)            -> ?__(280,"Doo Sabin");
translation_string(combine)              -> ?__(281,"Combine");
translation_string(separate)             -> ?__(282,"Separate");
translation_string(cleanup)              -> ?__(283,"Cleanup");
translation_string(auto_smooth)          -> ?__(284,"Auto Smooth");
translation_string(duplicate)            -> ?__(285,"Duplicate");
translation_string(vertex_color_mode)    -> ?__(286,"Vertex Color Mode");
translation_string(to_arealight)         -> ?__(287,"To Area Light");
translation_string(materials_to_colors)  -> ?__(288,"Materials to Colors");

%%%%
%%%%   Translation strings used so far 1 - 290
%%%%

%%%% Others as yet to be added are proccessed here
translation_string(Atom) when is_atom(Atom) ->
    wings_util:cap(atom_to_list(Atom)).
