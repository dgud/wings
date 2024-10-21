%%
%%  wpc_autouv.erl --
%%
%%     A semi-simple semi-automatic UV-mapping semi-plugin.
%%
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2,redraw/1]).
-export([handle_event/2,bg_image/0]). %% Debug
-import(lists, [sort/1,keysort/2,map/2,foldl/3,reverse/1,keysearch/3]).

%% Exports to auv_seg_ui.
-export([init_show_maps/4]).
%% Exports to auv_texture.
-export([material_faces/1,get_textureset_info/1,remap_uv_tile/1]).


init() -> true.

menu({body}, Menu) ->
    case wpc_snap_win:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,auv_menu()]
    end;
menu({face}, Menu) ->
    case wpc_snap_win:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,auv_menu()]
    end;
menu({window}, Menu) ->
    Menu ++ [separator,
	     {?__(1,"UV Editor Window"),uv_editor_window,
	      ?__(2,"Open a UV Editor window for each selected object")}];
menu(_Dbg, Menu) ->
    Menu.

auv_menu() ->
    {?__(1,"UV Mapping"), {?MODULE, fun auv_menu/2}}.
auv_menu(help,_) ->
    {?__(2,"Generate UV mapping or texture"),
     ?__(25,"Re-segment object(s)"),
     ?__(3,"Force to segmenting mode (delete old segments)")};
auv_menu(1,_What) -> {?MODULE, segment};
auv_menu(2,_) -> {?MODULE, segment_old};
auv_menu(3,_) -> {?MODULE, force_seg}.

auv_show_menu(label) ->
    ?__(1,"Show/Hide Background Image");
auv_show_menu(help) ->
    ?__(2,"Toggle display of the background texture image");
auv_show_menu(Action) ->
    Cmd = {show,toggle_background},
    case Action of
        true ->
            Label = auv_show_menu(label),
            Help = auv_show_menu(help),
            wings_menu:update_menu(view, Cmd, {append, 0, Label},Help);
        false ->
            wings_menu:update_menu(view, Cmd, delete)
    end.

auv_show_tile_menu(label) ->
    ?__(1,"Tiled texture");
auv_show_tile_menu(help) ->
    ?__(2,"Toggle the show mode for the background texture image");
auv_show_tile_menu(Action) ->
    Cmd = {show,toggle_tiled_texture},
    case Action of
        true ->
            Label = auv_show_tile_menu(label),
            Help = auv_show_tile_menu(help),
            wings_menu:update_menu(view, Cmd, {append, 0, Label},Help);
        false ->
            wings_menu:update_menu(view, Cmd, delete)
    end.

auv_texture_set_menu(label) ->
    [?__(1,"Texture Set Mode"),
     ?__(3,"Show/Hide Tile ID")];
auv_texture_set_menu(help) ->
    [?__(2,"Toggle the editor mode for multiple texture set"),
     ?__(4,"Toggle display of the tile identification")];
auv_texture_set_menu(cmd) ->
    [toggle_texture_set_mode,
     toggle_texture_set_id];
auv_texture_set_menu(Action) ->
    [Cmd0,Cmd1] = auv_texture_set_menu(cmd),
    case Action of
        true ->
            [Label0,Label1] = auv_texture_set_menu(label),
            [Help0, Help1] = auv_texture_set_menu(help),
            wings_menu:update_menu(view, {show,Cmd1}, {append, 0, Label1},Help1),
            wings_menu:update_menu(view, {show,Cmd0}, {append, 0, Label0},Help0);
        false ->
            wings_menu:update_menu(view, {show,Cmd0}, delete),
            wings_menu:update_menu(view, {show,Cmd1}, delete)
    end.

auv_export_menu(label) ->
    ?__(1,"Export UV...");
auv_export_menu(help) ->
    ?__(2,"Exports the UV as cartoon edges (.eps, .svg)").

auv_txset_naming_menu() ->
    [{?__(1,"Default"), {txset_naming,uv_default}, "Wings3D default auv - name_00_auv"},
     {?__(2,"UV Tile Base-0"), {txset_naming,uv_base0}, "Zbrush standard - name_u0_v0"},
     {?__(3,"UV Tile Base-1"), {txset_naming,uv_base1}, "Mudbox standard - name_u1_v1"},
     {?__(4,"UDIM"), {txset_naming,uv_udim}, "Mari standard - name_1001"}].

command({body,{?MODULE, Op}} , St) ->
    start_uvmap(Op, St);
command({face,{?MODULE, Op}} , St) ->
    start_uvmap(Op, St);
command({?MODULE, Op}, St) ->
    start_uvmap(Op, St);
command({window,uv_editor_window}, St) ->
    start_uvmap(edit, St);
command(_Cmd, _) -> 
    next.

start_uvmap(edit, #st{sel=[]}) -> wings_u:error_msg(?__(1,"Nothing selected"));
start_uvmap(Action, #st{sel=Sel}=St) ->
    start_uvmap_1(Sel, Action, St).

start_uvmap_1([{Id,_}|T], Action, St) ->
    EditWin   = {autouv,Id},
    EditExists = wings_wm:is_window(EditWin),
    SegWin    = {autouv,{segment,Id}},
    SegExists = wings_wm:is_window(SegWin),
    case segment_or_edit(Action,Id,St) of
	{edit,Fs} when EditExists ->
	    wings_wm:send(EditWin, {add_faces,Fs,St}),
	    wings_wm:raise(EditWin);
	{seg_ui,Fs,_} when SegExists ->
	    wings_wm:send(SegWin, {add_faces,Fs,St}),
	    wings_wm:raise(SegWin);
	Op when element(1,Op) == edit ->	    
	    create_window(Op, EditWin, Id, St);
	Op ->	    
	    create_window(Op, SegWin, Id, St)
    end,
    start_uvmap_1(T, Action, St);
start_uvmap_1([], _, _) -> keep.

segment_or_edit(edit, _Id, _St) -> {edit,object};
segment_or_edit(segment,Id,#st{selmode=face,sel=Sel,shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    UVFs = gb_sets:from_ordset(wings_we:uv_mapped_faces(We)),
    {value,{_,Fs}} = lists:keysearch(Id, 1, Sel),
    case gb_sets:is_subset(Fs,UVFs) of
	false -> {seg_ui,Fs,delete_old};
	true ->  {edit, Fs}
    end;
segment_or_edit(segment,Id,#st{shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    case wings_we:uv_mapped_faces(We) of    
	[] -> {seg_ui,object,delete_old};
	_ ->  {edit,object}
    end;
segment_or_edit(force_seg,Id,#st{selmode=face,sel=Sel}) -> 
    {value,{_,Fs}} = lists:keysearch(Id, 1, Sel),
    {seg_ui,Fs,delete_old};
segment_or_edit(force_seg,_Id,_) ->
    {seg_ui,object,delete_old};
segment_or_edit(segment_old,Id,#st{selmode=face,sel=Sel}) -> 
    {value,{_,Fs}} = lists:keysearch(Id, 1, Sel),
    {seg_ui,Fs,keep_old};
segment_or_edit(segment_old,_Id,_) ->
    {seg_ui,object,keep_old}.
	    
create_window(Action, Name, Id, #st{shapes=Shs}=St) ->
    #we{name=ObjName} = We = gb_trees:get(Id, Shs),
    Op = {replace,fun(Ev) -> auv_event(Ev, St) end},
    Segment = if element(1,Action) == edit -> ""; true -> ?__(1,"Segmenting") end,
    Title = "AutoUV "++ Segment ++": " ++ ObjName,

    {Pos,Size} = init_drawarea(),
    {Frame,Ps} = wings_frame:make_win(Title, [{size, Size}, {pos, Pos}]),
    Canvas = wings_gl:window(Frame, ?GET(gl_context), true, true),
    Props = [{display_data,Name}|wings_view:initial_properties()++Ps],
    wings_wm:toplevel(Name, Canvas, Props, Op),
    wings_wm:send(Name, {init,{Action,We}}),
    Frame.

auv_event({init,Op}, St) ->
    wings:init_opengl(St),
    case Op of
	{{edit,What},We} -> start_edit(What, We, St);
	{{seg_ui,_,Oper},We} ->  auv_seg_ui:start(Oper, We, We, St)
    end;
auv_event(redraw, _) ->
    wings_wm:clear_background(),
    keep;
auv_event({crash,Crash}, _) ->
    wings_u:win_crash(Crash),
    delete;
auv_event(_Ev, _) -> keep.

%%%
%%% Start the UV editor.
%%%

start_edit(Mode, We, St) ->
    MatNames0 = wings_facemat:all(We),
    MatNames1 = sofs:from_external(MatNames0, [{face,material}]),
    MatNames2 = sofs:converse(MatNames1),
    MatNames3 = sofs:relation_to_family(MatNames2),
    MatNames4 = sofs:to_external(MatNames3),
    MatNames = [Mat || {Name,_}=Mat <- MatNames4, get_texture(Name, St) /= false],
    case MatNames of
	[{MatName,_}|_] ->
	    do_edit(MatName, Mode, We, St);
	_ ->
	    do_edit(none, Mode, We, St)
    end.

do_edit(MatName0, Mode, We0, #st{mat=Materials,shapes=Shs0}=GeomSt0) ->
    We =
        case get_textureset_info(We0) of
            {?MULTIPLE,[_,TxSet0]=TxSetInfo0} ->  %% object has multiple texture set enabled
                [_,[{_,#{mat:=MatName}}|_]=TxSet] = TxSetInfo = get_texture_set(TxSetInfo0,We0,Materials),
                %% We remove the textureset data if a different material was
                %% eventually assigned to the model.
                if (length(TxSet) =/= length(TxSet0)) ->
                    update_textureset_system(We0, ?SINGLE, []);
                true ->
                    ?SET({?MODULE,tiled_texture},false),  %% disable tiled texture
                    %% updating the texture set info to the #we{}
                    update_textureset_system(We0, ?MULTIPLE, TxSetInfo)
                end;
            _X ->
                MatName = MatName0,
                update_textureset_system(We0, ?SINGLE, [])
        end,
    Shs = gb_trees:update(We#we.id, We, Shs0),
    GeomSt = GeomSt0#st{shapes=Shs},

    AuvSt = create_uv_state(gb_trees:empty(), MatName, Mode, We, GeomSt),
    camera_reset(),
    new_geom_state(GeomSt, AuvSt).

init_show_maps(Charts0, Fs, #we{name=WeName,id=Id}, GeomSt0) ->
    Charts1 = auv_placement:place_areas(Charts0),
    Charts  = gb_trees:from_orddict(keysort(1, Charts1)),
    MatName0 = list_to_atom(WeName++"_auv"),
    {GeomSt1,MatName} = 
	case gb_trees:is_defined(MatName0, GeomSt0#st.mat) of
	    true -> 
		{GeomSt0,MatName0};
	    false ->
		Tx = bg_img_id(),
		add_material(Tx, WeName, GeomSt0)
	end,
    GeomSt = insert_initial_uvcoords(Charts, Id, MatName, GeomSt1),
    EditWin = {autouv,Id},
    case wings_wm:is_window(EditWin) of
	true ->
	    wings_wm:send(EditWin, {add_faces,Fs,GeomSt}),
	    wings_wm:send(geom, {new_state,GeomSt});
	false ->
	    %% we are going to ensure to open the AutoUV window in the same
	    %% display as the segment window was, since it can be in another
	    %% than the one in which the main window is.
	    SegWin = wings_wm:this_win(),
	    {X0,Y0} = wxWindow:getPosition(SegWin),
	    Pos = wxWindow:clientToScreen(SegWin,X0,Y0),
	    Win = create_window({edit,Fs}, EditWin, Id, GeomSt),
	    wxWindow:move(Win,Pos),
	    wings_wm:send(geom, {new_state,GeomSt})
    end,
    GeomSt.

material_faces(#we{mat=[MatFace|_]=FaceMats0}=We) when is_tuple(MatFace) ->
    UVF = gb_sets:from_list(wings_we:uv_mapped_faces(We)),
    FaceMats =
        lists:foldr(fun({Face,Mat}, Acc) ->
                case gb_trees:is_defined(Mat,Acc) of
                    true ->
                        FaceList = gb_trees:get(Mat,Acc),
                        gb_trees:enter(Mat,gb_sets:add(Face,FaceList),Acc);
                    false ->
                        case gb_sets:is_member(Face,UVF) of
                            true ->
                                gb_trees:enter(Mat,gb_sets:add(Face,gb_sets:empty()),Acc);
                            false ->
                                Acc
                        end
                end
            end, gb_trees:empty(), FaceMats0),
    gb_trees:to_list(FaceMats);
material_faces(#we{mat=FaceMat0}) ->
    FaceMat0.

txset_suffix(TxSetNaming, {U,V}) ->
    Suffix =
        case TxSetNaming of
            uv_base0 ->  %% 0-based (Zbrush)
                io_lib:format("u~w_v~w", [U,V]);
            uv_base1 ->  %% 1-based (Mudbox)
                io_lib:format("u~w_v~w", [U+1,V+1]);
            uv_udim ->  %% UDIM (Mari)
                io_lib:format("~w", [(1000+U+1+V*10)]);
            uv_default ->  %% default (Wings3D)
                io_lib:format("~w~w_auv", [U,V])
        end,
    lists:flatten(Suffix).

build_txset_name(TxSetNaming, Name0, Tile) ->
    Name0 ++ "_" ++ txset_suffix(TxSetNaming, Tile).

build_texture_set(TxSetNaming,Charts0, #we{name=Name}=We1, GeomSt0) ->
    {TxSet,Charts,We,St} =
        lists:foldl(fun(#we{id=Id,vp=Vs0,fs=Fs}=Chart0, {TSetAcc0,ChartsAcc0,WeAcc0,StAcc0}=Acc)->
                case array:sparse_to_list(Vs0) of
                    [] -> Acc;
                    Vs ->
                        %% picking faces and uv ids
                        {U0,V0,_} = e3d_vec:average(Vs),
                        Key = {trunc(U0),trunc(V0)},
                        %% creating the new material for each uv id
                        NameUV = build_txset_name(TxSetNaming,Name,Key),
                        {#st{mat=Matb}=StAcc,MatName} = add_material({txset, bg_img_tile_id(Key)}, NameUV, StAcc0),
                        TxId = get_texture_img(MatName, Matb),

                        %% assigning the material to the object and chart
                        CFaces = gb_trees:to_list(Fs),  %% faces mapped in Charts (UV)
                        Faces = [F || {F,_} <- CFaces, F >= 0],  %% faces remapped to Shape (Geom)
                        Chart = wings_facemat:assign(MatName, Faces, Chart0),
                        WeAcc = wings_facemat:assign(MatName, Faces, WeAcc0),

                        ChartsAcc = gb_trees:update(Id,Chart,ChartsAcc0),
                        TSetAcc = gb_trees:enter(Key,#{mat=>MatName,bg_img=>TxId},TSetAcc0),
                        {TSetAcc,ChartsAcc,WeAcc,StAcc}
                end
            end, {gb_trees:empty(),Charts0,We1,GeomSt0}, gb_trees:values(Charts0)),
    {[TxSetNaming,gb_trees:to_list(TxSet)],Charts,We,St}.

get_texture_set(OldTxSet, We, Materials) ->
    MatFaces = material_faces(We),
    get_texture_set(OldTxSet, MatFaces, We, Materials).

get_texture_set([TxSetNaming, _OldTxSet], [MatInfo|_]=MatNames, We, Materials) when is_tuple(MatInfo) ->
    TxSet =
        lists:foldr(fun({MatName, Fs}, Acc) ->
                            VsPos =
                                gb_sets:fold(fun(Face, Acc0) when Face < 0 ->
                                                     Acc0;
                                                (Face, Acc0) ->
                                                     UVPos = wings_va:face_attr(uv, Face, We),
                                                     [{U,V,0.0} || {U,V} <- UVPos]++Acc0
                                             end, [], Fs),
                            {U,V,_} = e3d_vec:average(VsPos),
                            TxId = get_texture_img(MatName, Materials),
                            gb_trees:enter({trunc(U),trunc(V)},#{mat=>MatName,bg_img=>TxId}, Acc)
                    end, gb_trees:empty(), MatNames),

    [TxSetNaming,gb_trees:to_list(TxSet)];
get_texture_set([TxSetNaming, _], MatName, #we{}, Materials) ->
    TxId = get_texture_img(MatName, Materials),
    TxSet = gb_trees:enter({0,0},#{mat=>MatName,bg_img=>TxId}, gb_trees:empty()),
    [TxSetNaming,gb_trees:to_list(TxSet)].


get_texture_img(MatName, Materials) ->
    case get_texture(MatName,Materials) of
        false -> bg_img_id();
        ImId -> ImId
    end.

update_textureset_system(#we{pst=Pst0}=We, ?SINGLE, _) ->
    wings_wm:set_prop(wings_wm:this(), texture_set_mode, false),
    Pst = gb_trees:delete_any(?TEXTURESET, Pst0),
    We#we{pst=Pst};
update_textureset_system(#we{pst=Pst0}=We, Type, TxInfo) ->
    wings_wm:set_prop(wings_wm:this(), texture_set_mode, true),
    Pst = gb_trees:enter(?TEXTURESET, {Type,TxInfo}, Pst0),
    We#we{pst=Pst}.

get_textureset_info(#we{pst=Pst}) ->
    case gb_trees:lookup(?TEXTURESET, Pst) of
        none -> none;
        {value,Value} -> Value
    end.

create_uv_state(Charts, MatName, Fs, We, #st{shapes=Shs0}=GeomSt) ->
    wings:mode_restriction([vertex,edge,face,body]),
    wings_wm:current_state(#st{selmode=body,sel=[]}),

    Shs = gb_trees:update(We#we.id, We#we{fs=undefined,es=array:new()}, Shs0),
    FakeGeomSt = GeomSt#st{sel=[],shapes=Shs},

    Image = case get_texture(MatName,GeomSt) of
                false -> bg_img_id();
                ImId -> ImId
            end,
    Uvs = #uvstate{st=wpa:sel_set(face, [], FakeGeomSt),
		   id      = We#we.id,
		   mode    = Fs,
		   bg_img  = Image,
		   tile    = {0,0},
		   matname = MatName},
    St = FakeGeomSt#st{selmode=body,sel=[],shapes=Charts,bb=Uvs,
		       repeatable=ignore,ask_args=none,drag_args=none},
    Win = wings_wm:this(),

    View = #view{origin={0.0,0.0,0.0},
		 distance=0.65,
		 azimuth=0.0,
		 elevation=0.0,
		 pan_x=-0.5,
		 pan_y=-0.5,
		 fov=90.0,
		 hither=0.0001,
		 yon=50.0},
    wings_view:set_current(View),
    wings_wm:set_prop(Win, drag_filter, fun drag_filter/1),
    wings_wm:set_prop(show_wire_backfaces, true),
    wings_wm:set_prop(show_info_text, false), %% Users want this
    wings_wm:set_prop(orthogonal_view, true),
    wings_wm:set_prop(show_axes, false),
    wings_wm:set_prop(show_groundplane, false),
    wings_wm:set_prop(wireframed_objects,
		      gb_sets:from_list(gb_trees:keys(Charts))),
    wings_wm:set_prop(allow_rotation, false),
    wings_wm:set_prop(select_backface, true),

    wings_wm:later(got_focus),

    case ?GET({?MODULE,show_background}) of
	undefined ->
        ?SET({?MODULE,show_background}, true),
        ?SET({?MODULE,tiled_texture}, false),
        ?SET({?MODULE,show_texture_set_id}, true);
	_ -> ignore
    end,
    wings:register_postdraw_hook(Win, ?MODULE,
				 fun draw_background/1),
    St.

insert_initial_uvcoords(Charts, Id, MatName, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We1 = update_uvs(gb_trees:values(Charts), We0),
    We2 = preserve_old_materials(We1, St),
    %% ensuring the object mapping is not enabled to texture set mode
    We3 = update_textureset_system(We2, ?SINGLE, []),
    We = insert_material(Charts, MatName, We3),
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.

update_selected_uvcoords(#st{bb=Uvs}=St) ->
    Charts = wpa:sel_fold(fun(_, We, Acc) -> [We|Acc] end, [], St),
    #uvstate{st=#st{shapes=Shs0}=GeomSt0,id=Id} = Uvs,
    We0 = gb_trees:get(Id, Shs0),
    We = update_uvs(Charts, We0),    
    Shs = gb_trees:update(Id, We, Shs0),
    GeomSt = GeomSt0#st{shapes=Shs},
    wings_wm:send(geom, {new_state,GeomSt}),
    clear_temp_sel(St#st{bb=Uvs#uvstate{st=GeomSt}}).

%% update_uvs(Charts, We0) -> We
%%  Update the UV coordinates for the original model.
update_uvs([#we{vp=Vpos0,name=#ch{vmap=Vmap}}=ChartWe|Cs], We0) ->
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [],
	       wings_we:visible(ChartWe), ChartWe),
    VFace1 = sofs:relation(VFace0),
    VFace2 = sofs:relation_to_family(VFace1),
    VFace = sofs:to_external(VFace2),
    Vpos = array:sparse_to_orddict(Vpos0),
    We = update_uvs_1(Vpos, VFace, Vmap, We0),
    update_uvs(Cs, We);
update_uvs([], We) -> We.

update_uvs_1([{V0,{X,Y,_}}|Vs], [{V0,Fs}|VFs], Vmap, We0) ->
    UV = {X,Y},
    V = auv_segment:map_vertex(V0, Vmap),
    We = wings_va:set_vtx_face_uvs(V, Fs, UV, We0),
    update_uvs_1(Vs, VFs, Vmap, We);
update_uvs_1([{V0,none}|Vs], [{V0,Fs}|VFs], Vmap, We0) ->
    V = auv_segment:map_vertex(V0, Vmap),
    We = wings_va:set_vtx_face_uvs(V, Fs, none, We0),
    update_uvs_1(Vs, VFs, Vmap, We);
update_uvs_1([], [], _, We) -> We.

%% preserve_old_materials(We0) -> We
%%  If the object contains materials with colors and no
%%  vertex colors, convert the materials to vertex colors.
preserve_old_materials(We, St) ->
    case not wings_va:any_colors(We) andalso
	wings_facemat:any_interesting_materials(We) of
	true ->
	    wings_we:uv_to_color(We, St);
	false ->
	    We
    end.

insert_material(Cs, MatName, We) ->
    Faces = lists:append([wings_we:visible(W) || W <- gb_trees:values(Cs)]),
    wings_facemat:assign(MatName, Faces, We).


%%%%% Material handling

get_texture(MatName, #st{mat=Materials}) ->
    get_texture(MatName, Materials);
get_texture(MatName, Materials) ->
    case gb_trees:lookup(MatName, Materials) of
	none -> false;
	{value,Mat} ->
	    Maps = proplists:get_value(maps, Mat, []),
	    proplists:get_value(diffuse, Maps, false)
    end.

add_material({txset,Tx}, Name, St0) ->
    add_material_0(Tx, list_to_atom(Name), St0);
add_material(Tx, Name, St0) ->
    add_material_0(Tx, list_to_atom(Name++"_auv"), St0).

add_material_0(Tx, MatName0, #st{mat=Matb0}=St0) ->
    case gb_trees:lookup(MatName0, Matb0) of
        none ->
            Mat = {MatName0,[{opengl,[]},{maps,[{diffuse,Tx}]}]},
            case wings_material:add_materials([Mat], St0) of
                {St,[]} ->
                    {St,MatName0};
                {St,[{MatName0,MatName}]} ->
                    {St,MatName}
            end;
        _ ->
            {St0,MatName0}
    end.

update_texture(Im = #e3d_image{},MatName,St) ->
    catch wings_material:update_image(MatName, diffuse, Im, St),
    {St,MatName}.

bg_img_id() ->
    Is = wings_image:images(),
    case [ImId || {ImId,#e3d_image{name="auvBG"}} <- Is] of
	[ImId] -> ImId;
	_ -> wings_image:new("auvBG",bg_image())
    end.

bg_img_tile_id({U,V}) ->
    ImgName = lists:flatten(io_lib:format("~s_u~w_v~w", ["auvBG",U,V])),
    Is = wings_image:images(),
    case [ImId || {ImId,#e3d_image{name=Name}} <- Is, Name==ImgName] of
        [ImId] -> ImId;
        _ -> wings_image:new_temp(ImgName,bg_image())
    end.

%%%% Menus.

command_menu(body, X, Y) ->
    First = [{?__(37,"Stretch optimization"), stretch_opt,
              ?__(38,"Optimize the chart stretch")},
             separator],
    Unfold = case erlang:system_info(wordsize) of
                 4 ->
                     [{?__(39,"Unfold"), lsqcm, ?__(40,"Unfold the chart")}];
                 8 ->
                     [{?__(39,"Unfold"), lsqcm, ?__(40,"Unfold the chart")},
                      {?__(391,"Unfold (slow)"),slim,
                       ?__(392,"Unfold the chart, slow for charts with many faces but better")}]
             end,
    Rest = [{?__(41,"Project Normal"), project,
             ?__(42,"Project UVs from chart normal")},
            {?__(43,"Spherical"), sphere,
             ?__(44,"Spherical mapping")}
           ],
    Remap = First ++ Unfold ++ Rest,

    Menu = [{?__(2,"Move"), {move, move_directions(false)},
	     ?__(3,"Move selected charts")},
	    {?__(4,"Scale"), {scale, scale_directions(false) ++
                                  [separator] ++ stretch_directions() ++
                                  [separator,
                                   {?__(411,"Normalize Sizes"), normalize,
                                    ?__(412,"Normalize Chart Sizes so that each"
                                        "chart get it's corresponding 2d area")}]},
	     ?__(5,"Scale selected charts")},
	    {?__(6,"Rotate"), {rotate,rotate_free(false)},
	     {?__(7,"Rotate selected charts"),[],?__(59,"Pick rotation center")},[]},
	    separator,
	    {?__(8,"Move to"),
	     {move_to,
	      [{?__(9,"Center"), center, ?__(10,"Move to Center")},
	       {?__(11,"Center X"), center_x, ?__(12,"Move to horizontal center")},
	       {?__(13,"Center Y"), center_y, ?__(14,"Move to vertical center")},
	       {?__(15,"Bottom"), bottom, ?__(16,"Move to bottom border")},
	       {?__(17,"Top"), top, ?__(18,"Move to top border")},
	       {?__(19,"Left"), left, ?__(20,"Move to left border")},
	       {?__(21,"Right"), right, ?__(22,"Move to right border")}
	      ]}, ?__(23,"Move charts to position")},
        align_menu(),
	    {?__(24,"Flip"),{flip,
                             [{?__(25,"Horizontal"),horizontal,?__(26,"Flip selection horizontally")},
                              {?__(27,"Vertical"),vertical,?__(28,"Flip selection vertically")}]},
	     ?__(29,"Flip selected charts")},
	    separator,
	    {?__(30,"Tighten"),tighten,
	     ?__(31,"Move UV coordinates towards average midpoint")},
	    separator,
	    {?__(32,"Hide"),hide,?__(33,"Hide selected charts but keep UV-coordinates")},
	    {?__(34,"Delete"),delete,?__(35,"Remove UV-coordinates for the selected charts")},
	    separator,
	    {?__(36,"ReMap UV"), {remap, Remap},
	     ?__(45,"Calculate new UVs with chosen algorithm")}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, {auv,body}, Menu);
command_menu(face, X, Y) ->
    Scale = scale_directions(true),
    Move = move_directions(true),
    Menu = [{?__(47,"Move"),{move,Move},?__(48,"Move selected faces"),[magnet]},
	    {?__(49,"Scale"),{scale,Scale},?__(50,"Scale selected faces"), [magnet]},
	    {?__(51,"Rotate"),{rotate,rotate_free(true)},
	     {?__(52,"Rotate selected faces"),[],?__(59,"Pick rotation center")}, [magnet]},
	    separator,
	    {?__(521,"Project-Unfold"),	{remap, proj_lsqcm},
	     ?__(522,"Project selected faces from normal and unfold the rest of chart")}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, {auv,face}, Menu);
command_menu(edge, X, Y) ->
    Scale = scale_directions(true),
    Move = move_directions(true),
    Align = 	    
	   [{?__(53,"Free"),rotate_free(true),
	     {?__(54,"Rotate selection freely"),[],?__(59,"Pick rotation center")}, [magnet]},
	    {?__(55,"Chart to X"), align_x, ?__(56,"Rotate chart to align selected edge to X-axis")},
	    {?__(57,"Chart to Y"), align_y, ?__(58,"Rotate chart to align selected edge to Y-axis")}],
    Menu = [{?__(60,"Move"),{move,Move},?__(61,"Move selected edges"),[magnet]},
	    {?__(62,"Scale"),{scale,Scale},?__(63,"Scale selected edges"), [magnet]},
	    {?__(64,"Rotate"),{rotate,Align},?__(65,"Rotate commands")},
	    {?__(641,"Slide"),slide,?__(642,"Slide along neighbor edges")},
	    {?__(643,"Distribute"),
	     {equal,
	      [{?__(25,"Horizontal"),horizontal,?__(644,"Distribute horizontally")},
	       {?__(27,"Vertical"),vertical,?__(645,"Distribute vertically")},
           separator,
           {?__(647,"Proportional"),proportional,
            ?__(648,"Proportionally distributes the vertices on loop to match the proportions on the object")}]},
	     ?__(646,"Distribute vertices evenly")},
	    separator,
	    {?__(66,"Stitch"), stitch, ?__(67,"Stitch edges/charts")},
	    {?__(68,"Cut"), cut_edges, ?__(69,"Cut selected edges")}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, {auv,edge}, Menu);
command_menu(vertex, X, Y) ->
    Scale = scale_directions(true),
    Move = move_directions(true),
    Align =
        [{?__(70,"Free"),rotate_free(true),
          {?__(71,"Rotate selection freely"),[],?__(59,"Pick rotation center")}, [magnet]},
        {?__(72,"Chart to X"), align_x,
        ?__(73,"Rotate chart to align (imaginary) edge joining selected verts to X-axis")},
        {?__(74,"Chart to Y"), align_y,
        ?__(75,"Rotate chart to align (imaginary) edge joining selected verts to Y-axis")}],

    Menu = [{?__(77,"Move"),{move,Move},?__(78,"Move selected vertices"),[magnet]},
	    {?__(79,"Scale"),{scale,Scale},?__(80,"Scale selected vertices"), [magnet]},
	    {?__(81,"Rotate"),{rotate,Align},?__(82,"Rotation commands")},
	    separator,
	    {?__(83,"Flatten"),{flatten,
                                [{"X", x, ?__(84,"Flatten horizontally")},
                                 {"Y", y, ?__(85,"Flatten vertically")}]},
	     ?__(86,"Flatten selected vertices")},
        align_menu(),
        {?__(76,"Bend"),bend_submenu_items(),?__(93,"Plastic Bend")},
	    {?__(87,"Tighten"),tighten,
	     ?__(88,"Move UV coordinates towards average midpoint"),
	     [magnet]},
	    separator,
	    {?__(89,"Unfold"),{remap, lsqcm},?__(90,"Unfold the chart (without moving the selected vertices)")},
	    {?__(91,"SphereMap"),sphere,?__(92,"Create a spherical mapping with "
	     "selected vertices being North/South pole")}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, {auv,vertex}, Menu);
command_menu(_, X, Y) ->
    case catch wpc_hlines:init() of
        true -> ExportMenu = [separator, {auv_export_menu(label), export_uv, auv_export_menu(help)}];
        _ -> ExportMenu = []
    end,
    CkdBackground = [{crossmark, ?GET({?MODULE,show_background})}],
    CkdTiled = [{crossmark, ?GET({?MODULE,tiled_texture})}],
    [Label0,Label1] = auv_texture_set_menu(label),
    [Help0,Help1] = auv_texture_set_menu(help),
    [Cmd0,Cmd1] = auv_texture_set_menu(cmd),
    TxSetMode = wings_wm:get_prop(wings_wm:this(), texture_set_mode),
    CkdTextureSet = [{crossmark, TxSetMode}],
    CkdTextureSetId = [{crossmark, ?GET({?MODULE,show_texture_set_id})}],
    case TxSetMode of
      false ->
          TiledMenu = [{auv_show_tile_menu(label),toggle_tiled_texture,auv_show_tile_menu(help),CkdTiled}],
          ShowTileId = [];
      true ->
          TiledMenu = [],
          ShowTileId = [{Label1,Cmd1,Help1,CkdTextureSetId}]
    end,
    Menu = [{auv_show_menu(label),toggle_background,auv_show_menu(help),CkdBackground}] ++
           ShowTileId ++
           [separator] ++ TiledMenu ++
           [{Label0,Cmd0,Help0,CkdTextureSet}] ++
           ExportMenu ++ option_menu(),
    wings_menu:popup_menu(X,Y, {auv,option}, Menu).

stretch_directions() ->
    [{?__(1,"Max Uniform"), max_uniform(),
      {?__(2,"Maximize either horizontally or vertically"),
       ?__(7,"Maximize by using the horizontal dimension"),
       ?__(8,"Maximize by using the vertical dimension")}, []},
     {?__(3,"Max Horizontal"), max_x, ?__(4,"Maximize horizontally (X dir)")},
     {?__(5,"Max Vertical"),   max_y, ?__(6,"Maximize vertically (Y dir)")}].

move_directions(true) ->
    [{?__(1,"Free"), free_2d, ?__(2,"Move in both directions"), [magnet]},
     {?__(3,"Horizontal"), x, ?__(4,"Move horizontally (X dir)"), [magnet]},
     {?__(5,"Vertical"),   y, ?__(6,"Move vertically (Y dir)"), [magnet]}];
move_directions(false) ->
    [{?__(1,"Free"), free_2d, ?__(2,"Move in both directions")},
     {?__(3,"Horizontal"), x, ?__(4,"Move horizontally (X dir)")},
     {?__(5,"Vertical"),   y, ?__(6,"Move vertically (Y dir)")}].

scale_directions(true) ->
    ChosePoint = ?__(7,"Choose point to scale from"),
    [{?__(1,"Uniform"),    uniform_scale([magnet]),
      {?__(2,"Scale in both directions"),[],ChosePoint}, [magnet]},
     {?__(3,"Horizontal"), scale(x,[magnet]),
      {?__(4,"Scale horizontally (X dir)"),[],ChosePoint}, [magnet]},
     {?__(5,"Vertical"),   scale(y,[magnet]),
      {?__(6,"Scale vertically (Y dir)"),[],ChosePoint}, [magnet]}];
scale_directions(false) ->
    ChosePoint = ?__(7,"Choose point to scale from"),
    [{?__(1,"Uniform"),    uniform_scale([]),
      {?__(2,"Scale in both directions"),[],ChosePoint}},
     {?__(3,"Horizontal"), scale(x,[]),
      {?__(4,"Scale horizontally (X dir)"),[],ChosePoint}},
     {?__(5,"Vertical"),   scale(y,[]),
      {?__(6,"Scale vertically (Y dir)"),[],ChosePoint}}].

uniform_scale(Flags) ->
    fun(B, Ns) ->
        case B of
            1 -> wings_menu:build_command({'ASK',{[],[center,uniform],Flags}}, Ns);
            _ -> wings_menu:build_command({'ASK',{[point],[uniform],Flags}}, Ns)
        end
    end.
scale(Axis, Flags) ->
    fun(B, Ns) ->
        case B of
            1 -> wings_menu:build_command({'ASK',{[],[center,Axis],Flags}}, Ns);
            _ -> wings_menu:build_command({'ASK',{[point],[Axis],Flags}}, Ns)
        end
    end.

rotate_free(true) ->
    rotate([magnet]);
rotate_free(false) ->
    rotate([]).

rotate(Flags) ->
    fun(B, Ns) ->
        case B of
            1 -> wings_menu:build_command({free,{'ASK',{[],[center,z],Flags}}}, Ns);
            _ -> wings_menu:build_command({free,{'ASK',{[point],[z],Flags}}}, Ns)
        end
    end.

align_menu() ->
    {?__(93,"Align"),
     {align,
      [{?__(9,"Center"), center, ?__(99,"Align to Center")},
       {?__(11,"Center X"), center_x, ?__(100,"Align to horizontal center")},
       {?__(13,"Center Y"), center_y, ?__(101,"Align to vertical center")},
       {?__(15,"Bottom"), bottom, ?__(95,"Align to bottom")},
       {?__(17,"Top"), top, ?__(96,"Align to top")},
       {?__(19,"Left"), left, ?__(97,"Align to left")},
       {?__(21,"Right"), right, ?__(98,"Align to right")}
      ]}, ?__(94,"Align charts relative each other")}.

bend_submenu_items() ->
    {bend,{plastic_bend,noclamp},
             {'ASK',{[{point, ?__(1,"Pick rod center")},
                      {point, ?__(2,"Pick rod top")},
                      {axis,  ?__(3,"Pick bend normal")}],[],[]}}}.

max_uniform() ->
    fun
        (1, _Ns) -> {auv,{scale,max_uniform}};
        (2, _Ns) -> {auv,{scale,{max_uniform,x}}};
        (3, _Ns) -> {auv,{scale,{max_uniform,y}}}
    end.

option_menu() ->
    [separator,
     {?__(1,"Create Texture"),create_texture,?__(2,"Make and Attach a texture to the model")}].

%%% Event handling

get_event(#st{}=St) ->
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    get_event_nodraw(St).

get_event_nodraw(#st{}=St) ->
    wings_wm:current_state(St),
    {replace,fun(Ev) -> ?MODULE:handle_event(Ev, St) end}.

handle_event({crash,Crash}, _) ->
    wings_u:win_crash(Crash),
    delete;
handle_event({command_error,Error}, _) ->
    wings_u:message(Error);
handle_event(redraw, St) ->
    redraw(St),
    get_event_nodraw(St);
handle_event(init_opengl, St) ->
    wings:init_opengl(St),
    get_event(St);
handle_event(resized, St) ->
    get_event(St);
handle_event({new_state,St}, _) ->
    new_state(St);
handle_event(revert_state, St) ->
    get_event(St);
handle_event({current_state,geom_display_lists,GeomSt}, AuvSt) ->
    new_geom_state(GeomSt, AuvSt);
handle_event({do_tweak, Type, St =#st{sh=Sh,selmode=Mode}}, _) ->
    case Type of 
	temp_selection ->
	    handle_command(move,St#st{temp_sel={Mode,Sh}});
	_ -> 
	    handle_command(move,St)
    end;
handle_event({cancel_tweak,Ev}, St) ->
    handle_event_1(Ev,St,wings_msg:free_lmb_modifier());
handle_event({add_faces,Fs,GeomSt}, St0) ->
    AuvSt0 = add_faces(Fs,St0),
    case update_geom_state(GeomSt, AuvSt0) of
	{AuvSt,true} ->
	    wings_wm:send(geom, {new_state,GeomSt}),
	    new_state(AuvSt);
	{AuvSt,false} ->
	    get_event(AuvSt)
    end;
handle_event(Ev, St) ->
    case wings_camera:event(Ev, St) of
	next ->
	    FreeLmbMod = wings_msg:free_lmb_modifier(),
%%	    io:format("Ev ~W~n",[Ev,3]),
	    handle_event_0(Ev, St, FreeLmbMod);
	Other -> 
	    Other
    end.

handle_event_0(Ev=#mousebutton{state=?SDL_PRESSED,
                               x=X,y=Y,
                               button=?SDL_BUTTON_LEFT,
                               mod=Mod}, #st{}=St0, FreeLmbMod)
  when (Mod band ?ALT_BITS) =/= 0 -> %% ALT modifier
    case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
        true ->
            St = pick_uv_tile(X,Y,St0),
            get_event(St);
        false ->
            handle_event_1(Ev, St0, FreeLmbMod)
    end;
%% Short cut for tweak like move
handle_event_0(Ev=#mousebutton{state=?SDL_PRESSED,
			       x=X,y=Y,
			       button=?SDL_BUTTON_LEFT,
			       mod=Mod},
	       #st{sel=Sel}=St0, FreeLmbMod) 
  when (Mod band 16#0FFF) == 0 -> %% No modifiers
    case Sel of
	[] ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} -> 
		    start_tweak(temp_selection, Ev, St);
		_ -> 
		    handle_event_1(Ev, St0, FreeLmbMod)
	    end;
	_ ->
	    case wings_pick:do_pick(X,Y,St0) of
		{delete,_,_} ->
		    start_tweak(selection, Ev, St0);
		_ -> 
		    handle_event_1(Ev, St0, FreeLmbMod)
	    end
    end;
handle_event_0(Ev, St, FreeLmbMod) ->
    handle_event_1(Ev, St, FreeLmbMod).

handle_event_1(Ev, St, _) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_2(Ev, St);
	Other -> Other
    end.

handle_event_2(Ev, St) ->
    case wings_hotkey:event(Ev, St) of
	next -> handle_event_3(Ev, St);
	Cmd -> wings_wm:later({action,Cmd})
    end.

handle_event_3(#mousebutton{button=?SDL_BUTTON_RIGHT}=Ev,
	       #st{selmode=Mode0,sel=Sel}) ->
    %% Note: Basic menus must be shown when the right mouse button
    %% is PRESSED; advanced menus when the button is RELEASED.
    %% wings_menu:is_popup_event/1 takes care of that.
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    Mode = case Sel of 
		       [] -> undefined; 
		       _ -> Mode0 
		   end,
	    command_menu(Mode, X, Y)
    end;
handle_event_3({drop,DropData}, St) ->
    handle_drop(DropData, St);
handle_event_3({action,{{auv,_},create_texture}}, St) ->
    ?SET({?MODULE,show_background}, true),
    auv_texture:draw_options(remap_uv_tile(St));
handle_event_3({action,{auv,{draw_options,restart}}}, St) ->
    ?SET({?MODULE,show_background}, true),
    ?SET({?MODULE,tiled_texture}, false),
    auv_texture:draw_options(St);
handle_event_3({action,{auv,{draw_options,Opt}}}, #st{bb=Uvs}=St) ->
    #uvstate{st=GeomSt0,matname=MatName0,bg_img=Image} = Uvs,
    Tx = ?SLOW(auv_texture:get_texture(remap_uv_tile(St), Opt)),
    case MatName0 of
	none ->
	    ok = wings_image:update(Image, Tx),
	    ?SET({?MODULE,show_background}, true),
	    get_event(St);
	_ ->
	    TexName = case get_texture(MatName0, St) of
			  false -> atom_to_list(MatName0);
			  Old  -> 
			      OldE3d = wings_image:info(Old),
			      case OldE3d#e3d_image.name of
				  "auvBG" -> atom_to_list(MatName0);
				  Other -> Other
			      end
		      end,
	    {GeomSt,MatName} = update_texture(Tx#e3d_image{name=TexName},
                                              MatName0, GeomSt0),
            ImId = get_texture(MatName, GeomSt),
	    wings_wm:send(geom, {new_state,GeomSt}),
	    get_event(St#st{bb=Uvs#uvstate{bg_img=ImId, st=GeomSt,matname=MatName}})
    end;
%% Others
handle_event_3({vec_command,Command,_St}, _) when is_function(Command, 0) ->
    %% Use to execute command with vector arguments (see wings_vec.erl).
    Command();
handle_event_3(close, _St) ->
    cleanup_before_exit(),
    delete;
handle_event_3({callback,Fun}, _) when is_function(Fun) ->
    Fun();
handle_event_3({action,{auv,quit}}, _St) ->
    cleanup_before_exit(),
    delete;
handle_event_3({action,{{auv,_},Cmd}}, St) ->
    %%    io:format("Cmd ~p ~n", [Cmd]),
    handle_command(Cmd, St);
handle_event_3({action,{auv,Cmd}}, St) ->
    handle_command(Cmd, St);
handle_event_3({action,{toggle_texture_set_mode,_}=Cmd}, St) ->
    %%    io:format("Cmd ~p ~n", [Cmd]),
    handle_command(Cmd, St);
handle_event_3({action,{select,show_all}}, #st{bb=#uvstate{st=GeomSt,id=Id}}) ->
    wings_wm:send({autouv,Id}, {add_faces,object,GeomSt}),
    keep;
handle_event_3({action,{select,oriented_faces}}, St0) ->
    Connected = wings_pref:get_value(similar_normals_connected,false),
    {Save,Angle} = case wings_pref:get_value(similar_normals_angle,{false,1.0E-3}) of
		       {true,A} -> {true,A};
		       {false,_} -> {false,1.0E-3}
		   end,
    handle_event_3({action,{select,{oriented_faces,[Angle,Connected,Save]}}}, St0);
handle_event_3({action,{select,similar_area}}, St0) ->
    handle_event_3({action,{select,{similar_area,[0.001]}}}, St0);
handle_event_3({action,{select,similar_material}}, St0) ->
    Connected = wings_pref:get_value(similar_materials_connected, false),
    Mode = wings_pref:get_value(similar_materials, material),
    handle_event_3({action,{select,{similar_material,[Connected,Mode]}}}, St0);
handle_event_3({action,{select,{ssels,sel_groups_win}}}, _) ->
    keep;
handle_event_3({action,{select,deselect_previous}=Command}, St0) ->
    case wpc_deselect_previous:command(Command, St0) of
	{save_state,St} -> ok;
	_ -> St = St0
    end,
    new_state(St);
handle_event_3({action,{select,Command}}, St0) ->
    case wings_sel_cmd:command(Command, St0) of
	{save_state,St} -> ok;
	#st{}=St -> ok;
	_ ->
	    %% That's avoid crash if any select option has a input dialog when
	    %% usually the returned value returned at the first time is 'keep'.
	    %% Also, for commands with preview dialog, the new state will not
	    %% be properly updated since somehow the local St seems to be messed
	    %% in {current_state,geom_display_lists,GeomSt} event handle
	    St = St0
    end,
    new_state(St);
handle_event_3({action,{edit,repeat}}, St) ->
    repeat(command, St);
handle_event_3({action,{edit,repeat_args}}, St) ->
    repeat(args, St);
handle_event_3({action,{edit,repeat_drag}}, St) ->
    repeat(drag, St);
handle_event_3({action,Ev}=Act, #st{selmode=AUVSel, bb=#uvstate{st=#st{selmode=GSel}}}=St) ->
    case Ev of  %% Keyboard shortcuts end up here (I believe)
	{_, {move,_}} ->
	    handle_command(move,St);
	{_, {rotate,_}} ->
	    handle_command(rotate,St);
	{_, {scale,{Dir,_S}}} ->
	    handle_command({scale,Dir},St);
	{_, {scale,_S}} ->
	    handle_command({scale,uniform},St);
	{_, slide} ->
	    handle_command(slide,St);
	{_, circularise} ->
	    handle_command(circularise,St);
	{view,{show,toggle_background}} ->
	    handle_command(toggle_background,St);
	{view,{show,toggle_tiled_texture}} ->
	    handle_command(toggle_tiled_texture,St);
    {view,{show,toggle_texture_set_mode}} ->
        handle_command(toggle_texture_set_mode,St);
	{view,aim} ->
	    St1 = fake_selection(St),
	    wings_view:command(aim, St1),
	    get_event(St);
	{view,highlight_aim} ->
            #st{sel=Sel} = St,
            case  Sel =:= [] of
                true ->
                    case fake_selection(St) of
                        #st{sel=[]} -> camera_reset();
                        St1 -> wings_view:command(aim, St1)
                    end,
                    get_event(St);
                false ->
                    {{_,Cmd},St1} = wings:highlight_aim_setup(St),
                    wings_view:command(Cmd,St1),
                    get_event(St)
            end;
	{view,Cmd} when Cmd == frame ->
	    wings_view:command(Cmd,St),
	    get_event(St);
	{view,Cmd} when Cmd == reset ->
	    camera_reset(),
	    get_event(St);
	{edit, repeat} ->
	    repeat(command, St);
	{edit, repeat_args} ->
	    repeat(args, St);
	{edit,repeat_drag} ->
	    repeat(drag, St);
        _ when AUVSel =:= GSel ->
            wings_wm:send_after_redraw(geom, Act),
            keep;
        {body, _} -> keep;
        {face, _} -> keep;
        {edge, _} -> keep;
        {vertex,_} -> keep;
        _ ->
            wings_wm:send_after_redraw(geom, Act),
            keep
    end;
handle_event_3(got_focus, _) ->
    Msg1 = wings_msg:button_format(?__(1,"Select")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(2,"Show menu")),
    Msg4 =
        case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
            true -> wings_msg:mod_format(?ALT_BITS, 1, "Set Active Tile");
            false -> []
        end,
    Message = wings_msg:join([Msg1,Msg2,Msg3,Msg4]),
    wings_wm:message(Message, ""),
    auv_show_menu(true),
    auv_show_tile_menu(true),
    auv_texture_set_menu(true),
    wings_wm:dirty();
handle_event_3(lost_focus, _) ->
    auv_show_menu(false),
    auv_show_tile_menu(false),
    auv_texture_set_menu(false),
    keep;
handle_event_3(_Event, _) ->
    %% io:format("MissEvent ~P~n", [_Event, 20]),
    keep.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.

-record(tweak, {type, st, pos, ev}).

start_tweak(Type, Ev = #mousebutton{x=X,y=Y}, St0) ->
    T = #tweak{type=Type,st=St0,pos={X,Y}, ev=Ev},
    {seq,push,get_tweak_event(T)}.

get_tweak_event(T) ->
    {replace,fun(Ev) -> tweak_event(Ev, T) end}.
tweak_event(#mousemotion{x=X,y=Y}, #tweak{pos={Sx,Sy},type=Type,st=St}) ->
    case (abs(X-Sx) > 2) orelse (abs(Y-Sy) > 2) of
	true -> 
	    if Type == temp_selection ->
		    wings_wm:later(clear_selection);
	       true -> ignore
	    end,
	    wings_wm:later({do_tweak,Type,St}),
	    pop;
	false ->
	    keep
    end;
tweak_event(Other, #tweak{ev=Ev}) ->
    wings_wm:later(Other),
    wings_wm:later({cancel_tweak,Ev}),
    pop.

new_state(#st{bb=#uvstate{}=Uvs}=St0) ->
    GeomSt = update_geom_selection(St0),
    St1 = St0#st{bb=Uvs#uvstate{st=GeomSt}},
    St = update_selected_uvcoords(St1),
    get_event(St).

handle_command(Cmd, St0) ->
    case handle_command_1(Cmd,remember_command(Cmd,St0)) of
	Drag = {drag, _} -> do_drag(Drag);
	Result -> Result
    end.

handle_ask(Cmd, St) ->
%%    io:format("Handle Ask ~p ~n",[Cmd]),
    do_drag(handle_command_1(Cmd,St)).

handle_command_1({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun handle_ask/2);
handle_command_1({remap,Method}, St0) ->
    St = remap(Method, St0),
    get_event(St);
handle_command_1(move, St) ->
    wings_move:setup(free_2d, St);
handle_command_1({move,{'ASK',Ask}}, St) ->
    wings:ask(Ask, St, fun(M,St0) ->
			       do_drag(wings_move:setup(M, St0))
		       end);
handle_command_1({move,Axis}, St) ->
    wings_move:setup(Axis, St);
handle_command_1({scale,Dir}, St0) %% Maximize chart
  when Dir == max_uniform; Dir == {max_uniform,x}; Dir == {max_uniform,y};
       Dir == max_x; Dir == max_y ->
    St1 = wpa:sel_map(fun(_, We) -> stretch(Dir,We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1({scale,normalize}, St0) -> %% Normalize chart sizes    
    #st{shapes=Sh0, bb=#uvstate{id=Id,st=#st{shapes=Orig}}} = St0,
    OWe = gb_trees:get(Id, Orig), 
    {TA2D,TA3D,List} = wings_sel:fold(fun(_,We,Areas) -> 
					      calc_areas(We,OWe,Areas)
				      end, {0.0,0.0,[]}, St0),
    TScale = TA2D/TA3D,
    Scale = fun({A2D,A3D,We0 = #we{id=WId}},Sh) ->
		    Scale = math:sqrt(TScale * A3D/wings_util:nonzero(A2D)),
		    Center = wings_vertex:center(We0),
		    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
		    SM = e3d_mat:scale(Scale, Scale, 1.0),
		    T1 = e3d_mat:mul(SM, T0),
		    T = e3d_mat:mul(e3d_mat:translate(Center), T1),
		    We = wings_we:transform_vs(T, We0),
		    gb_trees:update(WId,We,Sh)
	    end,
    Sh = lists:foldl(Scale, Sh0, List),
    St = update_selected_uvcoords(St0#st{shapes=Sh}),
    get_event(St);
handle_command_1({scale, {'ASK', Ask}}, St) ->
    wings:ask(Ask, St, fun({Dir,M},St0) when is_tuple(M), element(1,M) == magnet ->
			       do_drag(wings_scale:setup({Dir,center,M}, St0));
                          ({Dir,Point},St0) ->
			       do_drag(wings_scale:setup({Dir,Point}, St0));
                          ({Dir,Point,M},St0) ->
			       do_drag(wings_scale:setup({Dir,Point,M}, St0))
		       end);
handle_command_1({scale,{Dir,M}}, St) when is_tuple(M), element(1,M) == magnet ->
    wings_scale:setup({Dir,center,M}, St); % For repeat drag
handle_command_1({scale,{Dir,Point}}, St) ->
    wings_scale:setup({Dir,Point}, St);
handle_command_1({scale,{Dir,Point,M}}, St) ->
    wings_scale:setup({Dir,Point,M}, St);
handle_command_1({scale,Dir}, St) ->
    wings_scale:setup({Dir,center}, St);
handle_command_1(rotate, St) ->
    wings_rotate:setup({free,center}, St);
handle_command_1({rotate,{free,{z,Point}}}, St) ->
    wings_rotate:setup({free,Point}, St);
handle_command_1({rotate, {free, {'ASK', Ask}}}, St) ->
    wings:ask(Ask, St, fun({Dir,M},St0) when is_tuple(M), element(1,M) == magnet ->
			       do_drag(wings_rotate:setup({Dir,center,M}, St0));
                          ({Dir,Point},St0) ->
                              do_drag(wings_rotate:setup({Dir,Point}, St0));
                          ({Dir,Point,M},St0) ->
                              do_drag(wings_rotate:setup({Dir,Point,M}, St0))
		       end);
handle_command_1({rotate, {free,{z,Point,Magnet}}}, St) when element(1, Magnet) == magnet ->
    wings_rotate:setup({free,Point,Magnet}, St); % For repeat drag
handle_command_1({rotate,Dir}, St0) 
  when Dir == align_y; Dir == align_x; Dir == align_xy ->
    St1 = align_chart(Dir, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1({rotate,Deg}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> rotate_chart(Deg, We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1({move_to,Dir}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> move_to(Dir,We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1({align,Dir}, #st{selmode=Mode,sel=Sel}=St0) when Mode == body; Mode == vertex ->
    St =
        case length(Sel) of
            1 ->
                align_error(Mode),
                St0;
            _ ->
                BB = wings_sel:bounding_box(St0),
                St1 = wpa:sel_map(fun(_, We) -> align(Dir,BB,We) end, St0),
                update_selected_uvcoords(St1)
        end,
    get_event(St);
handle_command_1({bend,Type,{'ASK',Ask}}, St) ->
    wings:ask(Ask, St, fun(B,St0) ->
                    do_drag(wpc_bend:setup({Type,B}, St0))
               end);
handle_command_1({bend,{Type,Param}}, St) ->
    wpc_bend:setup({Type,Param}, St);
handle_command_1({bend,Type,Param}, St) ->
    wpc_bend:setup({Type,Param}, St);
handle_command_1({flip,horizontal}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_horizontal(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1({flip,vertical}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_vertical(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1(slide, St) ->
    wings_edge_cmd:command(slide,St);
handle_command_1({equal,Op}, St0) ->
    St1 = equal_length(Op,St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1(tighten, St) ->
    tighten(St);
handle_command_1({tighten,Magnet}, St) ->
    tighten(Magnet, St);
handle_command_1(delete, St) ->
    get_event(delete_charts(St));
handle_command_1(hide, St) ->
    get_event(hide_charts(St));
handle_command_1({flatten, Plane}, St0 = #st{selmode=vertex}) ->
    {save_state, St1} = wings_vertex_cmd:flatten(Plane, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command_1(stitch, St0 = #st{selmode=edge}) ->
    Es = wpa:sel_fold(fun(Es,We=#we{name=#ch{emap=Emap},es=Etab},A) ->
			      Vis = gb_sets:from_list(wings_we:visible(We)),
			      [auv_segment:map_edge(E,Emap)
			       || E<-gb_sets:to_list(Es),
				  begin 
				      #edge{lf=LF,rf=RF}=array:get(E, Etab),
				      border(gb_sets:is_member(LF,Vis),
					       gb_sets:is_member(RF,Vis))
				  end] ++ A
		      end, [], St0),
    St1 = stitch(lists:usort(Es),St0),
    AuvSt = #st{bb=#uvstate{id=Id,st=Geom}} = update_selected_uvcoords(St1),
    %% Do something here, i.e. restart uvmapper.
    St = rebuild_charts(gb_trees:get(Id,Geom#st.shapes), AuvSt, []),
    get_event(St);
handle_command_1(cut_edges, St0 = #st{selmode=edge,bb=#uvstate{id=Id,st=Geom}}) ->
    Es = wpa:sel_fold(fun(Es,We=#we{name=#ch{emap=Emap},es=Etab},A) ->
			      Vis = gb_sets:from_list(wings_we:visible(We)),
			      A ++ [auv_segment:map_edge(E,Emap)
				    || E <-gb_sets:to_list(Es),
				       begin 
					   #edge{lf=LF,rf=RF} = array:get(E,Etab),
					   gb_sets:is_member(LF,Vis) and 
					       gb_sets:is_member(RF,Vis)
				       end]
		      end, [], St0),
    %% Do something here, i.e. restart uvmapper.
    St1 = rebuild_charts(gb_trees:get(Id,Geom#st.shapes), St0, Es),
    %% Displace charts some distance
    St2 = displace_cuts(Es, St1),
    St  = update_selected_uvcoords(St2),
    get_event(St);
handle_command_1(toggle_background, _) ->
    Old = ?GET({?MODULE,show_background}),
    ?SET({?MODULE,show_background},not Old),
    wings_wm:dirty();
handle_command_1(toggle_tiled_texture, _) ->
    Old = ?GET({?MODULE,tiled_texture}),
    ?SET({?MODULE,tiled_texture},not Old),
    wings_wm:dirty();
handle_command_1(toggle_texture_set_id,_) ->
    Old = ?GET({?MODULE,show_texture_set_id}),
    ?SET({?MODULE,show_texture_set_id},not Old),
    wings_wm:dirty();
handle_command_1(toggle_texture_set_mode,St) ->
    case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
        false ->  %% object is going to have multiple texture set enabled
            Win = wings_wm:this_win(),
            Pos = wx_misc:getMousePosition(),
            wings_menu:popup_menu(Win,Pos,toggle_texture_set_mode,auv_txset_naming_menu());
        true ->
            handle_command_1({toggle_texture_set_mode, off},St)
    end;
handle_command_1({toggle_texture_set_mode,{txset_naming,TxSetNaming}},#st{bb=Uvs0,shapes=Charts0}=St) ->
    #uvstate{st=#st{shapes=Shs0}=GeomSt0, id=Id} = Uvs0,
    We0 = gb_trees:get(Id, Shs0),
    ?SET({?MODULE,tiled_texture},false),  %% disable tiled texture
    {TxSet,Charts,We1,#st{mat=Mtab}} = build_texture_set(TxSetNaming,Charts0,We0,GeomSt0),
    [_,[{Tile,#{mat:=MatName,bg_img:=Image}}|_]] = TxSet,
    %% updating the texture set info to the #we{}
    We = update_textureset_system(We1, ?MULTIPLE, TxSet),
    Shs = gb_trees:enter(Id,We,Shs0),
    GeomSt = GeomSt0#st{shapes=Shs,mat=Mtab},
    wings_wm:send(geom, {new_state,GeomSt}),
    new_state(St#st{shapes=Charts,mat=Mtab,bb=Uvs0#uvstate{st=GeomSt,tile=Tile,bg_img=Image,matname=MatName}});
handle_command_1({toggle_texture_set_mode,off},#st{bb=Uvs0}=St) ->
    #uvstate{st=#st{shapes=Shs0}=GeomSt0, id=Id} = Uvs0,
    We0 = gb_trees:get(Id, Shs0),
    Tile = {0,0},
    %% removing the texture set info to the #we{}
    We = update_textureset_system(We0, ?SINGLE, []),
    Shs = gb_trees:enter(Id,We,Shs0),
    GeomSt = GeomSt0#st{shapes=Shs},
    wings_wm:send(geom, {new_state,GeomSt}),
    new_state(St#st{bb=Uvs0#uvstate{st=GeomSt,tile=Tile}});
handle_command_1(export_uv, #st{}=St) ->
    wpc_hlines:command({file, {export_uv, {eps, true}}}, St);
handle_command_1(Cmd, #st{selmode=Mode}=St0) ->
    case wings_plugin:command({{auv,Mode},Cmd}, St0) of
      next ->
        io:format("Error unknown command ~p ~n", [Cmd]),
        keep;
      St0 -> St0;
      #st{}=St -> {save_state,St};
      Other -> Other
    end.

remember_command(Cmd,St0) ->
    St0#st{repeatable=Cmd,ask_args=none,drag_args=none}.

repeat(_Type, #st{sel=Sel,repeatable=Rep}) when Sel == []; Rep == ignore ->
    keep;
repeat(Type, St=#st{selmode=Mode, repeatable=Cmd}) ->
%%     io:format("Repeat ~p ~n", [{St#st.repeatable,St#st.ask_args,St#st.drag_args}]),
    case repeatable(Cmd,Mode) of
	false -> 
%% 	    io:format("Not repeatable ~p ~p ~p~n",[Type,Cmd,Mode]),
	    keep;
	true ->
%% 	    io:format("Repeatable ~p ~p ~p ~n",[Type,Cmd,Mode]),
	    repeat(Type, Cmd, St)
    end.

repeat(command, Cmd, St) ->
    repeat2(Cmd,St,none);
repeat(args, Cmd0, St = #st{ask_args=AskArgs}) ->
    Cmd =replace_ask(Cmd0, AskArgs),
    repeat2(Cmd,St,none);
repeat(drag, Cmd0, St = #st{ask_args=AskArgs,drag_args=DragArgs}) ->
    Cmd = replace_ask(Cmd0, AskArgs),
    repeat2(Cmd,St,DragArgs).

repeat2(Cmd,St,DragArgs) ->
    case handle_command_1(Cmd,St) of
	{drag,Drag} ->
	  wings_wm:set_prop(show_info_text, true),
	  wings_drag:do_drag(Drag, DragArgs); 
	Other -> Other
    end.

replace_ask(Term, none) -> Term;
replace_ask({'ASK',_}, AskArgs) -> AskArgs;
replace_ask(Tuple0, AskArgs) when is_tuple(Tuple0) ->
    Tuple = [replace_ask(El, AskArgs) || El <- tuple_to_list(Tuple0)],
    list_to_tuple(Tuple);
replace_ask(Term, _) -> Term.

repeatable({remap, proj_lsqcm}, Mode) -> Mode == face;
repeatable({remap, proj_slim}, Mode) -> Mode == face;
repeatable({remap,_},body) -> true;
repeatable({remap,_},Mode) -> Mode == vertex;
repeatable({scale, Dir}, Mode) 
  when ((Dir == max_uniform) or (Dir == max_x) or (Dir == max_y) or 
	(Dir == normalize)) and (Mode /= body) -> false;
repeatable({rotate, Dir}, Mode) 
  when ((Dir == align_y) or (Dir == align_x) or (Dir == align_xy)) 
       and ((Mode == body) or (Mode == face)) -> false;
repeatable({move_to,_},Mode) -> Mode == body;
repeatable({flip,_},Mode) -> Mode == body;
repeatable(slide, Mode) -> Mode == edge;
repeatable({equal,_}, Mode) -> Mode == edge;
repeatable(tighten, Mode) ->
    (Mode == vertex) orelse (Mode == body);
repeatable({tighten,_}, Mode) -> 
    (Mode == vertex) orelse (Mode == body);
repeatable(delete, Mode) -> Mode == body;
repeatable(hide, Mode) -> Mode == body;
repeatable(flatten, Mode) -> Mode == edge;
repeatable(stitch, Mode) ->  Mode == edge;
repeatable(cut_edges, Mode) -> Mode == edge;
repeatable(_Cmd,_Mode) ->
    true.

fake_selection(St) ->
    wings_dl:fold(fun(#dlo{src_sel=none}, S) ->
			  %% No selection, try highlighting.
			  fake_sel_1(S);
		     (#dlo{src_we=#we{id=Id},src_sel={Mode,Els}}, S) ->
			  S#st{selmode=Mode,sel=[{Id,Els}]}
		  end, St).

fake_sel_1(St0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
	{add,_,St} -> St;
	_ -> St0
    end.

add_faces(NewFs,St0=#st{bb=ASt=#uvstate{id=Id,mode=Mode,st=GeomSt=#st{shapes=Shs0}}}) ->
    case {NewFs,Mode} of
	{_,object} ->
            All = wings_obj:fold(fun(#{id:=I}, A) -> [I|A] end, [], St0),
            wings_obj:unhide(All, St0);
	{object,_} -> %% Force a chart rebuild, we are switching object mode
	    We = gb_trees:get(Id,Shs0),
	    Shs = gb_trees:update(Id, We#we{fs=undefined,es=array:new()}, Shs0),
	    Fake = GeomSt#st{sel=[],shapes=Shs},
	    St0#st{bb=ASt#uvstate{mode=object,st=Fake}};
	{NewFs,Fs0} ->
	    Fs = gb_sets:union(NewFs,Fs0),
	    case gb_sets:intersection(NewFs,Fs0) of
		NewFs -> 
		    St0#st{bb=ASt#uvstate{mode=Fs}};
		_ ->  %% Some new faces should be shown, force a chart rebuild
		    We = gb_trees:get(Id,Shs0),
		    Shs = gb_trees:update(Id, We#we{fs=undefined,es=array:new()}, Shs0),
		    Fake = GeomSt#st{sel=[],shapes=Shs},
		    St0#st{bb=ASt#uvstate{st=Fake,mode=Fs}}
	    end
    end.

do_drag({drag,Drag}) -> 
    wings:mode_restriction([vertex,edge,face,body]),
    wings_wm:set_prop(show_info_text, true),
    wings_drag:do_drag(Drag,none);
do_drag(Other) ->
    Other.

tighten(#st{selmode=vertex}=St) ->
    tighten_1(fun vertex_tighten/2, St);
tighten(#st{selmode=body}=St) ->
    tighten_1(fun(_, We) -> body_tighten(We) end, St).

tighten_1(Tighten, St) ->
    wings_drag:fold(Tighten, [percent], St).

vertex_tighten(Vs0, We) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_sets:to_list(Vs0), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten_vs(Vs, We).

body_tighten(#we{vp=Vtab}=We) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- wings_util:array_keys(Vtab), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten_vs(Vs, We).

tighten(Magnet, St) ->
    Flags = wings_magnet:flags(Magnet, []),
    wings_drag:fold(fun(Vs, We) ->
                            mag_vertex_tighten(Vs, We, Magnet)
                    end, [percent,falloff], Flags, St).

mag_vertex_tighten(Vs0, We, Magnet) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_sets:to_list(Vs0), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten_vs(Vs, We, Magnet).

equal_length(Op,#st{bb=Uvs}=St) when Op == proportional ->
    #uvstate{st=#st{shapes=Shs0},id=Id} = Uvs,
    #we{es=Etab} = We = gb_trees:get(Id, Shs0),
    wings_sel:map(fun(Es, #we{name=#ch{emap=Emap}}=UvWe) ->
                EuvToEwe = [{E,auv_segment:map_edge(E,Emap)} || E <- gb_sets:to_list(Es)],
                UvLinks = wings_edge_loop:edge_links(Es,UvWe),
                PickVs = fun(E) ->
                            #edge{vs=Vs,ve=Ve} = array:get(E, Etab),
                            {E,Vs,Ve}
                         end,
                WeVs = ordsets:from_list([PickVs(E) || {_,E} <- EuvToEwe]),
                RemapVs = fun(E0) ->
                        E = proplists:get_value(E0,EuvToEwe),
                        {_,Ve,Vs} = lists:keyfind(E,1,WeVs),
                        %% we use the edge id from island instead of the real object one
                        {E0,Ve,Vs}
                    end,
                %% sync edge loops from the island with the edges on real object
                WeLinks = [[RemapVs(E) || {E,_,_} <- UvLink] || UvLink <- UvLinks],
                make_proportional(UvLinks,UvWe,WeLinks,We)
          end, St);
equal_length(Op,St) ->
    wings_sel:map(fun(Es, We) ->
			  Links = wings_edge_loop:edge_links(Es,We),
			  make_equal(Op,Links,We)
		  end, St).

make_equal(Op,[Link0|R],We = #we{vp=Vtab}) ->
    E = if Op == horizontal -> 1; true -> 2 end,
    case length(Link0) of
	X when X < 2 -> make_equal(Op,R,We);
	No ->
	    Link = case Link0 of
		       [{_,A,_},{_,_,A}|_] -> Link0;
		       [{_,_,A},{_,A,_}|_] -> reverse(Link0)
		   end,
	    D = foldl(fun({_E,Ve,Vs}, {X,Y,_}) ->
			      {Xd,Yd,_} = e3d_vec:sub(array:get(Ve,Vtab),
						      array:get(Vs,Vtab)),
			      {X+(Xd),Y+(Yd),0.0}
		      end,
		      {0.0,0.0,0.0},Link),
	    Dist = element(E,D)/No,
	    Vt = foldl(fun({_E,Ve,Vs}, Vt) ->
			       Pos1 = array:get(Vs,Vt),
			       Pos2 = array:get(Ve,Vt),
			       Pos = setelement(E,Pos2,element(E,Pos1) + Dist),
			       array:set(Ve,Pos,Vt)
		       end,
		       Vtab,Link),	    
	    make_equal(Op,R,We#we{vp=Vt})
    end;
make_equal(_,[],We) -> We.

make_proportional([[_]|R], We, [_|RObj], WeObj) ->
    %% avoiding single edge selections
    make_proportional(R,We,RObj,WeObj);
make_proportional([Link0|R],We = #we{vp=Vtab1}, [LinkObj0|RObj], WeObj = #we{vp=VtabObj}) ->
    case length(Link0) of
        X when X < 2 -> make_proportional(R,We,RObj,WeObj);
        _No ->
            {Link,LinkObj} =
                case Link0 of
                   [{_,A,_},{_,_,A}|_] -> {Link0,LinkObj0};
                   [{_,_,A},{_,A,_}|_] -> {reverse(Link0),reverse(LinkObj0)}
                end,
            %% get the edge loop and edges length from the real object
            {LinkObjLen,EsObjLen} =
                lists:foldl(fun({E,Ve,Vs}, {Len,Acc}) ->
                                ELen = e3d_vec:dist(array:get(Ve,VtabObj),
                                                    array:get(Vs,VtabObj)),
                                {Len+ELen,[{E,ELen}|Acc]}
                            end,{0.0,[]},LinkObj),
            %% get the edge loop and edges length from the UV island
            {LinkLen,EsLen0} =
                lists:foldl(fun({_E,Ve,Vs}=E, {Len,Acc}) ->
                                {Xd,Yd,_} = e3d_vec:sub(array:get(Ve,Vtab1),
                                                        array:get(Vs,Vtab1)),
                                ELen = e3d_vec:len({Xd,Yd,0.0}),
                                    Dir = if abs(ELen) < ?EPSILON -> none;
                                             true -> e3d_vec:norm({Xd,Yd,0.0})
                                          end,
                                {Len+ELen,[{E,Dir}|Acc]}
                            end,{0.0,[]},Link),
            EsLen = validate_dir(EsLen0,[]),
            %% computing original loop information (BB, Length and rotation)
            [{{_,Ve0,_},_}|_] = EsLen,
            [{{_,_,Vs0},_}|_] = lists:reverse(EsLen),
            IsLoop = Ve0=:=Vs0,
            V0e = array:get(Ve0,Vtab1),
            V0s = array:get(Vs0,Vtab1),
            Es = [Ve || {{_,Ve,_},_} <- EsLen],
            if not IsLoop ->
                Mid0 = e3d_vec:average(V0e,V0s);
            true ->
                Mid0 = e3d_vec:average([array:get(V,Vtab1) || V <- Es])
            end,
            Vec0 = e3d_vec:sub(V0e,V0s),
            D0 = e3d_vec:len(Vec0),
            %% computing the new vertices location
            Vtab0 =
                lists:foldr(fun({{E,Ve,Vs},Dir0}, Acc) ->
                                EObjLen = proplists:get_value(E,EsObjLen),
                                if abs(LinkObjLen) > ?EPSILON -> Prc = EObjLen/LinkObjLen;
                                   true -> Prc = 1.0
                                end,
                                ELen = Prc*LinkLen,
                                Dir = e3d_vec:mul(Dir0,ELen),
                                Pos = e3d_vec:add(array:get(Vs,Acc),Dir),
                                array:set(Ve,Pos,Acc)
                            end,Vtab1,EsLen),
            %% computing new loop information (BB, Length and rotation)
            V1e = array:get(Ve0,Vtab0),
            V1s = array:get(Vs0,Vtab0),
            if not IsLoop ->
                Mid1 = e3d_vec:average(V1e,V1s);
            true ->
                Mid1 = e3d_vec:average([array:get(V,Vtab0) || V <- Es])
            end,
            Vec1 = e3d_vec:sub(V1e,V1s),
            D1 = e3d_vec:len(Vec1),

            %% making the new loop arrangement fit in the old BB length and alignment
            if abs(D1) > ?EPSILON -> Scl = round((D0/D1)*100.0)/100.0;
               true -> Scl = 1.0
            end,
            {_,_,RotSide} = e3d_vec:norm(e3d_vec:cross(Vec1,Vec0)),
            Rot = round(e3d_vec:degrees(Vec1,Vec0)*10.0)/10.0*RotSide,
            MToOri = e3d_mat:translate(e3d_vec:neg(Mid1)),
            if abs(Rot) > ?EPSILON orelse (Scl=/=1.0) ->
                    %% preparing transform matrices
                    MRot = e3d_mat:rotate(Rot,wings_util:make_vector(z)),
                    MScl = e3d_mat:scale({Scl,Scl,1.0}),
                    MToDst = e3d_mat:translate(Mid0),
                    M2 = e3d_mat:mul(MScl,MToOri),
                    M1 = e3d_mat:mul(MRot,M2),
                    M0 = e3d_mat:mul(MToDst,M1);
               true ->
                    MToDst = e3d_mat:translate(Mid0),
                    M0 = e3d_mat:mul(MToDst,MToOri)
            end,
            
            Vtab =
                lists:foldr(fun(V, Acc) ->
                                Pos = e3d_mat:mul_point(M0,array:get(V,Acc)),
                                array:set(V,Pos,Acc)
                            end,Vtab0,Es),
            make_proportional(R,We#we{vp=Vtab},RObj,WeObj)
    end;
make_proportional([],We,_,_) -> We.

%% ensuring the 0 length segments are going to have a valid direction
validate_dir([{_,none}], [{_,none}|Acc]) -> lists:reverse(Acc);
validate_dir([{E,none}], [{_,Dir}|_]=Acc) ->
    validate_dir([{E,Dir}],Acc);
validate_dir([E0], Acc) -> lists:reverse([E0|Acc]);
validate_dir([{E,none}|[{_,Dir}|_]=Es], Acc) when Dir =/= none ->
    validate_dir(Es, [{E,Dir}|Acc]);
validate_dir([{_,Dir}=E0,{E,none}], Acc) when Dir =/= none ->
    validate_dir([{E,Dir}], [E0|Acc]);
validate_dir([E0|Es], Acc) ->
    validate_dir(Es, [E0|Acc]).

calc_areas(We,OWe,{TA2D,TA3D,L}) ->
    Fs = wings_we:visible(We),
    {A2D,A3D} = 
	lists:foldl(fun(Face,{A2D,A3D}) ->
			    %% 2D
			    Vs2 = wpa:face_vertices(Face, We),
			    A2 = auv_mapping:calc_area(Vs2,{0.0,0.0,1.0}, We),
			    %% 3D
			    N3 = wings_face:normal(Face, OWe),
			    Vs3 = wpa:face_vertices(Face, OWe),
			    A3 = auv_mapping:calc_area(Vs3,N3, OWe),
			    {A2D+A2,A3+A3D}
		    end, {0.0,0.0}, Fs),
    {A2D+TA2D,A3D+TA3D,[{A2D,A3D,We}|L]}.

not_bordering(V, Vis, We) ->
    wings_vertex:fold(fun(_, _, _, false) -> false;
			 (_, F, _, true) -> gb_sets:is_member(F, Vis)
		      end, true, V, We).

hide_charts(#st{shapes=Shs0,bb=UVs}=St) ->
    Shs = wpa:sel_fold(fun(_, #we{id=Id}, Shs) ->
			       gb_trees:delete(Id, Shs)
		       end, Shs0, St),
    Fs = foldl(fun(#we{fs=Ftab},Acc) ->
		       Fs = gb_sets:from_ordset(gb_trees:keys(Ftab)),
		       gb_sets:union(Fs,Acc)
	       end, gb_sets:empty(), gb_trees:values(Shs)),
    St#st{shapes=Shs,sel=[],bb=UVs#uvstate{mode=Fs}}.

delete_charts(#st{shapes=Shs0}=St0) ->
    St1 = wpa:sel_map(fun(_, #we{vp=Vp0}=We) ->
			      Vp1 = array:sparse_to_orddict(Vp0),
			      Vp = [{V,none} || {V,_} <- Vp1],
			      We#we{vp=array:from_orddict(Vp)}
		      end, St0),
    St = update_selected_uvcoords(St1),
    Shs = wpa:sel_fold(fun(_, #we{id=Id}, Shs) ->
			       gb_trees:delete(Id, Shs)
		       end, Shs0, St),
    St#st{shapes=Shs,sel=[]}.

border(false,true) -> true;
border(true,false) -> true;
border(_,_) -> false.

stitch(WEs,St0) ->
    Mapped0 = map_edges(WEs,St0),
    %% 1st pass take care and remove all chart-internal cuts.
    {Mapped1,St1} = stitch_edges(Mapped0, St0, []),
    %% Cluster all edges between 2 charts together
    ChartStitches = cluster_chart_moves(lists:keysort(2,Mapped1),[]),
    %% 2nd pass take care of chart stitches
    stitch_charts(ChartStitches,gb_sets:empty(),St1).

stitch_edges([{_E,{Id,_,{Vs1,Ve1}},{Id,_,{Vs2,Ve2}}}|Rest],
	     St0=#st{shapes=Sh0},Acc) -> 
    %% Same We do the internal stiches
    We = #we{vp=Vpos0} = gb_trees:get(Id,Sh0),
    Same = [{Vs1,Vs2},{Ve1,Ve2}],
    Vpos = average_pos(Same, Vpos0),
    St = St0#st{shapes = gb_trees:update(Id, We#we{vp=Vpos}, Sh0)},
    stitch_edges(Rest,St,Acc);
stitch_edges([Other|Rest], St, Acc) ->
    stitch_edges(Rest,St,[Other|Acc]);
stitch_edges([],St,Acc) -> {Acc,St}.

stitch_charts([],_,St0) -> St0;
stitch_charts([ChartStitches|Other],Moved,St0=#st{shapes=Sh0}) ->
    {Id1,Id2,{Vs1,Ve1,Vs2,Ve2}} = find_longest_dist(ChartStitches, St0),
    We1_0 = #we{vp=Vpos1}=gb_trees:get(Id1,Sh0),
    We2_0 = #we{vp=Vpos2}=gb_trees:get(Id2,Sh0),
    Vs1P = array:get(Vs1,Vpos1),Ve1P = array:get(Ve1,Vpos1),
    Vs2P = array:get(Vs2,Vpos2),Ve2P = array:get(Ve2,Vpos2),
    C1 = e3d_vec:average(Vs1P,Ve1P),
    C2 = e3d_vec:average(Vs2P,Ve2P),
    Sh = case {gb_sets:is_member(Id2,Moved),gb_sets:is_member(Id2,Moved)} of
	     {false,_} ->
		 Dist = e3d_vec:sub(C1,C2),
		 Deg = (x_rad(Vs2P,Ve2P) - x_rad(Vs1P,Ve1P)) * 180.0/math:pi(),
		 We2_1 = rotate_chart(-Deg,C2,We2_0),
		 T = e3d_mat:translate(Dist),
		 We2 = wings_we:transform_vs(T, We2_1),
		 gb_trees:update(Id2, We2, Sh0);
	     {_,false} ->
		 Dist = e3d_vec:sub(C2,C1),
		 Deg = (x_rad(Vs1P,Ve1P)-x_rad(Vs2P,Ve2P)) * 180.0/math:pi(),
		 We1_1 = rotate_chart(-Deg,C1,We1_0),
		 T = e3d_mat:translate(Dist),
		 We1 = wings_we:transform_vs(T, We1_1),
		 gb_trees:update(Id1, We1, Sh0);
	     _ ->
		 wings_u:error_msg(?__(1,"Hmm, I can't stitch so many charts at the same time"))
	 end,
    St = foldl(fun stitch_charts2/2, St0#st{shapes=Sh}, ChartStitches),
    stitch_charts(Other, gb_sets:add(Id2,gb_sets:add(Id1,Moved)), St).

stitch_charts2({_E,{Id1,E1,{Vs1,Ve1}},{Id2,E2,{Vs2,Ve2}}}, 
	       St0=#st{shapes=Sh0,sel=Sel}) ->
    We1 = #we{vp=Vpos1} = gb_trees:get(Id1,Sh0),
    We2 = #we{vp=Vpos2} = gb_trees:get(Id2,Sh0),
    Same = [{Vs1,Vs2},{Ve1,Ve2}],
    {Vp1,Vp2} = average_pos(Same, Vpos1, Vpos2),
    Sh1 = gb_trees:update(Id1, We1#we{vp=Vp1}, Sh0),
    Sh  = gb_trees:update(Id2, We2#we{vp=Vp2}, Sh1),
    St0#st{shapes = Sh, sel=add_sel([{Id1,E1},{Id2,E2}],Sel)}.

add_sel([{Id,Edge}|R],Sel) ->
    case lists:keysearch(Id,1,Sel) of
	{value, {Id,Set}} ->
	    add_sel(R, lists:keyreplace(Id, 1, Sel, {Id,gb_sets:add(Edge,Set)}));
	false ->
	    add_sel(R, [{Id,gb_sets:singleton(Edge)}|Sel])
    end;
add_sel([],Sel) -> Sel.

x_rad({X1,Y1,_},{X2,Y2,_}) ->
    Rad =  math:atan2(Y2-Y1,X2-X1),
    if Rad < +0.0 -> 2*math:pi()+Rad;
       true -> Rad
    end.

find_longest_dist([{_,{Id1,_,{Vs1,Ve1}},{Id2,_,{Vs2,Ve2}}}|Rest],#st{shapes=Sh}) ->
    %% Need to find a vector to use as basis to rotate the other chart to
    %% we grab the longest distance between to verts of chart1
    #we{vp=Vpos} = gb_trees:get(Id1,Sh),
    Dist = e3d_vec:dist(array:get(Vs1,Vpos),array:get(Ve1,Vpos)),
    {Id1,Id2,find_longest_dist(Rest,Dist,Vs1,Ve1,Vs2,Ve2,Vpos)}.

find_longest_dist([],_Dist,Bs1,Be1,Bs2,Be2,_Vpos) -> {Bs1,Be1,Bs2,Be2};
find_longest_dist([This|Rest],Dist,Bs1,Be1,Bs2,Be2,Vpos) ->
    {_,{_,_,{Vs1,Ve1}},{_,_,{Vs2,Ve2}}} = This,
    Dist1 = e3d_vec:dist(array:get(Vs1,Vpos),array:get(Be1,Vpos)),
    Dist2 = e3d_vec:dist(array:get(Vs1,Vpos),array:get(Bs1,Vpos)),
    Dist3 = e3d_vec:dist(array:get(Ve1,Vpos),array:get(Be1,Vpos)),
    Dist4 = e3d_vec:dist(array:get(Ve1,Vpos),array:get(Bs1,Vpos)),
    if (Dist1>Dist),(Dist1>Dist2),(Dist1>Dist3),(Dist1>Dist4) ->
	    find_longest_dist(Rest,Dist1,Vs1,Be1,Vs2,Be2,Vpos);
       (Dist2>Dist),(Dist2>Dist3),(Dist2>Dist4) ->
	    find_longest_dist(Rest,Dist2,Vs1,Bs1,Vs2,Bs2,Vpos);
       (Dist3>Dist),(Dist3>Dist4) ->
	    find_longest_dist(Rest,Dist3,Ve1,Be1,Ve2,Be2,Vpos);
       (Dist4>Dist) ->
	    find_longest_dist(Rest,Dist4,Ve1,Bs1,Ve2,Bs2,Vpos);
       true -> 
	    find_longest_dist(Rest,Dist,Bs1,Be1,Bs2,Be2,Vpos)
    end.

cluster_chart_moves([EM={_,{Id1,_,_},{Id2,_,_}}|R], Acc) ->
    {Same,Rest} = cluster_chart_moves2(Id1,Id2,R,[EM],[]),
    cluster_chart_moves(Rest,[Same|Acc]);
cluster_chart_moves([],Acc) -> Acc.

cluster_chart_moves2(Id1,Id2,[EM={_,{Id1,_,_},{Id2,_,_}}|R],Same,Other) ->
    cluster_chart_moves2(Id1,Id2,R,[EM|Same],Other);
cluster_chart_moves2(Id1,Id2,[EM={_,{Id1,_,_},_}|R],Same,Other) ->
    cluster_chart_moves2(Id1,Id2,R,Same,[EM|Other]);
cluster_chart_moves2(_Id1,_Id2,R,Same,Other) ->
    {Same,Other++R}.

average_pos([{V1,V2}|R], Vpos0) ->
    Pos = e3d_vec:average(array:get(V1,Vpos0),array:get(V2,Vpos0)),
    Vpos1 = array:set(V1,Pos,Vpos0),
    Vpos  = array:set(V2,Pos,Vpos1),
    average_pos(R, Vpos);
average_pos([],Vpos) -> Vpos.

average_pos([{V1,V2}|R], Vpos1,Vpos2) ->
    Pos = e3d_vec:average(array:get(V1,Vpos1),array:get(V2,Vpos2)),
    Vp1 = array:set(V1,Pos,Vpos1),
    Vp2 = array:set(V2,Pos,Vpos2),
    average_pos(R, Vp1,Vp2);
average_pos([],Vpos1,Vpos2) -> {Vpos1,Vpos2}.

displace_cuts(SelEs,St=#st{shapes=Sh,bb=#uvstate{id=WeId,st=#st{shapes=GSh}}})->
    Elinks = wings_edge_loop:partition_edges(SelEs,gb_trees:get(WeId,GSh)),
    Remapped = [map_edges(Elink,St) || Elink <- Elinks],
    %% Remapped = [[{GeomEdge, [{AuvWeId,AuvEdge1},{AuvWeId2,AuvEdge2}]},..]
    Displaced = foldl(fun displace_cuts1/2, Sh, Remapped),
    %% Update selection to all new edges
    Sel0 = lists:append([[{Id1,E1},{Id2,E2}] || 
			    {_,{Id1,E1,_},{Id2,E2,_}} 
				<- lists:append(Remapped)]),
    Sel1 = sofs:to_external(sofs:relation_to_family(
			      sofs:relation(Sel0))),
    Sel = [{Id,gb_sets:from_ordset(Eds)} || {Id,Eds} <- Sel1],
    St#st{sel=Sel,shapes=Displaced}.

displace_cuts1(Eds, Sh0) -> 
    Sh1 = displace_edges(Eds,Sh0),
    displace_charts(Eds,gb_sets:empty(),Sh1).

displace_edges([{_,{Id,Edge1,{Vs1,Ve1}},{Id,_,{Vs2,Ve2}}}|Eds], Sh) ->
    We = #we{vp=Vpos0} = gb_trees:get(Id,Sh),
    %% Get the vertices
    Same = [{Vs1,Vs2},{Ve1,Ve2}],
    Vs = [{V1,V2} || {V1,V2} <- Same,
		     V1 /= V2,
		     array:get(V1,Vpos0) == array:get(V2,Vpos0)],
    case Vs of
	[] ->  %% Already displaced
	    displace_edges(Eds,Sh);
	_ ->
	    %% What Direction should we displace the verts?
	    [Move1,Move2] = displace_dirs(?EPSILON,Edge1,We),
	    %% Make the move
	    Vpos = foldl(fun({V1,V2},VpIn) ->
				 Pos1 = e3d_vec:add(Move1,array:get(V1,Vpos0)),
				 Vpos1 = array:set(V1,Pos1,VpIn),
				 Pos2 = e3d_vec:add(Move2,array:get(V2,Vpos0)),
				 array:set(V2,Pos2,Vpos1)
			 end, Vpos0, Vs),	    
	    displace_edges(Eds,gb_trees:update(Id, We#we{vp=Vpos},Sh))
    end;
displace_edges([_Skip|Eds], Sh) ->
    displace_edges(Eds,Sh);
displace_edges([],Sh) -> Sh.

displace_charts([],_,Sh) -> Sh;
displace_charts([{_,{Id,_,_},{Id,_,_}}|Eds],Moved,Sh) ->
    displace_charts(Eds,Moved,Sh);
displace_charts([{_,{Id1,_,_},{Id2,_,_}}|Eds], Moved, Sh) ->
    case gb_sets:is_member(Id1,Moved) or gb_sets:is_member(Id2,Moved) of
	true -> displace_charts(Eds,Moved,Sh);
	false ->
	    We0 = #we{vp=Vpos0} = gb_trees:get(Id1,Sh),
	    C1 = wings_vertex:center(We0),
	    C2 = wings_vertex:center(gb_trees:get(Id2,Sh)),
	    Disp0 = e3d_vec:mul(e3d_vec:norm(e3d_vec:sub(C1,C2)),?EPSILON),
	    Move = case Disp0 of
		       {+0.0,+0.0,_} -> {0.0,?EPSILON,0.0};
		       Disp -> Disp
		   end,
	    Vpos= [{V,e3d_vec:add(Pos,Move)} || 
		      {V,Pos} <- array:sparse_to_orddict(Vpos0)],
	    We = We0#we{vp=array:from_orddict(Vpos)},
	    displace_charts(Eds,gb_sets:add(Id1,Moved),
			    gb_trees:update(Id1,We,Sh))
    end.

displace_dirs(Dist,Edge1,We = #we{es=Etab,vp=Vpos}) ->
    #edge{vs=Vs1,ve=Ve1,lf=LF,rf=RF} = array:get(Edge1,Etab),
    Vp1 = array:get(Vs1,Vpos),
    Vp2 = array:get(Ve1,Vpos),
    {Dx,Dy,_} = e3d_vec:norm(e3d_vec:sub(Vp1,Vp2)),
    Dir = {Dy,-Dx,0.0},
    EdgeFace1 = if LF < 0 -> RF; true -> LF end,
    FaceCenter = wings_face:center(EdgeFace1, We),
    FaceDir = e3d_vec:sub(e3d_vec:average(Vp1,Vp2),FaceCenter),
    Moves = [e3d_vec:mul(Dir,-Dist),e3d_vec:mul(Dir,Dist)],
    case e3d_vec:dot(FaceDir,Dir) > 0.0 of
	true -> Moves;
	false -> reverse(Moves)
    end.

%% Create a mapping from wings_edges to autouv edges.
%% Res is [{GeomEdge, {AuvWeId,AuvEdge1,{E1Vs,E1Ve}},{AuvWeId2,AuvEdge2,..}},..]
map_edges(WingsEs,#st{shapes=Sh}) ->
    AuvEds = edge_sel_to_edge(gb_trees:to_list(Sh),WingsEs,[]),
    %% AuvEds = [{id,auv_eds},..], [{id,auv_eds},..]
    MapEds = fun({Id,Es},A) ->
		     #we{name=#ch{emap=Emap}} = gb_trees:get(Id,Sh),
		     [{auv_segment:map_edge(E,Emap),{Id,E}}
		      || E<-gb_sets:to_list(Es)] ++ A
	     end,
    Mapped0 = foldl(MapEds, [], AuvEds),
    Mapped = sofs:to_external(sofs:relation_to_family(sofs:relation(Mapped0))),
    foldl(
      fun(_L={E,[{Id1,E1},{Id2,E2}]},Acc) ->
	      #we{name=#ch{vmap=Vmap1},es=Etab1}=gb_trees:get(Id1,Sh),
	      #we{name=#ch{vmap=Vmap2},es=Etab2}=gb_trees:get(Id2,Sh),
	      #edge{vs=Vs1,ve=Ve1} = array:get(E1,Etab1),
	      #edge{vs=Vs2,ve=Ve2} = array:get(E2,Etab2),
	      Vs1map = auv_segment:map_vertex(Vs1, Vmap1),
	      R= case auv_segment:map_vertex(Vs2, Vmap2) of
		     Vs1map -> {E,{Id1,E1,{Vs1,Ve1}},{Id2,E2,{Vs2,Ve2}}};
		     _ ->      {E,{Id1,E1,{Vs1,Ve1}},{Id2,E2,{Ve2,Vs2}}}
		 end,
	      [R|Acc];
	 (_,Acc) -> Acc
      end, [], Mapped).
	      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drag_filter({image,_,_}) ->
    {yes,?__(1,"Drop: Change the texture image")};
drag_filter(_) -> no.

handle_drop(#{type:=image,id:=Id}, #st{bb=Uvs0}=St) ->
    Uvs = Uvs0#uvstate{bg_img=Id},
    get_event(St#st{bb=Uvs});
handle_drop(_DropData, _) ->
    ?dbg("Ignore ~P~n",[_DropData,30]),
    keep.

%% is_power_of_two(X) ->
%%     (X band -X ) == X.

%%%
%%% Update charts from new state of Geometry window.
%%%

new_geom_state(GeomSt, AuvSt0) ->
    case update_geom_state(GeomSt, AuvSt0) of
	{AuvSt,true} -> get_event(AuvSt);
	{AuvSt,false} -> get_event_nodraw(AuvSt);
	delete ->
	    cleanup_before_exit(),
	    delete
    end.

update_geom_state(#st{mat=Mat,shapes=Shs}=GeomSt, AuvSt0) ->
    case new_geom_state_0(Shs, Mat, AuvSt0) of
	{AuvSt1,ForceRefresh0} ->
	    {AuvSt,ForceRefresh1} = update_selection(GeomSt, AuvSt1),
	    {AuvSt,ForceRefresh0 or ForceRefresh1};
	Other -> Other  %% delete
    end.

new_geom_state_0(Shs, Mat, #st{bb=#uvstate{matname=none}}=AuvSt) ->
    new_geom_state_1(Shs, AuvSt#st{mat=Mat});
new_geom_state_0(Shs, Mtab0, #st{bb=#uvstate{matname=MatName}=BB, mat=Mtab1}=AuvSt) ->
    case {get_texture(MatName, Mtab0), get_texture(MatName, Mtab1)} of
        {Same,Same} ->
            new_geom_state_1(Shs, AuvSt#st{mat=Mtab0});
        {New, _} when New =/= false ->
            new_geom_state_1(Shs, AuvSt#st{mat=Mtab0, bb=BB#uvstate{bg_img=New}});
        _ ->
            new_geom_state_1(Shs, AuvSt#st{mat=Mtab0})
    end.

new_geom_state_1(Shs, #st{bb=#uvstate{id=Id,st=#st{shapes=Orig}}}=AuvSt) ->
    case {gb_trees:lookup(Id, Shs),gb_trees:lookup(Id, Orig)} of
	{none,_} -> delete;
	{{value,We},{value,We}} -> {AuvSt,false};
	{{value,#we{es=Etab}=We},{value,#we{es=Etab}=OldWe}} ->
	    case wings_va:any_update(We, OldWe) of
		false -> {AuvSt,false};
		true -> {rebuild_charts(We, AuvSt, []),true}
	    end;
	{{value,We},_} -> {rebuild_charts(We, AuvSt, []),true}
    end.

rebuild_charts(We, St = #st{bb=UVS=#uvstate{st=Old,mode=Mode}}, ExtraCuts) ->
    {Faces,FvUvMap} = auv_segment:fv_to_uv_map(Mode,We),
    {Charts0,Cuts0} = auv_segment:uv_to_charts(Faces, FvUvMap, We),
    {Charts1,Cuts} =
	case ExtraCuts of
	    [] -> {Charts0,Cuts0};
	    _ ->
		Cuts1 = gb_sets:union(Cuts0, gb_sets:from_list(ExtraCuts)),
		auv_segment:normalize_charts(Charts0, Cuts1, We)
	end,
    Charts2 = auv_segment:cut_model(Charts1, Cuts, We),
    Charts = update_uv_tab(Charts2, FvUvMap),
    wings_wm:set_prop(wireframed_objects,gb_sets:from_list(gb_trees:keys(Charts))),
    St#st{sel=[],bb=UVS#uvstate{mode=update_mode(Faces,We),st=Old#st{sel=[]}},
	  shapes=Charts}.

update_mode(Faces0, #we{fs=Ftab}) ->
    Fs = gb_sets:from_list(Faces0),
    case gb_sets:size(Fs) == gb_trees:size(Ftab) of 
	true -> object;
	false -> Fs
    end.
	    
update_uv_tab(Cs, FvUvMap) ->
    update_uv_tab_1(Cs, FvUvMap, []).

update_uv_tab_1([#we{id=Id,name=#ch{vmap=Vmap}}=We0|Cs], FvUvMap, Acc) ->
    Fs = wings_we:visible(We0),
    UVs0 = wings_face:fold_faces(
	     fun(F, V, _, _, A) ->
		     OrigV = auv_segment:map_vertex(V, Vmap),
		     [{V,[F|OrigV]}|A]
	     end, [], Fs, We0),
    case update_uv_tab_2(sort(UVs0), FvUvMap, 0.0, []) of
	error ->
	    %% No UV coordinate for at least some vertices (probably
	    %% all) in the chart. Throw away this chart.
	    update_uv_tab_1(Cs, FvUvMap, Acc);
	UVs1 ->
	    UVs = array:from_orddict(UVs1),
	    We = We0#we{vp=UVs},
	    update_uv_tab_1(Cs, FvUvMap, [{Id,We}|Acc])
    end;
update_uv_tab_1([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

update_uv_tab_2([{V,_}|T], FvUvMap, Z, [{V,_}|_]=Acc) ->
    update_uv_tab_2(T, FvUvMap, Z, Acc);
update_uv_tab_2([{V,Key}|T], FvUvMap, Z, Acc) ->
    case gb_trees:get(Key, FvUvMap) of
	{X,Y} ->
	    Pos = {X,Y,Z},
	    update_uv_tab_2(T, FvUvMap, Z, [{V,Pos}|Acc]);
	_ ->
	    %% No UV-coordinate for this vertex. Abandon the entire chart.
	    error
    end;
update_uv_tab_2([], _, _, Acc) -> reverse(Acc).

%% update_selection(GemoSt, AuvSt0) -> AuvSt
%%  Update the selection in the AutoUV window given a selection
%%  from a geometry window.
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{bb=#uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs}=AuvSt) ->
    {AuvSt#st{bb=Uvs#uvstate{st=St}},false};
update_selection(#st{selmode=Mode,sel=Sel}=St0,
		 #st{selmode=AuvMode,bb=#uvstate{id=Id}=Uvs,
		     shapes=Charts0}=AuvSt0) ->
    Charts = gb_trees:to_list(Charts0),
    {case keysearch(Id, 1, Sel) of
	 false ->
	     %% No selection in any chart - clear selection.
	     AuvSt0#st{sel=[],bb=Uvs#uvstate{st=St0}};
	 {value,{Id,Elems0}} when AuvMode == body ->
	     %% Body selection in charts - must be specially handled.
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_body_sel(Mode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}};
	 {value,{Id,Elems0}} when AuvMode =:= Mode->
	     %% Same selection mode in Geometry and AutoUV.
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_selection_1(AuvMode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}};
	 {value,IdElems} ->
	     %% Different selection modes. Convert Geom selection to
	     %% the mode in AutoUV.
	     St = St0#st{sel=[IdElems]},
	     #st{sel=[{Id,Elems0}]} = wings_sel_conv:mode(AuvMode, St),
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_selection_1(AuvMode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}}
     end,true}.

update_selection_1(vertex, Vs, Charts) ->
    vertex_sel_to_vertex(Charts, Vs, []);
update_selection_1(edge, Es, Charts) ->
    edge_sel_to_edge(Charts, Es, []);
update_selection_1(face, Faces, Charts) ->
    face_sel_to_face(Charts, Faces, []).

face_sel_to_face([{K,We}|Cs], Faces, Sel) ->
    ChartFaces = auv2geom_faces(wings_we:visible(We), We),
    case ordsets:intersection(ChartFaces, Faces) of
 	[] ->
	    face_sel_to_face(Cs, Faces, Sel);
 	FaceSel0 ->
	    FaceSel1 = geom2auv_faces(FaceSel0, We),
	    FaceSel = gb_sets:from_list(FaceSel1),
	    face_sel_to_face(Cs, Faces, [{K,FaceSel}|Sel])
    end;
face_sel_to_face([], _, Sel) -> Sel.

vertex_sel_to_vertex([{K,#we{vp=Vtab}=We}|Cs], Vs, Sel) ->
    ChartVs = auv2geom_vs(wings_util:array_keys(Vtab), We),
    case ordsets:intersection(ChartVs, Vs) of
 	[] ->
	    vertex_sel_to_vertex(Cs, Vs, Sel);
 	VertexSel0 ->
	    VertexSel1 = geom2auv_vs(VertexSel0, We),
	    VertexSel = gb_sets:from_list(VertexSel1),
	    vertex_sel_to_vertex(Cs, Vs, [{K,VertexSel}|Sel])
    end;
vertex_sel_to_vertex([], _, Sel) -> Sel.

edge_sel_to_edge([{K,#we{es=Etab}=We}|Cs], Es, Sel) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    ChartEs0 = [E || {E,#edge{lf=Lf,rf=Rf}} <- array:sparse_to_orddict(Etab),
		     gb_sets:is_member(Lf, Vis) orelse
			 gb_sets:is_member(Rf, Vis)],
    ChartEs = auv2geom_edges(ChartEs0, We),
    case ordsets:intersection(ChartEs, Es) of
 	[] ->
	    edge_sel_to_edge(Cs, Es, Sel);
	EdgeSel0 ->
	    EdgeSel1 = geom2auv_edges(EdgeSel0, We),
	    EdgeSel = gb_sets:from_list(EdgeSel1),
	    edge_sel_to_edge(Cs, Es, [{K,EdgeSel}|Sel])
    end;
edge_sel_to_edge([], _, Sel) -> Sel.

%% update_body_sel(SelModeInGeom, Elems, Charts) -> Selection
%%  Convert the selection from the geoemetry window to
%%  a body selection in the AutoUV window.

update_body_sel(face, Elems, Charts) ->
    face_sel_to_body(Charts, Elems, []);
update_body_sel(body, _, Charts) ->
    body_sel_to_body(Charts, []);
update_body_sel(_Mode, _, _) ->
    [].

face_sel_to_body([{K,We}|Cs], Faces, Sel) ->
    Fs = wings_we:visible(We),
    case ordsets:intersection(sort(Fs), Faces) of
 	[] ->
	    face_sel_to_body(Cs, Faces, Sel);
 	_ ->
	    Zero = gb_sets:singleton(0),
	    face_sel_to_body(Cs, Faces, [{K,Zero}|Sel])
    end;
face_sel_to_body([], _, Sel) -> Sel.

body_sel_to_body([{K,_}|Cs], Sel) ->
    body_sel_to_body(Cs, [{K,gb_sets:singleton(0)}|Sel]);
body_sel_to_body([], Sel) -> Sel.
    
%% update_geom_selection(AuvSt)
%%  Given the selection in the AutoUV window, update the selection
%%  in the geometry window.

update_geom_selection(#st{temp_sel={_,_},bb=#uvstate{st=GeomSt}}) ->
    GeomSt;
update_geom_selection(#st{sel=[],bb=#uvstate{st=GeomSt}}) ->
    wpa:sel_set(face, [], GeomSt);
update_geom_selection(#st{selmode=body,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(_, We, A) ->
			       Fs0 = wings_we:visible(We),
			       Fs = auv2geom_faces(Fs0, We),
			       Fs++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=face,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Fs, We, A) ->
			       auv2geom_faces(gb_sets:to_list(Fs), We)++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=edge,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Es0 = wpa:sel_fold(fun(Es, We, A) ->
			       auv2geom_edges(gb_sets:to_list(Es), We)++A
		       end, [], St#st{sel=Sel}),
    Es = gb_sets:from_list(Es0),
    wpa:sel_set(edge, [{Id,Es}], GeomSt);
update_geom_selection(#st{selmode=vertex,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Vs, We, A) ->
			       auv2geom_vs(gb_sets:to_list(Vs), We) ++ A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(vertex, [{Id,Fs}], GeomSt).


%%%% GUI Operations

rotate_chart(Angle, We) ->
    Center = wings_vertex:center(We),
    rotate_chart(Angle, Center, We).

rotate_chart(Angle, Center, We) ->
    Rot0 = e3d_mat:translate(e3d_vec:neg(Center)),
    Rot1 = e3d_mat:mul(e3d_mat:rotate(float(Angle), {0.0,0.0,1.0}), Rot0),
    Rot = e3d_mat:mul(e3d_mat:translate(Center), Rot1),
    wings_we:transform_vs(Rot, We).

align_chart(Dir, St = #st{selmode=Mode}) ->
    wings_sel:map(
      fun(Sel, We = #we{vp=Vtab,es=Etab}) ->
	      case gb_sets:to_list(Sel) of
		  [V1,V2] when Mode == vertex -> 
		      align_chart(Dir,array:get(V1,Vtab),
				  array:get(V2,Vtab),
				  We);
		  [E] when Mode == edge -> 
		      #edge{vs=V1,ve=V2} = array:get(E, Etab),
		      align_chart(Dir,array:get(V1,Vtab),
				  array:get(V2,Vtab),
				  We);
		  _ -> align_error(Mode)
	      end
      end, St).

align_chart(Dir, V1={X1,Y1,_},V2={X2,Y2,_}, We) ->
    Deg0 = 180.0/math:pi() *
	case Dir of
	    align_x -> math:atan2(Y2-Y1,X2-X1);
	    align_y -> math:atan2(X1-X2,Y2-Y1);
	    align_xy -> math:atan2(Y2-Y1,X2-X1) -
			    45/180*math:pi()
	end,
    Deg = if abs(Deg0) < 90.0 -> Deg0;
	     true -> Deg0 + 180
	  end,
    Center = e3d_vec:average(V1,V2),
    rotate_chart(-Deg,Center,We).

-spec align_error(term()) -> no_return().
align_error(vertex) ->
    wings_u:error_msg(?__(1,"Select at least two vertices. One in each chart that must be aligned"));
align_error(edge) ->
    wings_u:error_msg(?__(3,"Select only one edge in each chart that must be aligned"));
align_error(body) ->
    wings_u:error_msg(?__(2,"Select at least two charts to be aligned to each other")).

flip_horizontal(We) ->
    flip(e3d_mat:scale(-1.0, 1.0, 1.0), We).

flip_vertical(We) ->
    flip(e3d_mat:scale(1.0, -1.0, 1.0), We).

flip(Flip, We0) ->
    Center = wings_vertex:center(We0),
    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
    T1 = e3d_mat:mul(Flip, T0),
    T = e3d_mat:mul(e3d_mat:translate(Center), T1),
    We = wings_we:transform_vs(T, We0),
    wings_we:invert_normals(We).

move_to(Dir,We) ->
    [V1={X1,Y1,_},V2={X2,Y2,_}] = wings_vertex:bounding_box(We),
    ChartCenter = {CCX,CCY,CCZ} = e3d_vec:average(V1,V2),
    {OCX,OCY} =
        case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
            true -> {float(trunc(CCX)),float(trunc(CCY))};
            false -> {0.0,0.0}
        end,
    Translate
	= case Dir of
	      center ->   e3d_vec:sub({OCX+0.5,OCY+0.5,CCZ}, ChartCenter);
	      center_x -> e3d_vec:sub({OCX+0.5,CCY,CCZ}, ChartCenter);
	      center_y -> e3d_vec:sub({CCX,OCY+0.5,CCZ}, ChartCenter);
	      bottom ->   {0.0,OCY-Y1,0.0};
	      top ->      {0.0,OCY+1.0-Y2,0.0};
	      left ->     {OCX-X1,0.0,0.0};
	      right ->    {OCX+1.0-X2,0.0,0.0}
	  end,
    T = e3d_mat:translate(Translate),
    wings_we:transform_vs(T, We).

align(Dir,[{Xa,Ya,_},{Xb,Yb,_}],We) ->
    [V1={X1,Y1,_},V2={X2,Y2,_}] = wings_vertex:bounding_box(We),
    ChartCenter = {CCX,CCY,CCZ} = e3d_vec:average(V1,V2),
    Translate =
        case Dir of
            center ->   e3d_vec:sub({(Xa+Xb)/2.0,(Ya+Yb)/2.0,CCZ}, ChartCenter);
            center_x -> e3d_vec:sub({(Xa+Xb)/2.0,CCY,CCZ}, ChartCenter);
            center_y -> e3d_vec:sub({CCX,(Ya+Yb)/2.0,CCZ}, ChartCenter);
            bottom ->   {0.0,(Ya-Y1),0.0};
            top ->      {0.0,(Yb-Y2),0.0};   
            left ->     {(Xa-X1),0.0,0.0};
            right ->    {(Xb-X2),0.0,0.0}
        end,
    T = e3d_mat:translate(Translate),
    wings_we:transform_vs(T, We).

stretch(Dir,We) ->
    [{X1,Y1,_},{X2,Y2,_}] = wings_vertex:bounding_box(We),
    Center = {CX,CY,CZ} = {X1+(X2-X1)/2, Y1+(Y2-Y1)/2, 0.0},
    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
    SX0 = 1.0/wings_util:nonzero(X2-X1), SY0= 1.0/wings_util:nonzero(Y2-Y1),
    {SX,SY} = case Dir of
                max_x -> {SX0, 1.0};
                max_y -> {1.0, SY0};
                max_uniform ->
                    if SX0 < SY0 -> {SX0, SX0};
                    true -> {SY0, SY0}
                    end;
                {max_uniform, Axis} ->
                    case Axis of
                        x -> {SX0, SX0};
                        y -> {SY0, SY0}
                    end
              end,
    Stretch = e3d_mat:scale(SX, SY, 1.0),
    T1 = e3d_mat:mul(Stretch, T0),
    Pos = case Dir of
              {max_uniform,x} -> {CY,CY,CZ};
              {max_uniform,y} -> {CX,CX,CZ};
              max_uniform ->
                  case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
                      true -> {trunc(CX)+0.5,trunc(CY)+0.5,CZ};
                      false -> {0.5,0.5,CZ}
                  end;
              max_x -> {0.5,CY,CZ};
              max_y -> {CX,0.5,CZ}
          end,
    T = e3d_mat:mul(e3d_mat:translate(Pos), T1),
    wings_we:transform_vs(T, We).
    
remap(Method,#st{sel=Sel,selmode=vertex}=St0) ->
    %% Check correct pinning.
    Ch = fun(Vs, #we{vp=Vtab}, _) ->
		 case gb_sets:size(Vs) of
		     N when N /= 2, Method == sphere -> 
			 E = ?__(1,"Select two vertices, the North and South pole"),
			 wpa:error_msg(E);
		     N when N < 2 ->
			 E = ?__(2,"At least two vertices per chart must be pinned"),
			 wpa:error_msg(E);
		     N ->
			 case N < wings_util:array_entries(Vtab) of
			     true -> ok;
			     _ -> 
				 E = ?__(5,"All vertices can not be pinned"),
				 wpa:error_msg(E)
			 end
		 end
	 end,
    wings_sel:fold(Ch, ok, St0),

    %% OK. Go ahead and re-unfold.
    wings_pb:start(?__(3,"remapping")),
    wings_pb:update(0.001),
    N = length(Sel),
    R = fun(Vs, #we{vp=Vtab}=We, I) ->
		Msg = ?__(4,"chart")++" " ++ integer_to_list(I+1),
		wings_pb:update(I/N, Msg),
		Pinned = [begin
			      {S,T,_} = array:get(V, Vtab),
			      {V,{S,T}}
			  end || V <- gb_sets:to_list(Vs)],
		{remap(Method, Pinned, Vs, We, St0),I+1}
	end,
    {St,_} = wings_sel:mapfold(R, 1, St0),
    wings_pb:done(update_selected_uvcoords(St));
remap(Method, #st{sel=Sel}=St0) ->
    wings_pb:start(?__(3,"remapping")),
    wings_pb:update(0.001),
    N = length(Sel),
    Remap = fun(Elems, We, I) ->
		    Msg = ?__(4,"chart")++" " ++ integer_to_list(I+1),
		    wings_pb:update(I/N, Msg),
		    {remap(Method, none, Elems, We, St0),I+1}
	    end,
    {St,_} = wings_sel:mapfold(Remap, 1, St0),
    wings_pb:done(update_selected_uvcoords(St)).

remap(proj_lsqcm, Pinned, Sel, We, St) ->
    remap({proj,lsqcm}, Pinned, Sel, We, St);
remap(proj_slim, Pinned, Sel, We, St) ->
    remap({proj,slim}, Pinned, Sel, We, St);

remap(stretch_opt, _, _, We, St) ->
    Vs3d = orig_pos(We, St),
    ?SLOW(auv_mapping:stretch_opt(We, Vs3d));
remap({proj,Method}, _, Sel, We0, St = #st{selmode=face}) ->
    Vs3d = orig_pos(We0, St),
    try
	Fs0   = gb_sets:to_list(Sel),
	Vs0   = auv_mapping:projectFromChartNormal(Fs0,We0#we{vp=Vs3d}),
	case length(Vs0) == wings_util:array_entries(We0#we.vp) of
	    true -> %% Everything projected nothing to unfold
		update_and_scale_chart(Vs0,We0);
	    false -> %% Unfold rest of faces
		Vtab  = array:from_orddict(Vs0),
		SelVs = wings_vertex:from_faces(Sel,We0),
		Pinned = [begin
			      {S,T,_} = array:get(V, Vtab),
			      {V,{S,T}}
			  end || V <- SelVs],
		remap(Method, Pinned, Sel, We0, St)
	end
    catch throw:{_,What} ->
	    wpa:error_msg(What);
	throw:What ->
	    wpa:error_msg(What)
    end;
remap(Type, Pinned, _, We0, St) ->
    %% Get 3d positions (even for mapped vs).
    Vs3d = orig_pos(We0, St),
    case auv_mapping:map_chart(Type, We0#we{vp=Vs3d}, Pinned) of
	{error,Msg} -> 
	    wpa:error_msg(Msg);
	Vs0 -> 
	    update_and_scale_chart(Vs0,We0)
    end.

%% gb_tree of every vertex orig 3d position, (including the new cut ones)
orig_pos(We = #we{name=#ch{vmap=Vmap}},St) ->
    #st{bb=#uvstate{id=Id,st=#st{shapes=Sh}}} = St,
    #we{vp=Vs3d0} = gb_trees:get(Id, Sh),
    Vs3d = map(fun({V0,_Pos}) ->
		       case gb_trees:lookup(V0, Vmap) of
			   none -> 
			       {V0, array:get(V0, Vs3d0)};
			   {value,V} ->
			       {V0, array:get(V, Vs3d0)}
		       end 
	       end, array:sparse_to_orddict(We#we.vp)),
    array:from_orddict(Vs3d).

update_and_scale_chart(Vs0,We0) ->
    We1 = We0#we{vp=array:from_orddict(sort(Vs0))},
    Fs = wings_we:visible(We1),
    OldA = auv_mapping:fs_area(Fs,We0),
    NewA = auv_mapping:fs_area(Fs,We1),
    NewCenter = wings_vertex:center(We1),
    OldCenter = wings_vertex:center(We0),
    Scale = math:sqrt(OldA/NewA),
    T0 = e3d_mat:translate(e3d_vec:neg(NewCenter)),
    Smat = e3d_mat:scale(Scale),
    T1  = e3d_mat:mul(Smat, T0),
    T  = e3d_mat:mul(e3d_mat:translate(OldCenter),T1),
    wings_we:transform_vs(T, We1).

%%%
%%% Draw routines.
%%%

draw_background(#st{bb=#uvstate{bg_img=Image, tile={U0,V0}, st=#st{shapes=Shs},id=Id}}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Matrices = wings_u:get_matrices(Id, original),
    [{X0,_,_},{X1,_,_}] = obj_to_screen(Matrices, [{0.0,0.0,0.0},{1.0,0.0,0.0}]),
    TileWidth = X1-X0,
    TxSetMode = wings_wm:get_prop(wings_wm:this(), texture_set_mode),
    if (TxSetMode) ->
            Bin = << <<V:?F32,(?TILE_ROWS*1.0):?F32, V:?F32,0.0:?F32, (?TILE_ROWS*1.0):?F32,V:?F32, 0.0:?F32,V:?F32>>
                     || V <- lists:seq(0,?TILE_ROWS) >>;
       true ->
            Bin = << <<V:?F32,20.0:?F32, V:?F32,-20.0:?F32, 20.0:?F32,V:?F32, -20.0:?F32,V:?F32>>
                     || V <- lists:seq(-20,20) >>
    end,
    %% Draw border around the UV space.
    gl:enable(?GL_DEPTH_TEST),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:lineWidth(1.0),
    gl:color3f(0.0, 0.0, 0.7),
    gl:translatef(0.0, 0.0, -0.5),
    wings_vbo:draw(fun(_) -> gl:drawArrays(?GL_LINES, 0, 4*(20+20+1)) end, Bin, [vertex2d]),

    %% Draw border around the current UV tile
    gl:lineWidth(3.0),
    gl:color3f(0.0, 0.0, 1.0),
    gl:recti(U0, V0, U0+1, V0+1),

    %% Draw the background texture.
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),			%Clear
    case TxSetMode of
        true ->
            We = gb_trees:get(Id,Shs),
            case get_textureset_info(We) of
                {?MULTIPLE,[TxSetNaming,[_|_]=TxSet]} ->
                    [draw_texture(init_texture(Tile,Image0)) || {Tile,#{bg_img:=Image0}} <- TxSet],
                    case ?GET({?MODULE,show_texture_set_id}) of
                        true ->
                            Draw =
                                fun({U,V}=Tile) ->
                                    Pos = obj_to_screen(Matrices, {float(U),float(V+1),0.0}),
                                    draw_tile_id(Pos,TileWidth,TxSetNaming,Tile)
                                end,
                            [Draw(Tile) || {Tile,_} <- TxSet];
                        false -> ignore
                    end;
                _ ->
                    draw_texture(init_texture({0,0},Image))
            end;
        false ->
            draw_texture(init_texture({0,0},Image))
    end,
    gl:disable(?GL_TEXTURE_2D),
    gl:popAttrib().

redraw(St) ->
    wings_wm:set_prop(show_info_text, false),
    wings:redraw(St).

draw_tile_id({X,Y,_}, TileWidth, TxSetNaming, Tile) ->
    {_,_,_,H} = wings_wm:viewport(),
    TileStr = txset_suffix(TxSetNaming,Tile),
    InfoWidth = wings_text:width(TileStr)+?CHAR_WIDTH,
    if (InfoWidth < TileWidth) ->
            info(trunc(X),H-trunc(Y),InfoWidth,TileStr);
        true ->
            ok
    end.

%% based on the wings_io:info/3
info(X, Y, InfoWidth, Info) ->
    wings_io:ortho_setup(),
    blend(wings_pref:get_value(info_background_color),
          fun(Color) ->
              wings_io:set_color(Color),
              gl:recti(X+InfoWidth, Y+2, X+3, Y+?CHAR_HEIGHT+3)
          end),
    wings_io:set_color(wings_pref:get_value(info_color)),
    wings_text:render(X+trunc(?CHAR_WIDTH/2), Y+?CHAR_HEIGHT+1, Info).

blend({_,_,_,+0.0}, _) -> ok;
blend({_,_,_,1.0}=Color, Draw) -> Draw(Color);
blend(Color, Draw) ->
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    Draw(Color),
    gl:disable(?GL_BLEND).

obj_to_screen(Matrix, Points) when is_list(Points) ->
    [obj_to_screen(Matrix, Point) || Point <- Points];
obj_to_screen({MVM,PM,VP}, Point) ->
    e3d_transform:project(Point, MVM, PM, VP).

init_texture_area({U,V}, Tiled) ->
    case Tiled of
        true ->
            [{-20.0, -20.0},{-20.0, -20.0, -0.99999}, {20.0, -20.0},{20.0, -20.0, -0.99999},
             {20.0, 20.0},{20.0, 20.0, -0.99999}, {-20.0, 20.0},{-20.0, 20.0, -0.99999}];
        false ->
            [{0.0+U, 0.0+V},{0.0+U, 0.0+V, -0.99999}, {1.0+U, 0.0+V},{1.0+U, 0.0+V, -0.99999},
             {1.0+U, 1.0+V},{1.0+U, 1.0+V, -0.99999}, {0.0+U, 1.0+V},{0.0+U, 1.0+V, -0.99999}]
    end.

init_texture(Tile, Image) ->
    case ?GET({?MODULE,show_background}) of
        false ->
            init_texture_area(Tile,false);
        true ->
            case wings_image:txid(Image) of
                none -> %% Avoid crash if TexImage was deleted
                    init_texture_area(Tile,false);
                Tx ->
                    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
                    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
                    gl:enable(?GL_TEXTURE_2D),
                    gl:bindTexture(?GL_TEXTURE_2D, Tx),
                    init_texture_area(Tile,?GET({?MODULE,tiled_texture}))
            end
    end.

draw_texture(Q) ->
    wings_vbo:draw(fun(_) -> gl:drawArrays(?GL_QUADS, 0, 4) end, Q, [uv, vertex]).

init_drawarea() ->
    {W0,H0} = wings_wm:top_size(),
    W = W0 div 2,
    {{W,75},{W,H0-100}}.

cleanup_before_exit() ->
    %% Ensure the used vbo data will be released. By canceling the Option
    %% dialog doesn't remove it automatically.
    auv_texture:delete_preview_vbo(),
    wings:unregister_postdraw_hook(wings_wm:this(), ?MODULE),
    wings_dl:delete_dlists().

%% Generate a checkerboard image of 4x4 squares 
%% with given side length in pixels.

compressed_bg() ->
<<131,80,0,0,12,5,120,156,181,211,193,106,19,81,20,6,224,60,128,111,225,131,248,
 12,130,224,170,43,209,141,138,96,187,201,74,208,165,136,139,212,214,82,187,105,
 153,133,40,184,42,177,138,132,80,170,196,90,104,67,179,154,42,169,138,88,81,132,
 136,1,67,146,241,159,254,230,207,157,51,167,205,184,72,249,25,78,134,225,59,247,
 158,123,123,189,84,58,83,186,176,116,197,205,167,185,190,155,229,155,127,86,230,
 250,249,36,107,103,221,80,123,253,254,221,73,254,239,221,33,243,125,117,144,250,
 179,61,104,31,118,135,100,85,140,205,246,122,154,172,159,36,137,235,147,69,240,
 1,106,249,248,73,252,213,234,32,227,67,174,95,75,51,106,33,31,91,8,119,65,31,
 239,213,133,181,124,224,236,146,241,33,171,75,214,191,247,114,9,81,139,208,231,
 100,140,143,149,35,254,124,70,120,126,62,42,66,63,172,195,249,100,10,131,159,48,
 31,108,33,244,177,114,115,190,139,55,126,105,62,206,252,57,249,220,124,138,223,
 79,247,114,158,114,63,239,60,232,187,169,156,187,236,230,197,252,140,155,100,
 230,188,27,106,173,120,40,153,53,181,184,182,173,132,254,215,184,33,153,245,216,
 108,108,165,201,250,152,167,240,167,207,7,242,241,190,122,235,33,19,250,120,63,
 94,249,113,61,198,43,119,211,140,90,200,135,44,60,244,221,249,208,252,183,242,
 208,199,95,182,144,15,89,187,40,232,103,158,147,124,60,209,66,7,49,209,199,226,
 247,170,21,60,139,251,97,139,137,62,112,20,120,102,124,78,158,167,16,248,211,
 187,159,157,242,186,155,103,107,143,221,204,239,189,117,115,123,225,146,27,106,
 189,230,23,197,248,159,219,31,17,227,199,63,127,152,66,96,235,96,27,49,62,102,
 216,141,118,152,208,135,220,168,111,33,106,65,13,223,155,66,248,147,141,69,68,
 45,228,187,243,193,123,118,97,49,209,135,204,2,239,141,31,14,199,248,220,130,
 241,49,25,196,248,74,222,15,135,99,124,110,193,248,213,118,140,20,247,221,243,
 229,228,121,10,69,230,163,243,53,254,244,238,103,173,118,213,205,197,104,193,77,
 167,94,118,83,142,58,110,168,29,29,237,152,130,218,155,195,3,38,239,247,190,53,
 17,227,55,15,123,140,241,49,46,83,80,195,207,251,155,27,136,90,8,239,182,34,68,
 45,168,225,251,104,179,139,168,133,88,172,28,201,251,166,160,150,254,191,28,23,
 104,97,124,83,200,223,223,127,132,228,125,206,7,91,112,125,51,159,83,252,255,
 154,143,252,252,124,92,31,43,231,124,80,20,89,63,39,207,83,48,62,207,23,71,16,
 250,211,187,159,127,1,245,246,60,42>>.

bg_image() ->
    Orig = binary_to_term(compressed_bg()),
    Pixels = repeat_image(Orig, []),
    Width = Height = 256,
    #e3d_image{width=Width,height=Height,image=Pixels,
	       order=lower_left,name="auvBG"}.
    
repeat_image(<<Row:(32*3)/binary,Rest/binary>>, Acc) ->
    repeat_image(Rest,[Row,Row,Row,Row,Row,Row,Row,Row|Acc]);
repeat_image(<<>>,Acc) -> 
    Im = lists:reverse(Acc),
    list_to_binary([Im,Im,Im,Im,Im,Im,Im,Im]).

%%%
%%% Conversion routines.
%%%

auv2geom_faces(Fs, _) ->
    Fs.
geom2auv_faces(Fs, _) ->
    Fs.

auv2geom_vs(Vs, #we{name=#ch{vmap=Vmap}}) ->
    sort([auv_segment:map_vertex(V, Vmap) || V <- Vs]).

geom2auv_vs(Vs, #we{name=#ch{vmap=Vmap},vp=Vtab}) ->
    geom2auv_vs_1(wings_util:array_keys(Vtab), gb_sets:from_list(Vs), Vmap, []).

geom2auv_vs_1([V|Vs], VsSet, Vmap, Acc) ->
    case gb_sets:is_member(auv_segment:map_vertex(V, Vmap), VsSet) of
	true -> geom2auv_vs_1(Vs, VsSet, Vmap, [V|Acc]);
	false -> geom2auv_vs_1(Vs, VsSet, Vmap, Acc)
    end;
geom2auv_vs_1([], _, _, Acc) -> sort(Acc).

auv2geom_edges(Es, #we{name=#ch{emap=Emap}}) ->
    sort([auv_segment:map_edge(E, Emap) || E <- Es]).

geom2auv_edges(Es, #we{name=#ch{emap=Emap0}}) ->
    A2We = sofs:relation(gb_trees:to_list(Emap0)),
    W2Ae = sofs:relation_to_family(sofs:converse(A2We)),
    Tab = gb_trees:from_orddict(sofs:to_external(W2Ae)),
    foldl(fun(Edge, Acc) ->
		  case gb_trees:lookup(Edge, Tab) of
		      none -> [Edge|Acc];
		      {value,Hits} -> Hits ++ Acc
		  end
	  end, [], Es).

pick_uv_tile(X0, Y0, #st{bb=#uvstate{id=Id,st=#st{shapes=Shs}}=Uvs}=St) ->
    We = gb_trees:get(Id,Shs),
    case get_textureset_info(We) of
        {?MULTIPLE,[_,[_|_]=TxSet]} ->
            {_,H} = wings_wm:win_size(),
            X = float(X0),
            Y = H-float(Y0),
            Matrices = wings_u:get_matrices(0, original),
            {U0,V0,_} = screen_to_obj(Matrices, {X,Y,0.0}),
            U = if U0 < +0.0 -> U0-1.0; true-> U0 end,
            V = if V0 < +0.0 -> V0-1.0; true-> V0 end,
            Tile = {trunc(U),trunc(V)},
            case [MapInfo || {Tile0,MapInfo} <- TxSet, Tile0==Tile] of
                [] ->
                    St;
                [#{mat:=MatName,bg_img:=TxId}] ->
                    St#st{bb=Uvs#uvstate{tile=Tile,matname=MatName,bg_img=TxId}}
            end;
        _ ->
            St
    end.

remap_uv_tile(#st{bb=Uvs,shapes=Charts0}=St) ->
    #uvstate{tile=Tile,id=Id,st=#st{shapes=Shs0}} = Uvs,
    We = gb_trees:get(Id,Shs0),
    case get_textureset_info(We) of
        {?MULTIPLE,[_,[_|_]=TxSet]} ->
            case [MapInfo || {Tile0,MapInfo} <- TxSet, Tile0==Tile] of
                [] ->
                    St;
                [#{mat:=MatName}] ->
                    Charts = remap_uv_tile_0(Tile,MatName,gb_trees:values(Charts0),gb_trees:empty()),
                    St#st{shapes=Charts}
            end;
        _ ->
            St
    end.


remap_uv_tile_0(_, _, [], Acc) -> Acc;
remap_uv_tile_0({0,0}=Tile, MatName, [#we{id=Id}=Chart|Charts], Acc0) ->
    Acc = gb_trees:enter(Id,Chart,Acc0),
    remap_uv_tile_0(Tile,MatName,Charts,Acc);
remap_uv_tile_0(Tile, MatName, [#we{id=Id,mat=MatName}=Chart0|Charts], Acc0) ->
    Chart = remap_uv_tile_1(Tile, Chart0),
    Acc = gb_trees:enter(Id,Chart,Acc0),
    remap_uv_tile_0(Tile,MatName,Charts,Acc);
remap_uv_tile_0(Tile, MatName, [#we{id=Id}=Chart0|Charts], Acc0) ->
    FsMat = wings_facemat:all(Chart0),
    Acc =
        case [F || {F,Mat} <- FsMat, Mat==MatName] of
        [] ->
            gb_trees:enter(Id,Chart0,Acc0);
        _ ->
            Chart = remap_uv_tile_1(Tile, Chart0),
            gb_trees:enter(Id,Chart,Acc0)
        end,
    remap_uv_tile_0(Tile,MatName,Charts,Acc).
remap_uv_tile_1({U,V}, Chart) ->
    Transform = e3d_mat:translate(float(-U),float(-V),0.0),
    wings_we:transform_vs(Transform, Chart).

screen_to_obj({MVM,PM,VP}, Point) ->
    e3d_transform:unproject(Point, MVM, PM, VP).

camera_reset() ->
    View = wings_view:current(),
    {X,Y,Dist} =
        case wings_wm:get_prop(wings_wm:this(), texture_set_mode) of
            true -> {(?TILE_ROWS*-0.5),(?TILE_ROWS*-0.5),?TILE_ROWS*0.6};
            false -> {-0.5,-0.5,?CAMERA_DIST*0.08}
        end,
    wings_view:set_current(View#view{origin={X,Y,0.0},
                                     azimuth=0.0,elevation=0.0,
                                     distance=Dist,
                                     pan_x=0.0,pan_y=0.0,
                                     along_axis=none}).
