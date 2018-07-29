%%
%%  auv_seg_ui.erl --
%%
%%     Segmentation UI for AutoUV.
%%
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(auv_seg_ui).
-export([start/4]).

-define(NEED_ESDL, 1).
-include_lib("wings/src/wings.hrl").
-include("auv.hrl").

-import(lists, [sort/1,map/2,member/2,foldl/3,reverse/1]).

%%%
%%% Segmentation interface.
%%%

-record(seg, {st,				%Current St.
	      selmodes,				%Legal selection modes.
	      we,				%Original We.
	      orig_st,				%Original St.
	      fs,                               %Original Selected faces or object
	      err,                              %Last Error message
	      msg				%Message.
	     }).

start(Op, #we{id=Id}=We0, OrigWe, St0) ->
    Modes = [vertex,edge,face],
    wings:mode_restriction(Modes),
    wings_pref:set_value(scene_lights, false), %% ugly hack 
    
    St1 = seg_create_materials(St0),
    {Fs,We1} = seg_hide_other(Id,We0,St0),
    We = case Op of 
	     keep_old -> 
		 {Faces,FvUvMap} = auv_segment:fv_to_uv_map(Fs,We1),
		 {Charts,Cuts0} = auv_segment:uv_to_charts(Faces,FvUvMap,We1),
		 Cuts = 
		     foldl(fun(Chart,Acc) ->
				   Es = wings_face:outer_edges(Chart,We1),
				   gb_sets:union(gb_sets:from_list(Es),Acc)
			   end, Cuts0, Charts),
		 We1#we{he=Cuts};
	     delete_old ->
		 We1
	 end,

    St = St1#st{sel=[],selmode=face,shapes=gb_trees:from_orddict([{Id,We}])},
    Ss = seg_init_message(#seg{selmodes=Modes,st=St,orig_st=St0,we=OrigWe,fs=Fs}),
    
    %% Don't push here - instead replace the default crash handler
    %% which is the only item on the stack.
    get_seg_event(Ss).

seg_init_message(Ss) ->
    Msg1 = wings_msg:button_format(?__(1,"Select")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(2,"Show menu")),
    Msg  = wings_msg:join([Msg1,Msg2,Msg3]),
    Ss#seg{msg=Msg}.

get_seg_event(#seg{st=St}=Ss) ->
    wings_wm:current_state(St),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    get_seg_event_noredraw(Ss).

get_seg_event_noredraw(Ss) ->
    {replace,fun(Ev) -> seg_event(Ev, Ss) end}.

seg_event({crash,Crash}, _) ->
    wings_u:win_crash(Crash),
    delete;
seg_event(init_opengl, #seg{st=St}=Ss) ->
    wings:init_opengl(St),
    get_seg_event(Ss);
seg_event(redraw, #seg{st=St,msg=Msg}) ->
    wings_wm:message(Msg, ?__(1,"Segmenting")),
    wings:redraw(St),
    keep;
seg_event({add_faces,_,_},#seg{fs=object}) ->
    keep;
seg_event({add_faces,Mode0,_St},#seg{fs=Fs,st=St0}=Ss) ->
    #st{shapes=Shs0} = St0,
    [We0 = #we{id=Id}] = gb_trees:values(Shs0),
    {Mode,We} = case Mode0 of
		    object -> {object,wings_we:show_faces(We0)};
		    NewFs -> 
			Vis = gb_sets:union(NewFs,Fs),
			We1 = wings_we:show_faces(We0),
			Other = wings_sel:inverse_items(face, Vis, We1),
			We2 = wings_we:hide_faces(Other, We1),
			{Vis,We2}
		end,
    St = St0#st{shapes=gb_trees:update(Id,We,Shs0)},
    get_seg_event(Ss#seg{st=St,fs=Mode});
seg_event(close, _) ->
    seg_cancel();
seg_event(Ev, #seg{st=St}=Ss) ->
    case wings_camera:event(Ev, St) of
	next -> seg_event_2(Ev, Ss);
	Other -> Other
    end.

seg_event_2(Ev, #seg{st=St}=Ss) ->
    case wings_pick:event(Ev, St, fun() -> wings:redraw(St) end) of
	next -> seg_event_3(Ev, Ss);
	Other -> Other
    end.

seg_event_3(Ev, #seg{st=#st{selmode=Mode}}=Ss) ->
    case wings_menu:is_popup_event(Ev) of
	no -> seg_event_4(Ev, Ss);
	{yes,X,Y,_} -> 
	    Mappers = mappers(), 
	    Menu = [{?__(1,"Continue"),{continue, Mappers}},
		    separator,
		    {?__(2,"Segment by"),
		     {segment,
		      [{?__(3,"Projection"),autouvmap},
		       {?__(4,"Feature Detection"),feature}]}}|
		    seg_mode_menu(Mode, Ss, seg_debug([]))],
	    wings_menu:popup_menu(X, Y, auv_segmentation, Menu)
    end.

-ifndef(DEBUG).
seg_debug(Tail) -> Tail.
mappers() ->
    [{?__(1,"Unfolding"),lsqcm},				  
     {?__(2,"Projection Normal"),project},
     {?__(3,"Projection Camera"),camera},
     {?__(4,"Sphere Map"),sphere}
    ,{?__(5,"Cylindrical Map"),cyl}
    ].
-else.
seg_debug(Tail) ->
    [separator,
     {?__(1,"Debugging"),
      {debug,
       [{?__(2,"Select features"),select_features},
	{?__(3,"Select seeds"),select_seeds},
        {?__(4,"Select Pinned vertices"), select_pinned}]}}|Tail].
mappers() ->
    [{?__(1,"Unfolding"),lsqcm}, 
     {?__(2,"Two pass Unfolding"),lsqcm2},
     {?__(3,"Projection"),project}].
-endif.

seg_mode_menu(vertex, _, Tail) -> Tail;
seg_mode_menu(edge, _, Tail) ->
    [separator,
     {?__(1,"Mark Edges for Cut"),cut_edges},
     {?__(2,"Unmark Edges"),no_cut_edges},
     separator,
     {?__(3,"Select Marked Edges"),select_hard_edges}|Tail];
seg_mode_menu(face, _, Tail) ->
    Menu0 = map(fun({Name,Color}) ->
			{atom_to_list(Name),Name,[],[{color,Color}]};
		   (Other) -> Other
		end, auv_util:seg_materials()),
    Menu = Menu0 ++
	[{?__(4,"Ignore Faces"), ignore_faces},
	 separator,
	 {?__(5,"Select"),{select,Menu0}}|Tail],
    [separator|Menu].

seg_event_4(Ev, Ss) ->
    case translate_key(Ev) of
	next -> seg_event_5(Ev, Ss);
	Other -> Other
    end.

seg_event_5(Ev, #seg{st=St0}=Ss) ->
    case wings_hotkey:event(Ev, St0) of
	next -> seg_event_6(Ev, Ss);
	Action ->
	    wings_wm:later({action,Action}),
	    keep
    end.

seg_event_6({new_state,St}, Ss) ->
    get_seg_event(Ss#seg{st=St});

seg_event_6({action,{view,Cmd}}, #seg{st=St0}=Ss) ->
    case Cmd of
    aim -> 
        St1 = fake_selection(St0),
        wings_view:command(aim, St1),
        get_seg_event(Ss);
    highlight_aim ->
        #st{sel=Sel} = St0,
        case  Sel =:= [] of
          true ->
            St1 = fake_selection(St0),
            wings_view:command(aim, St1),
            get_seg_event(Ss);
          false ->
            {{_,Cmd0},St1} = wings:highlight_aim_setup(St0),
            wings_view:command(Cmd0,St1),
            get_seg_event(Ss)
        end;
    _Cmd ->
        case wings_view:command(Cmd, St0) of
            #st{}=St -> get_seg_event(Ss#seg{st=St});
            Other -> Other
        end
    end;

seg_event_6({action,{select,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_sel_cmd:command(Cmd, St0) of
	St0 ->     keep;
	{save_state,St} ->  filter_sel_command(Ss, St);
	#st{}=St ->  	    filter_sel_command(Ss, St);
	Other -> 	    Other
    end;
seg_event_6({action,{window,geom_viewer}}, _) ->
    keep;
seg_event_6({action,{window,Cmd}}, #seg{st=St0}=Ss) ->
    case wings:command({window,Cmd}, St0) of
	St0 -> keep;
	#st{}=St -> get_seg_event(Ss#seg{st=St});
	Other -> Other
    end;
seg_event_6({action,{material,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_material:command(Cmd, St0) of
	St0 -> keep;
	{save_state,St} -> seg_event({new_state,St}, Ss);
	#st{}=St -> seg_event({new_state,St}, Ss);
	Other -> Other
    end;
seg_event_6({action,{auv_segmentation,Cmd}}, Ss) ->
    seg_command(Cmd, Ss);
seg_event_6({callback, Fun}, _) when is_function(Fun, 0) ->
    Fun();
seg_event_6({message,Message}, _) ->
    wings_u:message(Message);
seg_event_6(#mousemotion{}, _) -> keep;
seg_event_6(#mousebutton{}, _) -> keep;
seg_event_6(#keyboard{}, _) -> keep;
seg_event_6(_Ev, _) ->
    keep.

translate_key(#keyboard{sym=27}) ->
    seg_cancel();
translate_key(_) -> next.

filter_sel_command(#seg{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	false -> keep;
	true -> seg_event({new_state,St}, Ss)
    end.

seg_command({continue,Method}, Ss) ->
    seg_map_charts(Method, Ss);
seg_command(cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge_cmd:hardness(hard, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(no_cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge_cmd:hardness(soft, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(select_hard_edges, #seg{st=St0}=Ss) ->
    case wings_sel_cmd:command({by,hard_edges}, St0) of
	{save_state,St} -> get_seg_event(Ss#seg{st=St});
	#st{}=St -> get_seg_event(Ss#seg{st=St})
    end;
seg_command({select,Mat}, #seg{st=St0}=Ss) ->
    {save_state,St} = wings_material:command({select,[atom_to_list(Mat),select]}, St0),
    get_seg_event(Ss#seg{st=St});
seg_command({segment,Type}, #seg{st=St0}=Ss) ->
    St = segment(Type, St0),
    get_seg_event(Ss#seg{st=St});
seg_command({debug,Cmd}, Ss) ->
    seg_command_debug({debug,Cmd}, Ss);
seg_command(ignore_faces,#seg{st=St0,fs=Mode0}=Ss) ->    
    HiddenFs = wpa:sel_fold(fun(Fs,_,Acc) -> gb_sets:union(Fs,Acc) end,
			    gb_sets:empty(), St0),
    #st{shapes=Shs0} = St0,
    [We0 = #we{id=Id}] = gb_trees:values(Shs0),
    {Mode,We} = case Mode0 of
		    object -> 
			Vis = wings_sel:inverse_items(face, HiddenFs, We0),
			{Vis,wings_we:hide_faces(HiddenFs, We0)};
		    ShownFs -> 
			Vis = gb_sets:subtract(ShownFs,HiddenFs),
			{Vis,wings_we:hide_faces(HiddenFs, We0)}
		end,
    St = St0#st{shapes=gb_trees:update(Id,We,Shs0),sel=[]},
    get_seg_event(Ss#seg{st=St,fs=Mode});
seg_command(Cmd, #seg{st=#st{mat=Mat}=St0}=Ss) ->
    case gb_trees:is_defined(Cmd, Mat) of
	false ->
	    keep;
	true ->
	    St = wings_material:command({assign,atom_to_list(Cmd)}, St0),
	    get_seg_event(Ss#seg{st=St})
    end.

-ifndef(DEBUG).
seg_command_debug(_, _) -> keep.
-else.
seg_command_debug({debug,select_features}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = array:sparse_size(We#we.es),
    {Es,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    Sel = [{Id,gb_sets:from_list(Es)}],
    get_seg_event(Ss#seg{st=St#st{selmode=edge,sel=Sel}});
seg_command_debug({debug,select_seeds}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = array:sparse_size(We#we.es),
    {Features,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    {Seeds0,_} = auv_segment:build_seeds(Features, We),
    Seeds = [S || {_,S} <- Seeds0],
    Sel = [{Id,gb_sets:from_list(Seeds)}],
    get_seg_event(Ss#seg{st=St#st{selmode=face,sel=Sel}});
seg_command_debug({debug,select_pinned}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    [{Id,SetOfFaces}] = St#st.sel,
    case {St#st.selmode == face, gb_sets:to_list(SetOfFaces)} of
	{true,Fs} when Fs /= [] ->
	    {{V1,_UV1},{V2,_UV2}} = auv_mapping:find_pinned(Fs, We),
	    ?DBG("Pinned ~p ~n", [{{V1,_UV1},{V2,_UV2}}]),
	    Sel = [{Id,gb_sets:from_list([V1,V2])}],
	    get_seg_event(Ss#seg{st=St#st{selmode=vertex,sel=Sel}});
	_ -> 
	    ?DBG("Not in face mode~n", []),
	    keep
    end.
-endif.

seg_cancel() ->
    wings_dl:delete_dlists(),
    delete.

seg_create_materials(St0) ->
    M0 = auv_util:seg_materials(),
    M = [{Name,auv_util:make_mat(Diff)} || {Name,Diff} <- M0],
    {St,[]} = wings_material:add_materials(M, St0),
    St.

seg_hide_other(Id, We0, #st{selmode=face,sel=[{Id,Faces}]}) ->
    Other = wings_sel:inverse_items(face, Faces, We0),
    We = wings_we:hide_faces(Other, We0),
    {Faces,We};
seg_hide_other(_,We,_) -> {object,We}.

seg_map_charts(Method, #seg{st=#st{shapes=Shs},we=OrigWe}=Ss) ->
    wings_pb:start(?__(1,"preparing mapping")),
    [#we{he=Cuts0}=We] = gb_trees:values(Shs),
    wings_pb:update(0.12, ?__(2,"segmenting")),
    Charts0 = auv_segment:segment_by_material(We),
    wings_pb:update(0.35, ?__(3,"normalizing")),
    {Charts1,Cuts} = auv_segment:normalize_charts(Charts0, Cuts0, We),
    wings_pb:update(1.0, ?__(4,"cutting")),
    Charts = auv_segment:cut_model(Charts1, Cuts, OrigWe),
    wings_pb:done(),
    case length(Charts) of
	0 ->
	    wings_u:error_msg(?__(5,"No mappable faces."));
	N ->
	    wings_pb:start(?__(6,"mapping")),
	    seg_map_charts_1(Charts, Method, 1, N, [], [], Ss)
    end.

seg_map_charts_1([We0|Cs], Type, Id, N, Acc,Failed,Ss) ->
    wings_pb:update(Id/N, lists:flatten(io_lib:format(?__(1,"chart")++" ~w/~w", [Id,N]))),
    We1 = We0#we{id=Id},
    case auv_mapping:map_chart(Type, We1, camera_dir(Type)) of
	{error,Message} ->
	    seg_map_charts_1(Cs,Type,Id+1,N,Acc,[We1|Failed],Ss#seg{err=Message});
	Vs0 ->
	    Vs = auv_placement:rotate_area(Vs0,We1),
	    We = We1#we{vp=array:from_orddict(sort(Vs))},
	    seg_map_charts_1(Cs, Type, Id+1, N, [We|Acc], Failed, Ss)
    end;
seg_map_charts_1([],_, _, _,Charts0,Failed,
		 Ss = #seg{orig_st=GeomSt0,fs=Fs,st=St0,we=#we{id=Id}}) ->
    wings_pb:done(),
    wings_dl:delete_dlists(),
    if Charts0 == [] -> 
	    wings_u:message(Ss#seg.err),
	    get_seg_event(seg_init_message(Ss));
       true ->
	    Charts = reverse(Charts0),
	    We = gb_trees:get(Id, GeomSt0#st.shapes),
	    GeomSt = wpc_autouv:init_show_maps(Charts, Fs, We, GeomSt0),
	    case Failed of 
		[] -> 
		    cleanup_before_exit(),
		    delete;
		_ ->
		    wings_u:message(Ss#seg.err),
		    #st{shapes=Shs} = St0,
		    [We0] = gb_trees:values(Shs),
		    Keep = foldl(fun(#we{fs=Ftab},Acc) -> 
					 New = gb_sets:from_ordset(gb_trees:keys(Ftab)),
					 gb_sets:union(New,Acc)
				 end, gb_sets:empty(), Failed),
		    Other = wings_sel:inverse_items(face, Keep, We0),
		    WeKeep = wings_we:hide_faces(Other, We0),
		    St = wings_sel:clear(St0#st{shapes=gb_trees:update(Id,WeKeep,Shs)}),
		    get_seg_event(seg_init_message(Ss#seg{st=St,fs=Keep,orig_st=GeomSt}))
	    end
    end.

segment(Mode, #st{shapes=Shs}=St) ->
    [We] = gb_trees:values(Shs),
    {Charts,Cuts} = auv_segment:create(Mode, We),
    auv_util:mark_segments(Charts, Cuts, We, St).

camera_dir(camera) ->
    Matrices = wings_u:get_matrices(0, original),
    {matrices, Matrices};
camera_dir(_) -> none.

cleanup_before_exit() ->
    wings_dl:delete_dlists().

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
