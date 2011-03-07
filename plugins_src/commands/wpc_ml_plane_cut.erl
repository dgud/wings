

-module(wpc_ml_plane_cut).
-export([init/0, menu/2, command/2]).
-import(lists, [foldl/3,usort/1,reverse/1]).
-include("wings.hrl").


-define(NON_ZERO, 1.0E-7).

init() ->
    true.
    
    
    
    
    
    
%% Copy these here in case submitted to Offical Devs for consideration. 
%% As this is a good plugin.
manifoldlabRootItem() ->
    { "Lots More...", { manifoldlab, [ ] } , "Lots More..." }.


parseMenuInsert(Menu, NewItem, ItemInto) ->
    
       { ItemIntoName, { ItemIntoAtom, _ItemIntoList }, ItemIntoHelp } = ItemInto,
       
       case lists:keymember(ItemIntoName, 1, Menu) of
           false ->  %% If ManifoldLab Menu not started yet. Get Crackin
               [ { ItemIntoName, { ItemIntoAtom, [ NewItem ] }, ItemIntoHelp } | Menu ];
           
           true ->  %% Insert Into it.
               { value, { _ItemIntoName, { _ItemIntoAtom, ItemIntoList }, _ItemIntoHelp } } = lists:keysearch(ItemIntoName,1, Menu),
               lists:keyreplace(ItemIntoName, 1, Menu, { ItemIntoName, { ItemIntoAtom, [ NewItem | ItemIntoList ]}, ItemIntoHelp })
       end.      
    
    
    
which_side_fun() ->
    fun(1, _Ns) ->
	    	{ body, { manifoldlab, hull_cuts_inside }  };
	   (3, _Ns) ->
	    	{ body, { manifoldlab, hull_cuts_outside }  };
	   (2, _Ns) -> 
	   		{ body, { manifoldlab, hull_cuts_all }  }
    end.   
    
    
    
    
menu({body}, Menu) ->
   SubMenu =[
   	{  "Cut X=Z",  ml_cut_xz,      "" },   
    { "Cut X=-Z", ml_cut_mxz,    "" }, 
    { "Cut X=0",  ml_cut_xzero,  "" },  
    { "Cut Z=0",  ml_cut_zzero,  "" },  
    { "Cut 8-Cut",  ml_cut_8,  "" }
   ],
         
   Menu2 = parseMenuInsert(Menu, { "Unauth Plane Cuts ...", { optigon_cuts,  SubMenu }, "Unauth Plane Cuts ..."} , manifoldlabRootItem()),
   parseMenuInsert(Menu2,  { "Polyhedra as Cutter ..." , which_side_fun(),   { "Keep Inside Bodies",  "Keep All" , "Keep Outside Bodies" }, [ ] } , 
   		manifoldlabRootItem());
       

menu({select }, Menu) ->

	MyAcc = fun( Item, AccMode ) ->
		case Item of 
			{ "All Faces", _ , _ } -> face;
			{ "All Edges", _ , _  } -> edge;
			{ "All Vertices", _, _  } -> vertex;
			_ ->
				AccMode
		end
	end,
	
	SelMode = lists:foldl(MyAcc, body, Menu),

   case SelMode of 
   		face  ->
   			parseMenuInsert(Menu, { "Face Partioned Objects", optigon_hull_partioned, "Face Partioned Objects"} , manifoldlabRootItem());
   		_ ->
   			Menu
   end;

   
              
menu(_,Menu) -> 
    Menu.
    
    
deleteWe(#we{id=ID}, #st{sel=Sel, shapes=Shapes}=St) ->
	Shapes2 = gb_trees:delete_any(ID, Shapes),
	Sel2 = lists:keydelete(ID, 1, Sel),
	St#st{shapes=Shapes2, sel=Sel2}.
	
	
	
pointInsideHull({ X, Y, Z } , #we{fs=Ftab,vp=Vtab}=We) ->
	 { CX, CY, CZ } = wings_vertex:center(We),
	 
	 %% Any outside ?
	 MyAny = fun( Fi ) ->
	 	Points = [  array:get(VIndex,Vtab)    || VIndex <- wings_face:vertices_ccw(Fi, We) ],
		[ P1, P2, P3 | _T ] = Points,
		{ A, B, C, D }  = plane_coefficients(P1, P2, P3),
		Sign1 = signPointToPlane( {CX, CY, CZ}, { A, B, C, D} ),
		Sign2 = signPointToPlane( {X, Y, Z}, { A, B, C, D} ),
		Sign1 /= Sign2
	end,
	
	lists:any(MyAny, gb_trees:keys(Ftab)).
	
    
    
command({select, { manifoldlab, optigon_hull_partioned }}  , #st{selmode=face, shapes=Shapes, sel=[{WeID, Set}] }=St ) ->
	case gb_sets:size(Set)== 1 of 
		false ->
			wings_u:error_msg("Please ... only 1 face to partion objects.");
		true ->
			ok
	end,
		
	We = gb_trees:get(WeID, Shapes),
	#we{vp=Vtab}=We,
    [ Fi ] = gb_sets:to_list(Set),
    
    N = wings_face:normal(Fi,We),
	{ CXf, CYf, CZf } = wings_face:center(Fi, We),
	St2 = plane_cut_mlab( N ,  { CXf, CYf, CZf }  , St),
	
	
	Points = [  array:get(VIndex,Vtab)    || VIndex <- wings_face:vertices_ccw(Fi, We) ],
	[ P1, P2, P3 | _T ] = Points,
	{ A, B, C, D }  = plane_coefficients(P1, P2, P3),
	
	{ CX, CY, CZ } = wings_vertex:center(We),
    Sign1 = signPointToPlane( {CX, CY, CZ}, { A, B, C, D} ),
		
	
	SelFun = fun( _,  _We ) ->
		{ _CX, _CY, _CZ } = wings_vertex:center(_We),
		Sign2 = signPointToPlane( { _CX, _CY, _CZ }, { A, B, C, D} ),
		Sign1 /= Sign2
	end,
	
	wings_sel:make(SelFun, body, St2#st{ selmode=face, sel=[ ] } );
	
	
command({face, { manifoldlab, optigon_hull_partioned }}  , _St ) ->
	wings_u:error_msg("Please select 1 face on one object."),
	_St;
	
%%--------------------------------------------------------------------------------------------
    
    
    
  
command({body, { manifoldlab, { optigon_cuts, ml_cut_xz}}}  , St) ->
    plane_cut_mlab( { 1.0, 0.0, 1.0 } ,  {0.0, 0.0, 0.0 }   , St);
    
command({body, { manifoldlab, { optigon_cuts, ml_cut_mxz}}}  , St) ->
    plane_cut_mlab( { 1.0, 0.0, -1.0 } ,  {0.0, 0.0, 0.0 } , St);
    
command({body, { manifoldlab, { optigon_cuts, ml_cut_xzero}}}  , St) ->
    plane_cut_mlab( { 1.0, 0.0, 0.0 } ,  {0.0, 0.0, 0.0 }  , St);
    
command({body, { manifoldlab, { optigon_cuts, ml_cut_zzero}}}  , St) ->
    plane_cut_mlab( { 0.0, 0.0, 1.0 } ,  {0.0, 0.0, 0.0 }  , St);
    
    
command({body, { manifoldlab, { optigon_cuts,  ml_cut_8}}}  , St) ->
    St1 = plane_cut_mlab( { 1.0, 0.0, 1.0 },  {0.0, 0.0, 0.0 }  , St),
    St2 = plane_cut_mlab( { 1.0, 0.0, -1.0},  {0.0, 0.0, 0.0 }  , St1),
    St3 = plane_cut_mlab( { 1.0, 0.0, 0.0 },  {0.0, 0.0, 0.0 }  , St2),
    plane_cut_mlab(       { 0.0, 0.0, 1.0 },  {0.0, 0.0, 0.0 }  , St3);
    

%%-----------------------------------------------------------------------------------------------


command({body, { manifoldlab, hull_cuts_all }}  , #st{shapes=Shapes, sel=[{WeID, _ }]} = St) ->  

	 We = gb_trees:get(WeID, Shapes),
	 #we{fs=Ftab,vp=Vtab}=We,
	 { CX, CY, CZ } = wings_vertex:center(We),
	 
	 %% Remove cutter object for now, testing phase.
	 Shapes2 = gb_trees:delete(WeID, Shapes),
	 
	 MyAcc = fun( Fi, #st{shapes=AccShapes}=AccSt) ->
	 	N = wings_face:normal(Fi,We),
	 	{ CXf, CYf, CZf } = wings_face:center(Fi, We),
	 	_Sel =  [  { KeyWe, gb_sets:singleton(0) } || KeyWe  <- gb_trees:keys(AccShapes) ], 
	 	case catch plane_cut_mlab( N ,  { CXf, CYf, CZf }  , AccSt#st{sel=_Sel} ) of 
	 		#st{sel=_}=AccSt2 ->
	 			removeTwoSidedShapes(AccSt2);  %% This should stop the catch from finding some errors.
	 		_ODD_ERROR ->
	 			io:format("Plane Cut Error  !\n", [ ] ),
	 			AccSt
	 	end	
	 end,
	 
	 St3 = lists:foldl(MyAcc, St#st{shapes=Shapes2}, gb_trees:keys(Ftab) ),
	 wings_shape:recreate_folder_system(St3#st{ sel=[ ] } );
	 
	 

    
command({body, { manifoldlab, hull_cuts_inside }}  , #st{shapes=Shapes, sel=[{WeID, _ }]} = St) ->  

	 We = gb_trees:get(WeID, Shapes),
	 #we{fs=Ftab,vp=Vtab}=We,
	 { CX, CY, CZ } = wings_vertex:center(We),
	 
	 %% Remove cutter object for now, testing phase.
	 Shapes2 = gb_trees:delete(WeID, Shapes),
	 
	 MyAcc = fun( Fi, AccSt) ->
	 	N = wings_face:normal(Fi,We),
	 	{ CXf, CYf, CZf } = wings_face:center(Fi, We),
	 	AccSt2 = plane_cut_mlab( N,  { CXf, CYf, CZf } , AccSt),
	 	#st{shapes=AccShapes2}=AccSt2,
	 	
	 	Points = [  array:get(VIndex,Vtab)    || VIndex <- wings_face:vertices_ccw(Fi, We) ],
		[ P1, P2, P3 | _T ] = Points,
		{ A, B, C, D }  = plane_coefficients(P1, P2, P3),
		Sign1 = signPointToPlane( {CX, CY, CZ}, { A, B, C, D} ),
	 	
	 	MyAcc2 = fun( _We, _AccSt) ->
			{ _CX, _CY, _CZ } = wings_vertex:center(_We),
			Sign2 = signPointToPlane( {_CX, _CY, _CZ}, { A, B, C, D} ),
			
			case Sign2 /= Sign1  of 
				true  ->
						deleteWe(_We, _AccSt);
						
				false  ->
						_AccSt
			end
		end,
		
		lists:foldl(MyAcc2, AccSt2, gb_trees:values(AccShapes2) )
	 end,
	 
	 Sel2 =  [  { KeyWe, gb_sets:singleton(0) } || KeyWe  <- gb_trees:keys(Shapes2) ], 
	 
	 St3 = lists:foldl(MyAcc, St#st{sel=Sel2, shapes=Shapes2}, gb_trees:keys(Ftab) ),
	 
	 wings_shape:recreate_folder_system(St3#st{ sel=[ ] } );
	 
	 
	 
command({body, { manifoldlab, hull_cuts_outside }}  , #st{shapes=Shapes, sel=[{WeID, _ }]} = St) ->    

	 We = gb_trees:get(WeID, Shapes),
	 #we{fs=Ftab}=We,

	 %% Remove cutter object for now, testing phase.
	 Shapes2 = gb_trees:delete(WeID, Shapes),
	 
	 MyAcc = fun( Fi, #st{shapes=AccShapes}=AccSt) ->
	 	_Sel =  [  { KeyWe, gb_sets:singleton(0) } || KeyWe  <- gb_trees:keys(AccShapes) ], 
	 	N = wings_face:normal(Fi,We),
	 	{ CXf, CYf, CZf } = wings_face:center(Fi, We),
	 	plane_cut_mlab( N ,  { CXf, CYf, CZf }  , AccSt#st{sel=_Sel} )
	 end,
	 
	 
	 St3 = lists:foldl(MyAcc, St#st{shapes=Shapes2}, gb_trees:keys(Ftab) ),
	 
	 MyAcc2 = fun(_We, _AccSt) ->
	 		{ _CX, _CY, _CZ } = wings_vertex:center(_We),
	 		case pointInsideHull({ _CX, _CY, _CZ }, We) of 
	 			true ->
	 				_AccSt;
	 			false ->
	 				deleteWe(_We, _AccSt)
	 		end
	 end,
	 
	 #st{shapes=Shapes3}=St3,
	 St4 = lists:foldl(MyAcc2, St3, gb_trees:values(Shapes3) ),
	 wings_shape:recreate_folder_system(St4#st{ sel=[ ] } );
	 
	 
	 
command({body, { manifoldlab, KIND }}  , #st{} = _St) when 
	KIND == hull_cuts_inside orelse
	KIND == hull_cuts_outside
	->    
	wings_u:error_msg("Wrong set up. Please pick 1 cutter object only."),
	_St;
    
%%-----------------------------------------------------------------------------------------------
    
 
command(_,_) -> next.



removeTwoSidedShapes(#st{shapes=Shapes}=St) ->

	MyAcc = fun( #we{id=ID,fs=Ftab}, #st{shapes=AccShapes}=Acc) ->
		case gb_trees:size(Ftab) < 3 of 
			true ->
				Acc#st{shapes=gb_trees:delete(ID, AccShapes)};
			false ->
				Acc
		end
	end,
	
	lists:foldl(MyAcc, St, gb_trees:values(Shapes) ).
	
	
	
renumberAll(#st{shapes=Shapes}=St) ->
	MyAcc = fun( #we{id=ID}=We, AccSt) ->
		wings_shape:replace(ID, wings_we:renumber(We, 0), AccSt)
	end,
	lists:foldl(MyAcc, St, gb_trees:values(Shapes) ).
	
	
	
selectAll(#st{selmode=body, shapes=Shapes}=St) ->
	Sel = [ { Key, gb_sets:singleton(0) } ||  Key <- gb_trees:keys(Shapes) ],
	St#st{sel=Sel}.
	
	
plane_cut_mlab( Axis0, Point, _St0 ) ->
    St0 = selectAll(_St0),
	{ _, St1 } = plane_cut(true, {e3d_vec:norm(Axis0),Point}, St0),
	selectAll(St1).
	
	
	


signum( X )->
    if
        X < 0.0000 ->
             -1;
        true ->
            1
    end.


signPointToPlane( {X, Y, Z}, { A, B, C, D} ) ->
    signum( A*X + B*Y + C*Z + D).
    
    
det_2x2( { A1, B1 }, 
         { A2, B2 } ) ->
             A1*B2 - A2*B1.
            
det_3x3( { A, B, C },
         { A1, B1, C1 }, 
         { A2, B2, C2 }) ->
             det_2x2(     { B1, C1 } , 
                        { B2, C2 } )*A - 
            
            det_2x2(     { A1, C1 } , 
                        { A2, C2 } )*B + 
                        
            det_2x2(    { A1, B1 }, 
                           { A2, B2 })*C.
                        
plane_coefficients( { X1, Y1, Z1} , { X2, Y2, Z2 } , { X3, Y3, Z3 } ) ->
        A = det_3x3( { 1, Y1, Z1 },
                     { 1, Y2, Z2 },
                     { 1, Y3, Z3 }),
                     
        B = det_3x3( { X1, 1, Z1 },
                     { X2, 1, Z2 },
                     { X3, 1, Z3 }),
                     
        C = det_3x3( { X1, Y1, 1 },
                     { X2, Y2, 1 },
                     { X3, Y3, 1 }),
                     
        D = -1.0 * det_3x3( { X1, Y1, Z1 },
                             { X2, Y2, Z2 },
                            { X3, Y3, Z3 }),
                            
        { A, B, C, D }.	
	
	
	
	
	
	
	
%%-------------------- CUT-PASTE COPIED from Optigons Plane Cut -----------------------------------------
	

plane_cut(Cut, {Axis0,Point}, St0) ->
    Axis = axis_conv(Axis0),
    {St1,Sel} = wings_sel:mapfold(fun(_, #we{id=Id}=We0, Acc) ->
            EdgePos2Cut = intersects(Axis, Point, We0),
            {NewEdges,We} = cut_intersecting_edges(Cut, Axis, EdgePos2Cut, We0),
            {We,[{Id,NewEdges}|Acc]}
        end,[],St0),
    St = wings_sel:set(edge, Sel, St1),
    case Cut of
      true ->
        {save_state,loop_cut(Axis, Point, wings_sel:valid_sel(St))};
      false ->
        {save_state,wings_sel:valid_sel(St)}
    end.

planecut_error() ->
    wings_u:error_msg(?__(1,"This is taking too long.\nTry again with a smaller selection.")).

%%%
%%% Calculate slice points distributed equally along the given axis
%%%

slice_points(Axis, N, #st{selmode=face}=St) ->
    Zero = e3d_vec:zero(),
    [{_,PosA},{_,PosB}] =
    wings_sel:fold(fun(Faces,#we{vp=Vtab}=We,Acc) ->
        Vs = wings_face:to_vertices(Faces,We),
        foldl(fun
          (V,none) ->
            Pos = array:get(V, Vtab),
            D = dist_along_vector(Pos, Zero, Axis),
            [{D,Pos},{D,Pos}];
          (V,[{MinD,_}=Min,{MaxD,_}=Max]) ->
            Pos = array:get(V, Vtab),
            D = dist_along_vector(Pos, Zero, Axis),
            if D < MinD -> [{D,Pos},Max];
               D > MaxD -> [Min,{D,Pos}];
               true -> [Min,Max]
            end
        end, Acc, Vs)
    end, none, St),
    Dist = abs(dist_along_vector(PosA, PosB, Axis)) / N,
    get_point_positions(PosA, Dist, Axis, N-1);
slice_points(Axis, N, #st{selmode=body}=St) ->
    Zero = e3d_vec:zero(),
    [{_,PosA},{_,PosB}] =
    wings_sel:fold(fun(_,#we{vp=Vtab},Acc) ->
        array:sparse_foldl(fun
          (_, Pos, none) ->
            D = dist_along_vector(Pos, Zero, Axis),
            [{D,Pos},{D,Pos}];
          (_, Pos, [{MinD,_}=Min,{MaxD,_}=Max]) ->
            D = dist_along_vector(Pos, Zero, Axis),
            if D < MinD -> [{D,Pos},Max];
               D > MaxD -> [Min,{D,Pos}];
               true -> [Min,Max]
            end
        end, Acc, Vtab)
    end, none, St),
    Dist = abs(dist_along_vector(PosA, PosB, Axis)) / N,
    get_point_positions(PosA, Dist, Axis, N-1).


dist_along_vector({Xa,Ya,Za},{Xb,Yb,Zb},{Vx,Vy,Vz}) ->
%% Return Distance between PosA and PosB along Normalized Vector
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

get_point_positions(_, _, _, 0) -> [];
get_point_positions(Pos0, Dist, Axis, N) ->
    Pos = e3d_vec:add(Pos0, e3d_vec:mul(Axis, Dist)),
    [Pos|get_point_positions(Pos, Dist, Axis, N-1)].


%% There are optimizations for Standard Axes (see opposite_sides).
%% Body mode is faster than face mode for wholly selected objects since face
%% mode has to cenvert the selection to edges as part of processing the
%% selection; body mode just folds over the Etab.

%%%
%%% Body mode
%%%

intersects({1.0,0.0,0.0}=Plane, {X,_,_}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {Xa,_,_} = PosA = array:get(Va, Vtab),
        {Xb,_,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Xa, Xb, X) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects({0.0,1.0,0.0}=Plane, {_,Y,_}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {_,Ya,_} = PosA = array:get(Va, Vtab),
        {_,Yb,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Ya, Yb, Y) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects({0.0,0.0,1.0}=Plane, {_,_,Z}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {_,_,Za} = PosA = array:get(Va, Vtab),
        {_,_,Zb} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Za, Zb, Z) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects(Plane, CutPoint, #we{es=Etab,vp=Vtab}) ->
    SideArray = assign_side(Vtab, Plane, CutPoint),
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {SideA,PosA} = array:get(Va, SideArray),
        {SideB,PosB} = array:get(Vb, SideArray),
        case oposite_sides(SideA, SideB) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab).

%%%
%%% Face mode
%%%

intersects(Faces, {1.0,0.0,0.0}=Plane, {X,_,_}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {Xa,_,_} = PosA = array:get(Va, Vtab),
        {Xb,_,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Xa, Xb, X) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, {0.0,1.0,0.0}=Plane, {_,Y,_}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {_,Ya,_} = PosA = array:get(Va, Vtab),
        {_,Yb,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Ya, Yb, Y) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, {0.0,0.0,1.0}=Plane, {_,_,Z}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {_,_,Za} = PosA = array:get(Va, Vtab),
        {_,_,Zb} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Za, Zb, Z) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, Plane, CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        PosA = array:get(Va, Vtab),
        PosB = array:get(Vb, Vtab),
        case oposite_sides(PosA, PosB, CutPoint, Plane) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0).

%%%
%%% Cut edges and connect the vertices
%%%

cut_intersecting_edges(Type, Plane, EdgePos, We0) ->
    {Vs0,We1} = cut_edges(EdgePos, We0),
    Vs = ordsets:from_list(Vs0),
    {GoodVs,We} = connect(Type, Plane, Vs, We1),
    Edges = vs_to_edges(GoodVs, We, []),
    {Edges,We}.

connect(true, Plane, Vs0, We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,Vs}, {VsA,NewWe}) ->
      FaceNorm = wings_face:normal(Face, We),
      case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
        true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
        false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
      end
    end, {Vs0,We}, FaceVs);
connect(false, Plane, Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun
           ({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
           ({Face,Vs}, {VsA,NewWe}) ->
              FaceNorm = wings_face:normal(Face, We),
              case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
                true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
                false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
              end
    end, {Vs0,We}, FaceVs);
connect(Faces, Plane, Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun
           ({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
           ({Face,Vs}, {VsA,NewWe}=Acc) ->
             case gb_sets:is_member(Face, Faces) of
               true ->
                 FaceNorm = wings_face:normal(Face, We),
                 case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
                   true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
                   false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
                 end;
               false -> Acc
             end
      end, {Vs0,We}, FaceVs).

vs_to_edges([Va|Vs], We, Acc0) ->
%% finds the edges between any two listed vertices and returns a gb_set fo edges
    Edges = wings_vertex:fold(fun(Edge, _, EdgeRec, Acc) ->
         Vb = wings_vertex:other(Va, EdgeRec),
         case ordsets:is_element(Vb,Vs) of
           true -> [Edge|Acc];
           _ -> Acc
         end
     end, Acc0, Va, We),
    vs_to_edges(Vs, We, Edges);
vs_to_edges([], _, Edges) ->
    gb_sets:from_list(usort(Edges)).

cut_edges(Es, We0) ->
%% Fold over the list Es of which each entry is a tuple with the Edge id and the
%% proposed cut position. If the proposed cut position is VERY close to one of
%% the edge's vertices, then don't cut that, but return that Vertex id.
%% If the edge is cut, then return the new vertex id. In a later function, all
%% of the collected vertex id will be connected with new edges making the final
%% edge loop.
    lists:mapfoldl(fun
        ({Edge,Pos}, #we{es=Etab,vp=Vtab}=We1) ->
            #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
            PosA = array:get(Va, Vtab),
            case e3d_vec:dist(PosA, Pos) < ?NON_ZERO of
              true -> {Va,We1};
              false ->
                PosB = array:get(Vb, Vtab),
                case e3d_vec:dist(PosB, Pos) < ?NON_ZERO of
                  true -> {Vb,We1};
                  false ->
                    {We,V} = wings_edge:fast_cut(Edge, Pos, We1),
                    {V,We}
                end
            end
         end, We0, Es).

%%%
%%% Plane Cut utilities
%%%

intersect_vec_plane(PosA, PosB, Plane, EdgeVec) ->
%% Return point where Vector through PosA intersects with plane at PosB
    case e3d_vec:dot(EdgeVec,Plane) of
      0.0 ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane),
        e3d_vec:add(PosB, e3d_vec:mul(Plane, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosA, PosB), Plane) / Dot,
        e3d_vec:add(PosB, e3d_vec:mul(EdgeVec, Intersection))
    end.

%% Tests whether the 2 vertices of an edge are on opposite sides of the Plane.
%% The opposite_sides function that is used depends on the Plane and the
%% selection mode.
oposite_sides(A, B) when A =:= on_vertex; B =:= on_vertex -> true;
oposite_sides(Side, Side) -> false;
oposite_sides(_, _) -> true.

oposite_sides(_, B, B) -> true;
oposite_sides(A, _, A) -> true;
oposite_sides(A, B, C) ->
    SideA = A < C,
    SideB = B < C,
    SideA =/= SideB.

oposite_sides(PosA, PosB, CutPoint, Plane) ->
    VecA = e3d_vec:norm_sub(PosA, CutPoint),
    VecB = e3d_vec:norm_sub(PosB, CutPoint),
    Zero = e3d_vec:zero(),
    case e3d_vec:dot(VecA, Plane) =< 0 of
      _ when VecA =:= Zero; VecB =:= Zero ->
        true;
      true ->
        e3d_vec:dot(VecB, Plane) >= 0;
      false ->
        e3d_vec:dot(VecB, Plane) =< 0
    end.

assign_side(Vtab, Plane, CutPoint) ->
%% Create an array using each vertex id as the index and {Dot < 0, Pos} as the
%% value. The Dot boolean is later used to determine the side of the plane on
%% which the vertex resides. The position is stored just so is doesn't have to
%% be looked up again.
    array:sparse_foldl(fun
      (V, Pos, Array) ->
        Vec = e3d_vec:norm_sub(Pos, CutPoint),
        Dot = e3d_vec:dot(Vec, Plane),
        case Vec =:= e3d_vec:zero() of
          true ->
            array:set(V, {on_vertex ,Pos}, Array);
          false ->
            array:set(V, {Dot =< 0 ,Pos}, Array)
        end
    end, Vtab, Vtab).

axis_conv(Axis) ->
%% Converts an atom axis to a tuple axis.
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      last_axis ->
        {_, Dir} = wings_pref:get_value(last_axis),
        Dir;
      default_axis ->
        {_, Dir} = wings_pref:get_value(default_axis),
        Dir;
      {X,Y,Z} -> {X,Y,Z};
      {LastAxis,_} -> LastAxis
    end.

%%%
%%% Loop Cut modified from wings_edge_cmd.erl
%%%

loop_cut(Axis, Point, St0) ->
    {Sel,St} = wings_sel:fold(fun(Edges, #we{id=Id,fs=Ftab}=We0, {Sel0,St1}) ->
        AdjFaces = wings_face:from_edges(Edges, We0),
        case loop_cut_partition(AdjFaces, Edges, We0, []) of
          [_] ->
            {Sel0, St1}; %% wings_u:error_msg(?__(2,"Edge loop doesn't divide mesh into two or more parts."));
          [_|Parts0] ->
            Parts = [gb_sets:to_list(P) || P <- Parts0],
            FirstComplement = ordsets:union(Parts),
            First = ordsets:subtract(gb_trees:keys(Ftab), FirstComplement),
            We = wings_dissolve:complement(First, We0),
            #st{shapes=Shapes} = St1,
            St = St1#st{shapes=gb_trees:update(Id, We, Shapes)},
            Sel = select_one_side(Axis, Point, Id, Sel0, We),
            loop_cut_make_copies(Parts, Axis, Point, We0, Sel, St)
        end
    end, {[],St0}, St0),
    wings_sel:set(body, Sel, St).

loop_cut_make_copies([P|Parts], Axis, Point, We0, Sel0, #st{onext=Id}=St0) ->
    We = wings_dissolve:complement(P, We0),
    Sel = select_one_side(Axis, Point, Id, Sel0, We),
    St = wings_shape:insert(We, cut, St0),
    loop_cut_make_copies(Parts, Axis, Point, We0, Sel, St);
loop_cut_make_copies([], _, _, _, Sel, St) -> {Sel,St}.

loop_cut_partition(Faces0, Edges, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
      true -> Acc;
      false ->
        {AFace,Faces1} = gb_sets:take_smallest(Faces0),
        Reachable = collect_faces(AFace, Edges, We),
        Faces = gb_sets:difference(Faces1, Reachable),
        loop_cut_partition(Faces, Edges, We, [Reachable|Acc])
    end.

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
      true -> Acc0;
      false ->
        {Face,Work1} = gb_sets:take_smallest(Work0),
        Acc = gb_sets:insert(Face, Acc0),
        Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
        collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(_, Edge, Rec, A) ->
          case gb_sets:is_member(Edge, Edges) of
            true -> A;
            false ->
              Of = wings_face:other(Face, Rec),
              case gb_sets:is_member(Of, Res) of
                true -> A;
                false -> gb_sets:add(Of, A)
              end
          end
      end, Work, Face, We).

select_one_side(_, all, Id, Sel, _) ->
    [{Id,gb_sets:singleton(0)}|Sel];
select_one_side(Plane, Point, Id, Sel, #we{fs=Fs}=We) ->
    {Face,_} = gb_trees:smallest(Fs),
    Center = wings_face:center(Face, We),
    Vec = e3d_vec:sub(Point, Center),
    case e3d_vec:dot(Plane, Vec) < 0.0 of
      true -> [{Id,gb_sets:singleton(0)}|Sel];
      false -> Sel
    end.


