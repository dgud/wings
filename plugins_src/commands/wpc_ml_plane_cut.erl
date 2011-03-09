

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
   	{ ?__(20,"Cut X=Z" 	),  ml_cut_xz,      "" },   
    { ?__(21,"Cut X=-Z"	),  ml_cut_mxz,     "" }, 
    { ?__(22,"Cut X=0" 	),  ml_cut_xzero,   "" },  
    { ?__(23,"Cut Z=0" 	),  ml_cut_zzero,   "" },  
    { ?__(24,"Cut 8-Cut"),  ml_cut_8,  		"" }
   ],
         
   Menu2 = 
   parseMenuInsert(Menu,  { ?__(1,"Standard Plane Cuts ..."), { optigon_cuts,  SubMenu }, ?__(2,"Standard Plane Cuts ...") } , manifoldlabRootItem()),
   parseMenuInsert(Menu2, { ?__(3,"Polyhedra as Cutter ...") , which_side_fun(),   
   						  { ?__(4,"Keep Inside Bodies"),  ?__(5, "Keep All") , ?__(6,"Keep Outside Bodies") }, [ ] } , 
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
   			parseMenuInsert(Menu, { ?__(30,"Face Partioned Bodies"), optigon_hull_partioned, ?__(31,"Face Partioned Bodies, (makes body selection)")} , manifoldlabRootItem());
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
	
	wings_sel:make(SelFun, body, St#st{ selmode=face, sel=[ ] } );
	
	
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
	 #we{fs=Ftab}=We,
	 
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
	
	
	
selectAll(#st{selmode=body, shapes=Shapes}=St) ->
	Sel = [ { Key, gb_sets:singleton(0) } ||  Key <- gb_trees:keys(Shapes) ],
	St#st{sel=Sel}.
	
	
plane_cut_mlab( Axis0, Point, _St0 ) ->
    St0 = selectAll(_St0),
	{ _, St1 } = wpc_plane_cut:plane_cut(true, {e3d_vec:norm(Axis0),Point}, St0),
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
	
	
	
	
	
	
	


