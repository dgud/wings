-module(wpc_ml_corners_loop).

%% This module encapsulated EXTRA tight so it can be looked at as a stand-alone plugin
%% but still uses wpml_charts.erl  as a resource. All other dependencies are on Wings3D core.

%% This module aims to take TWO vertices and will form a nice loop
%% from one to the other and back again ... all rectangular marquee ... or other
%% best ... nice fitting enclosing loops. You must see it to believe it.

%% See You tube videos.

-export([init/0, menu/2, command/2 ]).


init() ->
    true.
   
    
-include("wings.hrl").

	
menu({select}, Menu) ->

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
   		vertex  ->
   			parseMenuInsert(Menu, { "Corners Loop" , 
   					'[cl_straight,cl_classic,cl_ortho]', "Convert to small loop." }, 
   					manifoldlabRootItem());
   	    _  ->
   			Menu
   end;
   
              
menu(_,Menu) -> 
    Menu.
    
    
    
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
    
%%---------------  END MENU CODE ! -----------------------------------------------------    


%% make the cube (It might be the one that has two verts as diagonal corners here !
cube({X, Y, Z}) ->
	 Xi = X/2.0,
	 Yi = Y/2.0,
	 Zi = Z/2.0,
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    Vs = [{-Xi,-Yi,Zi},{-Xi,Yi,Zi},{Xi,Yi,Zi},{Xi,-Yi,Zi},
	  {-Xi,-Yi,-Zi},{-Xi,Yi,-Zi},{Xi,Yi,-Zi},{Xi,-Yi,-Zi}],
    %% The string below is intentionally not translated.
    We = wings_we:build(Fs, Vs),
    We#we{name="cube"}.
    


%% Are Two vectors parallel ... handle extremely short vector case with unknown return    
is_parallel(V1, V2) ->
	L1 = e3d_vec:len(V1),
	L2 = e3d_vec:len(V2),
	if 
		(L1 < 0.00001) or (L2 < 0.00001) ->
			unknown;
		true ->
			Angle = abs( e3d_vec:degrees(V1, V2) ),
			(Angle < 0.1) or ((180.0001 - Angle) < 1.0)
	end.
	
    
%%---------------------------------------------------------------------------------	
%% SELECT all edges on We (Target) that are colinear to edges on _WeIsBox shape.
%%  ---  With additional requirement tha the colinear edges are also 
%%  ---  fully are subsets of edges on _WeIsBox
%%  ---  Returns a gb_set of Edges
%%  ---  The idea is that this can marquee from vertex to vertex, even on a manger cube
%%---------------------------------------------------------------------------------	
colinearEdges(  #we{es=EtabBox,vp=VPosBox}=_WeIsBox,   #we{es=Etab,vp=VPos}=_We) ->
	BoxDict = array:sparse_to_orddict(EtabBox),
	MyAcc = fun(   { Ei, #edge{vs=VSi, ve=VEi} },   Acc  ) ->
		PSi = array:get(VSi, VPos),
		PEi = array:get(VEi, VPos),
		
		Vi = e3d_vec:sub(PSi,PEi),
		
		MyAcc2 = fun( { _Ej, #edge{vs=VSj, ve=VEj} }, Acc2 ) ->
				PSj = array:get(VSj, VPosBox),
				PEj = array:get(VEj, VPosBox),
				
				LenJ = e3d_vec:dist(PSj,PEj),
				
				Vj = e3d_vec:sub(PSj,PEj),
				IsParallel = is_parallel(Vi, Vj),

				if 
					IsParallel ->
						Vodd1 = e3d_vec:sub(PSj,PSi),
						Vodd2 = e3d_vec:sub(PSj,PEi),
						
						case { is_parallel(Vj,Vodd1), is_parallel(Vj,Vodd2) } of
							{ false, false } ->
								Acc2;
							{ false, unknown } ->
								Acc2;
							{ unknown, false } ->
								Acc2;
							{ _, _ }  ->
								DistList = [ e3d_vec:dist(PSi,PSj), e3d_vec:dist(PSi,PEj), e3d_vec:dist(PEi,PSj), e3d_vec:dist(PEi,PEj) ],
								
								MyAny = fun(X) ->
									(X > (LenJ + 0.001))
								end,
								
								case lists:any(MyAny, DistList) of
									true ->
										Acc2;
									false ->
										gb_sets:add(Ei, Acc2)
								end

						end;
					true ->
						Acc2
				end
		end,
		
		lists:foldl(MyAcc2, Acc, BoxDict )
	end,
	
	lists:foldl(MyAcc, gb_sets:empty(), array:sparse_to_orddict(Etab) ).
	



command({select, { manifoldlab, corners_bounding_box }}, #st{shapes=Shapes, sel=[{WeID, Set}] }=St) ->
	wings_io:hourglass(),
	
	Sz = gb_sets:size(Set),
	case Sz /= 2 of 
		true ->
			wings_u:error_msg("Select exactly two vertices.");
		false ->
			ok
	end,
	
	[ V1, V2 ] = gb_sets:to_list(Set),
	
	We = gb_trees:get(WeID, Shapes),
	#we{vp=VPos}=We,
	
	P1 = array:get(V1, VPos),
	P2 = array:get(V2, VPos),
	
	{ _DX, _DY, _DZ }  = e3d_vec:sub(P1,P2),
	
	{ CX, CY, CZ } = e3d_vec:average([ P1, P2 ]), 
	
	{ DX, DY, DZ } = { abs(_DX), abs(_DY), abs(_DZ) },
	
	WeBox = cube( { DX,DY,DZ } ),

	Mat  = e3d_mat:translate( CX, CY, CZ ),
	
	WeBox2 = wings_we:transform_vs(Mat, WeBox),
	
	EdgeSet = colinearEdges(WeBox2, We),
	
	
	St#st{selmode=edge, sel=[{WeID, EdgeSet}]};

    
	
	
%%--------------------------------------------------------------------------------------
%% Corners to loop experiment. 
%% This code finds smallest "rectangular" or other loop containing to vertices.
%% It assmes an iterator over chart regions.
%% Could potentially add third ortho loops method to the fallback scheme here.
command({select, { manifoldlab, KIND }}, #st{shapes=Shapes, sel=[{WeID, Set}] }=St) when 
	KIND == '[cl_straight,cl_classic,cl_ortho]' orelse
	KIND == '[cl_classic,cl_ortho]' orelse 
	KIND == '[cl_ortho]'  
	
	->
	wings_io:hourglass(),
	
	Temp = parseAtomList(atom_to_list(KIND) ),
	[  KIND1 | _T ] = Temp,
	
	
	Sz = gb_sets:size(Set),
	case Sz /= 2 of 
		true ->
			wings_u:error_msg("Select exactly two vertices.");
		false ->
			ok
	end,
	
	wings_io:hourglass(),
	
	[ V1, V2 ] = gb_sets:to_list(Set),
	
	
	We = gb_trees:get(WeID, Shapes),
	St2 = wings_sel_conv:mode(edge,St),
	
	case KIND1 of 
		cl_straight  ->
			We = gb_trees:get(WeID, Shapes),
			SetEdges = extendStraight( [ V1, V2 ], We),
			St3 = St2#st{selmode=edge,sel=[{WeID, SetEdges }] };
		cl_classic ->
			{ _, St3 } = wings_sel_cmd:command({edge_loop,complete_loops}, St2);
		cl_ortho ->
			St3 = command({select, { manifoldlab, corners_bounding_box }}, St)
	end,
	
	#st{selmode=edge, sel=[{_,EdgeSet}] }=St3,
	
	#we{es=Etab}=We,
	
	MyAcc = fun(Region, _Acc, _We) ->
	    Vs = wings_vertex:outer_vertices_ccw(Region, _We),
	    case Vs of 	
	    	error ->
	    		_Acc;
	    	_ ->
		
				case lists:member(V1,Vs) and lists:member(V2,Vs) of 
					true ->
						_OE = gb_sets:from_list(wings_face:outer_edges(Region, _We)),
						case gb_sets:size(_OE) < gb_sets:size(_Acc) of
							true ->
								_OE;
							_ ->
								_Acc
						end;
					false ->
						_Acc
				end
		end
	end,
	
	
	EList  = array:sparse_to_orddict(Etab),
	EList2 = [ Ei || { Ei, _ } <- EList ],
	
	SetAll = gb_sets:from_list(EList2),
	
	OE = wpml_charts:fold_chart_region(MyAcc, SetAll, We#we{he=EdgeSet} ),
	
	case { KIND1, gb_sets:size(OE) == gb_sets:size(SetAll), _T }  of 
		{ cl_ortho, _ , _ } ->
			St3;
		{ _, true, [ ] } ->
			wings_u:error_msg("Try Other Methods."),
			St;
			
		{ _, true, [ PART2, PART3 ] } ->
			ListNext = "["++atom_to_list(PART2)++","++atom_to_list(PART3)++"]",
			command({select, { manifoldlab, list_to_atom(ListNext) }}, St);
			
		{ _, true, [ PART2 ] } ->
		    ListNext = "["++atom_to_list(PART2)++"]",
			command({select, { manifoldlab, list_to_atom(ListNext) }}, St);
			
		{ _, false, _ }  ->
			St3#st{ sel=[{WeID, OE}] }
	end;
    
    
    

command(_,_) -> 
	next.
	
	
	
%% This is used when menu Item key is NON atom ... atomized by the single quotes.
%% This decomposes it into parts. 
%% If more Fallback schemes need ... this will need to be beefed up.
parseAtomList(Str) ->	
	 case erl_scan:tokens([], Str ++ ". ",1) of 
	 
	 	 {done,{ok,[{'[',1},
			   {atom,1,P1},
			   {',',1},
			   {atom,1,P2},
			   {',',1},
			   {atom,1,P3},
			   {']',1},
			   {dot,1}],
			  1},
		  []}  ->
		  		[ P1, P2, P3 ];

		 {done,{ok,[{'[',1},
			   {atom,1,P1},
			   {',',1},
			   {atom,1,P2},
			   {']',1},
			   {dot,1}],
			  1},
		  []}  ->
		  
				[ P1, P2 ];
				
		 {done,{ok,[{'[',1},
			   {atom,1,P1},
			   {']',1},
			   {dot,1}],
			  1},
		  []} ->
		  		[ P1 ]
		  		
	end.
	
	
%%------------------------------------------------------------------------	
%% Given a vertex ... and an edge coming out of it
%% Grab the other vertex id from other (OPPOSITE) end of the edge
%%------------------------------------------------------------------------
oppositeVert( Vs, E, #we{es=Etab} ) ->
	#edge{vs=VS2,ve=VE2}=array:get(E,Etab),
	case Vs == VS2 of
		true ->
			VE2;
		false ->
			VS2
	end.
	

%%------------------------------------------------------
%% extend set of edges from a list of vertices	
%% generally with no sharp turns. 
%%------------------------------------------------------
extendStraight([ _Vi | _ ] = VList, #we{}=We) ->	
	ListDeep = 
		[ begin    
				Es = wings_edge:from_vs([ Vi ], We), 
				[  { Vi, E }   || E <- Es  ]
	      end
	     ||  Vi  <- VList ],
	     
	List = lists:flatten(ListDeep),
	

	MyAcc = fun( { Vs, E } , Acc ) ->
			List2 = [ E  | extendStraight(We, { Vs, E } , gb_trees:from_orddict([{E,ok}]) ) ],
			Set = gb_sets:from_list(List2),
			gb_sets:union(Set,Acc)
	end,
	
	lists:foldl(MyAcc, gb_sets:empty(),  List ).
	
	
	
%%------------------------------------------------------	
%% extend set from a vertex and outgoing edge !  
%% generally with no sharp turns. 
%%------------------------------------------------------
extendStraight(#we{vp=VPos,es=Etab}= We, { Vs, E } , EdgeLook) ->

     Ve = oppositeVert(Vs,E,We),
     Es = wings_edge:from_vs( [ Ve ], We ),
     
     MyAcc = fun( EO , { SmallAngle, {_,_}=Pair } ) ->
            #edge{vs=VS2,ve=VE2}=array:get(EO, Etab),
            Seen = gb_trees:is_defined(EO, EdgeLook),
            if 
            	Seen ->                        { SmallAngle, Pair };
          
                (VS2 == Vs) or (VE2 == Vs) ->    { SmallAngle, Pair };
            			
            	true ->
            			{ VS3, VE3 } = 
            			case (VS2 == Ve) of 
            				true  -> { VS2, VE2 };
            				false -> { VE2, VS2 }
            			end,
            				
            		   	{ Ax, Ay, Az } = array:get(Vs, VPos),
						{ Bx, By, Bz } = array:get(VS3, VPos),
						{ Cx, Cy, Cz } = array:get(VE3, VPos),
						
						Angle = abs(e3d_vec:degrees({ Bx - Ax, By - Ay, Bz - Az}, { Cx - Bx, Cy - By, Cz - Bz } )),
						
						if 
							(Angle < SmallAngle)  ->
								{ Angle, { VS3, EO}  };
							true ->
								{ SmallAngle, Pair }
						end
            end
     end,
     
     { _Angle, Pair  } = lists:foldl(MyAcc, { 20.0, { none, none }  } , Es  ),
     
     case Pair of 
     	{ none, none } ->
     		[ ] ;
     	{ BestV, BestE }  ->
     		[ BestE | extendStraight(We, { BestV, BestE } , gb_trees:enter(BestE, ok, EdgeLook) ) ]
     end.
    
     
		  
      



    