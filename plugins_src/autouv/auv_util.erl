%%
%%  auv_util.erl --
%%
%%     Some utilities used by the other AutoUV modules.
%%
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(auv_util).

-export([maxmin/1]).
-export([moveAndScale/5]).
-export([outer_edges/2,outer_edges/3]).
-export([number/1,number/2]).
-export([mark_segments/4,make_mat/1,seg_materials/0]).

-include_lib("wings/src/wings.hrl").
-include("auv.hrl").

-import(lists, [foldl/3,reverse/1,sort/1]).

moveAndScale([{Id, {X0, Y0,_}}|R], XD, YD, Scale, Acc) ->
    moveAndScale(R, XD,YD, Scale, 
		 [{Id, {X0*Scale+XD,Y0*Scale+YD,0.0}}|Acc]);
moveAndScale([],_,_,_,Acc) ->
    reverse(Acc).

maxmin([{Id, {X,Y,_}}|Rest]) ->
    maxmin(Rest, {Id, X},{Id, X},{Id, Y},{Id, Y});
maxmin([{Id, {X,Y}}|Rest]) ->
    maxmin(Rest, {Id, X},{Id, X},{Id, Y},{Id, Y}).

maxmin([],Xmin,Xmax,Ymin,Ymax) ->
    {Xmin,Xmax,Ymin,Ymax};
maxmin([{Id, {X,Y,_}}|Rest], 
       XMin={_IdX0,X0}, XMax={_IdX1,X1}, 
       YMin={_IdY0,Y0}, YMax={_IdY1,Y1}) ->
    if 	X > X1 ->
	    if Y > Y1 -> maxmin(Rest, XMin, {Id,X}, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest, XMin, {Id,X}, {Id,Y}, YMax);
	       true ->   maxmin(Rest, XMin, {Id,X}, YMin, YMax)
	    end;
	X < X0 ->
	    if Y > Y1 -> maxmin(Rest,{Id,X}, XMax, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest,{Id,X}, XMax, {Id,Y}, YMax);
	       true ->   maxmin(Rest,{Id,X}, XMax, YMin, YMax)
	    end;
	Y > Y1 ->
	    maxmin(Rest,XMin, XMax, YMin, {Id,Y});
	Y < Y0 ->
	    maxmin(Rest,XMin, XMax, {Id,Y}, YMax);
	true ->
	    maxmin(Rest,XMin, XMax, YMin, YMax)
    end;
maxmin([{Id, {X,Y}}|Rest], 
       XMin={_IdX0,X0}, XMax={_IdX1,X1}, 
       YMin={_IdY0,Y0}, YMax={_IdY1,Y1}) ->
    if 	X > X1 ->
	    if Y > Y1 -> maxmin(Rest, XMin, {Id,X}, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest, XMin, {Id,X}, {Id,Y}, YMax);
	       true ->   maxmin(Rest, XMin, {Id,X}, YMin, YMax)
	    end;
	X < X0 ->
	    if Y > Y1 -> maxmin(Rest,{Id,X}, XMax, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest,{Id,X}, XMax, {Id,Y}, YMax);
	       true ->   maxmin(Rest,{Id,X}, XMax, YMin, YMax)
	    end;
	Y > Y1 ->
	    maxmin(Rest,XMin, XMax, YMin, {Id,Y});
	Y < Y0 ->
	    maxmin(Rest,XMin, XMax, {Id,Y}, YMax);
	true ->
	    maxmin(Rest,XMin, XMax, YMin, YMax)
    end.

outer_edges(Faces0, We) ->
    outer_edges(Faces0, We, true).
outer_edges(Faces0, We, VisibleOnly) ->    
    %% I use normals here to detect direction of face and remove 
    %% faces with wrong direction.
    Faces1 = case VisibleOnly of 
		 true ->
		     foldl(fun(Face, Acc)-> 
				   Zval = e3d_vec:dot(wings_face:normal(Face, We), 
						      {0.0,0.0,1.0}),
				   case Zval >= 0.0 of
				       true -> [Face|Acc];
				       _ -> Acc
				   end
			   end, [], Faces0);
		 false ->
		     Faces0
	     end,
    S = wings_face:fold_faces(fun(Face, _, E, _, A) -> [{E,Face}|A] end, [], Faces1, We),
    outer_edges_1(sort(S), []).
outer_edges_1([{E,_},{E,_}|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([E|T], Out) ->
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

number(L) ->
    ?MODULE:number(L,0).
number([H|T], N) ->
    [{N,H}|number(T, N+1)];
number([], _) -> [].


mark_segments(Charts0, Cuts, We0, St) ->
    We = We0#we{he=Cuts},			%Hard edges mark the cuts.

    %% Use materials to mark different charts.
    Template = list_to_tuple([auv_util:make_mat(Diff) ||
				 {_,Diff} <- seg_materials()]),
    Charts = reuse_materials(Charts0, We0, length(Charts0)),
    assign_materials(Charts, We, Template, 0, St).

assign_materials([Faces|T], We0, Template, I0, #st{mat=Mat0}=St0) ->
    I = I0 + 1,
    MatName = list_to_atom("AuvChart" ++ integer_to_list(I)),
    We = wings_facemat:assign(MatName, Faces, We0),
    case gb_trees:is_defined(MatName, Mat0) of
	true ->
	    assign_materials(T, We, Template, I, St0);
	false ->
	    MatDef = element(I0 rem size(Template) + 1, Template),
	    {St,[]} = wings_material:add_materials([{MatName,MatDef}], St0),
	    assign_materials(T, We, Template, I, St)
    end;
assign_materials([], #we{id=Id}=We, _, _, #st{shapes=Shs0}=St) ->
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.

make_mat(Diff) ->
    [{opengl,[{diffuse,Diff}, {metallic, 0.0}, {roughness, 1.0}]}].

seg_materials() -> % Intensity 0.7 for all
    [{'AuvChart1',{0.7,0.7,0.0}},  % Yellow
     {'AuvChart2',{0.0,0.7,0.7}},  % Cyan
     {'AuvChart3',{0.7,0.0,0.7}},  % Magenta
     separator,
     {'AuvChart4',{0.4,0.7,0.0}},  % Yellow -> Green
     {'AuvChart5',{0.0,0.4,0.7}},  % Cyan -> Blue
     {'AuvChart6',{0.7,0.0,0.4}},  % Magenta -> Red
     separator,
     {'AuvChart7',{0.0,0.7,0.4}},  % Cyan -> Green
     {'AuvChart8',{0.4,0.0,0.7}},  % Magenta -> Blue
     {'AuvChart9',{0.7,0.4,0.0}}]. 

reuse_materials(Charts, _We, Size)
  when Size < 10 ->
    Charts;
reuse_materials(Charts0, We, Size) ->
    MakeSet = fun(Chart) ->
		      Set0 = gb_sets:from_ordset(lists:sort(Chart)),
		      Set = wings_face:extend_border(Set0, We),
		      {gb_sets:size(Set), Set, Chart}
	      end,
    Charts1 = lists:sort([MakeSet(Chart) || Chart <- Charts0]),
    reuse_materials(lists:reverse(Charts1), We, Size, []).

reuse_materials([Chart|Rest], We, Size0, Done)
  when Size0 > 9 ->
    {Next, Combined, Size} =
	reuse_materials_1(Chart, Rest, Size0, We, []),
    reuse_materials(Next, We, Size, [Combined|Done]);
reuse_materials(Rest, _, _, Done) ->
    Done ++ [Fs || {_, _, Fs} <- Rest].

reuse_materials_1(Orig={_, Outer, Fs0}, [Keep = {_, Chart,Fs1}|Rest],
		    Size, We, Acc) ->
    case gb_sets:is_disjoint(Outer, Chart) of
	true ->
	    Border = gb_sets:union(Outer, Chart),
	    reuse_materials_1({0,Border,Fs0++Fs1}, Rest, Size-1, We, Acc);
	false ->
	    reuse_materials_1(Orig, Rest, Size, We, [Keep|Acc])
    end;
reuse_materials_1({_, _, Combined}, [], Size, _We, Acc) ->
    {lists:reverse(Acc), Combined, Size}.
