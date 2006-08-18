%%
%%  wpc_weld --
%%
%%     Plug-in for vertex weld
%%
%%  Copyright (c) 2006 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_weld.erl,v 1.2 2006/06/30 20:26:34 giniu Exp $
%%
-module(wpc_weld).

-include("wings.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [member/2,foldl/3]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({vertex},Menu) ->
    parse(Menu,[],false);
menu(_,Menu) -> Menu.

parse([],MenuNew,true) ->
    MenuNew;
parse([],MenuNew,false) ->
    MenuNew ++ [separator,draw_menu()];
parse([{_,dissolve,_}=Diss|Rest],MenuNew,_) ->
    MenuNew2 = MenuNew ++ [Diss],
    [Now|Rest2] = Rest,
    case Now of
        {_,collapse,_} -> 
            MenuNew3 = MenuNew2 ++ [Now] ++ draw_menu();
        _ ->
            MenuNew3 = MenuNew2 ++ draw_menu() ++ [Now]
    end,
    parse(Rest2,MenuNew3,true);
parse([{_,collapse,_}=Coll|Rest],MenuNew,_) ->
    MenuNew2 = MenuNew ++ [Coll] ++ draw_menu(),
    parse(Rest,MenuNew2,true);
parse([First|Rest],MenuNew,State) ->
    MenuNew2 = MenuNew ++ [First],
    parse(Rest,MenuNew2,State).

draw_menu() -> 
    [{?__(1,"Weld"),weld,
      ?__(2,"Weld selected vertex to another (sharing common edge)")}].

command({vertex,weld}, St) ->
    weld(St);
command(_,_) -> next.

%%
%% Weld one vertex to other
%%

weld(#st{sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vertices = gb_trees:size(We#we.vp),
   Mirror = We#we.mirror,
   if
      Mirror /= none -> 
         Verts = wings_face:vertices_cw(Mirror,We),
         case member(Vert,Verts) of
            true ->
               wings_u:error(?__(1,"You cannot weld at mirror plane")),
               St;
            _ -> ok
         end;
      true -> ok
   end,
   if
      Vertices < 4 -> 
         wings_u:error(?__(2,"Object must have at least 4 vertices")),
         St;
      true ->
         wings:ask(weld_select(St), St, fun weld/2)
   end;
weld(St) ->
   wings_u:error(?__(3,"You can weld only one vertex")),
   St.

weld_select(OrigSt) ->
    Desc = ?__(1,"Select target vertex for weld operation (both verticies must share a common edge) "),
    Fun = fun(check, St) -> weld_check_selection(St, OrigSt);
	     (exit, {_,_,#st{sel=Vert2}=St}) ->
		  case weld_check_selection(St, OrigSt) of
		      {_,[]} -> {[],[Vert2]};
		      {_,_} -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[vertex]}.

weld_check_selection(#st{sel=[{_Obj,{1,{Vert2,_,_}}}]},#st{sel=[{_Obj,{1,{Vert1,_,_}}}]}=St) ->
   if
      Vert1==Vert2 -> {none,?__(1,"You cannot weld vertex to itself")};
      true -> 
         St2=wings_sel_conv:mode(vertex,St),
         [{_,Sel2}]=St2#st.sel,
         CanDo = gb_sets:is_element(Vert2,Sel2),
         if
            CanDo -> {none,""};
            true -> {none,?__(2,"Vertices you want weld must share edge")}
         end
   end;
weld_check_selection(#st{sel=[{_Obj,_}]},#st{sel=[{_Obj,_}]}) ->
   {none,?__(3,"You can weld to only one point")};
weld_check_selection(#st{sel=[]},_) ->
   {none,?__(4,"Nothing selected")};
weld_check_selection(_,_) ->
   {none,?__(5,"You can weld only in same object")}.

weld([{_,{1,{Vert2,_,_}}}]=NewSel,#st{sel=[{Obj,{1,{Vert1,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   {RemoveEdge,LF,RF,FixMe} = get_edge_info(Vert1,Vert2,We),
   NewVp = gb_trees:delete(Vert1,We#we.vp),
   NewEs = fix_edge(Vert1,Vert2,RemoveEdge,FixMe,LF,RF,We),
   NewMat = fix_mat(FixMe,We),
   NewHe = fix_hardedge(NewEs, We#we.he),
   NewWe = wings_we:rebuild(We#we{vp=NewVp, es=NewEs, he=NewHe, vc=undefined, fs=undefined, mat=NewMat}),
   NewShs = gb_trees:update(Obj,NewWe,Shs),
   {save_state,St#st{shapes=NewShs,sel=NewSel}}.

get_edge_info(Vert1,Vert2,We) ->
   [{RemoveEdge,LF,RF}] = wings_vertex:edge_through(Vert1, Vert2, We),
   NLF = wings_face:vertices(LF, We),
   NRF = wings_face:vertices(RF, We),
   if
      NLF < 4 -> FixMe0 = [LF];
      true -> FixMe0 = []
   end,
   if
      NRF < 4 -> FixMe = [RF|FixMe0];
      true -> FixMe = FixMe0
   end,
   {RemoveEdge,LF,RF,FixMe}.

fix_edge(Vert1,Vert2,RemoveEdge,FixMe,LF,RF,We) ->
   New = gb_trees:empty(),
   ABTransform = calculate_ab(Vert1,Vert2,LF,RF,We),
   Etab = fix_edge_1(Vert1,Vert2,RemoveEdge,ABTransform,We#we.es,We,New),
   foldl(fun(Face,ParseEtab) -> remove_face(Face,ParseEtab) end,Etab,FixMe).

fix_edge_1(Vert1,Vert2,RemoveEdge,ABTransform,Es,Orig,Result) ->
   case gb_trees:size(Es) of
      0 -> Result;
      _ ->
         {Key,#edge{vs=V1,ve=V2,a=C1,b=C2,lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS},Es2} = gb_trees:take_smallest(Es),
         if
            Key == RemoveEdge -> fix_edge_1(Vert1,Vert2,RemoveEdge,ABTransform,Es2,Orig,Result);
            true ->
               #edge{lf=OLF,rf=ORF,ltpr=OLP,ltsu=OLS,rtpr=ORP,rtsu=ORS} = gb_trees:get(RemoveEdge,Orig#we.es),
               case V1 of
                  Vert1 -> NV1=Vert2,
                           NC1=transformAB(C1,ABTransform);
                  _ -> NV1=V1,
                       NC1=C1
               end,
               case V2 of
                  Vert1 -> NV2=Vert2,
                           NC2=transformAB(C2,ABTransform);
                  _ -> NV2=V2,
                       NC2=C2
               end,
               case LP of
                  RemoveEdge ->
                     if
                        LF == OLF -> NLP = OLP;
                        true -> NLP = ORP
                     end;
                  _ -> NLP = LP
               end,
               case LS of
                  RemoveEdge ->
                     if
                        LF == OLF -> NLS = OLS;
                        true -> NLS = ORS
                     end;
                  _ -> NLS = LS
               end,
               case RP of
                  RemoveEdge ->
                     if
                        RF == ORF -> NRP = ORP;
                        true -> NRP = OLP
                     end;
                  _ -> NRP = RP
               end,
               case RS of
                  RemoveEdge ->
                     if
                        RF == ORF -> NRS = ORS;
                        true -> NRS = OLS
                     end;
                  _ -> NRS = RS
               end,
               NewEdge = #edge{vs=NV1,ve=NV2,a=NC1,b=NC2,lf=LF,rf=RF,ltpr=NLP,ltsu=NLS,rtpr=NRP,rtsu=NRS},
               Result2 = gb_trees:insert(Key,NewEdge,Result),
               fix_edge_1(Vert1,Vert2,RemoveEdge,ABTransform,Es2,Orig,Result2)
         end
   end.

fix_mat(FixMe,We) ->
   NewMat = foldl(fun(Face,ParseWe) -> wings_facemat:delete_face(Face,ParseWe) end,We,FixMe),
   NewMat#we.mat.

fix_hardedge(Etab,He) ->
   New = gb_sets:empty(),
   fix_hardedge(Etab,He,New).

fix_hardedge(Etab,He,Result) ->
   case gb_sets:size(He) of
      0 -> Result;
      _ -> 
         {Edge,He2} = gb_sets:take_smallest(He),
         case gb_trees:is_defined(Edge, Etab) of
            true -> Result2 = gb_sets:add(Edge,Result);
            _ -> Result2 = Result
         end,
         fix_hardedge(Etab,He2,Result2)
   end.

remove_face(Face,Etab) ->
   [Key1,Key2]=find_edge(Face,Etab),
   OldEdge = gb_trees:get(Key1,Etab),
   NewEdge = gb_trees:get(Key2,Etab),
   K1 = find_edge_to_face(OldEdge,Face),
   K2 = find_edge_to_face(NewEdge,Face),
   if
      K1 == K2 ->
         if
            K1 == left ->
               NewEdge2=NewEdge#edge{a=OldEdge#edge.b, lf=OldEdge#edge.rf, ltpr=OldEdge#edge.rtpr, ltsu=OldEdge#edge.rtsu};
            true ->
               NewEdge2=NewEdge#edge{b=OldEdge#edge.a, rf=OldEdge#edge.lf, rtpr=OldEdge#edge.ltpr, rtsu=OldEdge#edge.ltsu}
         end;
      true ->
         if
            K1 == left ->
               NewEdge2=NewEdge#edge{b=OldEdge#edge.b, rf=OldEdge#edge.rf, rtpr=OldEdge#edge.rtpr, rtsu=OldEdge#edge.rtsu};
            true ->
               NewEdge2=NewEdge#edge{a=OldEdge#edge.a, lf=OldEdge#edge.lf, ltpr=OldEdge#edge.ltpr, ltsu=OldEdge#edge.ltsu}
         end
   end,
   Etab2 = gb_trees:update(Key2,NewEdge2,Etab),
   Etab3 = gb_trees:delete(Key1,Etab2),
   substitute(Key1,Key2,Etab3).

substitute(This,WithThis,Etab) ->
   New = gb_trees:empty(), 
   substitute(This,WithThis,Etab,New).

substitute(This,WithThis,Etab,New) ->
   case gb_trees:size(Etab) of 
      0 -> New;
      _ ->
         {Key,Edge,Etab2} = gb_trees:take_smallest(Etab),
         NewEdge = substitute_1(This,WithThis,Edge),
         New2 = gb_trees:insert(Key,NewEdge,New),
         substitute(This,WithThis,Etab2,New2)
   end.

substitute_1(This,WithThis,Edge) ->
   if
      Edge#edge.rtpr == This -> Rtpr = WithThis;
      true -> Rtpr = Edge#edge.rtpr
   end,
   if
      Edge#edge.rtsu == This -> Rtsu = WithThis;
      true -> Rtsu = Edge#edge.rtsu
   end,
   if
      Edge#edge.ltpr == This -> Ltpr = WithThis;
      true -> Ltpr = Edge#edge.ltpr
   end,
   if
      Edge#edge.ltsu == This -> Ltsu = WithThis;
      true -> Ltsu = Edge#edge.ltsu
   end,
   Edge#edge{ltsu=Ltsu, ltpr=Ltpr, rtsu=Rtsu, rtpr=Rtpr}.

find_edge(Face,Etab) ->
   find_edge(Face,Etab,[]).

find_edge(Face,Es,Result) ->
   case gb_trees:size(Es) of
      0 -> Result;
      _ ->
         {Key,#edge{lf=LF,rf=RF},Es2} = gb_trees:take_smallest(Es),
         if
            (LF == Face) or (RF == Face) -> Result2=[Key|Result];
            true -> Result2=Result
         end,
         find_edge(Face,Es2,Result2)
   end.

find_edge_to_face(#edge{lf=Face},Face) -> left;
find_edge_to_face(#edge{rf=Face},Face) -> right;
find_edge_to_face(_,_) -> none.

calculate_ab(VA,VB,FA,FB,We) ->
   VA1 = calculate_ab_1(VA,FA,We),
   VB1 = calculate_ab_1(VB,FA,We),
   VA2 = calculate_ab_1(VA,FB,We),
   VB2 = calculate_ab_1(VB,FB,We),
   [{VA1,VB1},{VA2,VB2}].

calculate_ab_1(Vertex,Face,We) ->
   Edge = gb_trees:get(Face,We#we.fs),
   calculate_ab_2(Vertex,Face,Edge,Edge,We#we.es,nil).

calculate_ab_2(_,_,Edge,Edge,_,Value) when (Value =/= nil) -> Value;
calculate_ab_2(_,_,_,_,_,Value) when ((Value =/= nil) and (Value =/= none)) -> Value;
calculate_ab_2(Vertex,Face,Edge,LastEdge,Etab,_) ->
   case catch gb_trees:get(Edge,Etab) of
      #edge{vs=Vertex,a=Value,lf=Face,ltsu=Next} ->
         calculate_ab_2(Vertex,Face,Next,LastEdge,Etab,Value);
      #edge{ve=Vertex,b=Value,rf=Face,rtsu=Next} ->
         calculate_ab_2(Vertex,Face,Next,LastEdge,Etab,Value);
      #edge{rf=Face,rtsu=Next} ->
         calculate_ab_2(Vertex,Face,Next,LastEdge,Etab,none);
      #edge{lf=Face,ltsu=Next} ->
         calculate_ab_2(Vertex,Face,Next,LastEdge,Etab,none)
   end.

transformAB(Value,[]) -> Value;
transformAB(_Value,[{_Value,To}|_]) -> To;
transformAB(Value,[{_,_}|Rest]) ->
   transformAB(Value,Rest).
