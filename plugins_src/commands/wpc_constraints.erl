%%
%%  wpc_constraints.erl --
%%
%%    Plugin for setting default constraints directly from a model
%%
%%  Copyright (c) 2008-2013 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%    $Id: wpc_constraints.erl optigon Exp $
%%

-module(wpc_constraints).
-export([init/0,menu/2,command/2]).
-define(NEED_ESDL, 1).
-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-define(NONZERO, 1.0e-6).

init() ->
    true.

menu({Mode},Menu) when Mode == vertex; Mode == edge; Mode == face ->
    lists:reverse(parse(Menu,Mode,[],false));
menu(_,Menu) -> Menu.


parse([], _, NewMenu, true) ->
    NewMenu;

parse([], Mode, NewMenu, false) ->
    [general_menu(Mode), separator|NewMenu];

parse([A={_,vertex_color,_}|Rest], Mode, NewMenu, false) ->
    parse(Rest, Mode, [general_menu(Mode),separator,A|NewMenu], true);

parse([Elem|Rest], Mode, NewMenu, Found) ->
    parse(Rest, Mode, [Elem|NewMenu], Found).


general_menu(Mode) ->
    MenuTitle = ?__(1,"Set Constraint"),
    {MenuTitle,{set_constraint,first_menu(Mode)}}.

%%% Menu
first_menu(vertex) ->
    [general_menu1(vertex,center)];

first_menu(edge) ->
    [general_menu1(edge,total),
     general_menu1(edge,average),
     separator,
     general_menu1(edge,angle),
     general_menu1(edge,sub_angle),
     general_menu1(edge,to_axis),
     separator,
     general_menu1(edge,scale),
     general_menu1(edge,diff),
     general_menu1(edge,center)];

first_menu(face) ->
    [general_menu1(face,center),
     general_menu1(face,scale),
     separator,
     general_menu1(face,angle),
     general_menu1(face,sub_angle),
     general_menu1(face,to_axis)].

general_menu1(Mode,Type) ->
    {menu_title(Mode,Type),{Type,menu_items1(Mode,Type)},menu_string1(Mode,Type)}.

general_menu2(Mode,Type,Axis) ->
    RmbStr = ?__(1,"Choose a different axis to measure the second selection"),
    F = fun(help, _Ns) ->
		{menu_string2(Mode,Type,Axis)++mod_string(),[],RmbStr};
	   (1,_Ns) -> {Mode,{set_constraint,{Type,{Axis,none}}}};
	   (2,_Ns) -> ignore;
	   (3,_Ns) -> menu_items2(Mode,Type,Axis)
        end,
    {menu_heading(Mode,Type,Axis),{Axis,F},[]}.

general_menu3(Mode,Type,Axis1,Axis2) ->
    Str = menu_string3(advanced,Mode,Type,Axis1,Axis2),
    F = {Mode,{set_constraint,{Type,{Axis1,Axis2}}}},
    {menu_heading(Mode,Type,Axis2),{Axis2,F},Str++mod_string()}.

%%% Menu Strings
menu_title(Mode,Type) ->
    case {Mode,Type} of
      {Mode,total} -> ?__(1,"Total Length");
      {Mode,average} -> ?__(2,"Average Length");
      {Mode,angle} -> ?__(3,"Angle");
      {Mode,sub_angle} -> ?__(4,"Subtract Angle");
      {edge,to_axis} -> ?__(5,"Edge To Axis");
      {face,to_axis} -> ?__(6,"Face To Axis");
      {Mode,scale} -> ?__(7,"Percentage");
      {Mode,diff} -> ?__(8,"Difference");
      {Mode,center} -> ?__(9,"Centers")
    end.

menu_heading(Mode,Type,Axis) ->
    case {Mode,Type,Axis} of
      {Mode,angle,normal} -> ?__(4,"Angle");
      {Mode,center,normal} -> ?__(3,"Direct");
      {Mode,Type,normal} -> ?__(1,"Normal");
      {Mode,Type,'ASK'} -> ?__(2,"Pick");
      {Mode,sub_angle,fifteen} -> [?__(5,"15"),?DEGREE];
      {Mode,sub_angle,twenty_two} -> [?__(6,"22.5"),?DEGREE];
      {Mode,sub_angle,thirty} -> [?__(7,"30"),?DEGREE];
      {Mode,sub_angle,forty_five} -> [?__(8,"45"),?DEGREE];
      {Mode,sub_angle,sixty} -> [?__(9,"60"),?DEGREE];
      {Mode,sub_angle,ninety} -> [?__(10,"90"),?DEGREE];
      {Mode,Type,Axis} -> wings_s:dir(Axis)
    end.
mode_strings(Ending,Mode) ->
    case {Ending,Mode} of
      {plural,vertex} -> ?__(1,"vertices");
      {plural,edge} -> ?__(2,"edges");
      {plural,face} -> ?__(3,"faces");
      {singular,vertex} -> ?__(4,"vertex");
      {singular,edge} -> ?__(5,"edge");
      {singular,face} -> ?__(6,"face");
      {second,Mode} -> ?__(8,"second");
      {units,vertex} -> ?__(9,"distance");
      {units,edge} -> ?__(10,"lengths");
      {units,face} -> ?__(11,"areas")
    end.

mod_string() ->
    ?__(1,". Constraint bound to held modifier key(s).").

%%%% Help Strings
menu_string1(Mode,Type) ->
          UnitStr = mode_strings(units,Mode),
          SMdeStr = mode_strings(singular,Mode),
          PMdeStr = mode_strings(plural,Mode),
    case {Mode,Type} of
      {edge,total} ->
          ?__(1,"Total the lengths of the selected edges and save the result as a distance constraint in the Preferences");
      {edge,average} ->
          ?__(2,"Calculate the average length of the selected edges and save the result as a distance constraint in the Preferences");
      {Mode,angle} ->
          Str = ?__(3,"Calculate the angle between any two ~s and save the result as a rotation constraint in the Preferences"),
          wings_util:format(Str,[PMdeStr]);
      {Mode,sub_angle} ->
          ?__(4,"Calculate the differnce between two angles and save the result as a rotation constraint in the Preferences");
      {Mode,to_axis} ->
          Str = ?__(5,"Measure the angle between a single ~s and a standard axis or defined vector. Save the result as a rotation constraint in the Preferences"),
          wings_util:format(Str,[SMdeStr]);
      {Mode,scale} ->
          Str = ?__(6,"Calculate the difference in scale between the ~s of two ~s selections and save the result as a scale constraint in the Preferences"),
          wings_util:format(Str,[UnitStr,SMdeStr]);
      {Mode,diff} ->
          Str = ?__(7,"Calculate the difference in ~s between two ~s selections and save the result as a distance constraint in the Preferences"),
          wings_util:format(Str,[UnitStr,SMdeStr]);
      {Mode,center} ->
          ?__(8,"Calculate the distance between the center points of two selections and save the result as a distance constraint in the Preferences")
    end.

menu_string2(Mode,Type,Axis) ->
    UnitStr = mode_strings(units,Mode),
    SMdeStr = mode_strings(singular,Mode),
    PMdeStr = mode_strings(plural,Mode),
    AxisStr = menu_heading(edge,Type,Axis),
    case {Mode,Type,Axis} of
      {Mode,scale,normal} ->
          Str = ?__(1,"Measure the ~s of both selections according to their ~s normals"),
          wings_util:format(Str,[UnitStr,SMdeStr]);
      {Mode,scale,'ASK'} ->
          Str = ?__(2,"Pick an axis along which to measure the ~s both selections"),
          wings_util:format(Str,[UnitStr]);
      {Mode,scale,Axis} ->
          Str = ?__(3,"Measure the ~s of both selections' ~s only along the ~s axis"),
          wings_util:format(Str,[UnitStr,PMdeStr,AxisStr]);
      {Mode,diff,normal} ->
          Str = ?__(1,"Measure the ~s of both selections according to their ~s normals"),
          wings_util:format(Str,[UnitStr,SMdeStr]);
      {Mode,diff,'ASK'} ->
          Str = ?__(2,"Pick an axis along which to measure the ~s both selections"),
          wings_util:format(Str,[UnitStr]);
      {Mode,diff,Axis} ->
          Str = ?__(3,"Measure the ~s of both selections' ~s only along the ~s axis"),
          wings_util:format(Str,[UnitStr,PMdeStr,AxisStr])
    end.

menu_string3(advanced,Mode,Type,Axis1,Axis2) ->
    UnitStr = mode_strings(units,Mode),
    PMdeStr = mode_strings(plural,Mode),
    SMdeStr = mode_strings(singular,Mode),
    SSelStr = mode_strings(second,Mode),
    Axs2Str = menu_heading(Mode,Type,Axis2),
    DegStr = menu_heading(Mode,sub_angle,Axis2),
    case {Mode,Type,Axis1,Axis2} of
      {Mode,total,none,normal} ->
          ?__(1,"Measure the selected edges along their normals");
      {Mode,total,none,'ASK'} ->
          ?__(2,"Pick an axis along which to measure the selected edges");
      {Mode,total,none,Axis2} ->
          Str = ?__(3,"Measure the selected edges only along the ~s axis"),
          wings_util:format(Str,[Axs2Str]);

      {Mode,average,none,normal} ->
          ?__(1,"Measure the selected edges along their normals")++
          ?__(22," and then caluculate their average length");
      {Mode,average,none,'ASK'} ->
          ?__(2,"Pick an axis along which to measure the selected edges")++
          ?__(22," and then caluculate their average length");
      {Mode,average,none,Axis2} ->
          Str = ?__(3,"Measure the selected edges only along the ~s axis"),
          wings_util:format(Str,[Axs2Str])++?__(22," and then caluculate their average length");

      {Mode,angle,none,normal} ->
          ?__(4,"Measure the selected angle");
      {Mode,angle,none,'ASK'} ->
          ?__(5,"Specify the axis from which to measure the selected angle");
      {Mode,angle,none,Axis2} ->
          Str = ?__(6,"Measure the selected angle as viewed from the ~s axis"),
          wings_util:format(Str,[Axs2Str]);

      {Mode,sub_angle,none,'ASK'} ->
          ?__(19,"Pick a second angle from which to subtract the currently selected angle");
      {Mode,sub_angle,none,Axis2} ->
          Str = ?__(18,"Subtract the currently selected angle from ~s"),
          wings_util:format(Str,[DegStr]);

      {Mode,to_axis,none,'ASK'} ->
          Str = ?__(20,"Pick a vector and calculate the angle to the original ~s"),
          wings_util:format(Str,[SMdeStr]);
      {Mode,to_axis,none,Axis2} ->
          Str = ?__(21,"Calculate the angle from the ~s axis to the original ~s"),
          wings_util:format(Str,[Axs2Str,SMdeStr]);

      {Mode,center,none,normal} ->
          ?__(7,"Measure the distance between the centers of the two selections");
      {Mode,center,none,'ASK'} ->
          ?__(8,"Pick an axis along which to measure the distance between the centers of the two selections");
      {Mode,center,none,Axis2} ->
          Str = ?__(9,"Measure only the distance along the ~s axis between the centers of the two selections"),
          wings_util:format(Str,[Axs2Str]);
      {Mode,Type,Axis1,normal} ->
          Str = ?__(13,"Measure the ~s of the ~s selection's ~s along their normals"),
          wings_util:format(Str,[UnitStr,SSelStr,PMdeStr]);
      {Mode,Type,Axis1,'ASK'} ->
          Str = ?__(11,"Pick an axis along which to measure the ~s of the ~s selection's ~s"),
          wings_util:format(Str,[UnitStr,SSelStr,PMdeStr]);
      {Mode,Type,Axis1,Axis2} ->
          Str = ?__(12,"Measure the ~s of the ~s selection's ~s only along the ~s axis"),
          wings_util:format(Str,[UnitStr,SSelStr,PMdeStr,Axs2Str])
    end.

%%% Menu items
menu_items1(Mode,Type) ->
    case Type of
      total -> last_menu(Mode,Type,none);
      average -> last_menu(Mode,Type,none);
      angle -> last_menu(Mode,Type,none);
      sub_angle -> last_menu(Mode,Type,none);
      to_axis -> last_menu(Mode,Type,none);
      scale -> middle_menu(Mode,Type);
      diff ->  middle_menu(Mode,Type);
      center ->last_menu(Mode,Type,none)
    end.

menu_items2(Mode,Type,Axis) ->
    case Type of
      scale -> last_menu(Mode,Type,Axis);
      diff -> last_menu(Mode,Type,Axis)
    end.

middle_menu(Mode,Type) ->
    case Type of
      Type ->
        [general_menu2(Mode,Type,normal),
         general_menu2(Mode,Type,x),
         general_menu2(Mode,Type,y),
         general_menu2(Mode,Type,z),
         general_menu2(Mode,Type,'ASK')]
    end.

last_menu(Mode,Type,Axis) ->
    case Type of
      sub_angle ->
        [general_menu3(Mode,sub_angle,none,fifteen),
         general_menu3(Mode,sub_angle,none,twenty_two),
         general_menu3(Mode,sub_angle,none,thirty),
         general_menu3(Mode,sub_angle,none,forty_five),
         general_menu3(Mode,sub_angle,none,sixty),
         general_menu3(Mode,sub_angle,none,ninety),
         general_menu3(Mode,sub_angle,none,'ASK')];
      to_axis ->
        [general_menu3(Mode,Type,none,x),
         general_menu3(Mode,Type,none,y),
         general_menu3(Mode,Type,none,z),
         general_menu3(Mode,Type,none,'ASK')];
      Type ->
        [general_menu3(Mode,Type,Axis,normal),
         general_menu3(Mode,Type,Axis,x),
         general_menu3(Mode,Type,Axis,y),
         general_menu3(Mode,Type,Axis,z),
         general_menu3(Mode,Type,Axis,'ASK')]
    end.

%%%
%%% Commands
%%%

%%% Length
command({edge,{set_constraint,{total,{none,'ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis]), St, fun distance/2);
command({edge,{set_constraint,{total,{none,Axis}}}}, St) ->
    distance(Axis,St);

%%% Average
command({edge,{set_constraint,{average,{none,'ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis]), St, fun average/2);
command({edge,{set_constraint,{average,{none,Axis}}}}, St) ->
    average(Axis,St);

%%% Angle
command({_Mode,{set_constraint,{angle,{none,'ASK'}}}},St) ->
    check_angle_sel(St),
    wings:ask(selection_ask([view_plane]), St, fun angle/2);
command({_Mode,{set_constraint,{angle,{none,Axis}}}}, St) ->
    check_angle_sel(St),
    angle(Axis,St);

%%% Sub Angle
command({_Mode,{set_constraint,{sub_angle,{none,'ASK'}}}},St) ->
    check_angle_sel(St),
    wings:ask(secondary_sel_ask(sub_angle,none,none,St), St, fun sub_angle/2);
command({_Mode,{set_constraint,{sub_angle,{none,Axis}}}}, St) ->
    check_angle_sel(St),
    sub_angle(Axis,St);

%%% Angle to Axis
command({_Mode,{set_constraint,{to_axis,{none,'ASK'}}}},St) ->
    check_element(St),
    wings:ask(selection_ask([to_axis]), St, fun to_axis/2);
command({_Mode,{set_constraint,{to_axis,{none,Axis2}}}},St) ->
    check_element(St),
    to_axis(Axis2,St);

%%% Scale Edge Mode
command({edge,{set_constraint,{scale,{'ASK','ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis1,along_axis2]), St,
      fun({Pn0,Pn1},St0) ->
          scale({Pn0,Pn1},St0)
    end);
command({edge,{set_constraint,{scale,{'ASK',none}}}},St) ->
    wings:ask(selection_ask([along_axis1]), St, fun(Pn1,St0) ->
        scale({Pn1,Pn1},St0)
    end);
command({edge,{set_constraint,{scale,{'ASK',Axis2}}}},St) ->
    wings:ask(selection_ask([along_axis1]), St, fun({Pn1},St0) ->
        scale({Pn1,Axis2},St0)
    end);
command({edge,{set_constraint,{scale,{Axis1,'ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis2]), St, fun({Pn1},St0) ->
        scale({Axis1,Pn1},St0)
    end);
command({edge,{set_constraint,{scale,{Axis1,none}}}},St) ->
    scale({Axis1,Axis1},St);
command({edge,{set_constraint,{scale,{Axis1,Axis2}}}},St) ->
    scale({Axis1,Axis2},St);
command({edge,{set_constraint,{scale,_}}},St) ->
    wings:ask(selection_ask([along_axis1,along_axis2]), St,
      fun({Pn0,Pn1},St0) ->
          scale({Pn0,Pn1},St0)
    end);

%%% Scale Face Mode
command({face,{set_constraint,{scale,{'ASK','ASK'}}}},St) ->
    wings:ask(selection_ask([scale_ax_pnt1,scale_ax_pnt2]), St,
      fun({Pn0,Pp0,Pn1,Pp1},St0) ->
          scale({{Pn0,Pp0},{Pn1,Pp1}},St0)
    end);
command({face,{set_constraint,{scale,{'ASK',none}}}},St) ->
    wings:ask(selection_ask([scale_ax_pnt1]), St, fun({Pn1,Pp1},St0) ->
        scale({{Pn1,Pp1},{Pn1,Pp1}},St0)
    end);
command({face,{set_constraint,{scale,{'ASK',Axis2}}}},St) ->
    wings:ask(selection_ask([scale_ax_pnt1]), St, fun({Pn1,Pp1},St0) ->
        scale({{Pn1,Pp1},Axis2},St0)
    end);
command({face,{set_constraint,{scale,{Axis1,'ASK'}}}},St) ->
    wings:ask(selection_ask([scale_ax_pnt2]), St, fun({Pn1,Pp1},St0) ->
        scale({Axis1,{Pn1,Pp1}},St0)
    end);
command({face,{set_constraint,{scale,{Axis1,none}}}},St) ->
    scale({Axis1,Axis1},St);
command({face,{set_constraint,{scale,{Axis1,Axis2}}}},St) ->
    scale({Axis1,Axis2},St);
command({face,{set_constraint,{scale,_}}},St) ->
    wings:ask(selection_ask([scale_ax_pnt1,scale_ax_pnt2]), St,
      fun({Pn0,Pp0,Pn1,Pp1},St0) ->
          scale({{Pn0,Pp0},{Pn1,Pp1}},St0)
    end);

%%% Difference
command({edge,{set_constraint,{diff,{'ASK','ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis1,along_axis2]), St,
      fun({Axis1,Axis2},St0) ->
          difference({Axis1,Axis2},St0)
    end);
command({edge,{set_constraint,{diff,{'ASK',none}}}},St) ->
    wings:ask(selection_ask([along_axis1]), St, fun(Axis1,St0) ->
        difference({Axis1,Axis1},St0)
    end);
command({edge,{set_constraint,{diff,{'ASK',Axis2}}}},St) ->
    wings:ask(selection_ask([along_axis1]), St, fun(Axis1,St0) ->
        difference({Axis1,Axis2},St0)
    end);
command({edge,{set_constraint,{diff,{Axis1,'ASK'}}}},St) ->
    wings:ask(selection_ask([along_axis2]), St, fun(Axis2,St0) ->
        difference({Axis1,Axis2},St0)
    end);
command({edge,{set_constraint,{diff,{Axis1,none}}}},St) ->
    difference({Axis1,Axis1},St);
command({edge,{set_constraint,{diff,{Axis1,Axis2}}}},St) ->
    difference({Axis1,Axis2},St);

%%% Centers
command({_Mode,{set_constraint,{center,{none,'ASK'}}}},St) ->
    wings:ask(selection_ask([centers]), St, fun centers/2);
command({_Mode,{set_constraint,{center,{none,Axis}}}}, St) ->
    centers(Axis,St);

command(_,_) -> next.

%%% Axis Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) ->
    lists:reverse(Ask);

selection_ask([Type|Rest],Ask) ->
    {Data,Desc} = case Type of
      along_axis ->
        {axis,?__(1,"Choose the axis each edge will be measured along")};
      along_axis1 ->
        {axis,?__(1,"Choose the axis each edge will be measured along")
            ++?__(2," for the first selection")};
      along_axis2 ->
        {axis,?__(1,"Choose the axis each edge will be measured along")
            ++?__(3," for the second selection")};
      view_plane ->
        {axis_point,?__(4,"Choose the axis along which the angle will be measured orthographically")};
      centers ->
        {axis,?__(5,"Choose an axis for measuring the distance between the centers of the selections")};
      scale_ax_pnt1 ->
        {axis_point,?__(6,"Choose an axis along which the areas of the first selection will be measure orthographically")};
      scale_ax_pnt2 ->
        {axis_point,?__(7,"Choose an axis along which the areas of the second selection will be measure orthographically")};
      to_axis ->
        {axis,?__(8,"Choose vector to calculate the angle to the to the original selection")}
    end,
    selection_ask(Rest,[{Data,Desc}|Ask]).

%%% Secondary selection
secondary_sel_ask(sub_angle,none,none,OrigSt) ->
    Desc = ?__(5,"Select an angle made from two edges or two faces to subtract the original angle from"),
    Data = fun(check, St) -> check_selection(sub_angle,none,none,St,OrigSt);
             (exit, {_,_,St}) ->
               case check_selection(sub_angle,none,none,St,OrigSt) of
                 {_," "++_} -> {[],[St]};
                 {_,_} -> error
               end
             end,
    {[{Data,Desc}],[],[],[edge,face]};
secondary_sel_ask(scale_area,Axis1,Axis2,OrigSt) ->
    Desc = ?__(4,"Select the faces whose total area will be divided into the first selection's area"),
    Data = fun(check, St) -> check_selection(scale_area,Axis1,Axis2,St,OrigSt);
             (exit, {_,_,St}) ->
               case check_selection(scale_area,Axis1,Axis2,St,OrigSt) of
                 {_," "++_} -> {[],[St]};
                 {_,_} -> error
               end
             end,
    {[{Data,Desc}],[],[],[face]};

secondary_sel_ask(center,Axis1,Axis2,OrigSt) ->
    Desc = ?__(3,"Center of selection will determine the distance from the center of the original selection"),
    Data = fun(check, St) -> check_selection(center,Axis1,Axis2,St,OrigSt);
             (exit, {_,_,St}) ->
               case check_selection(center,Axis1,Axis2,St,OrigSt) of
                 {_," "++_} -> {[],[St]};
                 {_,_} -> error
               end
             end,
    {[{Data,Desc}],[],[],[edge,vertex,face,body]};

secondary_sel_ask(Type,Axis1,Axis2,OrigSt) ->
    Desc = case Type of
      scale -> ?__(1,"Select the edges to divide into the original selection");
      difference -> ?__(2,"Select the edges to subtract from the original selection")
    end,
    Data = fun(check, St) -> check_selection(Type,Axis1,Axis2,St,OrigSt);
             (exit, {_,_,St}) ->
               case check_selection(Type,Axis1,Axis2,St,OrigSt) of
                 {_," "++_} -> {[],[St]};
                 {_,_} -> error
               end
             end,
    {[{Data,Desc}],[],[],[edge]}.

check_selection(_Type,_Axis1,_Axis2,#st{sel=[]},_OrigSt) ->
    {none,?__(1,"Nothing selected")};

check_selection(sub_angle,none,none,#st{sel=[{_Id0,Sel0},{_Id1,Sel1}]}=St,OrigSt) ->
    case gb_sets:size(Sel0) == 1 andalso gb_sets:size(Sel1) == 1 of
      true ->
        OrigA = measure_angle(normal,OrigSt),
        Angle = measure_angle(normal,St),
        A0 = abs(Angle - OrigA),
        A1 = case A0 of
               0.0 -> 180.0;
               _ -> A0
             end,
        Str = [?__(11," Original Angle ~s"),?DEGREE,?__(12,"\n Current Angle ~s"),
               ?DEGREE,?__(13,"\n Difference ~s"),?DEGREE],
        {none,wings_util:format(Str,[OrigA,Angle,A1])};
      false ->
        {none,?__(14,"Select exactly two edges or two faces to define angle")}
    end;

check_selection(sub_angle,none,none,#st{sel=[{_Id,Sel}]}=St,OrigSt) ->
    case gb_sets:size(Sel) == 2 of
      true ->
        Angle = measure_angle(normal,OrigSt),
        CAngle = measure_angle(normal,St),
        OrigA = wings_util:nice_float(Angle),
        CurrA = wings_util:nice_float(CAngle),
        A0 = abs(Angle - CAngle),
        A1 = case A0 of
               0.0 -> 180.0;
               _ -> A0
             end,
        A2 = wings_util:nice_float(A1),
        Str1 = [wings_util:format(?__(16," Angle ~s"),[OrigA]),?DEGREE],
        Str2 = [wings_util:format(?__(12,"\n Current Angle ~s"),[CurrA]),?DEGREE],
        Str3 = [wings_util:format(?__(13,"\n Difference ~s"),[A2]),?DEGREE],
        StrFinal = [Str1++Str2++Str3],
        {none,?__(15," Original")++StrFinal};
      false ->
        {none,?__(14,"Select exactly two edges or two faces to define angle")}
    end;

check_selection(sub_angle,none,none,_St,_OrigSt) ->
    {none,?__(14,"Select exactly two edges or two faces to define angle")};

check_selection(scale,Axis1,Axis2,#st{selmode=edge}=St,OrigSt) ->
    Original = add_edges(Axis1,OrigSt),
    Current = add_edges(Axis2,St),
    case Current < ?NONZERO of
      true ->
        {none,?__(3,"Current length is too short. Select edges that aren't perpendicular to the chosen axis.")};
      false ->
        Percent = Original/Current,
        case Percent < ?NONZERO of
          true ->
            {none,?__(5,"Resulting percentage is to small")};
          false ->
            OStr = wings_util:nice_float(Original),
            PStr = wings_util:nice_float(Percent*100),
            RStr = wings_util:nice_float(1/Percent*100),
            CStr = wings_util:nice_float(Current),
            AxStr1 = axis_to_string(Axis1),
            AxStr2 = axis_to_string(Axis2),
            Str = ?__(4," Original ~s ~s\n Current ~s ~s\n Percent ~s%  Reciprocal ~s%"),
            {none,wings_util:format(Str, [AxStr1,OStr,AxStr2,CStr,PStr,RStr])}
        end
    end;

check_selection(difference,Axis1,Axis2,#st{selmode=edge}=St,OrigSt) ->
    Original = add_edges(Axis1,OrigSt),
    Current = add_edges(Axis2,St),
    Difference = abs(Original - Current),
    case Difference < ?NONZERO of
      true ->
        {none,?__(6,"Difference is too small")};
      false ->
        OStr = wings_util:nice_float(Original),
        CStr = wings_util:nice_float(Current),
        DStr = wings_util:nice_float(Difference),
        AxStr1 = axis_to_string(Axis1),
        AxStr2 = axis_to_string(Axis2),
        Str = ?__(7," Original ~s ~s\n Current ~s ~s\n Difference ~s"),
        {none,wings_util:format(Str,[AxStr1,OStr,AxStr2,CStr,DStr])}
    end;

check_selection(center,Axis1,_Axis2,St,OrigSt) ->
    Original = wings_sel:center(OrigSt),
    Current = wings_sel:center(St),
    Distance = get_distance(Axis1,Original,Current),
    case Distance < ?NONZERO of
      true ->
        {none,?__(8,"Distance between centers is too short")};
      false ->
        OStr = axis_to_string({center,Original}),
        CStr = axis_to_string({center,Current}),
        DStr = wings_util:nice_float(Distance),
        AxStr1 = axis_to_string(Axis1),
        Str = ?__(9," Original center ~s\n Current center ~s\n Distance ~s ~s"),
        {none,wings_util:format(Str,[OStr,CStr,DStr,AxStr1])}
    end;

check_selection(scale_area,Axis1,Axis2,St,OrigSt) ->
    Original = add_areas(Axis1,OrigSt),
    Current = add_areas(Axis2,St),
    case Current < ?NONZERO of
      true ->
        {none,?__(10,"Current area is too small")};
      false ->
        Percent = Original/Current,
        case Percent < ?NONZERO of
          true ->
            {none,?__(5,"Resulting percentage is to small")};
          false ->
            OStr = wings_util:nice_float(Original),
            PStr = wings_util:nice_float(Percent*100),
            RStr = wings_util:nice_float(1/Percent*100),
            CStr = wings_util:nice_float(Current),
            AxStr1 = axis_to_string(Axis1),
            AxStr2 = axis_to_string(Axis2),
            Str = ?__(4," Original ~s ~s\n Current ~s ~s\n Percent ~s%  Reciprocal ~s%"),
            {none,wings_util:format(Str, [AxStr1,OStr,AxStr2,CStr,PStr,RStr])}
        end
    end.

axis_to_string(Axis) ->
    case Axis of
      normal ->
        [];
      {center,{_,_,_}} ->
        {center,{X,Y,Z}} = Axis,
        X1 = wings_util:nice_float(X),
        Y1 = wings_util:nice_float(Y),
        Z1 = wings_util:nice_float(Z),
        Str = "<~s  ~s  ~s>",
        wings_util:format(Str,[X1,Y1,Z1]);
      {_,_,_} ->
        {X,Y,Z} = Axis,
        X1 = wings_util:nice_float(X),
        Y1 = wings_util:nice_float(Y),
        Z1 = wings_util:nice_float(Z),
        Str = ?__(2,"along vector <~s  ~s  ~s>"),
        wings_util:format(Str,[X1,Y1,Z1]);
      {_,_} ->
        {{X,Y,Z},_Point} = Axis,
        X1 = wings_util:nice_float(X),
        Y1 = wings_util:nice_float(Y),
        Z1 = wings_util:nice_float(Z),
        Str = ?__(2,"along vector <~s  ~s  ~s>"),
        wings_util:format(Str,[X1,Y1,Z1]);
      _xyz ->
        Str = ?__(3,"along ~s axis"),
        wings_util:format(Str,[wings_s:dir(Axis)])
    end.

%%% Distance functions
distance(Axis,St) ->
    Set = atom_to_list(wings_pref:get_value(con_dist_set)),
    Keys = mod_key_combo(),
    Length = add_edges(Axis,St),
    length_check(Axis,Length),
    set_constraint(Keys, Set, Length),
    St.

average(Axis,#st{sel=Sel}=St) ->
    Set = atom_to_list(wings_pref:get_value(con_dist_set)),
    Keys = mod_key_combo(),
    EdgeNum = lists:foldl(fun({_Id,Sel0}, A) ->
                          gb_sets:size(Sel0)+A
                          end, 0, Sel),
    Length = add_edges(Axis,St),
    AvgLength = Length/EdgeNum,
    length_check(Axis,AvgLength),
    set_constraint(Keys, Set, AvgLength),
    St.

length_check(Axis,Length) ->
    case Length < ?NONZERO of
      true ->
        case Axis of
          area ->
            wings_u:error_msg(?__(1,"Selection must have an area greater than zero"));
          normal ->
            wings_u:error_msg(?__(2,"Selection must have length greater than zero"));
          {_,_,_} ->
            wings_u:error_msg(?__(3,"Length along vector is too short"));
          _xyz ->
            AxStr = wings_s:dir(Axis),
            Str = ?__(4,"Length along ~s axis is too short"),
            wings_u:error_msg(wings_util:format(Str,[AxStr]))
        end;
      false -> okay
    end.

add_edges(Axis,St) ->
    wings_sel:fold(fun(Edges,We,Acc)->
            Es = gb_sets:to_list(Edges),
            add_edges(Axis,Es,We) + Acc
            end, 0, St).
add_edges(Axis,Es,We) ->
    lists:foldl(fun(Edge,A)->
            #we{es=Etab} = We,
            #edge{vs=Va,ve=Vb} = array:get(Edge,Etab),
            Pos1 = wings_vertex:pos(Va,We),
            Pos2 = wings_vertex:pos(Vb,We),
            get_distance(Axis,Pos1,Pos2) + A
    end, 0, Es).

get_distance(Axis, {Xa,Ya,Za}, {Xb,Yb,Zb}) ->
    case Axis of
      x ->
        abs(e3d_vec:dist({Xa,0.0,0.0},{Xb,0.0,0.0}));
      y ->
        abs(e3d_vec:dist({0.0,Ya,0.0},{0.0,Yb,0.0}));
      z ->
        abs(e3d_vec:dist({0.0,0.0,Za},{0.0,0.0,Zb}));
      normal ->
        abs(e3d_vec:dist({Xa,Ya,Za},{Xb,Yb,Zb}));
      {_,_,_} ->
        {Vx,Vy,Vz} = e3d_vec:norm(Axis),
        abs(Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb))
    end.
%%% Angle selection checking
check_element(#st{sel=[{_,Sel}]}) ->
    case gb_sets:size(Sel) of
      1 -> ok;
      _ -> element_error()
    end;
check_element(_St) ->
    element_error().

-spec element_error() -> no_return().
element_error() ->
    Str = ?__(1,"Exactly one element must be selected"),
    wings_u:error_msg(Str).

check_angle_sel(#st{sel=[{_,Sel}]}) ->
    case gb_sets:size(Sel) of
      2 -> ok;
      _ -> angle_error()
    end;
check_angle_sel(#st{sel=[{_,Sel1},{_,Sel2}]}) ->
    case gb_sets:size(Sel1) == 1 andalso gb_sets:size(Sel2) == 1 of
      true -> ok;
      false -> angle_error()
    end;
check_angle_sel(_St) ->
    angle_error().

-spec angle_error() -> no_return().
angle_error() ->
    wings_u:error_msg(?__(1,"Exactly two elements must be selected")).

%%% Main Angle functions
sub_angle(Axis, St) ->
    Keys = mod_key_combo(),
    M = case Axis of
          fifteen -> 15.0;
          twenty_two -> 22.5;
          thirty -> 30.0;
          forty_five -> 45.0;
          sixty -> 60.0;
          ninety -> 90.0;
          _  -> measure_angle(normal,Axis)
        end,
    N = measure_angle(normal,St),
    Angle = abs(M - N),
    set_angle(Keys,Angle,St).

angle(Axis,St) ->
    Keys = mod_key_combo(),
    Angle = measure_angle(Axis,St),
    set_angle(Keys,Angle,St).

to_axis(Axis,#st{selmode=edge,shapes=Shs,sel=[{Id,Sel}]}=St) ->
    Keys = mod_key_combo(),
    Vec1 = wings_util:make_vector(Axis),
    We = gb_trees:get(Id, Shs),
    [Edge] = gb_sets:to_list(Sel),
    #edge{vs=V0s,ve=V0e} = array:get(Edge, We#we.es),
    Pos1 = wings_vertex:pos(V0s, We),
    Pos2 = wings_vertex:pos(V0e, We),
    Vec2 = e3d_vec:sub(Pos1,Pos2),
    Norm1 = e3d_vec:norm(Vec1),
    Norm2 = e3d_vec:norm(Vec2),
    Angle = e3d_vec:degrees(Norm1,Norm2),
    set_angle(Keys,Angle,St);

to_axis(Axis,#st{selmode=face,shapes=Shs,sel=[{Id,Sel}]}=St) ->
    Keys = mod_key_combo(),
    Vec1 = wings_util:make_vector(Axis),
    We = gb_trees:get(Id, Shs),
    [Face] = gb_sets:to_list(Sel),
    Norm1 = wings_face:normal(Face,We),
    Norm2 = e3d_vec:norm(Vec1),
    Angle = e3d_vec:degrees(Norm1,Norm2),
    set_angle(Keys,Angle,St).

measure_angle(Axis,#st{selmode=edge,shapes=Shs,sel=[{Id0,Sel0},{Id1,Sel1}]}) ->
    We0 = gb_trees:get(Id0, Shs),
    We1 = gb_trees:get(Id1, Shs),
    [E0] = gb_sets:to_list(Sel0),
    [E1] = gb_sets:to_list(Sel1),
    #edge{vs=V0s,ve=V0e} = array:get(E0, We0#we.es),
    #edge{vs=V1s,ve=V1e} = array:get(E1, We1#we.es),
    Pos1 = wings_vertex:pos(V0s, We0),
    Pos2 = wings_vertex:pos(V0e, We0),
    Pos3 = wings_vertex:pos(V1s, We1),
    Pos4 = wings_vertex:pos(V1e, We1),
    [Vec0,Vec1] = get_angle(Axis,[Pos1,Pos2,Pos3,Pos4]),
    raw_angle_to_angle(Vec0,Vec1,V0s,V0e,V1s,V1e);

measure_angle(Axis,#st{selmode=edge,shapes=Shs,sel=[{Id,Sel}]}) ->
    We = gb_trees:get(Id, Shs),
    [E0,E1] = gb_sets:to_list(Sel),
    #edge{vs=V0s,ve=V0e} = array:get(E0, We#we.es),
    #edge{vs=V1s,ve=V1e} = array:get(E1, We#we.es),
    Pos1 = wings_vertex:pos(V0s, We),
    Pos2 = wings_vertex:pos(V0e, We),
    Pos3 = wings_vertex:pos(V1s, We),
    Pos4 = wings_vertex:pos(V1e, We),
    [Vec0,Vec1] = get_angle(Axis,[Pos1,Pos2,Pos3,Pos4]),
    raw_angle_to_angle(Vec0,Vec1,V0s,V0e,V1s,V1e);

measure_angle(Axis,#st{selmode=face,shapes=Shs,sel=[{Id0,Sel0},{Id1,Sel1}]}) ->
    We0 = gb_trees:get(Id0, Shs),
    We1 = gb_trees:get(Id1, Shs),
    [F0] = gb_sets:to_list(Sel0),
    [F1] = gb_sets:to_list(Sel1),
    N0 = wings_face:normal(F0,We0),
    N1 = wings_face:normal(F1,We1),
    Pos1 = wings_face:center(F0,We0),
    Pos2 = e3d_vec:add(Pos1,e3d_vec:mul(N0,0.2)),
    Pos3 = wings_face:center(F1,We1),
    Pos4 = e3d_vec:add(Pos3,e3d_vec:mul(N1,0.2)),
    [Vec0,Vec1] = get_angle(Axis,[Pos1,Pos2,Pos3,Pos4]),
    e3d_vec:degrees(Vec0,Vec1);

measure_angle(Axis,#st{selmode=face,shapes=Shs,sel=[{Id,Sel}]}) ->
    We = gb_trees:get(Id, Shs),
    [F0,F1] = gb_sets:to_list(Sel),
    N0 = wings_face:normal(F0,We),
    N1 = wings_face:normal(F1,We),
    Pos1 = wings_face:center(F0,We),
    Pos2 = e3d_vec:add(Pos1,e3d_vec:mul(N0,0.2)),
    Pos3 = wings_face:center(F1,We),
    Pos4 = e3d_vec:add(Pos3,e3d_vec:mul(N1,0.2)),
    [Vec0,Vec1] = get_angle(Axis,[Pos1,Pos2,Pos3,Pos4]),
    e3d_vec:degrees(Vec0,Vec1).

get_angle(Axis,Vlist) ->
    [{X0s,Y0s,Z0s},{X0e,Y0e,Z0e},{X1s,Y1s,Z1s},{X1e,Y1e,Z1e}] = Vlist,
    case Axis of
      x ->
        [{0.0, Y0e-Y0s, Z0e-Z0s},
         {0.0, Y1e-Y1s, Z1e-Z1s}];
      y ->
        [{X0e-X0s, 0.0, Z0e-Z0s},
         {X1e-X1s, 0.0, Z1e-Z1s}];
      z ->
        [{X0e-X0s, Y0e-Y0s, 0.0},
         {X1e-X1s, Y1e-Y1s, 0.0}];
      normal ->
        [{X0e-X0s, Y0e-Y0s, Z0e-Z0s},
         {X1e-X1s, Y1e-Y1s, Z1e-Z1s}];
      {_,_} -> %% from axis_point ask
        {PlaneNorm,PlanePoint} = Axis,
        Pn = e3d_vec:norm(PlaneNorm),
        Dp = e3d_vec:dot(Pn, Pn),
        VPoints = lists:foldl(fun(Point,A) ->
                    M0 = e3d_vec:dot(e3d_vec:sub(PlanePoint, Point),Pn)/Dp,
                    M1 = e3d_vec:add(Point, e3d_vec:mul(Pn, M0)),
                    [M1|A]
          end,[],Vlist),
        [{X2s,Y2s,Z2s},{X2e,Y2e,Z2e},{X3s,Y3s,Z3s},{X3e,Y3e,Z3e}] = VPoints,

        [{X2e-X2s, Y2e-Y2s, Z2e-Z2s},{X3e-X3s, Y3e-Y3s, Z3e-Z3s}]
    end.

raw_angle_to_angle(Vec0,Vec1,V0s,V0e,V1s,V1e) ->
    RawAngle = e3d_vec:degrees(Vec0, Vec1),
    case {V0s,V0e} of
      {V1s,_} -> RawAngle;
      {_,V1e} -> RawAngle;
      {V1e,_} -> 180.0 - RawAngle;
      {_,V1s} -> 180.0 - RawAngle;
      {_,_}   -> RawAngle
    end.

set_angle(Keys, Angle, St) ->
    A = case Angle >= ?NONZERO of
        true -> Angle;
        false-> 180.0
    end,
    set_constraint(Keys, "con_rot_", A),
    St.

%%% Scale functions
scale({Axis1,Axis2},#st{selmode=edge}=St) ->
    Keys = mod_key_combo(),
    Length = add_edges(Axis1,St),
    length_check(Axis1,Length),
    wings:ask(secondary_sel_ask(scale,Axis1,Axis2,St), St, fun (St0,OrigSt) ->
       scale(Axis1,Axis2,Keys,OrigSt,St0)
    end);

scale({Axis1,Axis2},#st{selmode=face}=St) ->
    Keys = mod_key_combo(),
    Area = add_areas(Axis1,St),
    length_check(area,Area),
    wings:ask(secondary_sel_ask(scale_area,Axis1,Axis2,St), St, fun (St0,OrigSt) ->
       scale(Axis1,Axis2,Keys,OrigSt,St0)
    end).

scale(Axis1,Axis2,Keys,OrigSt,#st{selmode=edge}=St) ->
    Length1 = add_edges(Axis1,OrigSt),
    Length2 = add_edges(Axis2,St),
    Percent = Length1/Length2,
    set_constraint(Keys, "con_scale_", Percent),
    OrigSt;

scale(Axis1,Axis2,Keys,OrigSt,#st{selmode=face}=St) ->
    Area1 = add_areas(Axis1,OrigSt),
    Area2 = add_areas(Axis2,St),
    Percent = Area1/Area2,
    set_constraint(Keys, "con_scale_", Percent),
    OrigSt.

add_areas(Axis,St) ->
    wings_sel:fold(fun(Faces,We,Acc) ->
            Fs = gb_sets:to_list(Faces),
            add_areas(Axis,Fs,We) + Acc
            end, 0, St).

add_areas(Axis,Fs,We) ->
    lists:foldl(fun(Face,A) ->
            Area = case Axis of
                normal -> wings_face:area(Face,We);
                _axis -> get_area(Axis,Face,We)
            end,
            Area + A
    end, 0, Fs).

get_area(Axis,Face,We) ->
    #we{vp=Vtab} = We,
    Vs = wings_face:vertices_ccw(Face, We),
    Vlist0 = [array:get(V, Vtab) || V <- Vs],
    Vlist1 = flatten_vpos_to_axis(Axis,Vlist0),
    FaceVs = lists:seq(0, length(Vs)-1),
    E3dFaces = [#e3d_face{vs=FaceVs}],
    [Area] = e3d_mesh:face_areas(E3dFaces, Vlist1),
    Area.

flatten_vpos_to_axis(Axis,Vlist) ->
    {PlaneNorm,PlanePoint} = case Axis of
         x -> {{1.0,0.0,0.0},{1.0,0.0,0.0}};
         y -> {{0.0,1.0,0.0},{0.0,1.0,0.0}};
         z -> {{0.0,0.0,1.0},{0.0,0.0,1.0}};
         {_,_} -> Axis
    end,
    Pn = e3d_vec:norm(PlaneNorm),
    Dp = e3d_vec:dot(Pn, Pn),
    NewVlist = lists:foldl(fun(Point,A) ->
                M0 = e3d_vec:dot(e3d_vec:sub(PlanePoint, Point),Pn)/Dp,
                M1 = e3d_vec:add(Point, e3d_vec:mul(Pn, M0)),
                [M1|A]
      end,[],Vlist),
    NewVlist.

%%% Difference functions
difference({Axis1,Axis2},St) ->
    Keys = mod_key_combo(),
    wings:ask(secondary_sel_ask(difference,Axis1,Axis2,St), St, fun (St0,OrigSt) ->
       difference(Axis1,Axis2,Keys,OrigSt,St0)
    end).

difference(Axis1,Axis2,Keys1,OrigSt,St) ->
    Keys2 = mod_key_combo(),
    Keys = case Keys2 of
             {false,false,false} -> Keys1;
             _if_keys_held_again -> Keys2
           end,
    Set = atom_to_list(wings_pref:get_value(con_dist_set)),
    Length1 = add_edges(Axis1,OrigSt),
    Length2 = add_edges(Axis2,St),
    Difference = abs(Length1-Length2),
    set_constraint(Keys, Set, Difference),
    OrigSt.

%%% Centers functions
centers(Axis,St) ->
    Keys = mod_key_combo(),
    wings:ask(secondary_sel_ask(center,Axis,none,St), St, fun (St0,OrigSt) ->
       centers(Axis,Keys,OrigSt,St0)
    end).

centers(Axis,Keys1,OrigSt,St) ->
    Keys2 = mod_key_combo(),
    Keys = case Keys2 of
             {false,false,false} -> Keys1;
             _if_keys_held_again -> Keys2
           end,
    Set = atom_to_list(wings_pref:get_value(con_dist_set)),
    Center1 = wings_sel:center(OrigSt),
    Center2 = wings_sel:center(St),
    Distance = get_distance(Axis,Center1,Center2),
    set_constraint(Keys, Set, Distance),
    OrigSt.

%%% Modifier keys
mod_key_combo() ->
    Shift = wings_io:is_modkey_pressed(?KMOD_SHIFT),
    Ctrl  = wings_io:is_modkey_pressed(?KMOD_CTRL),
    Alt   = wings_io:is_modkey_pressed(?KMOD_ALT),
    {Shift,Ctrl,Alt}.

%%% Set preferences
set_constraint({Shift,Ctrl,Alt}, Key, Val) ->
    ModKeyCombo = case {Shift,Ctrl,Alt} of
      {true,false,false} -> "shift";
      {false,true,false} -> "ctrl";
      {true,true,false} -> "ctrl_shift";
      {false,false,true} -> "alt";
      {true,false,true} -> "shift_alt";
      {false,true,true} -> "ctrl_alt";
      {true,true,true} -> "ctrl_shift_alt";
      {false,false,false} ->
        case Key of
            "con_dist_" -> atom_to_list(wings_pref:get_value(con_dist_default));
          "con_dist_a_" -> atom_to_list(wings_pref:get_value(con_dist_default));
             "con_rot_" -> atom_to_list(wings_pref:get_value(con_rot_default));
           "con_scale_" -> atom_to_list(wings_pref:get_value(con_scale_default))
        end
    end,
    ComboStr = mod(ModKeyCombo),
    Tag = tag(Key),
    Msg = io_lib:format(?__(1,"The ~ts constraint bound to ~ts is now set to ~p"),[Tag,ComboStr,Val]),
    io:format("~ts\n",[Msg]),
    wings_u:message(Msg),
    wings_pref:set_value(list_to_atom(Key++ModKeyCombo), Val).

tag(Key) ->
    case Key of
      "con_rot_" -> ?__(1,"Rotation");
      "con_scale_" -> ?__(2,"Scale Factor");
      "con_dist_" -> ?__(3,"Distance");
      "con_dist_a_" -> ?__(4,"Alternate Distance")
    end.

mod(ModKeyCombo) ->
    wings_util:stringify(list_to_atom(ModKeyCombo)).
