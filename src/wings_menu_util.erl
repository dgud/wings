%%
%%  wings_menu_util.erl --
%%
%%     Menu utilities and helpers.
%%
%%  Copyright (c) 2002-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_menu_util).
-export([directions/1,directions/2,scale/1,rotate/1,flatten/0,all_xyz/0,
	 crossmark/1]).

-include("wings.hrl").

directions(#st{selmode=Mode}) ->
    fun(B, Ns) ->
	    dirs(B, Mode, Ns)
    end.

dirs(1, Mode, Ns) -> dirs_1(Mode, Ns);
dirs(2, body, [duplicate|_]) -> {body,duplicate};
dirs(2, body, [move|_]) -> ignore;
dirs(2, body, [move_light|_]) -> ignore;
dirs(2, face, [extrude|_]) -> {face,{extrude,{faces,{'ASK',{[axis],[]}}}}};
dirs(2, face, [extract|_]) -> {face,{extract,{faces,{'ASK',{[axis],[]}}}}};
dirs(2, _Mode, Ns) ->
    case magnet_props(normal, Ns) of
	[] -> 
	    wings_menu:build_command(normal, Ns);
	Flags ->
	    wings_menu:build_command({'ASK',{[],[normal],Flags}}, Ns)
    end;
dirs(3, face, [extrude|_]) -> {face,{extrude,{region,{'ASK',{[axis],[]}}}}};
dirs(3, face, [extract|_]) -> {face,{extract,{region,{'ASK',{[axis],[]}}}}};
dirs(3, _Mode, Ns) ->
    Flags = magnet_props(some_axis, Ns),
    wings_menu:build_command({'ASK',{[axis],[],Flags}}, Ns);
dirs(help, body, [move|_]) ->
    {?STR(dirs,1,"Move along std. axis"),[],?STR(dirs,2,"Pick axis to move along")};
dirs(help, _Mode, Ns) -> dirs_help(Ns).

dirs_help([move|_]) ->
    {?STR(dirs,3,"Move along std. axis"),
     ?STR(dirs,4,"Move along selection's normal"),
     ?STR(dirs,5,"Pick axis to move along")};
dirs_help([extrude,face]) ->
    {?STR(dirs,6,"Extrude along std. axis"),
     ?__(18,"Pick axis to extrude each face along"),
     ?__(19,"Pick axis to extrude each region of faces along")};
dirs_help([extrude|_]) ->
    {?STR(dirs,6,"Extrude along std. axis"),
     ?STR(dirs,7,"Extrude along selection's normal"),
     ?STR(dirs,8,"Pick axis to extrude along")};
dirs_help([extract|_]) ->
    {?STR(dirs,12,"Extract along std. axis"),
     ?__(20,"Pick axis to extract each face along"),
     ?__(21,"Pick axis to extract each region of faces along")};
dirs_help([duplicate|_]) ->
    {?STR(dirs,15,"Duplicate; move along std. axis"),
     ?STR(dirs,16,"Duplicate; don't move"),
     ?STR(dirs,17,"Duplicate; pick axis to move along")};
dirs_help([shell_extrude|_]) ->
    {?STR(dirs,22,"Extract Shell and Extrude along std. axis"),
     ?STR(dirs,23,"Extract Shell and Extrude along selection's normal"),
     ?STR(dirs,24,"Pick axis to Extract Shell and Extrude along")};
dirs_help(_) -> "".

dirs_1(body, Ns) -> directions([free,x,y,z], Ns);
dirs_1(_, Ns) ->
    directions([normal,free,x,y,z], Ns).

all_xyz() ->
    [{wings_s:dir(all),all},
     {wings_s:dir(x),x},
     {wings_s:dir(y),y},
     {wings_s:dir(z),z}].

%%%
%%% Scale sub-menu.
%%%

scale(#st{selmode=body}) -> adv_scale_1([]);
scale(_) -> adv_scale_1([magnet]).

adv_scale_1(MagFlags) ->
    [{?STR(adv_scale_1,1,"Scale Uniform"),{scale,fun(B, Ns) -> uniform_scale(B, Ns, MagFlags) end},
      [],MagFlags},
     {?STR(adv_scale_1,2,"Scale Axis"),{scale,fun(B, Ns) -> scale(B, Ns, [], MagFlags) end},
      [],MagFlags},
     {?STR(adv_scale_1,3,"Scale Radial"),{scale,fun(B, Ns) -> scale(B, Ns, [radial], MagFlags) end},
      [],MagFlags}].

uniform_scale(help, _, _) ->
    ChoosePoint = ?STR(uniform_scale,1,"Choose point to scale from"),
    {?STR(uniform_scale,2,"Scale uniformly from midpoint of selection"),ChoosePoint,ChoosePoint};
uniform_scale(1, Ns, Flags) ->
    wings_menu:build_command({'ASK',{[],[center,uniform],Flags}}, Ns);
uniform_scale(_, Ns, Flags) ->
    wings_menu:build_command({'ASK',{[point],[],Flags}}, Ns).

scale(help, _, [radial],_) ->
    {?STR(scale,1,"Scale outward from std. axis"),
     ?STR(scale,2,"Pick axis and point to scale from"),
     ?STR(scale,3,"Pick axis to scale out from")};
scale(help, _, [], _) ->
    {?STR(scale,4,"Scale along std. axis"),
     ?STR(scale,5,"Pick axis and point to scale from"),
     ?STR(scale,6,"Pick axis to scale along")};
scale(1, Ns, Flags, _MagFlags) ->
    [scale_fun(x, Ns, Flags),
     scale_fun(y, Ns, Flags),
     scale_fun(z, Ns, Flags),
     separator,
     scale_fun(last_axis, Ns, Flags),
     scale_fun(default_axis, Ns, Flags)];
scale(2, Ns, Flags, MagFlags) ->
    wings_menu:build_command({'ASK',{[axis,point],Flags,MagFlags}}, Ns);
scale(3, Ns, Flags, MagFlags) ->
    wings_menu:build_command({'ASK',{[axis_point],Flags,MagFlags}}, Ns).

scale_fun(Dir, Names, [radial]) ->
    scale_fun({radial,Dir}, Names, []);
scale_fun(Dir, Names, _Flags) ->
    DirString = wings_s:dir(Dir),
    F = magnet_scale_rot_fun(Dir, center),
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],?STR(scale_fun,1,"Pick point to scale from")},
    {DirString,F,Help,magnet_props(Dir, Names)}.



%%%
%%% Rotate sub-menu.
%%%

rotate(#st{selmode=body}) -> rotate_1([]);
rotate(_) -> rotate_1([magnet]).

rotate_1(Flags) ->    
    {?STR(rotate_1,1,"Rotate"),{rotate,fun rotate/2},[],Flags}.

rotate(help, _) ->
    {?STR(rotate,1,"Rotate around std. axis"),
     ?STR(rotate,2,"Pick axis and ref point"),
     ?STR(rotate,3,"Pick axis to rotate around")};
rotate(1, [rotate,Mode]=Ns) when Mode == vertex; Mode == body ->
    rotate_common(Ns);
rotate(1, Ns) ->
    [rotate_fun(normal, Ns)|rotate_common(Ns)];
rotate(2, Ns) ->
    MagFlags = magnet_props(any, Ns),
    wings_menu:build_command({'ASK',{[axis,point],[],MagFlags}}, Ns);
rotate(3, Ns) ->
    MagFlags = magnet_props(any, Ns),
    wings_menu:build_command({'ASK',{[axis_point],[],MagFlags}}, Ns).

rotate_common(Ns) ->
    [rotate_fun(free, Ns),
     rotate_fun(x, Ns),
     rotate_fun(y, Ns),
     rotate_fun(z, Ns),
     separator,
     rotate_fun(last_axis, Ns),
     rotate_fun(default_axis, Ns)].

rotate_fun(Dir, Names) ->
    DirString = wings_util:cap(wings_s:dir(Dir)),
    F = magnet_scale_rot_fun(Dir, center),
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],?STR(rotate_fun,1,"Pick point for axis to pass through")},
    Ps = magnet_props(Dir, Names),
    {DirString,F,Help,Ps}.

magnet_scale_rot_fun(Vec, Point) ->
    fun(1, Ns) ->
	    MagFlags = magnet_props(Vec, Ns),
	    wings_menu:build_command({'ASK',{[],[Point,Vec],MagFlags}}, Ns);
       (2, _Ns) ->
	    ignore;
       (3, Ns) ->
	    MagFlags = magnet_props(Vec, Ns),
	    wings_menu:build_command({'ASK',{[point],[Vec],MagFlags}}, Ns)
    end.

%%%
%%% Flatten submenu.
%%%

flatten() ->
    {?STR(flatten,1,"Flatten"),{flatten,fun flatten/2}}.

flatten(help, _) ->
    {?STR(flatten,2,"Flatten to std. planes"),
     ?STR(flatten,3,"Pick plane and ref point on plane"),
     ?STR(flatten,4,"Pick plane")};
flatten(1, [flatten,vertex]) ->
    %% Vertex mode flatten.
    flatten_common();
flatten(1, [flatten,edge]) ->
    %% Vertex mode flatten.
    flatten_common();
flatten(1, _) ->
    %% Face mode flatten.
    [flatten_fun(normal)|flatten_common()];
flatten(2, Ns) ->
    wings_menu:build_command({'ASK',{[axis,point],[],[]}}, Ns);
flatten(3, Ns) ->
    wings_menu:build_command({'ASK',{[axis],[],[]}}, Ns).

flatten_common() ->
    [flatten_fun(x),
     flatten_fun(y),
     flatten_fun(z),
     separator,
     flatten_fun(last_axis),
     flatten_fun(default_axis)].

flatten_fun(Vec) ->
    flatten_fun_1(Vec, Vec, wings_util:cap(wings_s:dir(Vec))).

flatten_fun_1(Vec, Axis, String) ->
    F = fun(1, Ns) ->
		wings_menu:build_command(Vec, Ns);
	   (2, _Ns) ->
		ignore;
	   (3, Ns) ->
		wings_menu:build_command({'ASK',{[point],[Vec]}}, Ns)
	end,
    Help0 = dir_help(Axis, [flatten]),
    Help = {Help0,[],?STR(flatten_fun_1,1,"Pick point on plane")},
    {String,F,Help,[]}.

%%%
%%% General directions.
%%%

directions([D|Dirs], Ns) ->
    [direction(D, Ns)|directions(Dirs, Ns)];
directions([], Ns) ->
    [separator,
     direction(last_axis, Ns),
     direction(default_axis, Ns)].

direction(Dir, [extrude,face]) ->
    Str  = wings_util:cap(wings_s:dir(Dir)),
    Help = {dir_help(Dir, [extrude_region]),[],dir_help(Dir, [extrude])},
    F = fun
      (1,_Ns) -> {face,{extrude,{region,Dir}}};
      (3,_Ns) -> {face,{extrude,{faces,Dir}}};
      (_, _) -> ignore
    end,
    {Str,F,Help,[]};
direction(Dir, [extract,face]) ->
    Str  = wings_util:cap(wings_s:dir(Dir)),
    Help = {dir_help(Dir, [extract_region]),[],dir_help(Dir, [extract])},
    F = fun
      (1,_Ns) -> {face,{extract,{region,Dir}}};
      (3,_Ns) -> {face,{extract,{faces,Dir}}};
      (_, _) -> ignore
    end,
    {Str,F,Help,[]};
direction(Dir, Ns) ->
    Str  = wings_util:cap(wings_s:dir(Dir)),
    Help = dir_help(Dir, Ns),
    Ps = magnet_props(Dir, Ns),
    {Str,Dir,Help,Ps}.

magnet_props(normal, [rotate|_]) -> [];
magnet_props(_, [_,body]) -> [];
magnet_props(_, [move|_]) -> [magnet];
magnet_props(_, [scale|_]) -> [magnet];
magnet_props(_, [rotate|_]) -> [magnet];
magnet_props(_, _) -> [].

dir_help(Axis, Ns) when Axis == x; Axis == y; Axis == z ->
    dir_help_1(Ns, wings_s:dir_axis(Axis));
dir_help(last_axis, Ns) ->
    dir_help_1(Ns, ?STR(dir_help,3,"the last axis"));
dir_help(default_axis, Ns) ->
    dir_help_1(Ns, ?STR(dir_help,4,"the default axis"));
dir_help({radial,Axis}, Ns) ->
    dir_help_1(Ns, [around|wings_s:dir_axis(Axis)]);
dir_help(radial_x, Ns) ->
    dir_help_1(Ns, [around|?STR(dir_help,7,"around") ++ " " ++ wings_s:dir_axis(x)]);
dir_help(radial_y, Ns) ->
    dir_help_1(Ns, [around|?STR(dir_help,7,"around") ++ " " ++ wings_s:dir_axis(y)]);
dir_help(radial_z, Ns) ->
    dir_help_1(Ns, [around|?STR(dir_help,7,"around") ++ " " ++ wings_s:dir_axis(z)]);
dir_help(normal, Ns) ->
    dir_help_1(Ns, [normal|?STR(dir_help,10,"along its normal")]);
dir_help(free, Ns) ->
    dir_help_1(Ns, [free|?STR(dir_help,11,"freely in all directions")]);
dir_help(uniform, [scale|_]) ->
    ?STR(dir_help,12,"Scale equally in all directions").

%% Normal/Free.
dir_help_1([move|_], [NF|Text]) when NF == normal; NF == free ->
    ?STR(dir_help_1,1,"Move each element ") ++ Text;
dir_help_1([rotate|_], [free|_Text]) ->
    ?STR(dir_help_1,2,"Rotate freely");
dir_help_1([rotate|_], [normal|_Text]) ->
    ?STR(dir_help_1,3,"Rotate around each element's normal");
dir_help_1([extrude|_], [NF|Text]) when NF == normal; NF == free ->
    ?STR(dir_help_1,4,"Extrude each element, then move it ")++ Text;
dir_help_1([extract|_], [NF|Text]) when NF == normal; NF == free ->
    ?__(24,"Extract each element, then move it ")++ Text;
dir_help_1([extrude_region|_], [normal|_]) ->
    ?STR(dir_help_1,5,"Extrude faces as region, then move faces along the region's normal");
dir_help_1([extrude_region|_], [free|Text]) ->
    ?STR(dir_help_1,6,"Extrude faces as region, then move faces ") ++ Text;
dir_help_1([extract_region|_], [normal|_]) ->
    ?STR(dir_help_1,7,"Extract faces, then move faces along the region's normal");
dir_help_1([extract_region|_], [free|Text]) ->
    ?STR(dir_help_1,8,"Extract faces, then move faces ") ++ Text;
dir_help_1([flatten|_], [normal|_Text]) ->
    ?STR(dir_help_1,9,"Flatten elements to normal plane");
dir_help_1([lift|_], [normal|_]) ->
    ?STR(dir_help_1,10,"Lift face along its normal");
dir_help_1([lift|_], [free|Text]) ->
    ?STR(dir_help_1,11,"Lift face and move it ") ++ Text;
dir_help_1([duplicate|_], [free|Text]) ->
    ?STR(dir_help_1,12,"Duplicate and move freely ")++ Text;
dir_help_1([shell_extrude|_], [normal|_]) ->
    ?STR(dir_help_1,24,"Extract and Extrude faces as region, then move faces along the region's normal");
dir_help_1([shell_extrude|_], [free|Text]) ->
    ?STR(dir_help_1,25,"Extract and Extrude faces as region, then move faces ") ++ Text;

%% Axis
dir_help_1([move|_], Text) ->
    ?STR(dir_help_1,13,"Move each element along ") ++ Text;
dir_help_1([extrude|_], Text) ->
    ?STR(dir_help_1,14,"Extrude elements, then move along ") ++ Text;
dir_help_1([extract|_], Text) ->
    ?__(25,"Extract elements, then move along ") ++ Text;
dir_help_1([extrude_region|_], Text) ->
    ?STR(dir_help_1,15,"Extrude faces as region, then move along ") ++ Text;
dir_help_1([extract_region|_], Text) ->
    ?STR(dir_help_1,16,"Extract faces, then move along ") ++ Text;
dir_help_1([rotate|_], Text) ->
    ?STR(dir_help_1,17,"Rotate around ") ++ Text;
dir_help_1([scale|_], [around|Text]) ->
    ?STR(dir_help_1,18,"Scale ") ++ Text;
dir_help_1([scale|_], Text) ->
    ?STR(dir_help_1,19,"Scale along ")++ Text;
dir_help_1([flatten|_], Text) ->
    ?STR(dir_help_1,20,"Flatten to ")++ Text;
dir_help_1([flatten_move|_], Text) ->
    ?STR(dir_help_1,21,"Flatten and move to ") ++ Text;
dir_help_1([lift|_], Text) ->
    ?STR(dir_help_1,22,"Lift face along ")++ Text;
dir_help_1([duplicate|_], Text) ->
    ?STR(dir_help_1,23,"Duplicate, then move along ")++ Text;
dir_help_1([shell_extrude|_], Text) ->
    ?STR(dir_help_1,26,"Extract and Extrude faces as region, then move along ") ++ Text;
dir_help_1(_, _) -> "".

%% Menu checkmark
crossmark(Key) ->
    Val = case wings_pref:get_value(Key) of
	      undefined ->
		  {_,Client} = wings_wm:this(),
		  wings_wm:get_prop(Client, Key);
	      Other -> Other
	  end,
    case Val of
	false -> [];
	true -> [crossmark]
    end.
