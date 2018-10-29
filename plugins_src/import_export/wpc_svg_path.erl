%%
%%  wpc_svg.erl --
%%
%% The importer is limited to paths. This means you should convert objects
%% to paths before saving. So in Inkscape you would select each object and
%% use Path | Object to Path before saving. Also, it is best if you save as a
%% 'plain' svg. The default svg can use the Arcto (A or a) declaration, and
%% currently there is no handling for this directive.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_svg_path).
-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,reverse/1,splitwith/2,map/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(SCALEFAC, 0.01). % amount to scale coords by

-record(cedge,% polyarea and cedge records must match definitions in wpc_tt.erl
    {vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs

-record(path,
    {ops=[],			%list of pathops
     close=false}).		%true or false

-record(pathop,
    {opkind,			%pmoveto, plineto, or pcurveto
     x1=0.0,
     y1=0.0,
     x2=0.0,
     y2=0.0,
     x3=0.0,
     y3=0.0}).

-record(pstate,
    {op,
     curpath=#path{},		%current path
     objects=[]}).          %object list (paths)

init() -> true.

menu({file,import}, Menu) ->
    Menu ++ [{".svg Path...", svg, [option]}];
menu(_, Menu) -> Menu.

command({file,{import,{svg,Ask}}}, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(wpc_svg_path, svg_bisections, 0),
    Dialog =
      [{hframe,[{label,?__(2,"Number of edge bisections")},{text,DefBisect}]},panel,
       {label,?__(3,"(For best results when importing from Inkscape,\nconvert all objects to paths and save as a plain .svg)")}],
    wpa:dialog(Ask, ?__(1,".svg Path Import Options"),
        Dialog,
        fun(Res) -> {file,{import, svg, Res}} end);
command({file,{import, svg, [Nsub]}}, St) ->
    Props = [{extensions,[{".svg",?__(4,".svg File")}]}],
    wpa:pref_set(wpc_svg_path, svg_bisections, Nsub),
    wpa:import(Props, fun(F) -> make_svg(F, Nsub) end, St);
command(_, _) ->
    next.

make_svg(Name, Nsubsteps) ->
    case catch try_import_svg(Name, Nsubsteps) of
    {ok, E3dFile} ->
        wpa:pref_set(wpc_svg, svg_bisections, Nsubsteps),
        {ok, E3dFile};
    {error,Reason} ->
        {error, ?__(1,"Inkscape .svg path import failed")++": " ++ Reason};
    E ->
        io:format("File Import Error Report:\n ~p\n",[E]),
        {error, ?__(2,"Inkscape .svg path import internal error")}
    end.

try_import_svg(Name, Nsubsteps) ->
    case file:read_file(Name) of
    {ok,<<Rest/binary>>} ->
        Objs = parse_bin_svg(Rest),
        Closedpaths = [ P || P <- Objs, P#path.close =:= true ],
        Cntrs = getcontours(Closedpaths),
        Pas = wpc_tt:findpolyareas(Cntrs),
        Pas1 = wpc_tt:subdivide_pas(Pas,Nsubsteps),
        {Vs0,Fs,HEs} = wpc_tt:polyareas_to_faces(Pas1),
        Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
        Vec = e3d_vec:sub(e3d_vec:zero(),Center),
        Vs = reverse(center_object(Vec,Vs0)),
        Efs = [ #e3d_face{vs=X} || X <- Fs],
        Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,he=HEs},
        {ObjName,_} = splitwith(fun
            (Char) ->
              case Char of
                $/ -> false;
                $\ -> false;
                _ -> true
              end
        end, reverse(Name)),
        Obj = #e3d_object{name=reverse(ObjName),obj=Mesh},
        {ok, #e3d_file{objs=[Obj]}};
    {ok,_} ->
        {error,?__(1,"Not an .svg file")};
        {error,Reason} ->
        {error,file:format_error(Reason)}
    end.

center_object(Vec,Vs) ->
    foldl(fun(V,Acc) ->
       {X,Y,Z} = e3d_vec:add(V,Vec),
       [{X,Y,Z}|Acc]
    end,[],Vs).

parse_bin_svg(Bin) ->
    Chars = after_end_setup_svg(Bin),
    parse_chars(Chars, #pstate{}).

%skip until "    <path", then turn rest of binary into a list
after_end_setup_svg(<<"<path",Rest/binary>>) ->
    binary_to_list(Rest);
after_end_setup_svg(<<_,Rest/binary>>) ->
    after_end_setup_svg(Rest);
after_end_setup_svg(_) -> [].

% Parse characters and find valid paths
parse_chars([], #pstate{objects=[]}) ->
    wings_u:error_msg(?__(1,"No closed paths."));
parse_chars([], #pstate{objects=Objs}) ->
    Objs;
parse_chars(" d=\"" ++ Rest0, #pstate{objects=Objs}) ->
    {Path0,Rest} = splitwith(fun(Char) -> Char =/= $" end, Rest0),
    Path = string:tokens(Path0, " ,\n\r"),
    Pst = parse_path(Path, ?SCALEFAC, #pstate{objects=Objs}),
    parse_chars(Rest, Pst);
parse_chars([_|Rest], Pst) ->
    parse_chars(Rest, Pst).

parse_path([Op,X0,Y0|Rest], F, #pstate{curpath=#path{ops=Ops}=Path0}=Pst)
           when Op =:= "M"; Op =:= "m" ->
    X = string_to_float(X0)*F,
    Y = string_to_float(Y0)*F,
    PathOp = #pathop{opkind=pmoveto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    L = if Op =:= "M" -> "L"; true -> "l" end,
    parse_path(Rest, F, Pst#pstate{op=L,curpath=Path});
parse_path([Op|Rest], F, Pst) when Op =:= "C";  Op =:= "c"; Op =:= "L"; Op =:= "l" ->
    parse_path(Rest, F, Pst#pstate{op=Op});
parse_path([Z], F, #pstate{curpath=#path{ops=Ops}=Path0}=Pst) when Z =:= "z"; Z =:= "Z" ->
    Path = Path0#path{ops=reverse(Ops),close=true},
    parse_path([], F, Pst#pstate{curpath=Path});
parse_path([Z|Rest], F, #pstate{curpath=#path{ops=Ops}=Path0,objects=Objs}) when Z =:= "z"; Z =:= "Z" ->
    Path = Path0#path{ops=reverse(Ops),close=true},
    parse_path(Rest, F, #pstate{objects=[Path|Objs]});
parse_path([X0,Y0|Rest], F, #pstate{op="L",curpath=#path{ops=Ops}=Path0}=Pst) ->
    X = string_to_float(X0)*F,
    Y = string_to_float(Y0)*F,
    PathOp = #pathop{opkind=plineto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([X0,Y0|Rest], F, #pstate{op="l",curpath=#path{ops=Ops}=Path0}=Pst) ->
    [LastPathOp|_] = Ops,
    {X2,Y2} = case LastPathOp of
      #pathop{opkind=plineto,x1=X1,y1=Y1} -> {X1,Y1};
      #pathop{opkind=pmoveto,x1=X1,y1=Y1} -> {X1,Y1};
      #pathop{opkind=pcurveto,x3=X1,y3=Y1} -> {X1,Y1}
    end,
    X = string_to_float(X0)*F + X2,
    Y = string_to_float(Y0)*F - Y2,
    PathOp = #pathop{opkind=plineto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([Xa,Ya,Xb,Yb,Xc,Yc|Rest], F, #pstate{op="C",curpath=#path{ops=Ops}=Path0}=Pst) ->
    X1 = string_to_float(Xa)*F,
    Y1 = string_to_float(Ya)*F,
    X2 = string_to_float(Xb)*F,
    Y2 = string_to_float(Yb)*F,
    X3 = string_to_float(Xc)*F,
    Y3 = string_to_float(Yc)*F,
    PathOp = #pathop{opkind=pcurveto,x1=X1,y1=-Y1,x2=X2,y2=-Y2,x3=X3,y3=-Y3},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([Xa,Ya,Xb,Yb,Xc,Yc|Rest], F, #pstate{op="c",curpath=#path{ops=Ops}=Path0}=Pst) ->
    [LastPathOp|_] = Ops,
    {Xa0,Ya0} = case LastPathOp of
      #pathop{opkind=plineto,x1=Xa1,y1=Ya1} -> {Xa1,Ya1};
      #pathop{opkind=pmoveto,x1=Xa1,y1=Ya1} -> {Xa1,Ya1};
      #pathop{opkind=pcurveto,x3=Xa1,y3=Ya1} -> {Xa1,Ya1}
    end,
    X1 = string_to_float(Xa)*F + Xa0,
    Y1 = string_to_float(Ya)*F - Ya0,
    X2 = string_to_float(Xb)*F + Xa0,
    Y2 = string_to_float(Yb)*F - Ya0,
    X3 = string_to_float(Xc)*F + Xa0,
    Y3 = string_to_float(Yc)*F - Ya0,
    PathOp = #pathop{opkind=pcurveto,x1=X1,y1=-Y1,x2=X2,y2=-Y2,x3=X3,y3=-Y3},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([], _, #pstate{curpath=#path{close=true}=CurrentPath,objects=Objs}=Pst) ->
    Pst#pstate{objects=[CurrentPath|Objs]};
parse_path([], _, Pst) ->
    Pst;
parse_path(_, _, _) ->
    wings_u:error_msg(?__(3,"Couldn't process this file.")).

string_to_float(String) ->
    case catch list_to_float(String) of
      {'EXIT',_} ->
        case catch float(list_to_integer(String)) of
          {'EXIT',_} ->
            % convert "5e-5" to 5.0e-5 (for example)
            {A,B} = splitwith(fun(E) -> E =/= $e end, String),
            list_to_float(A++".0"++B);
          Float -> Float
        end;
      Float -> Float
    end.

getcontours(Ps) ->
    map(fun getcedges/1, Ps).

getcedges(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops]}) ->
    getcedges(Ops,{X,Y},{X,Y},[]);
getcedges(_) -> [].

getcedges([],{X,Y},{X,Y},Acc) ->
    reverse(Acc);
getcedges([],Prev,{X,Y},Acc) ->		% prev != first, so close with line
    reverse([#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=plineto,x1=X,y1=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X,y3=Y}|Ops],
        Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,
        [#cedge{vs=Prev,cp1={X1,Y1},cp2={X2,Y2},ve={X,Y}}|Acc]);
getcedges([_|_],_,_,_) ->
    [].	% funny path (probably moveto in middle), so return nothing
