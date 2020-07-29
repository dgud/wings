%%
%%  wpc_stl.erl --
%%
%%     Binary StereoLithography File Format (*.stl) Import/Export
%%
%%  Copyright (c) 2005-2011 Anthony D'Agostino
%%                2020 Added export dialog by Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_stl).
-export([init/0, menu/2, command/2]).
-include_lib("wx/include/wx.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({file, import}, Menu) ->
    menu_entry(import,Menu);
menu({file, export}, Menu) ->
    menu_entry(export,Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(export,Menu);
menu(_, Menu) -> Menu.

command({file, {import, stl}}, St) ->
    Props = props(),
    wpa:import(Props, fun stl_import/1, St);
command({file,{export,{stl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{stl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

menu_entry(Opt,Menu) ->
    Menu ++ [{"StereoLithography (.stl)...", stl, [if Opt==export -> option; true -> [] end]}].

props() ->
    [{ext, ".stl"},{ext_desc, "StereoLithography Binary File"}].

%%% ================================
%%% === StereoLithography Export ===
%%% ================================
do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"STL Export Options"), dialog_export(),
               fun(Res) ->
                   {file,{Op,{stl,Res}}}
               end);
do_export(Attr0, _Op, Exporter, _St) when is_list(Attr0) ->
    %% we store only the selected row id. The table control
    %% will return the entire list plus the selection.
    {value,{slicer,_},Attr} = lists:keytake(slicer,1,Attr0),
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Ps = [{subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
        export(Filename, Contents, Attr)
    end.
set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

dialog_export() ->
    Hook = fun(Key, Value, Fields) ->
            {Sel,Lists} = Slicer = wings_dialog:get_value(slicer,Fields),
            case Key of
                slicer ->
                    wings_dialog:set_value(slicer_id,Sel,Fields),
                    case Sel of
                        [0] ->
                            ObjScale = 1.0,
                            ConvScale = 1.0,
                            wings_dialog:set_value(wu_equals,stl,Fields),
                            wings_dialog:set_value(obj_scale,ObjScale,Fields),
                            wings_dialog:set_value(conv_scale,ConvScale,Fields),
                            wings_dialog:enable(pnl_slicer,false,Fields),
                            wings_dialog:enable(obj_scale,false,Fields);
                        _ ->
                            ObjScale = wings_dialog:get_value(obj_scale,Fields),
                            ConvScale = comput_scale(wings_dialog:get_value(wu_equals,Fields),Slicer)
                    end,
                    wings_dialog:set_value(conv_scale,ConvScale,Fields),
                    wings_dialog:set_value(export_scale,ObjScale*ConvScale,Fields);
                wu_equals ->
                    %% workaround to set the previous selected item on table since
                    %% the table control doesn't allow us to set its initial value
                    SlicerId = wings_dialog:get_value(slicer_id,Fields),
                    ObjScale = wings_dialog:get_value(obj_scale,Fields),
                    case Value of
                        stl ->
                            %% setting table default selection to Wings3D scale
                            ConvScale = 1.0,
                            SlicerCtrl = wings_dialog:get_widget(slicer,Fields),
                            wxListCtrl:setItemState(SlicerCtrl,0,?wxLIST_STATE_SELECTED,?wxLIST_STATE_SELECTED);
                        _ ->
                            %% computing the conversion scale based on slicer preset
                            case Sel of
                                [] when SlicerId=/=[], Value=/=stl ->
                                    [Id] = SlicerId,
                                    SlicerCtrl = wings_dialog:get_widget(slicer,Fields),
                                    wxListCtrl:setItemState(SlicerCtrl,Id,?wxLIST_STATE_SELECTED,?wxLIST_STATE_SELECTED),
                                    ConvScale = comput_scale(Value, {SlicerId,Lists});
                                [_] ->
                                    ConvScale = comput_scale(Value,Slicer)
                            end
                    end,
                    wings_dialog:set_value(conv_scale,ConvScale,Fields),
                    wings_dialog:set_value(export_scale,ObjScale*ConvScale,Fields),
                    wings_dialog:enable(pnl_slicer,Value=/=stl,Fields),
                    wings_dialog:enable(conv_scale,false,Fields),
                    wings_dialog:enable(obj_scale,Value=/=stl,Fields),
                    wings_dialog:update(pnl_slicer,Fields);
                obj_scale ->
                    ConvScale = wings_dialog:get_value(conv_scale,Fields),
                    wings_dialog:set_value(export_scale,Value*ConvScale,Fields)
            end
        end,

    [{vframe, [
        {label_column, [
            {"1 Wings3D Unit (WU) equal to",
             {menu,[
                 {"1 STL unit",stl},
                 {"1 mm",mm},
                 {"1 cm",cm},
                 {"1 inch",inch}],wpa:pref_get(?MODULE, wu_equals, stl),[{key,wu_equals},{hook,Hook}]}
            }
        ]},
      {vframe, [
        {vframe, [
          {label, ?__(4,"Below you have some scale information to get 1WU = 1mm\n"
                        "in accord with information we got from Wings3D's users.\n\n"
                        "Select a row to compute the 'Export scale' value:")},
          {table,[{"Slicer Software", "1WU", "Scale"},
                  {{0,"default Wings3D settings"},{1.0,"1STL unit"},{1.0,"1.0"}},
                  {{1,"Chitubox"},                {1.0,"1mm"},      {1.0,"1.0"}},
                  {{2,"Cura (Ultimaker)"},        {1.0,"1mm"},      {1.0,"1.0"}},
                  {{3,"ideaMaker (Raise3D)"},     {1.0,"1mm"},      {1.0,"1.0"}},
                  {{4,"Makerbot Print"},          {1.0,"1mm"},      {1.0,"1.0"}},
                  {{5,"Repetier Host"},           {1.0,"1mm"},      {1.0,"1.0"}},
                  {{6,"Slic3r"},                  {1.0,"1mm"},      {1.0,"1.0"}},
                  {{7,"Tinkerine™ Suite"},        {1.0,"1mm"},      {1.0,"1.0"}},
                  {{8,"Up Studio (Tiertime)"},    {25.4,"1inch"},   {0.0393700787401575,"0.0393701"}},
                  {{9,"Z-Suite (Zortrax)"},       {1.0,"1mm"},      {1.0,"1.0"}},
                  {{10,"3DWOX Desktop (Sindoh)"}, {1.0,"1mm"},      {1.0,"1.0"}}
          ],[{key,slicer},{sel_style,single},{hook,Hook},
             {max_rows,4},{col_widths,{20,7,8}}]},
          {value,wpa:pref_get(?MODULE, slicer_id, []),[{key,slicer_id}]}
        ],[{margin,false},{key,pnl_slicer}]},
        {label_column, [
            {?__(7,"Conversion scale"),
             {text,1.0,[{key,conv_scale},{range,{0.0,infinity}}]}},
            {?__(8,"Object scale"),
             {text,wpa:pref_get(?MODULE,obj_scale,1.0),[{key,obj_scale},{range,{0.0,infinity}},{hook,Hook}]}}
        ]}
      ],[{title," "++?__(9,"Slicer software references")++" "},{margin,false}]},
      {label_column,
       [{?__(2,"Export scale"),
         {text,wpa:pref_get(?MODULE, export_scale, 1.0),
          [{key,export_scale},{range,{0.0,infinity}}]}},
        {?__(3,"Sub-division Steps"),
         {text,wpa:pref_get(?MODULE, subdivisions, 0),
          [{key,subdivisions},{range,{0,4}}]}}
       ]}
     ]}
    ].

comput_scale(WUeq, {Sel,List}) ->
    case Sel of
        [Idx] ->
            Row = element(Idx+1,list_to_tuple(List)),
            {Scale,_} = element(3,Row),
            %% Scale is defined in millimeters, so we convert in accord with user selection
            case WUeq of
                mm -> Scale;
                cm -> Scale*10.0;
                inch -> Scale*25.4;
                _ -> 1.0
            end;
        _ -> 1.0
    end.

export(FileName, Contents0, Attr) ->
    Contents = export_transform(Contents0, Attr),
    STL = make_stl(Contents),
    file:write_file(FileName, STL).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

make_stl(Contents) ->
    #e3d_file{objs=Objs,creator=Creator} = Contents,
    StlHead = make_header(Creator),
    CombinedObj = combine_objs(Objs),
    StlBody = make_body(CombinedObj),
    [StlHead|StlBody].

combine_objs(Objs) ->
    VFs = [get_vf_lists(Obj) || Obj <- Objs],
    {AllVs,AllFs} = lists:unzip(VFs),
    Nvs = lists:map(fun erlang:length/1, AllVs),
    Incs = get_incs(Nvs),
    Vs = lists:append(AllVs),
    Fs = lists:append(reindex_faces(Incs, AllFs)),
    {Vs,Fs}.

get_incs(L) ->
    A = lists:reverse(L),
    B = make_incs(A),
    lists:reverse(B).

make_incs([]) -> [];
make_incs(L) ->
    [_|T] = L,
    [lists:sum(T) | make_incs(T)].

get_vf_lists(Obj) ->
    #e3d_object{obj=Mesh} = Obj,
    #e3d_mesh{vs=Vs,fs=Fs} = e3d_mesh:triangulate(Mesh),
    {Vs, e3dfaces_to_faces(Fs)}.

reindex_faces([], []) -> [];
reindex_faces(Incs, AllFs) ->
    Add = fun(X) -> fun(Y) -> X+Y end end,
    [Inc|RestIncs] = Incs,
    [Fs|RestFs] = AllFs,
    [[lists:map(Add(Inc), F) || F<-Fs] | reindex_faces(RestIncs, RestFs)].

make_header(Creator) -> % An 80 Byte Header
    A = "Exported from " ++ Creator,
    B = "\nSTL plugin written by Anthony D'Agostino\n2005",
    C = list_to_binary([lists:sublist(A, 80-length(B)), B]),
    true = byte_size(C) =< 80, % Assertion.
    <<C/binary, 0:((80-byte_size(C))*8)>>.

make_body({Vs, Fs}) ->
    NumFaces = <<(length(Fs)):32/little>>,
    RawTriangles = e3d_util:indexed_to_raw(Vs, Fs),
    [NumFaces|raw_triangles_to_bin(RawTriangles)].

raw_triangles_to_bin(RawTriangles) ->
    VertToBinary = fun(Vertex) ->
	{X,Y,Z} = Vertex,
	<<X:32/float-little, -Z:32/float-little, Y:32/float-little>> end,
    TriToBinary = fun(Triangle) ->
	FaceNorm = <<0:32/float-little, 0:32/float-little, 0:32/float-little>>,
	FaceVerts = lists:map(VertToBinary, Triangle),
	FacePad = <<0:16>>, % can be used for 16-bit RGB color extension
	[FaceNorm, FaceVerts, FacePad] end,
    lists:map(TriToBinary, RawTriangles).

%%% ================================
%%% === StereoLithography Import ===
%%% ================================
stl_import(Name) ->
    case file:read_file(Name) of
    {ok,<<"solid", Bin/binary>>} ->
	print_boxed("FileName: " ++ Name),
	{Vs,Fs} = read_stl_ascii(Bin),
	Res = import(Vs,Fs),
	Res;
    {ok,Bin} ->
	print_boxed("FileName: " ++ Name),
	{Vs,Fs} = read_stl(Bin),
	Res = import(Vs,Fs),
	Res;
    {error,Reason} ->
	{error,file:format_error(Reason)}
    end.

import(Vs,Fs) ->
    Efs = faces_to_e3dfaces(Fs),
    Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs},
    Obj = #e3d_object{name="STL object",obj=Mesh},
    {ok, #e3d_file{objs=[Obj]}}.

read_stl_ascii(Data) ->
    RawTriangles = case re:run(Data, "facet normal\\s+\\S+\\s+\\S+\\s+\\S+\\s+"
				     "outer loop\\s+"
				     "vertex\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+"
				     "vertex\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+"
				     "vertex\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+"
				     "endloop\\s+"
				     "endfacet",
			       [global,{capture,all_but_first,list}]) of
			{match, Triangles} ->
			    lists:map(fun([X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3]) ->
					    [{str2float(X1), str2float(Y1), str2float(Z1)},
					     {str2float(X2), str2float(Y2), str2float(Z2)},
					     {str2float(X3), str2float(Y3), str2float(Z3)}]
				      end, Triangles);
			_ ->
			    []
		   end,
    e3d_util:raw_to_indexed(RawTriangles).

str2float(Str) ->
    wings_util:string_to_float(Str).

read_stl(Data) ->
    <<Header:80/binary, NumFs:32/little, Rest/binary>> = Data,
    print_header(Header),
    Raw_Triangles = read_raw_triangles(Rest),
    {Vs,Fs} = e3d_util:raw_to_indexed(Raw_Triangles),
    NumFs = length(Fs), % Assertion.
    {Vs,Fs}.

print_header(Header) ->
    Head1 = binary_to_list(Header),
    Head2 = [Char || Char <- Head1, Char>0, Char<127],
    io:fwrite("~s\n", [Head2]).

read_raw_triangles(<<>>) -> [];
read_raw_triangles(Data) ->
    <<_:12/binary, % Face Normal
    X1:32/float-little, Y1:32/float-little, Z1:32/float-little,
    X2:32/float-little, Y2:32/float-little, Z2:32/float-little,
    X3:32/float-little, Y3:32/float-little, Z3:32/float-little,
    _:2/binary, % Padding
    Rest/binary>> = Data,
    [[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}] | read_raw_triangles(Rest)].

%%% ============
%%% === Misc ===
%%% ============
print_boxed(String) ->
    A = length(String),
    io:fwrite("\n", []),
    io:fwrite("+-~s-+\n", [lists:duplicate(A, "-")]),
    io:fwrite("| ~ts |\n", [String]),
    io:fwrite("+-~s-+\n", [lists:duplicate(A, "-")]).

e3dfaces_to_faces(E3dFaces) ->
    [FaceVs || #e3d_face{vs=FaceVs} <- E3dFaces].

faces_to_e3dfaces(Faces) ->
    [#e3d_face{vs=Face} || Face <- Faces].

