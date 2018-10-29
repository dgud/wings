%%
%%  wpc_stl.erl --
%%
%%     Binary StereoLithography File Format (*.stl) Import/Export
%%
%%  Copyright (c) 2005-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_stl).
-export([init/0, menu/2, command/2]).
-include_lib("wings/e3d/e3d.hrl").

init() ->
    true.

menu({file, import}, Menu) ->
    menu_entry(Menu);
menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file, {import, stl}}, St) ->
    Props = props(),
    wpa:import(Props, fun stl_import/1, St);
command({file, {export, stl}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file, {export_selected, stl}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"StereoLithography (.stl)...", stl}].

props() ->
    [{ext, ".stl"},{ext_desc, "StereoLithography Binary File"}].

%%% ================================
%%% === StereoLithography Export ===
%%% ================================
export(FileName, Contents) ->
    STL = make_stl(Contents),
    file:write_file(FileName, STL).

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
    {ok,Bin} ->
	print_boxed("FileName: " ++ Name),
	Res = import(Bin),
	Res;
    {error,Reason} ->
	{error,file:format_error(Reason)}
    end.

import(Data) ->
    {Vs,Fs} = read_stl(Data),
    Efs = faces_to_e3dfaces(Fs),
    Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs},
    Obj = #e3d_object{name="STL object",obj=Mesh},
    {ok, #e3d_file{objs=[Obj]}}.

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

