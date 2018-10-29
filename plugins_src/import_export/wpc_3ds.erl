%%
%%  wpc_3ds.erl --
%%
%%     3ds max import/export.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_3ds).

-export([init/0,menu/2,command/2]).

-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(DEF_IMAGE_TYPE, ".bmp").

init() ->
    wpa:pref_set_default(?MODULE, swap_y_z, true),
    wpa:pref_set_default(?MODULE, default_filetype, ?DEF_IMAGE_TYPE),
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{tds,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{tds,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{tds,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"3D Studio (.3ds)...",tds,[option]}].

props() ->
    [{ext,".3ds"},{ext_desc,?__(1,"3D Studio File")}].

%%%
%%% Import.
%%%

do_import(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"3D Studio Import Options"), dialog(import),
	       fun(Res) ->
		       {file,{import,{tds,Res}}}
	       end);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import(props(), import_fun(Attr), St).

import_fun(Attr) ->
    fun(Filename) ->
	    case e3d_tds:import(Filename) of
		{ok,E3dFile0} ->
		    E3dFile = import_transform(E3dFile0, Attr),
		    {ok,E3dFile};
		{error,Error} ->
		    {error,Error}
	    end
    end.

%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"3D Studio Export Options"), dialog(export),
	       fun(Res) ->
		       {file,{Op,{tds,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Uvs = proplists:get_bool(include_uvs, Attr),
    %% If smoothing groups are not wanted, we'll turn off
    %% export of hard edges. That will create only one smoothing group.
    HardEdges = proplists:get_bool(include_normals, Attr),
    Ps = [{include_uvs,Uvs},{include_hard_edges,HardEdges},
	  {subdivisions,SubDivs},{include_hard_edges,HardEdges}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Filetype = proplists:get_value(default_filetype, Attr, ?DEF_IMAGE_TYPE),
    Contents1 = export_transform(Contents0, Attr),
    Contents2 = make_tx_uniq(Contents1),
    Contents = wpa:save_images(Contents2, filename:dirname(Filename), Filetype),
    case e3d_tds:export(Filename, Contents) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

dialog(Type) ->
    [wpa:dialog_template(?MODULE, Type, [include_colors])].

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = wpa:import_matrix(Attr),
    e3d_file:transform_matrix(Contents, Mat).

%%%
%%% Shorten image names to 8 letters and make sure that
%%% all images name are unique.
%%%

make_tx_uniq(#e3d_file{mat=Mat0}=E3DFile) ->
    Mat = make_tx_uniq_1(Mat0),
    E3DFile#e3d_file{mat=Mat}.

make_tx_uniq_1(Mat) ->
    Names = foldl(fun({_,Ps}, A) ->
			  case get_map(diffuse, Ps) of
			      none ->
				  A;
			      #e3d_image{filename=none,name=Name} ->
				  [Name|A];
			      #e3d_image{filename=Name0} ->
				  Name = filename:rootname(filename:basename(Name0)),
				  [Name|A]
			  end
		  end, [], Mat),
    MapTrans0 = e3d_util:make_uniq(Names, 8),
    MapTrans = gb_trees:from_orddict(sort(MapTrans0)),
    map(fun({N,Ps0}=M) ->
		case get_map(diffuse, Ps0) of
		    none -> M;
		    #e3d_image{filename=none,name=Name0}=Image0 ->
			Name = gb_trees:get(Name0, MapTrans),
			Image = Image0#e3d_image{name=Name},
			Ps = replace_map(diffuse, Image, Ps0),
			{N,Ps};
		    #e3d_image{filename=Name0}=Image0 ->
			Base = filename:rootname(filename:basename(Name0)),
			case gb_trees:get(Base, MapTrans) of
			    Base ->
				{N,Ps0};
			    Name ->
				Image = Image0#e3d_image{filename=none,
							 name=Name},
				Ps = replace_map(diffuse, Image, Ps0),
				{N,Ps}
			end
		end
	end, Mat).

get_map(Type, Ps) ->
    Maps = proplists:get_value(maps, Ps, []),
    proplists:get_value(Type, Maps, none).

replace_map(MapType, Val, Ps) ->
    Maps0 = proplists:get_value(maps, Ps, []),
    Maps = [{MapType,Val}|keydelete(MapType, 1, Maps0)],
    keyreplace(maps, 1, Ps, {maps,Maps}).
