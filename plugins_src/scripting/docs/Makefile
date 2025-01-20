#
#  Makefile --
#
#     Makefile for building scripting documentation
#
#  Copyright (c) 2025 Edward Blake
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

AWK=awk
MKDIR=mkdir -p
PACK_MANUAL=../../../tools/pack_manual

FILES= \
	en/INDEX \
	en/README.txt \
	en/wscr/intro.txt \
	en/wscr/langfiles.txt \
	en/wscr/pluginscripts.txt \
	en/wscr/query.txt \
	en/wscr/scriptfolders.txt \
	en/wscr/directives/type.txt \
	en/wscr/directives/name.txt \
	en/wscr/directives/mode.txt \
	en/wscr/directives/desc.txt \
	en/wscr/directives/include.txt \
	en/wscr/directives/params_title.txt \
	en/wscr/directives/params_templates.txt \
	en/wscr/directives/template.txt \
	en/wscr/directives/params.txt \
	en/wscr/directives/param.txt \
	en/wscr/directives/extensions.txt \
	en/wscr/directives/params_init.txt \
	en/wscr/directives/params_set.txt \
	en/wscr/directives/export_param.txt \
	en/wscr/directives/import_param.txt \
	en/wscr/directives/adv_param.txt \
	en/wscr/directives/params_preview.txt \
	en/wscr/directives/command_changes.txt \
	en/wscr/directives/command_inputs.txt \
	en/wscr/directives/extra_file.txt \
	en/wscr/directives/script_param.txt \
	en/wscr/directives/hradio.txt \
	en/wscr/directives/menu.txt \
	en/wscr/directives/browse.txt \
	en/wscr/directives/checkbox.txt \
	en/wscr/directives/vradio.txt \
	en/scheme/command_scripts.txt \
	en/scheme/exporter_scripts.txt \
	en/scheme/importer_scripts.txt \
	en/scheme/intro.txt \
	en/scheme/langres.txt \
	en/scheme/new_shape_scripts.txt \
	en/scheme/functions/e3d_face.txt \
	en/scheme/functions/e3d_file.txt \
	en/scheme/functions/e3d_image.txt \
	en/scheme/functions/e3d_mesh.txt \
	en/scheme/functions/e3d_object.txt \
	en/python/command_scripts.txt \
	en/python/exporter_scripts.txt \
	en/python/importer_scripts.txt \
	en/python/intro.txt \
	en/python/langres.txt \
	en/python/new_shape_scripts.txt \
	en/python/functions/e3d_face.txt \
	en/python/functions/e3d_file.txt \
	en/python/functions/e3d_image.txt \
	en/python/functions/e3d_mesh.txt \
	en/python/functions/e3d_object.txt \
	en/python/functions/list_of_arrays.txt \
	en/python/functions/list_of_tuples.txt \
	en/python/functions/material.txt \
	en/python/functions/material_maps.txt \
	en/python/functions/material_opengl.txt \
	en/python/functions/new_shape.txt \
	en/python/functions/output_list.txt

SCRIPTINITDIR=../../../plugins/scripting/wpc_scripting_shapes_init

all: ${SCRIPTINITDIR}/scripting-reference-en.manual

clean:
	rm generated/en/scm-we.txt
	rm generated/en/py-we.txt
	rmdir generated/en
	rmdir generated
	

generated/en:
	$(MKDIR) generated/en

generated/en/scm-we.txt: generated/en funs
	$(AWK) -v type=scm -f ../tools/gen-funs-doc.awk < funs > generated/en/scm-we.txt

generated/en/py-we.txt: generated/en funs
	$(AWK) -v type=py -f ../tools/gen-funs-doc.awk < funs > generated/en/py-we.txt

${SCRIPTINITDIR}/scripting-reference-en.manual: $(FILES) \
                               generated/en/scm-we.txt \
                               generated/en/py-we.txt
	$(PACK_MANUAL) en \
             --out ${SCRIPTINITDIR}/scripting-reference-en.manual



