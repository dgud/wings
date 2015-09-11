--- 2.0.1 ----------------------------------------------------------------------

- Added OpenGL version check, for future version requirements [bjorng]

- Unselected vertex size was not changed after changing it in the
  Preferences. [micheus]

- Fixed color selection for lights by using wings color selector
  when alpha value is required. [dgud]

- Added support for loading multiple images in Import Image command. [micheus]

- Fixed labels and textbox aligment for Absolute Command->Move/Scale [micheus]

- 1) Yafaray plugin update:
  - Reworked in the layout of the Yafaray "Render Options" dialog;
  - made some changes asked by oort;
  - fixed some bugs found by oort
  - updated the help information for material and lights;

  2) POV-Ray plugin update:
  - fixed some controls alignments

  3) Kerkythea plugin update:
  - fixed some controls alignments

  4) wpc_absolute_move.erl
  - fixed control alignments

  5) wings_light.erl
  - fixed alignment [micheus]

- Fixed double click drag selection [dgud]

- Fixed other minor problems reported in the forum including
 - Log/Console window related issues
 - Linux installer problems
 - Set keyboard focus in dialogs
 - Multisampling preference
 [dgud]

--- 2.0 ----------------------------------------------------------------------

- Rewrote to use wx as backend for graphics
  It allows us to use more (native) widgets in the future and
  copy-paste in text widgets, native fonts and so on.
  Much have been rewritten so much can be broken,
  please report bugs in the forum.
  [dgud] and [Micheus]

- Removed/Changed some features that was hard to port to wx
   Setting/removing keyboard shortcuts have change, see help.
   Preview rendering is always on.
   Right Click menu icons have been removed.

- Changed the installer for all OS'es, to behave more like native apps.
-- Linux: includes desktop shortcuts with icons.
-- Windows: allows the wings to moved after install
-- Mac: More native apperance, menu in the top bar

- Added edge/hardness/invert command [Mark Whittemore]

- Removed YafRay plugin and added a new YafaRay plugin [oort]
  Ported all render plugins to use the new gui. [Micheus]

- Added a file merge dialog so the user can choose what to import. [micheus]
