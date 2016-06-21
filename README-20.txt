--- 2.0.5 ----------------------------------------------------------------------

- Updates to the Japanese translation [tkbd]

- Fixed a hotkey issue that was not enabling replace a previous binded key as
  well as it was ignoring the hotkey deletion. Thanks ggaliens. [micheus]

- Added logic that allow the Yafaray - after the engine path be provided -
  get its render option available under File->Render option whithout need to
  restart Wings3D. Thanks oort for ask about it. [micheus]

- Fixed the problem with crash in the Ambient Occlusion plug-in. [Björn Gustavsson]

- Fixed the bad text formatting in the module wpc_constraints that was
  causing Wing3d crash. Thanks to tkbd. [micheus]

  Also, as suggested by the user tkbd, it was added a message dialog that
  shows to the user the message about the constraint that just has been set. [micheus]

- In OSX, when merge dialog is shown the mouse cursor becomes a clock.
  Thanks to tkbd. [micheus]

- Handle filename with unicode characters, reported by beng27 [dgud]

- Moving the mouse over 'Various Plans' dialog icon was causing Wings3d
  crach. Thanks to tkbd. [micheus]

- Fixed other hotkey issues. [dgud]

--- 2.0.4 ----------------------------------------------------------------------

- Further updates of the German translation by Roy. [Björn Gustavsson]

- Improved connect cut command to work in more cases [dgud]

- Fix memory duplication when opening face/edge menu. [dgud]

- Fixed a crash caused by the format_hotkey routine when an error was
  handled in wings_hotkey module. Thanks ggaliens. [micheus]

- Update French translation. [Enzo]

--- 2.0.3 ----------------------------------------------------------------------

- Added possibilty to cut and connect 2 or 3 vertices with MMB
  [Mark Whittemore] [dgud]

- Fixed the crash/hang caused by the slide color control. [micheus]

- There was a strange black line being drawn in the background starting from
  mini axis. Thanks to Asticles for report it. [Micheus]

- Fixed fragment shader code for Marble and Noise that was causing
  render crash in texture creation. Thanks tkbd. [micheus]

- Fixed tweak information line and hotkey handling back to as it in
  previous releases.  Thanks Fonte Boa. [micheus]

- It was not possible set a hotkey for commands assigned to
  RMB. Thanks Fonte Boa [micheus]

- The German translation has been updated by Roy. [bjorng]

- Fixed typo in absolute move, thanks Loefet [dgud]

- Fixed crash in plugin-manager, thanks tkbd [dgud]

- Fixed installation of vcredist package from Microsoft. [dgud]

- Some keyboard fixes on Mac

- Improved EPS/SVG Exporter [tkbd]

--- 2.0.2 ----------------------------------------------------------------------

- Fixed the background color exported when a Ambient light is used. Thanks oort;

- Fixed a crash caused by editing an old projects that a material can contain
  modulators with values out-of-range; Thanks oort.

- Fixed a bad path formation caused by a missing condition in the routine to
  get relative paths used by POV-Ray plugin. [micheus]

- Fixed the sub- menu location for the "Drop picked object" option.
  In the current implementation it seems there is no need for translate the
  cursor cordinate. Thanks oort; [micheus]

- Fixed the cause of Wings3d crash when 'Enable Develop Menu' was
  enable/disable. Thanks oort; [micheus]

- Fixed the missed control over the transparency color of magnet
  in the Tweak tool. [micheus]

- Fixed the missed dialog prompt action before overriden a file. Thanks oort.
  - Fixed wrong beharviour when trying to save an untitled file. Thanks tkbd. [micheus]

- Added help content to Yafaray Export dialog. by oort;
  - Fixed Transparency Refraction option in the Yafaray plugin. by oort; [micheus]

- Fix autouv texture generation, reported by tkbd.
  Many bugfixes and new shaders [micheus, dgud]

- For two mouse buttons settings the menu was ignoring the combo CTRL+RMB
  and was acting just like a RMB. Thanks to ggaliens. [micheus]

- Various other bugs reported on the forum. [dgud, micheus]

- Rewrote the internal rendering to use vbo instead of display lists. [bjorn]

- Fixed keyboard handling on mac. [bjorn]

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
