--- 2.1.1 --------------------------------------------

- Fixed starting wings with file argument, either double clicking
  on wings file on windows or dragging file to wings app on mac.
  Reported by sandman. [dgud]

- Auto scroll outliner when dragging images.
  Suggested by oxo. [dgud]

- Add show/hide toolbar option in preferences.
  Reported by Fonte Boa. [dgud]

- Tweak magnet adjust did not stop when key was released.
  Reported by Fonte Boa. [dgud]

- Press shift to disable docking window when moving.
  Suggested by many. [dgud]

- Rework drawing edges and selection to fix problems on some
  graphic cards and drivers, i.e. Macs with old Intel gfx cards.
  Debugged by tkdb [dgud]

- Indicate used magnet type, and fix status bar issues on Mac.
  Reported by tkbd. [dgud]

- Improved dialog handling on Mac. [dgud]

- Improve camera and mouse drag performance. [dgud]

- Various redraw & refresh improvements. [dgud]

--- 2.1 ---------------------------------------------

- Large gui rewrite
  Convert all windows to be real windows instead of OpenGL drawn.
  Windows can be attached and detached from main window.
  [dgud] with much help teseting, debugging and bug-fixes from [micheus]

- Added a new Subdivide(RMB) command to be side-by-side with Smooth (Catmull Clark) command.
  Suggested by Arg Arg. [micheus]

- Added a new bridge(RMB) with reference points. [micheus]

- Collada (.dae) improvments
  - Importer implemented
  - Exports vertex normals
  [dgud]

- Added new connect and slide command for Connect RMB in Edge mode.
  Suggested by tkbd. [micheus]

- Camera dialog improvments [micheus]
  - New formats
  - Fixed bugs
  - Image plane preview

- Added edge loop nth selection command [micheus] & [dgud]

- Povray improvements [micheus]

- Fixed a crash caused by bad formated text for "How To Define Hotkeys"
  instructions in language files and updated their description to the new
  process sequence. Thanks to tkbd. [micheus]

- Fixed a crash caused by renumber routine. That was noticed when a
  material map references a face that no longer existed after a dissolve
  command has been ran. Thanks to ptoing. [micheus]

- After execute a merge action the title was not showing the file was
  changed and a save action is required. Thanks to oort. [micheus]
