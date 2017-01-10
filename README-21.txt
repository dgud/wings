--- 2.1.5 --------------------------------------------
- Added Make Normal-Map command, in outliner.
  And improved bump-map to normal-map calculations. [Micheus]

- Keyboard shortcuts could sometimes be invoked twice [dgud]

- Outliner could get keyboard focus after some commands though
  the window was not marked as focused. Fixes focus issues. [dgud]

- Choosing 'Loaded Font Glyphs' from Development menu was causing
  Wings3D crash. Thanks to Fonte Boa [Micheus]

- Fixed the visualization of Tweak "Show Magnet Influence" was not
  working. Thanks to Fonte Boa. [Micheus]

- Invalid ps/eps file (absent or invalid token) was causing Wings3D crash.
  Thanks to tkbd. [Micheus]

- Fixed broken TGA image loader, wx's targa loader, fails with RLE
  encoding, bug is fixed but not released workaround by using wings
  own tga loader. Reported by tkbd. [dgud]

- Fixed loading files from root dir on Windows [Micheus].

- Code cleanup and Optimizations [bjorng] [dgud]

--- 2.1.4 --------------------------------------------
- Fix crash in autouv when doing geometry changes from the autouv
  window via keyboard shortcuts. Now the commands only work if selection
  mode is the same in both windows. Reported by Fonte Boa. [dgud]

- Bridge-RMB was crashing for two faces selected in the same object.
  Thanks to Fonte Boa. [Micheus]

- After select multiple views and activate the RMB Wings3d crashes.
  Thanks Fonte Boa [micheus]

- It was impossible to define hotkeys in Sculpt mode.
  Thanks to Fonte Boa. [Micheus]

- Performance improvements. [dgud]

- Selection display in ortho mode (and autouv).
  Reported by Loefet and kugelfang. [dgud]

- Fix olive green theme, report by tkbd. [dgud]

- Fix crash at start if username contained unicode chars,
  reported by infinder. [dgud]

- In GeometryGraph if user accidentally put the folder name in edit mode and
  then cancel it Wings3D was displaying a wrong message. Thanks tkbd. [Micheus]

- The Geometry Graph window closes unexpectedly in some folder/item
  operations. Avoid the crash and its cause should now
  be appended to Log window. Thanks to tkbd [Micheus]

--- 2.1.3 --------------------------------------------
- Fix dragging images in outliner on mac and linux [dgud]

- Add torus to correct place in menu when in Snap Image mode
  We can't add it to the second place when auv_snap image is activated,
  the torus comes in mixed with the snap commands. [dgud]

- Fix octotoad scaling during creation, report by tkbd. [dgud]

- Fix hotkey handling of keys F1-F15, reported by Fonteboa [dgud]

- Fix displaying selections in secondary selection mode, reported by tkbd [dgud]

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
