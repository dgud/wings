- Convert all windows to be real windows instead of OpenGL drawn.
  Windows can be attached and detached from main window.
  [dgud] with many help/debugging/bug-fixes from [micheus]

- Added a new Subdivide(RMB) command to be side-by-side with Smooth (Catmull Clark) command.
  Suggested by Arg Arg. [micheus]

- Added a new bridge(RMB) with reference points. [micheus]

- Fixed a crash caused by bad formated text for "How To Define Hotkeys"
  instructions in language files and updated their description to the new
  process sequence. Thanks to tkbd. [micheus]

- Fixed a crash caused by renumber routine. That was noticed when a
  material map references a face that no longer existed after a dissolve
  command has been ran. Thanks to ptoing. [micheus]

- After execute a merge action the title was not showing the file was
  changed and a save action is required. Thanks to oort. [micheus]

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

- Povray improvements [micheus]
