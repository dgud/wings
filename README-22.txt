--- 2.3 --------------------------------------------

- Added option to rotate the environment map. [micheus]

- Fixed misplaced context menu in a multiple display setup. [micheus]

- Fixed Align command in AutoUV editor that was not working properly. [micheus]

- Added Proportional option to the Distribution command in AutoUV editor. [micheus]

- Replaced the label 'Radial' by 'Radius' in Sphere primitive dialog.
  Thanks to e.m.hobo for the suggestion. [micheus]

- Fixed a crash when closing a window with the context menu active. Thanks to markie. [dgud]

- Fixed a crash when renaming screenshots. Thanks to markie. [micheus]

- Fixed the Connect RMB command that was entering in slide mode after no
  connection has been done. [micheus]

- Fixed a soft crash caused by selectiong MMB for Flow Connect command [micheus]

- Fixed the Collapse command that was creating wrong topolgy in
  face mode. Thanks to ptoing. [micheus]

- Fixed NormalMap rendering, thanks Markie. [dgud]

- Updated the splash screen image whth the one choosen in the contest.
  Congrats to user chosetec. [micheus]

- Fixed a crash which happened during start on some Intel GPUs when MSAA was
  disabled in a PC setup. [Micheus]

- Fixed the bind DELETE key after it has been unbound. Thanks to ptoing. [micheus]

- Added more import and export features to palette window. Fixed right
  click when outside palette square. Thanks tkbd and micheus for
  suggestions and testing. [Edward Blake]

- Fixed a division by zero crash in AutoUV when very tiny islands
  exists. Thanks tkbd [micheus]

- The file type in the file dialog now is in accordance with the glTF type
  choosen in the option dialog. Thanks to markie [micheus]

- Rewrote OpenGL window handling to make it work with EGL on linux,
  and various other linux fixes. [dgud]

- Fixed a bug in transparency rendering. [dgud]

- Lots of improvements in various Importers/Exporters by Edward Blake.
   Import DirectX (.x) files.
   Added X3D export, adding X3D and VRML import.
   SVG additions and improvments.
   Added WMF and EMF path import plugin.
   Improved Adode Postscript support.

- The file type in the file dialog now is in accordance with the glTF type
  choosen in the option dialog. Thanks to markie [micheus]

- Added Bend command to AutoUV toolset. [micheus]

- New primitive Oblong [micheus]

- Fixed a crach in autouv when rotating a char with multiple edges selected. [micheus]

- When objects and materials was including invalid charactes the .dae file
  couldn't be imported by other applications. Thanks to Wҽɳԃιҽ Bʅαƈƙƚԋσɾɳ [micheus]

- For the first time, there is a now separate Wings package
  optimized for Macs with an M1 or M2 chip (Apple Silicon). [bjorng]

- New Duplicate Radial command. [micheus]

--- 2.2.9 --------------------------------------------

- Drag and drop file feature was not importing files with uppercase file
  extensions. Thanks to rgcotl at Discord. [micheus]

- Fixed autouv crash in create_texture dialogs. [dgud]

- Fixed marquee transparency on Linux. Thanks to Klim. [micheus]

- Added zoom option to wings camera mode. [dgud]

- Fixed material dialog crash. [micheus]

- Updated geometry graph icons. [dgud]

- Fixed startup crash. [dgud]

- Fixed crash when creating menu. [dgud]

--- 2.2.8 --------------------------------------------

- Fixed a crash caused by checking image name. Thanks to rgcotl (at Discord) [micheus]

- Rewrote grid drawing. [dgud]

- Fixed pale colors for materials and vertex colors. [dgud]

- New splash screen image. Congrats to Hank. [micheus]

- Uses Erlang/OTP-24 in pre-built versions, which should give a performance boost. [dgud]

- The material properties for plugins are now accessed by choosing the
  plugin name in a drop-down list and clicking on the "Edit..." button. [micheus]

- Yafaray fixes:
-- Fixed Yafaray issues in both Material and Option dialogs. Thanks to oort [micheus]
-- Fixed the remaining non-latin characters handling in order to prevent
     further crashes in Yafaray exporter. Thanks to tkbd. [Micheus]
-- Small fixes to Yafaray plugin. Thanks to oort. [micheus]

--- 2.2.7 --------------------------------------------

New stuff

- Added "View Settings Window" where camera and light options can
  be edited. [dgud]

- Large auto-uv work
-- Added a new uv-unwrapping algorithm unfold for 64b versions (old is kept as slow). [dgud]
-- Added an option to AutoUV to export the UV mesh as .eps/.svg files. [micheus]
-- Added option to scale uniformly the UV to the max U or max V. [micheus]
-- Added Align option to AutoUV in order to make easier to align islands to each other [micheus]
-- Added triplanar and image mix shaders to AutoUV Shader. [micheus]
-- Added preview window for the shaders when generating textures. [micheus]

- A large re-write of the yafray plugin. Thanks David Bluecame [David Bluecame]

- Added missing option for Select menu that allows to lock the current
  selected objects. Thanks to tkbd. [micheus]

- Changed Separate command in order to keep all object selected.
  Thanks to tkbd for the suggestion [micheus]

- Added a greebles plugin [dgud]

- Added a dialog to STL exporter allowing users to set the scale.
  Thanks to dawntreader, imagine and Mert HANCIOGLU(instagram) for provide
  us information about the slicers measures. [Micheus]

- Added new primitive screw thread [Micheus]

- Folder system now shows in bold the folder which owns any selected objects; [micheus]

- Added a warning for Revert command on the  main menu File. [micheus]

- Decreased the near clipping value from 0.01 to 0.001;
  The ground grid is also now drawn for scales as 0.1, 0.01 and 0.001; [micheus]

- Added MMB option to Material menu item when Geometry window is in Face
  selection mode. Thanks Hank for the suggestion. [micheus]

- Do not attach temporary windows when moving them on top of wings. [dgud]

- New splash screen image by the user olve11. [micheus]

Bugs fixed

- WRML plugin was exporting inverted values for ambientIntensity and
  Transparency. Thanks to greg. [micheus]

- Fixed outliner hanging on linux, fixed lights nodes not
  expanding when redrawing. [dgud]

- Fixed crash when dropping images on geom window, thanks tkbd. [dgud]

- Fixed snap image window crash, thanks tkbd. [dgud]

- Fixed timing related crashes in outliner. Thanks Hank. [dgud]

- Run the Select->Similar command in an empty was causing Wings3D to
  display the 'Delete Hotkey' dialog. [micheus]

- Fixed the help windows visibility which were getting hidden when moving
  the mouse outside it. Thanks Grumbler. [micheus]

- Fixed the collada importer error caused by NaN values. Thanks to tkbd [micheus]

- Alignment commands not valid for Body or Face mode was causing the AutoUV
  window to crash when a repeat command was used in these modes. [micheus]

- Input dialogs to name/rename elements now is a little wider. Thanks tkbd
  for the suggestion. [Micheus]

- Fixed missing export parameters for SPPM and Bidirectional. Thanks oort [DavidBluecame]

- Lift command with LMB option was not working properly due the use of the
  selection state when it first was used. Thanks Hank for report it. [micheus]

- RMB menus have been rewritten again, should work better and faster on linux/windows
  [dgud]

--- 2.2.6 --------------------------------------------

- Error exporting .eps/.svg files using diacritical marks. Thanks Hank [micheus]

- The JSCAD plugin was rewritten to offer more flexibility. Thanks to the
  user gilboonet for his help. [micheus]

- Added the Tessellation option to the jscad exporter. [micheus]

- Improved Collada (.dae) import, thanks 'nigec' for good/small
  failing example files. [dgud]

- Added the toggle background option to the AutoUV window. [micheus]

- Context menu on Geometry can get options hidden. Thanks to nigec. [micheus]

- Binding a hotkey used by main menu was not removing it. Thanks yschaeff [micheus]

- Enable horizontal scroll to rotate view, improves laptop usage
  without mouse.  Can be used with options see:
  "Preferences/Camera/Wheel Pans and Rotates"
  and it's options. [dgud]

- Fixed marquee selection drawing on linux on undocked windows. [dgud]

- Fixed translation issues with "Edit Area Light" in body menu. [dgud]

- Improved TTF text rendering;
  - Better error reports for non supported formats
  - Fixed crash when rendering space character for some fonts
  - Improved font family name handling, so that more (correct) fonts will be found. [dgud]

- Added "/System/Library/Fonts/" to font search path on
  Mac. Thanks Tkbd. [dgud]

- Fixed the screenshot command which was only capturing the main
  geometry window and cutting it on top. [micheus]

- The right message used to show extra information to the user actions
  was missing with the previous message kept static. Thanks to Sevendy3 [micheus]

- Hexagon in texture shader was broken. Thanks to tkbd [micheus]

- Bad timing could cause the close button to hang forever. [dgud]

--- 2.2.5 --------------------------------------------

- Added wings_convert script for batch conversion of models. [dgud]

- Linux installer now accept an optional Install-Dir argument [dgud]

- When exporting ".gltf/.glb" swap_y_z and scale options was ignored. [dgud]

- Fixed importing non-square 'dds' textures with mipmaps
  which caused an eternal loop. Thanks Simon Griffiths. [dgud]

- Fixed crash when trying to edit area light properties
  and not selecting an area light. Thanks Lars Thorsen. [dgud]

- Fix ttf text plugin error handling. Thanks Hank. [dgud]

- Add more search dirs for fonts. [dgud]

- Rewrote "Text" primitive, it can now handle more truetype
  formats, collection files (.ttc) and opentype (.otf) files. [dgud]

- Fix lost text focus in dialogs. [dgud]

- Allow image drag'n'drop from outliner to autouv to change
  background image in autouv. [dgud]

- Fixed updating mipmaps when updating dds files. Thanks Vershner. [dgud]

- Fixed rendering bug when editing materials with vertex colors. Thanks Hank. [dgud]

- The option "File->Save Selected" was saving images not used by the
  selected object(s); Thanks Hank [Micheus Vieira]

- Added an exporter to OpenJSCAD file format (.jscad); [micheus]

- Added a minimal warp mouse camera workaround see 'Options/Misc/' tickbox.
  Enable it and test again if you have problems.

--- 2.2.4 --------------------------------------------

- Fixed View/Show Texture bug. [dgud]

- Fixed transparent rendering. [dgud]

- Added a fail-safe if OpenCL crashes during start,
  which will disable OpenCL for future runs. [dgud]

- Select by Material in Outliner window was not working in accord when in
  body selection mode. Thanks Hank. [micheus]

- Fixed some ui color settings. [micheus]

--- 2.2.3 --------------------------------------------

- Improved handling of patches and plugins.
  Now installs in the user data directory according to OS
  recommendations this avoids tampering with the original installation.
  I.e. on windows USERHOME/AppData/Local/Wings3D [dgud]

- Improve rendering of transparent faces in smooth preview. [dgud]

- Isometric view command [micheus]

- Upgraded to erlang/otp-21.2 on Windows which should improve performance on large models. [dgud]

- Boolean fixes [dgud]

- The submenu in 'AutoUV Segmentating' was shown in the wrong display for
  multiple monitors layout. Thanks to OXO. [micheus]

- Exporter fixes [Julian Richardson]

- Minor fixes in Outliner and Geom Graph windows. [dgud]

- Select an element with non latin character was causing Wings3D
  to crash. Thanks tkbd [micheus]

--- 2.2.2 --------------------------------------------

- Improved STL importer. [dgud]

- Fixed crash when zooming to a negative distance, thanks, Tkbd. [dgud]

- UV mapping windows are displayed in the same monitor as the first was.
  Thanks OXO. [micheus]

- Fixed hard crash caused by malformed faces. Thanks Hank. [micheus]

- Fixed the EPS/SVG plugin exporting back edges. Thanks to tkbd. [micheus]

- Fixed the crash caused by Tighten in AutoUV window. Thanks to Hank. [Micheus Vieira]

- Changing to a saved view in the middle of a vector input operation
  was causing Wings3D to crash. Thanks to Hank. [micheus]

- Wings3D was crashing when a material was selected by using it's color
  icon. Thanks to Hank. [micheus]

- Selection groups has not been saved for hidden/locked objects.
  Thanks to Hank. [micheus]

- Importing a .gbl project was causing a crash just before it to be
  displayed. Thanks to tkbd. [micheus]

- If a area light is present only the faces facing the camera can get
  selected by marquee selection. Thanks Hank. [micheus]

- Wings3D was crashing unexpected in module wings_frame. Added a
  workaround and some information to help us to find the cause. Thanks to  Hank [micheus]

- Select similar material isn't working properly in AutoUV. Thanks to Hank [micheus]

- Worked some more on the boolean operations. [dgud]

- Moving a light and using translation was causing Wings3D to crash.
  Thanks to tkbd. [micheus]

- Alpha channel of a diffuse texture was drawing in black. Thanks to tkbd [micheus]

- Upgraded to erlang/otp-21.2 (mac) which should improve performance on large models. [dgud]

- General Mac improvements, menus and material viewer. Reported by tkbd [dgud]

- Fixed opencl crash on old ATI cards. [dgud]

- Fixed image handling crash. [dgud]

- Fixed exporting lights to plugin. [dgud]

--- 2.2.1 --------------------------------------------

- Materials have been changed to have roughness and
  metallic parameters instead ambient, specular and shininess.
  Handling of more texture types have been added.
  Brdf emulation with background images when rendering materials
  and using camera light or scene lights.

  Image planes or background images used to set a diffuse
  texture and set emission to white. That doesn't work any more
  because a white emission material will overexpose the texture.

  Set the background texture as an emission texture instead will
  get the same result as in previous wings versions. New image
  plane will do this automatically. [dgud]

- Added new image commands to the outliner.
  - Create image from channel, to enable usage of single channels from example
    combined metallic roughness textures.
  - Invert channel, create a new image with one channel
    (or image in case of gray scale) inverted.
  To be used for example to change between DirectX and OpenGL normal map
  formats, or to create a simple roughness map from a metallic map. [dgud]

- Added a test version of Boolean commands.
  Does not handle coplanar faces.
  Try subdividing the intersecting faces if the Boolean commands doesn't
  work. [dgud]

- Added the PLY importer to the Wings3D project. [Micheus Vieira]

- Export normals for collada (dae) format. [dgud]

- Added export/import vertex color support for gltf files. [micheus]

- Improve import of file formats, by estimating hard edges for formats
  that does not have smoothing groups but includes normals. [dgud]

- Enabled the Flatten command to be repeatable in edge mode. Thanks to Hank. [micheus]

- Fixed a bug causing wings to not find relative image files when
  file and subdirectory had moved. [dgud]

- Check for EXT or ARB framebuffer extension, both works, but for
  some old cards/drivers only EXT is available. [dgud]

- Fixed the crash in Put On command (LMB option) when no destination was
  selected. Thanks to Hank. [micheus]

- Fixed the cause of Wings3D crash after a combination of hole and
  Cleanup command. Thanks to Hank. [micheus]

- Fixed the cause of Select By Short Edge be crashing when there was
  a active selection. Thanks to rv3. [micheus]

- Fixed the window docking feature for multiple monitors systems. Thanks to suzuki. [micheus]

- Fixed the wrong dialog location when user has multiple monitors.
  Thanks to suzuki. [micheus]

- Fixed the hard crash when creating a texture. [micheus]

- The information line was showing a wrong value for the edge difference
  and angle info was also missing. Thanks to rv3. [micheus]

- Made gltf and collada import more robust. [dgud] [micheus]

- Fixed a issue which was allowing locked objects be moved if them
  were previously selected. Thanks to OXO. [micheus]

- Moving any object to a folder was causing a crash. Thanks to Hank. [micheus]

- Floating windows was growing each time Wings3D was started. [Micheus Vieira]

- Combined objects in a folder were moved outside the folder. Thanks Hank [micheus]

- Fixed the problem that was causing geometry state rollback after
  save a project in Sculpt mode. Thanks to tkbd. [micheus]

- By starting Sculpt with a geometry window in Wire mode would make
  the sculpt never get back to Shading mode. Thanks to Hank. [micheus]

- Workaround for linux Wayland support. Thanks xyproto. [dgud]

- A bunch of more bug-fixes we have stumbled on but forgot to note down
  [micheus] [dgud] [bjorng]

For older release notes see OLD-NOTES
