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

- Fixed crash when trying to edit area ligth properties
  and not selecting an area ligth. Thanks Lars Thorsen. [dgud]

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

- General Mac improvements, menues and material viewer. Reported by tkbd [dgud]

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
