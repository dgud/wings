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
