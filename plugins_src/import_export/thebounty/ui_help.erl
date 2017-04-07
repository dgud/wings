%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%  See AUTHORS.txt for a complete list of authors.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

%
% Tooltip help text
%
% new code ---------------------->
help_export() ->
    Title = help(title, {options_dialog,general}),
    Text = help(text, {options_dialog,general}),
    {Title,Text}.
% end new   -------------------------------->
help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.

    % test
help(title, {options_dialog,_}) ->
    ?__(811,"TheBounty Render Options");
help(text, {options_dialog,general}) ->
    [?__(5,"Test..")];
% end test

help(title, {material_dialog,object}) ->
    ?__(6,"TheBounty Material Properties: Object Parameters");
help(text, {material_dialog,object}) ->
    [?__(7,"Object Parameters are applied to whole objects, namely those "
      "that have this material on a majority of their faces."),
    ?__(8,"Mesh: Standard 3D mesh."),
    ?__(9,"Volume: Defines an area for Volumetrics. "
      "Control simulated size by adjusting Min/Max settings. "
      "When using the Noise option, a Texture must also be defined in the Material "
      "Properties. Volumetrics must also be enabled under TheBounty Render Options."),
    ?__(10,"Mesh Light: Use for Neon lights or other glowing meshes. "
      "Limit the number of mesh lights in your scene, since render times are longer. "
      "Mesh Lights provide faster rendering than converting meshes to area lights "
      "with Object to Area Light."),
    ?__(11,"Light Portal: Controls light and photons coming through a window in a closed room. "
      "Apply to a flat plane. Light Portals are used to reduce render times."),
    ?__(12,"Autosmooth Angle: Controls TheBounty simulated smoothing of a mesh. "
      "For best results, adjust the Subdivisions setting under TheBounty Render "
      "Options to control real mesh smoothing.")];
help(title, {material_dialog,fresnel}) ->
    ?__(15,"TheBounty Material Properties: Fresnel Parameters");
help(text, {material_dialog,fresnel}) ->
    [?__(16,"Fresnel Parameters affect how rays reflect off and refract in glass-like materials. "
    "This is a different light model than the OpenGL (Diffuse,Specular,Shininess) model and they "
    "do not often go well together. "
      "A Photon Light must be present to produce Caustics."),
     ?__(17,"Mapping to TheBounty shader parameters:"),
     ?__(18,"Index Of Refraction -> 'ior' -> 1.52 for Glass/Caustics."),
     ?__(19,"Total Internal Reflection -> 'tir' -> Enable for Glass."),
     ?__(20,"Minimum Reflection -> 'min_refle' -> 1.0 for Metal."),
     ?__(21,"Reflected -> 'reflected' -> Reflective Caustics."),
     ?__(22,"Transmitted -> 'transmitted' -> Glass/Refractive Caustics."),
     ?__(23,"Use Default -> Sets 'transmitted' to Diffuse * (1 - Opacity). "
      "This makes a semi-transparent object in OpenGL look the same in "
      "TheBounty provided that Index Of Refraction is 1.1 minimum."),
     ?__(24,"Grazing Angle Colors -> Use the secondary Reflected and Transmitted colors following "
     "that show from grazing angles of the material. For a glass with green edges set Transmitted "
     "to white and Grazing Angle Transmitted to green."),
     ?__(25,"Absorption -> Sets the desired color for white light travelling the given distance through the material.")];
%%
help(title, {light_dialog,_}) ->
    ?__(26,"TheBounty Light Properties");
help(text, {light_dialog,Type}) ->
    [?__(27,"Diffuse Color and Spot Light Angle are the only OpenGL properties "
        " that map to the TheBounty light settings.")]++
    help(text,{light,Type});
help(text,{light,point}) ->
    [[{bold,?__(28,"Point Light")}],
        ?__(29,"A light with rays pointing in every direction. Use for a candle flame, gas light, or light bulb. "
        "Choose either Point Light or Sphere Light, which has the added options of setting the Radius and Samples.")];
help(text,{light,spot}) ->
    [[{bold,?__(30,"Spot Light")}],
     ?__(31,"A light with rays focused on a certain area.\n"
        "Choose either Spotlight or IES. The Photon Only option is good for enhancing refractive and reflective caustic patterns. "
        "The IES option enables the use of IES files to simulate real world lights, which produce uniquely shaped lighting.")];
help(text,{light,infinite}) ->
    [[{bold,?__(75,"Infinite Light")}],
        ?__(76,"In TheBounty there are two options to reproduce sun lighting which are Directional and Sun. "
        "Directional light is a traditional sun light model which produces parallel rays and hard-edged shadows. "
        "Sun light is a more advanced concept and will help us to get blurred-edged shadows when the shadow itself "
        "gets away from the casting object, as in real life. The Angle button sets the visible area of the sun. "
        "Real sun is visible in a cone angle of about 0,5º. A bigger angle mean a bigger sun, as well as softer shadows, "
        "which could be interesting for dawn or sunset scenes. "
        "A very big angle can be used to simulate sun light filtered by an overcast sky.")];
help(text,{light,ambient}) ->
    [[{bold,?__(77,"Ambient Light")}],
        ?__(78,"A light with rays pointing in every direction and emitting from all directions, with no shadows. "
        "Choose between the various Background Light/Environment options to control the appearance of the background and lighting. "
        "Disable the IBL option if no lighting is wanted. The HDRI option enables the use of real world environments to "
        "reflect onto reflective surfaces. HDRI also produces realistic lighting when Enlight is enabled.")];
help(text,{light,area}) ->
    [[{bold,?__(79,"Area Light")}],
        ?__(80,"Arealight is a area light type that can produce soft shadows and its shape can be seen in reflective surfaces. "
        "The arealight shadows need to be sampled several times and interpolated to reduce noise in shadows. "
        "This type of light takes more time to be computed in contrast with point light types such as spot and point. "
        "Wings3D objects can be converted to Area Lights with the Object to Area Light command. "
        "Expect longer render times when using Area Lights.")];

help(title, pref_dialog) ->
    ?__(33,"TheBounty Options");
help(text, pref_dialog) ->
    [?__(34,"These are user preferences for the TheBounty exporter plugin"),
     ?__(35,"Automatic Dialogs: ")
     ++wings_help:cmd([?__(36,"File"),?__(37,"Export"),?__(38,"TheBounty")])++", "
     ++wings_help:cmd([?__(39,"File"),?__(40,"Export Selected"),?__(41,"TheBounty")])++" "++?__(42,"and")++" "
     ++wings_help:cmd([?__(43,"File"),?__(44,"Render"),?__(45,"TheBounty")])++" "++
     ?__(46,"are enabled if the rendering executable is found (in the path), "
     "or if the rendering executable is specified with an absolute path."),
     %%
     ?__(47,"Disabled Dialogs:")++" "
     ++wings_help:cmd([?__(48,"File"),?__(49,"Export"),?__(50,"TheBounty")])++", "
     ++wings_help:cmd([?__(51,"File"),?__(52,"Export Selected"),?__(53,"TheBounty")])++" "++?__(54,"and")++" "
     ++wings_help:cmd([?__(55,"File"),?__(56,"Render"),?__(57,"TheBounty")])++" "++
     ?__(58,"are disabled."),
     %%
     ?__(59,"Enabled Dialogs:")++" "
     ++wings_help:cmd([?__(60,"File"),?__(61,"Export"),?__(62,"TheBounty")])++" "++?__(63,"and")++" "
     ++wings_help:cmd([?__(64,"File"),?__(65,"Export Selected"),?__(66,"TheBounty")])++" "++
     ?__(67,"are always enabled, but")++" "
     ++wings_help:cmd([?__(68,"File"),?__(69,"Render"),?__(70,"TheBounty")])++" "++
     ?__(71,"is the same as for \"Automatic Dialogs\"."),
     %%
     ?__(72,"Binary folder: The rendering command for the TheBounty raytrace renderer "
     "('/TheBounty/bin/thebounty-xml.exe') that is supposed to be found in the executables search path; "
     "or, the absolute path of that executable."),
     %%
     %?__(73,"TheBounty Plugins Path: The path to the TheBounty plugins folder "
     % "('c:/TheBounty/bin/plugins'). TheBounty will not work without this."),
     %%
     ?__(74,"Options: Rendering command line options to be inserted between"
     "the executable and the .xml filename, -dp (add render settings badge) "
     "-vl (verbosity level, 0=Mute,1=Errors,2=Warnings,3=All)." )].
