
Writing shaders to autouv.

It is standard opengl 2.0 GLSL programs, easy to learn and code.
No need for erlang knowledge (besides the simple config files).
Each shader needs a config file, it should be named 
wpc_SHADERNAME.auv in the plugins/autouv/ directory.

See the included samples for config options, currently the error
checking isn't the best so if you do something wrong you might crash
the autouv window.

The config should atleast contain:
{name, "Shader"}. 
{vertex_shader, "vertexshader_file.vs"}.
{fragment_shader, "fragmentshader_file.fs"}.

Comments are written after a percent sign, i.e % This is a comment

The data to the vertex shader comes as:
gl_Vertex contains the uv-coords.
gl_MultiTexCoord1 contains the real 3d vertex postions in world space.

There is also an option 'requires' which contains a list of
requirements, currently only 'normal' (and 'binormal' currently not
calculated correctly) are available.  
{requires, [normal]}. 
Will calculate each vertex normal and send it to the vertex shader, via
gl_Normal.  'binormal' are sent through 'gl_MultiTexCoord2.xyz'.

Other options:  Sent as uniforms
{auv, auv_bg}.    % sampler2D image of the previous pass
{auv, auv_txsz}.  % vec2 width and heigth of image
{auv, {auv_send_texture,"UserQuestion",true}}.    % Float 1.0 (true) or 0.0 (false) 
% Asks user if we should work on whole image or only the selected chart parts, sent as float 1.0 or 0.0.
{auv, auv_bbpos3d}. % Vec3[2] The Min and Max coords of the rendered charts
{auv, auv_bbpos2d}. % Vec2[2] The Min and Max uv-coords of each chart

User options/questions can be sent as uniforms as well. The format is:
{uniform, Type, VarID in shader, DefaultValue, StringInGUI}.

The different types are color(vec4), float, bool(float 0.0 or 1.0),image(sampler2D) 
and {slider, Min, Max} (float). There is also a special menu entry, see the filter shader example.
VarID should be the name of the uniform in your shader and DefaultValue is the default 
value, StringInGui is the user question.

Hopefully you can create and share some amazing shaders. More options, data, types can
be added, drop me an email (and code) if you need something.
If you improve the examples I have thrown together send it as well, I'm not very good at
writing shaders and need some help.

Beware that this is the first version so the interface might change some in the coming releases.

Some tips the shaders are compiled each time create texture is pressed so there is no need
to close wings or autouv when developing shaders, just edit/save the shader file and test it again.
Keep an eye on the erlang window or the console window for compiler info/errors.

Regards 
/Dan Gudmundsson (alias dgud)
