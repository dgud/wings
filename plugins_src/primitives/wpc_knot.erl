%%
%% Torus Knot -- Example Plugin for Wings 0.90 or higher.
%%
%% By Anthony D'Agostino (scorpius@compuserve.com)
%% Select the knot object and 'Smooth' it 2 or 3 times.
%% Press 'r', 'spacebar', 'tab', then 'u' to see it rotate.
%%
%% Adapted for Wings 0.90 by Bjorn Gustavsson.
%%
%% ----------------------------------------------------------------
%% To compile from the Erlang shell.
%%
%% c(wpc_knot).
%%
%% The compiled code will be put into the file wp0_knot.beam in the
%% current directory. You can then move it into the plugins
%% directory of the Wings installation. When you restart Wings,
%% "Torus Knot" will appear in the object creation menu.
%% ----------------------------------------------------------------

%%
%% The module/file name for a command plug-in must start with "wpc_".
%%

-module(wpc_knot).                              %The module name must
                                                %match the filename.
                                                %The filename must end
                                                %".erl".

%% The following two functions must be present in all object
%% creation plugins. They must be exported (callable from other
%% modules).

-export([init/0,menu/2,command/2]).

%% this include is added to make this module translatable

-include("wings_intl.hrl").

%% This function will be called once at the startup of Wings.
%% It must return true. (Return false to disable the plug-in.)

init() ->
    true.

%% This function will be called every time a menu is opened,
%% allowing this plug-in to extend any menu.

menu({shape,more}, []) ->
    torus_menu();
menu({shape,more}, Menu) ->
    %% Will appear in the object creation menu as "Torus Knot"
    %% (without any option box).
    Menu ++ [separator|torus_menu()];
menu(_, Menu) ->
    %% Any other menu. Must return the Menu parameter unchanged.
    Menu.

torus_menu() ->
    [{?__(1,"Torus Knot"),torus_knot}].
    
%% This function will be called before executing any command.

command({shape,{more,torus_knot}}, _) ->
    %% Our command.
    make_knot();
command(_, _) ->
    %% In this case we ignore the arguments. Underscore is a "dont't
    %% care" wildcard. Tell Wings that the next plug-in or
    %% the built-in command command should handle this command.
    next.

%% This function is local. It cannot be called from the another
%% Erlang module.

make_knot() ->
    %% We number the vertices of the object from 0 and upwards.

    %% Then we list for each face the vertex numbers.
    %%
    %% List elements are enclosed by [ and ].
    %%
    %% It is very important that the vertices are listed
    %% counter-clockwise order, or the Wings will have problems
    %% understanding.

    Faces = [
        [0, 3, 5, 2    ],
        [3, 0, 1, 4    ],
        [2, 5, 4, 1    ],
        [3, 6, 8, 5    ],
        [4, 7, 6, 3    ],
        [5, 8, 7, 4    ],
        [6, 9, 11, 8   ],
        [7, 10, 9, 6   ],
        [8, 11, 10, 7  ],
        [9, 12, 14, 11 ],
        [10, 13, 12, 9 ],
        [11, 14, 13, 10],
        [12, 15, 17, 14],
        [13, 16, 15, 12],
        [14, 17, 16, 13],
        [15, 18, 20, 17],
        [16, 19, 18, 15],
        [17, 20, 19, 16],
        [18, 21, 23, 20],
        [19, 22, 21, 18],
        [20, 23, 22, 19],
        [21, 24, 26, 23],
        [22, 25, 24, 21],
        [23, 26, 25, 22],
        [24, 0, 2, 26  ],
        [0, 24, 25, 1  ],
        [26, 2, 1, 25  ]
    ],


    %% Faces is a list, itself containing lists.
    
    %% For each vertex, we list its position.
    %% The first element in the list (i.e. {2.0,0.0,0.0})
    %% is the position for vertex 0, the second the position
    %% for vertex 1 and so on.
    %%
    %% Each element in the list is an *tuple*. Tuples are enclosed
    %% in by { and }. Tuples are used fixed size structures.
    %% A position in 3D space has X, Y, and Z coordinates,
    %% so we use a tuple with 3 elements.
    %%
    %% We use lists when we the number of elements can vary.
    
    %% VertexPositions = [{2.0,0.0,0.0},{-2.0,0.0,0.0},{0.0,2.0,0.0},
    %%                    {0.0,-2.0,0.0},{0.0,0.0,2.0},{0.0,0.0,-2.0}],

    VertexPositions = [
        {1.445509,0.246043,0.379514   },
        {1.923419,-0.425628,0.679360  },
        {1.477744,-0.505349,-0.071949 },
        {0.998362,0.506429,-1.198357  },
        {1.525929,0.918203,-1.765424  },
        {1.334724,0.069929,-1.880836  },
        {-0.455026,0.183123,-1.266398 },
        {-0.686298,1.029229,-1.258281 },
        {-1.253124,0.422306,-1.540772 },
        {0.499151,0.092699,1.426996   },
        {-0.016060,0.465731,2.031027  },
        {-0.332274,-0.171677,1.518026 },
        {0.303017,-1.417962,0.766045  },
        {0.618264,-2.137096,1.157085  },
        {-0.248190,-2.022720,1.082094 },
        {-0.193436,-1.192151,-0.621053},
        {0.595080,-1.346321,-0.973081 },
        {-0.124742,-1.392696,-1.472236},
        {0.503848,1.275947,0.641996   },
        {0.953674,1.846630,0.150645   },
        {0.175995,1.548476,-0.124613  },
        {-1.136311,1.020388,0.597381  },
        {-1.693220,1.516292,1.059312  },
        {-1.702575,1.546538,0.182701  },
        {-1.323700,-0.291531,-0.084710},
        {-1.468417,-0.711423,0.671727 },
        {-1.721339,-1.073407,-0.086196}
    ],



    %% Time to return the shape we have built.
    %% We pack it into a tuple. First we tell Wings that
    %% we want to create a new shape (new_shape as the first
    %% element).
    %%
    %% The second element is what the object should be named.
    %% Wings will append a number to this string ("knot");
    %% it will be "knot0" if this is the first object created
    %% after having started Wings.
    %%
    %% The third and fourth elements are the list of Faces and
    %% the list of VertexPositions.

    {new_shape,"knot",Faces,VertexPositions}.

