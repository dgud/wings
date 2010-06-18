%%
%%  pbr.hrl
%%
%%     Data structures
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-record(renderer, 
	{
	  cl,
	  cam,
	  scene,
	  film
	}).

-define(IS_POINT_LIGHT(We), (We#we.light =/= none 
			     andalso ((element(2, We#we.light) == point) orelse 
			              (element(2, We#we.light) == spot)))).

-define(PI, 3.141592653589793).
-define(INV_PI, 1/3.141592653589793).

-define(RAY_SZ, 32).		                % Binary size of ray structure
-define(RAYHIT_SZ, 16).				% Binary size of rayhit structure
-define(RAYBUFFER_SZ, 65536).			% Max Buffer size
-define(MAX_RAYS, ?RAYBUFFER_SZ div ?RAY_SZ).   % Max number of rays

-define(RAY_EPS, 0.00001).			% Error margin
-define(RAY_INFINITY, 3.402823e+38).            % 32 bits float max

-define(PHOTONS_PER_PASS, 500000).
-define(MAX_PHOTON_DEPTH, 3).
-define(MAX_EYE_DEPTH, 2).

-define(PPM_ALPHA, 0.7).

-record(ray, 
	{o,					% Origo
	 d,					% Dir
	 n=?RAY_EPS,				% Near = mint
	 f=?RAY_INFINITY}).			% Far  = maxt

%%-define(F32, 32/float-native).    % Defined in wings.hrl
%%-define(I32, 32/native).
