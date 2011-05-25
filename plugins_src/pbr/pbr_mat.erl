%%
%%  pbr_mat.erl
%%
%%     Pbr material handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_mat).

-export([snew/0, sdiv/2, smul/2, sadd/2, sY/1, s_is_black/1, to_rgb/1,
	 init/1, lookup_id/2,
	 pack_materials/2, mesh2tex/2, pack_textures/1,
	 type/2, create_arealight_mat/3,
	 sample_f/4, f/4, lookup_texture/3, is_light/1, is_diffuse/1]).

-export([spd/3, irregular_spd/2,irregular_spd/3,
	 sample/2, spd_to_rgb/1
	]).

-include_lib("wings/e3d/e3d_image.hrl").
-include("pbr.hrl").
-include("pbr_constants.hrl").

%%%%%%%%%%%
-define(MAT_MATTE, 0).	
-define(MAT_AREALIGHT, 1).
-define(MAT_MIRROR, 2).
-define(MAT_GLASS, 3).    
-define(MAT_MATTEMIRROR, 4).
-define(MAT_METAL, 5).
-define(MAT_MATTEMETAL, 6).
-define(MAT_ALLOY, 7).
-define(MAT_ARCHGLASS, 8).
-define(MAT_NULL, 9).

-define(MAT_MATTE_SZ, (3*4)).
-define(MAT_AREALIGHT_SZ, (3*4)).
-define(MAT_MIRROR_SZ, (4*4)).
-define(MAT_GLASS_SZ,  (11*4)).
-define(MAT_MATTEMIRROR_SZ, (?MAT_MATTE_SZ+?MAT_MIRROR_SZ+4*4)). % = 11*4
-define(MAT_METAL_SZ, (5*4)).
-define(MAT_MATTEMETAL_SZ, (?MAT_MATTE_SZ+?MAT_METAL_SZ+4*4)).   % = 12*4
-define(MAT_ALLOY_SZ, (9*4)).
-define(MAT_ARCHGLASS_SZ, (10*4+2*4)).

-define(MAT_MAX_SZ, (12*4)).

-record(material, 
	{label,
	 m_info,				% Term module specific
	 map,
	 bump
	}).

-record(arealight,  {gain}).
-record(matte,  {kd, kdOverPi}).
-record(mirror, {kr, sb=1}).
-record(mattemirror, {kd, kr, sb=1, mattef, totf, mattepdf, mirrorpdf}).
-record(metal,  {kr, exp, sb=1}).
-record(mattemetal,  {kd, kr, exp, sb=1, mattef, totf, mattepdf, metalpdf}).
-record(alloy,  {kd, kr, exp, r0=0.5, sb=1}).
-record(glass,  {refl, refr, ior, oior, r0, rsb=1, tsb=1}).
-record(archglass,  {refl, refr, rsb=1, tsb=1, reflf, totf, reflpdf, transpdf}).

-record(ms, {mats, n2id, maps, bumps}).

-record(spd, {n, min, max, delta, invdelta, samples}).

%% Spectrum functions
snew() ->
    {0.0,0.0,0.0}.

sdiv({R,G,B}, S) 
  when is_float(R), is_float(G), is_float(B) ->
    {R/S,G/S,B/S}.

smul({R,G,B}, {X,Y,Z}) 
  when is_float(R), is_float(G), is_float(B), 
       is_float(X), is_float(Y), is_float(Z) ->
    {R*X,G*Y,B*Z};
		     
smul({R,G,B}, S) 
  when is_float(R), is_float(G), is_float(B) ->
    {R*S,G*S,B*S}.

sadd({R,G,B}, {X,Y,Z}) 
  when is_float(R), is_float(G), is_float(B), 
       is_float(X), is_float(Y), is_float(Z) ->
    {R+X,G+Y,B+Z}.

sY({R,G,B}) ->
    0.212671 * R + 0.715160 * G + 0.072169 * B.

s_is_black({R,G,B}) 
  when R > 0.0; G > 0.0; B > 0.0 ->
    false;
s_is_black(_) -> 
    true.

to_rgb(Spectrum) ->
    RW = { 3.240479, -1.537150, -0.498535},
    GW = {-0.969256,  1.875991,  0.041556},
    BW = { 0.055648, -0.204043,  1.057311},
    {e3d_vec:dot(RW, Spectrum),
     e3d_vec:dot(GW, Spectrum),
     e3d_vec:dot(BW, Spectrum)}.

%% Material functions
init(Mtab) ->
    New = array:new(),
    Ms = #ms{mats=New, n2id=gb_trees:empty(), maps=New, bumps=New},
    lists:foldl(fun(Mat, Acc) ->
			create_mat(Mat, Acc)
		end, Ms, gb_trees:to_list(Mtab)).

create_mat({Name, WM}, Ms=#ms{mats=Mats, n2id=Tree0}) ->
    Id = array:size(Mats),
    Tree = gb_trees:insert(Name, Id, Tree0),
    
    OpenGL = proplists:get_value(opengl, WM),
    {R,G,B,A} = proplists:get_value(diffuse, OpenGL),
    Maps = proplists:get_value(maps, WM, []),
    Map  = proplists:get_value(diffuse, Maps, undefined),
    Bump = proplists:get_value(bump, Maps, undefined),
    Info = case A of
	       1.0 -> create_solid_mat({R,G,B}, OpenGL);
	       _ -> create_glass_mat(A, {R,G,B}, OpenGL)
	   end,
    Material = #material{label=Name, m_info=Info, map=Map, bump=Bump},
    Ms#ms{mats=array:set(Id,Material,Mats),n2id=Tree}.

lookup_id(Name, #ms{n2id=Tree0}) ->
    gb_trees:get(Name, Tree0).

create_arealight_mat(Name, Gain, Ms=#ms{mats=Mats, n2id=Tree0}) ->
    Id = array:size(Mats),
    Tree = gb_trees:insert(Name, Id, Tree0),
    AreaL = #material{label=Name, m_info=#arealight{gain=Gain}},
    Ms#ms{mats=array:set(Id,AreaL,Mats),n2id=Tree}.

create_solid_mat(Diff, OpenGL) ->
    Shine    = proplists:get_value(shininess, OpenGL),
    {R,G,B,_} = proplists:get_value(specular, OpenGL),
    Specular = {R,G,B},
    case Shine of
	1.0 -> create_mirror(Diff, Specular);
	_ -> create_solid_mat(Diff, Specular, Shine)
    end.

create_solid_mat(Diff, {0.0,0.0,0.0}, 0.0) ->
    create_diffuse(Diff);
create_solid_mat(Diff, Spec, Shin) 
  when Shin < 0.5 ->
    Exp = 1.000001-Shin,
    MaF = filter(Diff),
    MiF = filter(Spec),
    TF  = MaF + MiF,
    MaPdf = MaF/TF, 
    MiPdf = MiF/TF,    
    #mattemetal{kd=Diff, kr=Spec, exp=Exp,
		mattef=MaF, totf=TF, 
		mattepdf = MaPdf, metalpdf = MiPdf};
create_solid_mat({1.0,1.0,1.0}, Spec, Shin) ->
    Exp = 1.000001-Shin,
    #metal{kr=Spec, exp=Exp};
create_solid_mat(Diff, Spec, Shin) ->
    Exp = 1.000001-Shin,
    #alloy{kd=Diff, kr=Spec, exp=Exp}.

create_mirror({1.0,1.0,1.0}, Spec) ->
    #mirror{kr=Spec};
create_mirror(Diff, Spec) ->
    MaF = filter(Diff),
    MiF = filter(Spec),
    TF  = MaF + MiF,
    MaPdf = MaF/TF, 
    MiPdf = MiF/TF,
    #mattemirror{kd=Diff, kr=Spec, 
		 mattef=MaF, totf=TF, 
		 mattepdf = MaPdf, mirrorpdf = MiPdf}.

create_diffuse(Diff) ->
    #matte{kd=Diff, kdOverPi=smul(Diff, ?INV_PI)}.

create_glass_mat(A, Diff, OpenGL) ->
    {RR,RG,RB,_} = proplists:get_value(specular, OpenGL),
    Ior  = 1.0 + A,
    Oior = 1.0,
    T = Ior-Oior, B = Ior + Oior,
    R0 = T*T / (B*B),
    E  = 1.0,
    #glass{refl=Diff, refr={RR*E,RG*E,RB*E}, 
	   ior=Ior, oior=Oior, r0=R0}.

type(Mat, #ms{mats=Mats}) ->
    #material{m_info=Record} = array:get(Mat,Mats),
    element(1, Record).
    
%%%%%%%%%

mesh2tex(Mats, #ms{mats=Ms}) ->
    mesh2tex(Mats, Ms, 0, [], [], array:new(), false, false).

pack_textures(TexIds) ->
    pack_textures(TexIds, {<<>>, <<>>}, <<>>).

pack_materials(Mats, #ms{mats=Ms}) ->
    lists:foldl(fun(Id, Bin) -> 
			#material{m_info=Mat} = array:get(Id,Ms),
			pack_material(Mat, Bin)
		end, <<>>, Mats).
			 
pack_material(#matte{kd={R,G,B}}, Bin) ->
    <<Bin/binary, ?MAT_MATTE:?UI32, 
      R:?F32, G:?F32, B:?F32, 
      0:(8*(?MAT_MAX_SZ-?MAT_MATTE_SZ))>>;
pack_material(#arealight{gain={R,G,B}}, Bin) -> 
    <<Bin/binary, ?MAT_AREALIGHT:?UI32, 
      R:?F32, G:?F32, B:?F32, 
      0:(8*(?MAT_MAX_SZ-?MAT_AREALIGHT_SZ))>>;
pack_material(#mirror{kr={R,G,B}, sb=SB}, Bin) -> 
    <<Bin/binary, ?MAT_MIRROR:?UI32, 
      R:?F32, G:?F32, B:?F32, SB:?I32,
      0:(8*(?MAT_MAX_SZ-?MAT_MIRROR_SZ))>>;
pack_material(#glass{refl={LR,LG,LB}, refr={RR,RG,RB},
		     ior=Ior, oior=OIor, r0=R0,
		     rsb=ReflSpecBounce, tsb=TransSpecBounce}, Bin) -> 
    <<Bin/binary, ?MAT_GLASS:?UI32, 
      LR:?F32, LG:?F32, LB:?F32, 
      RR:?F32, RG:?F32, RB:?F32, 
      OIor:?F32, Ior:?F32, R0:?F32,
      ReflSpecBounce:?I32, TransSpecBounce:?I32,
      0:(8*(?MAT_MAX_SZ-?MAT_GLASS_SZ))>>;
pack_material(#mattemirror{kd={DR,DG,DB}, kr={RR,RG,RB}, sb=SB,  
			   mattef=MaF, totf=TF, mattepdf=MaPdf, mirrorpdf=MiPdf
			  }, Bin) -> 
    <<Bin/binary, ?MAT_MATTEMIRROR:?UI32, 
      DR:?F32, DG:?F32, DB:?F32, 
      RR:?F32, RG:?F32, RB:?F32, SB:?I32,      
      MaF:?F32, TF:?F32, MaPdf:?F32, MiPdf:?F32,
      0:(8*(?MAT_MAX_SZ-?MAT_MATTEMIRROR_SZ))>>;
pack_material(#metal{kr={R,G,B}, exp=Exp, sb=SB}, Bin) ->
    <<Bin/binary, ?MAT_METAL:?UI32, 
      R:?F32, G:?F32, B:?F32, Exp:?F32, SB:?F32,
      0:(8*(?MAT_MAX_SZ-?MAT_METAL_SZ))>>;

pack_material(#mattemetal{kd={DR,DG,DB}, kr={RR,RG,RB}, exp=Exp, sb=SB,
			  mattef=MaF, totf=TF, mattepdf=MaPdf, metalpdf=MiPdf
			 }, Bin) -> 
    <<Bin/binary, ?MAT_MATTEMETAL:?UI32, 
      DR:?F32, DG:?F32, DB:?F32, 
      RR:?F32, RG:?F32, RB:?F32, Exp:?F32, SB:?I32, 
      MaF:?F32, TF:?F32, MaPdf:?F32, MiPdf:?F32, 
      0:(8*(?MAT_MAX_SZ-?MAT_MATTEMETAL_SZ))>>;
pack_material(#alloy{kd={DR,DG,DB}, kr={RR,RG,RB}, exp=Exp, r0=R0, sb=SB}, Bin) -> 
    <<Bin/binary, ?MAT_ALLOY:?UI32, 
      RR:?F32, RG:?F32, RB:?F32, 
      DR:?F32, DG:?F32, DB:?F32, 
      Exp:?F32, R0:?F32, SB:?I32, 
      0:(8*(?MAT_MAX_SZ-?MAT_ALLOY_SZ))>>;
pack_material(#archglass{refl={DR,DG,DB}, refr={RR,RG,RB}, rsb=RSB, tsb=TSB,
			 reflf=MaF, totf=TF, reflpdf=MaPdf, transpdf=MiPdf
			}, Bin) -> 
    <<Bin/binary, ?MAT_ARCHGLASS:?UI32, 
      DR:?F32, DG:?F32, DB:?F32, 
      RR:?F32, RG:?F32, RB:?F32, 
      MaF:?F32, TF:?F32, MaPdf:?F32, MiPdf:?F32, 
      RSB:?UI32, TSB:?UI32,
      0:(8*(?MAT_MAX_SZ-?MAT_ARCHGLASS_SZ))>>;
pack_material(Mat, Bin) -> 
    io:format("Ignoring unknown material ~p~n", [Mat]),
    Bin.

mesh2tex([Mat|Mats], Ms, N0, Tex, Bump, All0, HaveTex, HaveBump) ->
    No = 16#ffffffff,
    case array:get(Mat,Ms) of
	#material{map=undefined, bump=undefined} ->
	    mesh2tex(Mats, Ms, N0, [No|Tex], [No|Bump], 
		     All0, HaveTex, HaveBump);
	#material{map=TId, bump=undefined} ->
	    {Id, N, All} = texId(TId, N0, All0),
	    mesh2tex(Mats, Ms, N, [Id|Tex], [No|Bump], 
		     All, true, HaveBump);
	#material{map=undefined, bump=TId} ->
	    {Id, N, All} = texId(TId, N0, All0),
	    mesh2tex(Mats, Ms, N, [No|Tex], [Id|Bump],
		     All, HaveTex, true);
	#material{map=MId0, bump=BId0} ->
	    {MId, N1, All1} = texId(MId0, N0, All0),
	    {BId, N, All} = texId(BId0, N1, All1),
	    mesh2tex(Mats, Ms, N, [MId|Tex], [BId|Bump], 
		     All, true, true)
    end;
mesh2tex([], _, _, Texs, Bumps, All0, HaveTex, HaveBump) -> 
    BumpScales = HaveBump andalso << <<1.0:?F32>> || _ <- Bumps >>,
    All = [Orig || {Orig,_} <- lists:keysort(2, array:sparse_to_orddict(All0))],
    %% io:format("Mesh2Tex ~p: ~w~n~w~n", [HaveTex, Texs,All]),
    %% io:format("Bumps ~p: ~w~n~w~n", [HaveBump, Bumps, BumpScales]),
    {mesh2texbin(HaveTex, Texs),
     mesh2texbin(HaveBump, Bumps),
     BumpScales,
     All}.

texId(Id, Next, Texs0) ->
    case array:get(Id, Texs0) of
	undefined ->
	    Texs = array:set(Id, Next, Texs0),
	    {Next, Next+1, Texs};
	Tex ->
	    {Tex, Next, Texs0}
    end.

mesh2texbin(false, _) -> false;
mesh2texbin(true, IntList) -> 
    << <<Int:?UI32>> || Int <- lists:reverse(IntList) >>.

pack_textures([Id|TexIds], Ts={RGB0, A0}, Desc0) ->
    #e3d_image{width=W,height=H,type=Bpp,image=Bin} = wings_image:info(Id),
    APos = alpha_offset(Bpp, A0),
    Pos  = rgb_offset(RGB0),
    %% io:format("Tex ~p Offset: ~p,~p ~p ~n", [Id, Pos,APos, {W,H}]),
    Desc = <<Desc0/binary, Pos:?UI32, APos:?UI32, W:?UI32, H:?UI32>>,
    pack_textures(TexIds, pack_texture(Bpp,Bin,Ts), Desc);
pack_textures([], {RGB,A}, Desc) ->
    {RGB /= <<>> andalso RGB,
     A /= <<>> andalso A,
     Desc /= <<>> andalso Desc}.

alpha_offset(r8g8b8a8, Bin) ->  byte_size(Bin) div 4;
alpha_offset(a8, Bin) ->        byte_size(Bin) div 4;
alpha_offset(_, _) ->           16#FFFFFFFF.

rgb_offset(Bin) ->  byte_size(Bin) div (3*4).
    

pack_texture(r8g8b8, Bin, {RGB,A}) ->
    {float_rgb(Bin, RGB), A};
pack_texture(g8, Bin, {RGB,A}) ->
    {float_g(Bin, RGB), A};
pack_texture(r8g8b8a8, Bin, {RGB,A}) ->
    float_rgba(Bin, RGB, A);
pack_texture(a8, Bin, {RGB,A}) ->
    float_a(Bin, RGB, A).

float_rgb(<<R:8,G:8,B:8,Bin/binary>>, RGB) ->
    S = 1/255,
    float_rgb(Bin, <<RGB/binary,(R*S):?F32,(G*S):?F32,(B*S):?F32>> );
float_rgb(<<>>, RGB) -> RGB.

float_g(<<G:8,Bin/binary>>, RGB) ->
    S = G/255,
    float_g(Bin, <<RGB/binary,S:?F32,S:?F32,S:?F32>> );
float_g(<<>>, RGB) -> RGB.

float_rgba(<<R:8,G:8,B:8,A:8,Bin/binary>>, RGB, A) ->
    S = 1/255,
    float_rgba(Bin, <<RGB/binary,(R*S):?F32,(G*S):?F32,(B*S):?F32>>,
	      <<A/binary, (A*S):?F32>>);
float_rgba(<<>>, RGB, A) -> {RGB,A}.    

float_a(<<A:8,Bin/binary>>, RGB, A) ->
    S = A/255,
    float_a(Bin, <<RGB/binary, 1.0:?F32, 1.0:?F32, 1.0:?F32>>,
	   <<A/binary, S:?F32>>);
float_a(<<>>, RGB, A) -> {RGB,A}.    

%%%%%
filter({R,G,B}) -> max(R,max(G,B));
filter({R,G,B,_}) -> max(R,max(G,B)).
    
sample_f(#material{m_info=#matte{kdOverPi=KdOPi}}, 
	 _RayD, _N, ShadeN = {NX,NY,NZ}) ->
    {X,Y,Z} = pbr_mc:sample_hemisphere(sfmt:uniform(), sfmt:uniform()),
    Pdf = Z * ?INV_PI,
    
    {{V1X,V1Y,V1Z}, {V2X,V2Y,V2Z}} = pbr_scene:coord_sys(ShadeN),
    Wi = {V1X * X + V2X * Y + NX * Z,
	  V1Y * X + V2Y * Y + NY * Z,
	  V1Z * X + V2Z * Y + NZ * Z},

    Dp = e3d_vec:dot(ShadeN, Wi),
    SpecBounce = false,
    case Dp =< 0.0001 of
	true -> 
	    {snew(), Wi, 0.0, SpecBounce};
	false ->	    
	    {KdOPi, Wi, Pdf / Dp, SpecBounce}
    end.

f(#material{m_info=#matte{kdOverPi=KdOPi}}, _W0,_Wi, _N) ->
    KdOPi.

is_diffuse(#material{m_info=#matte{}}) ->
    true.

is_light(Material) ->
    is_integer(Material).

%% Texture functions
lookup_texture(_Mat, _UV, _Type) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spd(Min, Max, Samples) when is_tuple(Samples) ->
    SN = tuple_size(Samples),
    Delta = (Max-Min)/(SN-1),
    #spd{n=SN, min=Min, max=Max, 
	 delta=Delta, invdelta=1.0/Delta, samples=Samples}.

irregular_spd(WLs, Samples) ->
    irregular_spd(WLs, Samples, 5).

irregular_spd(WLs, Samples0, Res) ->
    LambdaMin = element(1, WLs),
    LambdaMax = element(tuple_size(WLs), WLs),
    
    SN = trunc((LambdaMax - LambdaMin)/Res) +1,
    
    RevSamples = irregular_spd(LambdaMin, Res, LambdaMax, 1, WLs, Samples0, []),
    Samples = case SN-length(RevSamples) of
		  0 -> lists:reverse(RevSamples);
		  More -> lists:reverse(RevSamples, lists:duplicate(More, 0.0))
	      end,
    Delta = (LambdaMax-LambdaMin)/(SN-1),
    #spd{n=SN, min=LambdaMin, max=LambdaMax, 
	 delta=Delta, invdelta=1.0/Delta, samples=list_to_tuple(Samples)}.

irregular_spd(L, _Res, Max, _K, _WLs, _Ss, Acc) when L > Max ->
    Acc;
irregular_spd(L, Res, Max, K0, WLs, Ss, Acc) ->
    K = while_smaller(K0, L, WLs),
    case element(K, WLs) of
	L -> irregular_spd(L+Res, Res, Max, K, WLs, Ss, [element(K, Ss)|Acc]);
	WL1 -> 
	    WL0 = element(K-1, WLs),
	    Width = WL1 - WL0,
	    U = (L - WL0) / Width,
	    Sam = lerp(U, element(K-1, Ss), element(K, Ss)),
	    irregular_spd(L+Res, Res, Max, K, WLs, Ss, [Sam|Acc])
    end.

sample(_Lamba, #spd{n=N}) when N =< 1 -> 0.0;
sample(Lamba, #spd{min=Min}) when Lamba < Min -> 0.0;
sample(Lamba, #spd{max=Max}) when Lamba > Max -> 0.0;
sample(Lamba, #spd{n=N, min=Min, invdelta=ID, samples=S}) ->
    X = (Lamba - Min) * ID,
    B0 = trunc(X),
    B1 = min(B0+2, N),
    Dx = X - B0,
    lerp(Dx, element(B0+1, S), element(B1, S)).

spd_to_rgb(Spd = #spd{}) ->
    smul(spd_to_rgb(0, snew(), Spd), 683.0). 

spd_to_rgb(I0, {R,G,B}, Spd) when I0 < ?N_CIE ->
    V = sample(I0+?CIEstart, Spd),
    I1 = I0+1,
    RGB = {R+V*element(I1, ?CIE_X), 
	   G+V*element(I1, ?CIE_Y), 
	   B+V*element(I1, ?CIE_Z)},
    spd_to_rgb(I1, RGB, Spd);
spd_to_rgb(_, RGB, _) -> RGB.

while_smaller(K, L, T) when element(K,T) < L ->
    while_smaller(K+1, L, T);
while_smaller(K, _, T) -> 
    case tuple_size(T) < K of
	true -> tuple_size(T);
	false -> K
    end.
 
lerp(T, V1, V2) ->
    (1.0 - T) * V1 + T*V2.
    
