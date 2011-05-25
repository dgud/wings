%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(pbr_light).

-export([init/1, init_arealight/4,
	 %% Get lights
	 lookup_id/2, get_light/2, get_infinite_light/1,
	 %% Pack binaries
	 pack_light/2, pack_light/1, pack_arealights/1,
	 %% Sample
	 sample_all_lights/1,
	 sample_L/1, sample_L/2,
	 pdf/1, le/2, power/1
	]).

-include("pbr.hrl").
-include("pbr_constants.hrl").

%% List of lights
%% Light is {type, type_specific_record}

-record(lights, {all, n2id}).

-record(point, {pos, intensity}).
-record(spot,  {pos, l2wq, intensity, dir, cos_w_max, cos_fall_start}).
-record(skylight, {gain, thetaS, phiS, 
		   zenith_Y, zenith_x, zenith_y,
		   perez_Y, perez_x, perez_y}). %% perez_? is a 6-tuple
-record(sunlight, {gain, dir, 
		   turbidity,  %% From 1.0 - 20+ 2-6 are most useful for clear days
		   relSize,
		   %% Vec for cone sampling
		   x, y, 
		   cosThetaMax, 
		   sunColor}).
-record(sunskylight, {sun, sky}).
-define(TRI_AREALIGHT_SZ, ((3*3+3+1+3)*4)).
-record(arealight,   {gain, tris}).  %% tris is <<V1,V2,V2,N,Area,Gain>>

%%--------------------------------------------------------------------
%% @doc
%% @spec create_lookup([]) -> {array(), proplist()}
%% @end
%%--------------------------------------------------------------------

init(Ls0) ->
    Ls = lists:map(fun({Name, LO}) -> 
			   case proplists:get_value(visible, LO, true) of
			       true ->
				   L = proplists:get_value(opengl,LO),
				   init_light(proplists:get_value(type, L), L, LO);
			       false ->
				   io:format("Ignoring disabled light ~p~n",[Name])
			   end
		   end, Ls0),
    La = array:from_list(Ls),
    {_,Lookup} = lists:foldl(fun({Light,_}, {Id, Acc}) ->
				     {Id+1, [{Light, Id}|Acc]}
			     end, {0, []}, Ls0),
    #lights{all=La, n2id=gb_trees:from_orddict(lists:sort(Lookup))}.

lookup_id(Name, #lights{n2id=Lookup}) ->
    gb_trees:get(Name, Lookup).

get_light(Id, #lights{all=Ls}) ->
    array:get(Id, Ls).

get_infinite_light(_Ls) ->
    false.

init_light(point, L, _Orig) ->
    Pos = proplists:get_value(position, L),
    {IR,IG,IB,_} = proplists:get_value(diffuse, L),
    I = 1.0,
    new({point, Pos, {IR*I,IG*I,IB*I}});
init_light(spot, L, _Orig) ->
    Pos = proplists:get_value(position, L),
    {IR,IG,IB,_} = proplists:get_value(diffuse, L),
    I = 1.0,
    new({point, Pos, {IR*I,IG*I,IB*I}});
init_light(infinite, L, Orig) ->
    {Dx,Dy,Dz,_} = proplists:get_value(diffuse, L),
    I = 1.0,
    Diff = {Dx*I,Dy*I,Dz*I},
    Pos  = proplists:get_value(position, L),
    Aim  = proplists:get_value(aim_point, L),
    Pbr  = proplists:get_value(pbr, Orig, []),
    Turb = proplists:get_value(turbulance, Pbr, 2.2),
    Size = proplists:get_value(size, Pbr, 5.5),
    Vec = e3d_vec:norm(e3d_vec:sub(Pos,Aim)),
    Sun = new({sunlight, Vec, Turb, Diff, Size}),
    Sky = new({skylight, Vec, Turb, Diff}),
    #sunskylight{sun=Sun,sky=Sky};

init_light(ambient, L, Orig) ->
    {Dx,Dy,Dz,_} = proplists:get_value(ambient, L),
    I = 1.0,
    Diff = {Dx*I,Dy*I,Dz*I},
    Pos  = proplists:get_value(position, L),
    Aim  = proplists:get_value(aim_point, L, {0.0,0.0,0.0}),
    Pbr  = proplists:get_value(pbr, Orig, []),
    Turb = proplists:get_value(turbulance, Pbr, 2.2),
    Vec  = e3d_vec:norm(e3d_vec:sub(Pos,Aim)),
    new({skylight, Vec, Turb, Diff});
init_light(area, L, Orig) ->
    {Dx,Dy,Dz,_} = proplists:get_value(diffuse, L),
    P = proplists:get_value(power, Orig, 1.0),
    #arealight{gain={P*Dx,P*Dy,P*Dz}}.

init_arealight(Name, N, GetTri, State=#lights{all=Ls0}) ->
    Id = lookup_id(Name, State),
    L = #arealight{gain=G} = array:get(Id, Ls0),
    Tris = make_arealight_bin(0, N, G, GetTri, <<>>),
    Ls = array:set(Id, L#arealight{tris=Tris}, Ls0),
    {State#lights{all=Ls}, G}.

make_arealight_bin(I, N, G={Gr,Gg,Gb}, GetTri, Acc) 
  when I < N ->
    {V1,V2,V3} = GetTri(I),
    {Nx,Ny,Nz} = e3d_vec:normal(V1,V2,V3),
    A = e3d_vec:area(V1,V2,V3),
    {V1x,V1y,V1z} = V1, {V2x,V2y,V2z} = V2, {V3x,V3y,V3z} = V2,
    Bin = <<Acc/binary, 
	    V1x:?F32, V1y:?F32, V1z:?F32,
	    V2x:?F32, V2y:?F32, V2z:?F32,
	    V3x:?F32, V3y:?F32, V3z:?F32, 
	    Nx:?F32,   Ny:?F32,  Nz:?F32,
	    A:?F32,  
	    Gr:?F32, Gg:?F32, Gb:?F32>>,
    make_arealight_bin(I+1, N, G, GetTri, Bin);
make_arealight_bin(_,_,_,_,Bin) ->
    Bin.

pack_arealights(#lights{all=Ls}) ->
    Bin = array:foldl(fun(_, #arealight{tris=Tris}, Acc) ->
			      <<Acc/binary, Tris/binary>>;
			 (_,_,Acc) -> Acc
		      end, <<>>, Ls),
    case Bin of 
	<<>> -> {0, undefined};
	_ -> {byte_size(Bin) div ?TRI_AREALIGHT_SZ, Bin}
    end.

pack_light(Type, #lights{all=Ls0}) ->
    Ls = array:to_list(Ls0),
    case find_light(Type, Ls) of
	undefined -> undefined;
	Light -> pack_light(Light)
    end.

pack_light(#skylight{gain={GR,GG,GB}, thetaS=ThetaS, phiS=PhiS, 
		     zenith_Y=ZY, zenith_x=Zx, zenith_y=Zy,
		     perez_Y=PY, perez_x=Px, perez_y=Py
		    }) -> 
    {PY1,PY2,PY3,PY4,PY5,PY6} = PY,
    {Px1,Px2,Px3,Px4,Px5,Px6} = Px,
    {Py1,Py2,Py3,Py4,Py5,Py6} = Py,

    <<GR:?F32, GG:?F32, GB:?F32,
      ThetaS:?F32, PhiS:?F32,
      ZY:?F32, Zx:?F32, Zy:?F32,
      PY1:?F32,PY2:?F32,PY3:?F32,PY4:?F32,PY5:?F32,PY6:?F32,
      Px1:?F32,Px2:?F32,Px3:?F32,Px4:?F32,Px5:?F32,Px6:?F32,
      Py1:?F32,Py2:?F32,Py3:?F32,Py4:?F32,Py5:?F32,Py6:?F32
    >>;

pack_light(#sunlight{gain={GR,GG,GB}, 
		     dir={Vx,Vy,Vz}, 
		     turbidity=Turb,
		     relSize=Sz,
		     x={Xx,Xy,Xz}, y={Yx, Yy,Yz}, 
		     cosThetaMax=CTM, 
		     sunColor={SR,SG,SB}
		    }) ->     
    <<Vx:?F32, Vy:?F32, Vz:?F32,
      GR:?F32, GG:?F32, GB:?F32,
      Turb:?F32, Sz:?F32, 
      Xx:?F32, Xy:?F32, Xz:?F32,
      Yx:?F32, Yy:?F32, Yz:?F32,
      CTM:?F32, 
      SR:?F32, SG:?F32, SB:?F32
    >>.

find_light(sunlight, [#sunskylight{sun=Sun}|_]) -> Sun;
find_light(skylight, [#sunskylight{sky=Sky}|_]) -> Sky;    
find_light(Type, [Light|_]) when element(1,Light) =:= Type ->
    Light;
find_light(Type, [_|Ls]) -> find_light(Type, Ls);
find_light(_, []) -> undefined.


%%--------------------------------------------------------------------
%% @doc
%% @spec new(term) -> light()
%% @end
%%--------------------------------------------------------------------
new({point,Pos,Intensity}) ->
    #point{pos=Pos, intensity=Intensity};
new({spot,Pos,Dir0,Intensity, Width, Falloff}) ->
    CosW = math:cos(Width*?PI/180.0),
    CosF = math:cos(Falloff*?PI/180.0),
    Dir = e3d_vec:norm(Dir0),
    #spot{pos=Pos, intensity=Intensity, dir=Dir, 
	  l2wq=e3d_q:rotate_s_to_t({0.0,0.0,1.0}, Dir),
	  cos_w_max=CosW, cos_fall_start=CosF};
new({sunlight, Vec, Turb, Diff, RelSize}) ->
    {X,Y} = pbr_scene:coord_sys(Vec),
    SunRadius = 695500.0,
    SunMeanDistance = 149600000.0,
    
    CosThetaMax = 
	case RelSize * SunRadius of
	    Size when Size =< SunMeanDistance ->
		Sin2 = Size / SunMeanDistance,
		Sin2ThetaMax = Sin2 * Sin2,
		math:sqrt(1.0 - Sin2ThetaMax);
	    _ ->
		%% Sin2ThetaMax = 1.0,
		0.0
	end,
    ThetaS = min(1.638555, pbr_mc:spherical_theta(Vec)),
    
    Beta = 0.04608365822050 * Turb - 0.04586025928522,
    M = 1.0 / (math:cos(ThetaS) + 0.00094 * 
		   math:pow(1.6386 - ThetaS, -1.253)),
    Spd = calc_sun_spd(M, Beta),
    Suncolor = pbr_mat:sdiv(pbr_mat:smul(Diff,pbr_mat:spd_to_rgb(Spd)),
			    (1000000000.0 / (?PI * 100.0 * 100.0))),
    #sunlight{gain=Diff, dir=Vec,
	      turbidity=Turb, relSize=RelSize,
	      x=X, y=Y,
	      cosThetaMax=CosThetaMax,
	      sunColor=Suncolor};

new({skylight, Vec, T, Diff}) ->
    ThetaS = max(0.0, pbr_mc:spherical_theta(Vec)),
    PhiS   = pbr_mc:spherical_phi(Vec),
    Aconst = 1.0,
    Bconst = 1.0,
    Cconst = 1.0,
    Dconst = 1.0,
    Econst = 1.0,
    
    Theta2 = ThetaS*ThetaS,    
    Theta3 = Theta2*ThetaS,
    T2 = T * T,

    Chi = (4.0 / 9.0 - T / 120.0) * (?PI - 2.0 * ThetaS),
    Zenith_Y0 = (4.0453 * T - 4.9710) * math:tan(Chi) - 0.2155 * T + 2.4192,
    Zenith_Y1  = Zenith_Y0*0.06,

    Zenith_x1 =
	(0.00166 * Theta3 - 0.00375 * Theta2 + 0.00209 * ThetaS) * T2 +
	(-0.02903 * Theta3 + 0.06377 * Theta2 - 0.03202 * ThetaS + 0.00394) * T +
	(0.11693 * Theta3 - 0.21196 * Theta2 + 0.06052 * ThetaS + 0.25886),

    Zenith_y1 =
	(0.00275 * Theta3 - 0.00610 * Theta2 + 0.00317 * ThetaS) * T2 +
	(-0.04214 * Theta3 + 0.08970 * Theta2 - 0.04153 * ThetaS  + 0.00516) * T +
	(0.15346 * Theta3 - 0.26756 * Theta2 + 0.06670 * ThetaS  + 0.26688),

    Perez_Y = {0.0, 
	       (0.1787 * T  - 1.4630) * Aconst,
	       (-0.3554 * T  + 0.4275) * Bconst,
	       (-0.0227 * T  + 5.3251) * Cconst,
	       (0.1206 * T  - 2.5771) * Dconst,
	       (-0.0670 * T  + 0.3703) * Econst},
    
    Perez_x = {0.0, 
	       (-0.0193 * T  - 0.2592) * Aconst,
	       (-0.0665 * T  + 0.0008) * Bconst,
	       (-0.0004 * T  + 0.2125) * Cconst,
	       (-0.0641 * T  - 0.8989) * Dconst,
	       (-0.0033 * T  + 0.0452) * Econst},

    Perez_y = {0.0, 
	       (-0.0167 * T  - 0.2608) * Aconst,
	       (-0.0950 * T  + 0.0092) * Bconst,
	       (-0.0079 * T  + 0.2102) * Cconst,
	       (-0.0441 * T  - 1.6537) * Dconst,
	       (-0.0109 * T  + 0.0529) * Econst},

    Zenith_Y = Zenith_Y1 / perezBase(Perez_Y, 0, ThetaS),
    Zenith_x = Zenith_x1 / perezBase(Perez_x, 0, ThetaS),
    Zenith_y = Zenith_y1 / perezBase(Perez_y, 0, ThetaS),
    
    #skylight{gain=Diff, 
	      thetaS=ThetaS, phiS=PhiS, 
	      zenith_Y=Zenith_Y, zenith_x=Zenith_x, zenith_y=Zenith_y, 
	      perez_Y=Perez_Y, perez_x=Perez_x, perez_y=Perez_y
	     };

new(_) -> undefined.


perezBase(Lam, Theta, Gamma) ->
    (1.0 + element(2,Lam) * math:exp(element(3,Lam) / math:cos(Theta))) *
	(1.0 + element(4,Lam) * math:exp(element(5,Lam) * Gamma)  
	 + element(6,Lam) * math:cos(Gamma) * math:cos(Gamma)).

%%

calc_sun_spd(M, Beta) ->
    K_oCurve = pbr_mat:irregular_spd(?sun_k_oWavelengths, ?sun_k_oAmplitudes),
    K_gCurve = pbr_mat:irregular_spd(?sun_k_gWavelengths, ?sun_k_gAmplitudes),
    K_waCurve= pbr_mat:irregular_spd(?sun_k_waWavelengths, ?sun_k_waAmplitudes),
    SolCurve = pbr_mat:spd(380, 750, ?sun_solAmplitudes),
    
    Lspd = calc_sun_lspd(350, M,  Beta, K_oCurve, K_gCurve, K_waCurve, SolCurve),
    pbr_mat:spd(350, 800, list_to_tuple(Lspd)).

calc_sun_lspd(Lambda, M, Beta, K_oCurve, K_gCurve, K_waCurve, SolCurve) 
  when Lambda < 801.0 ->
    %% Rayleigh Scattering
    TauR = math:exp( -M * 0.008735 * math:pow(Lambda / 1000.0, -4.08)),
    %% Aerosol (water + dust) attenuation
    %% beta - amount of aerosols present
    %% alpha - ratio of small to large particle sizes. (0:4,usually 1.3)
    Alpha = 1.3,    
    TauA = math:exp(-M * Beta * math:pow(Lambda/1000.0, -Alpha)),  %% lambda should be in um
    %% Attenuation due to ozone absorption
    %% lOzone - amount of ozone in cm(NTP)
    LOzone = 0.35,    
    TauO = math:exp(-M * pbr_mat:sample(Lambda, K_oCurve) * LOzone),
    %% Attenuation due to mixed gases absorption
    KgSample = pbr_mat:sample(Lambda, K_gCurve),
    TauG = math:exp(-1.41 * KgSample * M / 
			math:pow(1.0 + 118.93 * KgSample * M, 0.45)),
    %% Attenuation due to water vapor absorbtion
    %% w - precipitable water vapor in centimeters (standard = 2)
    W = 2.0,    
    KwaSample = pbr_mat:sample(Lambda, K_waCurve), 
    TauWA = math:exp(-0.2385 * KwaSample* W * M /
			 math:pow(1.0 + 20.07 * KwaSample * W * M, 0.45)),
    Ldata = pbr_mat:sample(Lambda, SolCurve) * TauR * TauA * TauO * TauG * TauWA,
    [Ldata|calc_sun_lspd(Lambda + 5.0, M, Beta, 
			 K_oCurve, K_gCurve, K_waCurve, SolCurve)];
calc_sun_lspd(_, _, _, _, _, _, _) ->
    [].


%%--------------------------------------------------------------------
%% @doc
%% @spec sample_all_lights([Lights]) -> {Light, Pdf :: float()}
%% @end
%%--------------------------------------------------------------------

sample_all_lights(Lights) ->
    N = array:size(Lights),
    U = sfmt:uniform(),
    Light = array:get(trunc(U*N), Lights),
    {Light, 1.0/N}.

%%--------------------------------------------------------------------
%% @doc  
%% @spec sample_L(Light, Point) -> {::spectrum(), Wi::vector(), Pdf :: float()}
%% @end
%%--------------------------------------------------------------------
sample_L(#point{pos=LPos, intensity=I}, Point) ->
    Wi = e3d_vec:norm_sub(LPos, Point),
    {e3d_vec:divide(I, e3d_vec:dist_sqr(LPos, Point)), Wi, 1.0};
sample_L(S=#spot{pos=LPos, intensity=I}, Point) ->
    Wi = e3d_vec:norm_sub(LPos, Point),
    Fallof = spot_falloff(S,Wi),
    {e3d_vec:mul(I, Fallof/e3d_vec:dist_sqr(LPos, Point)), Wi, 1.0}.

%%--------------------------------------------------------------------
%% @doc  
%% @spec sample_L(Light) -> {::spectrum(), ray(), Pdf :: float()}
%% @end
%%--------------------------------------------------------------------
sample_L(#point{pos=LPos, intensity=I}) ->
    Dir = pbr_mc:sample_sphere(sfmt:uniform(),sfmt:uniform()),
    Ray = #ray{o=LPos, d=Dir},
    {I, Ray, pbr_mc:sphere_pdf()};
sample_L(S=#spot{pos=LPos, intensity=I, l2wq=L2Wq, cos_w_max=CosW}) ->
    Vec = pbr_mc:sample_cone(sfmt:uniform(),sfmt:uniform(),CosW),
    Dir = e3d_q:vec_rotate(Vec, L2Wq),
    Ray = #ray{o=LPos, d=Dir},
    {e3d_vec:mul(I,spot_falloff(S,Dir)), Ray, pbr_mc:cone_pdf(CosW)}.


%%--------------------------------------------------------------------
%% @doc  
%% @spec power(Light) -> float()
%% @end
%%--------------------------------------------------------------------
power(#point{intensity=I}) ->
    4.0 * ?PI * I;
power(#spot{intensity=I, cos_w_max=CosW, cos_fall_start=CosF}) ->
    2.0 * ?PI * I * (1.0 - 0.5*(CosF+CosW)).

%%--------------------------------------------------------------------
%% @doc  
%% @spec pdf(Light) -> float()
%% @end
%%--------------------------------------------------------------------
pdf(_) ->
    0.0.

%%--------------------------------------------------------------------
%% @doc  
%% @spec le(Light) -> spectrum().
%% @end
%%--------------------------------------------------------------------
le(#point{intensity=I}, _Ray) ->
    I;
le(_Light, _Ray) ->
    {0.0,0.0,0.0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spot_falloff(#spot{dir=Dir, cos_w_max=CosW, cos_fall_start=CosF}, Wi) ->
    CosT = abs(e3d_vec:dot(Dir,Wi))*2.0,
    if CosT < CosW -> 0.0;
       CosT > CosF -> 1.0;
       true ->
	    Delta = (CosT-CosW) / (CosF-CosW),
	    Delta*Delta*Delta*Delta
    end.

