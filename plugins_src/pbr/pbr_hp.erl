%%
%%  pbr_hp.erl
%%
%%     Pbr hitpoint handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%
%%  An abstraction above array so I can change implementation later

-module(pbr_hp).
-export([new/2, update_const/4, update_hit/7, 
	 add_flux/5, accum_flux/1, fold_surface/3, splat_radiance/3]).

-include("pbr.hrl").

-record(hp,  %% The key is the screen ray origin i.e. w*h
 	{pos,					% 3D Position of hitpoint
	 n, 					% Normal
	 mat, 					% Material
	 w0, 					% Wo
	 color,					% Color

	 type,				        % The type of hit face
	 c_const =0,
	 c_surf  =0,

	 rad,					% Radius of hitpoint
	 photons=0,				% Number of photons
	 refl,					% Reflected spectrum
	 a_photons=0,				% Acc Photons
	 a_radiance,				% Static radiance
	 a_refl					% Acc Refl spectrum
	}).

new(_Size, MaxRad2) ->
    Black = pbr_mat:snew(),
    io:format("MaxR2 ~p~n",[MaxRad2]),
    array:new([{default, #hp{rad=MaxRad2, a_radiance=Black,
			     refl=Black,  a_refl=Black
			    }}]).
    
update_const(Pos, Type, Spectrum, Hps) ->
    Hp = #hp{a_radiance=R0} = array:get(Pos, Hps),
    R = pbr_mat:sadd(R0, Spectrum),
    array:set(Pos, Hp#hp{a_radiance=R, type=Type}, Hps).
    
update_hit(Pos, Point, Mat, Color, W0, N, Hps) ->
    Hp0 = array:get(Pos, Hps),
    Hp  = Hp0#hp{pos=Point, type=surface, mat=Mat, w0=W0, n=N, color=Color},
    array:set(Pos, Hp, Hps).

add_flux([Id|Ids], Point, Wi, PFlux, Hps) ->
    Hp0 = array:get(Id, Hps),
    #hp{pos=Pos, rad=R, w0=W0, n=N, mat=Mat, color=Color, 
	a_photons=AP, a_refl=AR0} = Hp0,
    case e3d_vec:dist_sqr(Point, Pos) > R of
	true  -> 
	    add_flux(Ids, Point, Wi, PFlux, Hps);
	false ->
	    Dot = e3d_vec:dot(Wi, N),
	    case Dot =< 0.00001 of
		true -> 
		    add_flux(Ids, Point, Wi, PFlux, Hps);
		false -> 
		    MatF = pbr_mat:f(Mat, W0, Wi, N),
		    Flux = pbr_mat:smul(pbr_mat:smul(PFlux, MatF),
					pbr_mat:smul(Color, Dot)), 
		    AR = pbr_mat:sadd(AR0, Flux),
		    HPA = array:set(Id, Hp0#hp{a_photons=AP+1, a_refl=AR}, Hps),
		    add_flux(Ids, Point, Wi, PFlux, HPA)
	    end
    end;
add_flux([], _, _, _, Hps) ->
    Hps.

accum_flux(Hp) ->
    Count = fun(_Id, Hit = #hp{type=surface, rad=R2, photons=PC, a_photons=APC,
			       refl=Refl, a_refl=ARefl, c_surf=SC}) ->
		    case APC > 0 of
			true ->
			    Count = PC+APC,
			    G = ?PPM_ALPHA * Count / (PC*?PPM_ALPHA + APC),
			    Flux = pbr_mat:smul(pbr_mat:sadd(Refl, ARefl), G),
			    Rad = R2*G,
			    Hit#hp{rad=Rad, a_photons=0, refl=Flux, photons=Count,
				   a_refl=pbr_mat:snew(), c_surf=SC+1};
			false ->
			    Hit
		    end;
	       (_, Hit = #hp{c_const=SC}) ->
		    Hit#hp{c_const=SC+1}
	    end,
    array:sparse_map(Count, Hp).

fold_surface(Fun, Acc, Hps) ->
    F1 = fun(Index, #hp{type=surface, pos=Pos, rad=Rad}, A) ->
		 Fun(Index, {Pos,Rad}, A);
	    (_, _, A) ->
		 A
	 end,
    array:sparse_foldl(F1, Acc, Hps).

splat_radiance(TotalPhotons, Renderer, Hp) ->
    Film0 = pbr_film:get_sample_buffer(Renderer),
    F = fun(Index, HP=#hp{type=Type, rad=R2}, {R0, MaxR2}) ->
		Radiance = calc_radiance(HP, TotalPhotons),
		case Type of
		    surface -> 
			{pbr_film:splat(Index, Radiance, R0), max(R2,MaxR2)};
		    _ ->
			{pbr_film:splat(Index, Radiance, R0), MaxR2}
		end
	end,
    {Film, MaxR2} = array:foldl(F, {Film0,0.0}, Hp),
    {pbr_film:splat_sample_buffer(Film, Renderer), MaxR2}.

calc_radiance(#hp{c_const=0, c_surf=0}, _) ->
    {0.0,0.0,0.0};
calc_radiance(#hp{c_const=CC, c_surf=CS, rad=Rad, a_radiance=AR, refl=Refl}, Total) ->
    Count = CC+CS,
    K = 1.0 / (?PI * Rad * Total),
    pbr_mat:sdiv(pbr_mat:sadd(AR,pbr_mat:smul(Refl,CS*K)), Count).
    %% case CC > CS of
    %% 	true -> {0.0,0.0,1.0};
    %% 	false -> {1.0,1.0,1.0}
    %% end.
	     
