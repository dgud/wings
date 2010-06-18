%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_sppm).

%%-include("e3d.hrl").
-include("pbr.hrl").

-export([start/2]).

%% %TODO
%% {{pbr_hp,splat_radiance,3},{pbr_film,splat,3}},

-record(s,
	{renderer,				% Render state
	 %% Photon info
	 max_rad2,				% Max Photon Radius
	 ps=0,   				% Current Photons
	 %% Stats
	 no_p = 0,			        % Total number photons
	 pass = 0,				% Number of photon passes
	 %% Hitpoint info
	 hp,					% Hitpoints
	 lup}).					% Hitpoint lookup

start(Attrs, Renderer) ->
    Scale = proplists:get_value(photon_radius_scale, Attrs, 1.0),
    {BBmin,BBmax}  = pbr_scene:bb(Renderer),
    {Sx,Sy,Sz} = e3d_vec:sub(BBmax, BBmin),
    {W, H} = pbr_camera:get_size(Renderer), 
    PhotonRadius = Scale * ((Sx+Sy+Sz) / 3.0) / 100, %((W+H)/2) *2,
    PRad2 = PhotonRadius * PhotonRadius,
    io:format("BB ~p ~p~n", [BBmin,BBmax]),
    io:format("R ~p MR2 ~p~n", [PhotonRadius, PRad2]),
    S1 = init_hitpoints(#s{renderer=Renderer,
			   hp = pbr_hp:new(W*H, PRad2),
			   max_rad2 = PRad2}),
    S2 = init_photon_pass(S1),
    io:format("~p mem ~p kb ~n",[?LINE, erlang:process_info(self(), memory)]),
    loop(S2),  
    io:format("Stopping the renderer~n~n",[]),
    normal.

loop(S0) ->
    S1 = ?TC(photon_passes(S0)),
    S2 = ?TC(accum_flux(S1)),
    S3 = ?TC(eval_hitpoints(S2)),
    ?TC(pbr_film:show(S3#s.renderer)),
    S4 = ?TC(init_hitpoints(S3)),
    io:format("~p mem ~p kb ~n", [?LINE, element(2,erlang:process_info(self(), memory)) div 1024]),
    receive stop -> ok
    after 0 -> loop(S4)
    end.

accum_flux(S = #s{hp=Hp}) ->
    S#s{hp=pbr_hp:accum_flux(Hp)}.

eval_hitpoints(S = #s{renderer=R0, hp=Hp, no_p=TotalPhotons}) ->
    {R, MaxRadius} = pbr_hp:splat_radiance(TotalPhotons, R0, Hp),
    io:format("Total photons: ~p Max radius ~f~n",[TotalPhotons, MaxRadius]),
    S#s{renderer=R, max_rad2=MaxRadius}.

%% Photon pass
photon_passes(S0) ->
    photon_passes(0, S0).
photon_passes(N, S = #s{renderer=R, ps=Ps0, lup=Lup, hp=Hp0}) 
  when N < ?PHOTONS_PER_PASS ->
    {Buffer, []} = create_raybuffer(Ps0, ?MAX_RAYS, <<>>),
    {_, Hits} = pbr_scene:intersect(Buffer, R),
    {Count, Ps, Hp} = photon_pass(Ps0, Hits, R, Lup, Hp0, 0, []),
    photon_passes(Count+N, S#s{ps=Ps, hp=Hp});
photon_passes(Count, S=#s{no_p=Ps}) ->
    S#s{no_p=Ps+Count}.

photon_pass([_|Ps], <<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
	    R, Lup, Hp, New, Acc) -> %% Miss; cast a new photon
    PPath = init_photon_path(R),
    photon_pass(Ps, Rest, R, Lup, Hp, New+1, [PPath|Acc]);
photon_pass([{Ray=#ray{d=RayD},Flux,Depth}|Ps],
	    <<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
	    R, Lup, Hp0, New, Acc) -> %% Hit something
    case pbr_scene:get_face_info(Ray,T,B1,B2,Face,R) of
	{transparent, Pos} ->
	    photon_pass(Ps,Rest,R,Lup,Hp0,New,[{Ray#ray{o=Pos},Flux,Depth}|Acc]);
	{light, Pos, _Light} -> %% Let light go through ligths
	    photon_pass(Ps,Rest,R,Lup,Hp0,New,[{Ray#ray{o=Pos},Flux,Depth}|Acc]);
	{Point, Mat, SurfaceCol, N, ShadeN} ->
	    {F0, Wi, Fpdf, SpecBounce} = pbr_mat:sample_f(Mat, RayD, N, ShadeN),
	    Hp = if SpecBounce == false ->
			 add_flux(Point, RayD, Flux, Lup, Hp0);
		    true ->
			 Hp0
		 end,
	    case Depth < ?MAX_PHOTON_DEPTH of
		false ->
		    PPath = init_photon_path(R),
		    photon_pass(Ps, Rest, R, Lup, Hp, New+1, [PPath|Acc]);
		true ->
		    F  = pbr_mat:smul(F0, SurfaceCol),
		    PFlux = pbr_mat:smul(Flux, pbr_mat:smul(F,Fpdf)),
		    IsBlack = pbr_mat:s_is_black(PFlux),
		    Rand = sfmt:uniform(),
		    if IsBlack -> 
			    PPath = init_photon_path(R),
			    photon_pass(Ps,Rest,R,Lup,Hp,New+1,[PPath|Acc]);
		       Depth < 1 orelse SpecBounce ->
			    PPath = {#ray{o=Point,d=Wi}, PFlux, Depth+1},
			    photon_pass(Ps,Rest,R,Lup,Hp,New,[PPath|Acc]);
		       Rand < 0.7 -> %% Russian Roulette
			    PFluxRR = pbr_mat:smul(PFlux, 1.0/0.7),
			    PPath = {#ray{o=Point,d=Wi}, PFluxRR, Depth+1},
			    photon_pass(Ps,Rest,R,Lup,Hp,New,[PPath|Acc]);
		       true ->
			    PPath = init_photon_path(R),
			    photon_pass(Ps,Rest,R,Lup,Hp,New+1,[PPath|Acc])
		    end
	    end
    end;
photon_pass([], <<>>, _R, _Lup, Hp, New, Acc) -> 
    {New, Acc, Hp}.

init_photon_path(Renderer) ->
    Ls = pbr_scene:get_lights(Renderer),
    {Light, Lpdf} = pbr_light:sample_all_lights(Ls),
    {Flux0, Ray, Pdf} = pbr_light:sample_L(Light),
    Flux = pbr_mat:sdiv(Flux0,(Lpdf*Pdf)),
    {Ray, Flux, 0}.

add_flux(Point, RayD, Flux, Lup, Hp) -> 
    HPs = pbr_grid:nearest(Point, Lup),
    pbr_hp:add_flux(HPs, Point, e3d_vec:neg(RayD), Flux, Hp).

%% init_photon_pass
init_photon_pass(S = #s{renderer=R}) ->
    Photons = init_photon_pass(0, R, []),
    S#s{ps=Photons, no_p=?MAX_RAYS}.

init_photon_pass(N, R, Acc) when N < ?MAX_RAYS ->
    Photon = init_photon_path(R),
    init_photon_pass(N+1, R, [Photon|Acc]);
init_photon_pass(_, _, Acc) -> Acc.

%% Cam to hitpoint pass
init_hitpoints(State0=#s{renderer=Renderer, hp=HP0, max_rad2=Rad2}) ->
    HP1 = cam_pass(HP0, State0),
    BB  = pbr_scene:bb(Renderer),
    G0  = pbr_grid:init(HP1, Rad2, BB),
    State0#s{hp=HP1, lup=G0}.

cam_pass(Hp0, #s{renderer=Renderer}) ->
    {W,H} = pbr_camera:get_size(Renderer),
    Spectrum = {1.0,1.0,1.0},
    Rays = [{pbr_camera:generate_ray(Renderer, 
				     float(X)+sfmt:uniform(), 
				     float(Y)+sfmt:uniform()),
	     X+Y*W,0,Spectrum}
	    || Y <- lists:seq(0, H-1), X <- lists:seq(0, W-1)],
    trace_rays(Rays, Hp0, Renderer).

trace_rays([], Hp, _) -> Hp;
trace_rays(Rays0, Hp0, Renderer) ->
    {Buffer, Rays1} = create_raybuffer(Rays0, ?MAX_RAYS, <<>>),
    {_RaysB, Hits}  = pbr_scene:intersect(Buffer, Renderer),
    {Hp, Rays} = update_hitpoints(Hits, Rays0, Rays1, Renderer, Hp0),
    trace_rays(Rays, Hp, Renderer).

update_hitpoints(<<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
		 [{Ray,Pos,_,TP}|Rays], Work, R, Hp0) -> 
    %% Miss, check background
    Troughput = 
	case pbr_scene:get_infinite_light(R) of
	    false -> 
		{1.0, 0.0, 0.0};   %DEBUG
	    %% pbr_mat:snew();
	    Light -> 
		pbr_mat:smul(pbr_light:le(Light, Ray),TP)
	end,
    Hp = pbr_hp:update_const(Pos, background, Troughput, Hp0),
    update_hitpoints(Rest, Rays, Work, R, Hp);
update_hitpoints(<<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
		 [{Ray, Pos, Depth, TP}|Rays], Work, R, Hp0) -> 
    %% Hit
    case pbr_scene:get_face_info(Ray,T,B1,B2,Face,R) of
	{transparent, Point} ->
	    update_hitpoints(Rest,Rays, [{Ray#ray{o=Point},Pos,Depth,TP}|Work], R, Hp0);
	{light, _Point, LightId} ->
	    Light = pbr_light:get_light(LightId,pbr_scene:get_lights(R)),
	    TPd = pbr_mat:smul(pbr_light:le(Light, Ray), TP),
	    Hp  = pbr_hp:update_const(Pos, light, TPd, Hp0),
	    update_hitpoints(Rest, Rays, Work, R, Hp);
	{Point, Mat, SurfaceCol, N, ShadeN} ->
	    {F0, Wi, Fpdf, SpecBounce} = pbr_mat:sample_f(Mat, Ray, N, ShadeN),
	    IsNotDiffuse = not pbr_mat:is_diffuse(Mat),
	    if 
		Fpdf =:= 0.0, F0 =:= {0.0,0.0,0.0} -> %% Black
		    Hp = pbr_hp:update_const(Pos, black_surface, pbr_mat:snew(), Hp0),
		    update_hitpoints(Rest, Rays, Work, R, Hp);
		SpecBounce, IsNotDiffuse ->
		    case Depth > ?MAX_EYE_DEPTH of
			false ->
			    TPd = pbr_mat:smul(TP, pbr_mat:sdiv(F0, Fpdf)),
			    Bounce = {#ray{o=Point, d=Wi}, Pos, Depth+1, TPd},
			    update_hitpoints(Rest, Rays, [Bounce|Work], R, Hp0);
			true ->
			    Hp = pbr_hp:update_const(Pos, max_bounce, pbr_mat:snew(), Hp0),
			    update_hitpoints(Rest, Rays, Work, R, Hp)
		    end;
		true ->
		    W0 = e3d_vec:neg(Ray#ray.d),
		    Color = pbr_mat:smul(SurfaceCol, TP),
		    Hp = pbr_hp:update_hit(Pos, Point, Mat, Color, W0, ShadeN, Hp0),
		    update_hitpoints(Rest, Rays, Work, R, Hp)
	    end
    end;
update_hitpoints(<<>>, _, Work, _, Hp) ->
    {Hp, Work}.

create_raybuffer([RayInfo|Rest], No, Buff0) 
  when No > 0 ->
    #ray{o={OX,OY,OZ},d={DX,DY,DZ},n=N,f=F} = element(1, RayInfo),
    Buff = <<Buff0/binary,  
	     OX:?F32, OY:?F32, OZ:?F32,
	     DX:?F32, DY:?F32, DZ:?F32, 
	     N:?F32,  F:?F32>>,
    create_raybuffer(Rest, No-1, Buff);
create_raybuffer(Rays, _, Buff) ->
    {{byte_size(Buff) div ?RAY_SZ, Buff}, Rays}.


