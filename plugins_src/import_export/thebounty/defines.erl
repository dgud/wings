%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%  See AUTHORS.txt for a complete list of authors.
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the GNU General Public License for more details.
%%

-define(TAG, thebounty).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, thebounty_render).

key(Key) -> {key,?KEY(Key)}.
-define(LOCAL_MODULE, ?MODULE).

-define(NONZERO, 1.0e-10).

%%% Default values
-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "thebounty-xml").
-define(DEF_PLUGINS_PATH, "plugins").
-define(DEF_OPTIONS, "").

%% Shader
-define(DEF_FRESNEL, false).
-define(DEF_MATERIAL_TYPE, shinydiffuse).
-define(DEF_VOLUME_SIGMA_A, 0.4).
-define(DEF_VOLUME_SIGMA_S, 0.05).
-define(DEF_VOLUME_HEIGHT, 0.5).
-define(DEF_VOLUME_STEEPNESS, 0.2).
-define(DEF_VOLUME_ATTGRIDSCALE, 3).
-define(DEF_VOLUME_COVER, 0.05).
-define(DEF_VOLUME_DENSITY, 1.0).
-define(DEF_LIGHTPORTAL_POWER, 2.0).
-define(DEF_LIGHTPORTAL_SAMPLES, 24).
-define(DEF_LIGHTPORTAL_DIFFUSEPHOTONS, false).
-define(DEF_LIGHTPORTAL_CAUSTICPHOTONS, false).
-define(DEF_LIGHTPORTAL_PHOTON_ONLY, true).
-define(DEF_MESHLIGHT_POWER, 5.0).
-define(DEF_MESHLIGHT_SAMPLES, 16).
-define(DEF_MESHLIGHT_COLOR, {1.0,1.0,1.0}).
-define(DEF_MESHLIGHT_DOUBLE_SIDED, false).
-define(DEF_USE_HARDNESS, false).
-define(DEF_AUTOSMOOTH, false).
-define(DEF_AUTOSMOOTH_ANGLE, 181.0).
-define(DEF_SSS_ABSORPTION_COLOR, {0.649,0.706,0.655}).
-define(DEF_SCATTER_COLOR, {0.599,0.680,0.511}).
-define(DEF_SSS_SPECULAR_COLOR, {1.0,1.0,1.0}).
-define(DEF_ABSORPTION_DIST, 3.0).
-define(DEF_DISPERSION_POWER, 0.0).
-define(DEF_DISPERSION_SAMPLES, 10).


%% Render
-define(DEF_LIGHTING_METHOD, directlighting).
-define(DEF_PM_DIFFUSE_PHOTONS, 500000).
-define(DEF_PM_BOUNCES, 5).
-define(DEF_PM_SEARCH, 100).
-define(DEF_PM_DIFFUSE_RADIUS, 1.0).
-define(DEF_PM_CAUSTIC_PHOTONS, 500000).
-define(DEF_PM_CAUSTIC_RADIUS, 1.0).
-define(DEF_PM_CAUSTIC_MIX, 100).
-define(DEF_PM_USE_FG, true).
-define(DEF_PM_FG_BOUNCES, 3).
-define(DEF_PM_FG_SAMPLES, 100).
-define(DEF_PM_FG_SHOW_MAP, false).
-define(DEF_PT_DIFFUSE_PHOTONS, 500000).
-define(DEF_PT_BOUNCES, 5).
-define(DEF_PT_CAUSTIC_TYPE, path).
-define(DEF_PT_CAUSTIC_RADIUS, 1.0).
-define(DEF_PT_CAUSTIC_MIX, 100).
-define(DEF_PT_CAUSTIC_DEPTH, 10).
-define(DEF_PT_SAMPLES, 32).
-define(DEF_SPPM_PHOTONS, 500000).
-define(DEF_SPPM_BOUNCES, 4).
-define(DEF_SPPM_SEARCH, 100).
-define(DEF_SPPM_RADIUS, 1.0).
-define(DEF_SPPM_TIMES, 1.0).
-define(DEF_SPPM_PASSES, 1).
-define(DEF_SPPM_IRE, false).
-define(DEF_VOLINTEGR_TYPE, none).
-define(DEF_VOLINTEGR_ADAPTIVE, true).
-define(DEF_VOLINTEGR_OPTIMIZE, true).
-define(DEF_VOLINTEGR_STEPSIZE, 0.2).
-define(DEF_USE_SSS, false).
-define(DEF_SSS_PHOTONS, 1000).
-define(DEF_SSS_DEPTH, 10.0).
-define(DEF_SSS_SCALE, 2.0).
-define(DEF_SSS_SINGLESCATTER_SAMPLES, 32).
-define(DEF_USE_CAUSTICS, false).
-define(DEF_CAUSTIC_PHOTONS, 900000).
-define(DEF_CAUSTIC_DEPTH, 10).
-define(DEF_CAUSTIC_MIX, 200).
-define(DEF_CAUSTIC_RADIUS, 0.5).
-define(DEF_DO_AO, false).
-define(DEF_AO_DISTANCE, 1.0).
-define(DEF_AO_SAMPLES, 16.0).
-define(DEF_AO_COLOR, {1.0,1.0,1.0}).
-define(DEF_AA_PASSES, 3).
-define(DEF_AA_MINSAMPLES, 1).
-define(DEF_AA_THRESHOLD, 0.02).
-define(DEF_CLAMP_RGB, true).
-define(DEF_AA_FILTER_TYPE, box).
-define(DEF_TRANSPARENT_SHADOWS, false).
-define(DEF_BACKGROUND_TRANSP_REFRACT, false).
-define(DEF_SHADOW_DEPTH, 2).
-define(DEF_RAYDEPTH, 3).
-define(DEF_BIAS, 0.001).
-define(DEF_WIDTH, 320).
-define(DEF_HEIGHT, 240).
-define(DEF_LENS_TYPE, perspective).
-define(DEF_LENS_ORTHO_SCALE, 7.0).
-define(DEF_LENS_ANGULAR_CIRCULAR, true).
-define(DEF_LENS_ANGULAR_MIRRORED, false).
-define(DEF_LENS_ANGULAR_MAX_ANGLE, 90.0).
-define(DEF_LENS_ANGULAR_ANGLE, 90.0).
-define(DEF_APERTURE, 0.0).
-define(DEF_BOKEH_TYPE, triangle).
-define(DEF_BOKEH_BIAS, uniform).
-define(DEF_BOKEH_ROTATION, 0.0).
-define(DEF_DOF_DISTANCE, 7.0).

%% Light
-define(DEF_ATTN_POWER, 10.0).
-define(DEF_POINT_TYPE, pointlight).

%% Spotlight
-define(DEF_SPOT_TYPE, spotlight).
-define(DEF_CONE_ANGLE, 45.0).
-define(DEF_SPOT_EXPONENT, 2.0).
-define(DEF_SPOT_BLEND, 0.5).
-define(DEF_SPOT_PHOTON_ONLY, false).
-define(DEF_SPOT_SOFT_SHADOWS, false).
-define(DEF_SPOT_FUZZYNESS, 1.0).

%% IESlight
-define(DEF_SPOT_IES_FILENAME, "").
-define(DEF_SPOT_IES_SAMPLES, 16).

%% Photonlight
-define(DEF_MODE,caustic).
-define(DEF_PHOTONS,5000000).
-define(DEF_SEARCH,64).
-define(DEF_DEPTH,3).
-define(DEF_CAUS_DEPTH,4).
-define(DEF_DIRECT,false).
-define(DEF_MINDEPTH,1).
-define(DEF_FIXEDRADIUS,0.08).
-define(DEF_CLUSTER,0.01).

%% Softlight
-define(DEF_RES, 100).
-define(DEF_RADIUS, 1).

%% Sunlight
-define(DEF_POWER, 1.0).
-define(DEF_BACKGROUND_COLOR, {0.25,0.25,0.50}).
-define(DEF_HORIZON_COLOR, {1.0,1.0,1.0}).
-define(DEF_ZENITH_COLOR, {0.4,0.5,1.0}).
-define(DEF_GRADIENT_BACK_POWER, 1.0).
-define(DEF_TURBIDITY, 3.0).
-define(DEF_SUNSKY_VAR, 1.0).
-define(DEF_SUN_SAMPLES, 16).
-define(DEF_SUN_ANGLE, 0.5).
-define(DEF_SKY_BACKGROUND_LIGHT, false).
-define(DEF_SKY_BACKGROUND_POWER, 1.0).
-define(DEF_SKY_BACKGROUND_SAMPLES, 16).
-define(DEF_DARKSKY_ALTITUDE, 0.0).
-define(DEF_DARKSKY_NIGHT, false).
-define(DEF_DARKSKY_DIFFUSEPHOTONS, false).
-define(DEF_DARKSKY_CAUSTICPHOTONS, false).
-define(DEF_SUN_REAL, false).
-define(DEF_SUN_REAL_POWER, 5.0).

%% Infinite Light
-define(DEF_INFINITE_TYPE, sunlight).

%% Modulator
-define(MAX_MODULATORS, 4).
-define(DEF_MOD_DEFCOLOR, {1.0,0.0,1.0}).
-define(DEF_MOD_ENABLED, true).
-define(DEF_MOD_MODE, mix).
-define(DEF_MOD_SIZE, 1.0).
-define(DEF_MOD_SIZE_X, 1.0).
-define(DEF_MOD_SIZE_Y, 1.0).
-define(DEF_MOD_SIZE_Z, 1.0).
-define(DEF_MOD_OPACITY, 1.0).
-define(DEF_MOD_DIFFUSE, 1.0).
-define(DEF_MOD_COLORFACTOR, 1.0).
-define(DEF_MOD_SHININESS, 1.0).
-define(DEF_MOD_NORMAL, 0.0).
-define(DEF_MOD_TEXTURETYPE, clouds).
-define(DEF_MOD_FILENAME, "").
-define(DEF_MOD_COLOR1, {0.0,0.0,0.0}).
-define(DEF_MOD_COLOR2, {1.0,1.0,1.0}).
-define(DEF_MOD_DEPTH, 8).
-define(DEF_MOD_NOISEBASIS, blender).
-define(DEF_MOD_NOISESIZE, 1.75).
-define(DEF_MOD_HARD, true).
-define(DEF_MOD_TURBULENCE, 12.0).
-define(DEF_MOD_SHARPNESS, 15.0).
-define(DEF_MOD_WOODTYPE, rings).
-define(DEF_MOD_SHAPE, "sin").
-define(DEF_MOD_CELLTYPE, intensity).
-define(DEF_MOD_CELLSHAPE, actual).
-define(DEF_MOD_CELLSIZE, 4.0).
-define(DEF_MOD_INTENSITY, 1.0).
-define(DEF_MOD_CELL_WEIGHT1, 1.0).
-define(DEF_MOD_CELL_WEIGHT2, 0.0).
-define(DEF_MOD_CELL_WEIGHT3, 0.0).
-define(DEF_MOD_CELL_WEIGHT4, 0.0).
-define(DEF_MOD_MUSGRAVE_TYPE, multifractal).

-define(DEF_MOD_MUSGRAVE_NOISESIZE, 0.5).
-define(DEF_MOD_MUSGRAVE_INTENSITY, 2.0).
-define(DEF_MOD_MUSGRAVE_CONTRAST, 0.1).
-define(DEF_MOD_MUSGRAVE_LACUNARITY, 2.0).
-define(DEF_MOD_MUSGRAVE_OCTAVES, 8.0).
-define(DEF_MOD_DISTORTION_TYPE, blender).

-define(DEF_MOD_DISTORTION_INTENSITY, 10.0).
-define(DEF_MOD_DISTORTION_NOISESIZE, 1.0).
-define(DEF_MOD_ALPHA_INTENSITY, off).
-define(DEF_SHADER_TYPE, diffuse).

range(T) -> {range,range_1(T)}.

%% generic float ranges
range_1(zero_one)               -> {0.0,1.0};
range_1(zero_five)              -> {0.0,5.0};
range_1(zero_to_eight)          -> {0.0,8.0};
range_1(zero_eight)             -> {0.0,8.0};
range_1(zero_six)               -> {0.0,6.0};
range_1(zero_to_ten)            -> {0.0,10.0};
range_1(zero_ten)               -> {0.0,10.0};
range_1(zero_to_twenty)         -> {0.0,20.0};
range_1(one_twenty)             -> {1.0,20.0};
range_1(zero_twenty)            -> {0.0,20.0};
range_1(neg_one_to_one)         -> {-1.0,1.0};
range_1(neg_two_two)            -> {-2.0,2.0};
range_1(one_ten)                -> {1.0,10.0};
range_1(sss_phase)              -> {-0.99,0.99};

% integer type
range_1(izero_to_one)            -> {0,1};
range_1(izero_to_five)           -> {0,5};
range_1(izero_to_ten)            -> {0,10};
range_1(izero_to_twenty)         -> {0,20};

%% Material ranges
range_1(volume_sigma_a)         -> {0.0,1.0};
range_1(volume_sigma_s)         -> {0.0,1.0};
range_1(volume_height)          -> {0.0,1000.0};
range_1(volume_steepness)       -> {0.0,10.0};
range_1(volume_attgridscale)    -> {1,5};
range_1(volume_sharpness)       -> {1.0,100.0};
range_1(volume_cover)           -> {0.0,1.0};
range_1(volume_density)         -> {0.0,1.0};
range_1(volume_region)          -> {0.5,1000.0};
range_1(portal_power)           -> {0.0,10000.0};
range_1(portal_samples)         -> {0,512};
range_1(meshlight_power)        -> {0.0,10000.0};
range_1(meshlight_samples)      -> {0,512};
range_1(autosmooth_angle)       -> {0.0,181.0};
range_1(ior)                    -> {0.0,3.0};
range_1(min_refle)              -> {0.0,1.0};
range_1(size)                   -> {0.0,1000.0}; 
range_1(modulation)             -> {-5.0,5.0};
range_1(mod_colorfactor)        -> {-1.0,1.0};
range_1(turbulence)             -> {0.0001,1024.0};
range_1(scale)                  -> {0.0001,1024.0};
range_1(cell_size)              -> {0.0,1024.0};
range_1(intensity)              -> {0.010,1024.0};
range_1(musgrave_noisesize)     -> {0.05,2.0};
range_1(musgrave_intensity)     -> {0.0,10.0};
range_1(musgrave_contrast)      -> {0.0,10.0};
range_1(musgrave_lacunarity)    -> {0.0,6.0};
range_1(musgrave_octaves)       -> {0.0,8.0};
range_1(distortion_intensity)   -> {0.0,10.0};
range_1(distortion_noisesize)   -> {0.0001,2.0};
range_1(sharpness)              -> {1.0,1024.0};
range_1(noise_depth)            -> {0,1024};
range_1(noise_size)             -> {0.0,1024.0};
range_1(absorption_dist)        -> {0.1,1024.0};
range_1(dispersion_power)       -> {0.0,1.0};
range_1(dispersion_samples)     -> {1,512};
range_1(transparency)           -> {0.0,1.0};
range_1(transmit_filter)        -> {0.0,1.0};
range_1(translucency)           -> {0.0,1.0};
range_1(sss_translucency)       -> {0.0,1.0};
range_1(sigmas_factor)          -> {1.0,10.0};
range_1(diffuse_reflect)        -> {0.0,1.0};
range_1(specular_reflect)       -> {0.0,1.0};
range_1(glossy_reflect)         -> {0.0,1.0};
range_1(emit)                   -> {0.0,25.0};
range_1(exponent)               -> {1.0,2000.0};
range_1(roughness)              -> {0.0,1.0};
range_1(lightmat_power)         -> {0.0,10.0};
range_1(blend_value)            -> {0.0,1.0};
range_1(sigma)                  -> {0.0,1.0};

%% Light ranges
range_1(power)                  -> {0.0,1024.0};
range_1(bias)                   -> {0.0,1.0};
range_1(verbosity_level)        -> {0,3};
range_1(res)                    -> {0,1024};
range_1(radius)                 -> {0,1024};
range_1(blur)                   -> {0.0,1.0};
range_1(samples)                -> {1,512};
range_1(spot_ies_samples)       -> {1,512};
range_1(photons)                -> {0,10000000};
range_1(depth)                  -> {0,12};
range_1(fixedradius)            -> {0.0,1024.0};
range_1(search)                 -> {0,1024};
range_1(cluster)                -> {0.0,1024.0};
range_1(turbidity)              -> {0.0,5.0};
range_1(angle_threshold)        -> {0.0,1.0};
range_1(raydepth)               -> {1,12};
range_1(shadow_depth)           -> {1,64};
range_1(shadow_threshold)       -> {0.0,1024.0};
range_1(exposure_adjust)        -> {0.0,50.0};
range_1(psamples)               -> {0,1024};
range_1(arealight_radius)       -> {0.0,1024.0};
range_1(maxdistance)            -> {0.0,1024.0};
range_1(infinite_radius)        -> {0.0,1024.0};
range_1(sun_angle)              -> {0.0,80.0};

%backg
range_1(ibl_rotation)           -> {-180.0,180.0};
%range_1(sky_background_power)   -> {0.0,1024.0};
range_1(background_samples)     -> {0,512};
range_1(altitude)               -> {0.0,1024.0};
range_1(sun_power)              -> {0.0,1024.0};

%% Render ranges
range_1(pm_diffuse_photons)     -> {1,100000000};
range_1(pm_bounces)             -> {0,50};
range_1(pm_search)              -> {1,10000};
range_1(pm_diffuse_radius)      -> {0.0,100.0};
range_1(pm_caustic_photons)     -> {1,100000000};
range_1(pm_caustic_radius)      -> {0.0,100.0};
range_1(pm_caustic_mix)         -> {1,10000};
range_1(pm_fg_bounces)          -> {1,20};
range_1(pm_fg_samples)          -> {1,1024};
range_1(pt_diffuse_photons)     -> {1,100000000};
range_1(pt_bounces)             -> {0,50};
range_1(pt_caustic_radius)      -> {0.0,100.0};
range_1(pt_caustic_mix)         -> {1,10000};
range_1(pt_caustic_depth)       -> {0,1024};
range_1(pt_samples)             -> {1,4096};
range_1(sppm_photons)           -> {1,100000000};
range_1(sppm_bounces)           -> {0,50};
range_1(sppm_search)            -> {1,10000};
range_1(sppm_radius)            -> {0.0,100.0};
range_1(sppm_times)             -> {0.0,20.0};
range_1(sppm_passes)            -> {0,1024};
range_1(sss_photons)            -> {0,10000000};
range_1(sss_depth)              -> {1.0,50.0};
range_1(sss_scale)              -> {0.0,100.0};
range_1(scatter_samples)        -> {0,128};
range_1(caustic_photons)        -> {0,10000000};
range_1(caustic_depth)          -> {0,512};
range_1(caustic_mix)            -> {0,512};
range_1(caustic_radius)         -> {0.0,1.0};
range_1(ao_distance)            -> {1.0,100.0};
range_1(ao_samples)             -> {1.0,128.0};
range_1(volintegr_stepsize)     -> {0.0,100.0};
range_1(subdivisions)           -> {0,10};
range_1(threads_number)         -> {1,100};
range_1(aa_pixelwidth)          -> {1.0,2.0};
range_1(aa_passes)              -> {0,1024};
range_1(aa_threshold)           -> {0.0,1.0};
range_1(aa_samples)             -> {1,1024};
range_1(gamma)                  -> {0.0,3.0};
range_1(exposure)               -> {0.0,1024.0};
range_1(pixels)                 -> {1,1024};
range_1(lens_scale)             -> {0.0,100.0};
range_1(lens_max_angle)         -> {0.0,360.0};
range_1(lens_angle)             -> {0.0,360.0};
range_1(aperture)               -> {0.0,360.0};
range_1(bokeh_rotation)         -> {-180.0,180.0};
range_1(dof_distance)           -> {0.0,250.0}.

%% used to fix old data that now can be out of range and crash Wings3d
%fit_range(Value,Id) ->
%    {Low,High}=range_1(Id),
%    if Value < Low -> Low;
%        true ->
%            if Value > High -> High;
%                true -> Value
%            end
%    end.
