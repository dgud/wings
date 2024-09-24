%%
%%  wpc_yafaray.erl
%%
%%     YafaRay Plugin User Interface, for YafaRay Core v3.1.0
%%
%%  Copyright (c) 2003-2008 Raimo Niskanen
%%                2013-2015 Code Conversion from Yafray to YafaRay by Bernard Oortman (Wings3d user oort)
%%                2015 Micheus (porting to use wx dialogs)
%%                2016 David Bluecame (adaptation for YafaRay Core v3.1.0)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_yafaray).
-export([init/0,menu/2,dialog/2,has_dialog/1,command/2]).

%% Debug exports
%% -export([now_diff_1/1]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keydelete/3,
                foreach/2,foldl/3,foldr/3]).
-compile({no_auto_import,[max/2]}).

-define(TAG, yafaray).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, yafaray_render).
-define(LOCAL_MODULE, ?MODULE).

key(Key) -> {key,?KEY(Key)}.

-define(NONZERO, 1.0e-10).

%%% Default values
-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "yafaray-xml").
-define(DEF_OPTIONS, "").
-define(DEF_RENDER_PASSES_ENABLE, false).
-define(DEF_RENDER_PASSES, 10).
-define(DEF_COMPUTER_NODE, 0).
-define(DEF_THREADS_AUTO, true).
-define(DEF_THREADS_NUMBER, 1).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_KEEP_XML, false).
-define(DEF_SAVE_ALPHA, true).
-define(DEF_GAMMA, 2.2).
-define(DEF_RENDER_FORMAT, png).
-define(DEF_EXR_FLAG_COMPRESSION, compression_zip).
-define(DEF_TILE_SIZE, 32).
-define(DEF_TILE_ORDER, centre).
-define(DEF_IMG_AUTOSAVE_TYPE, none).
-define(DEF_IMG_AUTOSAVE_PASSES, 1).
-define(DEF_IMG_AUTOSAVE_SECONDS, 300.0).
-define(DEF_IMG_DENOISE_ENABLE, false).
-define(DEF_IMG_DENOISE_MIX, 0.8).
-define(DEF_IMG_DENOISE_H_LUM, 5).
-define(DEF_IMG_DENOISE_H_CHROM, 5).
-define(DEF_FILM_PROCESSING, none).
-define(DEF_FILM_BINARY_FORMAT, true).
-define(DEF_FILM_AUTOSAVE_TYPE, none).
-define(DEF_FILM_AUTOSAVE_PASSES, 1).
-define(DEF_FILM_AUTOSAVE_SECONDS, 300.0).

%% Shader
-define(DEF_SHADER_TYPE, shinydiffuse).
-define(DEF_TIR, false).
-define(DEF_IOR, 1.4).
-define(DEF_MIN_REFLE, 0.0).
-define(DEF_OBJECT_TYPE, mesh).
-define(DEF_VISIBILITY, normal).
-define(DEF_VOLUME_TYPE, uniformvolume).
-define(DEF_VOLUME_SIGMA_A, 0.4).
-define(DEF_VOLUME_SIGMA_S, 0.05).
-define(DEF_VOLUME_HEIGHT, 0.5).
-define(DEF_VOLUME_STEEPNESS, 0.2).
-define(DEF_VOLUME_ATTGRIDSCALE, 3).
-define(DEF_VOLUME_SHARPNESS, 2.0).
-define(DEF_VOLUME_COVER, 0.05).
-define(DEF_VOLUME_DENSITY, 1.0).
-define(DEF_VOLUME_MINMAX_X, 2.0).
-define(DEF_VOLUME_MINMAX_Y, 2.0).
-define(DEF_VOLUME_MINMAX_Z, 2.0).
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
-define(DEF_AUTOSMOOTH, true).
-define(DEF_AUTOSMOOTH_ANGLE, 60.0).
-define(DEF_ABSORPTION_DIST, 3.0).
-define(DEF_DISPERSION_POWER, 0.0).
-define(DEF_FAKE_SHADOWS, false).
-define(DEF_TRANSPARENCY, 0.0).
-define(DEF_TRANSMIT_FILTER, 0.5).
-define(DEF_TRANSLUCENCY, 0.0).
-define(DEF_SIGMAS_FACTOR, 1.0).
-define(DEF_DIFFUSE_REFLECT, 0.2).
-define(DEF_SPECULAR_REFLECT, 0.0).
-define(DEF_GLOSSY_REFLECT, 0.01).
-define(DEF_EMIT, 0.0).
-define(DEF_EXPONENT, 25.0).
-define(DEF_ANISOTROPIC, false).
-define(DEF_ANISOTROPIC_U, 5.0).
-define(DEF_ANISOTROPIC_V, 1500.0).
-define(DEF_ROUGHNESS, 0.2).
-define(DEF_LIGHTMAT_POWER, 0.9).
-define(DEF_BLEND_MAT1, "New Material").
-define(DEF_BLEND_MAT2, "New Material 2").
-define(DEF_BLEND_VALUE, 0.5).
-define(DEF_OREN_NAYAR, false).
-define(DEF_OREN_NAYAR_SIGMA, 0.25).

%% Arealight
-define(DEF_AREALIGHT, false).
-define(DEF_AREALIGHT_SAMPLES, 50).
-define(DEF_AREALIGHT_PSAMPLES, 0).
-define(DEF_DUMMY, false).
-define(DEF_QMC_METHOD, 0).
-define(DEF_AREALIGHT_RADIUS, 1.0).

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
-define(DEF_USE_CAUSTICS, false).
-define(DEF_CAUSTIC_PHOTONS, 900000).
-define(DEF_CAUSTIC_DEPTH, 10).
-define(DEF_CAUSTIC_MIX, 200).
-define(DEF_CAUSTIC_RADIUS, 0.5).
-define(DEF_DO_AO, false).
-define(DEF_AO_DISTANCE, 5.0).
-define(DEF_AO_SAMPLES, 32.0).
-define(DEF_AO_COLOR, {1.0,1.0,1.0}).
-define(DEF_TRANSPARENT_SHADOWS, false).
-define(DEF_BACKGROUND_TRANSP, false).
-define(DEF_BACKGROUND_TRANSP_REFRACT, false).
-define(DEF_SHADOW_DEPTH, 2).
-define(DEF_RAYDEPTH, 4).
-define(DEF_SHADOW_BIAS_AUTO, true).
-define(DEF_SHADOW_BIAS, 0.0005).
-define(DEF_RAY_MINDIST_AUTO, true).
-define(DEF_RAY_MINDIST, 0.00005).
-define(DEF_WIDTH, 640).
-define(DEF_HEIGHT, 480).
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
-define(DEF_ATTN_POWER, 30.0).
-define(DEF_POINT_TYPE, pointlight).
-define(DEF_CAST_SHADOWS, true).
-define(DEF_USE_QMC, false).
-define(DEF_GLOW_INTENSITY, 0.0).
-define(DEF_GLOW_OFFSET, 0.0).
-define(DEF_GLOW_TYPE, 0).

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
-define(DEF_BACKGROUND_INFINITE, undefined).
-define(DEF_BACKGROUND_AMBIENT, constant).
-define(DEF_BACKGROUND_COLOR, {0.0,0.0,0.0}).
-define(DEF_CONSTANT_BACK_POWER, 1.0).
-define(DEF_HORIZON_COLOR, {1.0,1.0,1.0}).
-define(DEF_ZENITH_COLOR, {0.4,0.5,1.0}).
-define(DEF_GRADIENT_BACK_POWER, 1.0).
-define(DEF_TURBIDITY, 3.0).
-define(DEF_SUNSKY_VAR, 4.0).
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
-define(DEF_SUN_REAL_POWER, 50.0).

%% Infinite Light
-define(DEF_INFINITE_TYPE, sunlight).
-define(DEF_INFINITE_TRUE, true).
-define(DEF_INFINITE_RADIUS, 1.0).

%% Hemilight and Pathlight
-define(DEF_AMBIENT_TYPE, hemilight).
-define(DEF_USE_MAXDISTANCE, false).
-define(DEF_MAXDISTANCE, 1.0).
-define(DEF_BACKGROUND_FILENAME, "").
-define(DEF_BACKGROUND_EXPOSURE_ADJUST, 1.0).
-define(DEF_BACKGROUND_MAPPING, probe).
-define(DEF_BACKGROUND_POWER, 5.0).
-define(DEF_BACKGROUND_PREFILTER, true).
-define(DEF_BACKGROUND_ENLIGHT, false).
-define(DEF_AMBIENT_DIFFUSEPHOTONS, false).
-define(DEF_AMBIENT_CAUSTICPHOTONS, false).
-define(DEF_BACKGROUND_ROTATION, 0.0).
-define(DEF_SAMPLES, 128).
-define(DEF_SMARTIBL_BLUR, 0.0).

%% Pathlight
-define(DEF_PATHLIGHT_MODE, undefined).
-define(DEF_CACHE, false).
-define(DEF_CACHE_SIZE, 0.01).
-define(DEF_ANGLE_THRESHOLD, 0.2).
-define(DEF_SHADOW_THRESHOLD, 0.3).
-define(DEF_GRADIENT, false).
-define(DEF_SHOW_SAMPLES, false).

%% Global Photonlight
-define(DEF_GLOBALPHOTONLIGHT_PHOTONS, 50000).
-define(DEF_GLOBALPHOTONLIGHT_RADIUS, 1.0).
-define(DEF_GLOBALPHOTONLIGHT_DEPTH, 2).
-define(DEF_GLOBALPHOTONLIGHT_SEARCH, 200).

%% Modulator
-define(MAX_MODULATORS, 5).
-define(DEF_MOD_ENABLED, true).
-define(DEF_MOD_MODE, mix).
-define(DEF_MOD_SIZE, 1.0).
-define(DEF_MOD_SIZE_X, 1.0).
-define(DEF_MOD_SIZE_Y, 1.0).
-define(DEF_MOD_SIZE_Z, 1.0).
-define(DEF_MOD_OPACITY, 1.0).
-define(DEF_MOD_DIFFUSE, 1.0).
%%-define(DEF_MOD_SPECULAR, 0.0).
%%-define(DEF_MOD_AMBIENT, 0.0).
-define(DEF_MOD_SHININESS, 1.0).
-define(DEF_MOD_NORMAL, 0.0).
-define(DEF_MOD_TYPE, image).
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
-define(DEF_TEXTURE_TYPE, diffusetexture).

%% Render Passes
-define(DEF_RENDER_PASS, disabled).
-define(DEF_PASS_MASK_MAT_INDEX, 0).
-define(DEF_PASS_MASK_INVERT, false).
-define(DEF_PASS_MASK_ONLY, false).

%% Logging and Parameters Badge
-define(DEF_LOG_SAVE_TXT, false).
-define(DEF_LOG_SAVE_HTML, false).
-define(DEF_LOG_VERBOSITY_CONSOLE, info).
-define(DEF_LOG_VERBOSITY_TXT_HTML, info).
-define(DEF_BADGE_POSITION, none).
-define(DEF_BADGE_TITLE, "").
-define(DEF_BADGE_AUTHOR, "").
-define(DEF_BADGE_CONTACT, "").
-define(DEF_BADGE_COMMENTS, "").
-define(DEF_BADGE_CUSTOM_ICON_PATH, "").
-define(DEF_BADGE_DRAW_RENDER_SETTINGS, true).
-define(DEF_BADGE_DRAW_AA_NOISE_SETTINGS, true).
-define(DEF_BADGE_FONT_PATH, "").
-define(DEF_BADGE_FONT_SIZE_FACTOR, 1.0).

%% AA and Noise Control Parameters
-define(DEF_AA_PASSES, 3).
-define(DEF_AA_MINSAMPLES, 1).
-define(DEF_AA_INCSAMPLES, 1).
-define(DEF_AA_PIXELWIDTH, 1.5).
-define(DEF_AA_THRESHOLD, 0.05).
-define(DEF_AA_FILTER_TYPE, gauss).
-define(DEF_AA_NOISE_RESAMPLED_FLOOR, 0.0).
-define(DEF_AA_NOISE_SAMPLE_MULTIPLIER_FACTOR, 1.0).
-define(DEF_AA_NOISE_LIGHT_SAMPLE_MULTIPLIER_FACTOR, 1.0).
-define(DEF_AA_NOISE_INDIRECT_SAMPLE_MULTIPLIER_FACTOR, 1.0).
-define(DEF_AA_NOISE_DETECT_COLOR_NOISE, false).
-define(DEF_AA_NOISE_DARK_DETECTION_TYPE, none).
-define(DEF_AA_NOISE_DARK_THRESHOLD_FACTOR, 0.9).
-define(DEF_AA_NOISE_VARIANCE_EDGE_SIZE, 10).
-define(DEF_AA_NOISE_VARIANCE_PIXELS, 0).
-define(DEF_AA_NOISE_CLAMP_SAMPLES, 0.0).
-define(DEF_AA_NOISE_CLAMP_INDIRECT, 0.0).


range(T) -> {range,range_1(T)}.

%% Material ranges
range_1(volume_sigma_a)         -> {0.0,1.0};
range_1(volume_sigma_s)         -> {0.0,1.0};
range_1(volume_height)          -> {0.0,1000.0};
range_1(volume_steepness)       -> {0.0,10.0};
range_1(volume_attgridscale)    -> {1,5};
range_1(volume_sharpness)       -> {1.0,100.0};
range_1(volume_cover)           -> {0.0,1.0};
range_1(volume_density)         -> {0.0,1.0};
range_1(volume_minmax_x)        -> {1.0,1000.0};
range_1(volume_minmax_y)        -> {1.0,1000.0};
range_1(volume_minmax_z)        -> {1.0,1000.0};
range_1(lightportal_power)	-> {0.0,10000.0};
range_1(lightportal_samples)	-> {0,512};
range_1(meshlight_power)        -> {0.0,10000.0};
range_1(meshlight_samples)      -> {0,512};
range_1(autosmooth_angle)       -> {0.0,180.0};
range_1(ior)                    -> {0.0,3.0};
range_1(min_refle)              -> {0.0,1.0};
range_1(size)                   -> {0.0,infinity};
range_1(modulation)             -> {-5.0,5.0};
range_1(turbulence)             -> {?NONZERO,infinity};
range_1(scale)                  -> {?NONZERO,infinity};
range_1(cell_size)		-> {0.0,infinity};
range_1(intensity)		-> {0.010,infinity};
range_1(cell_weight1)		-> {-2.0,2.0};
range_1(cell_weight2)		-> {-2.0,2.0};
range_1(cell_weight3)		-> {-2.0,2.0};
range_1(cell_weight4)		-> {-2.0,2.0};
range_1(musgrave_noisesize)	-> {0.0,infinity};
range_1(musgrave_intensity)	-> {0.0,10.0};
range_1(musgrave_contrast)	-> {0.0,10.0};
range_1(musgrave_lacunarity)	-> {0.0,10.0};
range_1(musgrave_octaves)	-> {0.0,8.0};
range_1(distortion_intensity)	-> {0.0,10.0};
range_1(distortion_noisesize)	-> {0.0,infinity};
range_1(sharpness)		-> {1.0,infinity};
range_1(noise_depth)		-> {0,infinity};
range_1(noise_size)		-> {0.0,infinity};
range_1(absorption_dist)	-> {0.1,100.0};
range_1(dispersion_power)	-> {0.0,1.0};
range_1(transparency)           -> {0.0,1.0};
range_1(transmit_filter)        -> {0.0,1.0};
range_1(translucency)           -> {0.0,1.0};
range_1(sigmas_factor)          -> {1.0,10.0};
range_1(diffuse_reflect)        -> {0.0,1.0};
range_1(specular_reflect)       -> {0.0,1.0};
range_1(glossy_reflect)         -> {0.0,1.0};
range_1(emit)                   -> {0.0,25.0};
range_1(exponent)               -> {1.0,2000.0};
range_1(anisotropic_u)		-> {1.0,2000.0};
range_1(anisotropic_v)		-> {1.0,2000.0};
range_1(roughness)		-> {0.0,1.0};
range_1(lightmat_power)		-> {0.0,10.0};
range_1(blend_value)		-> {0.0,1.0};
range_1(oren_nayar_sigma)	-> {0.0,1.0};

%% Light ranges
range_1(power)                  -> {0.0,infinity};
range_1(shadow_bias)            -> {0.0,1.0};
range_1(ray_mindist)            -> {0.0,1.0};
range_1(res)                    -> {0,infinity};
range_1(radius)                 -> {0,infinity};
range_1(samples)                -> {1,infinity};
range_1(spot_ies_samples)       -> {1,512};
range_1(glow_intensity)         -> {0.0,1.0};
range_1(glow_offset)            -> {0.0,infinity};
range_1(spot_blend)		-> {0.0,1.0};
range_1(spot_fuzzyness)		-> {0.0,1.0};
range_1(photons)                -> {0,infinity};
range_1(depth)                  -> {0,infinity};
range_1(fixedradius)            -> {0.0,infinity};
range_1(search)                 -> {0,infinity};
range_1(cluster)                -> {0.0,infinity};
range_1(turbidity)              -> {0.0,infinity};
range_1(angle_threshold)        -> {0.0,1.0};
range_1(raydepth)               -> {1,infinity};
range_1(shadow_depth)           -> {1,64};
range_1(cache_size)             -> {0.0,infinity};
range_1(shadow_threshold)       -> {0.0,infinity};
range_1(cache_search)           -> {3,infinity};
range_1(exposure_adjust)        -> {0.0,50.0};
range_1(psamples)               -> {0,infinity};
range_1(arealight_radius)       -> {0.0,infinity};
range_1(maxdistance)            -> {0.0,infinity};
range_1(infinite_radius)        -> {0.0,infinity};
range_1(sun_samples)            -> {0,infinity};
range_1(sun_angle)              -> {0.0,80.0};
range_1(background_rotation)	-> {0.0,360.0};
range_1(sky_background_power)   -> {0.0,infinity};
range_1(sky_background_samples) -> {0,infinity};
range_1(darksky_altitude)	-> {0.0,infinity};
range_1(sun_real_power)		-> {0.0,infinity};
range_1(smartibl_blur)	-> {0.0,0.75};

%% Render ranges
range_1(tile_size)              -> {1,256};
range_1(pm_diffuse_photons)     -> {1,100000000};
range_1(pm_bounces)             -> {0,50};
range_1(pm_search)              -> {1,10000};
range_1(pm_diffuse_radius)      -> {0.0,100.0};
range_1(pm_caustic_photons)     -> {1,100000000};
range_1(pm_caustic_radius)      -> {0.0,100.0};
range_1(pm_caustic_mix)         -> {1,10000};
range_1(pm_fg_bounces)          -> {1,20};
range_1(pm_fg_samples)          -> {1,4096};
range_1(pt_diffuse_photons)     -> {1,100000000};
range_1(pt_bounces)             -> {0,50};
range_1(pt_caustic_radius)      -> {0.0,100.0};
range_1(pt_caustic_mix)         -> {1,10000};
range_1(pt_caustic_depth)       -> {0,infinity};
range_1(pt_samples)             -> {1,4096};
range_1(sppm_photons)		-> {1,100000000};
range_1(sppm_bounces)		-> {0,50};
range_1(sppm_search)		-> {1,10000};
range_1(sppm_radius)		-> {0.0,100.0};
range_1(sppm_times)		-> {0.0,20.0};
range_1(sppm_passes)		-> {0,infinity};
range_1(caustic_photons)        -> {0,infinity};
range_1(caustic_depth)          -> {0,infinity};
range_1(caustic_mix)            -> {0,infinity};
range_1(caustic_radius)         -> {0.0,1.0};
range_1(ao_distance)            -> {1.0,100.0};
range_1(ao_samples)             -> {1.0,128.0};
range_1(volintegr_stepsize)     -> {0.0,100.0};
range_1(subdivisions)		-> {0,10};
range_1(threads_number)         -> {1,100};
range_1(gamma)                  -> {0.0,infinity};
range_1(pixels)                 -> {1,infinity};
range_1(lens_ortho_scale)       -> {0.0,100.0};
range_1(lens_angular_max_angle) -> {0.0,360.0};
range_1(lens_angular_angle)     -> {0.0,360.0};
range_1(aperture)               -> {0.0,infinity};
range_1(bokeh_rotation)         -> {-180.0,180.0};
range_1(dof_distance)           -> {0.0,250.0};
range_1(pass_mask_mat_index)    -> {0,infinity};
range_1(img_autosave_passes)    -> {1,infinity};
range_1(img_autosave_seconds)   -> {3.0,infinity};
range_1(img_denoise_mix)        -> {0.0,1.0};
range_1(img_denoise_h_lum)      -> {2,40};
range_1(img_denoise_h_chrom)    -> {2,40};
range_1(film_autosave_passes)   -> {1,infinity};
range_1(film_autosave_seconds)  -> {3.0,infinity};
range_1(render_passes)          -> {1,32};
range_1(computer_node)          -> {0,1000};

%% AA and Noise Control Parameters
range_1(aa_pixelwidth)          -> {1.0,2.0};
range_1(aa_passes)              -> {0,infinity};
range_1(aa_threshold)           -> {0.0,1.0};
range_1(aa_minsamples)          -> {1,infinity};
range_1(aa_incsamples)          -> {1,infinity};
range_1(aa_noise_resampled_floor) -> {0.0,100.0};
range_1(aa_noise_sample_multiplier_factor) -> {0.0,4.0};
range_1(aa_noise_light_sample_multiplier_factor) -> {0.0,4.0};
range_1(aa_noise_indirect_sample_multiplier_factor) -> {0.0,4.0};
range_1(aa_noise_dark_threshold_factor) -> {0.0,1.0};
range_1(aa_noise_variance_edge_size) -> {4,20};
range_1(aa_noise_variance_pixels) -> {0,10};
range_1(aa_noise_clamp_samples) -> {0.0,infinity};
range_1(aa_noise_clamp_indirect) -> {0.0,infinity};

%% Log/Badge Parameters
range_1(badge_font_size_factor) -> {0.0,4.0}.

%% used to fix old data that now can be out of range and crash Wings3d
fit_range(Value,Id) ->
    {Low,High}=range_1(Id),
    if Value < Low -> Low;
        true ->
            if Value > High -> High;
                true -> Value
            end
    end.


%% Exported plugin callback functions
%%

init() ->
    ets:new(?LOCAL_MODULE, [named_table,public,ordered_set]),
    init_pref(),
    set_var(rendering, false),
    true.

menu({file,export}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,export_selected}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,render}, Menu) ->
    maybe_append(render, Menu, menu_entry(render));
menu({edit,plugin_preferences}, Menu) ->
    Menu++menu_entry(pref);
menu(_, Menu) ->
    Menu.

command({file,{Op,?TAG}}, St) ->
    command_file(Op, St);
command({file,{Op,{?TAG,A}}}, St) ->
    command_file(Op, A, St);
command({edit,{plugin_preferences,?TAG}}, St) ->
    pref_dialog(St);
command(_Spec, _St) ->
    %% erlang:display({?MODULE,?LINE,_Spec}),
    next.

%%% checking for Material / Light Dialogs support
has_dialog(Kind) when is_atom(Kind) ->
    case get_var(dialogs) of
        false-> false;
        _ ->
            %% these are the dialogs handled by the plugin
            Handled = [material_editor_setup, material_editor_result,
                       light_editor_setup, light_editor_result],
            case lists:member(Kind,Handled) of
                true -> {?__(1,"YafaRay"),?TAG};
                false -> false
            end
    end;
has_dialog(_) -> false.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    case is_plugin_active(edit) of
        false -> Dialog;
        _ -> maybe_append(edit, Dialog, material_dialog(Name, Mat))
    end;
dialog({material_editor_result,Name,Mat}, Res) ->
    case is_plugin_active(edit) of
        false -> {Mat,Res};
        _ -> material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    case get_var(dialogs) of
        false-> Dialog;
        _ -> Dialog ++ [{?__(1,"YafaRay"), light_dialog(Name, Ps)}]
    end;
dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
        false -> {Ps0,Res};
        _ -> light_result(Name, Ps0, Res)
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.

%%
%% End of exported plugin callback functions


init_pref() ->
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    RendererPath =
        case filename:pathtype(Renderer) of
            absolute ->
                case filelib:is_file(Renderer)of
                    false -> false;
                    _ -> Renderer
                end;
            _ ->
                case wings_job:find_executable(Renderer) of
                    false -> false;
                    Path -> Path
                end
        end,
    case get_pref(dialogs, ?DEF_DIALOGS) of
        auto ->
            set_var(renderer, RendererPath),
            set_var(dialogs, case RendererPath of
                                 false -> false;
                                 _ -> true
                             end);
        enabled ->
            set_var(renderer, RendererPath),
            set_var(dialogs, true);
        disabled ->
            set_var(renderer, false),
            set_var(dialogs, false)
    end,
    ok.

maybe_append(Condition, Menu, PluginMenu) ->
    case {is_plugin_active(Condition),Menu} of
        {_,[plugin_manager_category]} -> Menu++PluginMenu;
        {false,_} -> Menu;
        {_,_} -> Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
        export -> get_var(dialogs);
        edit -> get_var(dialogs);
        render -> get_var(renderer)
    end.

menu_entry(export) ->
    [{?__(1,"YafaRay")++" (.xml)...",?TAG}];
menu_entry(_) ->
    %% render or pref
    [{?__(1,"YafaRay")++"...",?TAG}].

command_file(render=Op, _St) ->
    case get_var(rendering) of
        false ->
            export_dialog(Op, ?__(2,"YafaRay Render Options"));
        true ->
            wpa:error_msg(?__(1,"Already rendering."))
    end;
command_file(Op, _St) ->
    export_dialog(Op, ?__(3,"YafaRay Export Options")).

command_file(render, Attr, St) when is_list(Attr) ->
    set_prefs(Attr),
    do_export(export,
              props(render, Attr),
              [{?TAG_RENDER,true}|Attr], St);
command_file(Op, Attr, St) when is_list(Attr) ->
    %% when Op =:= export; Op =:= export_selected
    set_prefs(Attr),
    do_export(Op, props(Op, Attr), Attr, St).

-record(camera_info, {pos,dir,up,fov}).

do_export(Op, Props0, Attr0, St0) ->
    SubDiv = proplists:get_value(subdivisions, Attr0, ?DEF_SUBDIVISIONS),

    Props = [{subdivisions,SubDiv}|Props0],
    [{Pos,Dir,Up},Fov] = wpa:camera_info([pos_dir_up,fov]),
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov},
    Attr = [CameraInfo,{lights,wpa:lights(St0)}|Attr0],
    ExportFun =
        fun (Filename, Contents) ->
            case catch export(Attr, Filename, Contents) of
                ok ->
                    ok;
                Error ->
                    io:format(?__(1,"ERROR: Failed to export")++":~n~p~n", [Error]),
                    {error,?__(2,"Failed to export")}
            end
        end,
    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id,wpa:vm_freeze(We)} || {Id,We} <- Shapes0],
    St = St0#st{shapes=gb_trees:from_orddict(Shapes)},
    wpa:Op(Props, ExportFun, St).

props(render, Attr) ->
    RenderFormat =
        proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    {value,{RenderFormat,Ext,Desc}} =
        lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
    Title =
        case os:type() of
            {win32,_} -> "Render";
            _Other    -> ?__(1,"Render")
        end,
    [{title,Title},{ext,Ext},{ext_desc,Desc}];
props(export, _Attr) ->
    {Title,File} =
        case os:type() of
            {win32,_} -> {"Export","YafaRay File"};
            _Other    -> {?__(2,"Export"),?__(5,"YafaRay File")}
        end,
    [{title,Title},{ext,".xml"},{ext_desc,File}];
props(export_selected, _Attr) ->
    {Title,File} =
        case os:type() of
            {win32,_} -> {"Export Selected","YafaRay File"};
            _Other    -> {?__(4,"Export Selected"),?__(5,"YafaRay File")}
        end,
    [{title,Title},{ext,".xml"},{ext_desc,File}].


%%%
%%% Dialogues and results
%%%

%%% Object Specific Material Properties
%%%
material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    DefReflected = alpha(proplists:get_value(specular, OpenGL, wings_material:specular_from_metal(OpenGL))),
    DefTransmitted = def_transmitted(proplists:get_value(diffuse, OpenGL)),
    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),
    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),
    YafaRay = proplists:get_value(?TAG, Mat, []),
    DefShaderType = get_pref(shader_type, YafaRay),
    ShaderType = proplists:get_value(shader_type, YafaRay, DefShaderType),
    Object_Type = proplists:get_value(object_type, YafaRay, ?DEF_OBJECT_TYPE),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Volume_Type = proplists:get_value(volume_type, YafaRay, ?DEF_VOLUME_TYPE),
    Volume_Sigma_a = proplists:get_value(volume_sigma_a, YafaRay, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, YafaRay, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, YafaRay, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, YafaRay, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, YafaRay, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Sharpness = proplists:get_value(volume_sharpness, YafaRay, ?DEF_VOLUME_SHARPNESS),
    Volume_Cover = proplists:get_value(volume_cover, YafaRay, ?DEF_VOLUME_COVER),
    Volume_Density = proplists:get_value(volume_density, YafaRay, ?DEF_VOLUME_DENSITY),
    Volume_Minmax_X = proplists:get_value(volume_minmax_x, YafaRay, ?DEF_VOLUME_MINMAX_X),
    Volume_Minmax_Y = proplists:get_value(volume_minmax_y, YafaRay, ?DEF_VOLUME_MINMAX_Y),
    Volume_Minmax_Z = proplists:get_value(volume_minmax_z, YafaRay, ?DEF_VOLUME_MINMAX_Z),
    Lightportal_Power = proplists:get_value(lightportal_power, YafaRay, ?DEF_LIGHTPORTAL_POWER),
    Lightportal_Samples = proplists:get_value(lightportal_samples, YafaRay, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Diffusephotons = proplists:get_value(lightportal_diffusephotons, YafaRay, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, YafaRay, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, YafaRay, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    Meshlight_Power = proplists:get_value(meshlight_power, YafaRay, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, YafaRay, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, YafaRay, ?DEF_MESHLIGHT_COLOR),
    Meshlight_Double_Sided = proplists:get_value(meshlight_double_sided, YafaRay, ?DEF_MESHLIGHT_DOUBLE_SIDED),
    TIR = proplists:get_value(tir, YafaRay, ?DEF_TIR),
    AutosmoothAngle = proplists:get_value(autosmooth_angle, YafaRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafaRay,
                                     if AutosmoothAngle == 0.0 -> false;
                                        true -> ?DEF_AUTOSMOOTH end),

    %% IOR Material Property for all Materials except Glossy
    %%
    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),

    %% Color Properties Transmitted = Diffuse and Refracted
    %% Color Properties Reflected = Glossy and Reflected
    %%
    Reflected = proplists:get_value(reflected, YafaRay, DefReflected),
    Transmitted = proplists:get_value(transmitted, YafaRay, DefTransmitted),

    %% Glass Properties
    %%
    AbsorptionColor = proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),
    AbsorptionDist = proplists:get_value(absorption_dist, YafaRay, ?DEF_ABSORPTION_DIST),
    DispersionPower = proplists:get_value(dispersion_power, YafaRay, ?DEF_DISPERSION_POWER),
    FakeShadows = proplists:get_value(fake_shadows, YafaRay, ?DEF_FAKE_SHADOWS),
    Roughness = proplists:get_value(roughness, YafaRay, ?DEF_ROUGHNESS),

    %% Shiny Diffuse Properties
    %% Transmit Filter also for Glass and Rough Glass
    Transparency = proplists:get_value(transparency, YafaRay, ?DEF_TRANSPARENCY),
    TransmitFilter = proplists:get_value(transmit_filter, YafaRay, ?DEF_TRANSMIT_FILTER),
    Translucency = proplists:get_value(translucency, YafaRay, ?DEF_TRANSLUCENCY),
    SpecularReflect = proplists:get_value(specular_reflect, YafaRay, ?DEF_SPECULAR_REFLECT),
    Emit = proplists:get_value(emit, YafaRay, ?DEF_EMIT),

    %% Shiny Diffuse, Glossy, Coated Glossy Properties
    %%
    DiffuseReflect = proplists:get_value(diffuse_reflect, YafaRay, ?DEF_DIFFUSE_REFLECT),
    OrenNayar = proplists:get_value(oren_nayar, YafaRay, ?DEF_OREN_NAYAR),
    OrenNayar_Sigma = proplists:get_value(oren_nayar_sigma, YafaRay, ?DEF_OREN_NAYAR_SIGMA),

    %% Glossy and Coated Glossy Properties
    %%
    GlossyReflect = proplists:get_value(glossy_reflect, YafaRay, ?DEF_GLOSSY_REFLECT),
    Exponent = proplists:get_value(exponent, YafaRay, ?DEF_EXPONENT),
    Anisotropic = proplists:get_value(anisotropic, YafaRay, ?DEF_ANISOTROPIC),
    Anisotropic_U = proplists:get_value(anisotropic_u, YafaRay, ?DEF_ANISOTROPIC_U),
    Anisotropic_V = proplists:get_value(anisotropic_v, YafaRay, ?DEF_ANISOTROPIC_V),

    %% Light Material Properties
    %%
    Lightmat_Color = proplists:get_value(lightmat_color, YafaRay, DefLightmatColor),
    Lightmat_Power = proplists:get_value(lightmat_power, YafaRay, ?DEF_LIGHTMAT_POWER),

    %% Blend Material Properties
    %%
    Blend_Mat1 = proplists:get_value(blend_mat1, YafaRay, ?DEF_BLEND_MAT1),
    Blend_Mat2 = proplists:get_value(blend_mat2, YafaRay, ?DEF_BLEND_MAT2),
    Blend_Value = proplists:get_value(blend_value, YafaRay, ?DEF_BLEND_VALUE),

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            ?KEY(oren_nayar) ->
                wings_dialog:enable(?KEY(pnl_sigma_shiny), Value =/= ?DEF_OREN_NAYAR, Store);
            ?KEY(autosmooth) ->
                wings_dialog:enable(?KEY(pnl_autosmooth), Value =/= ?DEF_OREN_NAYAR, Store);
            ?KEY(anisotropic) ->
                wings_dialog:enable(?KEY(pnl_exp_coated), Value =/= ?DEF_ANISOTROPIC, Store)
        end
    end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(object_type) ->
                    Value0 = wings_dialog:get_value(?KEY(volume_type),Store),
                    wings_dialog:show(?KEY(pnl_desnity_volume), Value0 =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value0 =:= noisevolume, Store),

                    wings_dialog:show(?KEY(pnl_volume), Value =:= volume, Store),
                    wings_dialog:show(?KEY(pnl_mesh_light), Value =:= meshlight, Store),
                    wings_dialog:show(?KEY(pnl_lightportal), Value =:= lightportal, Store),
                    case Value of
                        lightportal -> wings_dialog:set_value(?KEY(visibility),normal,Store);
                        _ -> ignore
                    end,
                    wings_dialog:enable(?KEY(visibility), Value =/= lightportal, Store),
                    wings_dialog:update(?KEY(pnl_obj_params), Store);
                ?KEY(volume_type) ->
                    wings_dialog:show(?KEY(pnl_desnity_volume), Value =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value =:= noisevolume, Store),
                    wings_dialog:update(?KEY(pnl_volume_type), Store);
                ?KEY(shader_type) ->
                    %% IOR
                    wings_dialog:show(?KEY(pnl_ior), not is_member(Value, [glossy,lightmat,blend_mat]), Store),
                    %% Glossy Color
                    Gc = is_member(Value, [glossy,coatedglossy,translucent]),
                    Rl = is_member(Value, [glass,rough_glass]),
                    Rc = Value =:= shinydiffuse,
                    wings_dialog:show(?KEY(pnl_gc), Gc, Store),
                    %% Reflected Light
                    wings_dialog:show(?KEY(pnl_rl), Rl, Store),
                    %% Reflected Color
                    wings_dialog:show(?KEY(pnl_rc), Rc, Store),
                    wings_dialog:show(?KEY(pnl_rf), is_member(Value, [shinydiffuse,glass,rough_glass,glossy,coatedglossy,translucent]), Store),
                    %% Diffuse Color
                    wings_dialog:show(?KEY(pnl_dc_l), is_member(Value, [shinydiffuse,glossy,coatedglossy,translucent]), Store),
                    %% Filtered Light
                    wings_dialog:show(?KEY(pnl_fl_l), is_member(Value, [glass,rough_glass]), Store),
                    wings_dialog:show(?KEY(pnl_fl), is_member(Value, [shinydiffuse,glass,rough_glass,glossy,coatedglossy,translucent]), Store),
                    %% Transparency
                    wings_dialog:show(?KEY(pnl_transp), Value =:= shinydiffuse, Store),
                    %% Specular Color
                    wings_dialog:show(?KEY(pnl_sc), Value =:= translucent, Store),
                    %% Absorption Color & Absorption Distance
                    wings_dialog:show(?KEY(pnl_abs_reg), is_member(Value, [glass,rough_glass]), Store),
                    wings_dialog:show(?KEY(pnl_abs), is_member(Value, [glass,rough_glass,translucent]), Store),
                    %% Transmit Filter
                    wings_dialog:show(?KEY(pnl_tf), is_member(Value, [shinydiffuse,glass,rough_glass]), Store),
                    %% Translucency
                    wings_dialog:show(?KEY(pnl_transl), Value =:= shinydiffuse, Store),
                    %% Scatter Color & SigmaS Factor
                    wings_dialog:show(?KEY(pnl_sct), Value =:= translucent, Store),
                    %% Diffuse Reflection
                    wings_dialog:show(?KEY(pnl_dr), is_member(Value, [shinydiffuse,glossy,coatedglossy,translucent]), Store),
                    %% Mirror Reflection & Emit Light
                    wings_dialog:show(?KEY(pnl_mr), Value =:= shinydiffuse, Store),
                    %% Glossy Reflection & Exponent
                    wings_dialog:show(?KEY(pnl_gr), is_member(Value, [glossy,coatedglossy,translucent]), Store),
                    %% Anisotropic
                    wings_dialog:show(?KEY(pnl_an), Value =:= coatedglossy, Store),
                    %% Roughness
                    wings_dialog:show(?KEY(pnl_rg), Value =:= rough_glass, Store),
                    %% Dispersion Power & Dispersion Samples
                    wings_dialog:show(?KEY(pnl_dsp), is_member(Value, [glass,rough_glass]), Store),
                    %% nayar
                    wings_dialog:show(?KEY(pnl_on), is_member(Value, [shinydiffuse,glossy,coatedglossy]), Store),
                    %% Fresnel Effect
                    wings_dialog:show(?KEY(pnl_fe), Value =:= shinydiffuse, Store),
                    %% Light Material: Color & Power
                    wings_dialog:show(?KEY(pnl_lm), Value =:= lightmat, Store),
                    %% Blend: Material 1, Material 2 & Blend Mix
                    wings_dialog:show(?KEY(pnl_bl), Value =:= blend_mat, Store),
%                    wings_dialog:update(?KEY(pnl_shader_l), Store),

                    wings_dialog:update(?KEY(pnl_shader), Store);
                _ -> ok
            end
        end,

    %% Object Specific Material Properties Dialog
    %%
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    ObjectFrame =
        {vframe, [
            {hframe, [
                {?__(6,"Autosmooth"),Autosmooth,[key(autosmooth),{hook,Hook_Enable}]},
                panel,
                {hframe, [
                    {label,?__(7,"Angle")},
                    {slider,{text,AutosmoothAngle,[range(autosmooth_angle),key(autosmooth_angle)]}},
                    help_button({material_dialog,object})
                ],[key(pnl_autosmooth),{margin,false}]}
            ]},

            %% Start Object Type Menu
            {vframe, [
                {hframe, [
                    {label, ?__(113,"Object Type")},
                    {menu,[
                        {?__(31,"Mesh"),mesh},
                        {?__(32,"Volume"),volume},
                        {?__(33,"Mesh Light"),meshlight},
                        {?__(34,"Light Portal"),lightportal}
                    ],Object_Type,[key(object_type),{hook,Hook_Show}]},
                    panel,
                    {label, ?__(108,"Visibility")},
                    {menu,[
                        {?__(109,"Normal"),normal},
                        {?__(110,"No shadows"),no_shadows},
                        {?__(111,"Shadows only"),shadow_only},
                        {?__(112,"Invisible"),invisible}
                    ],Visibility,[key(visibility)]}
                ]},
                {hframe, [
                    {hframe, [
                        {vframe, [
                            {menu,[
                                {?__(82,"Uniform"),uniformvolume},
                                {?__(83,"ExpDensity"),expdensityvolume},
                                {?__(126,"Noise"),noisevolume}
                            ],Volume_Type,[key(volume_type),{hook,Hook_Show}]},
                            panel,
                            panel
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(84,"Absorption"), {text,Volume_Sigma_a,[range(volume_sigma_a),key(volume_sigma_a)]}},
                                {?__(85,"Scatter"), {text,Volume_Sigma_s,[range(volume_sigma_s),key(volume_sigma_s)]}},
                                {?__(86,"AttgridScale"), {text,Volume_Attgridscale,[range(volume_attgridscale),key(volume_attgridscale)]}}
                            ],[{margin,false}]},
                            %% Start ExpDensity Volume - ONLY
                            {label_column, [
                                {?__(90,"Height"), {text,Volume_Height,[range(volume_height),key(volume_height)]}},
                                {?__(91,"Steepness"), {text,Volume_Steepness,[range(volume_steepness),key(volume_steepness)]}},
                                {" ", panel}
                            ], [key(pnl_desnity_volume),{show,false},{margin,false}]},
                            %% End ExpDensity Volume - ONLY

                            %% Start Noise Volume - ONLY
                            {label_column, [
                                {?__(130,"Sharpness"), {text,Volume_Sharpness,[range(volume_sharpness),key(volume_sharpness)]}},
                                {?__(131,"Cover"), {text,Volume_Cover,[range(volume_cover),key(volume_cover)]}},
                                {?__(132,"Density"), {text,Volume_Density,[range(volume_density),key(volume_density)]}}
                            ], [key(pnl_noise_volume),{margin,false}]},
                            %% End Noise Volume - ONLY
                            % FIXME: This is not really good, volume Min/Max coordinates should be automatically calculated based on the object bounds, not entered manually by the user. This is counter-intuitive.
                            {label_column, [
                                {?__(133,"Min/Max X"), {text,Volume_Minmax_X,[range(volume_minmax_x),key(volume_minmax_x)]}},
                                {?__(134,"Min/Max Y"), {text,Volume_Minmax_Y,[range(volume_minmax_y),key(volume_minmax_y)]}},
                                {?__(135,"Min/Max Z"), {text,Volume_Minmax_Z,[range(volume_minmax_z),key(volume_minmax_z)]}}
                            ],[{margin,false}]}
                        ],[key(pnl_volume_type),{margin,false}]}
                    ], [{title,"params"},key(pnl_volume),{margin,false}]},

                    {hframe, [
                        {label_column, [
                            {?__(121,"Power"), {text,Meshlight_Power,[range(meshlight_power),key(meshlight_power)]}},
                            {?__(122,"Samples"), {text,Meshlight_Samples,[range(meshlight_samples),key(meshlight_samples)]}}
                        ]},
                        {label_column, [
                            {?__(123,"Color"), {slider, {color, Meshlight_Color, [key(meshlight_color)]}}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(124,"Double Sided"),Meshlight_Double_Sided,[key(meshlight_double_sided)]},
                            panel
                        ]}
                    ], [key(pnl_mesh_light),{show,false}]},
                    {hframe, [
                        {label_column, [
                            {?__(121,"Power"), {text,Lightportal_Power,[key(lightportal_power),range(lightportal_power)]}},
                            {?__(122,"Samples"), {text,Lightportal_Samples,[key(lightportal_samples),range(lightportal_samples)]}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(142,"Diffuse Photons"),Lightportal_Diffusephotons,[key(lightportal_diffusephotons)]},
                            {?__(143,"Caustic Photons"),Lightportal_Causticphotons,[key(lightportal_causticphotons)]},
                            {?__(144,"Photon Only"),Lightportal_Photon_Only,[key(lightportal_photon_only)]}
                        ]}
                    ], [key(pnl_lightportal),{show,false}]}
                ],[{margin,false}]}
            ], [key(pnl_obj_params)]}
        %% End Object Type Menu
        ]},

    %% Shader Specific Material Properties Dialog
    %%
    ShaderFrame =
        {vframe, [
            {menu,menu_shader(),ShaderType,[key(shader_type),{hook,Hook_Show}]},

            %% label column
            {vframe, [
                %% 1st row
                {label_column, [
                    {"IOR", {slider, {text,IOR,[range(ior),key(ior)]}}}
                ],[key(pnl_ior),{margin,false}]},
                %% 3rd row
                {hframe, [
                    {hframe, [
                        {label, "Reflected Color"}
                    ],[key(pnl_rc),{show,false},{margin,false}]},
                    {hframe, [
                        {label, "Reflected Light"}
                    ],[key(pnl_rl),{show,false},{margin,false}]},
                    {hframe, [
                        {label, "Glossy Color"}
                    ],[key(pnl_gc),{margin,false}]},
                    {hframe, [
                        {slider, {color,Reflected,[key(reflected)]}}
                    ],[{margin,false}]}
                ],[key(pnl_rf)]},
                %% 4th row
                {hframe, [
                    {vframe, [
                        {label, "Diffuse Color"}
                    ],[key(pnl_dc_l),{margin,false}]},
                    {vframe, [
                        {label, "Filtered Light"}
                    ],[key(pnl_fl_l),{show,false},{margin,false}]},
                    {vframe, [
                        {slider, {color,Transmitted,[key(transmitted)]}}
                    ],[{margin,false}]}
                ],[key(pnl_fl)]},
                %% 5th row
                {label_column, [
                    {"Transparency", {slider, {text,Transparency,[range(transparency),key(transparency)]}}}
                ],[key(pnl_transp),{show,false},{margin,false}]},
                %% 7th row
                {vframe, [
                    {label_column, [
                        {"Absorption Color", {slider, {color,AbsorptionColor,[key(absorption_color)]}}}
                    ],[key(pnl_abs_reg),{margin,false}]},
                    {label_column, [
                        {"Absorption Distance", {slider, {text,AbsorptionDist,[range(absorption_dist),key(absorption_dist)]}}}
                    ],[key(pnl_abs_v),{margin,false}]}
                ],[key(pnl_abs),{margin,false}]},
                %% 8th row
                {label_column, [
                    {"Transmit Filter", {slider, {text,TransmitFilter,[range(transmit_filter),key(transmit_filter)]}}}
                ],[key(pnl_tf),{show,false},{margin,false}]},
                %% 9th row
                {label_column, [
                    {"Translucency", {slider, {text,Translucency,[range(translucency),key(translucency)]}}}
                ],[key(pnl_transl),{margin,false},{show,false}]},
                %% 11th row
                {label_column, [
                    {"Diffuse Reflection", {slider, {text,DiffuseReflect,[range(diffuse_reflect),key(diffuse_reflect)]}}}
                ],[key(pnl_dr),{margin,false}]},
                %% 12th row
                {label_column, [
                    {"Mirror Reflection", {slider, {text,SpecularReflect,[range(specular_reflect),key(specular_reflect)]}}},
                    {"Emit Light", {slider, {text,Emit,[range(emit),key(emit)]}}}
                ],[key(pnl_mr),{show,false},{margin,false}]},
                %% 13th row
                {label_column, [
                    {"Glossy Reflection", {slider, {text,GlossyReflect,[range(glossy_reflect),key(glossy_reflect)]}}},
                    {"Exponent", {slider, {text,Exponent,[range(exponent),key(exponent)]}}}
                ],[key(pnl_gr),{margin,false}]},
                %% 14th row
                {hframe, [
                    {hframe, [
                        {"Anisotropic",Anisotropic,[key(anisotropic),{hook,Hook_Enable}]}
                    ],[{margin,false}]},
                    {hframe,[
                        {label, "Exp U"},
                        {text,Anisotropic_U,[range(anisotropic_u),key(anisotropic_u) ]},
                        panel,
                        {label, "Exp V"},
                        {text,Anisotropic_V,[range(anisotropic_v),key(anisotropic_v) ]}
                    ],[key(pnl_exp_coated),{margin,false}]}
                ],[key(pnl_an),{show,false},{margin,false}]},
                %% 15th row
                {label_column, [
                    {"Roughness", {slider, {text,Roughness,[range(roughness),key(roughness)]}}}
                ],[key(pnl_rg),{show,false},{margin,false}]},
                %% 16th row
                {vframe, [
                    {label_column, [
                        {"Dispersion Power", {slider, {text,DispersionPower,[range(dispersion_power),key(dispersion_power)]}}}
                    ],[{margin,false}]},
                    {"Fake Shadows",FakeShadows,[key(fake_shadows)]}
                ],[key(pnl_dsp),{margin,false}]},
                %% 17th row
                {hframe, [
                    {"Oren-Nayar",OrenNayar,[key(oren_nayar),{hook,Hook_Enable}]},
                    {label_column,[
                        {"Sigma", {text,OrenNayar_Sigma,[range(oren_nayar_sigma),key(oren_nayar_sigma)]}}
                    ],[key(pnl_sigma_shiny),{margin,false}]}
                ],[key(pnl_on),{margin,false}]},
                %% 18th row
                {hframe, [
                    {"Fresnel Effect",TIR,[key(tir)]}
                ],[key(pnl_fe),{margin,false}]},
                %% 19th row
                {label_column, [
                    {"Color", {slider, {color,Lightmat_Color,[key(lightmat_color)]}}},
                    {"Power", {slider, {text,Lightmat_Power,[range(lightmat_power),key(lightmat_power)]}}}
                ],[key(pnl_lm),{show,false},{margin,false}]},
                %% 20th row
                {label_column, [
                    {"Material 1", {text,Blend_Mat1,[key(blend_mat1)]}},
                    {"Material 2", {text,Blend_Mat2,[key(blend_mat2)]}},
                    {"Blend Mix", {slider, {text,Blend_Value,[range(blend_value),key(blend_value)]}}}
                ],[key(pnl_bl),{show,false},{margin,false}]}

            ],[key(pnl_shader), {margin,false}]}
        ]},
    %% End of Material Dialogs

    Modulator_Frame =
        {vframe,
            modulator_dialogs(Modulators, Maps)
        },

    [{
        ?__(1,"YafaRay"),
        {vframe, [
            {oframe, [
                {"Material", ShaderFrame},
                {"Textures", Modulator_Frame},
                {?__(8,"Object Parameters"), ObjectFrame}
                ], 1, [{style, buttons}]
            }
        ]}
    }].

alpha({R,G,B,A}) -> {R*A,G*A,B*A}.

%%% Define Lightmat Color
def_lightmat_color({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.

%%% Define Absorption Color
def_absorption_color({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.

%%% Grab OpenGL Transmitted Default Button
def_transmitted({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.


def_modulators([]) ->
    [];
def_modulators([{diffuse,_}|Maps]) ->
    [{modulator,[{type,{map,diffuse}},{diffuse,1.0}]}
     |def_modulators(Maps)];
def_modulators([{ambient,_}|Maps]) ->
    [{modulator,[{type,{map,ambient}},{ambient,1.0}]}
     |def_modulators(Maps)];
def_modulators([{bump,_}|Maps]) ->
    [{modulator,[{type,{map,bump}},{normal,1.0}]}
     |def_modulators(Maps)];
def_modulators([{gloss,_}|Maps]) ->
    [{modulator,[{type,{map,gloss}},{shininess,1.0}]}
     |def_modulators(Maps)];
def_modulators([_|Maps]) ->
    def_modulators(Maps).


material_result(_Name, Mat0, Res) ->
    %% Rip out all yafaray material properties
    {Found0, Remaining} = rip_all(?TAG, Res),
    {Mod, Mat} = process_modulator(Found0),
    NewMat = [{?TAG, Mat++Mod} | lists:keydelete(?TAG, 1, Mat0)],
    {NewMat, Remaining}.

modulator_dialogs(Modulators0, Maps) ->
    ModCount = length(Modulators0),
    Modulators =
        if (ModCount < ?MAX_MODULATORS) ->
            Modulators0 ++ modulator_add(?MAX_MODULATORS-ModCount);
        true -> Modulators0
        end,
    [{oframe, modulator_dialogs(Modulators, Maps, 1), 1, [{style, buttons}]}].

modulator_dialogs([], _Maps, _M) -> [];
modulator_dialogs([Modulator|Modulators], Maps, M) ->
    modulator_dialog(Modulator, Maps, M)++
    modulator_dialogs(Modulators, Maps, M+1).

modulator_dialog({modulator,Ps}, Maps, M) when is_list(Ps) ->
    {Enabled,Mode,Type} = mod_enabled_mode_type(Ps, Maps),
    AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),
    TextureType = proplists:get_value(texture_type, Ps, ?DEF_TEXTURE_TYPE),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
%%    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    ImageFormats = images_format(),
    BrowseProps = [{dialog_type,open_dialog}, {extensions,ImageFormats}],
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    NoiseSize = proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE),
    NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    WoodType = proplists:get_value(wood_type, Ps, ?DEF_MOD_WOODTYPE),
    Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),
    CellType = proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE),
    CellShape = proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE),
    CellSize = proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE),
    Intensity = proplists:get_value(intensity, Ps, ?DEF_MOD_INTENSITY),
    CellWeight1 = proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1),
    CellWeight2 = proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2),
    CellWeight3 = proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3),
    CellWeight4 = proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4),
    MusgraveType = proplists:get_value(musgrave_type, Ps, ?DEF_MOD_MUSGRAVE_TYPE),

    MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),
    MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),
    MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, ?DEF_MOD_MUSGRAVE_CONTRAST),
    MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),
    MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),
    DistortionType = proplists:get_value(distortion_type, Ps, ?DEF_MOD_DISTORTION_TYPE),

    DistortionIntensity = proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY),
    DistortionNoiseSize = proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE),

    MapsItems = [{atom_to_list(Map),{map,Map}} || {Map,_} <- Maps],

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            {?TAG,{M,enabled}} ->
                wings_dialog:enable(?KEY({pnl_mode,M}), Value =:= true, Store),
                wings_dialog:enable(?KEY({pnl_mod,M}), Value =:= true, Store);
            _ -> ok
        end
    end,

    Hook_Show = fun(Key, Value, Store) ->
        case Key of
            {?TAG,{M,type}} ->
                wings_dialog:show(?KEY({pnl_image,M}), Value =:= image, Store),
                wings_dialog:show(?KEY({pnl_base1,M}), is_member(Value,[clouds,marble,wood,musgrave,distorted_noise]), Store),
                wings_dialog:show(?KEY({pnl_base2,M}), is_member(Value,[clouds,marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_base3,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_sharpness,M}), Value =:= marble, Store),
                wings_dialog:show(?KEY({pnl_turb,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_wood,M}), Value =:= wood, Store),
                wings_dialog:show(?KEY({pnl_voronoi,M}), Value =:= voronoi, Store),
                wings_dialog:show(?KEY({pnl_musgrave,M}), Value =:= musgrave, Store),
                wings_dialog:show(?KEY({pnl_dist_noise,M}), Value =:= distorted_noise, Store),
                wings_dialog:update(?KEY({pnl_mod,M}), Store)
        end
    end,

    ModFrame =
        {vframe, [
            {hframe, [
                {?__(5,"Enabled"),Enabled,[key({M,enabled}),{hook,Hook_Enable}]},
                panel,
                {hframe, [
                    {menu,[
                        {?__(6,"Mix"),mix},
                        {?__(7,"Add"),add},
                        {?__(8,"Multiply"),mul},
                        {?__(109,"Subtract"),sub},
                        {?__(110,"Screen"),scr},
                        {?__(111,"Divide"),divide},
                        {?__(112,"Difference"),dif},
                        {?__(113,"Darken"),dar},
                        {?__(114,"Lighten"),lig}
                    ],Mode,[key({M,mode})]},
                    panel,
                    {menu,[
                        {?__(115,"Alpha Off"),off},
                        {?__(116,"Alpha Transparency"),transparency},
                        {?__(117,"Diffuse+Alpha Transparency"),diffusealphatransparency},
                        {?__(118,"Alpha Translucency"),translucency},
                        {?__(119,"Specularity"),specularity},
                        {?__(120,"Stencil"),stencil}
                    ],AlphaIntensity,[key({M,alpha_intensity})]},
                    panel,
                    {menu,[
                        {?__(121,"Diffuse (Shiny Diffuse, Glossy)"),diffusetexture},
                        {?__(122,"Mirror Color (Shiny Diffuse, Glass)"),mirrorcolortexture},
                        {?__(123,"Mirror (Shiny Diffuse)"),mirrortexture},
                        {?__(124,"Glossy (Glossy)"),glossytexture},
                        {?__(125,"Glossy Reflect (Glossy)"),glossyreflecttexture},
                        {?__(126,"Transparency (Shiny Diffuse)"),transparencytexture},
                        {?__(127,"Translucency (Shiny Diffuse)"),translucencytexture},
                        {?__(128,"Bump (All)"),bumptexture}
                    ],TextureType,[key({M,texture_type})]}
                ],[key({pnl_mode,M}),{margin,false},{hook,Hook_Show}]}
            ]},
            {vframe, [
                {hframe,[
                    {hframe,[
                        {label,?__(10,"Size X")},{text,SizeX,[key({M,size_x}), range(size)]}
                    ]},
                    {hframe,[
                        {label,?__(11,"Y")},{text,SizeY,[key({M,size_y}), range(size)]}
                    ]},
                    {hframe,[
                        {label,?__(12,"Z")},{text,SizeZ,[key({M,size_z}), range(size)]}
                    ]}
                ],[{margin,false}]},
                {hframe,[
                    {vframe,[
                        {label,?__(13,"Color Factor")},
                        {label,?__(16,"Value Factor")},
                        {label,?__(17,"Normal")}
                    ]},
                    {vframe,[
                        {slider,{text,fit_range(Diffuse,modulation),[key({M,diffuse}), range(modulation)]}},
                        {slider,{text,fit_range(Shininess,modulation),[key({M,shininess}), range(modulation)]}},
                        {slider,{text,fit_range(Normal,modulation),[key({M,normal}), range(modulation)]}}
                    ]}
                ],[{margin,false}]},
                {menu, MapsItems++[
                    {?__(18,"Image"),image},
                    {?__(19,"Clouds"),clouds},
                    {?__(20,"Marble"),marble},
                    {?__(21,"Wood"),wood},
                    {?__(46,"Voronoi"),voronoi},
                    {?__(62,"Musgrave"),musgrave},
                    {?__(82,"Distorted Noise"),distorted_noise}
                ],Type,[key({M,type}), {hook,Hook_Show}]},
                {vframe, [
                    {hframe, [
                        {label,?__(22,"Filename")},
                        {button,{text,Filename,[key({M,filename}), {width,35},{props,BrowseProps}]}}
                    ],[key({pnl_image,M}), {show,false}]},
                    %% Clouds,Marble,Wood Specific Procedurals Line 1
                    {hframe, [
                        {label,?__(23,"Texture")},{color,Color1, [key({M,color1})]},
                        panel,
                        {label,?__(24,"Base")},{color,Color2, [key({M,color2})]},
                        panel,
                        {?__(25,"Hard Noise"),Hard, [key({M,hard})]},
                        %% Start Noise Basis Select
                        {menu,[{?__(36,"Blender-Basis"),blender},
                            {?__(37,"Cellnoise"),cellnoise},
                            {?__(38,"New Perlin"),newperlin},
                            {?__(39,"Perlin"),stdperlin},
                            {?__(40,"Voronoi Crackle"),voronoi_crackle},
                            {?__(41,"Voronoi F1"),voronoi_f1},
                            {?__(42,"Voronoi F2"),voronoi_f2},
                            {?__(43,"Voronoi F3"),voronoi_f3},
                            {?__(44,"Voronoi F4"),voronoi_f4},
                            {?__(45,"Voronoi F1F2"),voronoi_f2f1}],
                            NoiseBasis,[key({M,noise_basis})]}
                        %% End Noise Basis Select
                    ],[key({pnl_base1,M})]},

                    %% Clouds,Marble,Wood Specific Procedurals Line 2
                    {hframe, [
                        {hframe, [
                            {label,?__(26,"Noise Size")},
                            {text,NoiseSize,[key({M,noise_size}), range(noise_size)]}
                        ]},
                        {hframe, [
                            {label,?__(27,"Noise Depth")},
                            {text,Depth,[key({M,depth}), range(noise_depth)]}
                        ]}
                    ],[key({pnl_base2,M}),{margin,false},{show,false}]},

                    %% Marble Specific Procedurals
                    {hframe, [
                        {hframe, [
                            {label,?__(28,"Sharpness")},
                            {text,Sharpness,[key({M,sharpness}), range(sharpness)]}
                        ],[key({pnl_sharpness,M})]},
                        %],[hook(open, [member,{?TAG,type,M},marble])]},

                        %% Marble,Wood Specific Procedurals
                        {hframe, [
                            {hframe, [
                                {label,?__(29,"Turbulence")},
                                {text,Turbulence,[key({M,turbulence}), range(turbulence)]}
                            ]},
                            %% Start Shape Select
                            {menu,[
                                {?__(30,"sin"),"sin"},
                                {?__(31,"saw"),saw},
                                {?__(32,"tri"),tri}
                            ],Shape,[key({M,shape})]}
                            %% End Shape Select
                        ],[key({pnl_turb,M}),{margin,false}]},

                        %% Wood Specific Procedurals
                        {hframe, [
                            %% Start Wood Type Select
                            {menu,[
                                {?__(33,"Rings"),rings},
                                {?__(34,"Bands"),bands}
                            ],WoodType,[key({M,wood_type})]}
                            %% End Wood Type Select
                        ],[key({pnl_wood,M})]}
                    ],[key({pnl_base3,M}),{margin,false},{show,false}]},

                    %% Voronoi Specific Procedurals
                    {vframe, [
                        %% Start Voronoi Line 1
                        {hframe, [
                            %% Start Voronoi Cell Type Select
                            {menu,[
                                {?__(47,"Intensity"),intensity},
                                {?__(48,"Color"),col1},
                                {?__(49,"Color+Outline"),col2},
                                {?__(50,"Color+Outline+Intensity"),col3}
                            ],CellType,[key({M,cell_type})]},
                            %% End Voronoi Cell Type Select
                            panel,
                            %% Start Voronoi Cell Shape Select
                            {menu,[{?__(51,"Actual Distance"),actual},
                                   {?__(52,"Distance Squared"),squared},
                                   {?__(53,"Manhattan"),manhattan},
                                   {?__(54,"Chebychev"),chebychev},
                                   {?__(55,"Minkovsky"),minkovsky}],
                                CellShape,[key({M,cell_shape})]}
                            %% End Voronoi Cell Shape Select
                        ],[{margin,false}]},
                        %% End Voronoi Line 1

                        %% Start Voronoi Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(56,"Cell Size")},
                                {text,CellSize,[key({M,cell_size}), range(cell_size)]}
                            ]},
                            {hframe, [
                                {label,?__(57,"Intensity")},
                                {text,Intensity,[key({M,intensity}), range(intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Voronoi Line 2

                        %% Start Voronoi Line 3
                        {hframe, [
                            {hframe, [
                                {label,?__(58,"W1")},
                                {text,CellWeight1,[key({M,cell_weight1}), range(cell_weight1)]}
                            ]},
                            {hframe, [
                                {label,?__(59,"W2")},
                                {text,CellWeight2,[key({M,cell_weight2}), range(cell_weight2)]}
                            ]},
                            {hframe, [
                                {label,?__(60,"W3")},
                                {text,CellWeight3,[key({M,cell_weight3}), range(cell_weight3)]}
                            ]},
                            {hframe, [
                                {label,?__(61,"W4")},
                                {text,CellWeight4,[key({M,cell_weight4}), range(cell_weight4)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Voronoi Line 3
                    ],[key({pnl_voronoi,M}),{margin,false},{show,false}]},

                    %% Start Musgrave Specific Procedurals
                    {vframe,[
                        {hframe,[
                            %% Start Musgrave Type Select
                            {menu,[
                                {?__(63,"Multifractal"),multifractal},
                                {?__(64,"Ridged"),ridgedmf},
                                {?__(65,"Hybrid"),hybridmf},
                                {?__(66,"FBM"),fBm}
                            ],MusgraveType,[key({M,musgrave_type})]},
                            panel,
                            {hframe,[
                                {label,?__(77,"Noise Size")},
                                {text,MusgraveNoiseSize,[key({M,musgrave_noisesize}), range(musgrave_noisesize)]}
                            ]},
                            {hframe,[
                                {label,?__(78,"Intensity")},
                                {text,MusgraveIntensity,[key({M,musgrave_intensity}), range(musgrave_intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Musgrave Line 1

                        %% Start Musgrave Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(79,"Contrast (H)")},
                                {text,MusgraveContrast,[key({M,musgrave_contrast}), range(musgrave_contrast)]}
                            ]},
                            {hframe, [
                                {label,?__(80,"Lacunarity")},
                                {text,MusgraveLacunarity,[key({M,musgrave_lacunarity}), range(musgrave_lacunarity)]}
                            ]},
                            {hframe, [
                                {label,?__(81,"Octaves")},
                                {text,MusgraveOctaves,[key({M,musgrave_octaves}), range(musgrave_octaves)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Musgrave Line 2
                    ],[key({pnl_musgrave,M})]},

                    %% Start Distorted Noise Specific Procedurals
                    {vframe, [
                        {hframe, [
                            %% Start Distorted Noise Type Select
                            {menu,[
                                {?__(87,"Blender-Distort"),blender},
                                {?__(88,"Cellnoise"),cellnoise},
                                {?__(89,"New Perlin"),newperlin},
                                {?__(90,"Perlin"),stdperlin},
                                {?__(91,"Voronoi Crackle"),voronoi_crackle},
                                {?__(92,"Voronoi F1"),voronoi_f1},
                                {?__(93,"Voronoi F2"),voronoi_f2},
                                {?__(94,"Voronoi F3"),voronoi_f3},
                                {?__(95,"Voronoi F4"),voronoi_f4},
                                {?__(96,"Voronoi F1F2"),voronoi_f2f1}
                            ],DistortionType,[key({M,distortion_type})]},
                            %% End Distorted Noise Type Select
                            {label,?__(107,"Noise Size")},{text,DistortionNoiseSize,[key({M,distortion_noisesize}), range(distortion_noisesize)]},
                            {label,?__(108,"Distortion")},{text,DistortionIntensity,[key({M,distortion_intensity}), range(distortion_intensity)]}
                        ],[{margin,false}]}
                    ],[key({pnl_dist_noise,M}),{show,false}]}
                ],[key({pnl_type,M})]}
            ],[key({pnl_mod,M})]}
        ]},
    [{?__(35,"Texture")++" "++integer_to_list(M)++mod_legend(Enabled, Mode, Type), ModFrame}];

modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

mod_enabled_mode_type(Ps, Maps) ->
    {Enabled,Mode} =
        case proplists:get_value(mode, Ps, ?DEF_MOD_MODE) of
            off -> {false,?DEF_MOD_MODE};
            Mode1 -> {proplists:get_value(enabled, Ps, ?DEF_MOD_ENABLED),Mode1}
        end,
    Type = proplists:get_value(type, Ps, ?DEF_MOD_TYPE),
    case Type of
        {map,Map} ->
            case lists:keymember(Map, 1, Maps) of
                true -> {Enabled,Mode,Type};
                false -> {false,Mode,?DEF_MOD_TYPE}
            end;
        _ -> {Enabled,Mode,Type}
    end.

mod_legend(Enabled, Mode, {map,Map}) ->
    mod_legend(Enabled, Mode, atom_to_list(Map));
mod_legend(Enabled, Mode, Type) when is_atom(Mode) ->
    mod_legend(Enabled, wings_util:cap(Mode), Type);
mod_legend(Enabled, Mode, Type) when is_atom(Type) ->
    mod_legend(Enabled, Mode, wings_util:cap(Type));
mod_legend(Enabled, Mode, Type) when is_list(Mode), is_list(Type) ->
    case Enabled of
        true -> " ("++?__(1,"enabled")++", ";
        false -> " ("++?__(2,"disabled")++", "
    end++Mode++", "++Type++")".


process_modulator(Ps) ->
    {Modulators,Remaining} =
        lists:foldr(fun(Id, {Mod0,Ps0})->
            {Mod1, Ps1} = modulator_result_find(Ps0, Id, {[],[]}),
            {[{modulator, lists:sort(Mod1)}]++Mod0,Ps1}
        end, {[],Ps}, lists:seq(1,?MAX_MODULATORS)),
    {[{modulators, Modulators}], Remaining}.

modulator_result_find([], _, Acc) -> Acc;
modulator_result_find([{{Id,Field},Value}|Ps], Id, {Mod, Remaining}) ->
    modulator_result_find(Ps, Id, {Mod ++[{Field,Value}], Remaining});
modulator_result_find([Item|Ps], Id, {Mod, Remaining}) ->
    modulator_result_find(Ps, Id, {Mod, Remaining ++ [Item]}).


%%% Creates new Modulator
modulator_add(M) ->
    modulator_add(M,[]).
modulator_add(0,Acc) ->
    Acc;
modulator_add(M,Acc) ->
    modulator_add(M-1, Acc++[modulator_init(?DEF_MOD_MODE)]).

modulator_init(Mode) ->
    Ps = [{enabled,false},
        {mode,Mode},
        {alpha_intensity,?DEF_MOD_ALPHA_INTENSITY},
        {size_x,?DEF_MOD_SIZE_X},
        {size_y,?DEF_MOD_SIZE_Y},
        {size_z,?DEF_MOD_SIZE_Z},
        {diffuse,?DEF_MOD_DIFFUSE},
%        {specular,?DEF_MOD_SPECULAR},
        {shininess,?DEF_MOD_SHININESS},
        {normal,?DEF_MOD_NORMAL},
        {type,?DEF_MOD_TYPE},
        {filename,?DEF_MOD_FILENAME},
        {color1,?DEF_MOD_COLOR1},
        {color2,?DEF_MOD_COLOR2},
        {hard,?DEF_MOD_HARD},
        {noise_basis,?DEF_MOD_NOISEBASIS},
        {noise_size,?DEF_MOD_NOISESIZE},
        {depth,?DEF_MOD_DEPTH},
        {sharpness,?DEF_MOD_SHARPNESS},
        {turbulence,?DEF_MOD_TURBULENCE},
        {shape,?DEF_MOD_SHAPE},
        {wood_type,?DEF_MOD_WOODTYPE},
        {cell_type,?DEF_MOD_CELLTYPE},
        {cell_shape,?DEF_MOD_CELLSHAPE},
        {cell_size,?DEF_MOD_CELLSIZE},
        {intensity,?DEF_MOD_INTENSITY},
        {cell_weight1,?DEF_MOD_CELL_WEIGHT1},
        {cell_weight2,?DEF_MOD_CELL_WEIGHT2},
        {cell_weight3,?DEF_MOD_CELL_WEIGHT3},
        {cell_weight4,?DEF_MOD_CELL_WEIGHT4},
        {musgrave_type,?DEF_MOD_MUSGRAVE_TYPE},
        {musgrave_noisesize,?DEF_MOD_MUSGRAVE_NOISESIZE},
        {musgrave_intensity,?DEF_MOD_MUSGRAVE_INTENSITY},
        {musgrave_contrast,?DEF_MOD_MUSGRAVE_CONTRAST},
        {musgrave_lacunarity,?DEF_MOD_MUSGRAVE_LACUNARITY},
        {musgrave_octaves,?DEF_MOD_MUSGRAVE_OCTAVES},
        {distortion_type,?DEF_MOD_DISTORTION_TYPE},
        {distortion_noisesize,?DEF_MOD_DISTORTION_NOISESIZE},
        {distortion_intensity,?DEF_MOD_DISTORTION_INTENSITY}
    ],
    {modulator,Ps}.


%%%
%%% Light dialogs
%%%
light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafaRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = 
        case Type of
            point -> ?DEF_ATTN_POWER;
            spot -> ?DEF_ATTN_POWER;
            area -> ?DEF_ATTN_POWER;
            _ -> ?DEF_POWER
        end,
    Power = proplists:get_value(power, YafaRay, DefPower),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    PowerStr =
        [{vframe, [
            {hframe, [
                {label_column, [
                    {?__(1,"Power"), {text,Power,[key(power),range(power),key(power)]}}
                ]},
                panel,
                help_button({light_dialog,Type})],[{margin,false}]
            },
            {hframe, [
                {?__(2,"Cast Shadows"),CastShadows,[key(cast_shadows)]}
            ],[{margin,false}]}
         ]}
        ],
    {vframe, PowerStr ++ light_dialog(Name, Type, YafaRay)}.

%%% Point Light Dialog
light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    ArealightRadius = proplists:get_value(arealight_radius, Ps, ?DEF_AREALIGHT_RADIUS),
    ArealightSamples = proplists:get_value(arealight_samples, Ps, ?DEF_AREALIGHT_SAMPLES),

    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(type) ->
                    wings_dialog:show(?KEY(pnl_sphere), Value =:= spherelight, Store)
            end
        end,

    [
        {hradio,[
            {?__(3,"Pointlight"),pointlight},
            {?__(5,"Spherelight"),spherelight}
        ],Type,[key(type),{hook,Hook_Show}]},
        {hframe, [
            {label_column, [
                {?__(15,"Radius"),
                {text,ArealightRadius,[key(arealight_radius), range(arealight_radius)]}}
            ]},
            panel,
            {label_column, [
                {?__(17,"Samples"),
                {text,ArealightSamples,[key(arealight_samples), range(samples)]}}
            ]}
        ],[key(pnl_sphere),{margin,false}]}
    ];

%%% Spot Light Dialog
light_dialog(_Name, spot, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_SPOT_TYPE),
    SpotPhotonOnly = proplists:get_value(spot_photon_only, Ps, ?DEF_SPOT_PHOTON_ONLY),
    SpotSoftShadows = proplists:get_value(spot_soft_shadows, Ps, ?DEF_SPOT_SOFT_SHADOWS),
    SpotBlend = proplists:get_value(spot_blend, Ps, ?DEF_SPOT_BLEND),
    SpotFuzzyness = proplists:get_value(spot_fuzzyness, Ps, ?DEF_SPOT_FUZZYNESS),
    SpotIESFilename = proplists:get_value(spot_ies_filename, Ps, ?DEF_SPOT_IES_FILENAME),
    SpotIESSamples = proplists:get_value(spot_ies_samples, Ps, ?DEF_SPOT_IES_SAMPLES),
    BrowsePropsIES = [{dialog_type,open_dialog},{extensions,[{".ies",?__(30,"IES")}]}],

    Hook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(spot_soft_shadows) ->
                    wings_dialog:enable(?KEY(spot_ies_samples), Value=/=?DEF_SPOT_SOFT_SHADOWS, Store),
                    wings_dialog:enable(?KEY(spot_fuzzyness), Value=/=?DEF_SPOT_SOFT_SHADOWS, Store)
            end
        end,

    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(type) ->
                    wings_dialog:show(?KEY(pnl_ies), Value =:= spot_ies, Store),
                    wings_dialog:show(?KEY(pnl_spt_photon), Value =:= spotlight, Store),
                    wings_dialog:update(?KEY(pnl_spot_light), Store)
            end
        end,

    [
        {hradio,[
            {?__(29,"Spotlight"),spotlight},
            {?__(30,"IES"),spot_ies}
        ],Type,[key(type),{hook,Hook_Show}]},
        {vframe, [
            {label_column, [
                {?__(100,"Filename"),
                {button,{text,SpotIESFilename,[key(spot_ies_filename),{width,35},{props,BrowsePropsIES}]}}}
            ],[key(pnl_ies)]},
            {vframe, [
                {hframe, [
                    {label_column, [
			{?__(101,"Blend"),
                    	{text,SpotBlend,[key(spot_blend),range(spot_blend)]}}
		    ]},
                    panel,
                    {?__(97,"Photon Only"),SpotPhotonOnly,[key(spot_photon_only)]}
                ],[key(pnl_spt_photon),{margin,false}]},
                {hframe, [
                    {?__(38,"Soft Shadows"),SpotSoftShadows,[key(spot_soft_shadows),{hook,Hook_Enabled}]},
                    panel,
                    {label_column, [
			{?__(35,"Samples"),
                    	{text,SpotIESSamples,[range(spot_ies_samples),key(spot_ies_samples)]}}]},
                    panel,
                    {label_column, [
			{?__(102,"Fuzzyness"),
                    	{text,SpotFuzzyness,[range(spot_fuzzyness),key(spot_fuzzyness)]}}
		    ]}
		], [{margin,false}]}
            ],[{margin,false}]}
        ],[key(pnl_spot_light),[{margin,false}]]}
    ];

%%% Infinite Light Dialog
light_dialog(_Name, infinite, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_INFINITE_TYPE),
    SunSamples = proplists:get_value(sun_samples, Ps, ?DEF_SUN_SAMPLES),
    SunAngle = proplists:get_value(sun_angle, Ps, ?DEF_SUN_ANGLE),
    SkyBackgroundLight = proplists:get_value(sky_background_light, Ps, ?DEF_SKY_BACKGROUND_LIGHT),
    SkyBackgroundPower = proplists:get_value(sky_background_power, Ps, ?DEF_SKY_BACKGROUND_POWER),
    SkyBackgroundSamples = proplists:get_value(sky_background_samples, Ps, ?DEF_SKY_BACKGROUND_SAMPLES),
    InfiniteTrue = proplists:get_value(infinite_true, Ps, ?DEF_INFINITE_TRUE),
    InfiniteRadius = proplists:get_value(infinite_radius, Ps, ?DEF_INFINITE_RADIUS),
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND_INFINITE),
    %%
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    A_var = proplists:get_value(a_var, Ps, ?DEF_SUNSKY_VAR),
    B_var = proplists:get_value(b_var, Ps, ?DEF_SUNSKY_VAR),
    C_var = proplists:get_value(c_var, Ps, ?DEF_SUNSKY_VAR),
    D_var = proplists:get_value(d_var, Ps, ?DEF_SUNSKY_VAR),
    E_var = proplists:get_value(e_var, Ps, ?DEF_SUNSKY_VAR),
    %%
    DarkskyAltitude = proplists:get_value(darksky_altitude, Ps, ?DEF_DARKSKY_ALTITUDE),
    SunReal = proplists:get_value(sun_real, Ps, ?DEF_SUN_REAL),
    SunRealPower = proplists:get_value(sun_real_power, Ps, ?DEF_SUN_REAL_POWER),

    DarkskyNight = proplists:get_value(darksky_night, Ps, ?DEF_DARKSKY_NIGHT),
    DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, Ps, ?DEF_DARKSKY_DIFFUSEPHOTONS),
    DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, Ps, ?DEF_DARKSKY_CAUSTICPHOTONS),

    Hook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(infinite_true) ->
                    wings_dialog:enable(?KEY(pnl_inf_radius), Value =:= false, Store);
                ?KEY(sun_real) ->
                    wings_dialog:enable(?KEY(pnl_sun_real), Value =/= ?DEF_SUN_REAL, Store);
                ?KEY(sky_background_light) ->
                    wings_dialog:enable(?KEY(pnl_bkg_power), Value =/= ?DEF_SKY_BACKGROUND_LIGHT, Store),
                    wings_dialog:enable(?KEY(pnl_bkg_photons), Value =/=?DEF_SKY_BACKGROUND_LIGHT, Store)
            end
        end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(type) ->
                    wings_dialog:show(?KEY(pnl_sunlight), Value =:= sunlight, Store),
                    wings_dialog:show(?KEY(pnl_directional), Value =:= directional, Store),
                    wings_dialog:update(?KEY(pnl_base1),Store);
                ?KEY(background) ->
                    wings_dialog:show(?KEY(pnl_darksky_altitude), Value =:= darksky, Store),
                    wings_dialog:show(?KEY(darksky_night), Value =:= darksky, Store),
                    wings_dialog:show(?KEY(pnl_sky), Value =/= undefined, Store),
                    wings_dialog:update(?KEY(pnl_sky), Store)
            end
        end,
    [
        {vframe, [
            {vframe, [
                {hradio, [
                    {?__(110,"Sunlight"),sunlight},
                    {?__(111,"Directional"),directional}
                ],Type,[key(type),{hook,Hook_Show}]},
                %% Sunlight Settings Start
                {hframe, [
                    {label_column, [
                        {?__(114,"Samples")++" ",
                        {text,SunSamples,[key(sun_samples),range(sun_samples)]}}
                    ]},
                    panel,
                    {label_column, [
                        {?__(115,"Angle")++" ",
                        {text,SunAngle,[key(sun_angle),range(sun_angle)]}}
                    ]}
                ],[key(pnl_sunlight), {margin,false}]},
                %% Sunlight Settings End

                %% Directional Semi-infinite Radius Settings Start
                {hframe, [
                    {?__(112,"Infinite"),InfiniteTrue,[key(infinite_true),{hook,Hook_Enabled}]},
                    panel,
                    %% Directional Semi-infinite Radius
                    {label_column, [
                        {?__(113,"Semi-infinite Radius"),
                        {text,InfiniteRadius,[key(infinite_radius), range(infinite_radius)]}}
                    ],[key(pnl_inf_radius), {margin,false}]}
                ],[key(pnl_directional),{show,false}]}
                %% End Directional Semi-infinite Radius
            ],[key(pnl_base1),{margin,false}]},

            {vframe, [
                %% Sunsky Background
                {hradio, [
                    {?__(45,"None"), undefined},
                    {?__(44,"Sunsky"),sunsky},
                    {?__(46,"Darktide Sunsky"),darksky}
                ],Bg,[key(background),{hook,Hook_Show}]},
                {vframe, [
                    {hframe, [
                        {label_column, [
                            {?__(47,"Turbidity"),{text,Turbidity,[key(turbidity),range(turbidity)]}},
                            {"a: "++?__(48,"Horizon Brightness"),{text,A_var,[key(a_var)]}},
                            {"b: "++?__(49,"Horizon Spread"),{text,B_var,[key(b_var)]}}
                        ],[{margin,false}]},
                        {label_column, [
                            {"c: "++?__(50,"Sun Brightness"),{text,C_var,[key(c_var)]}},
                            {"d: "++?__(51,"Sun Distance"),{text,D_var,[key(d_var)]}},
                            {"e: "++?__(52,"Backscattered Light"),{text,E_var,[key(e_var)]}}
                        ],[{margin,false}]}
                    ],[{margin,false}]},

                    {hframe,[
                        {hframe,[
                            {label,?__(122,"Altitude Adjustment")},
                            {text,DarkskyAltitude,[range(darksky_altitude),key(darksky_altitude)]}
                        ],[key(pnl_darksky_altitude),{margin,false}]}
                    ]},

                    %% Start Sun Real Settings
                    {hframe,[
                        {?__(120,"Real Sun"),SunReal,[key(sun_real),{hook,Hook_Enabled}]},
                        panel,
                        {label_column,[
                            {?__(121,"Sun Power"),
                            {text,SunRealPower,[range(sun_real_power),key(sun_real_power)]}}
                        ],[key(pnl_sun_real),{margin,false}]}
                    ]},

                    %% Start Skylight Settings
                    {vframe,[
                        {hframe, [
                            {?__(116,"Skylight"),SkyBackgroundLight,[key(sky_background_light),{hook,Hook_Enabled}]},
                            panel,
                            {hframe, [
                                {label,?__(117,"Power")},
                                {text,SkyBackgroundPower,[key(sky_background_power),range(sky_background_power)]},
                                panel,
                                {label,?__(118,"Samples")},
                                {text,SkyBackgroundSamples,[key(sky_background_samples),range(sky_background_samples)]}
                            ], [key(pnl_bkg_power),{margin,false}]}
                        ]},
                        {hframe, [
                            {?__(123,"Diffuse Photons"),DarkskyDiffusePhotons,[key(darksky_diffusephotons)]},
                            panel,
                            {?__(124,"Caustic Photons"),DarkskyCausticPhotons,[key(darksky_causticphotons)]}
                        ],[key(pnl_bkg_photons)]}
                    ],[{margin,false}]},
                    {vframe,[
                        {?__(119,"Night"),DarkskyNight,[key(darksky_night)]}
                    ]}
                %% End Skylight Settings
                ],[key(pnl_sky),{margin,false}]}
            ],[{title,?__(53,"Sky")},key(pnl_base2),{margin,false}]}
        ]}
    ];

%%% Ambient Light Dialog
light_dialog(_Name, ambient, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND_AMBIENT),
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    %%
    HorizonColor = proplists:get_value(horizon_color, Ps, ?DEF_HORIZON_COLOR),
    ZenithColor = proplists:get_value(zenith_color, Ps, ?DEF_ZENITH_COLOR),
    %%
    BgFnameImage = proplists:get_value(background_filename_image, Ps, ?DEF_BACKGROUND_FILENAME),
    ImageFormats = images_format(),
    BrowsePropsImage = [{dialog_type,open_dialog}, {extensions,ImageFormats}],
    BgFnameHDRI = proplists:get_value(background_filename_HDRI, Ps,
                                      ?DEF_BACKGROUND_FILENAME),
    BrowsePropsHDRI = [{dialog_type,open_dialog},
                       {extensions,[{".hdr",?__(56,"High Dynamic Range image")},
                                    {".exr",?__(95,"OpenEXR image")}]}],
    BgMapping = proplists:get_value(background_mapping, Ps, ?DEF_BACKGROUND_MAPPING),
    BgEnlight = proplists:get_value(background_enlight, Ps, ?DEF_BACKGROUND_ENLIGHT),
    AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, Ps, ?DEF_AMBIENT_DIFFUSEPHOTONS),
    AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, Ps, ?DEF_AMBIENT_CAUSTICPHOTONS),
    BgRotation = proplists:get_value(background_rotation, Ps, ?DEF_BACKGROUND_ROTATION),
    %%
    Type = proplists:get_value(type, Ps, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, Ps, ?DEF_SAMPLES),
    Smartibl_blur = proplists:get_value(smartibl_blur, Ps, ?DEF_SMARTIBL_BLUR),

    Hook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(background_enlight) ->
                    wings_dialog:enable(?KEY(pnl_enlight_samples), Value, Store),
                    wings_dialog:enable(?KEY(pnl_enlight_photons), Value, Store)
            end
        end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(background) ->
                    wings_dialog:show(?KEY(pnl_file), is_member(Value, ['HDRI',image]), Store),
                    wings_dialog:show(?KEY(pnl_img_hdri), Value =:= 'HDRI', Store),
                    wings_dialog:show(?KEY(pnl_img_bkg), Value =:= image, Store),
                    wings_dialog:show(?KEY(pnl_const), Value =:= constant, Store),
                    wings_dialog:show(?KEY(pnl_gradient), Value =:= gradientback, Store),
                    wings_dialog:show(?KEY(pnl_background), Value =/= undefined, Store),
                    wings_dialog:update(?KEY(pnl_background), Store)
            end
        end,

    [
        %% Global Photonlight
        %% Backgrounds
        {vframe, [
            {value,Type,[key(type)]},
            {label_column, [
                {?__(91,"Background Light/Environment")++" ",
                {menu, [
                    {?__(79,"HDRI"),'HDRI'},
                    {?__(80,"Image"),image},
                    {?__(81,"Constant"),constant},
                    {?__(105,"Gradient"),gradientback},
                    {?__(82,"None"), undefined}
                ], Bg, [key(background),{hook,Hook_Show}]}}
            ]},

            {vframe, [
                %% HDRI Background
                {vframe, [
                    {label_column, [
                        {?__(83,"Filename"),
                            {vframe, [
                                {hframe, [
                                    {button,{text,BgFnameImage,[key(background_filename_image),{width,35},{props,BrowsePropsImage}]}}
                                ],[key(pnl_img_bkg),{margin,false},{show,false}]},
                                {hframe, [
                                    {button,{text,BgFnameHDRI,[key(background_filename_HDRI),{width,35},{props,BrowsePropsHDRI}]}},
                                    {menu, [
                                        {?__(86,"Light Probe"),probe},
                                        {?__(87,"Spherical"),spherical}
                                    ], BgMapping, [key(background_mapping)]}
                                ],[key(pnl_img_hdri),{margin,false}]}
                            ],[{margin,false}]}},
                        {?__(108,"Rotation"),
                                {text,BgRotation,[key(background_rotation),range(background_rotation)]}}
                    ],[{margin,false}]}
                ],[key(pnl_file),{margin,false}]},
                %% Constant Background
                {label_column, [
                    {?__(90,"Color"),
                    {color,BgColor,[key(background_color)]}}
                ],[key(pnl_const),{show,false}]},
                %% Gradient Background
                {hframe,[
                    {label_column, [
			{?__(106,"Horizon Color"),
                    	{color,HorizonColor,[key(horizon_color)]}}
		    ]},
		    panel,
                    {label_column, [
			{?__(107,"Zenith Color"),
                    	{color,ZenithColor,[key(zenith_color)]}}
		    ]}
                ],[key(pnl_gradient),{show,false},{margin,false}]},
                %% Common parameters
                {vframe,[
                    {hframe,[
                        {hframe,[
                            {?__(89,"Enlight"),BgEnlight,[key(background_enlight),{hook,Hook_Enabled}]}
                        ]},
                        panel,
                        {hframe,[
                            {label_column, [
			     	{?__(60,"Samples"),
                            	{text,Samples,[key(samples),range(samples)]}}
			    ]},
                            {label_column, [
				{?__(126,"SmartIBL blur"),
                            	{text,Smartibl_blur,[key(smartibl_blur),range(smartibl_blur)]}}
			    ]}
                        ],[key(pnl_enlight_samples),{margin,false}]}
                    ],[{margin,false}]},
                    {hframe, [
                        {?__(94,"Diffuse Photons"),AmbientDiffusePhotons,[key(ambient_diffusephotons)]},
                        panel,
                        {?__(96,"Caustic Photons"),AmbientCausticPhotons,[key(ambient_causticphotons)]}
                    ],[key(pnl_enlight_photons)]}
                ]}
            ],[key(pnl_background),{margin,false}]}
        ]}];

%%% Area Light Dialog
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, ?DEF_AREALIGHT_SAMPLES),

    [
        {label_column,[
	    {?__(93,"Samples"),
	    {text,ArealightSamples,[key(arealight_samples),range(samples)]}}
        ]}
    ];

light_dialog(_Name, _Type, _Ps) ->
%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Light, Res) ->
    {Found, Remaining} = rip_all(?TAG, Res),
    NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Light)],
    {NewLight, Remaining}.

pref_dialog(St) ->
    [{dialogs,Dialogs},{renderer,Renderer},
     {options,Options},{shader_type,ShaderType},
     {render_passes_enable,PassesEnable},
     {render_passes,RenderPasses},
     {computer_node,ComputerNode}]
     = get_user_prefs([{dialogs,?DEF_DIALOGS},{renderer,?DEF_RENDERER},
                                                     {options,?DEF_OPTIONS},{shader_type,?DEF_SHADER_TYPE},
                                                     {render_passes_enable,?DEF_RENDER_PASSES_ENABLE},
                                                     {render_passes,?DEF_RENDER_PASSES},
                                                     {computer_node,?DEF_COMPUTER_NODE}]),
    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            render_passes_enable ->
                wings_dialog:enable(render_passes, Value =:= true, Store);
            _ -> ok
        end
    end,
    Dialog = [
        {vframe, [
            {hframe, [
                {menu,[
                    {?__(1,"Disabled Dialogs"),disabled},
                    {?__(2,"Automatic Dialogs"),auto},
                    {?__(3,"Enabled Dialogs"),enabled}
                ], Dialogs,[{key,dialogs}]},
                panel,
                help_button(pref_dialog)
            ]},
            {label_column, [
                {?__(4,"Executable"),{button,{text,Renderer,[{key,renderer},{width,35},wings_job:browse_props()]}}},
                {?__(5,"Options"),{text,Options,[{key,options}]}},
                {?__(8,"Default Shader"),{menu,menu_shader(), ShaderType, [{key,shader_type}]}},
                {?__(10,"Computer Node"),{text, ComputerNode,[{key,computer_node}]}}
            ]},
            {hframe,[
                {?__(9,"Render Passes Enable"), PassesEnable, [{key,render_passes_enable},{hook,Hook_Enable}]},
                panel,
                {slider, {text, RenderPasses,[{key,render_passes},range(render_passes)]}}
            ]}
        ], [{title,""}]}],
    wpa:dialog(?__(6,"YafaRay Options"), Dialog, fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_user_prefs(Attr),
    OldVal = get_var(renderer),
    init_pref(),
    case get_var(renderer) of
        OldVal -> ok;
        false ->
            wings_menu:update_menu(file, {render, ?TAG}, delete);
        _ ->
            [{Label, _}] = menu_entry(render),
            wings_menu:update_menu(file, {render, ?TAG}, {append, -1, Label})
    end,
    St.

export_dialog(Op, Title) ->
    wpa:dialog(true, Title,
               export_dialog_qs(Op, get_prefs(export_prefs())),
               fun(Attr) -> {file,{Op,{?TAG,Attr}}} end, fun () -> help_export() end).

%% Export Render Options Dialog Settings
export_prefs() ->
    {_,Max} = range_1(render_passes),
    RenderPass =
        lists:foldl(fun(N, Acc) ->
                         Acc++[{render_pass_id(N),?DEF_RENDER_PASS}]
                     end, [], lists:seq(1,Max)),
    [{subdivisions,?DEF_SUBDIVISIONS},
        {keep_xml,?DEF_KEEP_XML},
        {threads_number,?DEF_THREADS_NUMBER},
        {threads_auto,?DEF_THREADS_AUTO},
        {tile_size,?DEF_TILE_SIZE},
        {tile_order,?DEF_TILE_ORDER},
        {lighting_method,?DEF_LIGHTING_METHOD},
        {use_caustics,?DEF_USE_CAUSTICS},
        {caustic_photons,?DEF_CAUSTIC_PHOTONS},
        {caustic_depth,?DEF_CAUSTIC_DEPTH},
        {caustic_mix,?DEF_CAUSTIC_MIX},
        {caustic_radius,?DEF_CAUSTIC_RADIUS},
        {do_ao,?DEF_DO_AO},
        {ao_distance,?DEF_AO_DISTANCE},
        {ao_samples,?DEF_AO_SAMPLES},
        {ao_color,?DEF_AO_COLOR},
        {pm_diffuse_photons,?DEF_PM_DIFFUSE_PHOTONS},
        {pm_bounces,?DEF_PM_BOUNCES},
        {pm_search,?DEF_PM_SEARCH},
        {pm_diffuse_radius,?DEF_PM_DIFFUSE_RADIUS},
        {pm_caustic_photons,?DEF_PM_CAUSTIC_PHOTONS},
        {pm_caustic_radius,?DEF_PM_CAUSTIC_RADIUS},
        {pm_caustic_mix,?DEF_PM_CAUSTIC_MIX},
        {pm_use_fg,?DEF_PM_USE_FG},
        {pm_fg_bounces,?DEF_PM_FG_BOUNCES},
        {pm_fg_samples,?DEF_PM_FG_SAMPLES},
        {pm_fg_show_map,?DEF_PM_FG_SHOW_MAP},
        {pt_diffuse_photons,?DEF_PT_DIFFUSE_PHOTONS},
        {pt_bounces,?DEF_PT_BOUNCES},
        {pt_caustic_type,?DEF_PT_CAUSTIC_TYPE},
        {pt_caustic_radius,?DEF_PT_CAUSTIC_RADIUS},
        {pt_caustic_mix,?DEF_PT_CAUSTIC_MIX},
        {pt_caustic_depth,?DEF_PT_CAUSTIC_DEPTH},
        {pt_samples,?DEF_PT_SAMPLES},
        {sppm_photons,?DEF_SPPM_PHOTONS},
        {sppm_bounces,?DEF_SPPM_BOUNCES},
        {sppm_search,?DEF_SPPM_SEARCH},
        {sppm_radius,?DEF_SPPM_RADIUS},
        {sppm_times,?DEF_SPPM_TIMES},
        {sppm_passes,?DEF_SPPM_PASSES},
        {sppm_ire, ?DEF_SPPM_IRE},
        {volintegr_type,?DEF_VOLINTEGR_TYPE},
        {volintegr_adaptive,?DEF_VOLINTEGR_ADAPTIVE},
        {volintegr_optimize,?DEF_VOLINTEGR_OPTIMIZE},
        {volintegr_stepsize,?DEF_VOLINTEGR_STEPSIZE},
        {raydepth,?DEF_RAYDEPTH},
        {gamma,?DEF_GAMMA},
        {shadow_bias_auto,?DEF_SHADOW_BIAS_AUTO},
        {shadow_bias,?DEF_SHADOW_BIAS},
        {ray_mindist_auto,?DEF_RAY_MINDIST_AUTO},
        {ray_mindist,?DEF_RAY_MINDIST},
        {transparent_shadows,?DEF_TRANSPARENT_SHADOWS},
        {shadow_depth,?DEF_SHADOW_DEPTH},
        {render_format,?DEF_RENDER_FORMAT},
        {exr_flag_float,false},
        {exr_flag_zbuf,false},
        {exr_flag_compression,?DEF_EXR_FLAG_COMPRESSION},
        {aa_passes,?DEF_AA_PASSES},
        {aa_minsamples,?DEF_AA_MINSAMPLES},
        {aa_incsamples,?DEF_AA_INCSAMPLES},
        {aa_threshold,?DEF_AA_THRESHOLD},
        {aa_pixelwidth,?DEF_AA_PIXELWIDTH},
        {aa_filter_type,?DEF_AA_FILTER_TYPE},
        {aa_noise_resampled_floor,?DEF_AA_NOISE_RESAMPLED_FLOOR},
        {aa_noise_sample_multiplier_factor,?DEF_AA_NOISE_SAMPLE_MULTIPLIER_FACTOR},
        {aa_noise_light_sample_multiplier_factor,?DEF_AA_NOISE_LIGHT_SAMPLE_MULTIPLIER_FACTOR},
        {aa_noise_indirect_sample_multiplier_factor,?DEF_AA_NOISE_INDIRECT_SAMPLE_MULTIPLIER_FACTOR},
        {aa_noise_detect_color_noise,?DEF_AA_NOISE_DETECT_COLOR_NOISE},
        {aa_noise_dark_detection_type,?DEF_AA_NOISE_DARK_DETECTION_TYPE},
        {aa_noise_dark_threshold_factor,?DEF_AA_NOISE_DARK_THRESHOLD_FACTOR},
        {aa_noise_variance_edge_size,?DEF_AA_NOISE_VARIANCE_EDGE_SIZE},
        {aa_noise_variance_pixels,?DEF_AA_NOISE_VARIANCE_PIXELS},
        {aa_noise_clamp_samples,?DEF_AA_NOISE_CLAMP_SAMPLES},
        {aa_noise_clamp_indirect,?DEF_AA_NOISE_CLAMP_INDIRECT},       
        {background_color,?DEF_BACKGROUND_COLOR},
        {save_alpha,?DEF_SAVE_ALPHA},
        {background_transp,?DEF_BACKGROUND_TRANSP},
        {background_transp_refract,?DEF_BACKGROUND_TRANSP_REFRACT},
        {lens_type,?DEF_LENS_TYPE},
        {lens_ortho_scale,?DEF_LENS_ORTHO_SCALE},
        {lens_angular_circular,?DEF_LENS_ANGULAR_CIRCULAR},
        {lens_angular_mirrored,?DEF_LENS_ANGULAR_MIRRORED},
        {lens_angular_max_angle,?DEF_LENS_ANGULAR_MAX_ANGLE},
        {lens_angular_angle,?DEF_LENS_ANGULAR_ANGLE},
        {bokeh_use_QMC,?DEF_USE_QMC},
        {width,?DEF_WIDTH},
        {aperture,?DEF_APERTURE},
        {bokeh_type,?DEF_BOKEH_TYPE},
        {height,?DEF_HEIGHT},
        {aperture,?DEF_APERTURE},
        {bokeh_bias,?DEF_BOKEH_BIAS},
        {bokeh_rotation,?DEF_BOKEH_ROTATION},
        {dof_distance,?DEF_DOF_DISTANCE},
        {img_autosave_type,?DEF_IMG_AUTOSAVE_TYPE},
        {img_autosave_passes,?DEF_IMG_AUTOSAVE_PASSES},
        {img_autosave_seconds,?DEF_IMG_AUTOSAVE_SECONDS},
        {img_denoise_enable,?DEF_IMG_DENOISE_ENABLE},
        {img_denoise_mix,?DEF_IMG_DENOISE_MIX},
        {img_denoise_h_lum,?DEF_IMG_DENOISE_H_LUM},
        {img_denoise_h_chrom,?DEF_IMG_DENOISE_H_CHROM},
        {film_processing,?DEF_FILM_PROCESSING},
        {film_binary_format,?DEF_FILM_BINARY_FORMAT},
        {film_autosave_type,?DEF_FILM_AUTOSAVE_TYPE},
        {film_autosave_passes,?DEF_FILM_AUTOSAVE_PASSES},
        {film_autosave_seconds,?DEF_FILM_AUTOSAVE_SECONDS},
        {log_save_txt,?DEF_LOG_SAVE_TXT},
        {log_save_html,?DEF_LOG_SAVE_HTML},
        {log_verbosity_console,?DEF_LOG_VERBOSITY_CONSOLE},
        {log_verbosity_txt_html,?DEF_LOG_VERBOSITY_TXT_HTML},
        {badge_position,?DEF_BADGE_POSITION},
        {badge_title,?DEF_BADGE_TITLE},
        {badge_author,?DEF_BADGE_AUTHOR},
        {badge_contact,?DEF_BADGE_CONTACT},
        {badge_comments,?DEF_BADGE_COMMENTS},
        {badge_custom_icon_path,?DEF_BADGE_CUSTOM_ICON_PATH},
        {badge_draw_render_settings,?DEF_BADGE_DRAW_RENDER_SETTINGS},
        {badge_draw_aa_noise_settings,?DEF_BADGE_DRAW_AA_NOISE_SETTINGS},
        {badge_font_path,?DEF_BADGE_FONT_PATH},
        {badge_font_size_factor,?DEF_BADGE_FONT_SIZE_FACTOR}
        ] ++ RenderPass.

f_stop_str(Value) when is_float(Value) ->
    %% we must use the same number of decimals used in the aperture edit box
    float_to_list(Value,[{decimals,6}]);
f_stop_str(Value) -> Value.

f_stop_find(Value,List) ->
    case lists:keyfind(f_stop_str(Value),2,List) of
        false -> {f_stop_str(-1.0),-1.0};
        {_,Idx,Val} -> {Idx,Val}
    end.

export_dialog_qs(Op, Attr) ->
    Aperture = get_pref(aperture,Attr),
    Custom = f_stop_str(-1.0),
    ApertureList = [
        {F, f_stop_str(math:sqrt(A)),math:sqrt(A)}
        || {F, A} <- [{"1.0", 1 / 1},
                        {"1.4", 1 / 2},
                        {"2", 1 / 4},
                        {"2.8", 1 / 8},
                        {"4", 1 / 16},
                        {"5.6", 1 / 32},
                        {"8", 1 / 64},
                        {"11", 1 / 128},
                        {"16", 1 / 256},
                        {"22", 1 / 512},
                        {"32", 1 / 1024},
                        {?__(47, "pinhole"), 0.0}]],
    {ApertureIdx,_} = f_stop_find(f_stop_str(Aperture),ApertureList),
    ImageFormats = images_format(),
    BrowseProps = [{dialog_type,open_dialog}, {extensions,ImageFormats}],
    FontProps = [{dialog_type,open_dialog}, {extensions,[{".ttf","True Type Fonts"}]}],

    %% this fix wrong setup in old project files
    case get_pref(save_alpha,Attr) of
        false ->
            set_prefs([{background_transp,false},{background_transp_refract,false}]);
        true ->
            set_prefs([{background_transp,false}]);
        _ -> ignore
    end,

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            threads_auto ->
                wings_dialog:enable(?KEY(pnl_threads), Value =:= false, Store);
            use_caustics ->
                wings_dialog:enable(?KEY(pnl_dl1), Value =:= true, Store);
            do_ao ->
                wings_dialog:enable(?KEY(pnl_use_ao), Value =:= true, Store);
            pm_use_fg ->
                wings_dialog:enable(?KEY(pnl_use_fg), Value =:= true, Store);
            transparent_shadows ->
                wings_dialog:enable(?KEY(pnl_transp_shadow), Value =:= true, Store);
            render_format ->
                wings_dialog:enable(?KEY(pnl_exr_option), Value =:= exr, Store);
            aperture ->
                {Value0,_} = f_stop_find(Value,ApertureList),
                wings_dialog:set_value(aperture_idx, Value0, Store),
                wings_dialog:enable(bokeh_use_QMC, Value =/= +0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_type), Value =/= +0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_sliders), Value =/= +0.0, Store);
            aperture_idx ->
                if ((Value =/= "") and (Value =/= Custom)) ->
                        {_,Value0} = f_stop_find(Value,ApertureList),
                        wings_dialog:set_value(aperture, Value0, Store);
                    true -> ok
                end,
                Enabled = wings_dialog:get_value(aperture, Store) =/= +0.0,
                wings_dialog:enable(bokeh_use_QMC, Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_type), Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_sliders), Enabled, Store);
            _ -> ok
        end
    end,
    Hook_Show = fun(Key, Value, Store) ->
        case Key of
            lighting_method ->
                %% 1st column of panels
                %% Direct Light
                wings_dialog:enable(?KEY(pnl_dl1), wings_dialog:get_value(use_caustics, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_caustics), Value =:= directlighting, Store),
                %% Photon Mapping
                wings_dialog:show(?KEY(pnl_pm1), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt1), Value =:= pathtracing, Store),
                %% SPPM - GI
                wings_dialog:show(?KEY(pnl_sppm1), Value =:= sppm, Store),

                %% 2nd column of panels
                %% Direct Light
                wings_dialog:enable(?KEY(pnl_use_ao), wings_dialog:get_value(do_ao, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_ao), Value =:= directlighting, Store),
                %% Photon Mapping
                wings_dialog:show(?KEY(pnl_pm2), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt2), Value =:= pathtracing, Store),
                %% SPPM - GI
                wings_dialog:show(?KEY(pnl_sppm2), Value =:= sppm, Store),

                %% 3rd column of panels
                %% Photon Mapping
                wings_dialog:enable(?KEY(pnl_use_fg), wings_dialog:get_value(pm_use_fg, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_pm3), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt3), Value =:= pathtracing, Store),

                wings_dialog:update(?KEY(pnl_light), Store);
            volintegr_type ->
                wings_dialog:enable(?KEY(pnl_volumetric), Value =:= singlescatterintegrator, Store);
            lens_type ->
                wings_dialog:show(?KEY(pnl_lens_scale), Value =:= orthographic, Store),
                wings_dialog:show(?KEY(pnl_lens_angle), Value =:= angular, Store),
                wings_dialog:update(?KEY(pnl_camera), Store);
            shadow_bias_auto ->
                wings_dialog:show(?KEY(pnl_shadow_bias), Value =:= false, Store);
            ray_mindist_auto ->
                wings_dialog:show(?KEY(pnl_ray_mindist), Value =:= false, Store);
            img_autosave_type ->
                wings_dialog:show(?KEY(pnl_img_autosave_seconds), Value =:= 'time-interval', Store),
                wings_dialog:show(?KEY(pnl_img_autosave_passes), Value =:= 'pass-interval', Store);
            img_denoise_enable ->
                wings_dialog:show(?KEY(pnl_img_denoise), Value =:= 'true', Store);
            film_processing ->
                wings_dialog:show(?KEY(pnl_film_binary_format), Value =/= none, Store),
                wings_dialog:show(?KEY(pnl_film_save_options), Value =/= none, Store);
            film_autosave_type ->
                wings_dialog:show(?KEY(pnl_film_autosave_seconds), Value =:= 'time-interval', Store),
                wings_dialog:show(?KEY(pnl_film_autosave_passes), Value =:= 'pass-interval', Store);
            _ -> ok
        end
    end,

    Check_bkground = fun(Key, Value, Store) ->
        case Key of
            save_alpha ->
                case Value of
                    false ->
                        wings_dialog:set_value(background_transp, false, Store),
                        wings_dialog:set_value(background_transp_refract, false, Store);
                    _ ->
                        wings_dialog:set_value(background_transp, true, Store)
                end,
                wings_dialog:enable(background_transp_refract, Value=/=false, Store);
            background_transp ->
                wings_dialog:enable(background_transp, false, Store)
        end
    end,
%%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%     ButtonsHook = fun(Key,_,_Store) ->
%%         io:format("Button ~p pressed...\n",[Key])
%%     end,

    GeneralOpt =
        {?__(0, "General options"),
            {vframe, [
                %% Pre-Render group
                {vframe, [
                    {hframe, [
                        {label, ?__(1, "Subdivisions")++" "},
                        {slider, {text, get_pref(subdivisions,Attr),[{key,subdivisions}, range(subdivisions)]}}
                    ]},
                    {hframe, [
                        case Op of
                            render ->
                                {hframe,[
                                    {?__(2, "Write .xml file"), get_pref(keep_xml,Attr),[{key,keep_xml}]},
                                    panel
                                ]};
                            _ ->
                                {value, get_pref(keep_xml,Attr), [{key,keep_xml}]}
                        end,
                        {hframe, [
                            {label, "Threads"++" "},
                            {text, get_pref(threads_number,Attr), [range(threads_number),{key,threads_number}]}
                        ],[key(pnl_threads)]},
                        {hframe,[
                            {?__(160, "Auto"), get_pref(threads_auto,Attr),
                                                    [{key,threads_auto},{hook,Hook_Enable}]}
                        ]}
                    ],[{margin,false}]}
                ],[{title, ?__(3, "Pre-rendering")},{margin,true}]},

                %% Render group
                {hframe, [
                    {label_column, [
                        {?__(4, "Ray depth"),{text, get_pref(raydepth,Attr), [range(raydepth),{key,raydepth}]}},
                        {?__(5, "Gamma"),{text, get_pref(gamma,Attr), [range(gamma),{key,gamma}]}}
                    ]},
                    {vframe, [
                        {vframe, [
                            {menu, [
                                {?__(133, "Transp Shadows Off"), false},
                                {?__(134, "Transp Shadows On"), true}
                            ], get_pref(transparent_shadows,Attr), [{key,transparent_shadows},{hook,Hook_Enable}]}
                        ]},
                        {hframe, [
                            {label, ?__(135, "Shadow depth")},
                            {text, get_pref(shadow_depth,Attr), [range(shadow_depth),{key,shadow_depth}]}
                        ], [key(pnl_transp_shadow), {enabled,false},{margin,false}]}
                    ]},
                    {vframe, [
                        {hframe, [
                            {label, ?__(216, "Tile size ")},
                            {text, get_pref(tile_size,Attr), [range(tile_size),{key,tile_size}]},
                            {menu, [
                                {?__(217, "Centre"), centre},
                                {?__(218, "Random"), random},
                                {?__(219, "Linear"), linear}
                            ], get_pref(tile_order,Attr), [{key,tile_order}]}
                        ], [{margin,false}]}
                    ]}
                ],[{title, ?__(8, "Render")},{margin,true}]},

                %% Output group
                {hframe, [
		    {label_column, [
			{?__(13, "Output")++" ", {
			    menu, [
				{Ext ++ " (" ++ Desc ++ ")", Format}
				|| {Format, Ext, Desc} <- wings_job:render_formats(),
				(Format == tga) or (Format == tif) or (Format == png) or
				(Format == hdr) or (Format == exr)
			    ], get_pref(render_format,Attr), [{key,render_format},{hook,Hook_Enable}]}
			}]},
                    panel,
                    {hframe, [
                        {?__(9, "Float"), get_pref(exr_flag_float,Attr), [{key,exr_flag_float}]},
                        panel,
                        {?__(10, "Zbuf"), get_pref(exr_flag_zbuf,Attr), [{key,exr_flag_zbuf}]},
                        panel,
                        {label, ?__(11, "Compression")++" "},
                        {menu, [
                            {?__(12, "none"), compression_none},
                            {"piz", compression_piz},
                            {"rle", compression_rle},
                            {"pxr24", compression_pxr24},
                            {"zip", compression_zip}
                        ], get_pref(exr_flag_compression,Attr), [{key,exr_flag_compression}]}
                    ], [key(pnl_exr_option),{enabled,false},{margin,false}]}
                ],[{margin,false}]},

                {hframe, [
                    {label, ?__(21, "Color")++" "},
                    {color, get_pref(background_color,Attr), [{key,background_color}]},
                    panel,
                    {label, ?__(22, "Alpha Channel")++" "},
                    {menu, [
                        {?__(23, "Off"), false},
                        {?__(61, "On"), true},
                        {?__(24, "Premultiply"), premultiply}
                    ], get_pref(save_alpha,Attr), [{key,save_alpha},{hook,Check_bkground}]},
                    panel,
                    {?__(161, "Transp Background"),get_pref(background_transp,Attr),
                        [{key,background_transp},{hook,Check_bkground}]},
                    panel,
                    {?__(159, "Transp Refraction"),get_pref(background_transp_refract,Attr),
                     [{key,background_transp_refract}]}
                ],[{title, ?__(26, "Background")},{margin,true}]},

                %% Image Autosaving group
                {hframe, [
                    {label, ?__(221, "Autosave interval")++" "},
                    {menu, [
                        {"Passes interval", 'pass-interval'},
                        {"Time interval", 'time-interval'},
                        {"Disabled", none}
                    ], get_pref(img_autosave_type,Attr), [{key,img_autosave_type},{hook,Hook_Show}]},
                    {hframe, [
                        panel,
                        {label, ?__(222, "Interval (s)")}, {text, get_pref(img_autosave_seconds,Attr), [range(img_autosave_seconds),{key,img_autosave_seconds}]}
                    ],[key(pnl_img_autosave_seconds),{margin,true}]},
                    {hframe, [
                        panel,
                        {label, ?__(223, "Interval (passes)")}, {text, get_pref(img_autosave_passes,Attr), [range(img_autosave_passes),{key,img_autosave_passes}]}
                    ],[key(pnl_img_autosave_passes),{margin,true}]}
                ],[{title, ?__(220, "Output Image AutoSave options. Too small intervals can cause slowdowns/hangs")},{margin,true}]},

                %% Image Denoise group
                {hframe, [
                    {?__(232, "Image Output DeNoise"),get_pref(img_denoise_enable,Attr), [{key,img_denoise_enable},{hook,Hook_Show}]},
                    {hframe, [
                        panel,
                        {label, ?__(233, "Denoise Mix")++" "}, {text, get_pref(img_denoise_mix,Attr), [range(img_denoise_mix),{key,img_denoise_mix}]},
                        panel,
                        {label, ?__(234, "Denoise hLum")++" "}, {text, get_pref(img_denoise_h_lum,Attr), [range(img_denoise_h_lum),{key,img_denoise_h_lum}]},
                        panel,
                        {label, ?__(235, "Denoise hChrom")++" "}, {text, get_pref(img_denoise_h_chrom,Attr), [range(img_denoise_h_chrom),{key,img_denoise_h_chrom}]}
                    ],[key(pnl_img_denoise),{margin,true}]}
                ],[{title, ?__(231, "Output Image de-noise options")},{margin,true}]},


                %% Internal Film Processing/Autosaving group
                {vframe, [
                    {hframe, [
                        {label, ?__(226, "Film processing type")++" "},
                            {menu, [
                                {"Load and Save", 'load-save'},
                                {"Save", save},
                                {"Disabled", none}
                            ], get_pref(film_processing,Attr), [{key,film_processing},{hook,Hook_Show}]},
                        {hframe, [
                            {?__(227, "Film binary format"),get_pref(film_binary_format,Attr), [{key,film_binary_format}]}
                        ],[key(pnl_film_binary_format),{margin,true}]}
                    ]},
                    {hframe, [
                        {label, ?__(228, "Film Autosave interval")++" "},
                        {menu, [
                            {"Passes interval", 'pass-interval'},
                            {"Time interval", 'time-interval'},
                            {"Disabled", none}
                        ], get_pref(film_autosave_type,Attr), [{key,film_autosave_type},{hook,Hook_Show}]},
                        {hframe, [
                            panel,
                            {label, ?__(229, "Interval (s)")}, {text, get_pref(film_autosave_seconds,Attr), [range(film_autosave_seconds),{key,film_autosave_seconds}]}
                        ],[key(pnl_film_autosave_seconds),{margin,true}]},
                        {hframe, [
                            panel,
                            {label, ?__(230, "Interval (passes)")}, {text, get_pref(film_autosave_passes,Attr), [range(film_autosave_passes),{key,film_autosave_passes}]}
                        ],[key(pnl_film_autosave_passes),{margin,true}]}
                    ],[key(pnl_film_save_options),{margin,true}]}
                ],[{title, ?__(224, "Internal Film Processing/AutoSave options. Too small intervals can cause slowdowns/hangs")},{margin,true}]}

%%                 % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%                 {hframe, [
%%                     {button, ?__(55, "Save"), save, [{info, ?__(56, "Save to user preferences")},{hook,ButtonsHook}]},
%%                     {button, ?__(57, "Load"), load, [{info, ?__(58, "Load from user preferences")},{hook,ButtonsHook}]},
%%                     {button, ?__(59, "Reset"), reset, [{info, ?__(60, "Reset to default values")},{hook,ButtonsHook}]}
%%                 ]}
            ]}
        },

    %% AA and Noise Control group
    Noise_AA_Control =
        {?__(189, "Noise/AA Control"),
            {vframe, [
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label, ?__(165, "Method ")},
                            {menu, [
                                {?__(136, "Box Filter"), box},
                                {?__(137, "Gaussian Filter"), gauss},
                                {?__(138, "Mitchell-Netravali Filter"), mitchell},
                                {?__(139, "Lanczos Filter"), lanczos}
                            ], get_pref(aa_filter_type,Attr), [{key,aa_filter_type}]}
                        ],[{margin,true}]},
                        {hframe, [
                            {label_column, [
                                {?__(15, "Min Samples "),{text, get_pref(aa_minsamples,Attr),
                                                        [range(aa_minsamples),{key,aa_minsamples}]}},
                                {?__(14, "AA Passes "),{text, get_pref(aa_passes,Attr),
                                                        [range(aa_passes),{key,aa_passes}]}},
                                {?__(190, "Additional Samples "),{text, get_pref(aa_incsamples,Attr),
                                                        [range(aa_incsamples),{key,aa_incsamples}]}}
                            ]},
                            {label_column, [
                                {?__(17, "AA Threshold "),{text, get_pref(aa_threshold,Attr),
                                    [range(aa_threshold),{key,aa_threshold}]}},
                                {?__(18, "Pixelwidth "),{text, get_pref(aa_pixelwidth,Attr),
                                    [range(aa_pixelwidth),{key,aa_pixelwidth}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{title, ?__(191, "Basic controls")},{margin,true}]},
                    {vframe, [
			{label_column, [
			    {?__(193, "AA Dark thresh. compensation "),{
				menu, [
				    {?__(194, "None"), none},
				    {?__(195, "Linear"), linear},
				    {?__(196, "Curve"), curve}
				], get_pref(aa_noise_dark_detection_type,Attr), [{key,aa_noise_dark_detection_type}]}},
			    {?__(197, "AA Dark thr.factor "),{text, get_pref(aa_noise_dark_threshold_factor,Attr),
						    [range(aa_noise_dark_threshold_factor),{key,aa_noise_dark_threshold_factor}]}},
			    {?__(198, "AA Resampled Floor (% image) "),{text, get_pref(aa_noise_resampled_floor,Attr),
						    [range(aa_noise_resampled_floor),{key,aa_noise_resampled_floor}]}}
			]},
		    	{hframe, [
			    {?__(199, "Detect Color Noise "), get_pref(aa_noise_detect_color_noise,Attr), [{key,aa_noise_detect_color_noise}]}
                        ],[{margin,true}]}
                    ],[{title, ?__(192, "Additional controls")},{margin,true}]}
                ],[{margin,false}]},
                
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label_column, [
                                {?__(200, "Variance Window "),{text, get_pref(aa_noise_variance_edge_size,Attr), [range(aa_noise_variance_edge_size),{key,aa_noise_variance_edge_size}]}},
                                {?__(201, "Variance Pixels "),{text, get_pref(aa_noise_variance_pixels,Attr), [range(aa_noise_variance_pixels),{key,aa_noise_variance_pixels}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{title, ?__(202, "Optional Variance noise detection (pixels = 0 disables it)")},{margin,true}]},
                    {vframe, [
                        {hframe, [
                            {label_column, [
                                {?__(203, "Clamp all samples "),{text, get_pref(aa_noise_clamp_samples,Attr), [range(aa_noise_clamp_samples),{key,aa_noise_clamp_samples}]}},
                                {?__(204, "Clamp indirect samples "),{text, get_pref(aa_noise_clamp_indirect,Attr), [range(aa_noise_clamp_indirect),{key,aa_noise_clamp_indirect}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{title, ?__(205, "Clamping for noise reduction (0.0 disables clamp)")},{margin,true}]}
                ],[{margin,false}]},
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label_column, [
                                {?__(206, "Samples multiplier factor "),{text, get_pref(aa_noise_sample_multiplier_factor,Attr), [range(aa_noise_sample_multiplier_factor),{key,aa_noise_sample_multiplier_factor}]}},
                                {?__(207, "Light samples multiplier factor "),{text, get_pref(aa_noise_light_sample_multiplier_factor,Attr), [range(aa_noise_light_sample_multiplier_factor),{key,aa_noise_light_sample_multiplier_factor}]}},
                                {?__(208, "Indirect samples multiplier factor "),{text, get_pref(aa_noise_indirect_sample_multiplier_factor,Attr), [range(aa_noise_indirect_sample_multiplier_factor),{key,aa_noise_indirect_sample_multiplier_factor}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{title, ?__(209, "Sampling factor Pass multipliers")},{margin,true}]}
                ],[{margin,false}]}
            ]}
        },

    %% Lighting group
    Lighting =
        {?__(113, "Lighting"),
            {vframe, [
                {vframe, [
                    {hframe,[
                        {menu, [
                            {?__(114, "Direct Light"), directlighting},
                            {?__(115, "Photon Mapping - Global Illumination"), photonmapping},
                            {?__(140, "Path Tracing - Global Illumination"), pathtracing},
                            {?__(116, "Bidirectional Path Tracing - Global Illumination"), bidirectional},
                            {?__(157, "SPPM - Global Illumination"),sppm}
                        ], get_pref(lighting_method,Attr), [{key,lighting_method}, {hook,Hook_Show}]}
                    ]},
                    %% Start Direct Lighting Menu Section
                    {hframe,[
                        %% 1st collunm of panels
                        {vframe, [
                            {hframe,[
                                {?__(82, "Caustics"), get_pref(use_caustics,Attr), [{key,use_caustics},{hook,Hook_Enable},{show,false}]}
                            ]},
                            {label_column, [
                                {?__(84, "Photons"),{text, get_pref(caustic_photons,Attr), [range(caustic_photons),{key,caustic_photons}]}},
                                {?__(85, "Depth"),{text, get_pref(caustic_depth,Attr), [range(caustic_depth),{key,caustic_depth}]}},
                                {?__(86, "Mix"),{text, get_pref(caustic_mix,Attr), [range(caustic_mix),{key,caustic_mix}]}},
                                {?__(87, "Radius"),{text, get_pref(caustic_radius,Attr), [range(caustic_radius),{key,caustic_radius}]}}
                            ], [key(pnl_dl1)]}
                        ], [key(pnl_caustics),{show,false},{magin,false}]},
                        {label_column, [
                            {?__( 84, "Photons"),{text, get_pref(pm_diffuse_photons,Attr), [range(pm_diffuse_photons),{key,pm_diffuse_photons}]}},
                            {?__(122, "Bounces"),{text, get_pref(pm_bounces,Attr), [range(pm_bounces),{key,pm_bounces}]}},
                            {?__(123, "Search"),{text, get_pref(pm_search,Attr), [range(pm_search),{key,pm_search}]}},
                            {?__(124, "Diffuse Radius"),{text, get_pref(pm_diffuse_radius,Attr), [range(pm_diffuse_radius),{key,pm_diffuse_radius}]}}
                        ], [key(pnl_pm1)]},
                        {label_column, [
                            {?__( 84, "Photons"),{text, get_pref(pt_diffuse_photons,Attr), [range(pt_diffuse_photons),{key,pt_diffuse_photons}]}},
                            {?__(122, "Bounces"),{text, get_pref(pt_bounces,Attr), [range(pt_bounces),{key,pt_bounces}]}}
                        ], [key(pnl_pt1),{show,false}]},
                        {label_column, [
                            {?__( 84, "Photons"),{text,get_pref(sppm_photons,Attr),[range(sppm_photons),{key,sppm_photons}]}},
                            {?__(122, "Bounces"),{text,get_pref(sppm_bounces,Attr),[range(sppm_bounces),{key,sppm_bounces}]}},
                            {?__(123, "Search"),{text,get_pref(sppm_search,Attr),[range(sppm_search),{key,sppm_search}]}},
                            {?__( 87, "Radius"),{text,get_pref(sppm_radius,Attr),[range(sppm_radius),{key,sppm_radius}]}}
                        ], [key(pnl_sppm1),{show,false}]},

                        %% 2nd column of panels
%                        {vframe, [
                            {vframe, [
                                {hframe, [
                                    {?__(95, "Ambient Occlusion"), get_pref(do_ao,Attr), [{key,do_ao},{hook,Hook_Enable}]}
                                ]},
                                {label_column, [
                                    {?__(97, "AO Distance"),{text, get_pref(ao_distance,Attr), [range(ao_distance),{key,ao_distance}]}},
                                    {?__(98, "AO Samples"),{text, get_pref(ao_samples,Attr), [range(ao_samples),{key,ao_samples}]}},
                                    {?__(99, "AO Color"),{color, get_pref(ao_color,Attr), [{key,ao_color}]}}
                                ], [key(pnl_use_ao)]}
                            ], [key(pnl_ao),{show,false},{magin,false}]},
                            {label_column, [
                                {?__(125, "Caustic Photons"),{text, get_pref(pm_caustic_photons,Attr), [range(pm_caustic_photons),{key,pm_caustic_photons}]}},
                                {?__(126, "Caustic Radius"),{text, get_pref(pm_caustic_radius,Attr), [range(pm_caustic_radius),{key,pm_caustic_radius}]}},
                                {?__(127, "Caustic Mix"),{text, get_pref(pm_caustic_mix,Attr), [range(pm_caustic_mix),{key,pm_caustic_mix}]}}
                            ],[key(pnl_pm2)]},
                            {vframe,[
                                {hframe,[
                                    {label, ?__(145, "Caustic Type")++" "},
                                    {menu, [
                                        {?__(153, "path"), path},
                                        {?__(154, "photons"), photons},
                                        {?__(155, "both"), both},
                                        {?__(156, "none"), none}
                                    ], get_pref(pt_caustic_type,Attr), [{key,pt_caustic_type}]}
                                ]},
                                {label_column, [
                                    {?__(126, "Caustic Radius"),{text, get_pref(pt_caustic_radius,Attr), [{key,pt_caustic_radius},range(pt_caustic_radius)]}},
                                    {?__(127, "Caustic Mix"),{text, get_pref(pt_caustic_mix,Attr), [{key,pt_caustic_mix},range(pt_caustic_mix)]}},
                                    {?__(148, "Caustic Depth"),{text, get_pref(pt_caustic_depth,Attr), [{key,pt_caustic_depth},range(pt_caustic_depth)]}}
                                ],[{margin,false}]}
                            ],[key(pnl_pt2),{show,false}]},
                            {vframe,[
                                {label_column, [
                                    {?__(146, "Times"),{text,get_pref(sppm_times,Attr),[{key,sppm_times},range(sppm_times)]}},
                                    {?__(147, "Passes"),{text,get_pref(sppm_passes,Attr),[{key,sppm_passes}, range(sppm_passes)]}}
                                ]},
                                {hframe,[
                                    {?__(166,"IRE"),get_pref(sppm_ire,Attr),[{key,sppm_ire}]}
                                ]}
                            ],[key(pnl_sppm2),{show,false},{margin,false}]},
%                        ], [{margin,false}]},

                        %% 3rd column of panels
                        {vframe, [
                            {vframe, [
                                {vframe,[
                                    {?__(128, "Final Gather"), get_pref(pm_use_fg,Attr), [{key,pm_use_fg},{hook,Hook_Enable}]}
                                ],[{border,1}]},
                                {vframe, [
                                    {label_column, [
                                        {?__(130, "FG Bounces"),{text, get_pref(pm_fg_bounces,Attr), [{key,pm_fg_bounces},range(pm_fg_bounces)]}},
                                        {?__(131, "FG Samples"),{text, get_pref(pm_fg_samples,Attr), [{key,pm_fg_samples},range(pm_fg_samples)]}}
                                    ],[{margin,false}]},
                                    {vframe,[
                                        {?__(132, "Show Map"), get_pref(pm_fg_show_map,Attr), [{key,pm_fg_show_map}]}
                                    ],[{border,1}]}
                                ], [key(pnl_use_fg),[{margin,false}]]}
                            ], [key(pnl_pm3)]},
                            {label_column, [
                                {?__(152, "Path Samples"),{text, get_pref(pt_samples,Attr), [{key,pt_samples},range(pt_samples)]}}
                            ], [key(pnl_pt3)]}
                        ], [{margin,false}]}
                    ],[key(pnl_light)]}
                ],[{title, ""}]},

                %% Volumetrics group
                {hframe, [
                    {menu, [
                        {?__(89, "None"), none},
                        {?__(90, "SingleScatter"), singlescatterintegrator}
                    ], get_pref(volintegr_type,Attr), [{key,volintegr_type},{hook,Hook_Show}]},
                    panel,
                    {hframe, [
                        {?__(91, "Adaptive"), get_pref(volintegr_adaptive,Attr), [{key,volintegr_adaptive}]},
                        panel,
                        {label, ?__(93, "StepSize")},
                        panel,
                        {text, get_pref(volintegr_stepsize,Attr), [{key,volintegr_stepsize},range(volintegr_stepsize)]},
                        panel,
                        {?__(92, "Optimize"), get_pref(volintegr_optimize,Attr),[{key,volintegr_optimize}]}
                    ], [key(pnl_volumetric)]}
                ],[{title, ?__(88, "Volumetrics")}]}
            ],[{margin,true}]}
        },


    Camera =
        {?__(51, "Camera"),
            {vframe, [
                {vframe, [
                    {menu, [
                        {?__(102, "Perspective"), perspective},
                        {?__(103, "Orthographic"), orthographic},
                        {?__(104, "Architect"), architect},
                        {?__(105, "Fish Eye (Angular)"), angular}
                    ], get_pref(lens_type,Attr), [{key,lens_type},{hook,Hook_Show}]},
                    {hframe, [
                        {hframe, [
                            {label, ?__(33, "Width")++" "},
                            {text, get_pref(width,Attr), [range(pixels),{key,width}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(44, "Height")++" "},
                            {text, get_pref(height,Attr), [range(pixels),{key,height}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(108, "Scale")++" "},
                            {text, get_pref(lens_ortho_scale,Attr), [range(lens_ortho_scale),{key,lens_ortho_scale}]}
                        ],[key(pnl_lens_scale)]}
                    ],[{margin,false}]},
                    {vframe, [
                        {hframe, [
                            {?__(109, "Circular"), get_pref(lens_angular_circular,Attr),
                                        [{key,lens_angular_circular},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(111, "Circle/Max Angle"), {text, get_pref(lens_angular_max_angle,Attr),
                                            [range(lens_angular_max_angle),{key,lens_angular_max_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]},
                        {hframe, [
                            {?__(110, "Mirrored"), get_pref(lens_angular_mirrored,Attr),
                                        [{key,lens_angular_mirrored},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(112, "Frame/Angle"), {text, get_pref(lens_angular_angle,Attr),
                                            [range(lens_angular_angle),{key,lens_angular_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]}
                    ],[key(pnl_lens_angle),{margin,false}]},

                    {hframe, [
                        {hframe, [
                            {label, ?__(34, "Aperture")++" "},
                            {text, Aperture, [range(aperture),{key,aperture},{hook,Hook_Enable}]}
                        ]},
                        panel,
                        {label, ?__(45, "f-stop")++" "},
                        {menu, [{F,A} || {F,A,_} <- ApertureList]++[{"Custom",Custom}],
                            ApertureIdx, [{key,aperture_idx},{hook,Hook_Enable}]},
                        panel,
                        {?__(32, "Use QMC"), get_pref(bokeh_use_QMC,Attr),[{key,bokeh_use_QMC}]}
                    ],[{margin,false}]},

                    {hframe, [
                        {label, ?__(35, "DOF Type")++" "},
                        {menu, [
                            {?__(37, "Disk1"), disk1}, {?__(38, "Disk2"), disk2},
                            {?__(39, "Triangle"), triangle},
                            {?__(40, "Square"), square}, {?__(41, "Pentagon"), pentagon},
                            {?__(42, "Hexagon"), hexagon}, {?__(43, "Ring"), ring}
                        ], get_pref(bokeh_type,Attr), [{key,bokeh_type}]},
                        panel,
                        {label, ?__(46, "Bias")++" "},
                        {menu, [
                            {?__(48, "Uniform"), uniform},
                            {?__(49, "Center"), center},
                            {?__(50, "Edge"), edge}
                        ], get_pref(bokeh_bias,Attr), [{key,bokeh_bias}]}
                    ],[key(pnl_dof_type)]},
                    {vframe, [
                        {hframe, [
                            {label, ?__(36, "DOF Rotation")++" "},
                            {slider, {text, get_pref(bokeh_rotation,Attr), [range(bokeh_rotation),{key,bokeh_rotation}]}}
                        ]},
                        {hframe, [
                            {label, ?__(100, "DOF Distance")++" "},
                            {slider, {text, get_pref(dof_distance,Attr), [range(dof_distance),{key,dof_distance}]}}
                        ]}
                    ],[key(pnl_dof_sliders),{margin,false}]}
                ],[key(pnl_camera)]}
            ],[{title,""}, {margin,true}]}
        },

    Logging_Badge =
        {?__(168, "Logging/Badge"),
            {vframe, [
                {hframe, [
                    {label, ?__(170, "Badge position ")},
                    {menu, [
                        {?__(171, "Top"), top},
                        {?__(172, "Bottom"), bottom},
                        {?__(173, "None"), none}
                    ], get_pref(badge_position,Attr), [{key,badge_position}]},
                    panel,
                    {?__(174, "Draw Render Settings "), get_pref(badge_draw_render_settings,Attr),[{key,badge_draw_render_settings}]},
                    panel,
                    {?__(175, "Draw AA/Noise Settings "), get_pref(badge_draw_aa_noise_settings,Attr),[{key,badge_draw_aa_noise_settings}]}
                ]},
                {hframe, [
                    {label, ?__(180, "Console Verbosity ")},
                    {menu, verbosity_menu(), get_pref(log_verbosity_console,Attr), [{key,log_verbosity_console}]},
                    panel,
                    {label, ?__(181, "Log TXT/HTML Verbosity ")},
                    {menu, verbosity_menu(), get_pref(log_verbosity_txt_html,Attr), [{key,log_verbosity_txt_html}]}
                ]},
                {hframe, [
                    {?__(182, "Save TXT log "), get_pref(log_save_txt,Attr), [{key,log_save_txt}]},
                    panel,
                    {?__(183, "Save HTML log "), get_pref(log_save_html,Attr), [{key,log_save_html}]}
                ]},
                panel,
                {label_column, [
                    {?__(184, "Title "), {text, get_pref(badge_title,Attr), [{key,badge_title}]}},
                    {?__(185, "Author "), {text, get_pref(badge_author,Attr), [{key,badge_author}]}},
                    {?__(186, "Contact info "), {text, get_pref(badge_contact,Attr), [{key,badge_contact}]}},
                    {?__(187, "Comments "), {text, get_pref(badge_comments,Attr), [{key,badge_comments}]}},
                    {?__(188, "Custom icon path "), {button,{text, get_pref(badge_custom_icon_path,Attr),
                                                             [{key,badge_custom_icon_path}, {props,BrowseProps}]}}},
                    {?__(240, "Font size factor"), {text, get_pref(badge_font_size_factor,Attr), [range(badge_font_size_factor), {key,badge_font_size_factor}]}},
                    {?__(241, "Custom font path "), {button,{text, get_pref(badge_font_path,Attr),
                                                             [{key,badge_font_path}, {props,FontProps}]}}}
                ]}
            ],[{title,""}, {margin,true}]}
        },

    RenderPasses = build_render_pass_frame(?__(167, "Render Passes"), Attr),

    Advanced =
        {?__(213, "Advanced"),
            {vframe, [
                {hframe, [
                    {?__(210, "Automatic Shadow Bias"), get_pref(shadow_bias_auto,Attr), [{key,shadow_bias_auto},{hook,Hook_Show}]},
                    {hframe, [
			panel,
                        {label, ?__(6, "Shadow Bias")}, {text, get_pref(shadow_bias,Attr), [range(shadow_bias),{key,shadow_bias}]}
                    ],[key(pnl_shadow_bias),{margin,false}]}
                ]},
                {hframe, [
                    {?__(211, "Automatic Min Ray Distance"), get_pref(ray_mindist_auto,Attr), [{key,ray_mindist_auto},{hook,Hook_Show}]},
                    {hframe, [
			panel,
                        {label, ?__(212, "Min Ray Distance")}, {text, get_pref(ray_mindist,Attr), [range(ray_mindist),{key,ray_mindist}]}
                    ],[key(pnl_ray_mindist),{margin,false}]}
                ]}
            ]}
        },

    [
        {oframe, [
            GeneralOpt,
            Noise_AA_Control,
            Lighting,
            Camera,
            Logging_Badge] ++
            RenderPasses ++ [
            Advanced
        ], 1, [{style, buttons}]}
    ].

verbosity_menu() ->
    [
        {?__(1,"Debug"),'debug'},
        {?__(2,"Verbose"),'verbose'},
        {?__(3,"Info"),'info'},
        {?__(4,"Params"),'params'},
        {?__(5,"Warning"),'warning'},
        {?__(6,"Error"),'error'},
        {?__(7,"Mute (silent)"),'mute'}
    ].

render_pass_id(N) when is_integer(N) ->
    list_to_atom("render_pass"++integer_to_list(N)).

build_render_pass_frame(Label, Attr) ->
    [{render_passes_enable,PassesEnable},
     {render_passes,MaxRenderPasses}] = get_user_prefs([{render_passes_enable,?DEF_RENDER_PASSES_ENABLE},
                                                        {render_passes,?DEF_RENDER_PASSES}]),

    {_,Max} = range_1(render_passes),
    UsedPasses =
        lists:foldl(fun(N, Acc) ->
            Id = render_pass_id(N),
            case get_pref(Id, Attr) of
                ?DEF_RENDER_PASS -> Acc;
                _ -> N
            end
        end,0, lists:seq(1,Max)),
    Passes =
        case {PassesEnable,(UsedPasses =/= 0)} of
            {false,true} -> UsedPasses;
            {true,true} -> max(MaxRenderPasses, UsedPasses);
            {true,false} -> MaxRenderPasses;
            _ -> 0
        end,
    if (Passes =/= 0) ->
        [{Label, {hframe, render_pass_frame(Attr,Passes),[{title,""}]}}];
    true -> []
    end.

render_pass_frame(Attr, Passes) ->
    ItemCol = max(1,Passes/2),
    PassLabel = ?__(1,"Pass"),
    {Col1,Col2} =
        lists:foldl(fun(N, {Acc1, Acc2}) ->
            Id = render_pass_id(N),
            Value = get_pref(Id, Attr),
            if N=<ItemCol ->
                {Acc1++[{PassLabel ++ io_lib:format("~w", [N]),
                    {menu, render_pass_menu(), Value ,[{key,Id}]}}], Acc2};
            true ->
                {Acc1, Acc2++[{PassLabel ++ io_lib:format("~w", [N]),
                    {menu, render_pass_menu(), Value ,[{key,Id}]}}]}
            end
        end, {[],[]}, lists:seq(1,Passes)),
    [{vframe, [{label_column, Col1}]}, {vframe, [{label_column, Col2}]}].

render_pass_menu() ->
    [
        {?__(1,"Disabled"),'disabled'},
        {?__(2,"Basic: Combined image"),'combined'},
        {?__(3,"Basic: Diffuse"),'diffuse'},
        {?__(4,"Basic: Diffuse (no shadows)"),'diffuse-noshadow'},
        {?__(5,"Basic: Shadow"),'shadow'},
        {?__(6,"Basic: Environment"),'env'},
        {?__(7,"Basic: Indirect"),'indirect'},
        {?__(8,"Basic: Emit"),'emit'},
        {?__(9,"Basic: Reflection"),'reflect'},
        {?__(10,"Basic: Refraction"),'refract'},
        {?__(11,"Basic: Mist"),'mist'},
        {?__(12,"Z-Depth (absolute)"),'z-depth-abs'},
        {?__(13,"Z-Depth (normalized)"),'z-depth-norm'},
        {?__(14,"Ambient Occlusion (color)"),'ao'},
        {?__(15,"Ambient Occlusion (clay)"),'ao-clay'},
        {?__(16,"Index-Object (auto)"),'obj-index-auto'},
        {?__(17,"Index-Material (auto)"),'mat-index-auto'},
        {?__(18,"Index-Material (absolute)"),'mat-index-abs'},
        {?__(19,"Index-Material (normalized)"),'mat-index-norm'},
        {?__(21,"Index-Material Mask"),'mat-index-mask'},
        {?__(22,"Index-Material Mask Shadow"),'mat-index-mask-shadow'},
        {?__(23,"Index-Material Mask All (Object+Shadow)"),'mat-index-mask-all'},
        {?__(24,"Adv: Reflection"),'adv-reflect'},
        {?__(25,"Adv: Refraction"),'adv-refract'},
        {?__(26,"Adv: Indirect"),'adv-indirect'},
        {?__(27,"Adv: Photon Radiance map"),'adv-radiance'},
        {?__(28,"Adv: Diffuse Indirect"),'adv-diffuse-indirect'},
        {?__(29,"Adv: Diffuse color"),'adv-diffuse-color'},
        {?__(30,"Adv: Glossy"),'adv-glossy'},
        {?__(31,"Adv: Glossy Indirect"),'adv-glossy-indirect'},
        {?__(32,"Adv: Glossy color"),'adv-glossy-color'},
        {?__(33,"Adv: Transmissive"),'adv-trans'},
        {?__(34,"Adv: Trans.Indirect"),'adv-trans-indirect'},
        {?__(35,"Adv: Trans.color"),'adv-trans-color'},
        {?__(36,"Adv: SubSurface"),'adv-subsurface'},
        {?__(37,"Adv: SubSurf.Indirect"),'adv-subsurface-indirect'},
        {?__(38,"Adv: SubSurf.color"),'adv-subsurface-color'},
        {?__(39,"Adv: Surface Integration"),'adv-surface-integration'},
        {?__(40,"Adv: Volume Integration"),'adv-volume-integration'},
        {?__(41,"Adv: Volume Transmittance"),'adv-volume-transmittance'},
        {?__(42,"Debug: AA sample count"),'debug-aa-samples'},
        {?__(43,"Debug: UV"),'debug-uv'},
        {?__(44,"Debug: dSdV"),'debug-dsdv'},
        {?__(45,"Debug: dSdU"),'debug-dsdu'},
        {?__(46,"Debug: dPdV"),'debug-dpdv'},
        {?__(47,"Debug: dPdU"),'debug-dpdu'},
        {?__(48,"Debug: NV"),'debug-nv'},
        {?__(49,"Debug: NU"),'debug-nu'},
        {?__(50,"Debug: Normals (geometric)"),'debug-normal-geom'},
        {?__(51,"Debug: Normals (smooth)"),'debug-normal-smooth'},
        {?__(52,"Debug: LE Light Dirac"),'debug-light-estimation-light-dirac'},
        {?__(53,"Debug: LE Light Sampling"),'debug-light-estimation-light-sampling'},
        {?__(54,"Debug: LE Mat Sampling"),'debug-light-estimation-mat-sampling'}
    ].

%%% TO DO: this implementation depends on wings_dialog changes for button operation
%%%
%%% Increase split_list # +1 per line if add Render Settings to Dialog
%%%
%% export_dialog_loop({Op,Fun}=Keep, Attr) ->
%%     io:format("export_dialog_loop...\n",[]),
%%     {Prefs,Buttons} = split_list(Attr, 78),
%%     case Buttons of
%%         [true,false,false] -> % Save
%%             set_user_prefs(Prefs),
%%             {dialog,
%%              export_dialog_qs(Op, Attr),
%%              export_dialog_fun(Keep)};
%%         [false,true,false] -> % Load
%%             {dialog,
%%              export_dialog_qs(Op,
%%                               get_user_prefs(export_prefs())
%%                               ++[save,load,reset]),
%%              export_dialog_fun(Keep)};
%%         [false,false,true] -> % Reset
%%             {dialog,
%%              export_dialog_qs(Op,
%%                               export_prefs()++[save,load,reset]),
%%              export_dialog_fun(Keep)};
%%         [false,false,false] -> % Ok
%%             Fun(Prefs)
%%     end.


%% Used to lookup in Store for combinations of values.
is_member(Value, Members) ->
    lists:member(Value,Members).


%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    wpa:popup_console(),
    ExportTS = os:timestamp(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    KeepXML = proplists:get_value(keep_xml, Attr, ?DEF_KEEP_XML),
    RenderFormat =
        proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    ExportDir = filename:dirname(Filename),
    {ExportFile,RenderFile} =
        case {Render,KeepXML} of
            {true,true} ->
                {filename:rootname(Filename)++".xml",
                 Filename};
            {true,false} ->
                {filename:join(ExportDir,
                               ?MODULE_STRING++"-"
                               ++wings_job:uniqstr()++".xml"),
                 Filename};
            {false,_} ->
                {value,{RenderFormat,Ext,_}} =
                    lists:keysearch(RenderFormat, 1,
                                    wings_job:render_formats()),
                {Filename,filename:rootname(Filename)++Ext}
        end,
    F = open(ExportFile, export),
    io:format(?__(1,"Exporting  to:")++" ~ts~n"++
                  ?__(2,"for render to:")++" ~ts~n", [ExportFile,RenderFile]),
    CreatorChg = re:replace(Creator,"-","_",[global]),
    CameraName = "x_Camera",
    ConstBgName = "x_ConstBackground",
    Lights = proplists:get_value(lights, Attr, []),
    %%
    uniprintln(F,  "<?xml version=\"1.0\"?>~n"++
                "<!-- ~ts: Exported from ~ts -->~n"++
                "~n"++
                "<scene type=\"triangle\">", [filename:basename(ExportFile), CreatorChg]),
    %%
    section(F, "Materials"),
    MatsGb =
        foldl(fun ({Name,Mat}, Gb) ->
                      export_shader(F, "w_"++format(Name), Mat, ExportDir),
                      println(F),
                      gb_trees:insert(Name, Mat, Gb)
              end, gb_trees:empty(), Mats),
    %%
                                                %*   MatsBlend =
    foldl(fun ({Name,Mat}, Gb) ->
                  export_shaderblend(F, "w_"++format(Name), Mat, ExportDir),
                  println(F),
                  gb_trees:insert(Name, Mat, Gb)
          end, gb_trees:empty(), Mats),


    %%

    %%Start Micheus Code for Meshlights Even Better
    section(F, "Objects"),
    foldr(fun (#e3d_object{name=Name,obj=Mesh}, Id) ->
                  export_object(F, "w_"++format(Name), Mesh, MatsGb, Id),
                  println(F),
                  Id+1
          end, 1, Objs),
    %%End Micheus Code for Meshlights Even Better


    %%
    section(F, "Lights"),
    BgLights =
        reverse(
          foldl(fun ({Name,Ps}=Light, Bgs) ->
                        Bg = export_light(F, "w_"++format(Name), Ps),
                        println(F),
                        case Bg of
                            undefined -> Bgs;
                            _ -> [Light|Bgs]
                        end
                end, [], Lights)),
    %%
    section(F, "Background, Camera, Filter and Render"),
    warn_multiple_backgrounds(BgLights),
    if BgLights =/= [] ->
            [{_,Ps0}|_] = BgLights,
            ValidBg = proplists:get_value(?TAG, Ps0, undefined) =/= undefined;
        true -> ValidBg = false
    end,

    BgName =
        case ValidBg of
            false ->
                BgColor = proplists:get_value(background_color, Attr,
                                              ?DEF_BACKGROUND_COLOR),
                Ps = [{?TAG,[{background,constant},
                             {background_color,BgColor}]}],
                export_background(F, ConstBgName, Ps),
                ConstBgName;
            _ ->
                [{Name,Ps}|_] = BgLights,
                N = "w_"++format(Name),
                export_background(F, N, Ps),
                N
        end,
    println(F),
    export_camera(F, CameraName, Attr),
    println(F),
    export_render(F, CameraName, BgName, Attr),
    %%
    println(F),
    export_logging_badge(F, Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),
    %%
    [{options,Options}] =
        get_user_prefs([{options,?DEF_OPTIONS}]),
    case {get_var(renderer),Render} of
        {_,false} ->
            wings_job:export_done(ExportTS),
            io:nl();
        {false,true} ->
            %% Should not happen since the file->render dialog
            %% must have been disabled
            if KeepXML -> ok; true -> file:delete(ExportFile) end,
            no_renderer;
        {_,true} when ExportFile == RenderFile ->
            export_file_is_render_file;
        {Renderer,true} ->
            LogVerbosityConsole = proplists:get_value(log_verbosity_console, Attr),
            LogVerbosityTxtHtml = proplists:get_value(log_verbosity_txt_html, Attr),
            SaveAlpha = proplists:get_value(save_alpha, Attr),
            AlphaChannel =  case SaveAlpha of
                                false -> "";
                                _ ->
                                    "-a "
                            end,

            ArgStr = Options++case Options of
                                  [] -> [];
                                  _ -> " "
                              end
                ++wings_job:quote(filename:basename(ExportFile)),
            PortOpts = [{cd,filename:dirname(ExportFile)}],
            Handler =
                fun (Status) ->
                        if KeepXML -> ok; true -> file:delete(ExportFile) end,
                        set_var(rendering, false),
                        case Status of
                            ok -> {RenderFormat,RenderFile};
                            _  -> Status
                        end
                end,
            file:delete(RenderFile),
            set_var(rendering, true),
            wings_job:render(ExportTS, Renderer,"-ccd"++" "++AlphaChannel++"-f "++format(RenderFormat)++" "++"-vl "++format(LogVerbosityConsole)++" "++"-lvl "++format(LogVerbosityTxtHtml)++" "++ArgStr++" "++wings_job:quote(filename:rootname(Filename))++" ", PortOpts, Handler)
    end.

warn_multiple_backgrounds([]) ->
    ok;
warn_multiple_backgrounds([_]) ->
    ok;
warn_multiple_backgrounds(BgLights) ->
    io:format(?__(1,"WARNING: Multiple backgrounds")++" - ", []),
    foreach(fun ({Name,_}) ->
                    io:put_chars([format(Name), $ ])
            end, BgLights),
    io:nl(),
    ok.

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).


%%% Export Material Properties
%%%

export_shader(F, Name, Mat, ExportDir) ->
    YafaRay = proplists:get_value(?TAG, Mat, []),

    DefShaderType = get_pref(shader_type, YafaRay),
    ShaderType =
        proplists:get_value(shader_type, YafaRay, DefShaderType),

    case ShaderType of

        shinydiffuse ->
            export_shinydiffuse_shader(F, Name, Mat, ExportDir, YafaRay);
        glossy ->
            export_glossy_shader(F, Name, Mat, ExportDir, YafaRay);
        coatedglossy ->
            export_coatedglossy_shader(F, Name, Mat, ExportDir, YafaRay);

        glass ->
            export_glass_shader(F, Name, Mat, ExportDir, YafaRay);

        lightmat ->
            export_lightmat_shader(F, Name, Mat, ExportDir, YafaRay);

        rough_glass ->
            export_rough_glass_shader(F, Name, Mat, ExportDir, YafaRay);

        blend_mat ->
            ok

    end.

%%% Export Shiny Diffuse Material
%%%

export_shinydiffuse_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"shinydiffusemat\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, mirror_color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),

    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),
    TIR = proplists:get_value(tir, YafaRay, ?DEF_TIR),


    Transparency = proplists:get_value(transparency, YafaRay, ?DEF_TRANSPARENCY),
    TransmitFilter = proplists:get_value(transmit_filter, YafaRay, ?DEF_TRANSMIT_FILTER),
    Translucency = proplists:get_value(translucency, YafaRay, ?DEF_TRANSLUCENCY),
    DiffuseReflect = proplists:get_value(diffuse_reflect, YafaRay, ?DEF_DIFFUSE_REFLECT),
    SpecularReflect = proplists:get_value(specular_reflect, YafaRay, ?DEF_SPECULAR_REFLECT),
    Emit = proplists:get_value(emit, YafaRay, ?DEF_EMIT),
    OrenNayar = proplists:get_value(oren_nayar, YafaRay, ?DEF_OREN_NAYAR),
    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor = proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),


    case AbsorptionColor of
        [ ] -> ok;
        {AbsR,AbsG,AbsB} ->
            AbsD =
                proplists:get_value(absorption_dist, YafaRay,
                                    ?DEF_ABSORPTION_DIST),
            export_rgb(F, absorption, {-math:log(max(AbsR, ?NONZERO))/AbsD,
                                       -math:log(max(AbsG, ?NONZERO))/AbsD,
                                       -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,

    case OrenNayar of
        false -> ok;
        _ ->
            OrenNayarSigma = proplists:get_value(oren_nayar_sigma, YafaRay, ?DEF_OREN_NAYAR_SIGMA),
            println(F, "        <diffuse_brdf sval=\"oren_nayar\"/>~n"
                    "        <sigma fval=\"~.10f\"/>",
                    [OrenNayarSigma])
    end,

    println(F, "        <IOR fval=\"~.10f\"/>~n"
            "        <fresnel_effect bval=\"~s\"/>~n"
            "        <transmit_filter fval=\"~.10f\"/>~n"
            "        <translucency fval=\"~.10f\"/>~n"
            "        <transparency fval=\"~.10f\"/>~n"
            "        <diffuse_reflect fval=\"~.10f\"/>~n"
            "        <specular_reflect fval=\"~.10f\"/>~n"
            "        <emit fval=\"~.10f\"/>~n",
            [IOR,format(TIR),TransmitFilter,Translucency,Transparency,DiffuseReflect,SpecularReflect,Emit]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").



%%% Export Glossy Material
%%%

export_glossy_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"glossy\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, diffuse_color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),


    DiffuseReflect = proplists:get_value(diffuse_reflect, YafaRay, ?DEF_DIFFUSE_REFLECT),

    GlossyReflect = proplists:get_value(glossy_reflect, YafaRay, ?DEF_GLOSSY_REFLECT),

    Exponent = proplists:get_value(exponent, YafaRay, ?DEF_EXPONENT),

    OrenNayar = proplists:get_value(oren_nayar, YafaRay, ?DEF_OREN_NAYAR),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor =
        proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),


    case AbsorptionColor of
        [ ] -> ok;
        {AbsR,AbsG,AbsB} ->
            AbsD =
                proplists:get_value(absorption_dist, YafaRay,
                                    ?DEF_ABSORPTION_DIST),
            export_rgb(F, absorption, {-math:log(max(AbsR, ?NONZERO))/AbsD,
                                       -math:log(max(AbsG, ?NONZERO))/AbsD,
                                       -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,

    case OrenNayar of
        false -> ok;
        _ ->
            OrenNayarSigma = proplists:get_value(oren_nayar_sigma, YafaRay,
                                                 ?DEF_OREN_NAYAR_SIGMA),

            println(F, "        <diffuse_brdf sval=\"oren_nayar\"/>~n"
                    "        <sigma fval=\"~.10f\"/>",
                    [OrenNayarSigma])
    end,



    println(F, "  <diffuse_reflect fval=\"~.10f\"/>~n"
            "        <glossy_reflect fval=\"~.10f\"/>~n"
            "        <exponent fval=\"~.10f\"/>~n",
            [DiffuseReflect,GlossyReflect,Exponent]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").




%%% Export Coated Glossy Material
%%%


export_coatedglossy_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"coated_glossy\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, diffuse_color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),

    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),

    DiffuseReflect = proplists:get_value(diffuse_reflect, YafaRay, ?DEF_DIFFUSE_REFLECT),

    GlossyReflect = proplists:get_value(glossy_reflect, YafaRay, ?DEF_GLOSSY_REFLECT),

    Exponent = proplists:get_value(exponent, YafaRay, ?DEF_EXPONENT),

    Anisotropic = proplists:get_value(anisotropic, YafaRay, ?DEF_ANISOTROPIC),

    Anisotropic_U = proplists:get_value(anisotropic_u, YafaRay, ?DEF_ANISOTROPIC_U),

    Anisotropic_V = proplists:get_value(anisotropic_v, YafaRay, ?DEF_ANISOTROPIC_V),

    OrenNayar = proplists:get_value(oren_nayar, YafaRay, ?DEF_OREN_NAYAR),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor =
        proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),


    case AbsorptionColor of
        [ ] -> ok;
        {AbsR,AbsG,AbsB} ->
            AbsD =
                proplists:get_value(absorption_dist, YafaRay,
                                    ?DEF_ABSORPTION_DIST),
            export_rgb(F, absorption, {-math:log(max(AbsR, ?NONZERO))/AbsD,
                                       -math:log(max(AbsG, ?NONZERO))/AbsD,
                                       -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,

    case OrenNayar of
        false -> ok;
        _ ->
            OrenNayarSigma = proplists:get_value(oren_nayar_sigma, YafaRay,
                                                 ?DEF_OREN_NAYAR_SIGMA),

            println(F, "        <diffuse_brdf sval=\"oren_nayar\"/>~n"
                    "        <sigma fval=\"~.10f\"/>",
                    [OrenNayarSigma])
    end,



    println(F, "        <IOR fval=\"~.10f\"/>~n"
            "        <diffuse_reflect fval=\"~.10f\"/>~n"
            "        <glossy_reflect fval=\"~.10f\"/>~n"
            "        <anisotropic bval=\"~s\"/>~n"
            "        <exp_u fval=\"~.10f\"/>~n"
            "        <exp_v fval=\"~.10f\"/>~n"
            "        <exponent fval=\"~.10f\"/>~n",
            [IOR,DiffuseReflect,GlossyReflect,Anisotropic,Anisotropic_U,Anisotropic_V,Exponent]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").



%%% Export Glass Material
%%%

export_glass_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"glass\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, mirror_color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, filter_color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),

    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),

    TransmitFilter = proplists:get_value(transmit_filter, YafaRay, ?DEF_TRANSMIT_FILTER),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor =
        proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),


    case AbsorptionColor of
        [ ] -> ok;
        {_AbsR,_AbsG,_AbsB} ->
            AbsD =
                proplists:get_value(absorption_dist, YafaRay,
                                    ?DEF_ABSORPTION_DIST),
            export_rgb(F, absorption,
                       proplists:get_value(absorption_color, YafaRay, AbsorptionColor)),

            println(F, "<absorption_dist fval=\"~.10f\"/>"
                    "        <transmit_filter fval=\"~.10f\"/>~n"

                   ,[AbsD,TransmitFilter])

    end,

    DispersionPower =
        proplists:get_value(dispersion_power, YafaRay, ?DEF_DISPERSION_POWER),
        println(F, "        <dispersion_power fval=\"~.10f\"/>~n", [DispersionPower]),

    FakeShadows =
        proplists:get_value(fake_shadows, YafaRay, ?DEF_FAKE_SHADOWS),

    println(F, "        <IOR fval=\"~.10f\"/>~n"
            "           <fake_shadows bval=\"~s\"/>~n"

            "",
            [IOR,format(FakeShadows)]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").



%%% Export Rough Glass Material
%%%

export_rough_glass_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"rough_glass\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, mirror_color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, filter_color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),

    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),

    TransmitFilter = proplists:get_value(transmit_filter, YafaRay, ?DEF_TRANSMIT_FILTER),

    Roughness = proplists:get_value(roughness, YafaRay, ?DEF_ROUGHNESS),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor =
        proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),


    case AbsorptionColor of
        [ ] -> ok;
        {_AbsR,_AbsG,_AbsB} ->
            AbsD =
                proplists:get_value(absorption_dist, YafaRay,
                                    ?DEF_ABSORPTION_DIST),
            export_rgb(F, absorption,
                       proplists:get_value(absorption_color, YafaRay, AbsorptionColor)),

            println(F, "<absorption_dist fval=\"~.10f\"/>~n"
                    "        <transmit_filter fval=\"~.10f\"/>~n"
                    "        <roughness fval=\"~.10f\"/>~n",[AbsD,TransmitFilter,Roughness])
    end,

    DispersionPower =
        proplists:get_value(dispersion_power, YafaRay, ?DEF_DISPERSION_POWER),
        println(F, "        <dispersion_power fval=\"~.10f\"/>~n", [DispersionPower]),

    FakeShadows =
        proplists:get_value(fake_shadows, YafaRay, ?DEF_FAKE_SHADOWS),

    println(F, "        <IOR fval=\"~.10f\"/>~n"
            "       <fake_shadows bval=\"~s\"/>~n"

            "",
            [IOR,format(FakeShadows)]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").






%%% Export Light Material
%%%

export_lightmat_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"light_mat\"/>", [Name]),
    _DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),

    Lightmat_Color =
        proplists:get_value(lightmat_color, YafaRay, DefLightmatColor),




    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.

    export_rgb(F, color,
               proplists:get_value(lightmat_color, YafaRay, Lightmat_Color)),


    Lightmat_Power = proplists:get_value(lightmat_power, YafaRay, ?DEF_LIGHTMAT_POWER),



    println(F, "  <power fval=\"~.10f\"/>~n",
            [Lightmat_Power]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").






%%% End of Basic Materials Export
%%%
%%% Start Blend Materials Export

export_shaderblend(F, Name, Mat, ExportDir) ->
    YafaRay = proplists:get_value(?TAG, Mat, []),

    DefShaderType = get_pref(shader_type, YafaRay),
    ShaderType =
        proplists:get_value(shader_type, YafaRay, DefShaderType),

    case ShaderType of

        blend_mat ->
            export_blend_mat_shader(F, Name, Mat, ExportDir, YafaRay);

        shinydiffuse ->
            ok;
        glossy ->
            ok;
        coatedglossy ->
            ok;

        translucent ->
            ok;

        glass ->
            ok;

        lightmat ->
            ok;

        rough_glass ->
            ok

    end.

%%% Export Blend Material


export_blend_mat_shader(F, Name, Mat, ExportDir, YafaRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Visibility = proplists:get_value(visibility, YafaRay, ?DEF_VISIBILITY),
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_texture(F, [Name,$_,format(N)],
                                      Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    uniprintln(F, "<material name=\"~ts\">~n"++
                  "<type sval=\"blend_mat\"/>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "       <hard fval=\"~.10f\"/>",
            [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, color,
               proplists:get_value(reflected, YafaRay, DefReflected)),
    export_rgb(F, diffuse_color,
               proplists:get_value(transmitted, YafaRay, DefTransmitted)),




    Blend_Mat1 = proplists:get_value(blend_mat1, YafaRay, ?DEF_BLEND_MAT1),

    Blend_Mat2 = proplists:get_value(blend_mat2, YafaRay, ?DEF_BLEND_MAT2),

    Blend_Value = proplists:get_value(blend_value, YafaRay, ?DEF_BLEND_VALUE),

    uniprintln(F, "  <material1 sval=\"w_\~ts\"/>~n"
                  "        <material2 sval=\"w_\~ts\"/>~n"
                  "        <blend_value fval=\"~.10f\"/>~n",
            [Blend_Mat1,Blend_Mat2,Blend_Value]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                  case export_modulator(F, [Name,$_,format(N)],
                                        Maps, M, Opacity) of
                      off -> N+1;
                      ok ->
                          println(F),
                          N+1
                  end;
              (_, N) ->
                  N % Ignore old modulators
          end, 1, Modulators),
    println(F, "        <visibility sval=\"~s\"/>~n", [Visibility]),
    println(F, "</material>").

%%% End Blend Materials Export


%%% Start Texture Export
%%%

export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,_,image} ->
            Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
            export_texture(F, Name, image, Filename);
        {true,_,jpeg} -> %% Old tag
            Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
            export_texture(F, Name, image, Filename);
        {true,_,{map,Map}} ->
            case proplists:get_value(Map, Maps, undefined) of
                undefined ->
                    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
                #e3d_image{name=ImageName, filename = FileName}=Image ->
                    case FileName of
                        none ->
                            MapFile = case get_map_type(ImageName) of
                                          sys -> ImageName++".png";
                                          _ -> ImageName
                                      end,
                            Filepath0 = filename:join(ExportDir, MapFile),
                            case e3d_image:save(Image, Filepath0) of
                                {error, _} -> % file type not supported by Wings3d
                                    Filepath = filename:join(ExportDir, set_map_type(ImageName,".png")),
                                    e3d_image:save(Image, Filepath);
                                _ -> Filepath = Filepath0
                            end;
                        _ -> Filepath = FileName
                    end,
                    export_texture(F, Name, image, Filepath)
            end;
        {true,_,Type} ->
            export_texture(F, Name, Type, Ps)
    end.

export_texture(F, Name, image, Filename) ->
    uniprintln(F, "<texture name=\"~ts\">~n"++
                  "    <filename sval=\"~ts\"/>~n"++
                  "<type sval=\"image\"/>~n" ++
                  "</texture>", [Name,Filename]);
export_texture(F, Name, Type, Ps) ->
    uniprintln(F, "<texture name=\"~ts\"> <type sval=\"~s\"/>", [Name,format(Type)]),

    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
    NoiseSize = proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE),
    export_rgb(F, color1, Color1),
    export_rgb(F, color2, Color2),
    println(F, "    <hard bval=\"~s\"/>" " <noise_type sval=\"~s\"/>"  " <size fval=\"~.6f\"/>", [format(Hard),NoiseBasis,NoiseSize]),

    case Type of
        clouds ->
            Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
            println(F, "  <depth ival=\"~w\"/>" , [Depth]);
        marble ->
            Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
            Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
            Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
            Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),
            println(F, "  <depth ival=\"~w\"/>" "      <turbulence fval=\"~.6f\"/>~n" "  <sharpness fval=\"~.6f\"/>" " <shape sval=\"~s\"/>", [Depth,Turbulence,Sharpness,Shape]);
        wood ->
            WoodType = proplists:get_value(wood_type, Ps, ?DEF_MOD_WOODTYPE),
            Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
            Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),
            %% Coordinate rotation, see export_pos/3.
            println(F, " <wood_type sval=\"~s\"/>"++
                        "    <turbulence fval=\"~.6f\"/>~n" " <shape sval=\"~s\"/>",
                        [WoodType,Turbulence,Shape]);
        voronoi ->
            CellType = proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE),
            CellShape = proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE),
            CellSize = proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE),
            Intensity = proplists:get_value(intensity, Ps, ?DEF_MOD_INTENSITY),
            CellWeight1 = proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1),
            CellWeight2 = proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2),
            CellWeight3 = proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3),
            CellWeight4 = proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4),
            %% Coordinate rotation, see export_pos/3.
            println(F, " <color_type sval=\"~s\"/>"++
                        "   <distance_metric sval=\"~s\"/>~n"
                        "    <size fval=\"~.6f\"/>"++
                        "    <intensity fval=\"~.6f\"/>~n"
                        "    <weight1 fval=\"~.6f\"/>"++
                        "    <weight2 fval=\"~.6f\"/>~n"
                        "    <weight3 fval=\"~.6f\"/>"++
                        "    <weight4 fval=\"~.6f\"/>",
                        [CellType,CellShape,CellSize,Intensity,CellWeight1,
                         CellWeight2,CellWeight3,CellWeight4]);
        musgrave ->
            MusgraveType = proplists:get_value(musgrave_type, Ps, ?DEF_MOD_MUSGRAVE_TYPE),
            NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
            MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),
            MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),
            MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, ?DEF_MOD_MUSGRAVE_CONTRAST),
            MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),
            MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),
            %% Coordinate rotation, see export_pos/3.
            println(F, " <musgrave_type sval=\"~s\"/>"++
                        " <noise_type sval=\"~s\"/>~n"
                        "    <size fval=\"~.6f\"/>"++
                        "    <intensity fval=\"~.6f\"/>~n"
                        "    <H fval=\"~.6f\"/>"++
                        "    <lacunarity fval=\"~.6f\"/>~n"
                        "    <octaves fval=\"~.6f\"/>",
                        [MusgraveType,NoiseBasis,MusgraveNoiseSize,MusgraveIntensity,
                         MusgraveContrast,MusgraveLacunarity,MusgraveOctaves]);
        distorted_noise ->
            NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
            DistortionType = proplists:get_value(distortion_type, Ps, ?DEF_MOD_DISTORTION_TYPE),
            DistortionNoiseSize = proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE),
            DistortionIntensity = proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY),
            %% Coordinate rotation, see export_pos/3.
            println(F, " <noise_type1 sval=\"~s\"/>"++
                        " <noise_type2 sval=\"~s\"/>~n"
                        "    <size fval=\"~.6f\"/>"++
                        "    <distort fval=\"~.6f\"/>~n",
                        [NoiseBasis,DistortionType,DistortionNoiseSize,DistortionIntensity]);
        _ ->
            ok
    end,
    println(F, "</texture>").

%%% images file type used by Yafaray
%%% http://www.yafaray.org/documentation/userguide/textureinput#imageinput
get_map_type(Filepath) ->
    Ext = filename:extension(Filepath),
    case Ext of
        ".tga" -> tga;
        ".jpg" -> jpg;
        ".png" -> png;
        ".tiff" -> tiff;
        ".hdr" -> hdr;
        ".exr" -> exr;
        _ -> sys
    end.

images_format() ->
    ImgInfo = wings_job:render_formats(),
    lists:foldr(fun(Type,Acc) ->
        case lists:keyfind(Type,1,ImgInfo) of
            {_,Ext,Desc} -> Acc ++[{Ext,Desc}];
            _ -> Acc
        end
    end, [{".tiff","Tagged Image File Format"}], [tga,jpg,png,hdr,exr]).

%%% Ext parameter must include the "." - ex. ".jpg"
%%% that will replace the extension in case the file name already includes it.
set_map_type(Filepath0,Ext) ->
    Filepath = filename:rootname(Filepath0),
    Filepath ++ Ext.

export_modulator(F, Texname0, Maps, {modulator,Ps}, _Opacity) when is_list(Ps) ->
    Texname = lists:flatten(Texname0),
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,Mode,Type} ->

            AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),

%%% Start Change Number from Texname for UpperLayer

            UpperLayerName =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_2","_1",[global,unicode]);
                    _-> re:replace(Texname,"_1","_2",[global,unicode])
                end,

%%% End Change Number from Texname for UpperLayer

%%% Start Change Number from Texname for Stencil Input

            StencilInputName =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_2","_3",[global,unicode]);
                    _-> ""
                end,

%%% End Change Number from Texname for Stencil Input

%%% Start Change Number from Texname for Stencil UpperLayer Name 2

            StencilUpperLayerName2 =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_1","_2",[global,unicode]);
                    _-> ""
                end,

%%% End Change Number from Texname for Stencil UpperLayer Name 2


            SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
            SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
            SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
            TextureType = proplists:get_value(texture_type, Ps, ?DEF_TEXTURE_TYPE),
            Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
%%	    _Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
%%	    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
            Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
            Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
%%            _Color = Diffuse * Opacity,
%%            _HardValue = Shininess,
%%            _Transmission = Diffuse * (1.0 - Opacity),
%%	    _Reflection = Ambient,
            TexCo =
                case Type of
                    image -> "<texco sval=\"uv\"/>";
                    jpeg -> "<texco sval=\"uv\"/>";
                    {map,_} -> "<texco sval=\"uv\"/>";
                    marble -> "<texco sval=\"global\"/>";
                    wood -> "<texco sval=\"global\"/>";
                    clouds -> "<texco sval=\"global\"/>";
                    _ -> ""
                end,

            ModeNumber =
                case Mode of
                    mix -> "0";
                    add -> "1";
                    mul -> "2";
                    sub -> "3";
                    scr -> "4";
                    divide -> "5";
                    dif -> "6";
                    dar -> "7";
                    lig -> "8";
                    _ -> ""
                end,

%% Start Identify Modulator # (w_default_Name_1 or w_default_Name_2)
            Split=re:split(Texname,"_",[{return, list},unicode]),
            Num=lists:last(Split),
            UpperLayer =
                case {Num,Mode,AlphaIntensity} of
                    {"1",mix,_} ->  "";
                    {"1",_,_} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
                    {"2",_,stencil} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
                    _ -> ""
                end,
%% End Identify Modulator #

            UpperColor =
                case Num of
                    "1" ->  "<upper_color r=\"1\" g=\"1\" b=\"1\" a=\"1\"/>";
                    _ -> ""
                end,

            UseAlpha =
                case {Num,AlphaIntensity} of
                    {"1",off} ->  "";
                    {_,transparency} -> "<do_scalar bval=\"true\"/>";
                    {_,diffusealphatransparency} -> "<use_alpha bval=\"true\"/>";
                    {_,translucency} -> "<do_scalar bval=\"true\"/>";
                    {_,specularity} -> "<do_scalar bval=\"true\"/>";
                    {_,stencil} -> "<use_alpha bval=\"true\"/>";
                    _ -> ""
                end,



            TextureShaderType =
                case {Normal,TextureType,AlphaIntensity} of
                    {+0.0,diffusetexture,off} -> "<diffuse_shader";
                    {+0.0,mirrorcolortexture,off} -> "<mirror_color_shader";
                    {+0.0,mirrortexture,off} -> "<mirror_shader";
                    {+0.0,glossytexture,off} -> "<glossy_shader";
                    {+0.0,glossyreflecttexture,off} -> "<diffuse_reflect_shader";
                    {+0.0,transparencytexture,off} -> "<transparency_shader";
                    {+0.0,translucencytexture,off} -> "<translucency_shader";
                    {+0.0,bumptexture,off} -> "<bump_shader";

                    {+0.0,diffusetexture,transparency} -> "<transparency_shader";
                    {+0.0,mirrorcolortexture,transparency} -> "<transparency_shader";
                    {+0.0,mirrortexture,transparency} -> "<transparency_shader";
                    {+0.0,glossytexture,transparency} -> "<transparency_shader";
                    {+0.0,glossyreflecttexture,transparency} -> "<transparency_shader";
                    {+0.0,transparencytexture,transparency} -> "<transparency_shader";

                    {+0.0,diffusetexture,diffusealphatransparency} -> "<diffuse_shader";
                    {+0.0,transparencytexture,diffusealphatransparency} -> "<diffuse_shader";

                    {+0.0,diffusetexture,translucency} -> "<translucency_shader";
                    {+0.0,glossytexture,translucency} -> "<translucency_shader";
                    {+0.0,translucencytexture,translucency} -> "<translucency_shader";

                    {+0.0,diffusetexture,specularity} -> "<mirror_shader";
                    {+0.0,mirrorcolortexture,specularity} -> "<mirror_shader";
                    {+0.0,mirrortexture,specularity} -> "<mirror_shader";
                    {+0.0,glossytexture,specularity} -> "<mirror_shader";
                    {+0.0,glossyreflecttexture,specularity} -> "<mirror_shader";

                    {+0.0,diffusetexture,stencil} -> "<diffuse_shader";
                    _ -> "<bump_shader"
                end,

            ShaderName =
                case {Num,Mode} of
                    {"1",_} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
                    {_,mix} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
                    _ -> ""
                end,


            case AlphaIntensity of
                stencil ->
%%Stencil Export Start
                    uniprintln(F, " <!--Start Stencil Section Here-->

                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"~ts\"/>
                                <input sval=\"~ts_mod\"/>

                                <noRGB bval=\"true\"/>
                                <stencil bval=\"true\"/>
                                "++UpperLayer++"

                                <type sval=\"layer\"/>
                                <mode ival=\""++ModeNumber++"\"/>
                                </list_element>

                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"~ts_mod\"/>
                                "++TexCo++"
                                <mapping sval=\"plain\"/>
                                <texture sval=\"~ts\"/>
                                <type sval=\"texture_mapper\"/>
                                <bump_strength fval=\"~.3f\"/>
                                </list_element>

                                <diffuse_shader sval=\"diff_layer2\"/>
                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"diff_layer2\"/>
                                <input sval=\""++StencilInputName++"_mod\"/>
                                <upper_layer sval=\""++StencilUpperLayerName2++"\"/>
                                <type sval=\"layer\"/>
                                <mode ival=\""++ModeNumber++"\"/>
                                </list_element>

        <!--End Stencil Section Here-->",
                        [Texname,Texname,Texname,Texname,Normal
                        ]);
%%Stencil Export End
                _ ->

                    uniprintln(F, "  "++ShaderName++"
                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"~ts\"/>
                                <input sval=\"~ts_mod\"/>
                                "++UpperLayer++"
                                "++UpperColor++"
                                "++UseAlpha++"
                                <type sval=\"layer\"/>
                                <mode ival=\""++ModeNumber++"\"/>
                                <colfac fval=\"~.3f\"/>
                                <valfac fval=\"~.3f\"/>
                                </list_element>
                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"~ts_mod\"/>
                                "++TexCo++"
                                <mapping sval=\"plain\"/>
                                <texture sval=\"~ts\"/>
                                <type sval=\"texture_mapper\"/>
                                <scale x=\"~.3f\" y=\"~.3f\" z=\"~.3f\"/>
                                <bump_strength fval=\"~.3f\"/>
                                </list_element>",
                        [Texname,Texname,Diffuse,Shininess,Texname,Texname,SizeX,SizeY,SizeZ,Normal
                        ])

            end

    end.



export_rgb(F, Type, {R,G,B,_}) ->
    export_rgb(F, Type, {R,G,B});
export_rgb(F, Type, {R,G,B}) ->
    println(F, ["        <",format(Type)," r=\"",format(R),
                "\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).




%% Return object with arealight faces only
%%
export_object(F, NameStr, Mesh=#e3d_mesh{fs=Fs}, MatsGb, Id) ->
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    MatPs = gb_trees:get(DefaultMaterial, MatsGb),
    export_object_1(F, NameStr, Mesh, DefaultMaterial, MatPs, Id).

%% Count the number of subsequent equal elements in the list.
%% Returns list of {Count,Element}.
%%
count_equal([H|T]) ->
    count_equal(T, 1, H, []).
%%
count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).






export_object_1(F, NameStr, Mesh0=#e3d_mesh{he=He0}, DefaultMaterial, MatPs, Id) ->
    YafaRay = proplists:get_value(?TAG, MatPs, []),
    _OpenGL = proplists:get_value(opengl, MatPs),
    UseHardness = proplists:get_value(use_hardness, YafaRay, ?DEF_USE_HARDNESS),
    Object_Type = proplists:get_value(object_type, YafaRay, ?DEF_OBJECT_TYPE),

    Volume_Sigma_a = proplists:get_value(volume_sigma_a, YafaRay, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, YafaRay, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, YafaRay, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, YafaRay, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, YafaRay, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Sharpness = proplists:get_value(volume_sharpness, YafaRay, ?DEF_VOLUME_SHARPNESS),
    Volume_Cover = proplists:get_value(volume_cover, YafaRay, ?DEF_VOLUME_COVER),
    Volume_Density = proplists:get_value(volume_density, YafaRay, ?DEF_VOLUME_DENSITY),
    Volume_Minmax_X = proplists:get_value(volume_minmax_x, YafaRay, ?DEF_VOLUME_MINMAX_X),
    Volume_Minmax_Y = proplists:get_value(volume_minmax_y, YafaRay, ?DEF_VOLUME_MINMAX_Y),
    Volume_Minmax_Z = proplists:get_value(volume_minmax_z, YafaRay, ?DEF_VOLUME_MINMAX_Z),
    Lightportal_Power = proplists:get_value(lightportal_power, YafaRay, ?DEF_LIGHTPORTAL_POWER),
    Lightportal_Samples = proplists:get_value(lightportal_samples, YafaRay, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Diffusephotons = proplists:get_value(lightportal_diffusephotons, YafaRay, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, YafaRay, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, YafaRay, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    Meshlight_Power = proplists:get_value(meshlight_power, YafaRay, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, YafaRay, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, YafaRay, ?DEF_MESHLIGHT_COLOR),

    Meshlight_Double_Sided = proplists:get_value(meshlight_double_sided, YafaRay, ?DEF_MESHLIGHT_DOUBLE_SIDED),
    AutosmoothAngle =
        proplists:get_value(autosmooth_angle, YafaRay, ?DEF_AUTOSMOOTH_ANGLE),

    Autosmooth = proplists:get_value(autosmooth, YafaRay,
                                     if AutosmoothAngle == 0.0 -> false;
                                        true -> ?DEF_AUTOSMOOTH end),


    %% Pre-process mesh
    Mesh1 = #e3d_mesh{} =
        case {He0,UseHardness} of
            {[_|_],true} ->
                io:format(?__(1,"Mesh ~ts: slitting hard edges..."), [NameStr]),
                M1 = e3d_mesh:slit_hard_edges(Mesh0, [slit_end_vertices]),
                io:format(?__(2,"done")++"~n"),
                M1;
            _ -> Mesh0
        end,


    io:format(?__(3,"Mesh ~ts: triangulating..."), [NameStr]),
    #e3d_mesh{fs=Fs,vs=Vs,vc=Vc,tx=Tx} = e3d_mesh:triangulate(Mesh1),
    io:format(?__(4,"done")++"~n"),
    io:format(?__(5,"Mesh ~ts: exporting..."), [NameStr]),
    %%

    %% Add Export Object Name Start

    uniprintln(F, "<!--Object Name ~ts, Object # ~w-->", [NameStr,Id]),

    %% Add Export Object Name End

    HasUV = case Tx of
                []-> "false";
                _ ->
                    "true"

            end,



    case Object_Type of
        mesh ->
            println(F," "),
            println(F, "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"0\">",[Id,length(Vs),length(Fs),HasUV]),
            println(F," ");

        volume ->
            println(F," "),
            println(F, "<volumeregion name=\"volumename\">"),

            case proplists:get_value(volume_type, YafaRay,
                                     ?DEF_VOLUME_TYPE) of
                uniformvolume ->
                    println(F, "<type sval=\"UniformVolume\"/>");


                expdensityvolume ->
                    println(F, "<type sval=\"ExpDensityVolume\"/>"),
                    println(F, "<a fval=\"~.10f\"/>",[Volume_Height]),
                    println(F, "<b fval=\"~.10f\"/>",[Volume_Steepness]);


                noisevolume ->
                    println(F, "<type sval=\"NoiseVolume\"/>"),
                    println(F, "<sharpness fval=\"~.10f\"/>",[Volume_Sharpness]),
                    println(F, "<cover fval=\"~.10f\"/>",[Volume_Cover]),
                    println(F, "<density fval=\"~.10f\"/>",[Volume_Density]),
                    uniprintln(F, "<texture sval=\"w_~ts_1\"/>",[DefaultMaterial])

            end,


            println(F, "<attgridScale ival=\"~w\"/>",[Volume_Attgridscale]),
            % FIXME: This is not really good, volume Min/Max coordinates should be automatically calculated based on the object bounds, not entered manually by the user. This is counter-intuitive.
            println(F, "<maxX fval=\"~.10f\"/>",[Volume_Minmax_Z]),
            println(F, "<maxY fval=\"~.10f\"/>",[Volume_Minmax_X]),
            println(F, "<maxZ fval=\"~.10f\"/>",[Volume_Minmax_Y]),
            println(F, "<minX fval=\"-\~.10f\"/>",[Volume_Minmax_Z]),
            println(F, "<minY fval=\"-\~.10f\"/>",[Volume_Minmax_X]),
            println(F, "<minZ fval=\"-\~.10f\"/>",[Volume_Minmax_Y]),
            println(F, "<sigma_a fval=\"~.10f\"/>",[Volume_Sigma_a]),
            println(F, "<sigma_s fval=\"~.10f\"/>",[Volume_Sigma_s]),
            println(F," ");


        meshlight ->
            println(F," "),
            uniprintln(F, "<light name=\"~ts\">",[NameStr]),

            export_rgb(F, color, proplists:get_value(meshlight_color, YafaRay, Meshlight_Color)),

            println(F, "<object ival= \"~w\"/>",[Id]),
            println(F, "<power fval=\"~.10f\"/>",[Meshlight_Power]),
            println(F, "<samples ival=\"~w\"/>",[Meshlight_Samples]),
            println(F, "<double_sided bval=\"~s\"/>",[Meshlight_Double_Sided]),
            println(F, "<type sval=\"~s\"/>",[Object_Type]),
            println(F, "</light>"),
            println(F," "),
            println(F, "<mesh id=\"~w\" type=\"0\">",[Id]),
            println(F," ");

        lightportal ->
            println(F," "),
            uniprintln(F, "<light name=\"~ts\">",[NameStr]),
            println(F, "<type sval=\"bgPortalLight\"/>"),
            println(F, "<power fval=\"~.10f\"/>",[Lightportal_Power]),
            println(F, "<samples ival=\"~w\"/>",[Lightportal_Samples]),
            println(F, "<object ival= \"~w\"/>",[Id]),
            println(F, "<with_diffuse bval=\"~s\"/>",[Lightportal_Diffusephotons]),
            println(F, "<with_caustic bval=\"~s\"/>",[Lightportal_Causticphotons]),
            println(F, "<photon_only bval=\"~s\"/>",[Lightportal_Photon_Only]),
            println(F, "</light>"),
            println(F, "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"256\">",[Id,length(Vs),length(Fs),HasUV]),
            println(F," ")

    end,

    case Object_Type of
        volume -> ok;
        _ ->
            export_vertices(F, Vs),

            %% Add Export UV_Vectors Part 1 Start
            case HasUV of
                "false" -> ok;
                "true" -> println(F, "        "),
                          println(F, "<!--uv_vectors Quantity=\"~w\" -->",[length(Tx)]),
                          println(F, "        "),
                          export_vectors2D(F, Tx)
            end,
            %% Add Export UV_Vectors Part 1 End

            export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc))
    end,

    case Object_Type of
        mesh ->
            println(F," "),
            println(F, "</mesh>"),
            println(F," ");

        volume ->
            println(F," "),
            println(F, "</volumeregion>"),
            println(F," ");

        meshlight ->
            println(F," "),
            println(F, "</mesh>"),
            println(F," ");

        lightportal ->
            println(F," "),
            println(F, "</mesh>"),
            println(F," ")
    end,


    case Object_Type of
        volume -> ok;
        _ ->
            case Autosmooth of
                false ->
                    println(F, "");
                true ->
                    println(F, "    <smooth ID=\"~w\" angle=\"~.3f\"/>", [Id,AutosmoothAngle])
            end
    end,
    
    io:format(?__(6,"done")++"~n").



export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



%% The coordinate system is rotated to make sunsky background
%% and environment images work as expected.
%% It assumes `South Y=East Z=Up in YafaRay coordinates.
%% Hence Z=South, X=East, Y=Up in Wings coordinates.
%%
export_pos(F, Type, {X,Y,Z}) ->
    println(F, ["    <",format(Type)," x=\"",format(Z),
                "\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).

%%Add Export UV_Vectors Part 2 Start

export_vectors2D(_F, [])->
    ok;

export_vectors2D(F, [{X, Y} | List])->
    println(F, "    <uv u=\"~f\" v=\"~f\"/>", [X, Y]),
    export_vectors2D(F, List).

%%Add Export UV_Vectors Part 2 End

export_faces(_F, [], _DefMat, _TxT, _VColT) ->
    ok;
export_faces(F, [#e3d_face{mat=[Mat|_],tx=Tx,vs=[A,B,C],vc=VCols}|T],

    DefaultMaterial, TxT, VColT) ->

    Shader =
        case Mat of
            DefaultMaterial -> "    <set_material sval=\"w_" ++ format(Mat) ++ "\"/>";
            _ -> "    <set_material sval=\"w_" ++ format(Mat) ++ "\"/>"
        end,




    UVIndices = case Tx of
                    []-> " uv_a=\"0\" uv_b=\"0\" uv_c=\"0\"/>";
                    _ ->
                        {U, V, W} = list_to_tuple(Tx),
                        (io_lib:format(" uv_a=\"~w\" uv_b=\"~w\" uv_c=\"~w\"/>", [U, V, W]))

                end,


    VCol = case {VColT,VCols} of
               {{},[]} -> "";
               {{},_} ->
                   io:format(?__(3,"WARNING! Face refers to non-existing "
                                 "vertex colors")++"~n"),
                   "";
               {_,[]} ->
                   %%io:format("WARNING! Face missing vertex colors~n"),
                   "";
               {_,[VcA,VcB,VcC]} ->
                   {VcAr,VcAg,VcAb} = element(1+VcA, VColT),
                   {VcBr,VcBg,VcBb} = element(1+VcB, VColT),
                   {VcCr,VcCg,VcCb} = element(1+VcC, VColT),
                   [io_lib:nl(),"           vcol_a_r=\"",format(VcAr),
                    "\" vcol_a_g=\"",format(VcAg),
                    "\" vcol_a_b=\"",format(VcAb),"\"",
                    io_lib:nl(),"           vcol_b_r=\"",format(VcBr),
                    "\" vcol_b_g=\"",format(VcBg),
                    "\" vcol_b_b=\"",format(VcBb),"\"",
                    io_lib:nl(),"           vcol_c_r=\"",format(VcCr),
                    "\" vcol_c_g=\"",format(VcCg),
                    "\" vcol_c_b=\"",format(VcCb),"\""];
               _ ->
                   io:format(?__(4,"WARNING! Face has ~w =/= 3 vertex colors")++"~n",
                             [length(VCols)]),
                   ""
           end,
    uniprintln(F, "~ts    <f a=\"~s\" b=\"~s\" c=\"~s\"~s~s",
               [Shader,format(A),format(B),format(C),UVIndices,VCol]),


    export_faces(F, T, DefaultMaterial, TxT, VColT).



export_light(F, Name, Ps) ->
    case proplists:get_value(visible, Ps, true) of
        true ->
            OpenGL = proplists:get_value(opengl, Ps, []),
            YafaRay = proplists:get_value(?TAG, Ps, []),
            Type = proplists:get_value(type, OpenGL, []),
            export_light(F, Name, Type, OpenGL, YafaRay);
        _ ->
            undefined
    end.

%% Export Point Light

export_light(F, Name, point, OpenGL, YafaRay) ->
    Power = proplists:get_value(power, YafaRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafaRay, ?DEF_POINT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    uniprintln(F,"<light name=\"~ts\"> <type sval=\"~w\"/>  <power fval=\"~.3f\"/> ",
            [Name,Type,Power]),
    case Type of
        pointlight ->
            ignore;
        spherelight ->
            ArealightRadius =
                proplists:get_value(arealight_radius, YafaRay,
                                    ?DEF_AREALIGHT_RADIUS),
            ArealightSamples =
                proplists:get_value(arealight_samples, YafaRay,
                                    ?DEF_AREALIGHT_SAMPLES),



            println(F,"       <radius fval=\"~.10f\"/>"++
                        "~n       <samples ival=\"~w\"/>
                                ", [ArealightRadius,ArealightSamples]
                    )
    end,
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),

    println(F,"       <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
    println(F, "</light>"),
    undefined;

%%% Export Infinite Light Sun and Directional
export_light(F, Name, infinite, OpenGL, YafaRay) ->
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_INFINITE),
    Type = proplists:get_value(type, YafaRay, ?DEF_INFINITE_TYPE),
    InfiniteTrue = proplists:get_value(infinite_true, YafaRay, ?DEF_INFINITE_TRUE),
    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    SunSamples = proplists:get_value(sun_samples, YafaRay, ?DEF_SUN_SAMPLES),
    SunAngle = proplists:get_value(sun_angle, YafaRay, ?DEF_SUN_ANGLE),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),

    %% Directional Infinite Light Start
    case Type of
        directional when Power > 0.0 ->
            uniprintln(F,"<light name=\"~ts\"> <type sval=\"~w\"/> "++
                         "<power fval=\"~.3f\"/>",
                    [Name, Type, Power]),

            %% Add Semi-infinite Start

            case proplists:get_value(infinite_true, YafaRay,
                                     ?DEF_INFINITE_TRUE) of
                false ->
                    InfiniteRadius = proplists:get_value(infinite_radius, YafaRay,
                                                         ?DEF_INFINITE_RADIUS),
                    println(F, " <infinite bval=\"~s\"/>   <radius fval=\"~.10f\"/>",
                            [format(InfiniteTrue),InfiniteRadius]),
                    export_pos(F, from, Position);
                true -> ok
            end,
            %% Add Semi-infinite End

            export_pos(F, direction, Position),
            export_rgb(F, color, Diffuse),
            println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
            println(F, "</light>"),
            Bg;


        directional -> Bg;

        %% Directional Infinite Light End
        %% Sunlight Infinite Light Start
        sunlight when Power > 0.0 ->
            uniprintln(F,"<light name=\"~ts\"> <type sval=\"~w\"/> "++
                         "<power fval=\"~.10f\"/> <samples ival=\"~w\"/> <angle fval=\"~.3f\"/>",
                    [Name, Type, Power, SunSamples, SunAngle]),

            export_pos(F, direction, Position),
            export_rgb(F, color, Diffuse),
            println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
            println(F, "</light>"),
            Bg;

        sunlight -> Bg

                    %% Sunlight Infinite Light End
    end;


%%% Export Spot Light
export_light(F, Name, spot, OpenGL, YafaRay) ->
    Power = proplists:get_value(power, YafaRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafaRay, ?DEF_SPOT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    uniprintln(F,"<light name=\"~ts\"> <power fval=\"~.3f\"/> ",
               [Name,Power]),
    case Type of
        spotlight ->

            SpotPhotonOnly =
                proplists:get_value(spot_photon_only, YafaRay, ?DEF_SPOT_PHOTON_ONLY),

            SpotSoftShadows =
                proplists:get_value(spot_soft_shadows, YafaRay, ?DEF_SPOT_SOFT_SHADOWS),

            SpotIESSamples = proplists:get_value(spot_ies_samples, YafaRay,
                                                 ?DEF_SPOT_IES_SAMPLES),

            SpotExponent =
                proplists:get_value(spot_exponent, OpenGL, ?DEF_SPOT_EXPONENT),
            SpotBlend = proplists:get_value(spot_blend, YafaRay, ?DEF_SPOT_BLEND),
            SpotFuzzyness = proplists:get_value(spot_fuzzyness, YafaRay, ?DEF_SPOT_FUZZYNESS),
            print(F, "<type sval=\"spotlight\"/> <photon_only bval=\"~s\"/> <cone_angle fval=\"~.3f\"/>~n"++
            "       <beam_falloff fval=\"~.10f\"/> <blend fval=\"~.3f\"/> <soft_shadows bval=\"~s\"/> <shadowFuzzyness fval=\"~.3f\"/> <samples ival=\"~w\"/>",
                [SpotPhotonOnly, ConeAngle, SpotExponent, SpotBlend,SpotSoftShadows,SpotFuzzyness,SpotIESSamples]);

        spot_ies ->

            SpotSoftShadows =
                proplists:get_value(spot_soft_shadows, YafaRay, ?DEF_SPOT_SOFT_SHADOWS),


            SpotIESFilename = proplists:get_value(spot_ies_filename, YafaRay,
                                                  ?DEF_SPOT_IES_FILENAME),

            SpotIESSamples = proplists:get_value(spot_ies_samples, YafaRay,
                                                 ?DEF_SPOT_IES_SAMPLES),

            uniprintln(F, "<type sval=\"ieslight\"/> <cone_angle fval=\"~.3f\"/> <soft_shadows bval=\"~s\"/> <samples ival=\"~w\"/>~n"++
                          "       <file sval=\"~ts\"/>",
                    [ConeAngle,SpotSoftShadows,SpotIESSamples,SpotIESFilename])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
    println(F, "</light>"),
    undefined;

%% Export Ambient Light
export_light(F, Name, ambient, _OpenGL, YafaRay) ->
    Type = proplists:get_value(type, YafaRay, ?DEF_AMBIENT_TYPE),
    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_AMBIENT),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    case Type of
        hemilight when Power > 0.0 ->
            println(F,"",
                    []),
            println(F, "",
                    []),
            case proplists:get_value(use_maxdistance, YafaRay,
                                     ?DEF_USE_MAXDISTANCE) of
                true ->
                    Maxdistance = proplists:get_value(maxdistance, YafaRay,
                                                      ?DEF_MAXDISTANCE),
                    println(F, "    <maxdistance fval=\"~.10f\"/>",
                            [Maxdistance]);
                false -> ok
            end,

            println(F, ""),
            Bg;
        hemilight -> Bg;
        pathlight when Power > 0.0 ->
            uniprintln(F,"<light type sval=\"~w\" name sval=\"~ts\" power fval=\"~.3f\"",
                       [Type,Name,Power]),
            UseQMC = proplists:get_value(use_QMC, YafaRay,
                                         ?DEF_USE_QMC),
            Depth = proplists:get_value(depth, YafaRay, ?DEF_DEPTH),
            CausDepth = proplists:get_value(caus_depth, YafaRay, ?DEF_CAUS_DEPTH),
            Direct = proplists:get_value(direct, YafaRay, ?DEF_DIRECT),
            Samples = proplists:get_value(samples, YafaRay,
                                          ?DEF_SAMPLES),
            print(F, "       use_QMC=\"~s\" samples=\"~w\" "
                  "depth=\"~w\" caus_depth=\"~w\"",
                  [format(UseQMC),Samples,Depth,CausDepth]),
            case Direct of
                true ->
                    print(F, " direct=\"on\"");
                false ->
                    case proplists:get_value(cache, YafaRay, ?DEF_CACHE) of
                        true ->
                            CacheSize =
                                proplists:get_value(cache_size, YafaRay,
                                                    ?DEF_CACHE_SIZE),
                            AngleThreshold =
                                proplists:get_value(angle_threshold, YafaRay,
                                                    ?DEF_ANGLE_THRESHOLD),
                            ShadowThreshold =
                                proplists:get_value(shadow_threshold, YafaRay,
                                                    ?DEF_SHADOW_THRESHOLD),
                            Gradient =
                                proplists:get_value(gradient, YafaRay,
                                                    ?DEF_GRADIENT),
                            ShowSamples =
                                proplists:get_value(show_samples, YafaRay,
                                                    ?DEF_SHOW_SAMPLES),
                            Search =
                                proplists:get_value(search, YafaRay, ?DEF_SEARCH),
                            print(F, " cache=\"on\"~n"
                                  "       cache_size=\"~.10f\" "
                                  "angle_threshold=\"~.10f\"~n"
                                  "       shadow_threshold=\"~.10f\" "
                                  "gradient=\"~s\"~n"
                                  "       show_samples=\"~s\" search=\"~w\"",
                                  [CacheSize,AngleThreshold,
                                   ShadowThreshold,format(Gradient),
                                   format(ShowSamples),Search]);
                        false -> ok
                    end
            end,
            println(F, ">"),
            PathlightMode = proplists:get_value(pathlight_mode, YafaRay,
                                                ?DEF_PATHLIGHT_MODE),
            case PathlightMode of
                undefined ->
                    ok;
                _ ->
                    println(F, "    <mode sval=\"~s\"/>",
                            [format(PathlightMode)])
            end,
            case proplists:get_value(use_maxdistance, YafaRay,
                                     ?DEF_USE_MAXDISTANCE) of
                true ->
                    Maxdistance = proplists:get_value(maxdistance, YafaRay,
                                                      ?DEF_MAXDISTANCE),
                    println(F, "    <maxdistance fval=\"~.10f\"/>",
                            [Maxdistance]);
                false -> ok
            end,
            println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
            println(F, "</light>"),
            Bg;
        pathlight -> Bg;
        globalphotonlight ->
            uniprintln(F,"<light type sval=\"~w\" name sval=\"~ts\"", [Type,Name]),
            GplPhotons = proplists:get_value(
                           globalphotonlight_photons, YafaRay,
                           ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
            GplRadius = proplists:get_value(
                          globalphotonlight_radius, YafaRay,
                          ?DEF_GLOBALPHOTONLIGHT_RADIUS),
            GplDepth = proplists:get_value(
                         globalphotonlight_depth, YafaRay,
                         ?DEF_GLOBALPHOTONLIGHT_DEPTH),
            GplSearch = proplists:get_value(
                          globalphotonlight_search, YafaRay,
                          ?DEF_GLOBALPHOTONLIGHT_SEARCH),
            println(F,"       photons ival=\"~w\" radius=\"~.3f\" "
                    "depth=\"~w\" search=\"~w\">",
                    [GplPhotons,GplRadius,GplDepth,GplSearch]),
            println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
            println(F, "</light>"),
            Bg
    end;

%% Export Area Light

export_light(F, Name, area, OpenGL, YafaRay) ->
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    Power = proplists:get_value(power, YafaRay, ?DEF_ATTN_POWER),
    Samples = proplists:get_value(arealight_samples, YafaRay,
                                  ?DEF_AREALIGHT_SAMPLES),
    Dummy = proplists:get_value(dummy, YafaRay, ?DEF_DUMMY),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    Fs = foldr(fun (Face, Acc) ->
                       e3d_mesh:quadrangulate_face(Face, Vs)++Acc
               end, [], Fs0),
    As = e3d_mesh:face_areas(Fs, Vs),
    Area = foldl(fun (A, Acc) -> A+Acc end, 0.0, As),
    AFs = zip_lists(As, Fs),
    foldl(
      fun ({Af,#e3d_face{vs=VsF}}, I) ->
              case catch Power*Af/Area of
                  {'EXIT',{badarith,_}} -> I;
                  Pwr ->
                      NameI = Name++"_"++integer_to_list(I),
                      [A,B,C,D] = quadrangle_vertices(VsF, VsT),
                      uniprintln(F, "<light name=\"~ts\"> <type sval=\"arealight\"/>"
                                    "<power fval=\"~.3f\"/>~n"
                                    "<samples ival=\"~w\"/>"++
                                  if Dummy -> "";
                                     true ->
                                          ""
                                  end++"",
                              [NameI,Pwr,Samples]++
                                  if Dummy -> [];
                                     true -> []
                                  end),
                      export_rgb(F, color, Color),
                      export_pos(F, corner, A),
                      export_pos(F, from, B),
                      export_pos(F, point1, C),
                      export_pos(F, point2, D),
                      println(F," <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
                      println(F, "</light>"),
                      I+1
              end
      end, 1, AFs),
    undefined;
export_light(_F, Name, Type, _OpenGL, _YafaRay) ->
    io:format(?__(1,"WARNING: Ignoring unknown light \"~ts\" type: ~p")++"~n",
              [Name, format(Type)]),
    undefined.

%% Cut the longest edge of a triangle in half to make it a quad.
%% Lookup vertex positions.
%%
quadrangle_vertices([V1,V2,V3], VsT) ->
    P1 = element(V1+1, VsT),
    P2 = element(V2+1, VsT),
    P3 = element(V3+1, VsT),
    [L12,L23,L31] =
        [e3d_vec:dot(L, L) ||
            L <- [e3d_vec:sub(P1, P2),e3d_vec:sub(P2, P3),
                  e3d_vec:sub(P3, P1)]],
    if L23 > L31 ->
            if L12 > L23 -> [P1,e3d_vec:average([P1,P2]),P2,P3];
               true -> [P1,P2,e3d_vec:average([P2,P3]),P3]
            end;
       true -> [P1,P2,P3,e3d_vec:average([P3,P1])]
    end;
quadrangle_vertices([V1,V2,V3,V4], VsT) ->
    [element(V1+1, VsT),element(V2+1, VsT),
     element(V3+1, VsT),element(V4+1, VsT)].



export_camera(F, Name, Attr) ->
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} =
        proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Lens_Type = proplists:get_value(lens_type, Attr),
    Lens_Ortho_Scale = proplists:get_value(lens_ortho_scale, Attr),
    Lens_Angular_Circular = proplists:get_value(lens_angular_circular, Attr),
    Lens_Angular_Mirrored = proplists:get_value(lens_angular_mirrored, Attr),
    Lens_Angular_Max_Angle = proplists:get_value(lens_angular_max_angle, Attr),
    Lens_Angular_Angle = proplists:get_value(lens_angular_angle, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% YafaRay focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5*Ro)),
    Aperture = proplists:get_value(aperture, Attr),
    uniprintln(F, "<camera name=\"~ts\"> "++
                "<resx ival=\"~w\"/> <resy ival=\"~w\"/> <focal fval=\"~.10f\"/>"++
                if Aperture > 0.0 ->
                        "~n        <dof_distance fval=\"~.10f\"/> <aperture fval=\"~.10f\"/>"
                            "~n        <use_qmc bval=\"~s\"/> <bokeh_type sval=\"~s\"/>"
                            "~n        <bokeh_bias sval=\"~s\"/> <bokeh_rotation fval=\"~.10f\"/> <dof_distance fval=\"~.10f\"/>";
                   true -> ""
                end++


                case Lens_Type of
                    perspective   ->
                        "~n <type sval=\"~s\"/>";

                    orthographic  ->
                        "~n <type sval=\"~s\"/>"
                            "<scale fval=\"~.10f\"/>";

                    architect     ->
                        "~n <type sval=\"~s\"/>";

                    angular           ->
                        "~n <type sval=\"~s\"/>"
                            "<circular bval=\"~s\"/>"
                            "<mirrored bval=\"~s\"/>"
                            "~n <max_angle fval=\"~.10f\"/>"
                            "<angle fval=\"~.10f\"/>"

                end,


            [Name,Width,Height,FocalDist]++
                if Aperture > 0.0 ->
                        [e3d_vec:len(Dir),
                         Aperture,
                         format(proplists:get_value(bokeh_use_QMC, Attr)),
                         format(proplists:get_value(bokeh_type, Attr)),
                         format(proplists:get_value(bokeh_bias, Attr)),
                         proplists:get_value(bokeh_rotation, Attr),
                         proplists:get_value(dof_distance, Attr)];
                   true -> []
                end++


                case Lens_Type of
                    perspective -> [format(Lens_Type)];
                    orthographic  -> [format(Lens_Type),Lens_Ortho_Scale];

                    architect    -> [format(Lens_Type)];
                    angular    -> [format(Lens_Type),
                                   format(Lens_Angular_Circular),
                                   format(Lens_Angular_Mirrored),
                                   Lens_Angular_Max_Angle,
                                   Lens_Angular_Angle]

                end




           ),
    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.



export_background(F, Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafaRay = proplists:get_value(?TAG, Ps, []),
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_AMBIENT),
    CastShadows = proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
    case Bg of
%% Constant Background Export
        constant ->

            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),

            uniprintln(F, "<background name=\"~ts\">",
                [Name]),

            println(F, "<type sval=\"~s\"/>",
                [format(Bg)]),
            BgColor = proplists:get_value(background_color, YafaRay,
                ?DEF_BACKGROUND_COLOR),
            export_rgb(F, color, BgColor),
            ConstantBackPower = proplists:get_value(power, YafaRay,
                ?DEF_POWER),


%% Add Enlight Constant Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Constant Background End


            println(F, "<power fval=\"~w\"/>~n",
                [ConstantBackPower]);


%% Gradient Background Export
        gradientback ->

            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),


            uniprint(F, "<background name=\"~ts\">",
                [Name]),

            println(F, "<type sval=\"~s\"/>",
                [format(Bg)]),
            HorizonColor = proplists:get_value(horizon_color, YafaRay,
                ?DEF_HORIZON_COLOR),
            export_rgb(F, horizon_color, HorizonColor),
            ZenithColor = proplists:get_value(zenith_color, YafaRay,
                ?DEF_ZENITH_COLOR),
            export_rgb(F, zenith_color, ZenithColor),
            GradientBackPower = proplists:get_value(power, YafaRay,
                ?DEF_POWER),

%% Add Enlight Gradient Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Gradient Background End


            println(F, "<power fval=\"~w\"/>~n",
                [GradientBackPower]);

%% Sunsky Background Export
        sunsky ->
%%% 	    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
            Turbidity = proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY),
            A_var = proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR),
            B_var = proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR),
            C_var = proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR),
            D_var = proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR),
            E_var = proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR),

            SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
            SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES),

            SunReal = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            SunRealPower = proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER),


            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            uniprint(F, "<background name=\"~ts\"> <type sval=\"~s\"/>",
                [Name,format(Bg)]),

            println(F, "~n            <turbidity fval=\"~.3f\"/> <a_var fval=\"~.3f\"/>~n"
            "            <b_var fval=\"~.3f\"/> <c_var fval=\"~.3f\"/>~n"
            "            <d_var fval=\"~.3f\"/> <e_var fval=\"~.3f\"/> "
                ,
                [Turbidity,A_var,B_var,C_var,D_var,E_var]),


%% Add Sun Real Start
            case proplists:get_value(sun_real, YafaRay,
                ?DEF_SUN_REAL) of
                true ->
                    SunRealPower = proplists:get_value(sun_real_power, YafaRay,
                        ?DEF_SUN_REAL_POWER),
                    println(F, " <add_sun bval=\"~s\"/><sun_power fval=\"~.3f\"/>",
                        [format(SunReal),SunRealPower]);

                false -> ok
            end,


%% Add Sun Real End

%% Add Skylight Start

            case proplists:get_value(sky_background_light, YafaRay,
                ?DEF_SKY_BACKGROUND_LIGHT) of
                true ->
                    SkyBackgroundPower = proplists:get_value(sky_background_power, YafaRay,
                        ?DEF_SKY_BACKGROUND_POWER),
                    SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay,
                        ?DEF_SKY_BACKGROUND_SAMPLES),
                    println(F, " <background_light bval=\"~s\"/><power fval=\"~.3f\"/><light_samples ival=\"~w\"/>",
                        [format(SkyBackgroundLight),SkyBackgroundPower,SkyBackgroundSamples]);

                false -> ok
            end,
%% Add Skylight End

            export_pos(F, from, Position);

%% Darksky Background Export
        darksky ->
%%% 	    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
            Turbidity = proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY),
            A_var = proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR),
            B_var = proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR),
            C_var = proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR),
            D_var = proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR),
            E_var = proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR),

            DarkskyAltitude = proplists:get_value(darksky_altitude, YafaRay, ?DEF_DARKSKY_ALTITUDE),
            SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
            SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES),

            SunReal = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            SunRealPower = proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER),

            DarkskyNight = proplists:get_value(darksky_night, YafaRay, ?DEF_DARKSKY_NIGHT),
            DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, YafaRay, ?DEF_DARKSKY_DIFFUSEPHOTONS),
            DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, YafaRay, ?DEF_DARKSKY_CAUSTICPHOTONS),

            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            uniprint(F, "<background name=\"~ts\"> <type sval=\"~s\"/>",
                [Name,format(Bg)]),

            println(F, "~n            <turbidity fval=\"~.3f\"/> <a_var fval=\"~.3f\"/>~n"
            "            <b_var fval=\"~.3f\"/> <c_var fval=\"~.3f\"/>~n"
            "            <d_var fval=\"~.3f\"/> <e_var fval=\"~.3f\"/>~n"
            "            <altitude fval=\"~.3f\"/>"
                ,
                [Turbidity,A_var,B_var,C_var,D_var,E_var,DarkskyAltitude]),



%% Add Sun Real Start
            case proplists:get_value(sun_real, YafaRay,
                ?DEF_SUN_REAL) of
                true ->
                    SunRealPower = proplists:get_value(sun_real_power, YafaRay,
                        ?DEF_SUN_REAL_POWER),
                    println(F, " <add_sun bval=\"~s\"/><sun_power fval=\"~.3f\"/>",
                        [format(SunReal),SunRealPower]);

                false -> ok
            end,

%% Add Sun Real End





%% Add Skylight Start

            case proplists:get_value(sky_background_light, YafaRay,
                ?DEF_SKY_BACKGROUND_LIGHT) of
                true ->
                    SkyBackgroundPower = proplists:get_value(sky_background_power, YafaRay,
                        ?DEF_SKY_BACKGROUND_POWER),
                    SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay,
                        ?DEF_SKY_BACKGROUND_SAMPLES),
                    println(F, " <background_light bval=\"~s\"/><power fval=\"~.3f\"/><light_samples ival=\"~w\"/>",
                        [format(SkyBackgroundLight),SkyBackgroundPower,SkyBackgroundSamples]);

                false -> ok
            end,
%% Add Skylight End


%% Add Darksky Photons

            case proplists:get_value(darksky_diffusephotons, YafaRay,
                ?DEF_DARKSKY_DIFFUSEPHOTONS) of
                true ->

                    println(F, " <with_diffuse bval=\"~s\"/>",
                        [format(DarkskyDiffusePhotons)]);

                false -> ok
            end,

            case proplists:get_value(darksky_causticphotons, YafaRay,
                ?DEF_DARKSKY_CAUSTICPHOTONS) of
                true ->

                    println(F, " <with_caustic bval=\"~s\"/>",
                        [format(DarkskyCausticPhotons)]);

                false -> ok
            end,



%% Add Darksky Photons End



%% Add Darksky Night

            case proplists:get_value(darksky_night, YafaRay,
                ?DEF_DARKSKY_NIGHT) of
                true ->

                    println(F, " <night bval=\"~s\"/>",
                        [format(DarkskyNight)]);

                false -> ok
            end,
%% Add Darksky Night End


            export_pos(F, from, Position);


%% HDRI Background Export
        'HDRI' ->
            BgFname = proplists:get_value(background_filename_HDRI, YafaRay,
                ?DEF_BACKGROUND_FILENAME),
            BgExpAdj = proplists:get_value(power, YafaRay,
                ?DEF_POWER),
            BgMapping = proplists:get_value(background_mapping, YafaRay,
                ?DEF_BACKGROUND_MAPPING),

            BgRotation = proplists:get_value(background_rotation, YafaRay,
                ?DEF_BACKGROUND_ROTATION),

            Smartibl_blur = proplists:get_value(smartibl_blur, YafaRay,
                ?DEF_SMARTIBL_BLUR),
                
            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),


            uniprint(F, "<texture name=\"world_texture\">~n"
            "        <filename sval=\"~ts\"/>~n"
            "        <interpolate sval=\"bilinear\"/>~n"
            "        <type sval=\"image\"/>~n"
            "        </texture>",
                [BgFname]),
            uniprintln(F, "~n <background name=\"~ts\"> <type sval=\"textureback\"/> ",
                [Name]),
            println(F, "<power fval=\"~w\"/>~n"
            "<mapping sval=\"~s\"/>~n"
            "<rotation fval=\"~.3f\"/>"
                ,
                [BgExpAdj,format(BgMapping),BgRotation]),

%% Add Enlight HDRI Background Start

            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]),
                    println(F, "<smartibl_blur fval=\"~w\"/>",[Smartibl_blur]);
                    
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,
%% Add Enlight HDRI Background End

            print(F, "<texture sval=\"world_texture\"/>");

%% Image Background Export
        image ->
            BgFname = proplists:get_value(background_filename_image, YafaRay,
                ?DEF_BACKGROUND_FILENAME),
            BgPower = proplists:get_value(power, YafaRay,
                ?DEF_POWER),
            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),
            BgRotation = proplists:get_value(background_rotation, YafaRay,
                ?DEF_BACKGROUND_ROTATION),

            Smartibl_blur = proplists:get_value(smartibl_blur, YafaRay,
                ?DEF_SMARTIBL_BLUR),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),



            uniprint(F, "<texture name=\"world_texture\">~n"
            "        <filename sval=\"~ts\"/>~n"
            "        <interpolate sval=\"bilinear\"/>~n"
            "        <type sval=\"image\"/>~n"
            "        </texture>",
                [BgFname]),
            uniprintln(F, "~n <background name=\"~ts\"> <type sval=\"textureback\"/> ",
                [Name]),

            println(F, " <power fval=\"~.3f\"/>~n"
            "<rotation fval=\"~.3f\"/>"
                , [BgPower,BgRotation]),

%% Add Enlight Image Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]),
                    println(F, "<smartibl_blur fval=\"~w\"/>",[Smartibl_blur]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Image Background End


            println(F, "    <texture sval=\"world_texture\" />")
    end,
    println(F, "    <cast_shadows bval=\"~s\"/>", [format(CastShadows)]),
    println(F, "</background>").




export_render(F, CameraName, BackgroundName, Attr) ->
    ComputerNode = get_pref(computer_node, Attr),
    AA_passes = proplists:get_value(aa_passes, Attr),
    AA_minsamples = proplists:get_value(aa_minsamples, Attr),
    AA_incsamples = proplists:get_value(aa_incsamples, Attr),
    AA_pixelwidth = proplists:get_value(aa_pixelwidth, Attr),
    AA_threshold = proplists:get_value(aa_threshold, Attr),
    AA_noise_resampled_floor = proplists:get_value(aa_noise_resampled_floor, Attr),
    AA_noise_sample_multiplier_factor = proplists:get_value(aa_noise_sample_multiplier_factor, Attr),
    AA_noise_light_sample_multiplier_factor = proplists:get_value(aa_noise_light_sample_multiplier_factor, Attr),
    AA_noise_indirect_sample_multiplier_factor = proplists:get_value(aa_noise_indirect_sample_multiplier_factor, Attr),
    AA_noise_detect_color_noise = proplists:get_value(aa_noise_detect_color_noise, Attr),
    AA_noise_dark_detection_type = proplists:get_value(aa_noise_dark_detection_type, Attr),
    AA_noise_dark_threshold_factor = proplists:get_value(aa_noise_dark_threshold_factor, Attr),
    AA_noise_variance_edge_size = proplists:get_value(aa_noise_variance_edge_size, Attr),
    AA_noise_variance_pixels = proplists:get_value(aa_noise_variance_pixels, Attr),
    AA_noise_clamp_samples = proplists:get_value(aa_noise_clamp_samples, Attr),
    AA_noise_clamp_indirect = proplists:get_value(aa_noise_clamp_indirect, Attr),
    BackgroundTransp = proplists:get_value(background_transp, Attr),
    BackgroundTranspRefract = proplists:get_value(background_transp_refract, Attr),
    AA_Filter_Type = proplists:get_value(aa_filter_type, Attr),
    SaveAlpha = proplists:get_value(save_alpha, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    TransparentShadows = proplists:get_value(transparent_shadows, Attr),
    ShadowDepth = proplists:get_value(shadow_depth, Attr),
    TileSize = proplists:get_value(tile_size, Attr),
    TileOrder = proplists:get_value(tile_order, Attr),
    Gamma = proplists:get_value(gamma, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    UseCaustics = proplists:get_value(use_caustics, Attr),
    Caustic_Photons = proplists:get_value(caustic_photons, Attr),
    Caustic_Depth = proplists:get_value(caustic_depth, Attr),
    Caustic_Mix = proplists:get_value(caustic_mix, Attr),
    Caustic_Radius = proplists:get_value(caustic_radius, Attr),
    Do_AO = proplists:get_value(do_ao, Attr),
    AO_Distance = proplists:get_value(ao_distance, Attr),
    AO_Samples = proplists:get_value(ao_samples, Attr),
    AO_Color = 	proplists:get_value(ao_color,Attr),
    Lighting_Method = proplists:get_value(lighting_method, Attr),
    PM_Diffuse_Photons = proplists:get_value(pm_diffuse_photons, Attr),
    PM_Bounces = proplists:get_value(pm_bounces, Attr),
    PM_Search = proplists:get_value(pm_search, Attr),
    PM_Diffuse_Radius = proplists:get_value(pm_diffuse_radius, Attr),
    PM_Caustic_Photons = proplists:get_value(pm_caustic_photons, Attr),
    PM_Caustic_Radius =  proplists:get_value(pm_caustic_radius, Attr),
    PM_Caustic_Mix = proplists:get_value(pm_caustic_mix, Attr),
    PM_Use_FG = proplists:get_value(pm_use_fg, Attr),
    PM_FG_Bounces = proplists:get_value(pm_fg_bounces, Attr),
    PM_FG_Samples = proplists:get_value(pm_fg_samples, Attr),
    PM_FG_Show_Map = proplists:get_value(pm_fg_show_map, Attr),
    PT_Diffuse_Photons = proplists:get_value(pt_diffuse_photons, Attr),
    PT_Bounces = proplists:get_value(pt_bounces, Attr),
    PT_Caustic_Type = proplists:get_value(pt_caustic_type, Attr),
    PT_Caustic_Radius =  proplists:get_value(pt_caustic_radius, Attr),
    PT_Caustic_Mix = proplists:get_value(pt_caustic_mix, Attr),
    PT_Caustic_Depth = proplists:get_value(pt_caustic_depth, Attr),
    PT_Samples = proplists:get_value(pt_samples, Attr),
    SPPM_Photons = proplists:get_value(sppm_photons, Attr),
    SPPM_Bounces = proplists:get_value(sppm_bounces, Attr),
    SPPM_Search = proplists:get_value(sppm_search, Attr),
    SPPM_Radius = proplists:get_value(sppm_radius, Attr),
    SPPM_Times = proplists:get_value(sppm_times, Attr),
    SPPM_Passes = proplists:get_value(sppm_passes, Attr),
    SPPM_Ire = proplists:get_value(sppm_ire, Attr),
    Volintegr_Type = proplists:get_value(volintegr_type, Attr),
    Volintegr_Adaptive = proplists:get_value(volintegr_adaptive, Attr),
    Volintegr_Optimize = proplists:get_value(volintegr_optimize, Attr),
    Volintegr_Stepsize = proplists:get_value(volintegr_stepsize, Attr),
    ThreadsAuto = proplists:get_value(threads_auto, Attr),
    ThreadsNumber = proplists:get_value(threads_number, Attr),
    Shadow_bias_auto = proplists:get_value(shadow_bias_auto, Attr),
    Shadow_bias = proplists:get_value(shadow_bias, Attr),
    Ray_mindist_auto = proplists:get_value(ray_mindist_auto, Attr),
    Ray_mindist = proplists:get_value(ray_mindist, Attr),
    Img_autosave_type = proplists:get_value(img_autosave_type, Attr),
    Img_autosave_passes = proplists:get_value(img_autosave_passes, Attr),
    Img_autosave_seconds = proplists:get_value(img_autosave_seconds, Attr),
    Img_denoise_enable = proplists:get_value(img_denoise_enable, Attr),
    Img_denoise_mix = proplists:get_value(img_denoise_mix, Attr),
    Img_denoise_h_lum = proplists:get_value(img_denoise_h_lum, Attr),
    Img_denoise_h_chrom = proplists:get_value(img_denoise_h_chrom, Attr),
    Film_processing = proplists:get_value(film_processing, Attr),
    Film_binary_format = proplists:get_value(film_binary_format, Attr),
    Film_autosave_type = proplists:get_value(film_autosave_type, Attr),
    Film_autosave_passes = proplists:get_value(film_autosave_passes, Attr),
    Film_autosave_seconds = proplists:get_value(film_autosave_seconds, Attr),

    
    println(F," "),
    println(F, "<integrator name=\"default\">"),




    case Lighting_Method of
        directlighting ->
            println(F," "),
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<bg_transp bval=\"~s\"/>",[format(BackgroundTransp)]),
            println(F, "<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
            println(F, "<raydepth ival=\"~w\"/>",[Raydepth]),
            println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
            println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),
            println(F," ");

        photonmapping ->
            println(F," "),
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<bg_transp bval=\"~s\"/>",[format(BackgroundTransp)]),
            println(F, "<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
            println(F, "<raydepth ival=\"~w\"/>",[Raydepth]),
            println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
            println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),
            println(F, "<photons ival=\"~w\"/>",[PM_Diffuse_Photons]),
            println(F, "<bounces ival=\"~w\"/>",[PM_Bounces]),
            println(F, "<search ival=\"~w\"/>",[PM_Search]),
            println(F, "<diffuseRadius fval=\"~.10f\"/>",[PM_Diffuse_Radius]),
            println(F, "<cPhotons ival=\"~w\"/>",[PM_Caustic_Photons]),
            println(F, "<causticRadius fval=\"~.10f\"/>",[PM_Caustic_Radius]),
            println(F, "<caustic_mix ival=\"~w\"/>",[PM_Caustic_Mix]),
            println(F, "<finalGather bval=\"~s\"/>",[PM_Use_FG]),
            println(F, "<fg_bounces ival=\"~w\"/>",[PM_FG_Bounces]),
            println(F, "<fg_samples ival=\"~w\"/>",[PM_FG_Samples]),
            println(F, "<show_map bval=\"~s\"/>",[PM_FG_Show_Map]),
            println(F," ");

        pathtracing ->
            println(F," "),
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<bg_transp bval=\"~s\"/>",[format(BackgroundTransp)]),
            println(F, "<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
            println(F, "<raydepth ival=\"~w\"/>",[Raydepth]),
            println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
            println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),
            println(F, "<photons ival=\"~w\"/>",[PT_Diffuse_Photons]),
            println(F, "<bounces ival=\"~w\"/>",[PT_Bounces]),
            println(F, "<caustic_type sval=\"~s\"/>",[PT_Caustic_Type]),
            println(F, "<caustic_radius fval=\"~.10f\"/>",[PT_Caustic_Radius]),
            println(F, "<caustic_mix ival=\"~w\"/>",[PT_Caustic_Mix]),
            println(F, "<caustic_depth ival=\"~w\"/>",[PT_Caustic_Depth]),
            println(F, "<path_samples ival=\"~w\"/>",[PT_Samples]),
            println(F," ");

        bidirectional ->
            println(F," "),
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<bg_transp bval=\"~s\"/>",[format(BackgroundTransp)]),
            println(F, "<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
            println(F, "<raydepth ival=\"~w\"/>",[Raydepth]),
            println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
            println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),
            println(F," ");

        sppm ->
            println(F," "),
            println(F, "<type sval=\"SPPM\"/>"),
            println(F, "<bg_transp bval=\"~s\"/>",[format(BackgroundTransp)]),
            println(F, "<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
            println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
            println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),
            println(F, "<photons ival=\"~w\"/>",[SPPM_Photons]),
            println(F, "<bounces ival=\"~w\"/>",[SPPM_Bounces]),
            println(F, "<searchNum ival=\"~w\"/>",[SPPM_Search]),
            println(F, "<photonRadius fval=\"~.10f\"/>",[SPPM_Radius]),
            println(F, "<times fval=\"~.10f\"/>",[SPPM_Times]),
            println(F, "<passNums ival=\"~w\"/>",[SPPM_Passes]),
            println(F, "<pmIRE bval=\"~s\"/>",[format(SPPM_Ire)]),
            println(F," ")

    end,




    case UseCaustics of
        true ->
            println(F, "<caustics bval=\"true\"/>"),
            println(F, "<photons ival=\"~w\"/>",[Caustic_Photons]),
            println(F, "<caustic_depth ival=\"~w\"/>",[Caustic_Depth]),
            println(F, "<caustic_mix ival=\"~w\"/>",[Caustic_Mix]),
            println(F, "<caustic_radius fval=\"~.10f\"/>",[Caustic_Radius]);

        false ->
            println(F, "<caustics bval=\"false\"/>")

    end,

    case Do_AO of
        true ->
            println(F, "<do_AO bval=\"true\"/>"),
            println(F, "<AO_distance fval=\"~.10f\"/>",[AO_Distance]),
            println(F, "<AO_samples fval=\"~.10f\"/>",[AO_Samples]),
            export_rgb(F, "AO_color",AO_Color);


        false ->
            println(F, "<do_AO bval=\"false\"/>")

    end,


    println(F, "</integrator>"),



    case Volintegr_Type of
        none ->
            println(F," "),
            println(F, "<integrator name=\"volintegr\">"),
            println(F, "<type sval=\"~s\"/>",[Volintegr_Type]),
            println(F, "</integrator>"),
            println(F," ");

        singlescatterintegrator ->
            println(F," "),
            println(F, "<integrator name=\"volintegr\">"),
            println(F, "<type sval=\"SingleScatterIntegrator\"/>"),
            println(F, "<adaptive bval=\"~s\"/>",[format(Volintegr_Adaptive)]),
            println(F, "<optimize bval=\"~s\"/>",[format(Volintegr_Optimize)]),
            println(F, "<stepSize fval=\"~.10f\"/>",[Volintegr_Stepsize]),
            println(F, "</integrator>"),
            println(F," ")
    end,

    uniprintln(F, "<render>~n"
    "        <adv_computer_node ival=\"~w\"/>~n"
    "        <camera_name sval=\"~ts\"/>~n"
    "        <filter_type sval=\"~s\"/>~n"
    "        <AA_passes ival=\"~w\"/>~n"
    "        <AA_threshold fval=\"~.10f\"/>~n"
    "        <AA_minsamples ival=\"~w\"/>~n"
    "        <AA_inc_samples ival=\"~w\"/>~n"
    "        <AA_pixelwidth fval=\"~.10f\"/>~n"
    "        <AA_resampled_floor fval=\"~.10f\"/>~n"
    "        <AA_sample_multiplier_factor fval=\"~.10f\"/>~n"
    "        <AA_light_sample_multiplier_factor fval=\"~.10f\"/>~n"
    "        <AA_indirect_sample_multiplier_factor fval=\"~.10f\"/>~n"
    "        <AA_detect_color_noise bval=\"~s\"/>~n"
    "        <AA_dark_detection_type sval=\"~s\"/>~n"
    "        <AA_dark_threshold_factor fval=\"~.10f\"/>~n"
    "        <AA_variance_edge_size ival=\"~w\"/>~n"
    "        <AA_variance_pixels ival=\"~w\"/>~n"
    "        <AA_clamp_samples fval=\"~.10f\"/>~n"
    "        <AA_clamp_indirect fval=\"~.10f\"/>~n"++
        case SaveAlpha of
            premultiply ->
                "        <premult bval=\"true\"/>~n";
            _ -> ""
        end++
    "        <background_name sval=\"~ts\"/>~n"++
    "        <width ival=\"~w\"/> <height ival=\"~w\"/>~n"
    "        <gamma fval=\"~.10f\"/>~n"
    "        <adv_auto_shadow_bias_enabled bval=\"~s\"/>~n"
    "        <adv_shadow_bias_value fval=\"~.10f\"/>~n"
    "        <adv_auto_min_raydist_enabled bval=\"~s\"/>~n"
    "        <adv_min_raydist_value fval=\"~.10f\"/>~n"
    "        <tile_size ival=\"~w\"/>~n"
    "        <tiles_order sval=\"~s\"/>~n"
    "        <film_autosave_interval_passes ival=\"~w\"/>~n"
    "        <film_autosave_interval_seconds fval=\"~.10f\"/>~n"
    "        <film_autosave_interval_type sval=\"~s\"/>~n"
    "        <film_save_binary_format bval=\"~s\"/>~n"
    "        <film_save_load sval=\"~s\"/>~n"
    "        <images_autosave_interval_passes ival=\"~w\"/>~n"
    "        <images_autosave_interval_seconds fval=\"~.10f\"/>~n"
    "        <images_autosave_interval_type sval=\"~s\"/>~n"
    "        <denoiseEnabled bval=\"~s\"/>~n"
    "        <denoiseHLum ival=\"~w\"/>~n"
    "        <denoiseHCol ival=\"~w\"/>~n"
    "        <denoiseMix fval=\"~.10f\"/>~n"
    "        ",
        [ComputerNode,CameraName,AA_Filter_Type,AA_passes,AA_threshold,
            AA_minsamples,AA_incsamples,AA_pixelwidth,AA_noise_resampled_floor,AA_noise_sample_multiplier_factor,AA_noise_light_sample_multiplier_factor,AA_noise_indirect_sample_multiplier_factor,AA_noise_detect_color_noise,AA_noise_dark_detection_type,AA_noise_dark_threshold_factor,AA_noise_variance_edge_size,AA_noise_variance_pixels,AA_noise_clamp_samples,AA_noise_clamp_indirect,BackgroundName]++
            [Width,Height,Gamma]++[Shadow_bias_auto, Shadow_bias, Ray_mindist_auto, Ray_mindist]++[TileSize, TileOrder]++[Film_autosave_passes, Film_autosave_seconds, Film_autosave_type, Film_binary_format, Film_processing, Img_autosave_passes, Img_autosave_seconds, Img_autosave_type, Img_denoise_enable, Img_denoise_h_lum, Img_denoise_h_chrom, Img_denoise_mix]),

    println(F, "<integrator_name sval=\"default\"/>"),

    case ThreadsAuto of
        true -> println(F, "<threads ival=\"-1\"/>");

        false -> println(F, "<threads ival=\"~w\"/>",[ThreadsNumber])

    end,

    println(F, "<volintegrator_name sval=\"volintegr\"/>"),
    println(F, "</render>"),


    %% Render Passes

    println(F, "<render_passes name=\"render_passes\">"),

    {_,Max} = range_1(render_passes),

    lists:foldl(fun(N, _) ->
        RPValue = proplists:get_value(render_pass_id(N), Attr),
        case RPValue of
            undefined -> println(F, "	<pass_RenderPass_~w sval=\"disabled\"/>",[N]);
            _ -> println(F, "	<pass_RenderPass_~w sval=\"~s\"/>",[N,RPValue])
        end
    end, [], lists:seq(1,Max)),
    println(F, "</render_passes>").


export_logging_badge(F, Attr) ->
    LogSaveTxt = proplists:get_value(log_save_txt, Attr),
    LogSaveHtml = proplists:get_value(log_save_html, Attr),
    BadgePosition = proplists:get_value(badge_position, Attr),
    BadgeTitle = proplists:get_value(badge_title, Attr),
    BadgeAuthor = proplists:get_value(badge_author, Attr),
    BadgeContact = proplists:get_value(badge_contact, Attr),
    BadgeComments = proplists:get_value(badge_comments, Attr),
    BadgeCustomIconPath = proplists:get_value(badge_custom_icon_path, Attr),
    BadgeDrawRenderSettings = proplists:get_value(badge_draw_render_settings, Attr),
    BadgeDrawAANoiseSettings = proplists:get_value(badge_draw_aa_noise_settings, Attr),
    Badge_font_path = proplists:get_value(badge_font_path, Attr),
    Badge_font_size_factor = proplists:get_value(badge_font_size_factor, Attr),
    
    println(F, "<logging_badge name=\"logging_badge\">"),
    println(F, "	<logging_saveLog bval=\"~ts\"/>", [LogSaveTxt]),
    println(F, "	<logging_saveHTML bval=\"~ts\"/>", [LogSaveHtml]),
    println(F, "	<logging_paramsBadgePosition sval=\"~s\"/>", [BadgePosition]),
    uniprintln(F, " <logging_title sval=\"~ts\"/>", [BadgeTitle]),
    uniprintln(F, "	<logging_author sval=\"~ts\"/>", [BadgeAuthor]),
    uniprintln(F, "	<logging_contact sval=\"~ts\"/>", [BadgeContact]),
    uniprintln(F, "	<logging_comments sval=\"~ts\"/>", [BadgeComments]),
    uniprintln(F, "	<logging_customIcon sval=\"~ts\"/>", [BadgeCustomIconPath]),
    println(F, "	<logging_drawRenderSettings bval=\"~s\"/>", [BadgeDrawRenderSettings]),
    println(F, "	<logging_drawAANoiseSettings bval=\"~s\"/>", [BadgeDrawAANoiseSettings]),
    uniprintln(F, "	<logging_fontPath sval=\"~ts\"/>", [Badge_font_path]),
    println(F, "	<logging_fontSizeFactor fval=\"~.10f\"/>", [Badge_font_size_factor]),
    println(F, "</logging_badge>").


%%% Noisy file output functions. Fail if anything goes wrong.
%%%

open(Filename, export) ->
    case file:open(Filename, [write,{encoding, utf8},delayed_write]) of
        {ok, F} ->
            F;
        Error ->
            erlang:error(Error, [Filename, export])
    end.

println(F) ->
    println(F, "").

print(F, DeepString) ->
    case file:write(F, DeepString) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,DeepString])
    end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,DeepString])
    end.

print(F, Format, Args) ->
    case file:write(F, io_lib:format(Format, Args)) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,Format,Args])
    end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,Format,Args])
    end.

uniprint(F, Format, Args) ->
    UnicodeText = io_lib:format(Format, Args),
    io:put_chars(F, unicode:characters_to_binary(UnicodeText)).

uniprintln(F, Format, Args) ->
    UnicodeText = io_lib:format(Format, Args) ++ io_lib:nl(),
    io:put_chars(F, unicode:characters_to_binary(UnicodeText)).

close(F) ->
    case file:close(F) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F])
    end.



%% Convert certain terms to printable strings in a
%% hopefully efficient way.

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->
            [$-,integer_to_list(I)|format_decimals(D)];
       true ->
            [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->
    integer_to_list(I);
format(true) ->
    "true";
format(false) ->
    "false";
format(A) when is_atom(A) ->
    atom_to_list(A);
format(L) when is_list(L) ->
    L.

format_decimals(F) when is_float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(+0.0) ->
    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(+0.0) ->
    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
            [$0,(I+$0)|format_decimals_3(D)];
       true ->
            [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(+0.0) ->
    [];
format_decimals_3(F) when is_float(F) ->
    G = 1000.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
            [$0,$0,(I+$0)|format_decimals_4(D)];
       I < 100 ->
            [$0,integer_to_list(I)|format_decimals_4(D)];
       true ->
            [integer_to_list(I)|format_decimals_4(D)]
    end.

format_decimals_4(+0.0) ->
    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
            if I < 10 ->
                    [$0,$0,$0,(I+$0)];
               true ->
                    [$0,$0|integer_to_list(I)]
            end;
       true ->
            if I < 1000 ->
                    [$0|integer_to_list(I)];
               true ->
                    integer_to_list(I)
            end
    end.


%%% Set and get preference variables saved in the .wings file for this module
set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

set_user_prefs(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, [H|_]=KeyDefs) when is_tuple(H)  ->
    case proplists:lookup(Key,KeyDefs) of
        {Key, Def} -> Def;
        Def -> Def
    end,
    get_pref(Key,Def);
get_pref(Key, Def) ->
    [{Key,Val}] = get_prefs([{Key,Def}]),
    Val.

get_prefs(KeyDefs) when is_list(KeyDefs) ->
    get_prefs_1(KeyDefs, make_ref()).

get_prefs_1([], _Undefined) ->
    [];
get_prefs_1([{Key,Def}|KeyDefs], Undefined) ->
    [{Key,case wpa:scene_pref_get(?MODULE, Key, Undefined) of
              Undefined ->
                  wpa:pref_get(?MODULE, Key, Def);
              Val ->
                  Val
          end}|get_prefs_1(KeyDefs, Undefined)].

get_user_prefs(KeyDefs) when is_list(KeyDefs) ->
    [{Key,wpa:pref_get(?MODULE, Key, Def)} || {Key,Def} <- KeyDefs].

%% Set and get global variables (in the process dictionary)
%% per wings session for this module.
set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    ets:insert(?LOCAL_MODULE, {Name,Value}).

get_var(Name) ->
    case ets:lookup(?LOCAL_MODULE, Name) of
        [] -> undefined;
        [{Name,Val}] -> Val
    end.

erase_var(Name) ->
    ets:delete(?LOCAL_MODULE, Name).

menu_shader() ->
    [{?__(1,"Shiny Diffuse"),shinydiffuse},
        {?__(2,"Glass"),glass},
        {?__(3,"Rough Glass"),rough_glass},
        {?__(4,"Glossy"),glossy},
        {?__(5,"Coated Glossy"),coatedglossy},
        {?__(7,"Light Material"),lightmat},
        {?__(8,"Blend"),blend_mat}].


%%% pulls out all the values stored as {{KeyTag, SubKey}, Value}
%%% returns {ListOfFound, ListRemaining}
%%% ListOfFound is a list of {SubKey, Value}
rip_all(KeyTag, List) ->
    Keys = proplists:get_keys(List),
    rip_all(KeyTag, Keys, List).
rip_all(KeyTag, [Key | Keys], List) ->
    case rip_keytag(KeyTag, Key) of
	true ->
	    {_SetTag, SubTag} = Key,
	    Value = proplists:get_value(Key, List),
	    ListNext = proplists:delete(Key, List),
	    {Found, Remaining} = rip_all(KeyTag, Keys, ListNext),
	    {[{SubTag, Value} | Found], Remaining};
	false ->
	    rip_all(KeyTag, Keys, List)
    end;
rip_all(_K, _KL, List) ->
    {[], List}.

rip_keytag(KeyTag, {SetTag, _}) ->
    case KeyTag of
	SetTag -> true;
	_ -> false
    end;
rip_keytag(_KT, _ST) ->
    false.


%% Zip lists together into a list of tuples
%%
zip_lists([], []) -> [];
zip_lists([H1|T1], [H2|T2]) -> [{H1,H2}|zip_lists(T1, T2)].



%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end, List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%%     true -> filter2_1(Pred, T, [H|True], False);
%%%     false -> filter2_1(Pred, T, True, [H|False])
%%%     end.

max(X, Y) when X > Y -> X;
max(_, Y) -> Y.



-ifdef(print_mesh_1).
print_mesh(#e3d_mesh{type=T,vs=Vs,vc=Vc,tx=Tx,ns=Ns,fs=Fs,he=He,matrix=M}) ->
    io:format("#e3d_mesh{type=~p,~nvs=~p,~nvc=~p,~ntx=~p,~nns=~p,~nfs=~p,~n"
              "he=~p,~nmatrix=~p}.~n",
              [T,Vs,Vc,Tx,Ns,Fs,He,M]).
-endif.

help_export() ->
    Title = help(title, {options_dialog,general}),
    Text = help(text, {options_dialog,general}),
    {Title,Text}.

help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.

help(title, {material_dialog,object}) ->
    ?__(6,"YafaRay Material Properties: Object Parameters");
help(text, {material_dialog,object}) ->
    [?__(7,"Object Parameters are applied to whole objects, namely those "
      "that have this material on a majority of their faces."),
    ?__(8,"Mesh: Standard 3D mesh."),
    % FIXME: This is not really good, volume Min/Max coordinates should be automatically calculated based on the object bounds, not entered manually by the user. This is counter-intuitive.
    ?__(9,"Volume: Defines an area for Volumetrics. "
      "Control simulated size by adjusting Min/Max settings. "
      "When using the Noise option, a Texture must also be defined in the Material "
      "Properties, in that case the first texture will be used. Volumetrics must also be enabled under YafaRay Render Options."),
    ?__(10,"Mesh Light: Use for Neon lights or other glowing meshes. "
      "Limit the number of mesh lights in your scene, since render times are longer. "
      "Mesh Lights provide faster rendering than converting meshes to area lights "
      "with Object to Area Light."),
    ?__(11,"Light Portal: Controls light and photons coming through a window in a closed room. "
      "Apply to a flat plane. Light Portals are used to reduce render times."),
    ?__(12,"Autosmooth Angle: Controls YafaRay simulated smoothing of a mesh. "
      "For best results, adjust the Subdivisions setting under YafaRay Render "
      "Options to control real mesh smoothing."),
    [{bold,?__(100,"Material Visibity")}],
    [{bullet,[?__(101,"Normal: visible and casting shadows."),
              ?__(102,"No shadows: visible but not casting shadows."),
              ?__(103,"Shadows only: invisible but casting shadows."),
              ?__(104,"Invisible: totally invisible material.")]}]];
%%help(title, {material_dialog,fresnel}) ->
%%    ?__(15,"YafaRay Material Properties: Fresnel Parameters");
%%help(text, {material_dialog,fresnel}) ->
%%    [?__(16,"Fresnel Parameters affect how rays reflect off and refract in "
%%      "glass-like materials. This is a different light model than the "
%%      "OpenGL (Diffuse,Specular,Shininess) model and they do not often "
%%      "go well together. "
%%      "A Photon Light must be present to produce Caustics."),
%%     ?__(17,"Mapping to YafaRay shader parameters:"),
%%     ?__(18,"Index Of Refraction -> 'ior' -> 1.5 for Glass/Caustics."),
%%     ?__(19,"Total Internal Reflection -> 'tir' -> Enable for Glass."),
%%     ?__(20,"Minimum Reflection -> 'min_refle' -> 1.0 for Metal."),
%%     ?__(21,"Reflected -> 'reflected' -> Reflective Caustics."),
%%     ?__(22,"Transmitted -> 'transmitted' -> Glass/Refractive Caustics."),
%%     ?__(23,"Use Default -> Sets 'transmitted' to Diffuse * (1 - Opacity). "
%%      "This makes a semi-transparent object in OpenGL look the same in "
%%      "YafaRay provided that Index Of Refraction is 1.1 minimum."),
%%     ?__(24,"Grazing Angle Colors -> Use the secondary Reflected and Transmitted "
%%      "colors following that show from grazing angles of the material. "
%%      "For a glass with green edges set Transmitted to white and "
%%      "Grazing Angle Transmitted to green."),
%%     ?__(25,"Absorption -> Sets the desired color for white light travelling "
%%      "the given distance through the material.")];
%%%%
help(title, {light_dialog,_}) ->
    ?__(26,"YafaRay Light Properties");
help(text, {light_dialog,Type}) ->
    [?__(27,"Diffuse Color and Spot Light Angle are the only OpenGL properties "
        " that map to the YafaRay light settings.")]++
    help(text,{light,Type});
help(text,{light,point}) ->
    [[{bold,?__(28,"Point Light")}],
        ?__(29,"A light with rays pointing in every direction. Use for a candle flame, "
        "gas light, or light bulb. Choose either Point Light or Sphere Light, which has "
        "the added options of setting the Radius and Samples.")];
help(text,{light,spot}) ->
    [[{bold,?__(30,"Spot Light")}],
     ?__(31,"A light with rays focused on a certain area.\n"
        "Choose either Spotlight or IES. The Photon Only option is good for enhancing "
        "refractive and reflective caustic patterns. The IES option enables the use of "
        "IES files to simulate real world lights, which produce uniquely shaped lighting.")];
help(text,{light,infinite}) ->
    [[{bold,?__(76,"Infinite Light")}],
        ?__(77,"A distant light with rays pointing in a certain direction. Typically used "
        "for Sunlight.\n"
        "Choose either Sunlight or Directional. Combine with the included Sunsky or Darktide "
        "Sunsky to simulate a sky background. Enable Skylight to emit light from the sky "
        "background. Enable Real Sun to show a sun disc. The camera must be facing into the "
        "rays of the infinite light in order to see the sun disc. With Darktide Sunsky, "
        "enable the Night option to simulate moon light.")];
help(text,{light,ambient}) ->
    [[{bold,?__(78,"Ambient Light")}],
        ?__(79,"A light with rays pointing in every direction and emitting from all "
        "directions, with no shadows.\n"
        "Choose between the various Background Light/Environment options to control "
        "the appearance of the background and lighting. Disable the Enlight option if "
        "no lighting is wanted. The HDRI option enables the use of real world environments "
        "to reflect onto reflective surfaces. HDRI also produces realistic lighting "
        "when Enlight is enabled.")];
help(text,{light,area}) ->
    [[{bold,?__(80,"Area Light")}],
        ?__(81,"A rectangular light with rays emitting from the entire surface. Use for "
        "light coming through a window or fluorescent ceiling lights.\n"
        "Wings3D objects can be converted to Area Lights with the Object to Area Light "
        "command. Expect longer render times when using Area Lights.")];

help(title, pref_dialog) ->
    ?__(33,"YafaRay Options");
help(text, pref_dialog) ->
    [?__(34,"These are user preferences for the YafaRay exporter plugin"),
     ?__(35,"Automatic Dialogs: ")
     ++wings_help:cmd([?__(36,"File"),?__(37,"Export"),?__(38,"YafaRay")])++", "
     ++wings_help:cmd([?__(39,"File"),?__(40,"Export Selected"),?__(41,"YafaRay")])++" "++?__(42,"and")++" "
     ++wings_help:cmd([?__(43,"File"),?__(44,"Render"),?__(45,"YafaRay")])++" "++
     ?__(46,"are enabled if the rendering executable is found (in the path), "
        "or if the rendering executable is specified with an absolute path."),
     %%
     ?__(47,"Disabled Dialogs:")++" "
     ++wings_help:cmd([?__(48,"File"),?__(49,"Export"),?__(50,"YafaRay")])++", "
     ++wings_help:cmd([?__(51,"File"),?__(52,"Export Selected"),?__(53,"YafaRay")])++" "++?__(54,"and")++" "
     ++wings_help:cmd([?__(55,"File"),?__(56,"Render"),?__(57,"YafaRay")])++" "++
     ?__(58,"are disabled."),
     %%
     ?__(59,"Enabled Dialogs:")++" "
     ++wings_help:cmd([?__(60,"File"),?__(61,"Export"),?__(62,"YafaRay")])++" "++?__(63,"and")++" "
     ++wings_help:cmd([?__(64,"File"),?__(65,"Export Selected"),?__(66,"YafaRay")])++" "++
     ?__(67,"are always enabled, but")++" "
     ++wings_help:cmd([?__(68,"File"),?__(69,"Render"),?__(70,"YafaRay")])++" "++
     ?__(71,"is the same as for \"Automatic Dialogs\"."),
     %%
     ?__(72,"Executable: The rendering command for the YafaRay raytrace "
        "renderer ('c:/yafaray_v3/bin/yafaray-xml.exe') that is supposed to "
        "be found in the executables search path; or, the absolute path of "
        "that executable."),
     %%
     ?__(74,"Options: Rendering command line options to be inserted between the "
      "executable and the .xml filename.")];

help(title, {options_dialog,_}) ->
    ?__(811,"YafaRay Render Options");
help(text, {options_dialog,general}) ->
    [[{bold,?__(82,"Transparent Shadows:")++" "}],
        ?__(83,"Enable when a Glass material with Fake Shadows is used and when Transparency "
        "Maps are used."),
        [{bold,?__(84,"Transparent Refraction:")++" "}],
        ?__(85,"Disable if using Alpha Channel in Background, when Glass materials are used, "
        "and a refracted background needs to appear in the glass."),
     [{bold,?__(86,"Bidirectional Path Tracing:")++" "}],
        ?__(87,"This lighting method in the Lighting tab requires a high number of Passes "
        "in the Anti-Aliasing section of the General Options tab. Anti-Aliasing Threshold "
        "should be set to 0.0."),
     [{bold,?__(90,"Volumetrics:")++" "}],
        ?__(91,"This feature in the Lighting tab, adds fog or clouds to your scene, assuming "
        "there is a Volume object. For noise volumes, the first texture in the associated material will be used."),
     [{bold,?__(92,"Camera Width and Height:")++" "}],
        ?__(93,"This setting, in the Camera tab, controls the size of the rendered image. The size "
        "of the Geometry window affects camera framing. The proportions for Camera Width and "
        "Height and the Geometry window  width and height need to be the same.  For example, to "
        "render an (800 x 600) image the Geometry window needs to be (800 x 600) or (400 x 300) in size."),
     [{bold,?__(94,"Depth of Field (DOF):")++" "}],
        ?__(95,"Use the \"pinhole\" f-stop setting, in the Camera tab, to disable Depth of Field. "
        "Using Depth of Field results in longer render times. Consider simulating DOF with an "
        "image editor, if DOF is needed and render times are too long."),
     [{bold,?__(96,"No image rendered:")++" "}],
        ?__(97,"Check the Log Window for information. When rendering with Lighting methods with "
        "Global Illumination, no image will be rendered if not enough photons are present.  "
        "Try adding more lights to your scene."),
     [{bold,?__(98,"Rendering Error:")++" "}],
        ?__(99,"Check the Log Window for information. The error message may be the result of enabling "
        "Subsurface Scattering, in the Lighting tab, when using a version of YafaRay that does not "
        "support that feature.")
    ].
