%%
%%  pbr_film.erl
%%
%%     Pbr film handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%
%%  

-module(pbr_film).
-export([init/2, 
	 splat/3, get_sample_buffer/1, splat_sample_buffer/2, set_sample_frame_buffer/2,
	 resolution/1,
	 show/1,
	 %% Testing
	 gamma_table/1, gaussion_table/0
	]).

-include_lib("wings/src/wings.hrl").
-include("pbr.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-record(f, {type,			   % raw=erlang | opencl
	    res,			   % Resolution
	    filter,			   % Filter type
	    fb,				   % Framebuffer
	    sfb,			   % CL SampleFrame
	    sqb,			   % SampleQueueBuff
	    sq = {0,[]},                   % SampleQ
	    gamma_table,		   % Gamma lookup table
	    gammab,			   % CL Gamma table buffer
	    gausb}).			   % CL Gaussian filter buffer

-define(GAMMA_DX, 1.0/(?GAMMA_TABLE_SZ-1)).
-define(GAUSSION_FILTER_SZ, 16).

init(Attrs, R0 = #renderer{}) ->
    {W,H} = pbr_camera:get_size(R0),
    Raw = array:new([{default, {{0.0,0.0,0.0},0}},  {size, W*H}]),
    UseOpenCLFilm = proplists:get_value(accel_film, Attrs, true),
    GammaTable = gamma_table(proplists:get_value(gamma, Attrs, 2.2)),
    Film = #f{res={W,H}, gamma_table=GammaTable, fb=Raw, 
	      filter = proplists:get_value(film_filter, Attrs, none)},
    init_cl(UseOpenCLFilm, Film, R0).

resolution(#renderer{film=#f{res=Res}}) -> Res.

set_sample_frame_buffer(SFB, R=#renderer{film=F}) ->
    R#renderer{film=F#f{sfb=SFB}}.

get_sample_buffer(#renderer{film=#f{fb=Raw, type=raw}}) -> 
    Raw;
get_sample_buffer(#renderer{film=#f{sq=Sq}}) -> 
    Sq.

splat_sample_buffer(Raw, R=#renderer{film=F=#f{type=raw}}) -> 
    R#renderer{film=F#f{fb=Raw}};
splat_sample_buffer({Size, SQ}, RS=#renderer{cl=CL, film=Film}) -> 
    #f{filter=Filter, res={W,H}, sqb=SQB0, sfb=SFB, gausb=GB} = Film,
    SqBin = << <<(Id rem W):?F32, (Id div W):?F32, R:?F32, G:?F32, B:?F32>> 
	       || {Id, {R,G,B}} <- SQ >>,
    {SQB,W0} = case SQB0 of
		   undefined -> {wings_cl:buff(SqBin, [read_write], CL), []};
		   _ -> {SQB0, wings_cl:write(SQB0, SqBin, CL)}
	       end,
    case Filter of
	none ->
	    wings_cl:cast('PixelAddSampleBuffer',
			  [W,H,SFB,Size,SQB],Size,W0,CL);
	preview ->
	    wings_cl:cast('PixelAddSampleBufferPreview',
			  [W,H,SFB,Size,SQB],Size,W0,CL);
	gaussian ->
	    wings_cl:cast('PixelAddSampleBufferGaussian2x2',
			  [W,H,SFB,Size,SQB,GB],Size,W0,CL)
    end,
    RS#renderer{film=Film#f{sqb=SQB, sq={0,[]}}}.

splat(Index, Splat, Raw) ->
    {Rad, W} = array:get(Index, Raw),
    array:set(Index, {pbr_mat:sadd(Splat,Rad),W+1}, Raw).

gamma_table(Gamma) ->
    List = gamma_table(0.0, 1.0 / Gamma),
    list_to_tuple(List).

rad2pixel(X, GT) ->
    I = X * ?GAMMA_TABLE_SZ,
    element(clampi(I, 0.0, float(?GAMMA_TABLE_SZ)), GT).

%%%%% Move to film
show(#renderer{film=#f{type=raw, fb=Raw, gamma_table=GT, res=Res}}) ->
    Pixels = array:foldl(fun(_, {Intensity,Weight}, Acc) ->
				 {R,G,B} = pbr_mat:sdiv(Intensity,Weight),
    				 <<Acc/binary, 
    				   (clamp255(255.0*rad2pixel(R,GT))):8,
    				   (clamp255(255.0*rad2pixel(G,GT))):8, 
    				   (clamp255(255.0*rad2pixel(B,GT))):8,
				   255:8>>
    			 end, <<>>, Raw),
    show_image(Pixels, Res);
show(#renderer{cl=CL, film=#f{fb=FB, sfb=SFB, gammab=GB, res=Res={W,H}}}) ->
    SFB /= undefined orelse error(no_sample_buffer),
    W0 = wings_cl:cast('PixelUpdateFrameBuffer', [W,H,SFB,FB,GB], [W,H], [], CL),
    W1 = wings_cl:read(FB, W*H*?PIXEL_SZ, [W0], CL),
    {ok, Buff0} = cl:wait(W1),
    Buff = << <<(clamp255(255.0*R)):8,(clamp255(255.0*G)):8,(clamp255(255.0*B)):8>> 
	      || <<R:?F32, G:?F32, B:?F32>> <= Buff0>>,
    show_image(Buff, Res).

show_image(Pixels, {W,H}) when is_binary(Pixels) ->
    Image = #e3d_image{image=Pixels,width=W,height=H, 
		       order=upper_left,
    		       type=r8g8b8, bytes_pp=4},
    ShowImage = 
    	fun(_) -> 
    		Id = wings_image:new_temp("<<Render>>", Image),
    		wings_image:window(Id)
    	end,
    wings ! {external, ShowImage},
    ok.
    
%%%%%%%%%%%%%%%%%%%%

init_cl(false, _, R) -> R;
init_cl(true, Film = #f{gamma_table=GammaT, res={W,H}}, RS=#renderer{cl=CL0}) ->
    CL = wings_cl:compile("pbr/framebuffer.cl", CL0),
    GammaB = wings_cl:buff(table_to_bin(GammaT), CL0),
    GausFB = wings_cl:buff(table_to_bin(gaussion_table()), CL0),
    FB  = wings_cl:buff(W*H*?PIXEL_SZ,CL),
    %% Create this when/if needed
    %% SFB = wings_cl:buff(W*H*?SAMPLE_PIXEL_SZ,CL),
    %% SQB = wings_cl:buff(?SAMPLE_BUFF_SZ*?SAMPLE_SZ, CL),
    RS#renderer{cl=CL, film=Film#f{fb=FB, %% sfb=SFB, sqb=SB, 
				   gammab=GammaB, 
				   gausb=GausFB}}.

gamma_table(X, InvGamma) when X < (1.0+?RAY_EPS) ->
    [math:pow(X, InvGamma) | gamma_table(X+?GAMMA_DX, InvGamma)];
gamma_table(_, _) -> [].

gaussion_table() ->
    A = 2.0,
    ExpX = ExpY = math:exp(-A * 2.0 *2.0),
    GF = fun(X,Y) ->
		 max(0.0, math:exp(-A*X*X) - ExpX) * 
		     max(0.0, math:exp(-A*Y*Y) - ExpY)
	 end,
    list_to_tuple(gaussion_table_1(0, GF, [])).

gaussion_table_1(Y, GF, Acc0) 
  when Y < ?GAUSSION_FILTER_SZ ->
    FY = (Y+0.5)*2.0/?GAUSSION_FILTER_SZ,
    Acc = gaussion_table_2(0, FY, GF, Acc0),
    gaussion_table_1(Y+1, GF, Acc);
gaussion_table_1(_, _, Acc) -> lists:reverse(Acc).

gaussion_table_2(X, Y, GF, Acc) 
  when X < ?GAUSSION_FILTER_SZ ->
    FTE = GF((X+0.5)*2.0/?GAUSSION_FILTER_SZ, Y),
    gaussion_table_2(X+1, Y, GF, [FTE|Acc]);
gaussion_table_2(_, _, _, Acc) -> Acc.

table_to_bin(TupleOfFloats) ->
    << <<F:?F32>> || F <- tuple_to_list(TupleOfFloats) >>.

clamp255(C) -> clampi(C, 0.0, 255.0).

clampi(C, _Min, Max) when C > Max -> trunc(Max);
clampi(C, Min, _Max) when C < Min -> trunc(Min);
clampi(C, _, _) -> round(C).


