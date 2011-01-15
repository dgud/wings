%%
%%  auv_matrix.erl --
%%
%%     Provides matrix ops for sparsely populated matrixes.
%%
%%  Copyright (c) 2001-2002 Raimo Niskanen,
%%                2004-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(auv_matrix).

-export([dim/1]).
-export([vector/1, vector/2]).
-export([rows/2, cols/1, cols/2]).
-export([cat_cols/2, cat_rows/2]).
-export([diag/1, row_norm/1]).
-export([trans/1, mult/2, mult_trans/2]).
-export([add/2, sub/2]).
-export([reduce/1, backsubst/1]).

-ifdef(DEBUG).
-export([float_perf/2]).
-endif.

-define(TAG, ?MODULE).

-compile(inline).
-compile({inline_size, 100}).

-import(lists, [reverse/1]).

%% Exported
%%
dim({?TAG,N,M,_}) ->
    {N,M};
dim({?TAG,N,_}) ->
    {N,1};
dim(V) when is_number(V) ->
    {1,1};
dim(A) ->
    error(badarg, [A]).



%% Exported
%%
vector({?TAG,_N,A}) ->
    lists:reverse(vector_to_list_r(A, []));
vector(V) when is_number(V) ->
    [V];
vector(L) when is_list(L) ->
    case vector_from_list(0, L, []) of
	{[], 0} ->
	    error(badarg, [L]);
	{A, N} when is_list(A) ->
	    fix({?TAG,N,A});
	Fault ->
	    error(Fault, [L])
    end;
vector(L) ->
    error(badarg, [L]).

vector_to_list_r([], C) -> C;
vector_to_list_r([V | A], C) when is_float(V) ->
    vector_to_list_r(A, [V | C]);
vector_to_list_r([1 | A], C) ->
    vector_to_list_r(A, [0.0 | C]);
vector_to_list_r([Z | A], C) ->
    vector_to_list_r([Z-1 | A], [0.0 | C]).

vector_from_list(N, [], C) ->
    {lists:reverse(C), N};
vector_from_list(N, [V | L], C) when is_number(V) ->
    F = float(V),
    vector_from_list(N+1, L, push_v(F, C));
vector_from_list(_, _, _) ->
    badarg.



%% Exported
%%
vector(N, D) when is_integer(N), is_list(D), 1 =< N ->
    case vector_from_tuplist(1, N, lists:sort(D), []) of
	L when is_list(L) ->
	    fix({?TAG,N,L});
	Fault ->
	    error(Fault, [N, D])
    end;
vector(N, D) ->
    error(badarg, [N, D]).

vector_from_tuplist(I, N, [], C) when I =< N ->
    vector_from_tuplist(N+1, N, [], push_v(N+1-I, C));
vector_from_tuplist(_, _, [], C) ->
    lists:reverse(C);
vector_from_tuplist(I1, N, [{I2,V} | D], C)
  when is_integer(I2), is_number(V), 
       I1 =< I2, I2 =< N ->
    F = float(V),
    vector_from_tuplist(I2+1, N, D, push_v(F, push_v(I2-I1, C)));
vector_from_tuplist(_, _, _, _) ->
    badarg.



%% Exported
%%
rows({?TAG,1,M,[A]}) ->
    fix({?TAG,M,A});
rows({?TAG,_,M,A}) ->
    rows_to_list(M, A, []);
rows({?TAG,_,_} = A) ->
    vector(A);
rows(V) when is_number(V) ->
    [V];
rows(A) ->
    error(badarg, [A]).

rows_to_list(_, [], C) ->
    lists:reverse(C);
rows_to_list(M, [Row | A], C) ->
    rows_to_list(M, A, [fix({?TAG,M,Row}) | C]).



%% Exported
%%
rows(M, L) when is_integer(M), is_list(L), M >= 1 ->
    case vecs(M, L) of
	{A, N} when is_list(A) ->
	    fix({?TAG,N,M,A});
	Fault ->
	    error(Fault, [M, L])
    end;
rows(M, L) ->
    error(badarg, [M, L]).



%% Exported
%%
cols({?TAG,_,_,_} = A) ->
    rows(trans(A));
cols({?TAG,_,_} = A) ->
    [fix(A)];
cols(V) when is_number(V) ->
    [V];
cols(A) ->
    error(badarg, A).



%% Exported
%%
cols(N, L)
  when is_integer(N), is_list(L), N >= 1 ->
    case vecs(N, L) of
	{A, M} when is_list(A) ->
	    trans({?TAG,M,N,A});
	Fault ->
	    error(Fault, [N, L])
    end;
cols(N, L) ->
    error(badarg, [N, L]).



%% Exported
%%
cat_cols({?TAG,N,_,_} = A, {?TAG,N,_,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_,_} = A, {?TAG,N,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_} = A, {?TAG,N,_,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_} = A, {?TAG,N,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols(A, B) ->
    error(badarg, [A, B]).



%% Exported
%%
cat_rows({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na+Nb,M,A++B};
cat_rows({?TAG,_,_} = A, {?TAG,_,1,_} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows({?TAG,_,1,_} = A, {?TAG,_,_} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows({?TAG,_,_} = A, {?TAG,_,_} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows(A, B) ->
    error(badarg, [A, B]).



%% Exported
%%
diag({?TAG,_,_,A}) ->
    diag_rows(1, A, []);
diag({?TAG,_,[Z | _]}) when is_integer(Z) ->
    [0.0];
diag({?TAG,_,[V | _]}) ->
    [V];
diag(V) when is_number(V) ->
    [float(V)];
diag([V]) when is_number(V) ->
    float(V);
diag(L) when is_list(L) ->
    N = length(L),
    case diag_list(1, N, L, []) of
	A when is_list(A) ->
	    fix({?TAG,N,N,A});
	Fault ->
	    error(Fault, [L])
    end;
diag(A) ->
    error(badarg, [A]).

diag_rows(_, [], C) ->
    lists:reverse(C);
diag_rows(I, [Row | A], C) ->
    diag_cols(I, 1, A, Row, C).

diag_cols(I, I, A, [V | Row], C) when is_float(V) ->
    diag_rows(I+1,
	      if Row == [] -> []; true -> A  end,
	      [V | C]);
diag_cols(I, J, A, [V | Row], C) when is_float(V) ->
    diag_cols(I, J+1, A, Row, C);
diag_cols(I, J, A, [Z | Row], C) when J+Z > I ->
    diag_rows(I+1, 
	      if Row == [] -> []; true -> A end, 
	      [0.0 | C]);
diag_cols(I, J, A, [Z | Row], C) ->
    diag_cols(I, J+Z, A, Row, C).

diag_list(N, N, [V], C) when is_number(V) ->
    lists:reverse(C, [[N-1, float(V)]]);
diag_list(I, N, [V | L], C) when is_number(V), V == 0 ->
    diag_list(I+1, N, L, [[N] | C]);
diag_list(1, N, [V | L], C) when is_number(V) ->
    diag_list(2, N, L, [[float(V), N-1] | C]);
diag_list(I, N, [V | L], C) when is_number(V) ->
    diag_list(I+1, N, L, [[I-1, float(V), N-I] | C]);
diag_list(_, _, _, _) ->
    badarg.



%% Exported
%%
row_norm({?TAG,_,_,A}) ->
    row_norm_int(A, []);
row_norm({?TAG,_,A}) ->
    row_norm_col(A, []);
row_norm(A) ->
    error(badarg, [A]).

row_norm_int([], C) ->
    lists:reverse(C);
row_norm_int([Row | A], C) ->
    row_norm_int(A, [vec_sq(Row, 0.0) | C]).

row_norm_col([], C) ->
    lists:reverse(C);
row_norm_col([V | A], C) when is_float(V) ->
    row_norm_col(A, [V*V | C]);
row_norm_col([Z | A], C) ->
    row_norm_col(Z, A, C).

row_norm_col(0, A, C) ->
    row_norm_col(A, C);
row_norm_col(Z, A, C) ->
    row_norm_col(Z-1, A, [0.0 | C]).



%% Exported
%%
trans({?TAG,1,M,[A]}) ->
    fix({?TAG,M,A});
trans({?TAG,N,M,A}) ->
    fix({?TAG,M,N,trans_cols_forw(1, M, A, [])});
trans({?TAG,N,A}) ->
    fix({?TAG,1,N,[A]});
trans(A) when is_number(A) ->
    float(A);
trans(A) ->
    error(badarg, [A]).

trans_cols_forw(J, M, _, C_r) when J == M+1 ->
    lists:reverse(C_r);
trans_cols_forw(J, M, A, C_r) ->
    {Col_r, A_r} = trans_mk_col_r(A),
    trans_cols_rev(J+1, M, A_r, [lists:reverse(Col_r) | C_r]).

trans_cols_rev(J, M, _, C_r) when J == M+1 ->
    lists:reverse(C_r);
trans_cols_rev(J, M, A_r, C_r) ->
    {Col, A} = trans_mk_col_r(A_r),
    trans_cols_forw(J+1, M, A, [Col | C_r]).

trans_mk_col_r(A) ->
    trans_mk_col_r(A, [], []).

trans_mk_col_r([], B, C) ->
    {C, B};
trans_mk_col_r([[V | Row] | A], B, C) when is_float(V) ->
    trans_mk_col_r(A, [Row | B], [V | C]);
trans_mk_col_r([Row | A], B, C) ->
    trans_mk_col_r(A, [pop_z(Row) | B], push_v(1, C)).



%% Exported
%%
mult({?TAG,K,_M} = A, {?TAG,1,K,[B]}) ->
    mult_trans(A, {?TAG,K,B});
mult({?TAG,_,K,_} = A, {?TAG,K,_,_} = B) ->
    mult_trans(A, trans(B));
mult({?TAG,1,M,[A]}, {?TAG,M,B}) ->
    vec_mult(A, B);
mult({?TAG,N,M,A}, {?TAG,M,B}) ->
    %% Waste of time to call fix/1 here.
    {?TAG,N,mult_vec(B, A)};
mult(A, B) when is_number(A), A == 1 ->
    fix(B);
mult(A, {?TAG,N,1,[B]}) when is_number(A) ->
    {?TAG,N,1,[vec_mult_const(float(A), B, [])]};
mult(A, {?TAG,N,M,B}) when is_number(A) ->
    {?TAG,N,M,mult_const(float(A), B, [])};
mult(A, {?TAG,N,B}) when is_number(A) ->
    %% Waste of time to call fix/1 here.
    {?TAG,N,vec_mult_const(float(A), B, [])};
mult(A, B) when is_number(B), B == 1 ->
    fix(A);
mult({?TAG,N,1,[A]}, B) when is_number(B) ->
    {?TAG,N,1,[vec_mult_const(float(B), A, [])]};
mult({?TAG,N,M,A}, B) when is_number(B) ->
    {?TAG,N,M,mult_const(float(B), A, [])};
mult({?TAG,N,A}, B) when is_number(B) ->
    {?TAG,N,vec_mult_const(float(B), A, [])};
mult(A, B) when is_number(A), is_number(B) ->
    float(A*B);
mult(A, B) ->
    error(badarg, [A, B]).

mult_vec(VecA, B) ->
    mult_vec_1(list_to_tuple(reverse(vector_to_list_r(VecA, []))), B, 0.0, []).

mult_vec_1(_, [], _, C) -> reverse(C);
mult_vec_1(VecA, [VecB | B], Z, C) ->
    mult_vec_1(VecA, B, Z, [vec_mult_tuple(VecA, 1, VecB, Z) | C]).

%% Exported
%%
mult_trans({?TAG,1,M,[A]}, {?TAG,1,M,[B]}) ->
    vec_mult(A, B);
mult_trans({?TAG,_N,M,_} = A, {?TAG,1,M,_} = B) ->
    mult(A, trans(B));
mult_trans({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    fix({?TAG,Na,Nb,mult_row(A, B, [])});
%%
mult_trans(A, {?TAG,N,B}) ->
    mult(A, {?TAG,1,N,[B]});
mult_trans({?TAG,N,A}, B) ->
    mult_trans(trans({?TAG,1,N,[A]}), B);
%%
mult_trans(A, B) when is_number(A), is_number(B) ->
    float(A * B);
mult_trans(A, B) when is_number(B) ->
    mult(A, B);
mult_trans(A, B) when is_number(A) ->
    trans(mult(A, B));
mult_trans(A, B) ->
    error(badarg, [A, B]).

mult_row([], _, C) ->
    lists:reverse(C);
mult_row([RowA | A], B, C) ->
    mult_row(A, B, [mult_vec(RowA, B) | C]).

mult_const(_, [], C) ->
    lists:reverse(C);
mult_const(F, [Row | A], C) ->
    mult_const(F, A, [vec_mult_const(F, Row, []) | C]).



%% Exported
%% 
add({?TAG,N,M,A}, {?TAG,N,M,B}) ->
    fix({?TAG,N,M,add_row(A, B, [])});
add({?TAG,N,A}, {?TAG,N,B}) ->
    fix({?TAG,N,vec_add(A, B)});
add({?TAG,N,1,_} = A, {?TAG,N,B}) ->
    {?TAG,1,N,[C]} = trans(A),
    fix({?TAG,N,vec_add(C, B)});
add({?TAG,N,A}, {?TAG,N,1,_} = B) ->
    {?TAG,1,N,[C]} = trans(B),
    fix({?TAG,N,vec_add(A, C)});
add(Va, Vb) when is_number(Va), is_number(Vb) ->
    float(Va + Vb);
%%
add({?TAG,1,1,_} = A, Vb) when is_number(Vb) ->
    add(A, {?TAG,1,1,[push_v(Vb, [])]});
add({?TAG,1,_} = A, Vb) when is_number(Vb) ->
    add(A, {?TAG,1,push_v(Vb, [])});
add(Va, B) when is_number(Va) ->
    add(B, Va);
%%
add(A, B) ->
    error(badarg, [A, B]).

add_row([], [], C) ->
    lists:reverse(C);
add_row([RowA | A], [RowB | B], C) ->
    add_row(A, B, [vec_add(RowA, RowB) | C]).



%% Exported
%%
sub(A, B) ->
    add(A, mult(-1, B)).



%% Exported
%%
reduce({?TAG,N,M,A}) ->
    case catch reduce_sort(A, []) of
	{'EXIT',{badarith,_}} ->
	    illconditioned;
	B ->
	    {?TAG,N,M,B}
    end;
reduce(A) ->
    error(badarg, [A]).

reduce_sort([], C) ->
    reduce_row(lists:sort(C), []);
reduce_sort([Row | A], C) ->
    reduce_sort(A, [reduce_presort(0, Row) | C]).

reduce_presort(Z, []) ->
    {Z, infinity, []};
reduce_presort(Z, [0.0 | Row]) ->
    reduce_presort(Z+1, Row);
reduce_presort(Z, [V | _] = Row) when is_float(V) ->
    {Z, 1.0/abs(V), Row};
reduce_presort(Z, [Zr | Row]) ->
    reduce_presort(Z+Zr, Row).

reduce_row([], C) ->
    lists:reverse(C);
reduce_row([Row | A], C) ->
    reduce_row(reduce_zap(Row, A, []), [reduce_postsort(Row) | C]).

reduce_postsort({0, _, Row}) ->
    Row;
reduce_postsort({Z, _, Row}) ->
    [Z | Row].

reduce_zap({Z, _, [V | Row]} = R, [{Z, _, [Va | RowA]} | A], C) 
  when is_float(V), is_float(Va) ->
    reduce_zap(R, A, [reduce_presort(Z+1, vec_add(RowA, -Va/V, Row)) | C]);
reduce_zap(_, [], C) ->
    lists:sort(C);
reduce_zap(_, A, C) ->
    lists:merge(A, lists:sort(C)).



%% Exported
%%
backsubst({?TAG,N,M,A} = AA) when M == N+1 ->
    case catch backsubst_rev(0, A, []) of
	A_tri when is_list(A_tri) ->
	    case catch backsubst_const(A_tri, [], []) of
		X when is_list(X) ->
		    {?TAG,N,X};
		{error, Reason} ->
		    Reason;
		{'EXIT', {badarith, []}} ->
		    illconditioned;
		{'EXIT', Reason} ->
		    exit(Reason);
		Fault ->
		    error(Fault, [AA])
	    end;
	{error, Reason} ->
	    Reason;
	{'EXIT', {badarith, []}} ->
	    illconditioned;
	{'EXIT', Reason} ->
	    exit(Reason);
	Fault ->
	    error(Fault, [AA])
    end;
backsubst(A) ->
    error(badarg, [A]).

backsubst_rev(_, [], C) ->
    lists:reverse(C);
backsubst_rev(Z, [RowA | A], C) ->
    backsubst_rev_z(Z, 0, RowA, A, C).

backsubst_rev_z(Z, Za, _, _, _) when Za > Z ->
    {error, not_reduced};
backsubst_rev_z(_, _, [_], _, _) ->
    {error, not_reduced};
backsubst_rev_z(Z, Za, [0.0 | RowA], A, C) ->
    backsubst_rev_z(Z, Za+1, RowA, A, C);
backsubst_rev_z(Z, Za, [Va | _] = RowA, A, C) when is_float(Va) ->
    if Z == Za ->
	    backsubst_rev(Z+1, A, [vector_to_list_r(RowA, []) | C]);
       true ->
	    {error, undetermined}
    end;
backsubst_rev_z(Z, Za, [Zaa | RowA], A, C) ->
    backsubst_rev_z(Z, Za+Zaa, RowA, A, C).

backsubst_const([], C, B) ->
    backsubst_vec_r(C, B, []);
backsubst_const([[_]], _, _) ->
    {error, undetermined};
backsubst_const([[V | RowA] | A], C, B) when is_float(V) ->
    backsubst_const(A, [RowA | C], [-V | B]).

backsubst_vec_r([], [], X) ->
    lists:reverse(X);
backsubst_vec_r([RowA | A], [Vb | B], X) ->
    backsubst_vec_x(RowA, A, B, X, X, Vb).

backsubst_vec_x([Va], A, B, [], X0, S) 
  when is_float(Va), is_float(S) ->
    backsubst_vec_r(A, B, X0++[S/Va]);
backsubst_vec_x([Va | RowA], A, B, [Vx | X], X0, S) 
  when is_float(Va), is_float(Vx), is_float(S) ->
    backsubst_vec_x(RowA, A, B, X, X0, S - Va*Vx).



vecs(N, L) ->
    vecs(0, N, L, []).

vecs(I, _, [], C) ->
    {lists:reverse(C), I};
vecs(I, N, [{?TAG,N,D} | L], C) ->
    vecs(I+1, N, L, [D | C]);
vecs(I, 1, [V | L], C) when is_number(V) ->
    vecs(I+1, 1, L, [push_v(float(V), []) | C]);
vecs(_, _, _, _) ->
    badarg.



vec_add(A, B) ->
    vec_add(0, A, 0, B, 0, []).

vec_add(A, F, B) when is_float(F) ->
    vec_add(0, A, F, 0, B, 0, []).

vec_add(Za, [Va | A], Zb, B, Zc, C) when is_integer(Va) ->
    vec_add(Za+Va, A, Zb, B, Zc, C);
vec_add(Za, A, Zb, [Vb | B], Zc, C) when is_integer(Vb) ->
    vec_add(Za, A, Zb+Vb, B, Zc, C);
vec_add(0, [], 0, [], Zc, C) ->
    if Zc == 0 ->
	    lists:reverse(C);
       true ->
	    lists:reverse(C, [Zc])
    end;
vec_add(0, [Va | A], 0, [Vb | B], Zc, C)
  when is_float(Va), is_float(Vb) ->
    Vc = Va + Vb,
    vec_add(0, A, 0, B, 0, if Zc == 0 -> [Vc | C];
			      true -> [Vc, Zc | C]
			   end);
vec_add(0, [Va | A], Zb, B, Zc, C) ->
    vec_add(0, A, Zb-1, B, 0, if Zc == 0 -> [Va | C];
				 true -> [Va, Zc | C]
			      end);
vec_add(Za, A, 0, [Vb | B], Zc, C) 
  when is_float(Vb) ->
    vec_add(Za-1, A, 0, B, 0, if Zc == 0 -> [Vb | C];
				 true -> [Vb, Zc | C]
			      end);
vec_add(Za, A, Zb, B, Zc, C) ->
    if Za < Zb ->
	    vec_add(0, A, Zb-Za, B, Zc+Za, C);
       Zb < Za ->
	    vec_add(Za-Zb, A, 0, B, Zc+Zb, C);
       true ->
	    vec_add(0, A, 0, B, Zc+Za, C)
    end.

vec_add(Za, [Va | A], F, Zb, B, Zc, C) when is_integer(Va) ->
    vec_add(Za+Va, A, F, Zb, B, Zc, C);
vec_add(Za, A, F, Zb, [Vb | B], Zc, C) when is_integer(Vb) ->
    vec_add(Za, A, F, Zb+Vb, B, Zc, C);
vec_add(0, [], _, 0, [], Zc, C) ->
    if Zc == 0 ->
	    lists:reverse(C);
       true ->
	    lists:reverse(C, [Zc])
    end;
vec_add(0, [Va | A], F, 0, [Vb | B], Zc, C)
  when is_float(Va), is_float(F), is_float(Vb) ->
    Vc = Va + F*Vb,
    vec_add(0, A, F, 0, B, 0, if Zc == 0 -> [Vc | C];
				 true -> [Vc, Zc | C]
			      end);
vec_add(0, [Va | A], F, Zb, B, Zc, C) ->
    vec_add(0, A, F, Zb-1, B, 0, if Zc == 0 -> [Va | C];
				    true -> [Va, Zc | C]
				 end);
vec_add(Za, A, F, 0, [Vb | B], Zc, C) 
  when is_float(F), is_float(Vb) ->
    Vc = F*Vb,
    vec_add(Za-1, A, F, 0, B, 0, if Zc == 0 -> [Vc | C];
				    true -> [Vc, Zc | C]
				 end);
vec_add(Za, A, F, Zb, B, Zc, C) ->
    if Za < Zb ->
	    vec_add(0, A, F, Zb-Za, B, Zc+Za, C);
       Zb < Za ->
	    vec_add(Za-Zb, A, F, 0, B, Zc+Zb, C);
       true ->
	    vec_add(0, A, F, 0, B, Zc+Za, C)
    end.

vec_mult_const(F, [V | B], C) when is_float(F), is_float(V) ->
    vec_mult_const(F, B, [V*F | C]);
vec_mult_const(F, [Z | B], C) ->
    vec_mult_const(F, B, [Z | C]);
vec_mult_const(_, [], C) ->
    lists:reverse(C).

vec_mult_tuple(T, I, [Zb | B], S) when is_integer(Zb) ->
    vec_mult_tuple(T, I+Zb, B, S);
vec_mult_tuple(T, I, [Vb | B], S) when is_float(Vb), is_float(S) ->
    case element(I, T) of
	Va when is_float(Va) ->
	    vec_mult_tuple(T, I+1, B, Va*Vb + S)
    end;
vec_mult_tuple(_, _, [], S) -> S.

vec_mult(A, B) ->
    vec_mult(A, B, 0.0).

vec_mult([Va | A], [Vb | B], S) when is_float(Va), is_float(Vb) ->
    vec_mult(A, B, Va*Vb + S);
vec_mult([Za|A], [_|_]=B, S) when is_integer(Za) ->
    vec_mult_pop(Za, A, B, S);
vec_mult([_|_]=A, [Zb | B], S) -> %% when is_integer(Zb)
    vec_mult_pop(Zb, B, A, S);
vec_mult(_, _, S) -> S.

vec_mult_pop(_, [], _, S) -> S;
vec_mult_pop(0, A, B, S) ->
    vec_mult(A, B, S);
vec_mult_pop(Za, A, [Vb | B], S) when is_float(Vb) ->
    vec_mult_pop(Za-1, A, B, S);
vec_mult_pop(_, _, [_], S) -> % when is_integer(Zb)
    S;
vec_mult_pop(Za, A, [Zb | B], S) when Za < Zb ->
    vec_mult_pop(Zb-Za, B, A, S);
vec_mult_pop(Za, A, [Zb | B], S) when Zb < Za ->
    vec_mult_pop(Za-Zb, A, B, S);
vec_mult_pop(_, A, [_ | B], S) -> % when Za == Zb
    vec_mult(A, B, S);
vec_mult_pop(_, _, [], S) ->
    S.

vec_sq([], S) ->
    S;
vec_sq([V | A], S) when is_float(V), is_float(S) ->
    vec_sq(A, S + V*V);
vec_sq([_ | A], S) ->
    vec_sq(A, S).



%% Push value; zeros or float
push_v(0.0, C) ->
    case C of
	[Z | R] when is_integer(Z) ->
	    [Z+1 | R];
	R ->
	    [1 | R]
    end;
push_v(V, C) when is_float(V) ->
    [V | C];
push_v(0, C) ->
    C;
push_v(Z1, C) when is_integer(Z1) ->
    case C of
	[Z2 | R] when is_integer(Z2) ->
	    [Z1+Z2 | R];
	R ->
	    [Z1 | R]
    end.

%% Pop zero
pop_z([]) ->
    [];
pop_z([1]) ->
    [];
pop_z([1 | C]) ->
    C;
pop_z([Z | C]) when is_integer(Z) ->
    [Z-1 | C].


%% Fix 1x1 matrixes to become scalars
%%
fix({?TAG,1,1,[[1]]}) ->
    0.0;
fix({?TAG,1,1,[[V]]}) ->
    V;
fix({?TAG,1,[1]}) ->
    0.0;
fix({?TAG,1,[V]}) ->
    V;
fix(V) when is_integer(V) ->
    float(V);
fix(M) ->
    M.


-ifdef(DEBUG).
float_perf(A, L) ->
    float_perf(A, L, []).

float_perf(_, [], C) ->
    lists:reverse(C);
float_perf(A, [B | T], C) ->
    float_perf(A, T, [float_perf_int(A, B, 0.0), C]).

float_perf_int([], [], S) ->
    S;
float_perf_int([Va | A], [Vb | B], S) when is_float(Va), float(Vb), float(S) ->
    float_perf_int(A, B, Va*Vb + S).
-endif.
