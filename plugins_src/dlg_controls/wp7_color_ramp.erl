%%
%%  wp7_color_ramp.erl --
%%
%%     Gradient color control for dialogs - Color Ramp.
%%
%%  Copyright (c) 2013 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wp7_color_ramp).
-export([init/1,build_color_ramp/1,def_color_key/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wp7_color_ramp.hrl").
-include("wings.hrl").

-import(wings_ask, [mktree_control/8,var/2,color3_disabled/0,color4/0,color3_high/0]).

%% Static data for every field.
-include("wings_ask.hrl").

init(Next) ->
    fun (What) -> mktree(What,Next) end.

%%
%% Syntax of Qs.
%% See also wpc_test_ask.erl for examples.
%%
%%
%% {color_ramp,DefaultValue,[,Flags]}			-- Color ramp field
%%     Flags = [Flag]
%%     Flag = {key,Key}|{hook,Hook}|{info,String}|layout
%%
mktree({{color_ramp,Val}, Sto, I}, _Next) ->
    mktree_color_ramp(Val,?CR_WIDTH,?CR_HEIGHT,fun color_ramp_handle/2,Sto,I,[]);
mktree({{color_ramp,Val,Flags}, Sto, I}, _Next) ->
    mktree_color_ramp(Val,?CR_WIDTH,?CR_HEIGHT,fun color_ramp_handle/2,Sto,I,Flags);
mktree(What, Next) -> 
    Next(What).


%%%
%%% color_ramp
%%%

-record(color_ramp, 
    {handler :: fun(),
     sel_key=none :: 'none' | integer(),
     col_ramp
    }).

mktree_color_ramp(Val, W, H, Handler, Sto, I, Flags) ->
    Flags0=proplists:delete(stretch, Flags),
    InitSto=fun (Key) ->
        Sto0=gb_trees:enter(var(Key, I), Val, Sto),
        {Sto0,#color_ramp{handler=Handler, sel_key=none, col_ramp=build_color_ramp(Val)}}
    end,
    mktree_control(fun color_ramp_event/3,InitSto,active,undefined,W+1,H+1,I,Flags0++[{stretch,W}]).

color_ramp_event({redraw,Active,DisEnabled}, [#fi{x=X,y=Y,w=W,h=H,index=I,key=Key}|_], Store) ->
    color_ramp_draw(Active, X, Y, W, H, DisEnabled),
    color_ramp_handle(redraw,{var(Key, I),I,Store,{X+1,Y+H,W,H},Active,DisEnabled});
color_ramp_event(#mousemotion{x=Xm,y=Ym}=Ev, [#fi{key=Key,index=I,state=enabled,x=X,y=Y,w=W,h=H}|_], Store) ->
    color_ramp_handle(Ev#mousemotion{x=Xm-X,y=Y+H-Ym-1},{var(Key, I),I,Store,{0,0,W-1,H-1}});
color_ramp_event(#mousebutton{x=Xb,y=Yb}=Ev, [#fi{key=Key,index=I,state=enabled,x=X,y=Y,w=W,h=H}|_], Store) ->
    color_ramp_handle(Ev#mousebutton{x=Xb-X,y=Y+H-Yb-1},{var(Key, I),I,Store,{0,0,W-1,H-1}});
color_ramp_event({doubleclick,Xb,Yb}, [#fi{key=Key,index=I,state=enabled,x=X,y=Y,w=W,h=H}|_], Store) ->
    color_ramp_handle({doubleclick,Xb-X,Y+H-Yb-1},{var(Key, I),I,Store,{0,0,W-1,H-1}});
color_ramp_event({key,_Sym,_Mod,_Unicode}=Ev, [#fi{key=Key,index=I,state=enabled,w=W,h=H}|_], Store) ->
    color_ramp_handle(Ev,{var(Key, I),I,Store,{0,0,W-1,H-1}});
color_ramp_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
color_ramp_event(Ev, [#fi{key=Key,index=I,state=enabled,x=X,y=Y,w=W,h=H}|_], Store) ->
    color_ramp_handle(Ev,{var(Key, I),I,Store,{X,Y,W,H}}).

color_ramp_draw(Active, X, Y, W, H, DisEnabled) ->
    BkColor = case DisEnabled of
          enabled -> color3_high();
          _ -> color4()
          end,
    case Active of
    true ->
        wings_io:sunken_gradient(X, Y, W, H,
                 BkColor, color4(), Active);
    _ ->
        wings_io:sunken_rect(X, Y, W, H,
                BkColor, color3_disabled())
    end,
    gl:color3b(0, 0, 0).

%% process the color_ramp field events
color_ramp_handle(#mousemotion{x=X,y=Y,state=Bst}, {Var,I,Val,Store,_}) ->
    #color_ramp{sel_key=SelKey0}=ICr=gb_trees:get(-I, Store),
    #cr{color_keys=ColKeys0}=Val,
    case SelKey0 of
        none -> void;
        _ ->
            Key=X-?CR_START,
            case hit_color_ramp(X,Y) of
                true when 0 < Key, Key < ?CR_W ->
                    if (Bst band ?SDL_BUTTON_LMASK) =/= 0 ->
                        RGB=orddict:fetch(SelKey0,ColKeys0),
                        ColKeys1=orddict:erase(SelKey0,ColKeys0),
                        ColKeys=orddict:store(Key,RGB,ColKeys1),
                        ColRamp=build_color_ramp(ColKeys),
                        Store0=gb_trees:update(Var,Val#cr{name=custom,color_keys=ColKeys},Store),
                        {store,gb_trees:update(-I,ICr#color_ramp{sel_key=Key,col_ramp=ColRamp},Store0)};
                      true -> void
                    end;
                _ -> void
            end
    end;
color_ramp_handle(#mousebutton{x=X,y=Y,state=?SDL_PRESSED}, {Var,I,Store,_}) ->
    case hit_color_ramp(X,Y) of
    true ->
        Key=X-?CR_START,
        #color_ramp{col_ramp=ColRamp0}=ICr=gb_trees:get(-I, Store),
        case wings_io:is_modkey_pressed(?CTRL_BITS) of
            false ->
                RGB0=array:get(Key,ColRamp0),
                Owner = wings_wm:this(),
                wings_color:choose(RGB0, fun(RGB) ->
                    wings_wm:send(Owner,{set_color,{Key,RGB}})
                end);
            _ when 0 < Key, Key < ?CR_W ->
                Val=gb_trees:get(Var,Store),
                #cr{color_keys=ColKeys}=Val,
                SelKey=case orddict:find(Key, ColKeys) of
                    {ok,_} -> Key;
                    _ -> none
                end,
                {store,gb_trees:update(-I,ICr#color_ramp{sel_key=SelKey},Store)};
            _ -> void
        end;
    _ -> void
    end;
color_ramp_handle(#mousebutton{button=1,state=?SDL_RELEASED}, {_,I,_,Store,_}) ->
    ICr=gb_trees:get(-I, Store),
    {store,gb_trees:update(-I,ICr#color_ramp{sel_key=none},Store)};
color_ramp_handle({set_color,{Idx,RGB}}, {Var,I,Store,_}) ->
    Val=gb_trees:get(Var, Store),
    ICr=gb_trees:get(-I, Store),
    #cr{color_keys=ColKeys0}=Val,
    ColorKeys=orddict:store(Idx,RGB,ColKeys0),
    ColRamp=build_color_ramp(ColorKeys),
    Store0=gb_trees:update(Var,Val#cr{color_keys=ColorKeys},Store),
    {store,gb_trees:update(-I,ICr#color_ramp{col_ramp=ColRamp},Store0)};
color_ramp_handle(redraw, {Var,I,Store,Rect,_Active,_DisEnabled}) ->
    #color_ramp{sel_key=SelKey,col_ramp=ColRamp} = gb_trees:get(-I, Store),
    #cr{color_keys=ColKeys}=gb_trees:get(Var, Store),
    {Xo,Yo,_,_}=Rect,
    Top=Yo-(?CR_START+?CR_H),
    Left=Xo+?CR_START,
    array:foldr(fun(Key, Color, _Acc) ->
        gl:'begin'(?GL_LINES),
        gl:color3fv(Color),
        gl:vertex2f(Left+Key, Yo-?CR_START),
        gl:vertex2f(Left+Key, Top+1),
        gl:'end'(),
        case orddict:is_key(Key,ColKeys) of
            true ->
                gl:'begin'(?GL_LINES),
                gl:color3fv({0.0,0.0,0.0}),
                gl:vertex2f(Left+Key, Top),
                gl:vertex2f(Left+Key, Top-2),
                gl:'end'();
            _ -> ok
        end
    end, [], ColRamp),
    if SelKey=/=none ->
            gl:'begin'(?GL_LINE_LOOP),
            gl:color3fv({0.0,0.0,0.0}),
            gl:vertex2f(Left+SelKey+1, Yo-?CR_START+1),
            gl:vertex2f(Left+SelKey-1, Yo-?CR_START+1),
            gl:vertex2f(Left+SelKey-1, Top-2),
            gl:vertex2f(Left+SelKey+1, Top-1),
            gl:'end'(),
            gl:'begin'(?GL_LINES),
            gl:color3fv(orddict:fetch(SelKey,ColKeys)),
            gl:vertex2f(Left+SelKey-1, Top-1),
            gl:vertex2f(Left+SelKey, Yo-?CR_START),
            gl:'end'();
        true -> void
    end,
    keep;
color_ramp_handle(_,_) ->
    keep.

%%%
%%% Utilities
%%%

hit_color_ramp(X,Y) when (?CR_START) =< X,
                        X =< (?CR_START+?CR_W),
                        (?CR_START) =< Y,
                        Y =< (?CR_START+?CR_H) -> true;
hit_color_ramp(_,_) -> false.

build_color_ramp(#cr{color_keys=ColKeys}) ->
    build_color_ramp(ColKeys);
build_color_ramp(KeyCols1) ->
    [Col0|Cols]=orddict:to_list(KeyCols1),
    {_,KeyCols0}=lists:foldl(fun({Key1,_}=Item1, {{Key0,_}=Item0,Acc}) ->
        Acc0=[build_color_ramp_1(Key,Item0,Item1) || Key <- lists:seq(Key0,Key1)],
        {Item1,Acc++Acc0}
    end, {Col0,[]}, Cols),
    KeyCols=lists:foldl(fun({Key,Value}, Acc) ->
        orddict:store(Key,Value,Acc)
    end, orddict:new(), KeyCols0),
    array:from_orddict(KeyCols).
build_color_ramp_1(Key, {Key0,C0}, {Key1,C1}) ->
    Range=Key1-Key0,
    Cr=e3d_vec:sub(C1,C0),
    Rf=(Key-Key0)/Range, 
    Cr0=e3d_vec:mul(Cr,Rf),
    {Key,e3d_vec:add(C0,Cr0)}.

def_color_key() ->
    Ck0=orddict:store(0,{0.0,0.0,0.0},orddict:new()),
    Ck=orddict:store(?CR_MAX,{1.0,1.0,1.0},Ck0),
    #cr{color_keys=Ck}.

