%%%-------------------------------------------------------------------
%% @doc cs11_winter public API
%% @end
%%%-------------------------------------------------------------------

-module(cs11_winter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cs11_winter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
