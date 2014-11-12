-module(erlbot_app).

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlbot_sup:start_link().

stop(_State) ->
    ok.

