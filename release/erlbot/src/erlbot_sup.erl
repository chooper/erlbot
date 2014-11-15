-module(erlbot_sup).
-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% https://github.com/npwolf/erlbot/blob/master/src/erlbot_sup.erl
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 10,
    MaxTime = 10000,
    %% start order is important
    {ok, { {one_for_one, MaxRestart, MaxTime}, [
        ?CHILD_SUP(irc_lib_sup),
        ?CHILD(erlbot_callbacks, worker)
    ]} }.

