-module(erlbot_callbacks).

-behavior(gen_server).

-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
% state
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    irc_lib_sup:start_link(),
    %% dev.pearachute.net = 72.14.176.182
    irc_lib_sup:start_irc_client(?MODULE, {<<"dev.pearachute.net">>, <<>>}, 6667, [{<<"#hello">>, <<>>}], <<"mynameiserl">>, false, 30000),
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, From, IncomingMessage}, State) ->
    io:format("Incoming message: ~p ~p ~n", [IncomingMessage, From]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions
