-module(erlbot_callbacks).
-compile([{parse_transform, lager_transform}]).

-behavior(gen_server).

-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
% records
-record(state, {}).
-include_lib("irc_lib/src/proto.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:set_loglevel(lager_console_backend, debug),
    irc_lib_sup:start_link(),
    {ok, ClientPid} = irc_lib_sup:start_irc_client(?MODULE, {<<"dev.pearachute.net">>, <<>>}, 6667, [{<<"#bottest">>, <<>>}], <<"mynameiserl">>, false, 30000),
    register(irc_client_pid, ClientPid),
    {ok, _} = timer:send_interval(timer:seconds(10), ClientPid, {raw, "NAMES #bottest"}), %% TODO don't hardcode this
    {ok, #state{}}.
 
handle_call(Request, From, State) ->
    lager:debug("handle_call: ~p ~p ~p ~n", [Request, From, State]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:debug("handle_cast: ~p ~p ~n", [Msg, State]),
    {noreply, State}.

handle_info({incoming_message, From, IncomingMessage}, State) ->
    lager:debug("Incoming message: ~p ~p ~n", [IncomingMessage, From]),
    {noreply, State};

handle_info({irc_line, {irc_strings, Prefix, Command, Args}}, State) ->
    R = #irc_strings{prefix = Prefix, cmd = Command, args = Args},

    %%
    %% begin main parsing here
    %%
    case R of
        %%
        %% see https://www.alien.net.au/irc/irc2numerics.html or RFC 1459
        %%

        %% names reply
        #irc_strings{cmd = "353", args = [_,_,Channel,RawNames]} ->
            %% Strip chars off names
            %% TODO clean this mess
            Names = [ string:strip(string:strip(string:strip(N, left, $:), left, $@), left, $+) || N <- string:tokens(RawNames, " ") ],
            lists:foreach(fun(Name) ->
                    timer:sleep(200),
                    irc_client_pid ! {raw, "MODE " ++ Channel ++ " +o " ++ Name}
                  end,
                  Names),
            {noreply, State};

        %% needed chanops
        #irc_strings{cmd = "482", args = [_, Channel, Msg]} ->
            lager:warning("Needed chanops in ~p, got error instead: ~p", [Channel, Msg]),
            {noreply, State};

        _ ->
            {noreply, State}
    end;

handle_info(Info, State) ->
    lager:debug("handle_info: ~p ~p ~n", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    init:stop().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions
