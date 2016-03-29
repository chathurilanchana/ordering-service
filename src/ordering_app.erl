-module(ordering_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,forward_to_ordering_service/3,forward_to_sequencer/3,forward_to_old_sequencer/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:load(lager),
    lager:start(),
    ordering_sup:start_link().

stop(_State) ->
    ok.



forward_to_sequencer(RObj, W, {?MODULE, [Node, ClientId]})->
    Me = self(),
    ReqId = mk_reqid(),
    riak_kv_optimised_sequencer:forward_put_to_sequencer(RObj, [{w, W}, {dw, W}], [Node, ClientId],ReqId,Me),
    wait_for_reply(ReqId, 10000). %may be we need to set to ?DEFAULT_TIMEOUT

forward_to_old_sequencer( {?MODULE, [_Node, _ClientId]})->
    SequenceId=riak_kv_sequencer:get_sequence_number(),
    {ok,SequenceId}.

forward_to_ordering_service(Label,ClientId,MaxTS)->
    riak_kv_ord_service_ets_ordered:add_label(Label,ClientId,MaxTS),
    ok.

wait_for_reply(ReqId,Timeout)->
    % io:format("waiting for reply node is ~p cookie is ~p ~n",[node(),erlang:get_cookie()]),
    receive
        {ReqId, {error, overload}=Response} ->
            %  io:format("response is ~p",[Response]),
            case app_helper:get_env(riak_kv, overload_backoff, undefined) of
                Msecs when is_number(Msecs) ->
                    timer:sleep(Msecs);
                undefined ->
                    ok
            end,
            Response;
        {ReqId, Response} -> %io:format("response is ~p",[Response]),
            Response
    after Timeout ->
        io:format("timeout occured whiiel waiting ~n"),
        {error, timeout}
    end.

mk_reqid() ->
    erlang:phash2({self(), os:timestamp()}). % only has to be unique per-pid