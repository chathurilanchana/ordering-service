-module(ordering_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_ordering_service/0,start_optimised_sequencer/0,start_old_sequencer/0,start_ordering_service_ets/0,start_ordering_service_ets_ordered/0,start_receiver/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_optimised_sequencer()->
    lager:info("supervisor starting the optimised sequencer"),
    supervisor:start_child(?MODULE,{riak_kv_optimised_sequencer,
        {riak_kv_optimised_sequencer, start_link, []},
        permanent, 5000, worker, [riak_kv_optimised_sequencer]}).


start_ordering_service_ets()->
    lager:info("supervisor starting the ordering service ets"),
    supervisor:start_child(?MODULE,{riak_kv_ord_service_ets,
        {riak_kv_ord_service_ets, start_link, []},
        temporary, 5000, worker, [riak_kv_ord_service_ets]}).

start_ordering_service_ets_ordered()->
    lager:info("supervisor starting the ordering service normal"),
    supervisor:start_child(?MODULE,{riak_kv_ord_service_ets_ordered,
        {riak_kv_ord_service_ets_ordered, start_link, []},
        temporary, 5000, worker, [riak_kv_ord_service_ets_ordered]}).

start_receiver()->
    lager:info("supervisor starting the receiver for dummy label receiving"),
    supervisor:start_child(?MODULE,{riak_kv_ord_service_receiver,
        {riak_kv_ord_service_receiver, start_link, []},
        permanent, 5000, worker, [riak_kv_ord_service_receiver]}).

start_ordering_service()->
    lager:info("supervisor starting the ordering service normal"),
    supervisor:start_child(?MODULE,{riak_kv_ord_service,
        {riak_kv_ord_service, start_link, []},
        permanent, 5000, worker, [riak_kv_ord_service]}).

start_old_sequencer()->
    lager:info("supervisor starting the old sequencer"),
    supervisor:start_child(?MODULE,{riak_kv_sequencer,
        {riak_kv_sequencer, start_link, []},
        permanent, 5000, worker, [riak_kv_sequencer]}).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

