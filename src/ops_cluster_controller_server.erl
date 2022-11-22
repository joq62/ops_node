%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_cluster_controller_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,10000).

%% External exports
-export([
	 connect_nodes/1,
	 initiate/0,
	 heartbeat/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       cluster_deployment,
	       cookie,
	       controller_host_specs,
	       worker_host_specs,
	       connect_host_specs,
	       connect_nodename,
	       connect_nodes_info,
	       cluster_dir
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast
heartbeat()-> 
    gen_server:cast(?MODULE, {heartbeat}).
initiate()-> 
    gen_server:cast(?MODULE, {initiate}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) -> 
    io:format("Started Server ~p~n",[{?MODULE,?LINE}]),

    {ok, #state{}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({initiate}, State) ->
    
    AllEnvs=application:get_all_env(),
    {cluster_deployment,ClusterDeployment}=lists:keyfind(cluster_deployment,1,AllEnvs),
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_deployment,read,[cookie,ClusterDeployment],5000),
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_deployment,read,[dir,ClusterDeployment],5000),
    {ok,ControllerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_deployment,read,
					 [controller_hosts,ClusterDeployment],5000),
    {ok,WorkerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_deployment,read,
				     [worker_hosts,ClusterDeployment],5000),
    ConnectHostSpecs=list_duplicates:remove(lists:append(ControllerHostSpecs,WorkerHostSpecs)),
    ConnectNodeName=ClusterDeployment++"_"++"connect_node",
    ConnectNodesInfo=[{HostName,
		       ConnectNodeName,
		       list_to_atom(ConnectNodeName++"@"++HostName)}||HostName<-ConnectHostSpecs],    
    InitialState=State#state{cluster_deployment=ClusterDeployment,
			     cookie=Cookie,
			     controller_host_specs=ControllerHostSpecs,
			     worker_host_specs=WorkerHostSpecs,
			     connect_nodename=ConnectNodeName,
			     connect_host_specs=ConnectHostSpecs,
			     connect_nodes_info=ConnectNodesInfo,
			     cluster_dir=ClusterDir},
  %  gl=InitialState,
    ?MODULE:heartbeat(),
    {noreply,InitialState};


handle_cast({heartbeat}, State) ->
    io:format("  ~p~n",[{heartbeat,?MODULE,?LINE}]), 
    spawn(fun()->hbeat(State) end),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
hbeat(State)->
    ConnectResult=rpc:call(node(),?MODULE,connect_nodes,[State],5000),
    io:format("ConnectResult ~p~n",[{ConnectResult,?MODULE,?LINE}]), 
  
    timer:sleep(?HeartbeatTime),
    rpc:cast(node(),?MODULE,heartbeat,[]).



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10000).

connect_nodes(State)->
    Present=[{HostName,NodeName,Node}||{HostName,NodeName,Node}<-State#state.connect_nodes_info,
				       true=:=ops_vm:present(Node)],
    io:format("Present ~p~n",[{Present,?MODULE,?LINE}]), 
    MissingNodes=[NodeInfo||NodeInfo<-State#state.connect_nodes_info,
			    false=:=lists:member(NodeInfo,Present)],
    io:format("MissingNodes ~p~n",[{MissingNodes,?MODULE,?LINE}]), 
    NodeDir=State#state.cluster_dir,
    Cookie=State#state.cookie,
    PaArgs=" -pa "++NodeDir,
    EnvArgs=" -detached ",
    NodesToConnect=[Node||{_HostName,_NodeName,Node}<-State#state.connect_nodes_info],
    create_connect_nodes(MissingNodes,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,[]).
    

create_connect_nodes([],_NodeDir,_Cookie,_PaArgs,_EnvArgs,_NodesToConnect,Acc)->
    Acc;

create_connect_nodes([{HostName,NodeName,_Node}|T],NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,Acc)->
    R=ops_vm:ssh_create(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut),
    create_connect_nodes(T,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,[R|Acc]).


%cluster_deployment,
%	       cookie,
%	       controller_hosts,
%	       worker_hosts,
%	       connect_hosts,
%	       connect_nodename,
%	       connect_nodes_info,
%	       cluster_dir
