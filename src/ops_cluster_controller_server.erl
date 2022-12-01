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
-define(HeartbeatTime,20*1000).

%% External exports
-export([
	 connect_nodes/1,
	 initiate/0,
	 heartbeat/1,
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
	       cluster_dir,
	       current_cluster_state	       
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
heartbeat(Status)-> 
    gen_server:cast(?MODULE, {heartbeat,Status}).
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

    {ok, #state{current_cluster_state=n_a}}.   
 

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
handle_call({connect_nodes_info},_From, State) ->
    Reply=State#state.connect_nodes_info,
    {reply, Reply, State};

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
					 [controller_host_specs,ClusterDeployment],5000),
    {ok,WorkerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_deployment,read,
				     [worker_host_specs,ClusterDeployment],5000),
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
			     cluster_dir=ClusterDir,
			     current_cluster_state=[{present,[na]},{missing,[na]}]
			    },
  %  gl=InitialState,
    rpc:cast(node(),?MODULE,heartbeat,[InitialState#state.current_cluster_state]),
    {noreply,InitialState};


handle_cast({heartbeat,Status}, State) ->
    [{present,CurrentPresent},{missing,CurrentMissing}]=State#state.current_cluster_state,
    [{present,NewPresent},{missing,NewMissing}]=Status,
    
    NoChangeStatus=lists:sort(NewPresent) =:= lists:sort(CurrentPresent),
    case NoChangeStatus of
	false->
	    io:format("INFO: cluster state changed  ~p~n",[{date(),time()}]),  
	    CurrentPresentNodes=[Node||{_HostName,_NodeName,Node}<-CurrentPresent],
	    CurrentMissingNodes=[Node||{_HostName,_NodeName,Node}<-CurrentMissing],  
	    PresentNodes=[Node||{_HostName,_NodeName,Node}<-NewPresent],
	    MissingNodes=[Node||{_HostName,_NodeName,Node}<-NewMissing],  

	    io:format("INFO:CurrentPresentNodes ~p~n",[CurrentPresentNodes]),   
	    io:format("INFO:CurrentMissingNodes ~p~n",[CurrentMissingNodes]),
	  
	    io:format("INFO:NewPresentNodes ~p~n",[PresentNodes]),   
	    io:format("INFO:NewMissingNodes ~p~n",[MissingNodes]);
	true->
	    ok
    end,

    NewState=State#state{current_cluster_state=Status},
  
    spawn(fun()->hbeat(NewState) end),
    {noreply, NewState};

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
    timer:sleep(?HeartbeatTime),
    Status=case rpc:call(node(),?MODULE,connect_nodes,[State],60*1000) of 
	       {badrpc,_}->
		   Present=[{HostName,NodeName,Node}||{HostName,NodeName,Node}<-State#state.connect_nodes_info,
						      true=:=ops_vm:present(Node)],
		   Missing=[NodeInfo||NodeInfo<-State#state.connect_nodes_info,
				      false=:=lists:member(NodeInfo,Present)], 
		   
		   [{present,Present},{missing,Missing}];
	       S->
		   S
	   end,
    
    rpc:cast(node(),?MODULE,heartbeat,[Status]).



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10000).

connect_nodes(State)->
    Present=[{HostName,NodeName,Node}||{HostName,NodeName,Node}<-State#state.connect_nodes_info,
				       true=:=ops_vm:present(Node)],
    Missing=[NodeInfo||NodeInfo<-State#state.connect_nodes_info,
			    false=:=lists:member(NodeInfo,Present)],    
    ControllerHostSpecs=State#state.controller_host_specs,
    ClusterDir=State#state.cluster_dir,
    Cookie=State#state.cookie,
    PaArgs=" ",
    EnvArgs=" -detached ",
    NodesToConnect=[Node||{_HostName,_NodeName,Node}<-State#state.connect_nodes_info],
    create_connect_nodes(Missing,ControllerHostSpecs,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,[]),
    UpdatedPresent=[{HostName,NodeName,Node}||{HostName,NodeName,Node}<-State#state.connect_nodes_info,
				       true=:=ops_vm:present(Node)],
    UpdatedMissing=[NodeInfo||NodeInfo<-State#state.connect_nodes_info,
			    false=:=lists:member(NodeInfo,UpdatedPresent)],    
    [{present,UpdatedPresent},{missing,UpdatedMissing}].
    

create_connect_nodes([],_ControllerHostSpecs,_ClusterDir,_Cookie,_PaArgs,_EnvArgs,_NodesToConnect,Acc)->
    Acc;

create_connect_nodes([{HostName,NodeName,_Node}|T],ControllerHostSpecs,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,Acc)->
    
    
    {ok,ConnectNode,ClusterDir,_}=ops_vm:ssh_create(HostName,NodeName,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut),
    io:format("INFO: Creating Connect Nodes and Ops Pod   ~p~n",[{date(),time()}]),
    io:format("INFO:ConnectNode created  ~p~n",[ConnectNode]),    
    [HostSpec]=[HostSpec||HostSpec<-ControllerHostSpecs,
			  {ok,HostName}=:=db_host_spec:read(hostname,HostSpec)],
    {ok,PodNode,PodDir}=ops_pod:create(ConnectNode,ClusterDir,HostSpec),
    io:format("INFO:Pod created  ~p~n",[PodNode]),    
    {ok,pod_app,_PodAppDir}= ops_pod:load_start(PodNode,PodDir,"pod_app"),
    io:format("INFO:Load_Start Application in Pod   ~p~n",[{pod_app,PodNode}]),    
    {ok,db_etcd_app,_DbEtcdDir}= ops_pod:load_start(PodNode,PodDir,"db_etcd_app"),
    io:format("INFO:Load_Start Application in Pod   ~p~n",[{db_etcd_app,PodNode}]),    
    create_connect_nodes(T,ControllerHostSpecs,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,[{ConnectNode,PodNode,PodDir}|Acc]).


%cluster_deployment,
%	       cookie,
%	       controller_hosts,
%	       worker_hosts,
%	       connect_hosts,
%	       connect_nodename,
%	       connect_nodes_info,
%	       cluster_dir
