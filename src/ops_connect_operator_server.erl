%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_connect_operator_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).

%% External exports
-export([
	 wanted_state/2,
	 
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
	       cluster_spec,
	       instance_id,
	       missing_controllers,
	       present_controllers,
	       present_connect_nodes,
	       missing_connect_nodes
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

    {ok, #state{cluster_spec=undefined,
		instance_id=undefined,
		missing_controllers=undefined,
		present_controllers=undefined}}.   
 

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
    Reply=State,
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
    {cluster_spec,ClusterSpec}=lists:keyfind(cluster_spec,1,AllEnvs),
    ok=rd:rpc_call(db_etcd,db_cluster_instance,create_table,[],5000),
    {ok,ControllerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
					 [controller_host_specs,ClusterSpec],5000),
    {ok,WorkerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
				     [worker_host_specs,ClusterSpec],5000),
    ConnectHostSpecs=list_duplicates:remove(lists:append(ControllerHostSpecs,WorkerHostSpecs)),
    InstanceId=erlang:integer_to_list(os:system_time(microsecond),36)++"_id",
    _InitialResult=create_connect_nodes(InstanceId,ClusterSpec,ConnectHostSpecs),
    
    PresentConnectNodes=present_connect_nodes(InstanceId),
    MissingConnectNodes=missing_connect_nodes(InstanceId),
    io:format("INFO:PresentConnectNodes ~p~n",[PresentConnectNodes]),   
    io:format("INFO:MissingConnectNodes ~p~n",[MissingConnectNodes]),

    InitialState=State#state{cluster_spec=ClusterSpec,
			     instance_id=InstanceId,
			     missing_connect_nodes=MissingConnectNodes,
			     present_connect_nodes=PresentConnectNodes
			    },
  %  gl=InitialState,
    rpc:cast(node(),?MODULE,heartbeat,[{PresentConnectNodes,MissingConnectNodes}]),
    {noreply,InitialState};


handle_cast({heartbeat,{NewPresentConnectNodes,NewMissingConnectNodes}}, State) ->
    NoChangeStatus=lists:sort(NewPresentConnectNodes) =:= lists:sort(State#state.present_connect_nodes),
    case NoChangeStatus of
	false->
	    io:format("INFO: cluster state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:PresentConnectNodes ~p~n",[State#state.present_connect_nodes]),   
	    io:format("INFO:MissingConnectNodes ~p~n",[State#state.missing_connect_nodes]),
	  
	    io:format("INFO:NewPresentConnectNodes ~p~n",[NewPresentConnectNodes]),   
	    io:format("INFO:NewMissingConnectNodes ~p~n",[NewMissingConnectNodes]);
	true->
	    ok
    end,

    NewState=State#state{present_connect_nodes=NewPresentConnectNodes,
			 missing_connect_nodes=NewMissingConnectNodes},
  
    spawn(fun()->hbeat(State#state.instance_id,State#state.cluster_spec) end),
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
handle_info({ssh_cm,_,{closed,0}}, State) ->
    {noreply, State};

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
hbeat(InstanceId,ClusterSpec)->
    timer:sleep(?HeartbeatTime),
    {Present,Missing}=rpc:call(node(),?MODULE,wanted_state,[InstanceId,ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[{Present,Missing}]).



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10000).
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
wanted_state(InstanceId,ClusterSpec)->
    NodesToConnect=db_cluster_instance:nodes(connect,InstanceId),
    MissingConnectNodes=missing_connect_nodes(InstanceId),
    [create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)||PodNode<-MissingConnectNodes],
    {present_connect_nodes(InstanceId),missing_connect_nodes(InstanceId)}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_connect_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(connect,InstanceId), 
	   pang=:=net_adm:ping(Node)].
present_connect_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(connect,InstanceId), 
	   pong=:=net_adm:ping(Node)].
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
create_connect_nodes(InstanceId,ClusterSpec,ConnectHostSpecs)->
    DbaseInfo=connect_dbase_info(ConnectHostSpecs,InstanceId,ClusterSpec,[]),
    io:format("DbaseInfo ~p~n",[{DbaseInfo,?MODULE,?LINE,?FUNCTION_NAME}]),
    NodesToConnect=db_cluster_instance:nodes(connect,InstanceId),
    MissingConnectNodes=[Node||Node<-NodesToConnect, 
			       pang=:=net_adm:ping(Node)],
    [create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)||PodNode<-MissingConnectNodes].
    

create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)->
    io:format("INFO: create new/restart connect_node  ~p~n",[{date(),time()}]), 
    io:format("INFO: Cluster and PodNode   ~p~n",[{ClusterSpec,PodNode}]),  
    
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    {ok,HostSpec}=rd:rpc_call(db_etcd,db_cluster_instance,read,[host_spec,InstanceId,PodNode],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    
    {ok,PodName}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_name,InstanceId,PodNode],5000),
    {ok,PodDir}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_dir,InstanceId,PodNode],5000),
    PaArgs=" -detached ",
    EnvArgs=" ",
    ops_vm:ssh_create(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut). 

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

connect_dbase_info([],_ClusterInstanceId,_ClusterSpec,Acc)->
    Acc;
connect_dbase_info([HostSpec|T],ClusterInstanceId,ClusterSpec,Acc)->
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_spec,read,[dir,ClusterSpec],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    PodName=ClusterSpec++"_connect",
    PodNode=list_to_atom(PodName++"@"++HostName),
    PodDir=ClusterDir,
    Type=connect,
    Status=candidate,
    NewAcc=case db_cluster_instance:create(ClusterInstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status) of
	       {atomic,ok}->
		   [ok|Acc];
	       Reason->
		   [{error,[Reason]}|Acc]
	   end,
    connect_dbase_info(T,ClusterInstanceId,ClusterSpec,NewAcc).
     
