%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_controller_operator_server).
 
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
	       present_controller_nodes,
	       missing_controller_nodes
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
    gen_server:call(?MODULE, {initiate},infinity).

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
		missing_controller_nodes=undefined,
		present_controller_nodes=undefined}}.   
 

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

handle_call({initiate},_From, State) ->
    AllEnvs=application:get_all_env(),
    {cluster_spec,ClusterSpec}=lists:keyfind(cluster_spec,1,AllEnvs),

    {ok,ControllerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
					 [controller_host_specs,ClusterSpec],5000),
    InstanceId=erlang:integer_to_list(os:system_time(microsecond),36)++"_id",
    
   % InitialResult=create_controller_nodes(InstanceId,ClusterSpec,ControllerHostSpecs),
   % io:format("INFO:InitialResult  ~p~n",[{InitialResult, ?MODULE,?LINE}]),   
    PresentConnectNodes= [], % present_connect_nodes(InstanceId),
    MissingConnectNodes=[], %missing_connect_nodes(InstanceId),
  %  io:format("INFO:PresentConnectNodes ~p~n",[PresentConnectNodes]),   
   % io:format("INFO:MissingConnectNodes ~p~n",[MissingConnectNodes]),

    InitialState=State#state{cluster_spec=ClusterSpec,
			     instance_id=InstanceId,
			     missing_controller_nodes=MissingConnectNodes,
			     present_controller_nodes=PresentConnectNodes
			    },
    rpc:cast(node(),?MODULE,heartbeat,[{PresentConnectNodes,MissingConnectNodes}]),
    Reply=ok,
    {reply, Reply, InitialState};


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

handle_cast({heartbeat,{error,[no_connect_nodes]}}, State) ->
    io:format("ERROR: no connect nodes available ~p~n",[{date(),time()}]),  
    spawn(fun()->hbeat(State#state.instance_id,State#state.cluster_spec) end),
    {noreply, State};
    
handle_cast({heartbeat,{ok,NewPresentControllerNodes,NewMissingControllerNodes}}, State) ->
    NoChangeStatus=lists:sort(NewPresentControllerNodes) =:= lists:sort(State#state.present_controller_nodes),
    case NoChangeStatus of
	false->
	    io:format("INFO: cluster state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:PresentControllerNodes ~p~n",[State#state.present_controller_nodes]),   
	    io:format("INFO:MissingControllerNodes ~p~n",[State#state.missing_controller_nodes]),
	  
	    io:format("INFO:NewPresentControllerNodes ~p~n",[NewPresentControllerNodes]),   
	    io:format("INFO:NewMissingControllerNodes ~p~n",[NewMissingControllerNodes]);
	true->
	    ok
    end,

    NewState=State#state{present_controller_nodes=NewPresentControllerNodes,
			 missing_controller_nodes=NewMissingControllerNodes},
  
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
    Result=rpc:call(node(),?MODULE,wanted_state,[InstanceId,ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[Result]).



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
    Result=case db_cluster_instance:nodes(connect,InstanceId) of
		     []->
			 {error,[no_connect_nodes]};
		     ConnectNodes->
			 MissingControllerNodes=missing_controller_nodes(InstanceId),
			 [create_controller_node(InstanceId,ClusterSpec,PodNode,ConnectNodes)||PodNode<-MissingControllerNodes],
			 {ok,present_controller_nodes(InstanceId),missing_controller_nodes(InstanceId)}
		 end,
    Result.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_controller_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(controller,InstanceId), 
	   pang=:=net_adm:ping(Node)].
present_controller_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(controller,InstanceId), 
	   pong=:=net_adm:ping(Node)].
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
create_controller_nodes(InstanceId,ClusterSpec,ControllerHostSpecs)->
    DbaseInfo=controller_dbase_info(ControllerHostSpecs,InstanceId,ClusterSpec,[]),
    io:format("DbaseInfo ~p~n",[{DbaseInfo,?MODULE,?LINE,?FUNCTION_NAME}]),

    ConnectNodes=db_cluster_instance:nodes(connect,InstanceId),
    ControllerNodes=db_cluster_instance:nodes(controller,InstanceId),

    MissingControllerNodes=[Node||Node<-ControllerNodes, 
			       pang=:=net_adm:ping(Node)],
    [create_controller_node(InstanceId,ClusterSpec,PodNode,ConnectNodes)||PodNode<-MissingControllerNodes].
    

create_controller_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)->
    io:format("INFO: create new/restart connect_node  ~p~n",[{date(),time()}]), 
    io:format("INFO: Cluster and PodNode   ~p~n",[{ClusterSpec,PodNode}]),  
    
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    {ok,HostSpec}=rd:rpc_call(db_etcd,db_cluster_instance,read,[host_spec,InstanceId,PodNode],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    
    {ok,PodName}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_name,InstanceId,PodNode],5000),
    {ok,PodDir}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_dir,InstanceId,PodNode],5000),
    PaArgs=" -detached ",
    EnvArgs=" ",

    ControllerNode=glurk,
    ops_pod:create(HostName,ControllerNode,PodName,PodDir,Cookie).
    

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

controller_dbase_info([],_ClusterInstanceId,_ClusterSpec,Acc)->
    Acc;
controller_dbase_info([HostSpec|T],ClusterInstanceId,ClusterSpec,Acc)->
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_spec,read,[dir,ClusterSpec],5000),
  %  {ok,PodNode,PodDir}=ops_pod:create(ConnectNode,RootDir,HostSpec),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    UniqueId=rpc:call(node(),os,system_time,[microsecond],5000),
    PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_controller",
    PodNode=list_to_atom(PodName++"@"++HostName),
    PodDirName=PodName++".dir",
    PodDir=filename:join(ClusterDir,PodDirName),
    Type=controller,
    Status=candidate,
    NewAcc=case db_cluster_instance:create(ClusterInstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status) of
	       {atomic,ok}->
		   [ok|Acc];
	       Reason->
		   [{error,[Reason]}|Acc]
	   end,
    controller_dbase_info(T,ClusterInstanceId,ClusterSpec,NewAcc).

