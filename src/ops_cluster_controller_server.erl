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
	 wanted_state_controllers/2,
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
	       present_controllers
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

    InstanceId=erlang:integer_to_list(os:system_time(microsecond),36)++"_id",
    
    InitialResult=[create_controller(InstanceId,ClusterSpec,HostSpec)||HostSpec<-ControllerHostSpecs],
    io:format("InitialResult  ~p~n",[{InitialResult, ?MODULE,?LINE,?FUNCTION_NAME}]),
    MissingControllers=[Node||{error,Node}<-InitialResult],
    PresentControllers=[Node||{ok,Node}<-InitialResult],

    io:format("INFO:PresentControllers ~p~n",[PresentControllers]),   
    io:format("INFO:MissingControllers ~p~n",[MissingControllers]),

    InitialState=State#state{cluster_spec=ClusterSpec,
			     instance_id=InstanceId,
			     missing_controllers=MissingControllers,
			     present_controllers=PresentControllers
			    },
  %  gl=InitialState,
    rpc:cast(node(),?MODULE,heartbeat,[{PresentControllers,MissingControllers}]),
    {noreply,InitialState};


handle_cast({heartbeat,{NewPresentControllers,NewMissingControllers}}, State) ->
    NoChangeStatus=lists:sort(NewPresentControllers) =:= lists:sort(State#state.present_controllers),
    case NoChangeStatus of
	false->
	    io:format("INFO: cluster state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:PresentControllers ~p~n",[State#state.present_controllers]),   
	    io:format("INFO:MissingControllers ~p~n",[State#state.missing_controllers]),
	  
	    io:format("INFO:NewMissingControllers ~p~n",[NewPresentControllers]),   
	    io:format("INFO:NewMissingControllers ~p~n",[NewMissingControllers]);
	true->
	    ok
    end,

    NewState=State#state{present_controllers=NewPresentControllers,
			 missing_controllers=NewMissingControllers},
  
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
    {Present,Missing}=rpc:call(node(),?MODULE,wanted_state_controllers,[InstanceId,ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[{Present,Missing}]).



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10000).

wanted_state_controllers(ClusterInstanceId,ClusterSpec)->
   
    check_status_controllers(ClusterInstanceId,ClusterSpec).
    
check_status_controllers(ClusterInstanceId,ClusterSpec)->
    {ok,ControllerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
					 [controller_host_specs,ClusterSpec],5000),
    
    ControllerNodes=rd:rpc_call(db_etcd,db_cluster_instance,controller_nodes,[ClusterInstanceId],5000),
    io:format("ControllerNodes  ~p~n",[{ControllerNodes, ?MODULE,?LINE,?FUNCTION_NAME}]),
    Present=[Node||Node<-ControllerNodes,
		   pong=:=net_adm:ping(Node)],
    Missing=[Node||Node<-ControllerNodes,
		pang=:=net_adm:ping(Node)],
    io:format("Present  ~p~n",[{Present, ?MODULE,?LINE,?FUNCTION_NAME}]),
    io:format("Missing  ~p~n",[{Missing, ?MODULE,?LINE,?FUNCTION_NAME}]),

    {Present,Missing}.
    

create_controller(ClusterInstanceId,ClusterSpec,HostSpec)->
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_spec,read,[dir,ClusterSpec],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
       
   %  PodName=erlang:integer_to_list(os:system_time(microsecond),36)++"_controller",
    PodName="controller",
    PaArgs=" -detached ",
    EnvArgs=" ",
    ControllerNodes=rd:rpc_call(db_etcd,db_cluster_instance,controller_nodes,[ClusterInstanceId],5000),
    io:format("ControllerNodes  ~p~n",[{ControllerNodes, ?MODULE,?LINE,?FUNCTION_NAME}]),

    % PodDir=filename:join(ClusterDir,PodName++".dir"),
    PodDir=ClusterDir,
    
    Result=case ops_vm:ssh_create(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ControllerNodes,?TimeOut) of
	       {ok,PodNode,PodDir,_}->
		   Type=controller,
		   Status=created,   
		   {atomic,ok}=db_cluster_instance:create(ClusterInstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status),
		   case net_adm:ping(PodNode) of
		       pong->
			   {ok,PodNode};
		       pang ->
			   {error,PodNode}
		   end;
	       {error,Reason}->
		   {error,list_to_atom(PodName++"@"++HostName)}
	   end,
    Result.
		   

		   
