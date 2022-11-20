%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_node_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 connect_nodes/0,
	 create_connect_node/1,
	 delete_connect_node/1
	]).

-export([
	 create_pod/3,
	 delete_pod/2,
	 pods/0
	]).

-export([
	 get_controller_node/1,
	 create_controller/1,
	 delete_controller/1,
	 controllers/0
	]).

%% Cluster
-export([
	 create_cluster_node/2,
	 delete_cluster_node/2,
	 is_cluster_node_present/2,
	 cluster_names/0,
	 cluster_intent/0,
	 cluster_intent/1
	]).

%% Pods
-export([
	 create_pod_node/3,
	 delete_pod_node/3,
	 is_pod_node_present/3,
	 pod_name_dir_list/2,
	 pod_intent/0,
	 pod_intent/1,
	 pod_candidates/1
	]).

%% Services
-export([
	 git_load_service/4,
	 load_service/4,
	 start_service/4,
	 stop_service/4,
	 unload_service/4,
	 is_service_running/4,
	 is_service_loaded/4,
	 service_intent/1
	]).


-export([
	 start/0,
	 stop/0,
	 appl_start/1,
	 ping/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-define(ControllerApp,controller_app).


-record(state,{
	       cluster_deployment,

	       pods,
	       controllers,
	       connect_nodes,
	       controller_app_env,
	       spec_files,
	       spec_dir,
	       cluster_name,
	      
	       cluster_spec,
	       nodes_to_connect
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

appl_start([])->
    application:start(?MODULE).
%% --------------------------------------------------------------------
connect_nodes()->
    gen_server:call(?MODULE,{connect_nodes},infinity).   

create_connect_node(HostName)->
    gen_server:call(?MODULE,{create_connect_node,HostName},infinity).   

delete_connect_node(HostName)->
    gen_server:call(?MODULE,{delete_connect_node,HostName},infinity). 

%% --------------------------------------------------------------------
create_pod(HostName,AppId,AppEnv)->
    gen_server:call(?MODULE,{create_pod,HostName,AppId,AppEnv},infinity).   

delete_pod(HostName,PodNode)->
    gen_server:call(?MODULE,{delete_pod,HostName,PodNode},infinity). 

pods()->
    gen_server:call(?MODULE,{pods},infinity).   


%% --------------------------------------------------------------------


create_controller(HostName)->
    gen_server:call(?MODULE,{create_controller,HostName},infinity).   

delete_controller(HostName)->
    gen_server:call(?MODULE,{delete_controller,HostName},infinity). 

controllers()->
    gen_server:call(?MODULE,{controllers},infinity).   
get_controller_node(HostName)->
    gen_server:call(?MODULE,{get_controller_node,HostName},infinity). 


%% --------------------------------------------------------------------
create_cluster_node(HostName,ClusterName)->
    gen_server:call(?MODULE,{create_cluster_node,HostName,ClusterName},infinity).   
delete_cluster_node(HostName,ClusterName)->
    gen_server:call(?MODULE,{delete_cluster_node,HostName,ClusterName},infinity).
is_cluster_node_present(HostName,ClusterName)->
    gen_server:call(?MODULE,{is_cluster_node_present,HostName,ClusterName},infinity).    
cluster_names()->
    gen_server:call(?MODULE,{cluster_names},infinity).   
cluster_intent()->
    gen_server:call(?MODULE,{cluster_intent},infinity). 
cluster_intent(ClusterName)->
    gen_server:call(?MODULE,{cluster_intent,ClusterName},infinity).   

%% --------------------------------------------------------------------

create_pod_node(HostName,ClusterName,PodName)->
    gen_server:call(?MODULE,{create_pod_node,HostName,ClusterName,PodName},infinity).   
delete_pod_node(HostName,ClusterName,PodName)->
    gen_server:call(?MODULE,{delete_pod_node,HostName,ClusterName,PodName},infinity).
is_pod_node_present(HostName,ClusterName,PodName)->
    gen_server:call(?MODULE,{is_pod_node_present,HostName,ClusterName,PodName},infinity).    
pod_name_dir_list(HostName,ClusterName)->
    gen_server:call(?MODULE,{pod_name_dir_list,HostName,ClusterName},infinity).   
pod_intent()->
    gen_server:call(?MODULE,{pod_intent},infinity). 
pod_intent(ClusterName)->
    gen_server:call(?MODULE,{pod_intent,ClusterName},infinity).  
pod_candidates(Constraints)->
    gen_server:call(?MODULE,{pod_candidates,Constraints},infinity).  

 

%% --------------------------------------------------------------------

git_load_service(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{git_load_service,HostName,ClusterName,PodName,Service},infinity). 
load_service(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{load_service,HostName,ClusterName,PodName,Service},infinity). 
start_service(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{start_service,HostName,ClusterName,PodName,Service},infinity). 
stop_service(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{stop_service,HostName,ClusterName,PodName,Service},infinity). 
unload_service(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{unload_service,HostName,ClusterName,PodName,Service},infinity). 
is_service_loaded(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{is_service_loaded,HostName,ClusterName,PodName,Service},infinity). 
is_service_running(HostName,ClusterName,PodName,Service)->
    gen_server:call(?MODULE,{is_service_running,HostName,ClusterName,PodName,Service},infinity). 

service_intent(ClusterName)->
    gen_server:call(?MODULE,{service_intent,ClusterName},infinity). 


  
	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

ping()->
    gen_server:call(?MODULE,{ping},infinity).

%% cast

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
    AllEnvs=application:get_all_env(),
    {cluster_deployment,ClusterDeployment}=lists:keyfind(cluster_deployment,1,AllEnvs),
    db_etcd:install(),
    db_etcd:load(),
    {ok,Cookie}=db_cluster_deployment:read(cookie,ClusterDeployment),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
        
    {ok, #state{cluster_deployment=ClusterDeployment}
    }.   
 

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
handle_call({connect_nodes},_From, State) ->
    Reply=State#state.connect_nodes,
    {reply, Reply, State};

handle_call({create_connect_node,HostName},_From, State) ->
    Reply=case lists:keymember(HostName,1,State#state.connect_nodes) of
	      true->
		  NewState=State,
		  {error,[connect_node_already_started_on_host,HostName]};
	      false->
		  NodeName=State#state.cluster_name++"_"++"connect",
		  PaArgs=" ",
		  EnvArgs=" -detached ",
		  Cookie=atom_to_list(erlang:get_cookie()),
		  NodeDir=State#state.cluster_name,
		  case ops_connect:create(HostName,
					  NodeName,
					  NodeDir,
					  Cookie,
					  PaArgs,
					  EnvArgs,
					  State#state.nodes_to_connect) of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      {ok,Node,NodeDir,PingResult}->
			  NewState=State#state{connect_nodes=[{HostName,Node,NodeDir,{date(),time()}}|State#state.connect_nodes]},
			  {ok,Node,PingResult}
		  end
	  end,
    {reply, Reply, NewState};

handle_call({delete_connect_node,HostName},_From, State) ->
    Reply=case lists:keyfind(HostName,1,State#state.connect_nodes) of
	      false->
		  NewState=State,
		  {error,[connect_node_not_started_on_host,HostName]};
	      {HostName,Node,NodeDir,{_Date,_Time}}->
		  case  ops_connect:delete(HostName,Node,NodeDir)  of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      ok->
			  NewState=State#state{connect_nodes=lists:keydelete(HostName,1,State#state.connect_nodes)},
			 ok
		  end
	  end,
	      
    {reply, Reply, NewState};

handle_call({pods},_From, State) ->
    Reply=State#state.pods,
    {reply, Reply, State};

handle_call({create_pod,HostName,AppId,AppEnv},_From, State) ->
    Reply=case lists:keyfind(HostName,1,State#state.connect_nodes) of
	      false->
		  NewState=State,
		  {error,[connect_node_not_started_on_host,HostName]};
	      {HostName,ConnectNode,ClusterDir,{_Date,_Time}}->
		  Cookie=atom_to_list(erlang:get_cookie()),
		  Unique=erlang:integer_to_list(os:system_time(microsecond),36),
		  PodNodeName=ClusterDir++"_"++Unique,
		  PodDirName=Unique++".pod_dir",
		  PodDir=filename:join(ClusterDir,PodDirName),
		  case ops_pod:create(HostName,ConnectNode,PodNodeName,PodDir,Cookie,AppId,AppEnv) of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      {ok,PodeNode,PodDir}->
			  NewState=State#state{pods=[
						     [{host_name,HostName},
						      {node,PodeNode},
						      {dir,PodDir},
						      {time,{date(),time()}}
						     ]|State#state.pods]},
			  {ok,HostName,PodeNode}		  
		  end
	  end,
    {reply, Reply, NewState};

handle_call({delete_pod,HostName,PodeNode},_From, State) ->
    Reply=case lists:keyfind(HostName,1,State#state.connect_nodes) of
	      false->
		  NewState=State,
		  {error,[connect_node_not_started_on_host,HostName]};
	      {HostName,ConnectNode,_ClusterDir,{_Date,_Time}}->
		  MemberPod=[PodInfoList||PodInfoList<-State#state.pods,
					  true=:=lists:member({node,PodeNode},PodInfoList)],
		  case MemberPod of
		      []->
			  NewState=State,
			  {error,[pod_not_started,HostName,PodeNode]};
		      [PodInfoList]->
			  {dir,PodDir}=lists:keyfind(dir,1,PodInfoList),
			  NewPodList=[X||X<-State#state.pods,
					 false=:=lists:member({node,PodeNode},X)],
			  NewState=State#state{pods=NewPodList},
			  ops_pod:delete(ConnectNode,PodeNode,PodDir)
		  end
	   end,
    {reply, Reply, NewState};
   

  
%% --------------------------------------------------------------------

handle_call({controllers},_From, State) ->
    Reply=State#state.controllers,
    {reply, Reply, State};


handle_call({get_controller_node,HostName},_From, State) ->
    Member=[ControllerInfoList||ControllerInfoList<-State#state.controllers,
				true=:=lists:member({host_name,HostName},ControllerInfoList)],
    Reply= case Member of
	       []->
  		   {error,[not_started,HostName]};
	       [ControllerInfoList]->
		   {node,ControllerNode}=lists:keyfind(node,1,ControllerInfoList),
		   {ok,ControllerNode}
	   end,
    {reply, Reply, State};


handle_call({create_controller,HostName},_From, State) ->
    %[{host_name,HostName},{node,Node},{dir,Dir},{time,{Date,Time}}]
    Reply=case lists:keyfind(HostName,1,State#state.connect_nodes) of
	      false->
		  NewState=State,
		  {error,[connect_node_not_started_on_host,HostName]};
	      {HostName,ConnectNode,ClusterDir,{_Date,_Time}}->
		  AppId="controller_app",
		  AppEnv=State#state.controller_app_env,
		  Cookie=atom_to_list(erlang:get_cookie()),
		  Unique=erlang:integer_to_list(os:system_time(microsecond),36),
		  PodNodeName=ClusterDir++"_"++Unique,
		  PodDirName=Unique++".pod_dir",
		  PodDir=filename:join(ClusterDir,PodDirName),
		  case ops_pod:create(HostName,ConnectNode,PodNodeName,PodDir,Cookie,AppId,AppEnv) of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      {ok,PodeNode,PodDir}->
			  NewState=State#state{pods=[
						     [{host_name,HostName},
						      {node,PodeNode},
						      {dir,PodDir},
						      {time,{date(),time()}}
						     ]|State#state.pods]},
			  {ok,HostName,PodeNode}		  
		  end
	  end,
    {reply, Reply, NewState};




handle_call({delete_controller,HostName},_From, State) ->
    
    Member=[ControllerInfoList||ControllerInfoList<-State#state.controllers,
				true=:=lists:member({host_name,HostName},ControllerInfoList)],
    Reply= case Member of
	       []->
		   NewState=State,
		   {error,[not_started,HostName]};
	       [ControllerInfoList]->
		   {node,Node}=lists:keyfind(node,1,ControllerInfoList),
		   {dir,Dir}=lists:keyfind(dir,1,ControllerInfoList),
		   NewControllerList=[X||X<-State#state.controllers,
					 false=:=lists:member({host_name,HostName},X)],
		   NewState=State#state{controllers=NewControllerList},
		   ops_controller:delete(Node,Dir)
	   end,
    {reply, Reply, NewState};
   

  
%% --------------------------------------------------------------------

handle_call({git_load_service,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:git_load_service(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({load_service,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:load_service(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({start_service,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:start_service(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({stop_service,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:stop_service(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({unload_service,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:unload_service(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({is_service_running,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:is_service_running(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({is_service_loaded,HostName,ClusterName,PodName,Service},_From, State) ->
    Reply=ops_misc:is_service_loaded(HostName,ClusterName,PodName,Service,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({service_intent,ClusterName},_From, State) ->
    Reply=ops_misc:service_intent(ClusterName,State#state.cluster_spec),
    {reply, Reply, State};


  
%% --------------------------------------------------------------------


handle_call({cluster_intent},_From, State) ->
    Reply=ops_misc:cluster_intent(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({cluster_intent,ClusterName},_From, State) ->
    Reply=ops_misc:cluster_intent(ClusterName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({cluster_names},_From, State) ->
    Reply=ops_misc:cluster_names(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({create_cluster_node,HostName,ClusterName},_From, State) ->
    Reply=ops_misc:create_cluster_node(HostName,ClusterName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({delete_cluster_node,HostName,ClusterName},_From, State) ->
    Reply=ops_misc:delete_cluster_node(HostName,ClusterName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({is_cluster_node_present,HostName,ClusterName},_From, State) ->
    Reply=ops_misc:is_cluster_node_present(HostName,ClusterName,State#state.cluster_spec),
    {reply, Reply, State};


handle_call({cluster_spec},_From, State) ->
    Reply=cluster_data:cluster_all_names(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({deployment_spec},_From, State) ->
 %   Reply=cluster_data:deployment_all_names(State#state.deployment_spec),
    Reply='not implmented',
    {reply, Reply, State};

 
%% --------------------------------------------------------------------

handle_call({pod_candidates,Constraints},_From, State) ->
    Reply=ops_misc:pod_candidates(Constraints,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({pod_intent},_From, State) ->
    Reply=ops_misc:pod_intent(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({pod_intent,ClusterName},_From, State) ->
    Reply=ops_misc:pod_intent(ClusterName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({pod_name_dir_list,HostName,ClusterName},_From, State) ->
    Reply=ops_misc:pod_name_dir_list(HostName,ClusterName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({create_pod_node,HostName,ClusterName,PodName},_From, State) ->
    Reply=ops_misc:create_pod_node(HostName,ClusterName,PodName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({delete_pod_node,HostName,ClusterName,PodName},_From, State) ->
    Reply=ops_misc:delete_pod_node(HostName,ClusterName,PodName,State#state.cluster_spec),
    {reply, Reply, State};

handle_call({is_pod_node_present,HostName,ClusterName,PodName},_From, State) ->
    Reply=ops_misc:is_pod_node_present(HostName,ClusterName,PodName,State#state.cluster_spec),
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

handle_info({ssh_cm,_,_}, State) ->
   
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
