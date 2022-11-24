%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(pod_deployment_2_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=read_specs_test(),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
     %{HostSpec,ConnectNode,PodNode, PodDir, [{“app_id”,app,Gitpath},,,],candidate,loaded, started 
    
    ClusterDeploymentSpec=ops_node:cluster_deployment_spec(),
    "many_c100_c200"=ClusterDeploymentSpec,

    {ok,WorkerHostSpecs}=ops_node:worker_host_specs(ClusterDeploymentSpec),
    ["c100","c200"]=WorkerHostSpecs,
       
    ClusterApplicationDeployments=ops_node:cluster_application_deployments(cluster_spec,ClusterDeploymentSpec),
    [ClusterSpec|_]=ClusterApplicationDeployments,
    "a"=ClusterSpec,
    {ok,ApplDeploymentSpecs}=ops_node:cluster_application_deployments(appl_deployment_specs,ClusterSpec),
    ["math"]=ApplDeploymentSpecs,

    %%%
    PodDeployInfoResult=pod_deployment_info(ClusterApplicationDeployments),
  %  PodDeployInfoList=pod_deployment_info(ApplDeploymentSpecs,WorkerHostSpecs),
   % [true,true]=[lists:member({spec_id,"math"},L)||L<-PodDeployInfoList],
  
%% Store in db_etcd
%% DeployId, Stats, PodDeployInfo + Status candidate, deployed 
   % [
   %  [{spec_id,"math"},{host_spec,"c200"},{pod_node_name,"GFP3LIDSE4_pod"},
   %   {pod_node,'GFP3LIDSE4_pod@c200'},{pod_dir,"GFP3LIDSE4_pod.dir"},
   %   {appl_name,"math"},{app,math},{gitpath,"https://github.com/joq62/math.git"}
   %  ],
   %  [{spec_id,"math"},{host_spec,"c100"},{pod_node_name,"GFP3LIDRZ1_pod"},
   %   {pod_node,'GFP3LIDRZ1_pod@c100'},{pod_dir,"GFP3LIDRZ1_pod.dir"},
   %   {appl_name,"math"},{app,math},{gitpath,"https://github.com/joq62/math.git"}
   %  ]
   % ]=PodDeployInfoList,

  %  [Status,DepId,ClusterApplicationDeploymentId,ClusterSpecId,HostSpecId,ApplSpecId]=x,
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.



pod_deployment_info(ClusterApplicationDeployments)->
    pod_deployment_info(ClusterApplicationDeployments,[]).

pod_deployment_info([],Acc)->
    Acc;
pod_deployment_info([ClusterApplicationDeploymentId|T],Acc) ->

    {ok,ClusterDeploymentId}=ops_node:cluster_application_deployments(cluster_specs,ClusterApplicationDeploymentId),
    {ok,WorkerHostSpecs}=db_cluster_deployment:read(worker_host_specs,ClusterDeploymentId),
    {ok,ApplDeploymentSpecs}=ops_node:cluster_application_deployments(appl_deployment_specs,ClusterApplicationDeploymentId),
  %  DeplId=erlang:integer_to_list(os:system_time(microsecond),36)++"_pod_info",
    glurk=pod_deployment_info(ApplDeploymentSpecs,WorkerHostSpecs,[]),
    


  %  R=ops_node:create_pod_info(DeplId,ApplDeployId,Status,
%			       PodName,PodNode,PodDir,
%			       ClusterApplicationDeploymentId,ClusterSpecId,
%			       HostSpecId,
%			       ApplSpecId),
    
    pod_deployment_info(T,[glurk|Acc]).

pod_deployment_info([],WorkerHostSpecs,Acc)->
    Acc;
pod_deployment_info([ApplDeploymentSpec|T],WorkerHostSpecs,Acc) ->
 %   {_SpecId,ApplSpec,Vsn,NumInstances,[]}=ops_node:application_deployment_info(ApplDeploymentSpec),
    PodInfoList=appl_info(ApplDeploymentSpec,WorkerHostSpecs),
  %  NewAcc=lists:append(PodInfoList,Acc),
    NewAcc=[PodInfoList|Acc],
    pod_deployment_info(T,WorkerHostSpecs,NewAcc).
   

%% Affinity = []

appl_info({SpecId,ApplSpec,Vsn,NumInstances,[]},HostSpecList)->
    appl_info(SpecId,ApplSpec,Vsn,NumInstances,HostSpecList,[]).
appl_info(_SpecId,_ApplSpec,_Vsn,0,_HostSpecList,Acc)->    
    Acc;
appl_info(SpecId,ApplSpec,Vsn,N,[HostSpec|T],Acc)->
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    PodName=erlang:integer_to_list(os:system_time(microsecond),36)++"_pod",  
    PodNode=list_to_atom(PodName++"@"++HostName),
    PodDir=PodName++".dir",
    Info=[{host_spec,HostSpec},{appl_spec,ApplSpec},
		{pod_name,PodName},{pod_node,PodNode},
		{pod_dir,PodDir}],
    NewAcc=[Info|Acc],
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    appl_info(SpecId,ApplSpec,Vsn,N-1,RotatedHostSpecList,NewAcc).
 
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=db_etcd:ping(),
    pong=ops_node:ping(),
    pong=ops_cluster_controller_server:ping(),
    pong=ops_application_controller_server:ping(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.