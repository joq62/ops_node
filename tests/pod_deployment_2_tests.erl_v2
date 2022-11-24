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

    PodDeployInfoList=pod_deployment_info(ApplDeploymentSpecs,WorkerHostSpecs),
    [true,true]=[lists:member({spec_id,"math"},L)||L<-PodDeployInfoList],
  
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


    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

pod_deployment_info(ApplDeploymentSpecs,WorkerHostSpecs)->
    pod_deployment_info(ApplDeploymentSpecs,WorkerHostSpecs,[]).

						
pod_deployment_info([],WorkerHostSpecs,Acc)->
    Acc;
pod_deployment_info([ApplDeploymentSpec|T],WorkerHostSpecs,Acc) ->
    ApplDeploymentInfo=ops_node:application_deployment_info(ApplDeploymentSpec),
    PodInfoList=appl_info(ApplDeploymentInfo,WorkerHostSpecs),
    NewAcc=lists:append(PodInfoList,Acc),
    pod_deployment_info(T,WorkerHostSpecs,NewAcc).
   

%% Affinity = []
appl_info({SpecId,ApplSpec,Vsn,NumInstances,[]},HostSpecList)->
    appl_info(SpecId,ApplSpec,Vsn,NumInstances,HostSpecList,[]).

appl_info(_SpecId,_ApplSpec,_Vsn,0,_HostSpecList,Acc)->    
    Acc;
appl_info(SpecId,ApplSpec,Vsn,N,[HostSpec|T],Acc)->
    {ok,HostName}=ops_node:hostname(HostSpec),
    PodNodeName=erlang:integer_to_list(os:system_time(microsecond),36)++"_pod",  
    PodNode=list_to_atom(PodNodeName++"@"++HostName),
    PodDir=PodNodeName++".dir",
    {ok,ApplName}=ops_node:appl_name(ApplSpec),
    {ok,App}=ops_node:app(ApplSpec),
    {ok,GitPath}=ops_node:gitpath(ApplSpec),

    NewAcc=[[{spec_id,SpecId},
	     {host_spec,HostSpec},
	     {pod_node_name,PodNodeName},
	     {pod_node,PodNode},
	     {pod_dir,PodDir},
	     {appl_name,ApplName},
	     {app,App},
	     {gitpath,GitPath}
	    ]|Acc],
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
