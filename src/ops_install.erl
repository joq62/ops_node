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
-module(ops_install).   

-define(TimeOut,7000).
 
-export([
	 start/0,
	 install/3
	]).
		 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
start()->
    {ok,_}=db_etcd_server:start(),
    {ok,_}=common:start(),
    pong=db_etcd:ping(),
    pong=common:ping(),
    db_etcd:install(),
    db_etcd:load(),
    install("many_c100_c200","c100","ops_node"),
    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------

install(ClusterDeployment,StartHostSpec,ApplSpec)->    
    %% Kill ops_node == connect Node
    {ok,Cookie}=db_cluster_deployment:read(cookie,ClusterDeployment),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    {ok,ClusterDir}=db_cluster_deployment:read(dir,ClusterDeployment),
    {ok,ControllerHostSpecs}=db_cluster_deployment:read(controller_hosts,ClusterDeployment),
    [{ok,HostName}]=[db_host_spec:read(hostname,HostSpec)||HostSpec<-ControllerHostSpecs,
							   StartHostSpec=:=HostSpec],
    NodeName=ClusterDeployment++"_"++"connect_node",    
    NodesToConnect=[node()],
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[]),
    timer:sleep(3000),
    PaArgs=" -pa "++ClusterDir,
    EnvArgs=" -detached ",
    ops_connect_node:create(HostName,NodeName,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut),
    pong=net_adm:ping(Node),
    
  
    {ok,GitPath}=db_appl_spec:read(gitpath,ApplSpec),
    {ok,AppId}=db_appl_spec:read(appl_name,ApplSpec),
    {ok,App}=db_appl_spec:read(app,ApplSpec),

    DirToClone=filename:join(ClusterDir,AppId),  
    ok=rpc:call(Node,file,make_dir,[DirToClone],5000),
    {ok,_CloneDir}=appl:git_clone_to_dir(Node,GitPath,DirToClone),
    Paths=[filename:join(DirToClone,"ebin")],
    ok=appl:load(Node,App,Paths),
    AppEnv=[{ops_node,[{cluster_deployment,ClusterDeployment}]}],
    ok=rpc:call(Node,application,set_env,[AppEnv],5000), 
    ok=appl:start(Node,App),
    ok.
    


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
