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
-module(pod_tests).      
 
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
    {ok,SimConNode,HostSpec}=create_sim_connect_node_test(),
    {ok,PodNode,PodDir}= ops_pod:create(SimConNode,HostSpec),
    {ok,pod_app,PodAppDir}= ops_pod:load_start(PodNode,PodDir),
    {ok,test_add,TestAddDir}= ops_pod:load_start(PodNode,PodDir,"adder"),
    {ok,db_etcd_app,DbEtcdDir}= ops_pod:load_start(PodNode,PodDir,"db_etcd_app"),
    
 %% resource_discovery
    pong=rpc:call(PodNode,db_etcd,ping,[]),
    pong=rpc:call(PodNode,common,ping,[]),
    pong=rpc:call(PodNode,rd,ping,[]),
   
    42=rpc:call(PodNode,rd,rpc_call,[adder,test_add,add,[20,22]]),
    pong=rpc:call(PodNode,rd,rpc_call,[db_etcd,db_etcd,ping,[]]),
    {error,[eexists_resources]}=rpc:call(PodNode,rd,rpc_call,[glurk,test_add,add,[20,22]]),
     
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_sim_connect_node_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %create a pod at c200 connect node 
    HostSpec="c50",
    Cookie=atom_to_list(erlang:get_cookie()),
    {ok,HostName}=inet:gethostname(),
    ClusterSpec="sim_con",
    {ok,SimConNode}=slave:start(HostName,ClusterSpec++"_connect"," -setcookie "++Cookie),
    pong=net_adm:ping(SimConNode),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,SimConNode,HostSpec}.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_start_appl(PodNode,PodDir)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=net_adm:ping(PodNode),
    true=filelib:is_dir(PodDir),

    % test_add
    BasicGitPath="https://github.com/joq62/",
    GitPath=BasicGitPath++"test_add.git",
    ApplDir=filename:join(PodDir,"test_add"),
    ApplEbin=filename:join(ApplDir,"ebin"),
    ok=file:make_dir(ApplDir),
    {ok,_}=appl:git_clone_to_dir(PodNode,GitPath,ApplDir),
    ok=appl:load(PodNode,test_add,[ApplDir,ApplEbin]),
    ok=appl:start(PodNode,test_add),

   

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_start_pod_appl(PodNode,PodDir)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    pong=net_adm:ping(PodNode),
    true=filelib:is_dir(PodDir),

    % common, nodelog , resource_discovery
    BasicGitPath="https://github.com/joq62/",
    PodAppGitPath=BasicGitPath++"pod_app.git",
    PodAppDir=filename:join(PodDir,"pod_app"),
    PodAppEbin=filename:join(PodAppDir,"ebin"),
    ok=file:make_dir(PodAppDir),
    {ok,_}=appl:git_clone_to_dir(PodNode,PodAppGitPath,PodAppDir),
    ok=appl:load(PodNode,pod_app,[PodAppDir,PodAppEbin]),
    ok=appl:start(PodNode,pod_app),
    pong=rpc:call(PodNode,common,ping,[]),
    pong=rpc:call(PodNode,rd,ping,[]),
    

    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_pod_test(SimConNode)->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    Cookie=atom_to_list(erlang:get_cookie()),
    {ok,HostName}=inet:gethostname(),
    PodName="pod1",
    PodDir=PodName++".dir",

    os:cmd("rm -rf "++PodDir),
  
    {ok,PodNode,PodDir}=ops_vm:create(HostName,SimConNode,PodName,PodDir,Cookie),
    
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,PodNode,PodDir}.
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
        
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
