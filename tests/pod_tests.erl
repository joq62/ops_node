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
    {ok,SimConNode}=create_sim_connect_node_test(),
    {ok,PodNode,PodDir}= create_pod_test(SimConNode),
    ok=load_start_pod_appl(PodNode,PodDir),
    ok= load_start_appl(PodNode,PodDir),
  

    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.



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

    %% resource_discovery
    ok=rpc:call(PodNode,rd,add_local_resource,[math_add,PodNode],5000),
    ok=rpc:call(PodNode,rd,add_target_resource_type,[math_add],5000),
    ok=rpc:call(PodNode,rd,trade_resources,[],5000),
    timer:sleep(2000),

    42=rpc:call(PodNode,rd,rpc_call,[math_add,test_add,add,[20,22]]),
    {error,[eexists_resources]}=rpc:call(PodNode,rd,rpc_call,[glurk,test_add,add,[20,22]]),
     

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
create_sim_connect_node_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %create a pod at c200 connect node 
    Cookie=atom_to_list(erlang:get_cookie()),
    {ok,HostName}=inet:gethostname(),
    ClusterSpec="many_c200_c201",
    {ok,SimConNode}=slave:start(HostName,ClusterSpec++"_connect"," -setcookie "++Cookie),
    pong=net_adm:ping(SimConNode),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,SimConNode}.
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
