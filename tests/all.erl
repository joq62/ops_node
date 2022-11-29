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
-module(all).      
 
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
    io:format("Start State ~p~n",[{ops_node:get_state(),?MODULE,?FUNCTION_NAME}]),
     
    ok=pod_tests:start(),
    
   
   
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 %   timer:sleep(2000),
 %  init:stop(),
    ok.


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
-define(ClusterDeployment,"many_c200_c201").

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
        
    AppEnv=[{ops_node,[{cluster_deployment,?ClusterDeployment}]}],
    erlang:set_cookie(node(),cookie_many_200_c201),
    rpc:multicall(['many_c200_c201_connect_node@c200',
		   'many_c200_c201_connect_node@c201'],init,stop,[]),
    ok=application:set_env(AppEnv),
    ok=application:start(ops_node),
    pong=ops_node:ping(),
    pong=ops_cluster_controller_server:ping(),
    pong=ops_application_controller_server:ping(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
