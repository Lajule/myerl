-module(myerl_status).

-export([infos/1]).

infos(_Req) ->
    Infos = #{version => erlang:system_info(version)},
    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Infos)}.
