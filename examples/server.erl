-module(server).
-export([main/0, send/2]).

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(pub, "Dealer"),
    % chumak:connect(Socket, tcp, "localhost", 5555),
    case chumak:connect(Socket, tcp, "localhost", 5555) of
        {ok, _BindPid} ->
            io:format("Binding OK wiht Pid: ~p\n", [Socket]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,
    Socket.


send(Socket, Text) ->
    chumak:send(Socket, Text).
