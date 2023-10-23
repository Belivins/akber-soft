-module(server).
-export([main/0, send/2]).

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(pub, "Dealer"),
    chumak:connect(Socket, tcp, "localhost", 5555),
    Socket.


send(Socket, Text) ->
    chumak:send(Socket, Text).
