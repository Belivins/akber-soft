-module(dealer).
-export([main/0]).


start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(dealer, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "127.0.0.1", 5555),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    {ok, Multipart} = chumak:recv_multipart(Socket),
    case Multipart of
        [<<"EXIT">>] ->
            ok;
        _ ->
            Parent ! {recv, Identity, Multipart}
    end.

main() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(router),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "127.0.0.1", 5555),

    start_worker("A"),
    start_worker("B"),

    timer:sleep(1000), %% wait workers to be established

    ok = chumak:send_multipart(Socket, [<<"A">>, <<"My message one">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"My message two">>]),

    ok = chumak:send_multipart(Socket, [<<"A">>, <<"EXIT">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"EXIT">>]),

    MessageA = receive
                   {recv, "A", MultipartA} ->
                       MultipartA
               end,

    MessageB = receive
                   {recv, "B", MultipartB} ->
                       MultipartB
               end,
    io:format("Received: ~p...~p\n", [MessageA, MessageB]).
