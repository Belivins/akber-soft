-module(client).
-export([start/2, send/2]).
-define(print(TEXT), io:format(io:put_chars([TEXT, $\n]))).

%% Запуск клиента
start(IP, Port) ->
	{ok, Socket} = gen_tcp:connect(IP, Port, [{active,true},binary]),
	Socket.

%% Функция отправки сообщений на сервер
send(Socket, Text) ->
	gen_tcp:send(Socket,Text).
