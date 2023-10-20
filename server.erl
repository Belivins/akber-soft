-module(server).
-export([start/2, send/2]).
-define(print(TEXT), io:format(io:put_chars([TEXT, $\n]))).

%% Запуск сервера
start(IP,Port) ->
	{ok, Listen} = gen_tcp:listen(Port, [{active,false},binary, {ip, IP}]),
	_Pid = spawn_link(fun() -> % Запуск процесса получателя
		spawn(fun() -> acceptor(Listen) end),
		timer:sleep(infinity) 
	end).
	% {ok, Pid}.
	% Listen.

%% Функция получения сообщений
acceptor(ListenSocket) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> acceptor(ListenSocket) end), % Зацикливание процесса получения
	handle(AcceptSocket). % Запуск обработчика сообщений

%% Обработчик сообщений
handle(Socket) ->
	inet:setopts(Socket, [{active,once}]), % Включение активного режима
	receive 
		{tcp, Socket, Msg} -> 
			?print(Msg),
			send(Socket, "I get it - " ++ Msg),
			handle(Socket)		
	end.

%% Функция отправки сообщений	
send(Socket, Text) ->
	gen_tcp:send(Socket,Text).