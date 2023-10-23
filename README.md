# Тестовое задание
Сделать простое приложение на Erlang, в котором создается сокет-сервер (dialer) ZMQ, а в другом сокет-слушатель (subscriber).
При вводе созданной команды отправки (допустим send) отправлялся текст в слушатель.

## Пример работы
Сокет-сервер:

<code> rebar3 shell
1> c("examples/server").
{ok,server}
2> Sock = server:main().
<0.197.0>
3> server:send(Sock, "Hello client").
ok
4></code>

Subscriber:

<code>rebar3 shell
1> c("examples/subscriber").
{ok,subscriber}
2> subscriber:main().
Binding OK with Pid: <0.197.0>
Received <<"Hello client">>
</code>
