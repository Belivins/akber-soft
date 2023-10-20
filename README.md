# Тестовое задание
Сделать простое приложение на Erlang, в котором создается сокет-сервер (dialer) ZMQ, а в другом сокет-слушатель (subscriber).
При вводе созданной команды отправки (допустим send) отправлялся текст в слушатель.

## Пример работы
Сокет-сервер:

<code>1> c(server).\
{ok,server}\
2> server:start({127,0,0,1},8090).\
<0.92.0>\
Hello server\
ok3></code>

Сокет-клиент:

<code>1> c(client).\
{ok,client}\
2> Socket = client:start({127,0,0,1},8090).\
#Port<0.2>\
3> client:send(Socket, "Hello server").\
ok\
4> flush().\
Shell got {tcp,#Port<0.2>,<<"I get it - Hello server">>}\
ok\
5></code>
