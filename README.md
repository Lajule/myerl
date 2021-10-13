myerl
=====

A MySQL/Erlang REST API skeleton.

Build
-----

    $ rebar3 compile

Docker Compose
--------------

    $ docker-compose up -d traefik mysql
    $ docker-compose up -d --build --scale myerl=2
    $ curl -H Host:myerl.docker.localhost http://127.0.0.1/books
