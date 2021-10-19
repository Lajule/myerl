myerl
=====

A MySQL/Erlang REST API skeleton.

Build
-----

To build a release use `rebar3` :

```sh
rebar3 as prod release
```

Docker image :

```sh
docker build -f docker/Dockerfile -t myerl .
```

Run
---

To run the release :

```sh
MYERL_DB_PASSWORD=book_shop ./_build/prod/rel/myerl/bin/myerl foreground
```

Docker image :

```sh
MYERL_DB_PASSWORD=book_shop docker rm -d --rm myerl
```

Docker Compose
--------------

```sh
docker-compose up -d --scale myerl=2
```

```sh
curl -H Host:myerl.docker.localhost http://127.0.0.1/books
```
