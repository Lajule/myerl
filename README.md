myerl
=====

A MySQL/Erlang REST API skeleton built around :

* [poolboy][1] is used to implement a connexion pool.
* [mysql-otp][2] is used as MySQL drive.
* [elli][3] is used as web server.
* [hackney][4] is used as HTTP client.

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

Config
------

```
[
  {kernel, [
    {logger_level, info}
  ]},
  {elli, [
    {min_acceptors, 20},
    {port, 3000}
  ]},
  {mysql, [
    {host, "db"},
    {user, "root"},
    {database, "book_shop"},
    {port, 3306}
  ]},
  {poolboy, [
    {size, 5},
    {max_overflow, 10}
  ]}
].
```

Run
---

To run the release :

```sh
MYERL_DB_PASSWORD=book_shop ./_build/prod/rel/myerl/bin/myerl foreground
```

Docker image :

```sh
MYERL_DB_PASSWORD=book_shop docker rm -d myerl
```

Docker Compose
--------------

```sh
docker-compose up -d --scale myerl=2
```

```sh
curl -H Host:myerl.docker.localhost http://127.0.0.1/books
```

[1]: https://github.com/devinus/poolboy
[2]: https://github.com/mysql-otp/mysql-otp
[3]: https://github.com/elli-lib/elli
[4]: https://github.com/benoitc/hackney
