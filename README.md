myerl
=====

A MySQL/Erlang REST API skeleton built around :

* [poolboy][1]: used to implement a connexion pool
* [mysql-otp][2]: used as MySQL driver
* [elli][3]: used as web server
* [hackney][4]: used as HTTP client
* [jsx][5]: used to manipulate JSON

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

Test
----

Run tests with :

```sh
rebar3 eunit ct
```

Config
------

In `config/sys.config.src` file :

```
[{kernel, [{logger_level, info}]},
 {elli, [{min_acceptors, 20}, {port, 3000}]},
 {mysql,
  [{host, "db"},
   {user, "root"},
   {password, "${MYERL_DB_PASSWORD}"},
   {database, "book_shop"},
   {port, 3306}]},
 {poolboy, [{size, 5}, {max_overflow, 10}]}].
```

Run
---

To run the release :

```sh
MYERL_DB_PASSWORD=book_shop ./_build/prod/rel/myerl/bin/myerl foreground
```

Docker container :

```sh
MYERL_DB_PASSWORD=book_shop docker run -d myerl
```

### Docker Compose :

You can run multi-container Docker application with :

```sh
docker-compose up -d --scale myerl=2
```

Test it with :

```sh
curl -H Host:myerl.docker.localhost http://localhost/status 
{"logical_processors":4,"logical_processors_available":4,"logical_processors_online":4,"memory":{"total":45672256,"processes":5735112,"processes_used":5735112,"system":39937144,"atom":671969,"atom_used":645673,"binary":863080,"code":18150646,"ets":616496},"otp_release":[50,52],"process_count":114,"run_queue":0,"schedulers":4,"system_architecture":[120,56,54,95,54,52,45,112,99,45,108,105,110,117,120,45,109,117,115,108],"thread_pool_size":1,"threads":true,"uptime":123287,"version":[49,50,46,49,46,50]}
curl -H Host:myerl.docker.localhost -d '{"title":"The Hobbit","author":"J. R. R. Tolkien"}' http://localhost/books
{"id":1}
curl -H Host:myerl.docker.localhost http://localhost/books
{"list":[{"author":"J. R. R. Tolkien","id":1,"title":"The Hobbit"}],"total":1}
```

[1]: https://github.com/devinus/poolboy
[2]: https://github.com/mysql-otp/mysql-otp
[3]: https://github.com/elli-lib/elli
[4]: https://github.com/benoitc/hackney
[5]: https://github.com/talentdeficit/jsx
