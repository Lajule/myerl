myerl
=====

A MySQL/Erlang REST API skeleton built around :

* [poolboy][1]: used to implement a connexion pool
* [mysql-otp][2]: used as MySQL driver
* [elli][3]: used as web server
* [hackney][4]: used as HTTP client

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

In `config/sys.config` file :

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
curl -H Host:myerl.docker.localhost -d '{"title":"The Hobbit","author":"J. R. R. Tolkien"}' http://localhost/books
Created
curl -H Host:myerl.docker.localhost http://localhost/books
{"list":[{"author":"J. R. R. Tolkien","id":1,"title":"The Hobbit"}],"total":1}
```

[1]: https://github.com/devinus/poolboy
[2]: https://github.com/mysql-otp/mysql-otp
[3]: https://github.com/elli-lib/elli
[4]: https://github.com/benoitc/hackney
