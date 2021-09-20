FROM erlang:alpine
RUN mkdir /src
COPY . /src
WORKDIR /src
RUN rebar3 as prod release

FROM erlang:alpine
COPY --from=0 /src/_build/prod/rel/myerl /myerl
EXPOSE 3000
CMD ["/myerl/bin/myerl", "foreground"]
