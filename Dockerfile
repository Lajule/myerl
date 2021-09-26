# Build stage 0
FROM erlang:alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . /buildroot

# And build the release
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/_build/prod/rel/myerl /myerl

# Expose relevant ports
EXPOSE 3000

CMD ["/myerl/bin/myerl", "foreground"]
