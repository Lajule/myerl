# Build stage 0
FROM erlang:alpine

# Copy application
COPY . /app

# Set working directory
WORKDIR /app

# Build the release
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /app/_build/prod/rel/myerl /myerl

# Expose relevant ports
EXPOSE 3000

CMD ["/myerl/bin/myerl", "foreground"]
