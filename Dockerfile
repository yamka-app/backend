FROM erlang:22
RUN mkdir /yamkabackend
WORKDIR /yamkabackend
COPY ./ ./
RUN rebar3 release
EXPOSE 1746/tcp 1747/udp
EXPOSE 9042/tcp

CMD _build/default/rel/yamkabackend/bin/yamkabackend foreground