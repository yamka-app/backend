FROM erlang:22
RUN mkdir /orderbackend
WORKDIR /orderbackend
COPY ./ ./
RUN rebar3 release
EXPOSE 1746/tcp 1747/udp
EXPOSE 9042/tcp

CMD _build/default/rel/orderbackend/bin/orderbackend foreground