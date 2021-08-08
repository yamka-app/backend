# Erlang Yamka backend :tada:

![Top language](https://img.shields.io/github/languages/top/yamka-app/backend)
![License](https://img.shields.io/github/license/yamka-app/backend)
![Platforms](https://img.shields.io/badge/platform-linux%20%7C%20windows%20%7C%20macos%20%7C%20freebsd-blueviolet)
![Last commit](https://img.shields.io/github/last-commit/yamka-app/backend)
![Status](https://img.shields.io/uptimerobot/status/m788461709-79464d516b1ef3af81a20454)
![Uptime (7 days)](https://img.shields.io/uptimerobot/ratio/7/m788461709-79464d516b1ef3af81a20454)
![Maintainer](https://img.shields.io/badge/maintainer-portasynthinca3-ff69b4)

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code (business logic) in the Erlang programming language
  - Database storage and search managed by Elassandra (a merge of Apache Cassandra and Elasticsearch)
  - User-uploaded file storage managed by GlusterFS

## Running
The entire project is containerized. Execute three commands to run it on your own machine:
```
git clone https://github.com/yamka-app/backend.git
cd backend
docker-compose build
docker-compose up
```

Some Elassandra configuration is required:
  - Set up authentication: create a user named `yamkadb` with a custom password ([this](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/configuration/secureConfigNativeAuth.html) tutorial might be helpful)
  - Execute the commands in `db_structure.cql`

Three secrets are required:
  - `tls_fullchain` - full TLS certificate chain (`.pem`)
  - `tls_privkey` - TLS certificate private key (`.pem`)
  - `cassandra_password` - Elassandra password

:construction: Your server instance is going to redirect your voice clients to our server because the domain names are hard coded in (at the moment). We're working on that. _While_ we're working on that, feel free to change the return value of `server_name/0` in `src/tasty/tasty.erl`.

## Control
The `admin` module provides some functions you can call from the shell:
  - `admin:powerup/0` starts this instance, begins accepting Sweet connections and doing other stuff
  - `admin:powerdown/0` stops this instance, stops accepting Sweet connections, etc.
  - `admin:seed_nodes/1` with a `list(atom())` as its argument connects to other nodes in the cluster
  - `stats:stats/0` prints some stats

## Development and testing
`sync` is declared as a rebar dependency for this project, so you should be able to just edit your files, save the changes and watch your code get reloaded in the shell after you have started the project with `rebar3 shell`. You can also run `rebar3 check` to run xref and dialyzer.