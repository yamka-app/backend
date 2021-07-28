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

## Development and testing
You don't want to rebuild the whole project every time you've made a small change to the source tree, then restart all three main containers. Me too. To develop locally,
  - Add `127.0.0.1 elassandra` to `/etc/hosts`
  - Start the Elassandra and Gluster containers up
  - Run `launch.sh`