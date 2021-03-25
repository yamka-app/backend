# Erlang Order backend rewrite

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code (business logic) in the Erlang programming language
  - Database storage managed by Apache Cassandra (thinking about getting rid of it and using Mnesia instead)
  - User-uploaded file storage managed by GlusterFS

## Running
The entire project is containerized. Execute three commands to run it on your own machine:
```
git clone https://github.com/ordermsg/backend-erl.git
cd backend
docker-compose build
docker-compose up
```

Some Cassandra configuration is required:
  - Set up authentication: create a user named `orderdb` with a custom password
  - Execute the commands in `db_structure.sql`

Three secrets are required:
  - `tls_fullchain` - full TLS certificate chain (`.pem`)
  - `tls_privkey` - TLS certificate private key (`.pem`)
  - `cassandra_password` - Cassandra password

:construction: Your server instance is going to redirect your voice clients to our server because the domain names are hard coded in (at the moment). We're working on that. _While_ we're working on that, feel free to change the return value of `server_name/0` in `src/tasty/tasty.erl`.