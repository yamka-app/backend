# Erlang Order backend rewrite

## Configuration
Do the following things before running:
- Install Erlang/OTP 21.0
- Set up a Cassandra cluster (it's going to function as the main DB)
- Create a Cassandra keyspace using the commands in `db_structure.sql`
- Set up a GlusterFS cluster (it's going to function as the user file storage). Mount it to `/data/brick1/gv0` and create the `file_storage` dir.
- Get a TLS certificate, export the full chain and the private key to two separate `PEM` files
- Create `credentials.sh` in the project root dir:
```sh
export CAS_LOGIN=cassandra_user_name && export CAS_PASS=cassandra_user_password && export CERT_PATH=/path/to/cert_chain.pem && export KEY_PATH=/path/to/cert_key.pem
```

## Running
`./launch.sh`