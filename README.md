# Erlang Yamka backend

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code (business logic) in the Erlang programming language
  - Database storage managed by Apache Cassandra
  - User-uploaded file storage managed by GlusterFS

## Running
The entire project is containerized. Execute three commands to run it on your own machine:
```
git clone https://github.com/yamka-app/backend.git
cd backend
docker-compose build
docker-compose up
```

Some Cassandra configuration is required:
  - Set up authentication: create a user named `yamkadb` with a custom password ([this](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/configuration/secureConfigNativeAuth.html) tutorial might be helpful)
  - Execute the commands in `db_structure.sql`

Three secrets are required:
  - `tls_fullchain` - full TLS certificate chain (`.pem`)
  - `tls_privkey` - TLS certificate private key (`.pem`)
  - `cassandra_password` - Cassandra password

:construction: Your server instance is going to redirect your voice clients to our server because the domain names are hard coded in (at the moment). We're working on that. _While_ we're working on that, feel free to change the return value of `server_name/0` in `src/tasty/tasty.erl`.

## Development and testing
You don't want to rebuild the whole project every time you've made a small change to the source tree, then restart all three main containers. Me too. To develop locally,
  - Add `127.0.0.1 cassandra` to `/etc/hosts`
  - Start the Cassandra and Gluster containers up
  - Run `launch.sh`

## 1.0 release checklist
  - [x] Learn Erlang
  - [x] Basic authentication
    - [ ] 2FA
  - [x] User account creation
    - [ ] E-Mail verification
  - [x] Protocol parsing
  - [x] File up-/downloading
  - [x] Account management
    - [x] Name, status
    - [x] E-Mail
    - [x] Profile picture
  - [x] User relations
    - [x] Friend requests
    - [ ] Blocking
  - [x] Chatting
    - [x] Direct messages
    - [x] Multiple message sections and types
      - [x] Text, files, code
      - [x] Replies
      - [ ] Group invites
      - [ ] User contacts
    - [x] Editing
      - [x] Edit history
    - [x] Deletion
  - [x] Groups
    - [x] Multiple channels
    - [ ] Roles and administration
    - [ ] Mentions
    - [ ] Access control
  - [x] Realtime communication
    - [x] Voice
      - [ ] "on the fly" voice server selection
    - [ ] Video
  - [ ] User walls
  - [ ] Make code ready for distribution
    - [ ] Use Mnesia instead of ets for "awareness" storage
    - [ ] Rewrite with Riak Core for true distribution
    - [ ] Fix `tasty:server_name/0` LMAO
  - [ ] Extensive testing and bugfixing