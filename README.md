# Erlang Order backend

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code (business logic) in the Erlang programming language
  - Database storage managed by Apache Cassandra (thinking about getting rid of it and using Mnesia instead)
  - User-uploaded file storage managed by GlusterFS

## Running
The entire project is containerized. Execute three commands to run it on your own machine:
```
git clone https://github.com/ordermsg/backend.git
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
  - [x] Chatting
    - [x] Direct messages
    - [x] Multiple message sections and types
      - [x] Text, files, code
      - [x] Replies
      - [ ] Group invites
      - [ ] User contacts
  - [x] Groups
    - [x] Multiple channels
    - [ ] Roles and administration
    - [ ] Mentions
    - [ ] Access control
  - [x] Realtime communication
    - [x] Voice
    - [ ] Video
  - [ ] Drop Cassandra in favor of Mnesia
  - [ ] Make code ready for distribution
    - [ ] Use Mnesia instead of ets for "awareness" storage
    - [ ] Fix `tasty:server_name/0` LMAO
  - [ ] Extensive testing and bugfixing