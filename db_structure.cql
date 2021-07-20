-- change keyspace name
CREATE KEYSPACE yamkadb
	WITH REPLICATION = {
		'class': 'org.apache.cassandra.locator.SimpleStrategy',
		'replication_factor': '3'
	}
	AND DURABLE_WRITES = true;

/*
################################################
################### TYPES ######################
################################################
*/
CREATE TYPE yamkadb.message_section(
	type smallint,
	txt text,
	blob bigint
);


/*
################################################
################# FUNCTIONS ####################
################################################
*/

/*
################################################
################# AGGREGATES ###################
################################################
*/

/*
################################################
################### TABLES #####################
################################################
*/
CREATE TABLE yamkadb.agents (
	id bigint,
	name text,
	owner bigint,
	type smallint,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.blob_store (
	id bigint,
	length int,
	name text,
	pixel_size text,
	preview text,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.channels (
	dm boolean,
	group bigint,
	id bigint,
	lcid bigint,
	name text,
	perms map<bigint, blob>,
	voice boolean,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.dm_channels (
	channel bigint,
	users frozen<set<bigint>>,
	PRIMARY KEY(users)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.email_conf (
	code text,
	user bigint,
	PRIMARY KEY((user, code))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.groups (
	everyone_role bigint,
	icon bigint,
	id bigint,
	name text,
	owner bigint,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.invites (
	code text,
	group bigint,
	PRIMARY KEY(code)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.key_limiter (
	subject bigint,
	user bigint,
	PRIMARY KEY((subject, user))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 900 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.mentions (
	channel bigint,
	lcid bigint,
	msg bigint,
	user bigint,
	PRIMARY KEY((channel, user))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.message_states (
	encrypted blob,
	id bigint,
	msg_id bigint,
	sections list<frozen<message_section>>,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.messages (
	channel bigint,
	id bigint,
	lcid bigint,
	sender bigint,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.pkeys (
	id bigint,
	key blob,
	signature blob,
	type smallint,
	user bigint,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.poll_votes (
	option int,
	poll bigint,
	user bigint,
	PRIMARY KEY((poll, user))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.polls (
	id bigint,
	option_votes list<int>,
	options list<text>,
	total_votes int,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.roles (
	color int,
	group bigint,
	id bigint,
	name text,
	permissions blob,
	priority smallint,
	PRIMARY KEY(id, group)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.roles_by_user (
	group bigint,
	role bigint,
	user bigint,
	PRIMARY KEY((group, user), role)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.tokens (
	agent bigint,
	hash blob,
	permissions set<int>,
	PRIMARY KEY(hash)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;


CREATE INDEX tokens_agent_idx ON yamkadb.tokens(agent);


CREATE TABLE yamkadb.unread (
	channel bigint,
	lcid bigint,
	msg bigint,
	user bigint,
	PRIMARY KEY((channel, user))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.user_notes (
	note text,
	subject bigint,
	user bigint,
	PRIMARY KEY((user, subject))
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE TABLE yamkadb.users (
	ava_file bigint,
	badges set<int>,
	blocked set<bigint>,
	bot_owner bigint,
	email text,
	email_confirmed boolean,
	friends set<bigint>,
	groups set<bigint>,
	id bigint,
	mfa_secret blob,
	name text,
	password blob,
	pending_in set<bigint>,
	pending_out set<bigint>,
	public boolean,
	salt blob,
	status int,
	status_text text,
	tag int,
	PRIMARY KEY(id)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;


CREATE INDEX bot_owner_idx ON yamkadb.users(bot_owner);

CREATE INDEX bot_token_idx ON yamkadb.users(password);


CREATE TABLE yamkadb.users_by_role (
	role bigint,
	user bigint,
	PRIMARY KEY(role, user)
) WITH 	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;


/*
################################################
############# MATERIALIZED VIEWS ###############
################################################
*/
CREATE MATERIALIZED VIEW yamkadb.agents_by_user
AS SELECT
id, owner
FROM yamkadb.agents
WHERE id IS NOT NULL AND owner IS NOT NULL
PRIMARY KEY(owner, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.channels_by_group
AS SELECT
group, id
FROM yamkadb.channels
WHERE id IS NOT NULL AND group IS NOT NULL
PRIMARY KEY(group, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.invites_by_group
AS SELECT
code, group
FROM yamkadb.invites
WHERE code IS NOT NULL AND group IS NOT NULL
PRIMARY KEY(group, code)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.message_states_by_msg
AS SELECT
id, msg_id
FROM yamkadb.message_states
WHERE id IS NOT NULL AND msg_id IS NOT NULL
PRIMARY KEY(msg_id, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.messages_id_by_chan
AS SELECT
channel, id
FROM yamkadb.messages
WHERE id IS NOT NULL AND channel IS NOT NULL AND lcid IS NOT NULL AND sender IS NOT NULL
PRIMARY KEY(channel, id)
WITH
CLUSTERING ORDER BY (id DESC) AND 
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.messages_id_by_chan_rev
AS SELECT
channel, id
FROM yamkadb.messages
WHERE id IS NOT NULL AND channel IS NOT NULL AND lcid IS NOT NULL AND sender IS NOT NULL
PRIMARY KEY(channel, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.pkeys_by_user
AS SELECT
id, type, user
FROM yamkadb.pkeys
WHERE id IS NOT NULL AND key IS NOT NULL AND type IS NOT NULL AND user IS NOT NULL
PRIMARY KEY(user, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.roles_by_group
AS SELECT
group, id
FROM yamkadb.roles
WHERE id IS NOT NULL AND group IS NOT NULL
PRIMARY KEY(group, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.user_ids_by_name
AS SELECT
id, name, tag
FROM yamkadb.users
WHERE id IS NOT NULL AND name IS NOT NULL AND tag IS NOT NULL
PRIMARY KEY(name, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;

CREATE MATERIALIZED VIEW yamkadb.users_by_email
AS SELECT
email, id, mfa_secret, password, salt
FROM yamkadb.users
WHERE id IS NOT NULL AND salt IS NOT NULL AND email IS NOT NULL AND password IS NOT NULL
PRIMARY KEY(email, id)
WITH
	dclocal_read_repair_chance = 0.1 AND 
	gc_grace_seconds = 864000 AND 
	bloom_filter_fp_chance = 0.01 AND 
	caching = { 'keys': 'ALL', 'rows_per_partition': 'NONE' } AND 
	comment = '' AND 
	compaction = { 'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold': '32', 'min_threshold': '4' } AND 
	compression = { 'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor' } AND 
	default_time_to_live = 0 AND 
	speculative_retry = '99PERCENTILE' AND 
	min_index_interval = 128 AND 
	max_index_interval = 2048 AND 
	crc_check_chance = 1;
