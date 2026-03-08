SET SESSION wait_timeout = 28800;
SET SESSION interactive_timeout = 28800;

CREATE TABLE demo_data AS
SELECT * FROM inagural_token WHERE party = 'democratic'
UNION ALL
SELECT * FROM union_token WHERE party = 'democratic'
UNION ALL
SELECT * FROM weekly_token WHERE party = 'democratic'
UNION ALL
SELECT * FROM spoken_token WHERE party = 'democratic';

CREATE TABLE repu_data AS
select * from inagural_token where party = "republican"
UNION ALL
select * from union_token where party = "republican"
UNION ALL
select * from weekly_token where party = "republican"
UNION ALL
select * from spoken_token where party = "republican";

SET SQL_SAFE_UPDATES = 0;

ALTER TABLE union_address 
ADD COLUMN id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
ADD COLUMN type VARCHAR(3) DEFAULT 'U';

ALTER TABLE weekly_address 
ADD COLUMN id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
ADD COLUMN type VARCHAR(3) DEFAULT 'W';

ALTER TABLE spoken_address 
ADD COLUMN id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
ADD COLUMN type VARCHAR(3) DEFAULT 'S';

ALTER TABLE inagural_address
ADD COLUMN type VARCHAR(3) DEFAULT 'I';

UPDATE union_token t
LEFT JOIN union_address a ON t.doc_id = a.id
SET t.name = a.name,
	t.year = a.year,
	t.party = a.party;

UPDATE weekly_token t
LEFT JOIN weekly_address a ON t.doc_id = a.id
SET t.name = a.name,
	t.year = a.year,
	t.party = a.party;
    
UPDATE spoken_token t
LEFT JOIN spoken_address a ON t.doc_id = a.id
SET t.name = a.name,
	t.year = a.year,
	t.party = a.party;
    
SET SQL_SAFE_UPDATES = 1;

alter table president_party_data ADD primary key (name);

alter table ttr_result
ADD constraint fk_ttr
foreign key (name) references president_party_data(name);

alter table tf_idf_result
ADD constraint fk_tf_idf
foreign key (name) references president_party_data(name);
----
alter table sentiment_score_result
ADD constraint fk_sentiment
foreign key (name) references president_party_data(name);

alter table sentiment_count_result
ADD constraint fk_sentiment_count
foreign key (name) references president_party_data(name);

alter table pronoun_result
ADD constraint fk_prnoun
foreign key (name) references president_party_data(name);

alter table sentiment_count_result
ADD constraint fk_sentiment_count
foreign key (name) references president_party_data(name);

alter table pos_ratio_result
ADD constraint fk_pos
foreign key (name) references president_party_data(name);

alter table log_odd_ratio_result
ADD constraint fk_log_odd
foreign key (name) references president_party_data(name);

alter table emotion_result
ADD constraint fk_emotion
foreign key (name) references president_party_data(name);

alter table depth_result
ADD constraint fk_depth
foreign key (name) references president_party_data(name);

alter table count_result
ADD constraint fk_count
foreign key (name) references president_party_data(name);

alter table avg_result
ADD constraint fk_avg
foreign key (name) references president_party_data(name);

alter table lda
ADD constraint fk_lda
foreign key (name) references president_party_data(name);

alter table lda_topic
ADD constraint fk_lda_topic
foreign key (name) references president_party_data(name);

alter table avg_result
ADD constraint fk_avg
foreign key (name) references president_party_data(name);

alter table pair_node
ADD COLUMN id_number INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
add constraint fk_pair_node
foreign key (name) references president_party_data(name);

alter table pair_edge
ADD constraint fk_pair_edge
foreign key (Source) references pair_node(id_number);

alter table cor_node
ADD COLUMN id_number INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
add constraint fk_cor_node
foreign key (name) references president_party_data(name);

alter table cor_edge
ADD constraint fk_cor_edge
foreign key (Source) references cor_node(id_number);

alter table bigram_node
ADD COLUMN id_number INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
add constraint fk_bigram_node
foreign key (name) references president_party_data(name);

alter table bigram_edge
ADD constraint fk_bigram_edge
foreign key (Source) references bigram_node(id_number);

select * from stm where name is NULL;

alter table stm
ADD constraint fk_stm
foreign key (name) references president_party_data(name);

alter table stm_party
ADD constraint fk_stm_party
foreign key (name) references president_party_data(name);

alter table stm_topic
ADD constraint fk_stm_topic
foreign key (name) references president_party_data(name);

alter table stm_topic_party
ADD constraint fk_stm_topic_party
foreign key (name) references president_party_data(name);

