BEGIN transaction;

create extension IF NOT EXISTS  citext;
create extension IF NOT EXISTS pgcrypto;

CREATE SCHEMA rusrom;


create table rusrom.users (
  user_id bigint primary key not null,
  username text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);

create table rusrom.messages (
  id bigserial primary key not null,
  user_id bigint not null,
  text text not null,
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES rusrom.users(user_id)
);

CREATE TABLE rusrom.word_rom (
	id int primary key NOT NULL,
	word_rom text NOT NULL,
	word_eng text
);

CREATE INDEX IX_word_rom ON rusrom.word_rom (word_rom);
CREATE INDEX IX_word_eng ON rusrom.word_rom (word_eng);


CREATE TABLE rusrom.word_rus (
	id int primary key NOT NULL,
	word_rus text NOT NULL
);
CREATE INDEX IX_word_rus ON rusrom.word_rus (word_rus);

CREATE TABLE rusrom.translations (
  idWord INT NOT NULL,
  idTranslation INT NOT NULL,
  CONSTRAINT IdWord_IdTranslation UNIQUE (idWord,idTranslation)
);
CREATE INDEX IX_translations ON rusrom.translations (idWord);

END TRANSACTION;