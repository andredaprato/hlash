drop table if exists Users cascade;
drop table if exists Questions cascade;
drop table if exists Default_Questions cascade;


create table Users (
       name text primary key,
       pass text 
);

create table Questions (
       id serial primary key,
       body text,
       user__name text references Users(name)
);
create table Default_Questions (
  question_id serial primary key,
  question_body text 
);
