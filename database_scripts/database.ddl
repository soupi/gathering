create table users (
    user_id serial primary key,
    user_name text unique not null,
    user_email text unique not null,
    user_isadmin bool not null,
    user_wants_updates bool not null,
    user_password_hash bytea not null
);

create table events (
    event_id serial primary key,
    event_name text not null,
    event_description text not null,
    event_location text not null,
    event_datetime timestamptz not null,
    event_duration interval not null
);

create table attendants (
    event_id integer references events(event_id),
    user_id integer references users(user_id),
    attending bool not null,
    follow_changes bool not null,
    primary key (event_id, user_id)
);

create table sessions (
    user_id integer references users(user_id),
    valid_until timestamptz not null,
    primary key (user_id)
);