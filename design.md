Gathering
---------

A self hosted website for announcing group events.

You can create events easily and share them with others.

## Requirements

- Create events
    - Name
    - Description
    - Date, time and duration
    - Location
    - Attendants
    - future:
        - Password protected?
        - invitation based?
        - Comments or no comments
- Subscription
    - Signup via mail
    - receive updates
        - on new event creation
        - on event edit
        - on event deletion
    - Say if you'll attend or not
- Users
    - Admin
        - Can create events
        - Can edit events
        - Can remove events
        - Can promote regular users to admins
        - Can demote admins to being regular users
        - Can block participants
        - Can receive mail on new comments
        - Do want a regular user can do
        - Future:
            - Can delete comments
    - Regular
        - Register via email
        - Mark attendance
        - Get updates or not
        - Future:
            - comment on event

## Design

### DB

```sql
create table users (
    user_id serial primary key,
    user_name text unique not null,
    user_email citext unique not null,
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
```

### Haskell

```hs

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  , userIsAdmin :: Bool
  , userWantsUpdates :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data Event = Event
  { eventId :: Int
  , eventName :: String
  , eventDesc :: String
  , eventLocation :: String
  , eventDateTime :: UTCTime
  , eventDuration :: DiffTime
  }
  deriving (Show, Eq, Ord)

data Attendant = Attendant
  { attendantUser :: User
  , attendantAttending :: Bool
  , attendantFollowsChanges :: Bool
  }
  deriving (Show, Read, Eq, Ord)

type Attendants = Map Event [Attendant]

```

### Event notifications

The content of the `events` table will be the one displayed in website.
Event notifications will be automatic processes that sends notifications
to those who ask. There are two kinds of notifications:

- Notifications about new events
- Reminders

Notifications of new events and event edits will be sent to all users that
want updates and did not mark 'will not attend the event'.

Reminders will be sent only to those who marked 'will attend the event'.

In the future we might extend the ability of each user to choose when to get events
and for what.

#### New events

New events will be upserted into two tables: the `events` table and the `new_events` table.

```sql
create table new_events (
    event_id integer references events(event_id),
    is_edit bool not null,
    modification_time timestamptz not null,
    primary key (event_id)
);
```

A worker will scan the table every 15 minutes and will send notifications
about all events that exists in this table more than 15 minutes
to all users that wants to get notifications about events.
After notifying about these events, the worker will delete them from the new_events table.

#### Reminders

A worker will scan the events list every 4 hours for events
that are going to occur between the next 22-28 hours and will notify those
who are going to attend the event.

### Email verification

New signups will be inserted into a `new_users` table upon registration

```sql
create table new_users (
    verification_rand int not null,
    user_name text unique not null,
    user_email citext unique not null,
    user_isadmin bool not null,
    user_wants_updates bool not null,
    user_password_hash bytea not null,
    expiration_date timestamptz not null
);

```

- The server will make sure not to create duplicate entries in new_users,
  but will delete expired entries and will insert new ones if needed.

- An email with the parameters `/verify-user/:key/:email` will be sent
  to the users' e-mail upon sign-up.

- When a user tries to access this page the server will verify
  the parameters in the `new_users` table and check the expiration date.

- If verified, the entry will be moved from `new_users` to `users`
  and will be deleted from `new_users`.

- If expired, the entry will be deleted from the `new_users` table.

### Sessions

Sessions will be saved in the `sessions` table and will be valid for 1 month.

```sql
create table sessions (
    user_id integer references users(user_id),
    valid_until timestamptz not null,
    primary key (user_id)
);
```

The server will add a session when the user logs in and will delete it when they sign out.

### Cleaners

A worker will run once a day and will clean expired sessions and new_users requests.


## Pages

### All

- Logo for group - link to main page
- Signup / Signin ~ guests
- Help
- New Event ~ Admins
- Settings
- Calendar
- Past Events

### Main

- Ascending list of future events

### Past Events

- Descending list of past events

### Calendar

- Monthly calendar
- Click to go to event page

### Event Page

- Name
- Date, time and duration - UTC time + your time
- Description
- Attend? (Yes/No) -> link to Signup/Signin if not logged in 
- Edit (For admin)

### Signin

- Email
- Password
- Forgot password?
- Signup

### Signup

- Name
- Email
- Password
- Verify
- receive updates?

=> Mail will be sent

### Mail verification

- Success!

### Settings - regular user

- Change password
- Change mail
- Change notification settings

### Control Panel - Admin

- Give other users admin rights

### Create event - Admin

- Name
- Description
- Time and date
- Location
- Duration
