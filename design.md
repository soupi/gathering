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
    user_password_hash bytea not null,
    user_password_salt bytea not null,
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
