Gathering is a self hosted website for announcing group events.

This project is also an excuse for me to learn about web development. 



> License: Apache License 2.0

## Deps

This package requires:

- `stack`
- `postgresql-9.5+`
- `sendmail`

## Setup

Here are the different `make` options:

- `setup`: Install GHC and deps for the package using stack
- `initdb`: Create the `gather` role and database and the relevant tables
- `build`: Build the haskell package
- `dev`: Continuously build on file changes - good for development
- `run`: Execute the server with the default settings
- `clean_all`: Delete the package build and the database
- `clean_db`: Drop the tables, the database and the 'gather' role

## Configuration

[example.cfg](example.cfg) is an example configuration file you can use as a reference.

Use `gather --config example.cfg` to run gather with the configuration taken from the `example.cfg` file.

## Additional commands

- `gather add-admin --user soupi`: Promote the user `soupi` to admin
- `gather rem-admin --user soupi`: Demote  the user `soupi` from being admin
- `gather del-user  --user soupi`: Delete  the user `soupi` from the system

