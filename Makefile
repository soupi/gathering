.PHONY: setup

setup:
	stack setup

.PHONY: initdb

initdb:
	psql -U postgres -f database_scripts/gather.ddl; psql -U gather -f database_scripts/database.ddl

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack build --fast --file-watch


.PHONY: run

run:
	stack exec gather

.PHONY: clean_all

clean_all:
	stack clean; psql -U gather -f database_scripts/delete_database.ddl; psql -U postgres -f database_scripts/delete_gather.ddl

.PHONY: clean_db

clean_db:
	psql -U gather -f database_scripts/delete_database.ddl; psql -U postgres -f database_scripts/delete_gather.ddl

