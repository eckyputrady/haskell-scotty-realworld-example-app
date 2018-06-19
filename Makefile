DBUSER=realworld_scotty_user
DBNAME=realworld_db
TEST_DBNAME=realworld_test

THIS_FILE := $(lastword $(MAKEFILE_LIST))

.DEFAULT_GOAL := help

db-up: ## Sets up the db, runs it as deamon
	docker-compose up -d
.PHONY: db-up

db-down: ## Shuts down the DB
	docker-compose down
.PHONY: db-down

dbconnect: ## Connect to the database
	PGPASSWORD=$(PG_PWD) psql -U $(DBUSER) -p 5432 -h localhost -d $(DBNAME)
.PHONY: dbconnect

run: ## Runs the app
	ENABLE_HTTPS=False DATABASE_URL="host=localhost dbname=$(DBNAME) user=$(DBUSER) password=$(PG_PWD) port=5432" stack exec realworld-exe
.PHONY: run

test: ## Runs the test
	ENABLE_HTTPS=False DATABASE_URL="host=localhost dbname=$(TEST_DBNAME) user=$(DBUSER) password=$(PG_PWD) port=5432" stack test
.PHONY: test

api-create-user: ## Create user via the API
	curl --header "Content-Type: application/json; charset=utf-8" \
	--request POST \
	--data '{"user":{"username":"Jacob","email":"jake@jake.jake","password":"jakejake"}}' \
	http://localhost:3000/api/users

api-log-in-user: ## Log in the user
	curl --header "Content-Type: application/json; charset=utf-8" \
	--request POST \
	--data '{"user":{"email":"jake@jake.jake","password":"jakejake"}}' \
	http://localhost:3000/api/users/login

api-get-user: ## Get a User
	curl --header "Content-Type: application/json; charset=utf-8; Authorization: Token $(token)" \
	--request GET \
	http://localhost:3000/api/user

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
