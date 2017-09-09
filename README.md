# ![RealWorld Example App](logo.png)

> ### Haskell/Scotty codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld-example-apps) spec and API.

### [Demo](https://haskell-scotty-realworld.herokuapp.com/api/health)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

[![Build Status](https://travis-ci.org/eckyputrady/haskell-scotty-realworld-example-app.svg?branch=master)](https://travis-ci.org/eckyputrady/haskell-scotty-realworld-example-app)

This codebase was created to demonstrate a fully fledged fullstack application built with **Haskell/Scotty** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell/Scotty** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works

## Environment Variables

| Name | Desc | Default Value
|----|----|----|
| `DATABASE_URL` | PostgreSQL Database URL | `postgresql://localhost/realworld` |
| `ENABLE_HTTPS` | Server will run on HTTPS if True | `True`
| `PORT` | Port | `3000`
| `JWK_PATH` | Path to JWK signature file | `secrets/jwk.sig`
| `JWT_EXPIRATION_SECS` | How long until JWT expire (in secs) | `7200`

## Notable Dependencies

- `classy-prelude` - Better Prelude for Haskell
- `scotty` - Web ~~framework~~ library
- `postgresql-simple` - PostgreSQL library
- `aeson` - JSON serialization & deserialization
- `digestive-functors` & `digestive-functors-aeson` - Input parsing & validation
- `jose-jwt` - JWT encode & decode
- `slug` - Slug string builder
- `hspec` - Test framework

## Quick Repo Walkthrough

- `/postgresql` - Database migration scripts.
- `/secrets` - Default HTTPS & JWT configuration files for development. **Please replace with something else for production**.
- `/scripts` - Bash scripts to help development.
- `/app` - Haskell files for application entry point.
- `/test` - Haskell test files.
- `/src` - Main Haskell code.

# Getting started

## Installation

Install [Stack](https://docs.haskellstack.org/en/stable/README/).

Install [PostgreSQL](https://www.postgresql.org/).

Clone the repository and cd to repo

    git clone https://github.com/eckyputrady/haskell-scotty-realworld-example-app.git

    cd haskell-scotty-realworld-example-app

Install GHC

    stack setup

Install dependencies and build project

    stack build
    
Run project

    stack exec realworld

Alternatively, to watch file changes and start the server automatically

    ./scripts/dev.sh

Alternatively, to watch file changes and run the tests automatically

    stack test --file-watch --coverage

To setup deployment to Heroku, please configure this custom buildpack

    heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack

# Misc

Logo image - credits to @EricSimmon