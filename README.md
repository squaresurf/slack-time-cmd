# Slack Channel Time Command

## Development

### Requirements

- Haskell Stack
- Docker
- Docker Compose
- Direnv
- PostgresSQL CLI (i.e. `psql`)

### Setup

- `cp .envrc-example .envrc`
- Edit .envrc
- `direnv allow`
- `docker-compose up -d db`
- `psql postgres://localhost/slack_time_cmd < schema.sql`

## Deployments

Deployments are using docker on heroku. See more here:
<https://devcenter.heroku.com/articles/container-registry-and-runtime>

### Example Deploy

- `docker-compose build slack-time-cmd`
- `heroku container:login`
- `docker push registry.heroku.com/slack-time-cmd/web`
- `heroku container:release web`
