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
