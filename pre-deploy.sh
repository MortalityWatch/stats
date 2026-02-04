#!/usr/bin/env sh

# Enable CORS plugin for this app (plugin is installed by deploy.sh from config.json)
SSH_TARGET="${SSH_ALIAS:-co2}"
ssh "$SSH_TARGET" "dokku nginx-cors:enable --no-restart ${APP_NAME}" 2>/dev/null || echo "nginx-cors not available, skipping"
