#!/usr/bin/env bash
set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
target_dir=${1:-${SKYVE_DEPLOYED_IT_CONFIG_DIR:-}}
content_dir=${SKYVE_DEPLOYED_IT_CONTENT_DIR:-}
database_path=${SKYVE_DEPLOYED_IT_DATABASE_PATH:-}
username=${SKYVE_DEPLOYED_IT_USERNAME:-}
password=${SKYVE_DEPLOYED_IT_PASSWORD:-}
customer=${SKYVE_DEPLOYED_IT_CUSTOMER:-bizhub}
base_url=${SKYVE_DEPLOYED_IT_BASE_URL:-http://127.0.0.1:8080/skyve/}

fail() {
	printf 'deployed-it H2 configuration: %s\n' "$1" >&2
	exit 1
}

[[ "$target_dir" = /* ]] || fail 'configuration target must be an absolute path'
[[ "$content_dir" = /* ]] || fail 'SKYVE_DEPLOYED_IT_CONTENT_DIR must be an absolute path'
[[ "$database_path" = /* ]] || fail 'SKYVE_DEPLOYED_IT_DATABASE_PATH must be an absolute path'
[[ "$content_dir" =~ ^/[A-Za-z0-9_./-]+$ && "$database_path" =~ ^/[A-Za-z0-9_./-]+$ ]] || \
	fail 'content and database paths may contain only letters, numbers, dot, underscore, slash and hyphen'
[[ "$username" =~ ^[A-Za-z][A-Za-z0-9_.-]{2,63}$ ]] || fail 'username has an invalid shape'
[[ "$customer" =~ ^[A-Za-z][A-Za-z0-9_.-]{0,63}$ ]] || fail 'customer has an invalid shape'
[[ "$password" =~ ^[A-Za-z0-9_-]{16,128}$ ]] || fail 'password must be a generated or protected 16-128 character value'
[[ "$base_url" =~ ^https?://(127\.0\.0\.1|localhost)(:[0-9]{1,5})?/skyve/$ ]] || \
	fail 'SKYVE_DEPLOYED_IT_BASE_URL must be a loopback Skyve application URL ending in /skyve/'
[[ ! -e "$target_dir" ]] || fail 'configuration target already exists'

server_url=${base_url%/skyve/}
mkdir -p "$target_dir" "$content_dir" "$(dirname -- "$database_path")"
sed -e "s|@CONTENT_DIR@|$content_dir/|g" \
	-e "s|@DATABASE_PATH@|$database_path|g" \
	-e "s|@CUSTOMER@|$customer|g" \
	-e "s|@USERNAME@|$username|g" \
	-e "s|@PASSWORD@|$password|g" \
	-e "s|@SERVER_URL@|$server_url|g" \
	"$script_dir/config/skyve.json.template" > "$target_dir/skyve.json"
sed -e "s|@DATABASE_PATH@|$database_path|g" \
	-e "s|@DATABASE_USERNAME@|$username|g" \
	-e "s|@DATABASE_PASSWORD@|$password|g" \
	"$script_dir/config/skyve-h2-ds.xml.template" > "$target_dir/skyve-ds.xml"
chmod 600 "$target_dir/skyve.json" "$target_dir/skyve-ds.xml"
printf 'Prepared isolated file-backed H2 configuration at %s\n' "$target_dir"
