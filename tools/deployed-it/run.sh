#!/usr/bin/env bash
set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_dir=$(CDPATH= cd -- "$script_dir/../.." && pwd)
validate_only=false
if [[ ${1:-} == '--validate-only' ]]; then
	validate_only=true
	shift
fi
[[ $# -eq 0 ]] || { printf 'Usage: %s [--validate-only]\n' "$0" >&2; exit 2; }

fail() {
	printf 'deployed-it: %s\n' "$1" >&2
	exit 2
}

test_class=${SKYVE_DEPLOYED_IT_TEST:-}
[[ "$test_class" =~ ^([A-Za-z_\$][A-Za-z0-9_\$]*\.)*[A-Za-z_\$][A-Za-z0-9_\$]*IT$ ]] || \
	fail 'SKYVE_DEPLOYED_IT_TEST must be a Java class name ending in IT'

maven_command=(mvn -pl skyve-war -am -Pdeployed-it -DskipIntegrationTests=false -DskipUnitTests=true -Dfailsafe.failIfNoSpecifiedTests=false "-Dit.test=$test_class" verify)
if $validate_only; then
	printf 'mode=validate-only\n'
	printf 'test=%s\n' "$test_class"
	printf 'runtime=<not-required>\nconfig=<not-required>\nbase-url=<not-required>\n'
	if [[ -n ${SKYVE_DEPLOYED_IT_USERNAME:-} || -n ${SKYVE_DEPLOYED_IT_PASSWORD:-} ]]; then
		printf 'credentials=<supplied-redacted>\n'
	else
		printf 'credentials=<not-supplied>\n'
	fi
	printf 'command='
	printf '%q ' "${maven_command[@]}"
	printf '\nvalidation=passed\n'
	exit 0
fi

runtime_dir=${SKYVE_DEPLOYED_IT_RUNTIME_DIR:-}
config_dir=${SKYVE_DEPLOYED_IT_CONFIG_DIR:-}
base_url=${SKYVE_DEPLOYED_IT_BASE_URL:-}
[[ "$runtime_dir" = /* && -x "$runtime_dir/bin/standalone.sh" ]] || fail 'SKYVE_DEPLOYED_IT_RUNTIME_DIR must be an absolute verified WildFly directory'
[[ "$config_dir" = /* && -d "$config_dir" ]] || fail 'SKYVE_DEPLOYED_IT_CONFIG_DIR must be an absolute configuration directory'
[[ "$base_url" =~ ^https?://(127\.0\.0\.1|localhost)(:[0-9]{1,5})?/[^?#[:space:]]*/$ ]] || \
	fail 'SKYVE_DEPLOYED_IT_BASE_URL must be a loopback HTTP(S) application URL ending in /'
marker="$runtime_dir/.skyve-deployed-it-runtime"
pin_file="$script_dir/runtime.properties"
[[ -f "$marker" && -f "$pin_file" ]] || fail 'the runtime pin and prepare-runtime verification marker are required'

result_dir="$repo_dir/skyve-war/target/deployed-it/$test_class"
rm -rf -- "$result_dir"
mkdir -p "$result_dir/browser"
temporary_dir=$(mktemp -d "${TMPDIR:-/tmp}/skyve-deployed-it-run.XXXXXX")
server_base="$temporary_dir/standalone"
server_pid=
result=failed
overlay_checksum=unavailable

property() {
	awk -F= -v key="$1" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' "$pin_file"
}

checksum() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | awk '{print $1}'
	else
		shasum -a 256 "$1" | awk '{print $1}'
	fi
}

finish() {
	status=$?
	if [[ -n "$server_pid" ]]; then
		kill "$server_pid" 2>/dev/null || true
		wait "$server_pid" 2>/dev/null || true
	fi
	if [[ -f "$server_base/log/server.log" ]]; then
		cp "$server_base/log/server.log" "$result_dir/wildfly.log"
	fi
	java_version=$(java -version 2>&1 | head -1 || true)
	browser_version=$(google-chrome --version 2>/dev/null || chromium --version 2>/dev/null || \
		"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --version 2>/dev/null || true)
	driver_version=$(chromedriver --version 2>/dev/null || true)
	{
		printf '# Deployed Integration Test Result\n\n'
		printf -- '- Contract version: 1\n'
		printf -- '- Command: `mvn -pl skyve-war -am -Pdeployed-it -DskipIntegrationTests=false -DskipUnitTests=true -Dfailsafe.failIfNoSpecifiedTests=false -Dit.test=%s verify`\n' "$test_class"
		printf -- '- Commit: `%s`\n' "${GITHUB_SHA:-${SKYVE_DEPLOYED_IT_COMMIT:-unknown}}"
		printf -- '- Selected test: `%s`\n' "$test_class"
		printf -- '- WildFly version: `%s`\n' "$(property wildfly.version)"
		printf -- '- WildFly SHA-256: `%s`\n' "$(property wildfly.sha256)"
		printf -- '- Mojarra version: `%s`\n' "$(property mojarra.version)"
		printf -- '- Java: `%s`\n' "$java_version"
		printf -- '- Browser: `%s`\n' "${browser_version:-unavailable}"
		printf -- '- Driver: `%s`\n' "${driver_version:-managed-by-selenium}"
		printf -- '- Test-overlay SHA-256: `%s`\n' "$overlay_checksum"
		printf -- '- Base URL shape: `%s`\n' "$base_url"
		printf -- '- Result: `%s`\n' "$result"
	} > "$result_dir/results.md"
	rm -rf -- "$temporary_dir"
	exit "$status"
}
trap finish EXIT INT TERM

if curl --silent --show-error --max-time 2 --output /dev/null "$base_url" 2>/dev/null; then
	fail 'SKYVE_DEPLOYED_IT_BASE_URL already responds; use an unoccupied loopback port'
fi

cp -R "$runtime_dir/standalone" "$server_base"
cp "$config_dir"/*-ds.xml "$server_base/deployments/"

rm -rf -- "$repo_dir/skyve-war/target/deployed-it-webapp"
package_command=(mvn -pl skyve-war -am -Pdeployed-it -DskipIntegrationTests=true -DskipUnitTests=true package)
(cd "$repo_dir" && "${package_command[@]}")
overlay_war="$repo_dir/skyve-war/target/skyve-deployed-it.war"
overlay_directory="$repo_dir/skyve-war/target/deployed-it-webapp"
[[ -f "$overlay_war" && -d "$overlay_directory" ]] || fail 'classified deployed-it WAR and exploded overlay were not produced'
overlay_checksum=$(checksum "$overlay_war")
cp -R "$overlay_directory" "$server_base/deployments/skyve.war"
touch "$server_base/deployments/skyve.war.dodeploy"

"$runtime_dir/bin/standalone.sh" "-Djboss.server.base.dir=$server_base" \
		"-DPROPERTIES_FILE_PATH=$config_dir/skyve.json" > "$result_dir/wildfly-console.log" 2>&1 &
server_pid=$!
ready=false
for _attempt in $(seq 1 120); do
	if curl --fail --silent --show-error --output /dev/null "$base_url" 2>/dev/null; then
		ready=true
		break
	fi
	if ! kill -0 "$server_pid" 2>/dev/null; then
		fail 'WildFly stopped before the application became ready'
	fi
	sleep 1
done
$ready || fail 'application readiness timed out'

(cd "$repo_dir" && SKYVE_DEPLOYED_IT_BASE_URL="$base_url" "${maven_command[@]}")
result=passed
