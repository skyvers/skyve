#!/usr/bin/env bash
set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
pin_file="$script_dir/runtime.properties"
target_dir=${1:-${SKYVE_DEPLOYED_IT_RUNTIME_DIR:-}}

fail() {
	printf 'deployed-it runtime preparation: %s\n' "$1" >&2
	exit 1
}

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

[[ -f "$pin_file" ]] || fail 'runtime.properties is awaiting release-owner approval'
[[ "$target_dir" = /* ]] || fail 'target runtime directory must be an absolute path'

wildfly_version=$(property wildfly.version)
wildfly_url=$(property wildfly.url)
wildfly_sha256=$(property wildfly.sha256)
mojarra_version=$(property mojarra.version)
[[ -n "$wildfly_version" && -n "$wildfly_url" && "$wildfly_sha256" =~ ^[0-9a-fA-F]{64}$ && -n "$mojarra_version" ]] || \
	fail 'runtime.properties is incomplete or malformed'

marker="$target_dir/.skyve-deployed-it-runtime"
if [[ -f "$marker" ]] && grep -Fqx "wildfly.sha256=$wildfly_sha256" "$marker" && \
		grep -Fqx "mojarra.version=$mojarra_version" "$marker"; then
	printf 'Verified cached deployed-IT runtime at %s\n' "$target_dir"
	exit 0
fi
[[ ! -e "$target_dir" ]] || fail 'target runtime directory exists without the matching verification marker'

cache_dir=${SKYVE_DEPLOYED_IT_CACHE_DIR:-${TMPDIR:-/tmp}/skyve-deployed-it-cache}
mkdir -p "$cache_dir"
archive=${SKYVE_DEPLOYED_IT_RUNTIME_ARCHIVE:-"$cache_dir/wildfly-$wildfly_version.archive"}
if [[ ! -f "$archive" ]]; then
	command -v curl >/dev/null 2>&1 || fail 'curl is required to download the approved runtime'
	curl --fail --location --silent --show-error --output "$archive.part" "$wildfly_url"
	mv "$archive.part" "$archive"
fi

actual_sha256=$(checksum "$archive")
[[ "$actual_sha256" == "$wildfly_sha256" ]] || fail 'runtime archive SHA-256 does not match runtime.properties'

extract_root=$(mktemp -d "${TMPDIR:-/tmp}/skyve-deployed-it-runtime.XXXXXX")
cleanup() {
	rm -rf -- "$extract_root"
}
trap cleanup EXIT
case "$wildfly_url" in
	*.zip)
		unzip -q "$archive" -d "$extract_root"
		;;
	*.tar.gz|*.tgz)
		tar -xzf "$archive" -C "$extract_root"
		;;
	*)
		fail 'approved runtime URL must end in .zip, .tar.gz or .tgz'
		;;
esac

runtime_root=$(find "$extract_root" -mindepth 1 -maxdepth 1 -type d -print -quit)
[[ -n "$runtime_root" && -x "$runtime_root/bin/standalone.sh" ]] || fail 'archive does not contain a WildFly standalone runtime'
mojarra_jar=$(find "$runtime_root/modules" -type f \( -name 'jsf-impl*.jar' -o -name 'jakarta.faces*.jar' \) -print -quit)
[[ -n "$mojarra_jar" ]] || fail 'bundled Mojarra implementation jar was not found'
manifest=$(unzip -p "$mojarra_jar" META-INF/MANIFEST.MF)
printf '%s\n' "$manifest" | grep -Eq "(^|[[:space:]])(Implementation-Version|Bundle-Version):[[:space:]]*$mojarra_version([[:space:]]|$)" || \
	fail 'bundled Mojarra version does not match runtime.properties'

mkdir -p "$(dirname -- "$target_dir")"
mv "$runtime_root" "$target_dir"
{
	printf 'wildfly.version=%s\n' "$wildfly_version"
	printf 'wildfly.sha256=%s\n' "$wildfly_sha256"
	printf 'mojarra.version=%s\n' "$mojarra_version"
} > "$marker"
printf 'Prepared and verified WildFly %s at %s\n' "$wildfly_version" "$target_dir"
