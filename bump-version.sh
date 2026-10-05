#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cargo_manifest="$root_dir/rust_compiler/Cargo.toml"
cargo_lock="$root_dir/rust_compiler/Cargo.lock"
about_xml="$root_dir/ModData/About/About.xml"
plugin_cs="$root_dir/csharp_mod/Plugin.cs"
csproj="$root_dir/csharp_mod/stationeersSlang.csproj"

usage() {
    cat <<'EOF'
Usage: ./bump-version.sh [patch|minor|major|X.Y.Z] [--dry-run]

With no version argument, increments the patch version. Updates the Rust
manifest and lockfile, mod metadata, plugin constant, and C# project version.
EOF
}

fail() {
    printf 'Error: %s\n' "$1" >&2
    exit 1
}

requested_version=""
dry_run=false
for argument in "$@"; do
    case "$argument" in
        --dry-run)
            dry_run=true
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            [[ -z "$requested_version" ]] || fail "Provide only one version argument."
            requested_version="$argument"
            ;;
    esac
done

read_line_version() {
    local file="$1"
    local label="$2"
    local pattern="$3"
    local match_count

    match_count="$(grep -Ec "$pattern" "$file" || true)"
    [[ "$match_count" == 1 ]] || fail "Expected exactly one $label declaration in $file."
    sed -nE "s#${pattern}#\\1#p" "$file"
}

read_lock_version() {
    awk '
        /^\[\[package\]\]$/ { is_slang = 0 }
        /^name = "slang"$/ { is_slang = 1; next }
        is_slang && /^version = "/ {
            sub(/^version = "/, "")
            sub(/"$/, "")
            print
            found++
            is_slang = 0
        }
        END { if (found != 1) exit 1 }
    ' "$1"
}

cargo_pattern='^version = "([0-9]+\.[0-9]+\.[0-9]+)"$'
xml_extract_pattern='^[[:space:]]*<Version>([0-9]+\.[0-9]+\.[0-9]+)</Version>[[:space:]]*$'
plugin_extract_pattern='^[[:space:]]*public const string PluginVersion = "([0-9]+\.[0-9]+\.[0-9]+)";[[:space:]]*$'

cargo_version="$(read_line_version "$cargo_manifest" "Cargo package version" "$cargo_pattern")"
lock_version="$(read_lock_version "$cargo_lock")" || fail "Could not read the slang package version from $cargo_lock."
about_version="$(read_line_version "$about_xml" "metadata version" "$xml_extract_pattern")"
plugin_version="$(read_line_version "$plugin_cs" "plugin version" "$plugin_extract_pattern")"
csproj_version="$(read_line_version "$csproj" "C# project version" "$xml_extract_pattern")"

current_version="$cargo_version"
for entry in \
    "Cargo.lock:$lock_version" \
    "About.xml:$about_version" \
    "Plugin.cs:$plugin_version" \
    "stationeersSlang.csproj:$csproj_version"; do
    label="${entry%%:*}"
    version="${entry#*:}"
    [[ "$version" == "$current_version" ]] || \
        fail "Version mismatch: Cargo.toml has $current_version, but $label has $version."
done

requested_version="${requested_version:-patch}"
case "$requested_version" in
    patch|minor|major)
        IFS=. read -r major minor patch <<< "$current_version"
        case "$requested_version" in
            patch) patch=$((10#$patch + 1)) ;;
            minor) minor=$((10#$minor + 1)); patch=0 ;;
            major) major=$((10#$major + 1)); minor=0; patch=0 ;;
        esac
        next_version="$major.$minor.$patch"
        ;;
    *)
        [[ "$requested_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || \
            fail "Version must be patch, minor, major, or a numeric X.Y.Z version."
        next_version="$requested_version"
        ;;
esac

target_files=()
staged_files=()
cleanup() {
    local staged
    for staged in "${staged_files[@]}"; do
        [[ -z "$staged" ]] || rm -f -- "$staged"
    done
}
trap cleanup EXIT

stage_sed_update() {
    local file="$1"
    local label="$2"
    local substitution_pattern="$3"
    local replacement="$4"
    local extract_pattern="$5"
    local match_count temporary actual_version

    match_count="$(grep -Ec "$substitution_pattern" "$file" || true)"
    [[ "$match_count" == 1 ]] || fail "Expected exactly one $label to update in $file."

    temporary="$(mktemp "${file}.tmp.XXXXXX")"
    staged_files+=("$temporary")
    target_files+=("$file")
    sed -E "s#${substitution_pattern}#${replacement}#" "$file" > "$temporary"
    chmod --reference="$file" "$temporary"

    actual_version="$(read_line_version "$temporary" "$label" "$extract_pattern")"
    [[ "$actual_version" == "$next_version" ]] || \
        fail "Staged $label version is $actual_version, expected $next_version."
}

stage_cargo_lock_update() {
    local temporary actual_version

    temporary="$(mktemp "${cargo_lock}.tmp.XXXXXX")"
    staged_files+=("$temporary")
    target_files+=("$cargo_lock")
    awk -v version="$next_version" '
        /^\[\[package\]\]$/ { is_slang = 0 }
        /^name = "slang"$/ { is_slang = 1; print; next }
        is_slang && /^version = "/ {
            print "version = \"" version "\""
            found++
            is_slang = 0
            next
        }
        { print }
        END { if (found != 1) exit 1 }
    ' "$cargo_lock" > "$temporary" || fail "Could not stage the slang version in Cargo.lock."
    chmod --reference="$cargo_lock" "$temporary"

    actual_version="$(read_lock_version "$temporary")" || fail "Could not verify the staged Cargo.lock."
    [[ "$actual_version" == "$next_version" ]] || \
        fail "Staged Cargo.lock version is $actual_version, expected $next_version."
}

stage_sed_update \
    "$cargo_manifest" \
    "Cargo package version" \
    '^version = "[0-9]+\.[0-9]+\.[0-9]+"$' \
    "version = \"$next_version\"" \
    "$cargo_pattern"
stage_sed_update \
    "$about_xml" \
    "metadata version" \
    '(<Version>)[0-9]+\.[0-9]+\.[0-9]+(</Version>)' \
    "\\1$next_version\\2" \
    "$xml_extract_pattern"
stage_sed_update \
    "$plugin_cs" \
    "plugin version" \
    '(PluginVersion = ")[0-9]+\.[0-9]+\.[0-9]+(";)' \
    "\\1$next_version\\2" \
    "$plugin_extract_pattern"
stage_sed_update \
    "$csproj" \
    "C# project version" \
    '(<Version>)[0-9]+\.[0-9]+\.[0-9]+(</Version>)' \
    "\\1$next_version\\2" \
    "$xml_extract_pattern"
stage_cargo_lock_update

if [[ "$dry_run" == true ]]; then
    printf 'Would bump version %s -> %s in:\n' "$current_version" "$next_version"
    printf '  %s\n' "${target_files[@]}"
    exit 0
fi

for index in "${!target_files[@]}"; do
    mv -- "${staged_files[$index]}" "${target_files[$index]}"
done

printf 'Bumped version %s -> %s.\n' "$current_version" "$next_version"