#!/usr/bin/env bash
# Re-apply the user Space Cadet XKB keymap to the active X display.
#
# Kanata creates/recreates a uinput keyboard on restart; X often assigns that
# new device the default layout. This helper is intentionally more robust than a
# hard-coded DISPLAY/XAUTHORITY pair: display managers such as SDDM may place the
# real MIT-MAGIC-COOKIE file in /tmp, and user systemd services do not inherit a
# shell's graphical environment.
set -u

keymap="${1:-$HOME/.config/xkb/keymap/spacecadet.xkb}"
include_dir="${2:-$HOME/.config/xkb}"

if [ ! -r "$keymap" ]; then
    echo "reload-spacecadet-xkb: missing keymap: $keymap" >&2
    exit 1
fi

if ! command -v xkbcomp >/dev/null 2>&1; then
    echo "reload-spacecadet-xkb: xkbcomp not found" >&2
    exit 127
fi

try_reload() {
    display="$1"
    xauthority="${2:-}"
    [ -n "$display" ] || return 1

    if [ -n "$xauthority" ]; then
        [ -r "$xauthority" ] || return 1
        DISPLAY="$display" XAUTHORITY="$xauthority" \
            xkbcomp -I"$include_dir" -w 0 "$keymap" "$display" >/dev/null 2>&1
    else
        DISPLAY="$display" xkbcomp -I"$include_dir" -w 0 "$keymap" "$display" >/dev/null 2>&1
    fi
}

# First try the environment we were given, if any.
if try_reload "${DISPLAY:-}" "${XAUTHORITY:-}"; then
    exit 0
fi

# Then inspect this user's graphical processes for DISPLAY/XAUTHORITY pairs.
# Reading /proc/$pid/environ is allowed for our own processes and avoids
# hard-coding SDDM/GDM-specific auth paths.
seen_pairs=""
for envfile in /proc/[0-9]*/environ; do
    [ -r "$envfile" ] || continue
    env_text="$( { tr '\0' '\n' < "$envfile"; } 2>/dev/null || true)"
    display="$(printf '%s\n' "$env_text" | awk -F= '$1=="DISPLAY" {print substr($0, index($0,"=")+1); exit}')"
    [ -n "$display" ] || continue
    xauthority="$(printf '%s\n' "$env_text" | awk -F= '$1=="XAUTHORITY" {print substr($0, index($0,"=")+1); exit}')"
    pair="$display|$xauthority"
    case " $seen_pairs " in
        *" $pair "*) continue ;;
    esac
    seen_pairs="$seen_pairs $pair"
    if try_reload "$display" "$xauthority"; then
        exit 0
    fi
done

# Last-resort guesses for simple sessions.
for sock in /tmp/.X11-unix/X*; do
    [ -S "$sock" ] || continue
    display=":${sock##*X}"
    for xauthority in "${XAUTHORITY:-}" "$HOME/.Xauthority" /tmp/xauth_* /run/user/"$(id -u)"/gdm/Xauthority; do
        [ -n "$xauthority" ] || continue
        if try_reload "$display" "$xauthority"; then
            exit 0
        fi
    done
    if try_reload "$display" ""; then
        exit 0
    fi
done

echo "reload-spacecadet-xkb: could not apply $keymap to any accessible X display" >&2
exit 1
