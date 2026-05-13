#!/usr/bin/env python3
"""Render Kanata keymaps with keymap-drawer.

This small converter reads a Kanata config, optionally reads an XKB snippet that
maps Kanata `arbitrary-code` outputs to keysyms, emits keymap-drawer YAML, and
can call `keymap draw` to produce an SVG.

It intentionally handles the Kanata subset used by these Kinesis configs:
`defsrc`, `deflayer`, `defalias` tap-holds/arbitrary-code, and `defchordsv2`.
"""

from __future__ import annotations

import argparse
import html
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any

import yaml


SPACE_CADET_ALIAS_LABELS = {
    "gr": "greek",
    "top": "top",
}


# Display modifiers by their semantic XKB role instead of Kanata's left/right
# key names. See config/.config/xkb/symbols/spacecadet for the matching XKB
# modifier map: LALT=Meta, LWIN=Super, RALT=Alt, RWIN=Hyper.
TOKEN_LABELS = {
    "bspc": "bspc",
    "ent": "ent",
    "esc": "esc",
    "lctl": "control",
    "rctl": "control",
    "lalt": "meta",
    "ralt": "alt",
    "lmet": "super",
    "rmet": "hyper",
    "lshift": "shift",
    "rshift": "shift",
    "grv": "grv",
    "lrld": "lrld",
    "spc": "spc",
}


MODIFIER_PALETTE = {
    "shift": ("mod-shift", "#eeeeee", "#8c8c8c"),
    "control": ("mod-control", "#fff3bf", "#d9a300"),
    "meta": ("mod-meta", "#ffe2e2", "#e03131"),
    "super": ("mod-super", "#d8fbff", "#00a9c8"),
    "alt": ("mod-alt", "#dff0ff", "#6aa8df"),
    "hyper": ("mod-hyper", "#ffd6ff", "#c026d3"),
    "greek": ("mod-greek", "#e4f8e7", "#2f9e44"),
    "top": ("mod-top", "#ffe8cc", "#f08c00"),
}

MODIFIER_TYPES = {name: spec[0] for name, spec in MODIFIER_PALETTE.items()}


MODIFIER_SVG_STYLE = "\n".join(
    f"rect.{css_class} {{ fill: {fill}; stroke: {stroke}; }}"
    for css_class, fill, stroke in MODIFIER_PALETTE.values()
) + """
text.greek-legend { fill: #1b7f33; font-weight: 700; }
text.top-legend { fill: #b35f00; font-weight: 700; }
""".rstrip()


NUMBER_ROW_GREEK_LEGENDS = {
    "=": "∅", "1": "†", "2": "‡", "3": "▿", "4": "¢", "5": "○", "6": "▯",
    "7": "÷", "8": "×", "9": "¶", "0": "○", "-": "≈",
    "[": "⟦", "]": "⟧", ";": "¨", "'": "·", "\\": "‖", ",": "«", ".": "»", "/": "∫",
}


KEYSYM_DISPLAY = {
    "Greek_alpha": "α", "Greek_beta": "β", "Greek_chi": "χ", "Greek_delta": "δ",
    "Greek_epsilon": "ε", "Greek_eta": "η", "Greek_finalsmallsigma": "ς",
    "Greek_gamma": "γ", "Greek_iota": "ι", "Greek_kappa": "κ", "Greek_lambda": "λ",
    "Greek_xi": "ξ", "Greek_zeta": "ζ",
    "Greek_mu": "μ", "Greek_nu": "ν", "Greek_omega": "ω", "Greek_omicron": "ο",
    "Greek_phi": "φ", "Greek_pi": "π", "Greek_psi": "ψ", "Greek_rho": "ρ",
    "Greek_sigma": "σ", "Greek_tau": "τ", "Greek_theta": "θ", "Greek_upsilon": "υ",
    "U03D1": "ϑ",
    "upcaret": "∧", "downcaret": "∨", "downshoe": "∪", "upshoe": "∩",
    "leftshoe": "⊂", "rightshoe": "⊃", "U2200": "∀", "infinity": "∞",
    "U2203": "∃", "partialderivative": "∂", "uptack": "⊥", "downtack": "⊤",
    "righttack": "⊢", "lefttack": "⊣", "uparrow": "↑", "downarrow": "↓",
    "leftarrow": "←", "rightarrow": "→", "U2194": "↔", "downstile": "⌊",
    "upstile": "⌈", "similarequal": "≃", "identical": "≡", "lessthanequal": "≤",
    "greaterthanequal": "≥", "guillemotleft": "«", "guillemotright": "»",
    "guillemetleft": "«", "guillemetright": "»", "integral": "∫",
    "diaeresis": "¨", "periodcentered": "·", "U27E6": "⟦", "U27E7": "⟧",
    "U2016": "‖",
}


class ConversionError(RuntimeError):
    pass


def strip_block_comments(text: str) -> str:
    return re.sub(r"#\|.*?\|#", "", text, flags=re.S)


def strip_line_comment(line: str) -> str:
    # Kanata comments in these files use `;;`. A lone `;` is also a key name.
    return line.split(";;", 1)[0]


def tokenize(text: str) -> list[str]:
    text = strip_block_comments(text)
    text = "\n".join(strip_line_comment(line) for line in text.splitlines())
    return re.findall(r"\(|\)|[^\s()]+", text)


def parse_sexps(text: str) -> list[Any]:
    tokens = tokenize(text)
    pos = 0

    def parse_one() -> Any:
        nonlocal pos
        if pos >= len(tokens):
            raise ConversionError("unexpected end of tokens")
        tok = tokens[pos]
        pos += 1
        if tok == "(":
            values = []
            while pos < len(tokens) and tokens[pos] != ")":
                values.append(parse_one())
            if pos >= len(tokens):
                raise ConversionError("unclosed list in Kanata config")
            pos += 1
            return values
        if tok == ")":
            raise ConversionError("unexpected ')' in Kanata config")
        return tok

    forms = []
    while pos < len(tokens):
        forms.append(parse_one())
    return forms


def top_forms(text: str, name: str) -> list[list[Any]]:
    return [f for f in parse_sexps(text) if isinstance(f, list) and f and f[0] == name]


def form_by_name(text: str, form_name: str, item_name: str | None = None) -> list[Any]:
    for form in top_forms(text, form_name):
        if item_name is None or (len(form) > 1 and form[1] == item_name):
            return form
    raise ConversionError(f"could not find ({form_name} {item_name or ''})")


def rows_from_form(form: list[Any], payload_start: int) -> list[list[str]]:
    rows: list[list[str]] = []
    current: list[str] = []
    for item in form[payload_start:]:
        if isinstance(item, list):
            # deflayer/defsrc should not contain nested lists in the key matrix.
            if current:
                rows.append(current)
                current = []
            continue
        current.append(item)
    if current:
        rows.append(current)
    return rows


def rows_from_raw_block(text: str, form_name: str, item_name: str | None = None) -> list[list[str]]:
    """Return visual rows from a simple matrix-like top-level form.

    S-expression parsing loses row boundaries, so this line-oriented extractor is
    used for `defsrc` and `deflayer` matrices.
    """
    cleaned = strip_block_comments(text)
    if item_name:
        header_re = re.compile(r"^\s*\(" + re.escape(form_name) + r"\s+" + re.escape(item_name) + r"\b")
    else:
        header_re = re.compile(r"^\s*\(" + re.escape(form_name) + r"(?:\s+[^\s)]+)?\b")
    lines = cleaned.splitlines()
    in_form = False
    rows: list[list[str]] = []
    depth = 0
    for line in lines:
        no_comment = strip_line_comment(line)
        if not in_form:
            if not header_re.search(no_comment):
                continue
            in_form = True
            depth = no_comment.count("(") - no_comment.count(")")
            rest = no_comment[no_comment.find("(") + 1 :]
            parts = rest.split(None, 2)
            if len(parts) >= (2 if item_name else 1) and len(parts) == 3:
                tokens = re.findall(r"[^\s()]+", parts[2])
                if tokens:
                    rows.append(tokens)
            continue

        depth += no_comment.count("(") - no_comment.count(")")
        before_close = no_comment.split(")", 1)[0]
        tokens = re.findall(r"[^\s()]+", before_close)
        if tokens:
            rows.append(tokens)
        if depth <= 0:
            break
    if not rows:
        raise ConversionError(f"could not extract rows for {form_name} {item_name or ''}")
    return rows


def parse_aliases(text: str) -> dict[str, Any]:
    aliases: dict[str, Any] = {}
    for form in top_forms(text, "defalias"):
        items = form[1:]
        if len(items) % 2:
            raise ConversionError("defalias contains an odd number of name/value entries")
        for name, expr in zip(items[0::2], items[1::2], strict=True):
            aliases[str(name)] = expr
    return aliases


def parse_chords(text: str) -> list[tuple[list[str], str]]:
    chords: list[tuple[list[str], str]] = []
    for form in top_forms(text, "defchordsv2"):
        items = form[1:]
        i = 0
        while i + 1 < len(items):
            trigger = items[i]
            output = items[i + 1]
            if isinstance(trigger, list):
                chords.append(([str(x) for x in trigger], str(output)))
            # Skip: trigger, output, timeout, mode, actions-list
            i += 5
    return chords


def parse_kanata_comment_keysyms(text: str) -> dict[int, dict[str, str]]:
    out: dict[int, dict[str, str]] = {}
    pattern = re.compile(r"([A-Za-z0-9_-]+)\s+\(arbitrary-code\s+(\d+)\)\s*;;\s*->\s*([A-Za-z0-9_]+)")
    for alias, code, keysym in pattern.findall(text):
        display = alias.removeprefix("sc-").replace("-", " ").title()
        out[int(code)] = {"keysym": keysym, "display": display}
    return out


def parse_xkb_legends(path: Path | None) -> dict[str, dict[str, str]]:
    """Return base-key -> keymap-drawer legends for top/Greek symbols."""
    if path is None:
        return {}
    text = path.read_text()
    legends: dict[str, dict[str, str]] = {}
    key_re = re.compile(r"key\s+<[^>]+>\s*\{(?P<body>.*?)\};", re.S)
    for match in key_re.finditer(text):
        body = match.group("body")
        g1 = re.search(r"symbols\[Group1\]\s*=\s*\[\s*([^\]]+)\]", body)
        if not g1:
            continue
        g1_syms = [part.strip() for part in g1.group(1).split(",")]
        if len(g1_syms) < 3:
            continue
        base = KEYSYM_DISPLAY.get(g1_syms[0], g1_syms[0])
        greek = KEYSYM_DISPLAY.get(g1_syms[2], g1_syms[2])
        if not base or greek == "NoSymbol":
            continue
        entry: dict[str, str] = {"h": greek}
        g2 = re.search(r"symbols\[Group2\]\s*=\s*\[\s*([^\]]+)\]", body)
        if g2:
            top_sym = g2.group(1).split(",", 1)[0].strip()
            top = KEYSYM_DISPLAY.get(top_sym, top_sym)
            if top and top != "NoSymbol":
                entry["s"] = top
        legends[base] = entry
    return legends


def parse_xkb_keysyms(path: Path | None) -> dict[int, dict[str, str]]:
    if path is None:
        return {}
    text = path.read_text()
    keycodes: dict[str, int] = {}
    for name, code in re.findall(r"<([^>]+)>\s*=\s*(\d+)\s*;", text):
        keycodes[name] = int(code)

    out: dict[int, dict[str, str]] = {}
    for line in text.splitlines():
        body, _, comment = line.partition("//")
        match = re.search(r"key\s+<([^>]+)>\s*\{.*?\[\s*([^\],\s]+)", body)
        if not match:
            continue
        name, keysym = match.groups()
        code = keycodes.get(name)
        if code is None:
            # XKB's evdev aliases name high keycodes as <I120>, <I208>, ...
            # while Kanata arbitrary-code uses the Linux evdev code. X11's
            # evdev keycode is normally Linux code + 8, so convert back.
            match = re.fullmatch(r"I(\d+)", name)
            if match:
                code = int(match.group(1)) - 8
            elif name == "HELP":
                code = 138
            else:
                continue
        comment = comment.strip()
        if comment and re.fullmatch(r"[IVX]+", comment):
            display = comment
        else:
            display = comment.replace("-", " ").title() if comment else keysym
        out[code] = {"keysym": keysym.strip(), "display": display}
    return out


def expand_kinesis_rows(rows: list[list[str]]) -> list[str]:
    """Expand the visual Kinesis matrix to the existing QMK info.json order."""
    counts = [len(r) for r in rows]
    expected_prefixes = ([12, 12, 12, 12, 8, 4, 2, 6], [12, 14, 14, 12, 8, 4, 2, 6])
    if counts not in expected_prefixes:
        # Fall back to a simple flatten for other keyboards.
        return [key for row in rows for key in row]

    out: list[str] = []
    out.extend(rows[0])
    for row in rows[1:3]:
        if len(row) == 12:
            out.extend(row[:6] + ["", ""] + row[6:])
        else:
            out.extend(row)
    out.extend(rows[3])
    out.extend([""] + rows[4] + [""])
    out.extend(rows[5])
    mid = rows[6]
    bottom = rows[7]
    # QMK physical order for the Kinesis thumb clusters has the right upper
    # thumb key (PgUp in defsrc) before the lower inner key (PgDn in defsrc).
    out.extend([bottom[0], bottom[1], mid[0], bottom[2], mid[1], bottom[3], bottom[4], bottom[5]])
    return out


def label_for_action(action: Any, aliases: dict[str, Any], code_labels: dict[int, dict[str, str]]) -> Any:
    if isinstance(action, str):
        return label_for_token(action, aliases, code_labels)
    if isinstance(action, list) and action:
        op = str(action[0])
        if op in {"layer-while-held", "layer-toggle", "layer-switch"} and len(action) >= 2:
            return str(action[1])
    return str(action)


def label_for_token(token: str, aliases: dict[str, Any], code_labels: dict[int, dict[str, str]]) -> Any:
    if token == "" or token == "_":
        return ""
    if token.startswith("@"):
        alias = token[1:]
        if alias not in aliases:
            return token
        return label_for_expr(alias, aliases[alias], aliases, code_labels)
    return TOKEN_LABELS.get(token, token)


def clean_layout_key(value: dict[str, str]) -> Any:
    cleaned = {k: v for k, v in value.items() if v}
    if set(cleaned) == {"t"}:
        return cleaned["t"]
    return cleaned


def annotate_symbol_legends(key: Any, legends: dict[str, dict[str, str]]) -> Any:
    """Add Space Cadet top/Greek legends to normal-layer printable keys."""
    tap = key if isinstance(key, str) else key.get("t") if isinstance(key, dict) else None
    if not isinstance(tap, str):
        return key
    extra = legends.get(tap, {})
    if tap in NUMBER_ROW_GREEK_LEGENDS:
        extra = {**extra, "h": NUMBER_ROW_GREEK_LEGENDS[tap]}
    if not extra:
        return key
    annotated = {"t": tap, **extra}
    if isinstance(key, dict):
        annotated = {**key, **annotated}
    return annotated


def chord_tap_label(key: Any) -> Any:
    """Use only the human chord name on the combined chords layer."""
    if isinstance(key, dict):
        return key.get("t") or key.get("tap") or ""
    return key


def colorize_modifier_key(key: Any) -> Any:
    """Color modifier keys while hiding the modifier word itself.

    Tap-hold keys keep their tap legend (e.g. `f`) but drop the hold legend
    (`shift`), relying on the fill color plus the legend. Pure modifier keys are
    drawn as empty colored keys.
    """
    if isinstance(key, str):
        key_type = MODIFIER_TYPES.get(key)
        if not key_type:
            return key
        # Keep Greek/Top labels visible on their normal-layer thumb keys while
        # still color-coding them like modifiers. Other pure modifiers stay
        # color-only to avoid visual clutter.
        tap = key if key in {"greek", "top"} else ""
        return {"t": tap, "type": key_type}
    if isinstance(key, dict):
        key = dict(key)
        hold_type = MODIFIER_TYPES.get(str(key.get("h") or ""))
        tap_type = MODIFIER_TYPES.get(str(key.get("t") or ""))
        key_type = hold_type or tap_type
        if key_type:
            if hold_type:
                key["h"] = ""
            if tap_type:
                key["t"] = ""
            key.setdefault("type", key_type)
    return key


def combo_center_span_width(qmk_info_json: Path, positions: list[int], key_pitch: int = 56) -> int | None:
    """Return a combo box width spanning trigger-key center to center."""
    try:
        info = json.loads(qmk_info_json.read_text())
        layout = next(iter(info["layouts"].values()))["layout"]
        centers = [float(layout[pos]["x"]) + float(layout[pos].get("w", 1)) / 2 for pos in positions]
    except (OSError, KeyError, IndexError, StopIteration, json.JSONDecodeError):
        return None
    return round((max(centers) - min(centers)) * key_pitch)


def add_modifier_legend(svg_path: Path) -> None:
    """Place a visual legend for modifier key colors at the top of an SVG."""
    svg = svg_path.read_text()
    match = re.search(
        r'<svg width="(?P<width>[0-9.]+)" height="(?P<height>[0-9.]+)" viewBox="(?P<minx>[0-9.\-]+) (?P<miny>[0-9.\-]+) (?P<vbw>[0-9.]+) (?P<vbh>[0-9.]+)"',
        svg,
    )
    if not match:
        return

    old_height = float(match.group("height"))
    old_miny = float(match.group("miny"))
    old_vbh = float(match.group("vbh"))
    extra_height = 110
    new_height = old_height + extra_height
    new_miny = old_miny - extra_height
    new_vbh = old_vbh + extra_height
    new_open = match.group(0).replace(
        f'height="{match.group("height")}"', f'height="{new_height:g}"'
    ).replace(
        f'viewBox="{match.group("minx")} {match.group("miny")} {match.group("vbw")} {match.group("vbh")}"',
        f'viewBox="{match.group("minx")} {new_miny:g} {match.group("vbw")} {new_vbh:g}"',
    )
    svg = svg[: match.start()] + new_open + svg[match.end() :]

    x0 = 30
    y0 = new_miny + 32
    item_w = 120
    row_h = 34
    swatch = 20
    parts = [
        f'<g transform="translate({x0:g}, {y0:g})" class="modifier-legend">',
        '<text x="0" y="0" class="label">modifier colors:</text>',
    ]
    for index, (name, (_css_class, fill, stroke)) in enumerate(MODIFIER_PALETTE.items()):
        col = index % 4
        row = index // 4
        x = col * item_w
        y = 28 + row * row_h
        parts.append(
            f'<rect x="{x:g}" y="{y - swatch / 2:g}" width="{swatch:g}" height="{swatch:g}" '
            f'rx="4" ry="4" fill="{fill}" stroke="{stroke}" stroke-width="1"/>'
        )
        parts.append(
            f'<text x="{x + swatch + 8:g}" y="{y:g}" style="text-anchor:start; dominant-baseline:middle">'
            f'{html.escape(name)}</text>'
        )
    parts.append("</g>")
    legend = "\n".join(parts)
    svg = svg.replace("</style>", "</style>\n" + legend, 1)
    # keymap-drawer has one class for all hold/shifted legends. In our
    # generated normal layer, hold=Greek/front and shifted=Top, so add more
    # specific classes for color-coding those symbols.
    svg = re.sub(r'class="key((?: [^"]+)?) hold"', r'class="key\1 hold greek-legend"', svg)
    svg = re.sub(r'class="key((?: [^"]+)?) shifted"', r'class="key\1 shifted top-legend"', svg)
    svg_path.write_text(svg)


def label_for_expr(alias: str, expr: Any, aliases: dict[str, Any], code_labels: dict[int, dict[str, str]]) -> Any:
    alias_label = SPACE_CADET_ALIAS_LABELS.get(
        alias,
        alias.removeprefix("sc-").replace("-", " ").title() if alias.startswith("sc-") else alias,
    )
    if isinstance(expr, str):
        return label_for_token(expr, aliases, code_labels)
    if not isinstance(expr, list) or not expr:
        return alias_label

    op = str(expr[0])
    if op in {"tap-hold-release", "tap-hold-next-release"}:
        if len(expr) < 4:
            return alias_label
        # tap-hold-release delay tap-delay tap hold; tap-hold-next-release delay tap hold
        tap_i, hold_i = (3, 4) if op == "tap-hold-release" else (2, 3)
        tap = label_for_action(expr[tap_i], aliases, code_labels)
        hold = label_for_action(expr[hold_i], aliases, code_labels)
        return clean_layout_key({"t": str(tap), "h": str(hold)})

    if op == "arbitrary-code" and len(expr) >= 2:
        try:
            code = int(str(expr[1]))
        except ValueError:
            return alias_label
        mapped = code_labels.get(code)
        if mapped:
            display = mapped.get("display") or mapped.get("keysym") or alias_label
            keysym = mapped.get("keysym", "")
            if keysym and keysym != display:
                return clean_layout_key({"t": display, "h": keysym})
            return display
        return alias_label

    if op in {"layer-toggle", "layer-switch"} and len(expr) >= 2:
        return str(expr[1]).title()

    return alias_label


def build_keymap_yaml(
    kanata_path: Path,
    xkb_path: Path | None,
    qmk_info_json: Path,
    layer_name: str,
    yaml_path: Path,
    combo_style: str,
) -> dict[str, Any]:
    text = kanata_path.read_text()
    aliases = parse_aliases(text)
    code_labels = parse_kanata_comment_keysyms(text)
    code_labels.update(parse_xkb_keysyms(xkb_path))
    symbol_legends = parse_xkb_legends(xkb_path)

    src_rows = rows_from_raw_block(text, "defsrc")
    layer_rows = rows_from_raw_block(text, "deflayer", layer_name)
    src_keys = expand_kinesis_rows(src_rows)
    layer_keys = expand_kinesis_rows(layer_rows)
    if len(src_keys) != len(layer_keys):
        raise ConversionError(f"defsrc has {len(src_keys)} keys but layer {layer_name!r} has {len(layer_keys)}")

    layers = {
        layer_name: [
            annotate_symbol_legends(colorize_modifier_key(label_for_token(k, aliases, code_labels)), symbol_legends)
            for k in layer_keys
        ]
    }
    for form in top_forms(text, "deflayer"):
        name = str(form[1]) if len(form) > 1 else ""
        if not name or name == layer_name:
            continue
        rows = rows_from_raw_block(text, "deflayer", name)
        keys = expand_kinesis_rows(rows)
        if len(keys) == len(src_keys):
            if name.startswith("sc-"):
                layers[name] = [chord_tap_label(label_for_token(k, aliases, code_labels)) for k in keys]
            else:
                layers[name] = [colorize_modifier_key(label_for_token(k, aliases, code_labels)) for k in keys]

    chords = parse_chords(text)
    combo_layer_name = "chords"
    if combo_style == "layer" and chords:
        # A second, single diagram for chords keeps the key/mod drawing clean
        # while avoiding dozens of separate combo mini-diagrams. Leave the
        # physical keys blank so chord labels can sit directly on the keys they
        # describe without competing with base legends.
        layers[combo_layer_name] = ["" for _ in src_keys]

    first_pos: dict[str, int] = {}
    for i, key in enumerate(src_keys):
        first_pos.setdefault(key, i)

    combos = []
    for triggers, output in chords:
        try:
            positions = [first_pos[t] for t in triggers]
        except KeyError as exc:
            raise ConversionError(f"combo trigger {exc.args[0]!r} is not present in defsrc") from exc

        output_label = label_for_token(output, aliases, code_labels)
        chord_label = chord_tap_label(output_label)
        combo: dict[str, Any] = {
            "p": positions,
            "k": chord_label if combo_style == "layer" else output_label,
            "l": [combo_layer_name if combo_style == "layer" else layer_name],
        }
        if combo_style == "hidden":
            combo["hidden"] = True
        elif combo_style == "layer":
            # On the dedicated chords layer, draw labels directly over the
            # involved blank keys rather than above/below the keyboard.
            combo.update({"a": "mid", "d": False})
            if chord_label in {"Clear Input", "Help"}:
                width = combo_center_span_width(qmk_info_json, positions)
                if width:
                    combo["w"] = width
        elif combo_style == "separate":
            # Render each chord as a small dedicated diagram.
            combo["draw_separate"] = True
        elif len(positions) == 2:
            left = min(positions)
            # Adjacent horizontal chord labels are wider than a single key.
            # Stagger them into two vertical lanes so runs like q-w, w-e,
            # e-r, ... remain readable instead of stacking on top of each
            # other. Non-adjacent same-row chords still draw as top chords
            # with dendrons rather than sitting in the key well.
            combo.update({"a": "top", "o": 0.25 if left % 2 == 0 else 0.95, "d": True})
        else:
            combo.update({"a": "mid", "o": 0.15})
        combos.append(combo)

    layout_path = Path(qmk_info_json)
    # Resolve the output path itself (not just its parent) so generating through
    # dotfile symlinks still writes layout paths relative to the real output
    # file in the submodule.
    yaml_dir = yaml_path.resolve(strict=False).parent
    layout_ref = os.path.relpath(layout_path.resolve(), yaml_dir)

    return {
        "layout": {"qmk_info_json": layout_ref},
        "layers": layers,
        "combos": combos,
        "draw_config": {
            "combo_w": 52 if combo_style == "layer" else 64,
            "combo_h": 30 if combo_style == "layer" else 26,
            "arc_radius": 6,
            "separate_combo_diagrams": combo_style == "separate",
            "svg_extra_style": MODIFIER_SVG_STYLE,
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kanata", required=True, type=Path, help="Kanata .kbd config to read")
    parser.add_argument("--xkb", type=Path, help="XKB snippet mapping arbitrary-code numbers to keysyms")
    parser.add_argument("--qmk-info-json", required=True, type=Path, help="keymap-drawer/QMK physical layout JSON")
    parser.add_argument("--layer", default="normal", help="Kanata deflayer to render (default: normal)")
    parser.add_argument("--output-yaml", required=True, type=Path, help="Generated keymap-drawer YAML path")
    parser.add_argument("--output-svg", type=Path, help="Generated SVG path; omitted means only YAML is written")
    parser.add_argument(
        "--combo-style",
        choices=("layer", "separate", "inline", "hidden"),
        default="layer",
        help="How to render defchordsv2 combos: one chords layer, separate diagrams, inline labels, or hide them (default: layer)",
    )
    parser.add_argument("--keymap", default=shutil.which("keymap") or "keymap", help="keymap-drawer executable")
    args = parser.parse_args()

    try:
        data = build_keymap_yaml(args.kanata, args.xkb, args.qmk_info_json, args.layer, args.output_yaml, args.combo_style)
        args.output_yaml.parent.mkdir(parents=True, exist_ok=True)
        args.output_yaml.write_text(yaml.safe_dump(data, sort_keys=False, allow_unicode=True))
        if args.output_svg:
            args.output_svg.parent.mkdir(parents=True, exist_ok=True)
            yaml_for_draw = args.output_yaml.resolve(strict=False)
            svg_for_draw = args.output_svg.resolve(strict=False)
            subprocess.run(
                [args.keymap, "draw", str(yaml_for_draw), "-o", str(svg_for_draw)],
                check=True,
                cwd=str(yaml_for_draw.parent),
            )
            add_modifier_legend(svg_for_draw)
    except (ConversionError, OSError, subprocess.CalledProcessError) as exc:
        print(f"draw-kanata-keymap.py: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
