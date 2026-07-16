# Schema (CFG) syntax reference

RStone reads its variable set from a plain-text `.cfg` file in this folder. The
format is the **E5 CFG format** by Shannon McPherron
(<https://github.com/surf3s/E5>), extended by RStone with sections, numeric
ranges, regex patterns, conditional-required, and external menu lookups.

Start from [`default.cfg`](default.cfg), edit it until it matches your recording
sheet, and the wizard adapts automatically. You can edit CFGs in the app
(**Schema** tab, with Validate / Apply & Save) or in any text editor.

---

## The shape of a file

```ini
[E5]                     ; optional metadata block (must be named E5)
TABLE=Trial-Cave          ; project/table name
DATABASE=data/trial-cave.csv    ; default CSV path suggested at setup

[*Section name*]         ; groups the fields that follow it
[FIELDNAME]              ; one block per variable
KEY=value                ; properties of that field
```

- Lines starting with `#` are comments.
- Everything after a `;` on a line is ignored, so **don't use `;` inside a prompt**.
- Field names are case-insensitive (stored uppercase internally). No spaces.
- The **first field** becomes the unique record key if you don't mark one with
  `UNIQUE=True`, and is treated as required if nothing else is.
- A `DATEOFDATAENTRY` column is added to every dataset automatically — you don't
  declare it.

---

## Field properties

| Key | Applies to | Meaning |
|---|---|---|
| `TYPE=` | all | `TEXT`, `MENU`, `NUMERIC`, `NOTE`, `BOOLEAN`, `IMAGE` (`INSTRUMENT` is treated as `NUMERIC`) |
| `PROMPT=` | all | The question shown in the wizard |
| `MENU=` | MENU | Comma-separated options: `MENU=Flake,Blade,Core` |
| `MENU_FILE=` | MENU | Load options from an external file (one per line): `MENU_FILE=rawmaterials.txt` |
| `MIN=` / `MAX=` | NUMERIC | Range bounds; out-of-range entries are rejected |
| `PATTERN=` | TEXT | Regex the value must match, e.g. `PATTERN=^BPA-[0-9]+$` |
| `REQUIRED=True` | all | Field must be filled before the record can save |
| `REQUIRED_IF=` | all | Conditionally required (same syntax as a condition, below) |
| `CARRY=True` | all | Value pre-fills on the next new record (e.g. Square, Unit) |
| `UNIQUE=True` | all | Marks the record-key field (used for auto-increment) |
| `CONDITION1=`, `CONDITION2=`… | all | Show this field only when the condition(s) hold |

`BOOLEAN` is shorthand for a `MENU` of `True,False`.

---

## Sections

Group fields with a marker line:

```ini
[*Cortex*]
[CORTEX]
TYPE=MENU
MENU=0%,1-50%,51-99%,100%
```

Every field after a `[*...*]` marker belongs to that section until the next
marker. The wizard shows a breadcrumb ("Section 3 of 7: Cortex") so you always
know where you are.

---

## Conditions (skip logic)

A condition names a field, an optional `NOT`, and one or more comma-separated
values:

```ini
CONDITION1=CLASS Tool               ; show when CLASS == Tool
CONDITION1=CORTEX NOT 0%            ; show when CORTEX is anything but 0%
CONDITION1=CLASS Tool,Core          ; show when CLASS is Tool OR Core (value list)
```

Multiple conditions combine with **AND** by default. Append `OR` to a line to
OR it with the next one. `OR` binds tighter than `AND`:

```ini
CONDITION1=A x
CONDITION2=B y OR
CONDITION3=C z
; evaluates as:  A==x  AND  (B==y OR C==z)
```

An **empty** field never satisfies a condition (so a field gated on an unanswered
question stays hidden). `REQUIRED_IF=` uses exactly the same syntax as one
condition:

```ini
REQUIRED_IF=CLASS Tool             ; required only when CLASS == Tool
```

---

## The Yes/No note gate (a handy pattern)

```ini
[HASNOTES]
TYPE=MENU
PROMPT=Add a free-text note?
MENU=Yes,No

[NOTES]
TYPE=NOTE
PROMPT=Notes
CONDITION1=HASNOTES Yes
```

The big textarea only appears when you actually have something to write — reuse
this for any optional verbose entry.

---

## External menu files

For big shared lists (raw materials, typologies) keep the options in a text file
alongside your CFG, one option per line, and point at it:

```ini
[RAWMATERIAL]
TYPE=MENU
PROMPT=Raw material
MENU_FILE=rawmaterials.txt
```

RStone looks for the file as given, then inside `schemas/`.

---

## Where files live

- `schemas/default.cfg` — the generic starter (this repo ships it).
- `schemas/examples/` — fully worked schemas to study or copy. They appear in the
  app's schema picker with an `[example]` prefix.
- Any `.cfg` you add to `schemas/` (or upload via Settings) shows up in the picker
  automatically.
