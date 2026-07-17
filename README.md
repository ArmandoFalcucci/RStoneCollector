# RStone

**A wizard-style data-entry and analysis app for lithic analysis.** RStone walks you through recording one field at a time — conditional fields auto-skip when they don't apply, values carry forward sensibly, and each attribute appears in a live panel on the right as you confirm it. Your data lives in a plain CSV (with a parallel JSON copy) that you can back up, version, and push to GitHub straight from the app. Beyond data entry, RStone edits records in several ways, visualises them (including PCA and correspondence analysis), and exports everything — figures, tables, and the underlying **R code** — so your analysis is reproducible.

The variable set is **fully user-configurable**. RStone reads its schema from a plain-text CFG file that you can edit right inside the app (or in any text editor), and the wizard adapts automatically — no code changes needed. RStone ships with [`schemas/default.cfg`](schemas/default.cfg) as a generic starting point; drop any additional `.cfg` files into `schemas/` and switch between them in Settings.

The conditional data-entry concept and CFG format originate in **E5** by Shannon McPherron (<https://github.com/surf3s/E5>). RStone is an independent R/Shiny re-implementation that extends the idea considerably — see [What RStone adds](#what-rstone-adds) below and [`NOTICE.md`](NOTICE.md) for full attribution.

> **Setting up RStone for your own project?** See [DISTRIBUTION.md](DISTRIBUTION.md) — it walks you through cloning, pointing the project at your own GitHub repo, and the daily commit workflow.

---

## What RStone adds

RStone keeps the one-question-at-a-time, auto-skip philosophy of E5 and builds a full analysis environment around it:

- **Edit your schema live in the app.** The Schema tab lets you add, edit, reorder, and delete fields through friendly forms — with validation before anything is written — or edit the raw CFG directly. No restart, no external tools.
- **A live attribute panel.** As you confirm each field, it appears in the **Current Record** panel on the right, so you always see what you've entered so far.
- **Several ways to edit records.** Fix the last record, duplicate one as the basis for the next, edit any row from the table, or **mass-edit** every record matching a filter at once — all validated and audit-logged.
- **Visualisation built in.** Auto overview charts, a **Build-Your-Own** plot builder with optional statistical annotations, plus dedicated **PCA** and **Correspondence Analysis** tabs.
- **Reproducible by design.** Every analysis exports as a runnable **R script**, alongside publication-ready **PNG/JPG at 300 dpi** and **CSV** tables. A **Report Builder** compiles captured plots into a single PDF.
- **Your data stays safe.** Atomic CSV writes (no half-written files), automatic **rolling `.bak` backups**, a lock file for shared drives, structured logging, and a full **audit log** of every change.
- **Version control from the app.** Commit and push your CSV, JSON, and audit log to **GitHub** directly from the Export & Git tab — your off-machine backup and full history in one step.
- **Images, sections, and validation.** `IMAGE` fields with inline previews, `[*Section*]` breadcrumbs, and per-field rules (`MIN`/`MAX`, regex `PATTERN`, menu membership, conditional-required).

## Folder layout

```
RStone/
├── app.R                    ← Shiny UI + bootstrap (~580 lines)
├── helpers.R                ← parsing, validation, atomic I/O, audit
├── stats_helpers.R          ← ANOVA / chi-square / PCA / CA
├── install.R                ← one-time package installer
├── README.md
├── NOTICE.md                ← third-party attributions + E5 license
├── DISTRIBUTION.md          ← per-user setup & git workflow
├── CITATION.cff             ← how to cite RStone
├── LICENSE                  ← MIT
├── .gitignore
├── config.example.json      ← copy of the settings the app creates on first run
├── modules/
│   ├── ui_panels.R
│   ├── filter_module.R      ← reusable dynamic multi-select filters
│   ├── script_export.R      ← R-script generation for reproducibility
│   ├── server_wizard.R
│   ├── server_view.R
│   ├── server_reports.R
│   ├── server_builder.R     ← Build-Your-Own + Report Builder
│   ├── server_schema.R
│   └── server_meta.R        ← settings, export, git
├── schemas/
│   ├── default.cfg          ← generic starter (shipped)
│   └── README.md            ← CFG syntax guide
├── assets/                  ← optional logo.(svg|png|jpg)
├── data/                    ← your CSV + JSON (+ images/) — committed
├── backups/                 ← rolling backups (gitignored)
├── log/                     ← rstone.log (gitignored)
└── tests/
    └── testthat/
        └── test-helpers.R
```

Runtime files created on first launch: `config.json` (gitignored by default — see below), `audit.log`, and `captured_plots.rds`.

---

## First-time setup

1. **Install R** (≥ 4.2) and optionally RStudio: <https://posit.co/download/rstudio-desktop/>
2. From this folder, in R/RStudio:
   ```r
   source("install.R")   # installs shiny, shinyWidgets, DT, ggplot2, base64enc, testthat, …
   shiny::runApp()
   ```
3. In the welcome dialog: pick **site name**, **analyst name** (recorded in the audit log), **data file**, and **schema**. Click *Start*.

To re-run the welcome dialog later: **Settings → Run Welcome Setup**.

> `config.json` stores your per-machine settings and is **gitignored** so the template stays clean; the app recreates it from the welcome dialog. Want it versioned in your own repo? `git add -f config.json`.

---

## Daily use — the wizard

The **Data Entry** tab asks one field at a time.

| Action | How |
|---|---|
| Next | Click *Next →*, press `Enter`, or pick a menu option (auto-advances) |
| Back | Click *← Back* or press `Shift+Enter` (outside a notes field) |
| Skip (non-required) | Click *Skip* or press `Esc` |
| Duplicate last record | `Ctrl+D` from anywhere (or *Duplicate Selected* in View Records) |
| Edit last record | Sidebar button or *Edit Selected* in View Records |
| New blank record | Sidebar button |

When you reach the end of the applicable fields the record is **saved automatically** — no review step. The right-hand **Current Record** panel shows only fields you have *confirmed* this record; the field you're currently editing is highlighted.

**Record IDs are always unique.** If you enter an ID that already exists, RStone refuses it — you're warned the moment you press Next on the ID field, and again at save — so you can never overwrite an existing record by accident. How the next record's ID is pre-filled is your choice in **Settings → Record IDs**:

- **Kept the same** (default) — the previous ID carries over so you can edit it (e.g. type the next bag/find number, or change the last digit). This suits IDs that come from excavation records rather than a running counter.
- **Auto-incremented** — trailing digits increment automatically (`BPA-001 → BPA-002`) for consecutive numbering.

**Notes gate pattern.** The default schema demonstrates a Yes/No gate: a `HasNotes` field appears first, and only if you answer Yes does the `Notes` textarea appear. Reuse it for any optional verbose entry.

---

## Schema tab

Four sub-tabs:

- **Editor** — raw CFG with Validate / Apply & Save / Reload. Every save validates the whole file first, so a malformed edit can never corrupt the file on disk.
- **Add Field** — friendly form (type, prompt, menu options, MIN/MAX, regex, required, carry, dynamic condition rows).
- **Edit Field** — pick a field, see its properties pre-filled, change any subset, save.
- **Reorder / Delete** — move a field up/down or delete it (with confirmation).

Any `.cfg` file you place in `schemas/` appears in **Settings → Active schema** automatically. Uploading a CFG from Settings lands it in `schemas/` too.

**Full CFG syntax reference and a "designing your own schema" walkthrough:** [`schemas/README.md`](schemas/README.md).

---

## View Records

All columns with horizontal scroll and a frozen first column, multi-select field→value filters with Clear All, and row-level **Edit / Duplicate / Delete**. **Mass-edit** applies a validated new value to every record matching the current filter, each change audit-logged.

---

## Reports

- **Overview** — auto bar charts for menu fields, violin+box for numerics, a summary table, and a multi-page **PDF report**. Export the stats as CSV or the whole thing as a runnable **R script**.
- **Build Your Own** — plot-type-aware controls, facets, quick presets (fuzzy-matched across schemas), and an optional **statistical-test** annotation. Per-plot exports: add to the PDF queue, **PNG / JPG at 300 dpi**, **table CSV**, or an **R script**.
- **PCA** — pick 2+ numeric variables with optional grouping; scores, scree, and loadings. Capture to PDF or export plot/loadings/R-script.
- **Correspondence Analysis** — pick two categorical variables; biplot and inertia. Capture or export.
- **Report Builder** — everything you captured lands here; reorder, add section headings, and compile one **PDF** (or a **combined R script**). Captures persist across sessions in `captured_plots.rds`.

---

## What happens on save

1. **Required** fields (incl. `REQUIRED_IF`) checked.
2. Every entered value passed through `validate_value` (range, pattern, menu membership).
3. `.lock` acquired on the CSV path (10-minute stale detection; save aborts if another live process holds it).
4. **Rolling backup** rotated into `backups/` (10 kept).
5. **Atomic CSV write** (`data.csv.tmp.<pid>` → `file.rename`).
6. Parallel JSON written the same way.
7. **Audit line** appended to `audit.log` (action / record ID / analyst / timestamp).
8. Lock released.

Any failure is logged to `log/rstone.log` and shown as a red toast — the app stays alive.

---

## Database format

- `data/<site>.csv` — primary, git-diffable, opens everywhere.
- `data/<site>.json` — parallel JSON dump preserving structure.
- `audit.log` — JSONL audit trail.
- `backups/` — last 10 timestamped CSV backups (gitignored).

Migration to SQLite is a one-function swap in `helpers.R::save_data_to` if you ever go multi-user concurrent.

---

## Version control with GitHub

```bash
cd path/to/RStone
git init
git remote add origin https://github.com/YOUR-USERNAME/YOUR-REPO.git
git add .
git commit -m "Initial RStone setup"
git push -u origin main
```

From **Export & Git** in the app: **Pull from GitHub** fetches and reloads; **Commit & Push** stages CSV, JSON, and `audit.log` and pushes with a message including the record count and analyst name.

---

## Running tests

```r
testthat::test_dir("tests/testthat")
```

Covers the schema parser, condition evaluator (AND/OR precedence, NOT, empty fields), validation (range/pattern/menu), `REQUIRED_IF`, `next_id` auto-increment, reorder/delete CFG rewriting, and the atomic save round-trip.

---

## Troubleshooting

| Symptom | Fix |
|---|---|
| "could not find function 'X'" at launch | re-run `source("install.R")` |
| "Locked by another process" | Check no other RStone is open on the same CSV; if certain it's stale, delete the `.lock` next to the CSV |
| Wrong site / wrong CSV opens | Settings → fix paths → Save Settings, or **Run Welcome Setup** |
| Schema changes don't take effect | Schema tab → **Apply & Save** |
| Numeric field rejects a value | Check the field's `MIN=` / `MAX=` in the CFG |
| Reset everything | Settings → Run Welcome Setup (preserves data), or delete `config.json` manually |

The full log is at `log/rstone.log` — share it when reporting bugs.

---

## Citing RStone

If RStone helps your research, please cite it — see [`CITATION.cff`](CITATION.cff) (GitHub renders a "Cite this repository" button from it). Add your ORCID, affiliation, and the release version/date before publishing.

---

## Credits

RStone reuses the configuration-file (CFG) format and the conditional data-entry concept originally developed for **E5** by **Shannon P. McPherron** (Max Planck Institute for Evolutionary Anthropology), distributed under the MIT License — see <https://github.com/surf3s/E5>. E5 is itself the descendant of E4 (Shannon McPherron with Harold L. Dibble) and Entrer Trois (with Simon Holdaway).

RStone is an **independent re-implementation in R/Shiny** by **Armando Falcucci**. No source code from E5 is included; the CFG syntax has been extended with section markers, numeric ranges, regex patterns, conditional-required, and external menu lookups. Features built on top include the wizard's live summary panel, dynamic Build-Your-Own reports with statistical annotations, PCA and Correspondence Analysis, the Report Builder, reproducible R-script export, mass-edit, the audit log, atomic writes with rolling backups, and direct git integration.

RStone was built with the help of **Claude Opus 4.8** (Anthropic).

See [`NOTICE.md`](NOTICE.md) for full third-party attributions and the E5 license text.

## License

RStone is released under the **MIT License** — see [`LICENSE`](LICENSE). In short: do what you like with it, including commercial use or modification, as long as the copyright notice and license are kept with any substantial copy.
