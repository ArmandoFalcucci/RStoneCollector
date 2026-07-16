# Setting up RStone for your own project

Each user owns their own copy of RStone — code + data + schema — in their own git repository. The app folder *is* the project folder. One project = one repository.

If you want to run multiple projects, clone the repo multiple times into separate folders (e.g. `~/SiteX/`, `~/SiteY/`). They are fully independent.

---

## One-time setup

### 1. Install prerequisites

- **R** (≥ 4.2): https://posit.co/download/rstudio-desktop/
- **Git**: https://git-scm.com/downloads
- A free **GitHub** account (or any git host: GitLab, Bitbucket, your institution's git server)

### 2. Get the code

Open a terminal (or the **Terminal** tab in RStudio):

```bash
# Replace with the upstream URL provided by the RStone maintainer
git clone https://github.com/SOMEONE/RStone.git MyProject
cd MyProject
```

You now have a folder `MyProject/` containing the app code and an empty `data/` folder.

### 3. Install R packages

In R/RStudio, from inside `MyProject/`:

```r
source("install.R")
```

This installs every R package the app needs.

### 4. Point the project at your own GitHub repo

Create an empty repository on GitHub (or your git host). Don't initialise it with a README — leave it completely empty. Then:

```bash
# Replace with your own URL
git remote set-url origin https://github.com/YOU/MyProject.git
git push -u origin main
```

That detaches your copy from the upstream RStone repo and points it at your own. From now on, `git push` from inside your project goes to *your* repo, not the maintainer's.

### 5. Launch and configure

In R/RStudio, from inside `MyProject/`:

```r
shiny::runApp()
```

The welcome dialog appears. Fill in:

- **Site name**: e.g. "Trial Cave", "SiteX 2024 season"
- **Analyst**: your name (saved in the audit log)
- **Data file**: leave as default (`data/<site>.csv`) unless you have a reason to change it
- **Schema**: start with `default.cfg` (or an `[example]` schema from `schemas/examples/`), then adapt it in the Schema tab — or upload your own CFG

Click *Start*. The app creates `config.json`, an empty CSV+JSON pair, and you're ready to enter records.

---

## Daily use — commit your work

After a data-entry session, commit your changes from inside the project folder:

```bash
git add -A
git commit -m "Boomplaas: 47 flakes from Unit 3"
git push
```

Or use the **Export & Git** tab inside the app:
- **Commit & Push** stages `data/*.csv`, `data/*.json`, and `audit.log`, makes a commit with the record count + your analyst name, and pushes.

What ends up in git:
- `data/*.csv` and `data/*.json` (your records)
- `audit.log` (who changed what, when)
- `captured_plots.rds` (your PDF report queue, persists across sessions)
- `schemas/*.cfg` (any schemas you edited or added)

What stays local (gitignored, not committed):
- `config.json` (per-machine project settings). The app recreates it from the
  welcome dialog on any machine. If you *do* want your settings versioned in
  your own repo, force-add it once: `git add -f config.json`
- `log/` (rotating debug log)
- `backups/` (last 10 timestamped CSV backups, kept locally for crash recovery)
- `*.lock` files
- `*.tmp.*` (in-flight atomic writes)

---

## Receiving updates to the app

When the RStone maintainer publishes a new version, you can pull just the code changes without touching your data:

```bash
# Add the upstream remote (one-time)
git remote add upstream https://github.com/SOMEONE/RStone.git

# Each time you want to update
git fetch upstream
git merge upstream/main
```

Git will merge code changes (`*.R` files, modules, schema templates) with your committed data. Conflicts only happen if the maintainer changed the same file you customised — typically only a schema under `schemas/` if you edited one in place. Resolve, commit, push.

If you'd rather stay isolated and never receive updates: skip this step.

---

## Working from multiple machines

Because everything is in git, switching between laptops is trivial:

```bash
# On laptop B
git clone https://github.com/YOU/MyProject.git
cd MyProject
source("install.R")          # in R, one-time per machine
shiny::runApp()              # in R, picks up your data and config
```

You can also commit your work on laptop A, `git pull` on laptop B, and continue exactly where you left off. The audit log records timestamps and analyst names, so multi-machine work stays traceable.

---

## Multiple projects per user

Want a separate project for each site or season? Clone the upstream repo into a new folder, point its `origin` to a new GitHub repo, and treat it as a separate project:

```bash
git clone https://github.com/SOMEONE/RStone.git SiteX-2024
cd SiteX-2024
git remote set-url origin https://github.com/YOU/SiteX-2024.git
# ... and so on
```

Each project has its own `config.json`, `data/`, `audit.log`, etc. They never see each other.

---

## What if I outgrow the app?

Everything lives in two universal formats:

- `data/*.csv` opens in Excel, R, Python, anything.
- `data/*.json` keeps types intact.

The schema is plain text in `schemas/*.cfg`. The audit log is JSONL. No proprietary database, no lock-in. If you ever move to a different tool, your data comes with you with zero export effort.
