# Third-party notices and attributions

RStone is an independent R/Shiny implementation of a conditional data-entry workflow for lithic analysis. It is not affiliated with or endorsed by the original authors of the projects acknowledged below.

## E5 (configuration file format and conditional-entry concept)

RStone reuses the configuration file (CFG) format and the conditional data-entry concept originally developed for **E5** by **Shannon P. McPherron** (Max Planck Institute for Evolutionary Anthropology).

- Project: <https://github.com/surf3s/E5>
- License: MIT License
- Copyright (c) 2024 Shannon P. McPherron

E5 is itself based on E4 (Shannon McPherron with Harold L. Dibble) and ultimately on Entrer Trois (with Simon Holdaway). The CFG-based wizard approach and the idea of conditional fields auto-skipping when their conditions are not met are taken from this lineage.

RStone is an **independent re-implementation in R/Shiny**; it does not include any source code from E5. The CFG syntax has been extended with section markers `[*Section Name*]`, numeric ranges `MIN=`/`MAX=`, regex `PATTERN=`, conditional `REQUIRED_IF=`, and external `MENU_FILE=` lookups.

The full text of the E5 MIT License is reproduced below for reference.

---

```
MIT License

Copyright (c) 2024 Shannon P. McPherron

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

---

## R package dependencies

RStone uses the following CRAN packages, each distributed under its own open-source license. Full license texts are available on each package's CRAN page (https://cran.r-project.org/package=PKGNAME):

- shiny, shinydashboard, shinyjs (GPL-3)
- DT, ggplot2, dplyr, readr, tidyr, scales, rlang, later (MIT / GPL)
- jsonlite, base64enc (MIT)
- testthat (MIT)

No code from these packages is bundled with RStone; they are installed at runtime from CRAN.

## Bundled schemas

RStone ships with `schemas/default.cfg`, a generic lithic-analysis starter written for RStone. Worked, site-specific schemas live in `schemas/examples/` — for example a Boomplaas Cave assemblage schema (`schemas/examples/boomplaas.cfg`), if provided. In any such schema the variable list, menu options, and condition structure are the schema author's; the conditional-entry framework around them is RStone's. Reuse or replace these schemas freely with your own CFG.
