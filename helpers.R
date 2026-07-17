# helpers.R - core data, schema, condition, validation, persistence
# Sourced by app.R

# ===== utility =====

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  # Only NA / empty-string checks make sense for atomic length-1 values.
  # Functions, lists and environments are returned as-is (calling is.na()
  # on them would warn "applied to non-(list or vector)" and can error).
  if (is.atomic(a) && length(a) == 1) {
    if (is.na(a)) return(b)
    if (is.character(a) && !nzchar(a)) return(b)
  }
  a
}

is_empty_val <- function(v) {
  tryCatch({
    if (is.null(v)) return(TRUE)
    if (length(v) == 0) return(TRUE)
    v1 <- v[1]
    if (is.na(v1)) return(TRUE)
    !nzchar(trimws(as.character(v1)))
  }, error = function(e) TRUE)
}

# Display-friendly version of a PROMPT - drops trailing ':' so we don't end up
# with "Square::" when callers append their own ':' or punctuation.
prompt_label <- function(p) {
  if (is.null(p) || !length(p)) return("")
  trimws(sub("[\\s:]+$", "", as.character(p)[1], perl = TRUE))
}

# ===== logging =====

LOG_FILE <- "log/rstone.log"

log_event <- function(level, msg) {
  tryCatch({
    if (!dir.exists("log")) dir.create("log", recursive = TRUE)
    line <- sprintf("[%s] %-5s %s",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    toupper(level), msg)
    cat(line, "\n", file = LOG_FILE, append = TRUE, sep = "")
  }, error = function(e) NULL)
}
log_info  <- function(msg) log_event("info",  msg)
log_warn  <- function(msg) log_event("warn",  msg)
log_error <- function(msg) log_event("error", msg)

# ===== CFG parser (with sections) =====

parse_cfg <- function(text) {
  if (length(text) == 1 && grepl("\n", text, fixed = TRUE)) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  } else {
    lines <- text
  }
  lines <- sub(";.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[lines != "" & !startsWith(lines, "#")]

  blocks   <- list()
  sections <- list()   # field name -> section label
  current_section <- NULL
  current <- NULL

  for (line in lines) {
    # Section marker: [*Section Name*]
    if (grepl("^\\[\\*.+\\*\\]$", line)) {
      current_section <- trimws(sub("^\\[\\*(.+)\\*\\]$", "\\1", line))
      next
    }
    if (grepl("^\\[.+\\]$", line)) {
      current <- toupper(sub("^\\[(.+)\\]$", "\\1", line))
      blocks[[current]] <- list()
      if (!is.null(current_section) && current != "E5")
        sections[[current]] <- current_section
    } else if (!is.null(current) && grepl("=", line, fixed = TRUE)) {
      eq  <- regexpr("=", line, fixed = TRUE)
      key <- trimws(toupper(substr(line, 1, eq - 1)))
      val <- trimws(substr(line, eq + 1, nchar(line)))
      blocks[[current]][[key]] <- val
    }
  }
  list(blocks = blocks, sections = sections)
}

parse_condition <- function(s) {
  s <- trimws(s %||% "")
  if (!nzchar(s)) return(NULL)
  or_flag <- FALSE
  if (grepl("\\s+OR\\s*$",  s, ignore.case = TRUE)) {
    or_flag <- TRUE
    s <- sub("\\s+OR\\s*$",  "", s, ignore.case = TRUE)
  } else if (grepl("\\s+AND\\s*$", s, ignore.case = TRUE)) {
    s <- sub("\\s+AND\\s*$", "", s, ignore.case = TRUE)
  }
  parts <- strsplit(trimws(s), "\\s+", perl = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  field <- toupper(parts[1])
  rest  <- parts[-1]
  not_flag <- FALSE
  if (length(rest) && toupper(rest[1]) == "NOT") {
    not_flag <- TRUE
    rest <- rest[-1]
  }
  values <- trimws(strsplit(paste(rest, collapse = " "), ",", fixed = TRUE)[[1]])
  values <- values[nzchar(values)]
  list(f = field, v = values, not = not_flag, or = or_flag)
}

# load menu options from external file (E5 MENU_FILE)
load_menu_file <- function(path, schema_dir = "schemas") {
  candidates <- unique(c(path, file.path(schema_dir, basename(path)),
                         file.path(schema_dir, path)))
  for (p in candidates) {
    if (file.exists(p)) {
      v <- readLines(p, warn = FALSE)
      v <- trimws(v); v <- v[nzchar(v)]
      return(v)
    }
  }
  warning("MENU_FILE not found: ", path)
  character(0)
}

build_schema <- function(text_or_path, schema_dir = "schemas") {
  text <- if (length(text_or_path) == 1 && file.exists(text_or_path)) {
    paste(readLines(text_or_path, warn = FALSE), collapse = "\n")
  } else {
    text_or_path
  }
  parsed <- parse_cfg(text)
  blocks <- parsed$blocks
  sections <- parsed$sections
  meta   <- blocks[["E5"]] %||% list()
  fields <- setdiff(names(blocks), "E5")
  if (!length(fields)) stop("Schema has no field definitions.")

  OPTS <- list(); TYPES <- list(); PROMPTS <- list(); CONDS <- list()
  MINV <- list(); MAXV <- list(); PATTS <- list(); REQ_IF <- list()
  CARRY <- character(); REQ <- character(); UNQ <- character()

  for (f in fields) {
    b <- blocks[[f]]
    type_ <- toupper(b$TYPE %||% "TEXT")
    if (type_ == "INSTRUMENT") type_ <- "NUMERIC"
    if (type_ == "BOOLEAN") {
      OPTS[[f]] <- c("True", "False")
      type_ <- "MENU"
    }
    TYPES[[f]]   <- type_
    PROMPTS[[f]] <- b$PROMPT %||% f

    if (type_ == "MENU") {
      if (!is.null(b$MENU_FILE)) {
        OPTS[[f]] <- load_menu_file(b$MENU_FILE, schema_dir)
      } else if (!is.null(b$MENU)) {
        OPTS[[f]] <- trimws(strsplit(b$MENU, ",", fixed = TRUE)[[1]])
      }
    }
    if (toupper(b$CARRY %||% "FALSE") == "TRUE")    CARRY <- c(CARRY, f)
    if (toupper(b$REQUIRED %||% "FALSE") == "TRUE") REQ   <- c(REQ,   f)
    if (toupper(b$UNIQUE %||% "FALSE") == "TRUE")   UNQ   <- c(UNQ,   f)

    # Numeric range
    if (!is.null(b$MIN)) MINV[[f]] <- suppressWarnings(as.numeric(b$MIN))
    if (!is.null(b$MAX)) MAXV[[f]] <- suppressWarnings(as.numeric(b$MAX))
    # Regex pattern
    if (!is.null(b$PATTERN)) PATTS[[f]] <- b$PATTERN
    # Conditional required
    if (!is.null(b$REQUIRED_IF))
      REQ_IF[[f]] <- parse_condition(b$REQUIRED_IF)

    cnames <- grep("^CONDITION", names(b), value = TRUE)
    if (length(cnames)) cnames <- cnames[order(as.integer(gsub("CONDITION", "", cnames)))]
    cl <- list()
    for (cn in cnames) {
      pc <- parse_condition(b[[cn]])
      if (!is.null(pc)) cl[[length(cl) + 1]] <- pc
    }
    CONDS[[f]] <- cl
  }

  if (!length(REQ)) REQ <- fields[1]
  if (!length(UNQ)) UNQ <- fields[1]

  # ordered section list (in order of first appearance among fields)
  ordered_sections <- character()
  for (f in fields) {
    s <- sections[[f]] %||% ""
    if (nzchar(s) && !(s %in% ordered_sections))
      ordered_sections <- c(ordered_sections, s)
  }

  list(
    meta         = meta,
    site_name    = meta$TABLE %||% "RStone",
    suggested_db = meta$DATABASE %||% file.path("data", paste0(meta$TABLE %||% "data", ".csv")),
    fields       = fields,
    OPTS         = OPTS, TYPES = TYPES, PROMPTS = PROMPTS, CONDS = CONDS,
    MIN = MINV, MAX = MAXV, PATTERN = PATTS, REQUIRED_IF = REQ_IF,
    CARRY        = CARRY,
    REQUIRED     = REQ,
    UNIQUE       = UNQ,
    SECTIONS     = sections,
    SECTION_ORDER = ordered_sections
  )
}

# ===== condition eval =====

eval_conds <- function(conds, vals) {
  if (!length(conds)) return(TRUE)
  n <- length(conds)
  res <- logical(n)
  for (i in seq_len(n)) {
    cl <- conds[[i]]
    v  <- vals[[cl$f]]
    if (is_empty_val(v)) { res[i] <- FALSE; next }
    v1 <- v[1]
    m <- as.character(v1) %in% as.character(cl$v)
    res[i] <- if (isTRUE(cl$not)) !m else m
  }
  grps <- list(); cur <- res[1]
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      if (isTRUE(conds[[i]]$or)) cur <- cur | res[i + 1]
      else { grps <- c(grps, list(cur)); cur <- res[i + 1] }
    }
  }
  grps <- c(grps, list(cur))
  isTRUE(Reduce("&", grps))
}

# ===== validation =====

validate_value <- function(field, value, schema) {
  # returns NULL if OK, else error string
  if (is_empty_val(value)) return(NULL)
  type_ <- toupper(schema$TYPES[[field]] %||% "TEXT")
  v <- as.character(value[1])

  if (type_ %in% c("NUMERIC","INSTRUMENT")) {
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) return(sprintf("Not a number: '%s'", v))
    if (!is.null(schema$MIN[[field]]) && !is.na(schema$MIN[[field]]) && n < schema$MIN[[field]])
      return(sprintf("Below minimum (%s)", schema$MIN[[field]]))
    if (!is.null(schema$MAX[[field]]) && !is.na(schema$MAX[[field]]) && n > schema$MAX[[field]])
      return(sprintf("Above maximum (%s)", schema$MAX[[field]]))
  }
  if (type_ == "MENU") {
    opts <- schema$OPTS[[field]] %||% character()
    if (length(opts) && !(v %in% opts))
      return(sprintf("Not a valid option: '%s'", v))
  }
  if (!is.null(schema$PATTERN[[field]])) {
    if (!grepl(schema$PATTERN[[field]], v, perl = TRUE))
      return(sprintf("Doesn't match pattern: %s", schema$PATTERN[[field]]))
  }
  NULL
}

# Conditional required: REQUIRED_IF condition met -> required
field_is_required <- function(field, entries, schema) {
  if (field %in% schema$REQUIRED) return(TRUE)
  cond <- schema$REQUIRED_IF[[field]]
  if (is.null(cond)) return(FALSE)
  eval_conds(list(cond), entries)
}

# ===== data I/O with atomic write + backups + lock =====

LOCK_FILE_SUFFIX <- ".lock"
BACKUP_DIR       <- "backups"
MAX_BACKUPS      <- 10

acquire_lock <- function(path) {
  lf <- paste0(path, LOCK_FILE_SUFFIX)
  if (file.exists(lf)) {
    mtime <- file.mtime(lf)
    if (!is.na(mtime) && difftime(Sys.time(), mtime, units = "secs") < 600) {
      return(list(ok = FALSE,
                  msg = sprintf("Locked by another process (lockfile %s, %s old). Delete it manually if you're sure no one else is editing.",
                                lf, format(round(difftime(Sys.time(), mtime, units = "secs"))))))
    }
  }
  tryCatch({
    writeLines(as.character(Sys.getpid()), lf)
    list(ok = TRUE, file = lf)
  }, error = function(e) list(ok = FALSE, msg = paste("Could not lock:", e$message)))
}

release_lock <- function(path) {
  lf <- paste0(path, LOCK_FILE_SUFFIX)
  if (file.exists(lf)) try(file.remove(lf), silent = TRUE)
}

rotate_backups <- function(path) {
  if (!file.exists(path)) return(invisible())
  if (!dir.exists(BACKUP_DIR)) dir.create(BACKUP_DIR, recursive = TRUE)
  base <- basename(path)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dest <- file.path(BACKUP_DIR, sprintf("%s.%s.bak", base, ts))
  tryCatch(file.copy(path, dest, overwrite = TRUE), error = function(e) NULL)
  # prune old backups for this file
  bks <- list.files(BACKUP_DIR, pattern = paste0("^", base, "\\."), full.names = TRUE)
  if (length(bks) > MAX_BACKUPS) {
    bks <- bks[order(file.mtime(bks))]
    file.remove(head(bks, length(bks) - MAX_BACKUPS))
  }
}

atomic_write_csv <- function(df, path) {
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(if (file.exists(tmp)) try(file.remove(tmp), silent = TRUE))
  readr::write_csv(df, tmp, na = "")
  # atomic rename
  ok <- file.rename(tmp, path)
  if (!ok) {
    # fallback: copy then delete
    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(TRUE)
}

load_data_for <- function(path, cols) {
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    df <- tryCatch(
      suppressWarnings(readr::read_csv(path,
                                       col_types = readr::cols(.default = "c"),
                                       show_col_types = FALSE)),
      error = function(e) { log_error(sprintf("read_csv failed: %s", e$message)); NULL }
    )
    if (is.null(df)) df <- data.frame()
    for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_character_
    return(df[, cols, drop = FALSE])
  }
  df <- as.data.frame(matrix(NA_character_, 0, length(cols)), stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

save_data_to <- function(df, path) {
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  d <- dirname(path); if (d != "" && !dir.exists(d)) dir.create(d, recursive = TRUE)

  # Acquire lock
  lk <- acquire_lock(path)
  if (!lk$ok) { log_warn(lk$msg); warning(lk$msg); return(invisible(FALSE)) }
  on.exit(release_lock(path), add = TRUE)

  # Rotate backup
  rotate_backups(path)

  # Atomic CSV write
  ok_csv <- tryCatch({ atomic_write_csv(df, path); TRUE },
                     error = function(e) {
                       log_error(sprintf("CSV write failed: %s", e$message)); FALSE
                     })
  # Parallel JSON
  json_path <- sub("\\.csv$", ".json", path, ignore.case = TRUE)
  if (identical(json_path, path)) json_path <- paste0(path, ".json")
  ok_json <- tryCatch({
    tmp <- paste0(json_path, ".tmp.", Sys.getpid())
    jsonlite::write_json(df, tmp, pretty = TRUE, na = "null", dataframe = "rows")
    file.rename(tmp, json_path)
    TRUE
  }, error = function(e) {
    log_error(sprintf("JSON write failed: %s", e$message)); FALSE
  })

  if (ok_csv && ok_json) log_info(sprintf("Saved %d records to %s", nrow(df), path))
  invisible(ok_csv && ok_json)
}

# ===== config =====

load_config <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
           error = function(e) { log_warn(paste("Bad config:", e$message)); NULL })
}
save_config <- function(cfg, path) {
  tryCatch(jsonlite::write_json(cfg, path, pretty = TRUE, auto_unbox = TRUE),
           error = function(e) NULL)
}

# ===== id auto-increment =====

next_id <- function(last_id) {
  if (is_empty_val(last_id)) return("")
  s <- as.character(last_id[1])
  m <- regmatches(s, regexpr("[0-9]+$", s))
  if (!length(m)) return("")
  paste0(sub("[0-9]+$", "", s), formatC(as.integer(m) + 1, width = nchar(m), flag = "0"))
}

# ===== schema-folder listing =====

list_schemas <- function(dir_path) {
  fs <- list.files(dir_path, pattern = "\\.(cfg|CFG|txt|ini)$",
                   full.names = TRUE, recursive = TRUE)
  if (!length(fs)) return(character(0))
  # Label: relative path from dir_path; prefix examples for clarity
  labels <- vapply(fs, function(p) {
    rel <- sub(paste0("^", normalizePath(dir_path, winslash = "/", mustWork = FALSE), "/?"),
               "", normalizePath(p, winslash = "/", mustWork = FALSE))
    if (grepl("^examples/", rel)) paste0("[example] ", sub("^examples/", "", rel))
    else rel
  }, character(1))
  # Sort: top-level first, then examples
  is_example <- grepl("^\\[example\\]", labels)
  ord <- order(is_example, labels)
  setNames(fs[ord], labels[ord])
}

# ===== CFG block builder for in-app Add Field =====

build_field_block <- function(name, type_, prompt, options = NULL,
                              required = FALSE, carry = FALSE,
                              min_val = NULL, max_val = NULL, pattern = NULL,
                              conditions = list()) {
  if (!nzchar(name) || grepl("\\s", name)) stop("Field name required, no spaces.")
  if (!nzchar(prompt)) prompt <- name

  block <- c(sprintf("[%s]", toupper(name)),
             sprintf("TYPE=%s", type_),
             sprintf("PROMPT=%s", prompt))
  if (isTRUE(required)) block <- c(block, "REQUIRED=True")
  if (isTRUE(carry))    block <- c(block, "CARRY=True")
  if (toupper(type_) == "MENU") {
    if (!length(options) || !nzchar(paste(options, collapse = "")))
      stop("MENU fields need at least one option.")
    block <- c(block, sprintf("MENU=%s", paste(options, collapse = ",")))
  }
  if (!is.null(min_val) && nzchar(as.character(min_val)))
    block <- c(block, sprintf("MIN=%s", min_val))
  if (!is.null(max_val) && nzchar(as.character(max_val)))
    block <- c(block, sprintf("MAX=%s", max_val))
  if (!is.null(pattern) && nzchar(pattern))
    block <- c(block, sprintf("PATTERN=%s", pattern))
  if (length(conditions)) {
    for (i in seq_along(conditions)) {
      cn <- conditions[[i]]
      if (!nzchar(cn$field) || !length(cn$values) || !nzchar(paste(cn$values, collapse = "")))
        next
      not_token <- if (isTRUE(cn$not)) " NOT" else ""
      op_token  <- if (i < length(conditions) && nzchar(cn$op) && toupper(cn$op) != "AND")
                     paste0(" ", toupper(cn$op)) else ""
      block <- c(block,
                 sprintf("CONDITION%d=%s%s %s%s",
                         i, cn$field, not_token,
                         paste(cn$values, collapse = ","),
                         op_token))
    }
  }
  paste(block, collapse = "\n")
}

# ===== schema reorder / delete (rewrite the CFG text) =====

extract_blocks_text <- function(cfg_text) {
  # returns list with: header_text (everything before first block), block_texts (named list of raw text per block)
  lines <- strsplit(cfg_text, "\n", fixed = TRUE)[[1]]
  idx_block <- grep("^\\[.+\\]$", lines)
  if (!length(idx_block)) return(list(header = cfg_text, blocks = list(), names = character()))
  header <- paste(lines[seq_len(idx_block[1] - 1)], collapse = "\n")
  starts <- idx_block
  ends   <- c(idx_block[-1] - 1, length(lines))
  blocks <- character(); nms <- character()
  for (i in seq_along(starts)) {
    chunk <- paste(lines[starts[i]:ends[i]], collapse = "\n")
    h <- lines[starts[i]]
    if (grepl("^\\[\\*", h)) next   # section marker, keep in header position for simplicity
    nm <- toupper(sub("^\\[(.+)\\]$", "\\1", h))
    blocks <- c(blocks, chunk)
    nms <- c(nms, nm)
  }
  list(header = header, blocks = blocks, names = nms)
}

reorder_cfg <- function(cfg_text, ordered_field_names) {
  ex <- extract_blocks_text(cfg_text)
  # keep [E5] always first
  e5_idx <- which(ex$names == "E5")
  others <- setdiff(seq_along(ex$names), e5_idx)
  name_map <- setNames(ex$blocks[others], ex$names[others])
  ordered <- toupper(ordered_field_names)
  ordered <- intersect(ordered, names(name_map))
  remaining <- setdiff(names(name_map), ordered)
  reordered <- c(name_map[ordered], name_map[remaining])

  parts <- c(if (nzchar(ex$header)) ex$header else NULL,
             if (length(e5_idx)) ex$blocks[e5_idx],
             reordered)
  paste(parts, collapse = "\n\n")
}

delete_cfg_field <- function(cfg_text, field_name) {
  ex <- extract_blocks_text(cfg_text)
  keep <- toupper(ex$names) != toupper(field_name) | ex$names == "E5"
  parts <- c(if (nzchar(ex$header)) ex$header else NULL, ex$blocks[keep])
  paste(parts, collapse = "\n\n")
}

# Replace an existing block in-place. new_block_text is the full text of the
# replacement block (as built by build_field_block).
replace_cfg_field <- function(cfg_text, field_name, new_block_text) {
  ex <- extract_blocks_text(cfg_text)
  idx <- which(toupper(ex$names) == toupper(field_name))
  if (!length(idx)) stop("Field not found in schema: ", field_name)
  ex$blocks[idx[1]] <- new_block_text
  parts <- c(if (nzchar(ex$header)) ex$header else NULL, ex$blocks)
  paste(parts, collapse = "\n\n")
}

# Extract the raw block text for a single field, for round-tripping in the editor.
get_cfg_field_block <- function(cfg_text, field_name) {
  ex <- extract_blocks_text(cfg_text)
  idx <- which(toupper(ex$names) == toupper(field_name))
  if (!length(idx)) return(NULL)
  ex$blocks[idx[1]]
}

# ===== audit log =====

AUDIT_FILE <- "audit.log"

audit <- function(action, id, user = "", note = "") {
  tryCatch({
    line <- jsonlite::toJSON(list(
      ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      action = action, id = as.character(id),
      user = as.character(user), note = as.character(note)
    ), auto_unbox = TRUE)
    cat(line, "\n", file = AUDIT_FILE, append = TRUE, sep = "")
  }, error = function(e) log_warn(paste("audit failed:", e$message)))
}

read_audit <- function(n = 200) {
  if (!file.exists(AUDIT_FILE)) return(data.frame())
  lines <- tail(readLines(AUDIT_FILE, warn = FALSE), n)
  if (!length(lines)) return(data.frame())
  rows <- lapply(lines, function(l) tryCatch(jsonlite::fromJSON(l), error = function(e) NULL))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, lapply(rows, as.data.frame))
}
