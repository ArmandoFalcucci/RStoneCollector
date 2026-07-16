# modules/script_export.R
# Generates standalone, runnable R scripts that reproduce a captured plot
# locally. The user can paste the script into their own RStudio session and
# get the same figure without needing RStone running.

# ---- small helpers ----------------------------------------------------------

# Quote a value safely for embedding in R source.
.rq <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("NULL")
  if (is.numeric(x))  return(deparse(x))
  if (is.logical(x))  return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (length(x) > 1)  return(deparse(as.character(x)))
  paste0('"', gsub('"', '\\\\"', as.character(x)), '"')
}

# Convert an active-conds list (from filter_module) into an R `dplyr::filter()`
# call, or "" if no conditions.
.conds_to_filter <- function(conds) {
  if (!length(conds)) return("")
  exprs <- vapply(conds, function(cnd) {
    if (length(cnd$values) == 1)
      sprintf("`%s` == %s", cnd$field, .rq(cnd$values))
    else
      sprintf("`%s` %%in%% %s", cnd$field, .rq(cnd$values))
  }, character(1))
  paste0("  dplyr::filter(", paste(exprs, collapse = ",\n                "), ") |>\n")
}

# Header block shown at the top of every exported script.
.script_header <- function(title, csv_path, n_records, conds) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  lines <- c(
    "# ============================================================",
    sprintf("# %s", title),
    sprintf("# Exported from RStone on %s", ts),
    sprintf("# Source data: %s  (%d records after filtering)",
            csv_path %||% "<unknown>", n_records),
    if (length(conds))
      sprintf("# Active filters: %s",
              paste(vapply(conds, function(c)
                sprintf("%s %%in%% {%s}", c$field, paste(c$values, collapse=",")),
                character(1)), collapse = "; "))
    else "# No filters applied.",
    "# ============================================================",
    "",
    "suppressPackageStartupMessages({",
    "  library(readr)",
    "  library(dplyr)",
    "  library(ggplot2)",
    "})",
    ""
  )
  paste(lines, collapse = "\n")
}

# ---- Build Your Own ---------------------------------------------------------

# Reproduces the build_plot() switch in server_reports.R. Kept in sync with
# that function — if you add a new plot type there, mirror it here.
bb_to_r <- function(plot_type, x, y, g, facet, flip,
                    conds, csv_path,
                    x_label = NULL, y_label = NULL, g_label = NULL) {

  title <- "Build-Your-Own plot"
  hdr   <- .script_header(title, csv_path, NA, conds)
  flt   <- .conds_to_filter(conds)

  # Column type coercion based on plot type
  coerce <- character()
  if (plot_type %in% c("density","hist","scatter") && nzchar(x %||% ""))
    coerce <- c(coerce, sprintf("  dplyr::mutate(`%s` = as.numeric(`%s`)) |>", x, x))
  else if (nzchar(x %||% ""))
    coerce <- c(coerce, sprintf("  dplyr::mutate(`%s` = as.character(`%s`)) |>", x, x))
  if (nzchar(y %||% ""))
    coerce <- c(coerce, sprintf("  dplyr::mutate(`%s` = as.numeric(`%s`)) |>", y, y))
  if (nzchar(g %||% ""))
    coerce <- c(coerce, sprintf("  dplyr::mutate(`%s` = as.character(`%s`)) |>", g, g))

  drop_na <- if (nzchar(x %||% "")) sprintf("  dplyr::filter(!is.na(`%s`))", x) else ""

  data_block <- paste0(
    "df <- readr::read_csv(",  .rq(csv_path),
    ",\n                       col_types = readr::cols(.default = \"c\"),",
    "\n                       show_col_types = FALSE) |>\n",
    flt,
    paste(coerce, collapse = "\n"),
    if (length(coerce)) "\n" else "",
    drop_na
  )

  pal_line <- 'pal <- c("#1565c0","#c62828","#2e7d32","#f57f17","#6a1b9a","#00695c","#ad1457","#37474f")\n'

  # Geometry block per plot type
  geom <- switch(plot_type,
    "bar" = if (nzchar(g %||% "")) sprintf(
      'cnt <- dplyr::count(df, `%s`, `%s`, name = "n")
p <- ggplot(cnt, aes(x = `%s`, y = n, fill = `%s`)) +
  geom_col(position = "dodge")', x, g, x, g)
    else sprintf(
      'cnt <- dplyr::count(df, `%s`, name = "n")
p <- ggplot(cnt, aes(x = reorder(`%s`, n), y = n)) +
  geom_col(fill = pal[1], width = 0.72) +
  geom_text(aes(label = n), hjust = -0.18, size = 3.5, fontface = "bold")', x, x),

    "stacked" = sprintf(
      'cnt <- dplyr::count(df, `%s`, `%s`, name = "n")
p <- ggplot(cnt, aes(x = `%s`, y = n, fill = `%s`)) + geom_col()', x, g, x, g),

    "grouped" = sprintf(
      'cnt <- dplyr::count(df, `%s`, `%s`, name = "n")
p <- ggplot(cnt, aes(x = `%s`, y = n, fill = `%s`)) +
  geom_col(position = "dodge")', x, g, x, g),

    "proportional" = sprintf(
      'cnt <- dplyr::count(df, `%s`, `%s`, name = "n")
p <- ggplot(cnt, aes(x = `%s`, y = n, fill = `%s`)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format())', x, g, x, g),

    "box" = if (nzchar(g %||% "")) sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`, fill = `%s`)) +
  geom_boxplot(alpha = .8, outlier.size = 1.2)', x, y, g)
    else sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`)) +
  geom_boxplot(fill = pal[1], alpha = .8, outlier.size = 1.2)', x, y),

    "violin" = if (nzchar(g %||% "")) sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`, fill = `%s`)) +
  geom_violin(alpha = .55) +
  geom_boxplot(width = .12, position = position_dodge(.9), outlier.size = .8)',
      x, y, g)
    else sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`)) +
  geom_violin(fill = pal[1], alpha = .55) +
  geom_boxplot(width = .12, fill = "white", outlier.size = .8)', x, y),

    "density" = if (nzchar(g %||% "")) sprintf(
      'p <- ggplot(df, aes(x = `%s`, fill = `%s`, color = `%s`)) +
  geom_density(alpha = .35)', x, g, g)
    else sprintf(
      'p <- ggplot(df, aes(x = `%s`)) + geom_density(fill = pal[1], alpha = .35)', x),

    "hist" = if (nzchar(g %||% "")) sprintf(
      'p <- ggplot(df, aes(x = `%s`, fill = `%s`)) +
  geom_histogram(bins = 25, alpha = .85, color = "white", position = "identity")',
      x, g)
    else sprintf(
      'p <- ggplot(df, aes(x = `%s`)) +
  geom_histogram(bins = 25, fill = pal[1], color = "white")', x),

    "scatter" = if (nzchar(g %||% "")) sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`, color = `%s`)) +
  geom_point(size = 2.5, alpha = .75) +
  geom_smooth(method = "lm", se = FALSE, color = "#37474f",
              linetype = "dashed", linewidth = .5)', x, y, g)
    else sprintf(
      'p <- ggplot(df, aes(x = `%s`, y = `%s`)) +
  geom_point(size = 2.5, alpha = .75, color = pal[1]) +
  geom_smooth(method = "lm", se = FALSE, color = "#37474f",
              linetype = "dashed", linewidth = .5)', x, y),

    sprintf('# Unknown plot type: %s', plot_type)
  )

  # Theme + labels + facet + flip
  labs_x <- if (!is.null(x_label) && nzchar(x_label)) x_label else (x %||% "")
  labs_y <- if (!is.null(y_label) && nzchar(y_label)) y_label
            else if (nzchar(y %||% "")) y else "Count"
  labs_g <- if (!is.null(g_label) && nzchar(g_label)) g_label else (g %||% "")
  title_str <- sprintf("%s by %s", labs_y, labs_x)
  if (nzchar(labs_g)) title_str <- paste0(title_str, " (by ", labs_g, ")")
  if (nzchar(facet %||% "")) title_str <- paste0(title_str, " - faceted by ", facet)

  finish <- c(
    sprintf('p <- p +
  labs(x = %s, y = %s, fill = %s, color = %s, title = %s) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a237e"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))',
            .rq(labs_x), .rq(labs_y),
            if (nzchar(labs_g)) .rq(labs_g) else "NULL",
            if (nzchar(labs_g)) .rq(labs_g) else "NULL",
            .rq(title_str)),
    if (nzchar(facet %||% ""))
      sprintf('p <- p + facet_wrap(~ `%s`)', facet) else "",
    if (isTRUE(flip) && plot_type %in%
        c("bar","stacked","grouped","proportional","box","violin"))
      'p <- p + coord_flip()' else "",
    "",
    "print(p)",
    "",
    "# To save as 300dpi PNG, uncomment:",
    sprintf('# ggsave("rstone_plot.png", p, width = 8, height = 6, dpi = 300)')
  )

  paste(c(hdr, data_block, "", pal_line, geom, "", finish), collapse = "\n")
}

# ---- PCA --------------------------------------------------------------------

pca_to_r <- function(numeric_vars, group_col, conds, csv_path) {
  if (length(numeric_vars) < 2)
    return("# Need at least 2 numeric variables for PCA.")
  hdr <- .script_header("PCA scores plot", csv_path, NA, conds)
  flt <- .conds_to_filter(conds)
  vars_r <- paste0("c(", paste(vapply(numeric_vars, .rq, character(1)),
                                collapse = ", "), ")")
  group_block <- if (!is.null(group_col) && nzchar(group_col))
    sprintf('group_vec <- as.character(df[["%s"]])
group_vec <- group_vec[ok]', group_col)
  else 'group_vec <- NULL'

  body <- sprintf('
df <- readr::read_csv(%s,
                       col_types = readr::cols(.default = "c"),
                       show_col_types = FALSE) |>
%s
numeric_vars <- %s
mat <- as.data.frame(lapply(df[, numeric_vars, drop = FALSE],
                             function(x) suppressWarnings(as.numeric(x))))
ok  <- stats::complete.cases(mat)
mat <- mat[ok, , drop = FALSE]
# Drop zero-variance columns
keep <- vapply(mat, function(x) stats::var(x) > 0, logical(1))
mat <- mat[, keep, drop = FALSE]
stopifnot(ncol(mat) >= 2, nrow(mat) >= 3)

pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
ve  <- (pca$sdev^2) / sum(pca$sdev^2)
scores <- as.data.frame(pca$x[, 1:2])

%s

p <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, alpha = .8,
             aes(color = if (!is.null(group_vec)) group_vec else NULL)) +
  labs(x = sprintf("PC1 (%%.1f%%%%)", ve[1] * 100),
       y = sprintf("PC2 (%%.1f%%%%)", ve[2] * 100),
       color = %s,
       title = "PCA scores") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a237e"))

print(p)

# Loadings (first two PCs):
print(round(pca$rotation[, 1:2], 3))
',
    .rq(csv_path), flt, vars_r, group_block,
    if (!is.null(group_col) && nzchar(group_col)) .rq(group_col) else "NULL")

  paste0(hdr, body)
}

# ---- Correspondence Analysis ------------------------------------------------

ca_to_r <- function(x, y, conds, csv_path) {
  hdr <- .script_header("Correspondence Analysis biplot", csv_path, NA, conds)
  flt <- .conds_to_filter(conds)
  body <- sprintf('
df <- readr::read_csv(%s,
                       col_types = readr::cols(.default = "c"),
                       show_col_types = FALSE) |>
%s
tab <- table(df[["%s"]], df[["%s"]])
stopifnot(nrow(tab) >= 2, ncol(tab) >= 2)

# SVD-based correspondence analysis (no extra deps)
N <- sum(tab); P <- tab / N
r <- rowSums(P); c <- colSums(P)
Z <- (P - outer(r, c)) / sqrt(outer(r, c))
sv <- svd(Z)
rs <- diag(1 / sqrt(r)) %%*%% sv$u
cs <- diag(1 / sqrt(c)) %%*%% sv$v
rownames(rs) <- rownames(tab); rownames(cs) <- colnames(tab)
ve <- (sv$d^2) / sum(sv$d^2)

pts <- rbind(
  data.frame(Dim1 = rs[,1], Dim2 = rs[,2], Label = rownames(rs), Kind = "row"),
  data.frame(Dim1 = cs[,1], Dim2 = cs[,2], Label = rownames(cs), Kind = "col")
)

p <- ggplot(pts, aes(Dim1, Dim2, color = Kind, label = Label)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  scale_color_manual(values = c(row = "#1565c0", col = "#c62828")) +
  labs(x = sprintf("Dim 1 (%%.1f%%%%)", ve[1] * 100),
       y = sprintf("Dim 2 (%%.1f%%%%)", ve[2] * 100),
       title = "Correspondence Analysis") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a237e"))

print(p)
',
    .rq(csv_path), flt, x, y)

  paste0(hdr, body)
}

# ---- Overview report (all menu fields, all numeric fields) -----------------

overview_to_r <- function(menu_fields, numeric_fields, conds, csv_path) {
  hdr <- .script_header("Overview report (all menu + numeric fields)",
                        csv_path, NA, conds)
  flt <- .conds_to_filter(conds)
  body <- sprintf('
df <- readr::read_csv(%s,
                       col_types = readr::cols(.default = "c"),
                       show_col_types = FALSE) |>
%s
menu_fields    <- %s
numeric_fields <- %s

# One bar chart per menu field
for (f in menu_fields) {
  d <- df[!is.na(df[[f]]) & df[[f]] != "", , drop = FALSE]
  if (!nrow(d)) next
  cnt <- as.data.frame(table(d[[f]]))
  names(cnt) <- c("Cat","n")
  p <- ggplot(cnt, aes(x = reorder(Cat, n), y = n)) +
    geom_col(fill = "#1565c0", width = .72) +
    geom_text(aes(label = n), hjust = -0.18, size = 3.5, fontface = "bold") +
    coord_flip() +
    labs(x = "", y = "Count", title = f) +
    theme_minimal()
  print(p)
}

# Summary statistics for numeric fields
for (cc in numeric_fields) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
stats <- do.call(rbind, lapply(numeric_fields, function(cc) {
  v <- stats::na.omit(df[[cc]])
  if (!length(v)) return(NULL)
  data.frame(Measurement = cc, N = length(v),
             Mean = round(mean(v), 2), Median = round(median(v), 2),
             SD = round(stats::sd(v), 2),
             Min = round(min(v), 2), Max = round(max(v), 2))
}))
print(stats)
',
    .rq(csv_path), flt,
    paste0("c(", paste(vapply(menu_fields, .rq, character(1)),
                       collapse = ", "), ")"),
    paste0("c(", paste(vapply(numeric_fields, .rq, character(1)),
                       collapse = ", "), ")"))
  paste0(hdr, body)
}
