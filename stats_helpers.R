# stats_helpers.R - statistical annotations for Build-Your-Own plots

# Returns list with method, p, statistic, df, label
quick_test_numeric <- function(d, x, y) {
  v <- d[[y]]
  if (!is.numeric(v)) v <- suppressWarnings(as.numeric(v))
  g <- as.factor(d[[x]])
  k <- length(levels(g))
  if (k < 2) return(NULL)
  ok <- !is.na(v) & !is.na(g)
  if (sum(ok) < (k + 1)) return(NULL)

  # Try ANOVA; if Shapiro on residuals fails, fall back to Kruskal-Wallis
  res <- tryCatch({
    fit <- stats::aov(v[ok] ~ g[ok])
    p_aov <- summary(fit)[[1]][["Pr(>F)"]][1]
    # Shapiro on residuals
    sw <- if (length(stats::residuals(fit)) > 3 && length(stats::residuals(fit)) <= 5000) {
      stats::shapiro.test(stats::residuals(fit))$p.value
    } else NA
    if (!is.na(sw) && sw < 0.05) {
      kw <- stats::kruskal.test(v[ok] ~ g[ok])
      list(method = "Kruskal-Wallis", p = kw$p.value,
           statistic = unname(kw$statistic), df = unname(kw$parameter))
    } else {
      list(method = "ANOVA (one-way)", p = p_aov,
           statistic = summary(fit)[[1]][["F value"]][1],
           df = paste(summary(fit)[[1]][["Df"]], collapse = ", "))
    }
  }, error = function(e) NULL)
  if (is.null(res)) return(NULL)

  pstr <- if (res$p < .001) "< 0.001" else sprintf("= %.3f", res$p)
  res$label <- sprintf("%s: stat = %.2f (df = %s), p %s",
                       res$method, res$statistic, res$df, pstr)
  res
}

# Chi-square for two categorical variables
quick_test_categorical <- function(d, x, g) {
  tab <- table(d[[x]], d[[g]])
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
  res <- tryCatch({
    expected_low <- any(suppressWarnings(stats::chisq.test(tab)$expected) < 5)
    if (expected_low && sum(tab) < 1000) {
      ft <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)
      list(method = "Fisher (simulated)", p = ft$p.value, statistic = NA, df = NA)
    } else {
      cs <- stats::chisq.test(tab)
      list(method = "Chi-square", p = cs$p.value,
           statistic = unname(cs$statistic), df = unname(cs$parameter))
    }
  }, error = function(e) NULL)
  if (is.null(res)) return(NULL)
  pstr <- if (res$p < .001) "< 0.001" else sprintf("= %.3f", res$p)
  res$label <- if (!is.na(res$statistic %||% NA))
    sprintf("%s: stat = %.2f, df = %s, p %s",
            res$method, res$statistic, res$df, pstr)
  else
    sprintf("%s, p %s", res$method, pstr)
  res
}

# Correlation for two numeric variables
quick_corr <- function(d, x, y) {
  vx <- suppressWarnings(as.numeric(d[[x]]))
  vy <- suppressWarnings(as.numeric(d[[y]]))
  ok <- !is.na(vx) & !is.na(vy)
  if (sum(ok) < 4) return(NULL)
  res <- tryCatch(stats::cor.test(vx[ok], vy[ok], method = "pearson"),
                  error = function(e) NULL)
  if (is.null(res)) return(NULL)
  p <- res$p.value
  pstr <- if (p < .001) "< 0.001" else sprintf("= %.3f", p)
  list(method = "Pearson r", p = p, statistic = unname(res$estimate),
       df = unname(res$parameter),
       label = sprintf("Pearson r = %.3f, n = %d, p %s",
                       res$estimate, sum(ok), pstr))
}

# PCA on numeric subset
do_pca <- function(df, numeric_cols, group_col = NULL) {
  if (length(numeric_cols) < 2) return(list(error = "Need at least 2 numeric variables."))
  d <- df[, numeric_cols, drop = FALSE]
  for (c in numeric_cols) d[[c]] <- suppressWarnings(as.numeric(d[[c]]))
  ok_rows <- stats::complete.cases(d)
  if (sum(ok_rows) < 3) return(list(error = "Need at least 3 complete rows."))
  d2 <- d[ok_rows, , drop = FALSE]
  # drop zero-variance cols
  vars <- vapply(d2, stats::var, numeric(1))
  d2   <- d2[, vars > 0, drop = FALSE]
  if (ncol(d2) < 2) return(list(error = "Not enough varying numeric columns."))

  pca <- stats::prcomp(d2, scale. = TRUE, center = TRUE)
  scores <- as.data.frame(pca$x[, 1:min(3, ncol(pca$x)), drop = FALSE])
  if (!is.null(group_col) && group_col %in% names(df))
    scores$Group <- as.character(df[[group_col]][ok_rows])
  loadings <- as.data.frame(pca$rotation[, 1:min(3, ncol(pca$rotation)), drop = FALSE])
  loadings$Variable <- rownames(loadings)
  var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
  list(scores = scores, loadings = loadings, var_exp = var_exp, model = pca)
}

# Correspondence analysis (categorical x categorical)
do_ca <- function(df, x, g) {
  tab <- table(df[[x]], df[[g]])
  if (nrow(tab) < 2 || ncol(tab) < 2)
    return(list(error = "Need a 2x2 or larger contingency table."))
  # Manual SVD-based CA (no extra deps)
  N <- sum(tab); P <- tab / N
  r <- rowSums(P); c <- colSums(P)
  if (any(r == 0) || any(c == 0))
    return(list(error = "Empty rows/cols after filtering."))
  Z <- (P - outer(r, c)) / sqrt(outer(r, c))
  sv <- svd(Z)
  rs <- diag(1/sqrt(r)) %*% sv$u
  cs <- diag(1/sqrt(c)) %*% sv$v
  rn <- rownames(tab); cn <- colnames(tab)
  rownames(rs) <- rn; rownames(cs) <- cn

  k <- min(2, ncol(sv$u))
  row_df <- data.frame(Dim1 = rs[, 1], Dim2 = if (k>1) rs[, 2] else 0,
                       Label = rn, Kind = "row", stringsAsFactors = FALSE)
  col_df <- data.frame(Dim1 = cs[, 1], Dim2 = if (k>1) cs[, 2] else 0,
                       Label = cn, Kind = "col", stringsAsFactors = FALSE)
  inertia <- sv$d^2
  var_exp <- inertia / sum(inertia)
  list(points = rbind(row_df, col_df), var_exp = var_exp)
}
