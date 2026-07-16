# tests/testthat/test-helpers.R
# Run with: testthat::test_dir("tests/testthat")
library(testthat)
library(readr)
library(jsonlite)
source("../../helpers.R")

# ===== %||% =====
test_that("%||% falls back on null/empty/NA", {
  expect_equal(NULL %||% "fb", "fb")
  expect_equal(NA   %||% "fb", "fb")
  expect_equal(""   %||% "fb", "fb")
  expect_equal("ok" %||% "fb", "ok")
  expect_equal(0    %||% "fb", 0)
})

# ===== is_empty_val =====
test_that("is_empty_val handles various inputs", {
  expect_true(is_empty_val(NULL))
  expect_true(is_empty_val(NA))
  expect_true(is_empty_val(""))
  expect_true(is_empty_val("   "))
  expect_false(is_empty_val("x"))
  expect_false(is_empty_val("0"))
  expect_false(is_empty_val(0))
})

# ===== parse_condition =====
test_that("parse_condition extracts field, values, NOT, OR", {
  pc <- parse_condition("RawMaterial Tool,Flake")
  expect_equal(pc$f, "RAWMATERIAL")
  expect_equal(pc$v, c("Tool","Flake"))
  expect_false(pc$not); expect_false(pc$or)

  pc2 <- parse_condition("RawMaterial NOT Indeterminate OR")
  expect_true(pc2$not); expect_true(pc2$or)
  expect_equal(pc2$v, "Indeterminate")
})

# ===== eval_conds: AND/OR precedence =====
test_that("eval_conds: simple AND", {
  conds <- list(parse_condition("A x"), parse_condition("B y"))
  expect_true(eval_conds(conds, list(A="x", B="y")))
  expect_false(eval_conds(conds, list(A="x", B="z")))
})

test_that("eval_conds: simple OR", {
  conds <- list(parse_condition("A x OR"), parse_condition("B y"))
  expect_true(eval_conds(conds, list(A="z", B="y")))
  expect_true(eval_conds(conds, list(A="x", B="z")))
  expect_false(eval_conds(conds, list(A="z", B="z")))
})

test_that("eval_conds: OR binds tighter than AND", {
  # A AND B OR C - groups as A AND (B OR C)
  conds <- list(parse_condition("A x"),
                parse_condition("B y OR"),
                parse_condition("C z"))
  expect_true(eval_conds(conds, list(A="x", B="y", C="?")))
  expect_true(eval_conds(conds, list(A="x", B="?", C="z")))
  expect_false(eval_conds(conds, list(A="x", B="?", C="?")))
  expect_false(eval_conds(conds, list(A="?", B="y", C="z")))
})

test_that("eval_conds: empty field makes that condition false", {
  conds <- list(parse_condition("A x"))
  expect_false(eval_conds(conds, list(A = NA)))
  expect_false(eval_conds(conds, list(A = "")))
  expect_false(eval_conds(conds, list(A = NULL)))
})

test_that("eval_conds: NOT operator works", {
  conds <- list(parse_condition("A NOT x"))
  expect_false(eval_conds(conds, list(A = "x")))
  expect_true (eval_conds(conds, list(A = "y")))
  # empty field with NOT still false (not present = condition unmet)
  expect_false(eval_conds(conds, list(A = "")))
})

# ===== build_schema =====
test_that("build_schema parses minimal CFG", {
  cfg <- "
[E5]
TABLE=Test

[ID]
TYPE=TEXT
PROMPT=ID:
UNIQUE=True

[CLASS]
TYPE=MENU
PROMPT=Class:
MENU=A,B,C
REQUIRED=True
"
  sch <- build_schema(cfg)
  expect_equal(sch$fields, c("ID","CLASS"))
  expect_equal(sch$TYPES$CLASS, "MENU")
  expect_equal(sch$OPTS$CLASS, c("A","B","C"))
  expect_true("CLASS" %in% sch$REQUIRED)
  expect_true("ID"    %in% sch$UNIQUE)
})

test_that("build_schema reads sections and MIN/MAX/PATTERN/REQUIRED_IF", {
  cfg <- "
[*Section One*]
[A]
TYPE=NUMERIC
PROMPT=A:
MIN=0
MAX=100

[*Section Two*]
[B]
TYPE=TEXT
PROMPT=B:
PATTERN=^[A-Z]+$

[C]
TYPE=TEXT
PROMPT=C:
REQUIRED_IF=A NOT 0
"
  sch <- build_schema(cfg)
  expect_equal(sch$MIN$A, 0)
  expect_equal(sch$MAX$A, 100)
  expect_equal(sch$PATTERN$B, "^[A-Z]+$")
  expect_equal(sch$SECTIONS$A, "Section One")
  expect_equal(sch$SECTIONS$B, "Section Two")
  expect_equal(sch$SECTIONS$C, "Section Two")
  expect_equal(sch$SECTION_ORDER, c("Section One","Section Two"))
  expect_false(is.null(sch$REQUIRED_IF$C))
})

# ===== validate_value =====
test_that("validate_value catches numeric range violations", {
  sch <- build_schema("
[A]
TYPE=NUMERIC
PROMPT=A:
MIN=0
MAX=10
")
  expect_null(validate_value("A", "5",  sch))
  expect_match(validate_value("A", "-1", sch), "Below minimum")
  expect_match(validate_value("A", "11", sch), "Above maximum")
  expect_match(validate_value("A", "abc",sch), "Not a number")
})

test_that("validate_value enforces regex", {
  sch <- build_schema("
[A]
TYPE=TEXT
PROMPT=A:
PATTERN=^[A-Z]+-[0-9]+$
")
  expect_null(validate_value("A", "BPA-001",  sch))
  expect_match(validate_value("A", "bpa-001", sch), "pattern")
})

test_that("validate_value rejects out-of-menu values", {
  sch <- build_schema("
[A]
TYPE=MENU
PROMPT=A:
MENU=X,Y,Z
")
  expect_null(validate_value("A", "Y", sch))
  expect_match(validate_value("A", "W", sch), "valid option")
})

# ===== field_is_required (REQUIRED_IF) =====
test_that("field_is_required honors REQUIRED_IF", {
  sch <- build_schema("
[A]
TYPE=TEXT

[B]
TYPE=TEXT
REQUIRED_IF=A x
")
  expect_false(field_is_required("B", list(A=""), sch))
  expect_true (field_is_required("B", list(A="x"), sch))
  expect_false(field_is_required("B", list(A="y"), sch))
})

# ===== next_id =====
test_that("next_id increments suffix and preserves padding", {
  expect_equal(next_id("BPA-001"), "BPA-002")
  expect_equal(next_id("BPA-009"), "BPA-010")
  expect_equal(next_id("BPA-099"), "BPA-100")
  expect_equal(next_id("FlakE-12345"), "FlakE-12346")
  expect_equal(next_id("noDigits"), "")
  expect_equal(next_id(""), "")
  expect_equal(next_id(NA), "")
})

# ===== build_field_block =====
test_that("build_field_block validates and emits correct CFG", {
  blk <- build_field_block("EdgeAngle", "MENU", "Edge angle:",
                            options = c("Acute","Right","Obtuse"),
                            required = TRUE,
                            conditions = list(list(field = "CLASS",
                                                    values = c("Tool"),
                                                    not = FALSE, op = "AND")))
  expect_match(blk, "\\[EDGEANGLE\\]")
  expect_match(blk, "TYPE=MENU")
  expect_match(blk, "REQUIRED=True")
  expect_match(blk, "MENU=Acute,Right,Obtuse")
  expect_match(blk, "CONDITION1=CLASS Tool")

  expect_error(build_field_block("Bad Name", "TEXT", "x"))
  expect_error(build_field_block("Ok", "MENU", "x", options = character(0)))
})

# ===== reorder_cfg / delete_cfg_field =====
test_that("reorder_cfg moves fields", {
  cfg <- "
[E5]
TABLE=Test

[A]
TYPE=TEXT
PROMPT=A

[B]
TYPE=TEXT
PROMPT=B

[C]
TYPE=TEXT
PROMPT=C
"
  out <- reorder_cfg(cfg, c("C","A","B"))
  sch <- build_schema(out)
  expect_equal(sch$fields, c("C","A","B"))
})

test_that("delete_cfg_field removes a field", {
  cfg <- "
[E5]
TABLE=Test

[A]
TYPE=TEXT

[B]
TYPE=TEXT
"
  out <- delete_cfg_field(cfg, "A")
  sch <- build_schema(out)
  expect_equal(sch$fields, "B")
})

# ===== atomic_write_csv + backups =====
test_that("save_data_to writes CSV and JSON atomically", {
  tdir <- tempfile("rstone_"); dir.create(tdir)
  old_wd <- setwd(tdir); on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  df <- data.frame(ID = c("a","b"), CLASS = c("Tool","Flake"), stringsAsFactors = FALSE)
  path <- "data.csv"
  dir.create("data", showWarnings = FALSE)
  ok <- save_data_to(df, path)
  expect_true(ok)
  expect_true(file.exists(path))
  expect_true(file.exists("data.json"))
  back <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  expect_equal(nrow(back), 2)
})
