# CLAUDE.md

Guidance for Claude Code when working in this repository.

## What this package does

`survey.tm` is an R package that manages translations of CAPI survey questionnaires. It
pulls text items out of a Survey Solutions questionnaire, syncs them with a Google
Sheets-based 'Translation Database' where translators work, and writes the translations
back into a questionnaire .xlsx that can be uploaded to the Survey Solutions Designer.

## The pipeline

The exported functions form one linear workflow, and the README documents it in this order:

1. `get_suso_tfiles()` — download questionnaire templates from the Designer (nested list of
   data.tables, one element per questionnaire, one inner element per sheet).
2. `parse_suso_titems()` — flatten those into a single data.table of text items.
3. `get_tdb_data()` — read the 'Translation Database' Google Sheet (one element per language).
4. `update_tdb()` — reconcile the two; new items get `Status = "to translate"`, items that
   disappeared from the source get `Status = "outdated"`.
5. `write_tdb_data()` — push one language back to the Google Sheet.
6. `create_suso_file()` — merge one language into one source questionnaire, write the .xlsx.

`syntax_check()`, `batchTranslate_Deepl2()` and `batchTranslate_GApi()` are optional utilities
hanging off step 1.

## Concepts worth knowing before editing

**`value.unique` is the join key throughout.** It is built by `create.unique.var()` in
`R/utils.R`: lowercase the text and remove all spaces. Everything merges on it, so any
change to how it is derived silently breaks matching between the questionnaire and the
database rather than erroring.

**`qcode_pattern` is stripped and re-attached.** Question codings (e.g. `"Q1. "`) are removed
from `Original text` before the join so that the same text with different codings matches one
database entry, then pasted back on afterwards. If a caller passes `qcode_pattern` to
`parse_suso_titems()` they must pass the same pattern to `create_suso_file()`, otherwise the
keys do not line up.

**`Status` filtering means `Translation` is legitimately `NA`.** `create_suso_file()` subsets
the database to `Status %in% statuses` and left-joins, so any excluded item ends up with
`Translation = NA`. `NA` is the correct output — it exports as an empty cell and Survey
Solutions falls back to the original text. Guard any `paste0()` on `Translation` with
`fifelse(is.na(Translation), NA_character_, ...)`; a bare `paste0()` produces the literal
string `"NA"` and ships it to the Designer.

## Conventions

- data.table throughout, and `NAMESPACE` has `import(data.table)`, so data.table functions
  are called unqualified (`copy`, `fifelse`, `is.data.table`). Other packages are namespaced
  (`stringr::`, `assertthat::`).
- Input validation is `assertthat::assert_that()` with an explicit `msg`.
- Docs are roxygen2 with markdown enabled. `NAMESPACE` and `man/` are generated — edit the
  roxygen blocks and run `devtools::document()`, never the generated files.
- `README.Rmd` is the source; `README.md` is the knitted github_document. Keep both in sync.

## Tests

testthat edition 3, in `tests/testthat/`.

`setup-r2.tms.R` hard-stops the whole suite unless `suso_designer_user` and
`suso_designer_pw` are set as environment variables, and most existing tests call the live
Designer API. Because setup files are sourced for `test_file()` too, there is no way to run a
single file through testthat without those credentials.

To run a self-contained test file (one that does not need the network) without credentials:

```r
Rscript -e 'pkgload::load_all(quiet=TRUE); library(testthat); library(data.table); source("tests/testthat/test-create_suso.R", local=TRUE)'
```

`test-create_suso.R` is written this way — it builds its fixtures inline and touches nothing
external. Prefer this style for new tests.

Internal (non-exported) functions such as `create_suso_sheet()` are reachable from tests
because tests run inside the package namespace.
