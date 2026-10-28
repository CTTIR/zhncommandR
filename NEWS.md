# zhncommandR 0.1.0 (development)

* Standardize the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.


* Initial CRAN-targeted release.
* Package extracts the ZHN Auditor Shiny app (v5) backend logic into
  reusable, namespaced functions: readers (`zhn_read_cohort()`,
  `zhn_read_therapy()`, `zhn_read_diagnostics()`, `zhn_read_tumorboard()`),
  parsers (`zhn_prepare_therapy_blocks()`, `zhn_prepare_diagnostic_blocks()`,
  `zhn_parse_oncoprint()`, `zhn_parse_cytogenetics()`), and alteration
  classification helpers (`zhn_normalize_alteration()`,
  `zhn_alteration_type()`, `zhn_is_mutation()`).
* Dashboard launcher `zhn_run_app()` plus bundled synthetic example data
  reachable through `zhn_example_path()`.
* Fixed `as_event01("10 months")` no longer classifies as an event
  (legacy `grepl("^1", ...)` regression).
* Sheet-detection consolidated into a single resolver with explicit overrides
  and clear error messages listing available sheets.
* Duplicate columns post-`janitor::clean_names()` are detected and reported;
  trailing all-empty `none*` columns are dropped automatically.
* Tumour-board decisions are kept in-session (`reactiveVal`) with explicit
  CSV upload/download; no implicit writes to the filesystem.
