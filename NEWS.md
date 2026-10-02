
# eq5dsuite 2.1.0

### Analysis and reporting

- **Breaking change:** EQ-5D value analyses now use a pre-calculated column specified by `name_utility`, rather than selecting a value set. This also applies to LSS, LFS and Health Profile Grid analyses that use values. The Health Profile Grid now ranks observed states and no longer takes `eq5d_version`.
- Added `eq5d_profile_shannon()` for Shannon's index and evenness. 
- EQ-5D value functions now return unnamed numeric vectors in input order. 

### Value sets and mappings

- Added the Nigerian EQ-5D-5L value set (`NG`) and `eqxwr_UK()` for mapping 3L responses to 2026 UK 5L values. 
- Renamed UK value set codes to `GB`, retaining `UK` as a deprecated alias.
- Removed interactive value set selection and improved custom value set validation and caching. 
- `eqvs_display(return_df = TRUE)` now returns without printing.

### Shiny app

- **Bug fix:** the value set selector offered the data's own instrument's value sets for every method. A crosswalk values one instrument's responses with the *other* instrument's value set, so with EQ-5D-3L data the reverse crosswalk listed 41 EQ-5D-3L sets where it should have listed the 45 EQ-5D-5L ones -- and 22 of those 41 were refused by `eq5d()`. About half the value sets are published for both instruments, which is why the list looked right. The selector now follows the method's target instrument and names it, and a selection the new method cannot use is cleared rather than carried over.
- Redesigned the app around the workflow. Integrated UK mapping into value calculation.
- Added utility-column selection, timepoint ordering and Shannon's indices. 
- Added result reordering and removal, Word reports using the supplied template, and reproducible R script exports.

### Deployment and maintenance

- Added online deployment support, with configurable upload limits, session cleanup and privacy notices, and `inst/shiny/DEPLOY.md` as a guide to deploying on Shiny Server.
- Added internal helpers for reading, preparing, validating and formatting data, and for age bands. These support the app and are not part of the package's interface: `eq5dsuite::` lists the analysis functions and the value set tools, and nothing else. The app reaches the helpers through `eq5dsuite:::`, and `app.R` on a server is `eq5dsuite:::eq5d_app(online = TRUE)`.
- The R script the app generates calls only the analysis functions, and writes reading, column renaming, the validation checks, the age-band midpoints and the display formatting out in full, so every step can be read and changed. A test runs the generated script and compares its value columns, results and formatted tables against the app's.
- Expanded tests and documentation.
-  Removed `providercode` from `example_data`.

---

# eq5dsuite 2.0.0 (Breaking change release)

## API rename — all 31 analysis functions have new descriptive names

All `table_X_X_X()` and `figure_X_X_X()` functions have been renamed to
descriptive equivalents following the `eq5d_<domain>_<what>()` convention.
The old numeric names no longer exist; update any existing code using the
table below.

| Old name | New name |
|---|---|
| `table_1_1_1` | `eq5d_profile_level_summary` |
| `table_1_1_2` | `eq5d_profile_level_summary_by_group` |
| `table_1_1_3` | `eq5d_profile_top_states` |
| `table_1_2_1` | `eq5d_profile_change_summary` |
| `table_1_2_2` | `eq5d_profile_pchc_table` |
| `table_1_2_3` | `eq5d_profile_pchc_with_no_problems_table` |
| `table_1_2_4` | `eq5d_profile_dimension_change_table` |
| `figure_1_2_1` | `eq5d_profile_pchc_by_group_plot` |
| `figure_1_2_2` | `eq5d_profile_better_dimensions_by_group_plot` |
| `figure_1_2_3` | `eq5d_profile_worse_dimensions_by_group_plot` |
| `figure_1_2_4` | `eq5d_profile_mixed_dimensions_by_group_plot` |
| `figure_1_2_5` | `eq5d_profile_health_profile_grid` |
| `table_1_3_1` | `eq5d_profile_lss_utility_summary` |
| `table_1_3_2` | `eq5d_profile_lfs_distribution` |
| `table_1_3_3` | `eq5d_profile_lfs_mean_utility` |
| `table_1_3_4` | `eq5d_profile_lfs_utility_summary` |
| `figure_1_3_1` | `eq5d_profile_lss_utility_plot` |
| `figure_1_3_2` | `eq5d_profile_lfs_utility_plot` |
| `figure_1_4_1` | `eq5d_profile_density_curve` |
| `table_2_1` | `eq5d_vas_summary` |
| `table_2_2` | `eq5d_vas_distribution_table` |
| `figure_2_1` | `eq5d_vas_histogram` |
| `figure_2_2` | `eq5d_vas_grouped_distribution_plot` |
| `table_3_1` | `eq5d_utility_summary` |
| `table_3_2` | `eq5d_utility_summary_by_group` |
| `table_3_3` | `eq5d_utility_norms_comparison` |
| `figure_3_1` | `eq5d_utility_over_time_plot` |
| `figure_3_2` | `eq5d_utility_by_group_plot` |
| `figure_3_3` | `eq5d_utility_change_by_group_plot` |
| `figure_3_4` | `eq5d_utility_distribution_plot` |
| `figure_3_5` | `eq5d_utility_vas_scatter_plot` |

## New features

* **`update_value_sets()`** — check for and install new EQ-5D value sets
  from the online repository without requiring a package update. 
  Value sets are hosted at
  <https://github.com/MathsInHealth/eq5dsuite-value-sets>.

* **Automatic value set migration** — when value set codes change (e.g.
  when a country publishes a second value set and the original code is
  disambiguated with a year suffix), `update_value_sets()` automatically
  applies the necessary renames.

* **Package documentation** — `?eq5dsuite` now opens a package-level help
  page listing all exported functions organised by category.

* **Vignettes** — five vignettes are now available via
  `browseVignettes("eq5dsuite")`:
  - *Getting started* — installation, value calculation, value set
    management
  - *Analysing EQ-5D data* — complete analytical workflow using NHS
    PROMs data
  - *Crosswalk methods* — when and how to use each crosswalk method
  - *Custom value sets* — adding, saving, and managing custom value sets
  - *Keeping value sets up to date* — using the online update system

## Bug fixes and improvements

* `make_dummies()` — column matching is now case-insensitive and no longer
  renames columns in place. 

---

# eq5dsuite 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* New EQ-5D value sets available
* New analysis functions  (figure 1_2_5 and figure 1_4_1)
* Updated example data 

# eq5dsuite 1.0.2

* Re-written parts of the code to reduce package dependencies.
* Added a Shiny app for interactive EQ-5D data analysis.
* Improved utility calculation workflows and support for pipeline-based use.

# eq5dsuite 1.0.3

* EQ-5D-3L Netherlands value set renamed: VS_code `NL` → `NL_2006` (Name: `Netherlands_2006`).
* New EQ-5D-3L value set: Netherlands_2026 (VS_code `NL_2026`, doi: 10.1007/s10198-025-01892-2).
* New EQ-5D-5L value set: United Kingdom (VS_code `UK`, doi: 10.1016/j.jval.2026.03.008).