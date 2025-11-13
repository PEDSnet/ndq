# Facts Over Time (Original Postgres Implementation)

Facts Over Time (Original Postgres Implementation)

## Usage

``` r
check_fot_loop(
  fot_tbl,
  time_frame,
  omop_or_pcornet = "omop",
  lookback_interval = 1,
  check_string = "fot",
  visits_only = FALSE,
  distinct_visits = TRUE
)
```

## Arguments

- fot_tbl:

  a table with information describing the fact tables that should be
  examined; see
  [`?fot_input_omop`](https://pedsnet.github.io/ndq/reference/fot_input_omop.md)
  or
  [`?fot_input_pcornet`](https://pedsnet.github.io/ndq/reference/fot_input_pcornet.md)
  for details

- time_frame:

  a table of dates that should be iterated through to retrieve the facts
  for each time period; has columns: time_start, time_end

- omop_or_pcornet:

  string indicating the CDM format of the data; defaults to `omop`

- lookback_interval:

  the number of time periods (defined in check_fot) to look back;
  defaults to 1

- check_string:

  the abbreviated name of the check; defaults to `fot`

- visits_only:

  if TRUE, counts ONLY distinct visits and not patients or rows

- distinct_visits:

  if TRUE, counts distinct visits as well as total counts and total
  patients

## Value

a dataframe with one row for each time period within the specified time
span for each check;

- if visits_only = TRUE, will produce only counts of visits for the
  check + time period

- if visits_only = FALSE and distinct_visits = TRUE, will produce counts
  of patients, rows, and visits for the check + time period

- if visits_only = FALSE and distinct_visits = FALSE, will produce
  counts of patients and rows for the check + time period
