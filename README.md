Export UK Office of National Statistics Lifetables
================
Altan Orhon
2024-11-02

This notebook downloads UK Office of National Statistics Lifetables for
multiple years, turns them into long format, and exports them to `TSV`
and `RDS`.

Download data:

``` r
data_path <- params$data_path %||% here::here("data", basename(params$data_url))
dir.create(data_path, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(data_path)) {
  message("Downloading data to ", data_path)
  download.file(data_url, data_path)
}
```

Function to load dataset from the `XLSX` file:

``` r
load_nltuk_dataset <- function(path = "data/nltuk198020203.xlsx", sheet_names = NULL, sheet_pattern = "(^[2][0-9]{3})-([2][0-9]{3})", sheet_range = cellranger::cell_limits(c(6, 1), c(NA, 6)), year_min = -Inf, year_max = Inf, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  requireNamespace("readxl", quietly = TRUE) #
  if (is.null(sheet_names)) {
    sheet_names <- readxl::excel_sheets(path) |>
      purrr::keep(stringr::str_detect, sheet_pattern) |>
      unique()
    checkmate::assert_character(sheet_names, min.len = 1)
    cli::cli_inform("Identified {length(sheet_names)} sheet names ({sheet_names})")
  }
  if (is.null(names(sheet_names))) {
    names(sheet_names) <- as.character(sheet_names)
  }
  if (inherits(sheet_range, "cell_limits")) {
    sheet_range <- rep(list(sheet_range), length(sheet_names)) |> rlang::set_names(sheet_names)
  }
  checkmate::assert_list(sheet_range, len = length(sheet_names))
  res <- purrr::map2(sheet_names, sheet_range, ~ {
    readxl::read_excel(path,
      sheet = .x,
      range = .y,
      col_types = c(age = "numeric", mx = "numeric", qx = "numeric", lx = "numeric", dx = "numeric", ex = "numeric")
    )
  }) |>
    dplyr::bind_rows(.id = "sheet") |>
    dplyr::arrange(dplyr::desc(sheet), age) |>
    tidyr::separate_wider_delim(sheet, delim = "-", names = c("y1", "y2"), cols_remove = FALSE) |>
    dplyr::mutate(y1 = as.integer(y1), y2 = as.integer(y2))

  res |>
    dplyr::group_by(sheet) |>
    dplyr::reframe(year = min(y1):max(y2)) |>
    dplyr::ungroup() |>
    dplyr::filter(year >= year_min, year <= year_max) |>
    dplyr::left_join(res, by = "sheet", relationship = "many-to-many") |>
    dplyr::group_by(year, age) |>
    dplyr::arrange(dplyr::desc(year)) |>
    # Use the most recent value for that year
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, age) |>
    dplyr::select(-y1, -y2) |>
    dplyr::rename(origin = sheet)
}
```

Load the data:

``` r
# Load everything from 2004
nltuk_df <- load_nltuk_dataset(data_path, year_min = 2004)
#> Identified 21 sheet names (2020-2022, 2019-2021, 2018-2020, 2017-2019,
#> 2016-2018, 2015-2017, 2014-2016, 2013-2015, 2012-2014, 2011-2013, 2010-2012,
#> 2009-2011, 2008-2010, 2007-2009, 2006-2008, 2005-2007, 2004-2006, 2003-2005, …,
#> 2001-2003, and 2000-2002)
```

## Data Description

- `mx` is the central rate of mortality (number of deaths at age `x`
  last birthday in the 3-year period to which the life table relates
  divided by the average population at that age over the same period)

- `qx` is the mortality rate between age `x` and `x + 1` (probability
  that a person aged `x` exactly will die before reaching age (`x` +1))

- `lx` is the number of survivors to exact age `x` of 100,000 live
  births of the same sex (who are assumed to be subject throughout their
  lives to the mortality rates experienced in the 3-year period to which
  the life table relates)

- `dx` is the number dying between exact age `x` and `x + 1`, described
  similarly to `l[x]`, that is `d[x] = l[x] - l[x +1]`.

- `ex` is the average period expectation of life at exact age `x`, that
  is the average number of years that those aged `x` exactly will live
  thereafter based on the mortality rates experienced in the 3-year
  period to which the life table relates.

## Preview data

``` r
head(nltuk_df)
#> # A tibble: 6 × 8
#>   origin     year   age       mx       qx      lx    dx    ex
#>   <chr>     <int> <dbl>    <dbl>    <dbl>   <dbl> <dbl> <dbl>
#> 1 2002-2004  2004     0 0.00578  0.00577  100000  577.   76.2
#> 2 2002-2004  2004     1 0.000431 0.000431  99423.  42.9  75.6
#> 3 2002-2004  2004     2 0.000255 0.000255  99380.  25.4  74.6
#> 4 2002-2004  2004     3 0.000195 0.000195  99355   19.3  73.6
#> 5 2002-2004  2004     4 0.000159 0.000159  99336.  15.8  72.7
#> 6 2002-2004  2004     5 0.000123 0.000123  99320.  12.2  71.7
```

## Export data

Export TSV:

``` r
output_path_tsv <- params$output_path_tsv %||% here::here("data", paste0(tools::file_path_sans_ext(basename(data_path)), "_", paste0(range(nltuk_df$year, na.rm = TRUE), collapse = "-"), ".tsv"))
message("Exporting TSV to ", output_path_tsv)
#> Exporting TSV to /Users/altan/work/code/uk-lifetables/data/nltuk198020203_2004-2022.tsv
dir.create(dirname(output_path_tsv), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(output_path_tsv)) {
  message("Writing data to ", output_path_tsv)
  write.table(nltuk_df, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, file = output_path_tsv)
} else {
  message("Data already exists at ", output_path_tsv)
}
#> Data already exists at /Users/altan/work/code/uk-lifetables/data/nltuk198020203_2004-2022.tsv
```

Export RDS:

``` r
output_path_rds <- params$output_path_rds %||% here::here("data", paste0(tools::file_path_sans_ext(basename(data_path)), "_", paste0(range(nltuk_df$year, na.rm = TRUE), collapse = "-"), ".rds"))
message("Exporting RDS to ", output_path_rds)
#> Exporting RDS to /Users/altan/work/code/uk-lifetables/data/nltuk198020203_2004-2022.rds
dir.create(dirname(output_path_rds), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(output_path_rds)) {
  message("Writing data to ", output_path_rds)
  saveRDS(nltuk_df, output_path_rds)
} else {
  message("Data already exists at ", output_path_rds)
}
#> Data already exists at /Users/altan/work/code/uk-lifetables/data/nltuk198020203_2004-2022.rds
```

## Session Info

``` r
sessionInfo()
#> R version 4.4.1 (2024-06-14)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Sonoma 14.6.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: America/Los_Angeles
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] magrittr_2.0.3 stringr_1.5.1  dplyr_1.1.4    rlang_1.1.4    purrr_1.0.2   
#> [6] fs_1.6.4       tibble_3.2.1   pacman_0.5.1  
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.4.1    tidyselect_1.2.1  parallel_4.4.1    tidyr_1.3.1      
#>  [5] yaml_2.3.10       fastmap_1.2.0     readxl_1.4.3      here_1.0.1       
#>  [9] R6_2.5.1          generics_0.1.3    knitr_1.48        backports_1.5.0  
#> [13] checkmate_2.3.2   rprojroot_2.0.4   pillar_1.9.0      utf8_1.2.4       
#> [17] stringi_1.8.4     xfun_0.48         cli_3.6.3         withr_3.0.1      
#> [21] digest_0.6.37     rstudioapi_0.17.1 lifecycle_1.0.4   vctrs_0.6.5      
#> [25] evaluate_1.0.1    glue_1.8.0        cellranger_1.1.0  fansi_1.0.6      
#> [29] rmarkdown_2.28    tools_4.4.1       pkgconfig_2.0.3   htmltools_0.5.8.1
```
