
<!-- README.md is generated from README.Rmd. Please edit that file -->
amt (animal movement tools)
===========================

The aim of `amt` is to make handling and analyzing animal telemetry data easier by providing functions that simplify common tasks, such as data filtering, calculation of path characteristics and home ranges, and the preparing data for more complex analyses (e.g., step selection functions and alike).

Installation
------------

To install the current development version of `amt` use:

``` r
devtools::install_github("jmsigner/amt")
```

Examples
--------

First we use some simulated dummy data and nest it by id:

``` r
set.seed(123)

library(amt)
#> Loading required package: tidyverse
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats

n <- 1e6
df1 <- data_frame(
  id = sample(1:30, n, TRUE),
  object_ = 1:n,
  x_ = cumsum(rnorm(n)),
  y_ = cumsum(rnorm(n))
)

df1 %>% nest(-id) -> df2
```

### Calculating step lengths

We can calculate some simple statistics (i.e step lengths):

``` r
# add distance
df2 %>% mutate(data = map(data, ~ mutate(.x, dist = distance(.x)))) -> df2

# calcualte the mean distance per individual: 2 ways
# 1. using the nested structure
df2 %>% mutate(mean_dist = map_dbl(data, ~ mean(.$dist, na.rm = TRUE)))
#> # A tibble: 30 × 3
#>       id                  data mean_dist
#>    <int>                <list>     <dbl>
#> 1      9 <tibble [33,315 × 4]>  6.118892
#> 2     24 <tibble [33,119 × 4]>  6.148599
#> 3     13 <tibble [33,307 × 4]>  6.139466
#> 4     27 <tibble [33,497 × 4]>  6.117219
#> 5     29 <tibble [33,277 × 4]>  6.110744
#> 6      2 <tibble [33,377 × 4]>  6.101386
#> 7     16 <tibble [33,189 × 4]>  6.137071
#> 8     17 <tibble [33,181 × 4]>  6.122217
#> 9     14 <tibble [33,473 × 4]>  6.105678
#> 10    21 <tibble [33,082 × 4]>  6.109843
#> # ... with 20 more rows

# 2. using dplyr::group_by
df2 %>% unnest() %>% group_by(id) %>% summarise(mdist = mean(dist, na.rm = TRUE))
#> # A tibble: 30 × 2
#>       id    mdist
#>    <int>    <dbl>
#> 1      1 6.113753
#> 2      2 6.101386
#> 3      3 6.088565
#> 4      4 6.095482
#> 5      5 6.076407
#> 6      6 6.092243
#> 7      7 6.133160
#> 8      8 6.122456
#> 9      9 6.118892
#> 10    10 6.130984
#> # ... with 20 more rows
```

### Calculating Minimum Convex Polygon (MCP) home ranges

And as a proof of concept, calculate a MCP home range

``` r
# Calculate MCP home ranges
df2 %>% mutate(mcp = map(data, ~ mcp2(.x, level = c(50, 75, 95)))) -> df3

# obtain the HR area
df3 %>% mutate(mcp_area = map(mcp, hr_area)) %>% select(id, mcp_area) %>% unnest()
#> # A tibble: 90 × 3
#>       id  level      area
#>    <int> <fctr>     <dbl>
#> 1      9     50  439670.8
#> 2      9     75  435550.8
#> 3      9     95 1275932.1
#> 4     24     50  477525.4
#> 5     24     75  419132.3
#> 6     24     95 1248246.6
#> 7     13     50  444309.9
#> 8     13     75  490393.5
#> 9     13     95 1373099.5
#> 10    27     50  537071.9
#> # ... with 80 more rows
```
