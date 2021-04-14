Data Source
================
Caitlin Bolz, Matt Voss, Steven Xia
3/24/2021

Our goal is to compare the biogas produced from landfills to more
traditional fuels such as petroleum and natural gas. Our data set for
landfills can be found at this
[link](https://www.epa.gov/lmop/landfill-technical-data). Our data set
for petroleum and natural gas can be found at this
[link](https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2018-ghg)

Our github repository for this project can be found at this
[link](https://github.com/cabolz/STAT433-Project)

Put code here for reading in data

``` r
  library(readr)
  naturalGas<-read_csv("LandFillDataEasierToAccess.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   `Landfill Name` = col_character(),
    ##   State = col_character(),
    ##   `Landfill Alias` = col_character(),
    ##   `Physical Address` = col_character(),
    ##   City = col_character(),
    ##   County = col_character(),
    ##   `Zip Code` = col_character(),
    ##   `Composting?` = col_character(),
    ##   `Ownership Type` = col_character(),
    ##   `Landfill Owner Organization(s)` = col_character(),
    ##   `Landfill Operator Organization` = col_character(),
    ##   `Current Landfill Status` = col_character(),
    ##   `Landfill Design Capacity (tons)` = col_number(),
    ##   `Waste in Place (tons)` = col_number(),
    ##   `Annual Waste Acceptance Rate (tons per year)` = col_number(),
    ##   `Does LF Recirculate Leachate?` = col_character(),
    ##   `Leachate Recirc Frequency (Past 10 Yrs)` = col_character(),
    ##   `Permitted as RD&D LF?` = col_character(),
    ##   `LFG Collection System In Place?` = col_character(),
    ##   `Flares in Place?` = col_character()
    ##   # ... with 1 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
  head(naturalGas)
```

    ## # A tibble: 6 x 40
    ##   `GHGRP ID` `Landfill ID` `Landfill Name` State `Landfill Alias`
    ##        <dbl>         <dbl> <chr>           <chr> <chr>           
    ## 1    1007341          1994 Anchorage Regi… AK    <NA>            
    ## 2    1010389         11941 Capitol Dispos… AK    <NA>            
    ## 3    1005349         12216 Central Penins… AK    Soldotna Landfi…
    ## 4         NA         10960 Kodiak Island … AK    <NA>            
    ## 5    1004380         11020 Merrill Field … AK    <NA>            
    ## 6    1011448         10980 Palmer Central… AK    Central Landfil…
    ## # … with 35 more variables: `Physical Address` <chr>, City <chr>, County <chr>,
    ## #   `Zip Code` <chr>, Latitude <dbl>, Longitude <dbl>, `Composting?` <chr>,
    ## #   `Ownership Type` <chr>, `Landfill Owner Organization(s)` <chr>, `Landfill
    ## #   Operator Organization` <chr>, `Year Landfill Opened` <dbl>, `Landfill
    ## #   Closure Year` <dbl>, `Current Landfill Status` <chr>, `Design Landfill Area
    ## #   (acres)` <dbl>, `Current Landfill Area (acres)` <dbl>, `Design Landfill
    ## #   Depth (feet)` <dbl>, `Current Landfill Depth (feet)` <dbl>, `Landfill
    ## #   Design Capacity (tons)` <dbl>, `Waste in Place (tons)` <dbl>, `Waste in
    ## #   Place Year` <dbl>, `Annual Waste Acceptance Rate (tons per year)` <dbl>,
    ## #   `Annual Waste Acceptance Year` <dbl>, `Does LF Recirculate
    ## #   Leachate?` <chr>, `Leachate Recirc Frequency (Past 10 Yrs)` <chr>,
    ## #   `Permitted as RD&D LF?` <chr>, `LFG Generated (mmscfd)` <dbl>, `LFG
    ## #   Collection System In Place?` <chr>, `LFG Collected (mmscfd)` <dbl>, `LFG
    ## #   Collected Year` <dbl>, `Percent Methane` <dbl>, `Flares in Place?` <chr>,
    ## #   `Number of Flares` <dbl>, `LFG Flared (mmscfd)` <dbl>, `LFG Flared
    ## #   Year` <dbl>, `Gas Collection System Comments` <chr>
