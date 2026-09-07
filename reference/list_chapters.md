# List Available Chapters

Lists textbook chapters and the learning materials available for each
chapter, including the Quarto chapter file, helper script, and completed
script.

## Usage

``` r
list_chapters()
```

## Value

A data frame describing the available chapter materials.

## Examples

``` r
list_chapters()
#>    chapter                             title  qmd helper  full
#> 1        1                 Introduction To R TRUE   TRUE  TRUE
#> 2        2         Introduction To tidyverse TRUE   TRUE  TRUE
#> 3        3                    Visualizations TRUE   TRUE  TRUE
#> 4        4              Comparing Two Groups TRUE   TRUE  TRUE
#> 5        5         Comparing Multiple Groups TRUE   TRUE  TRUE
#> 6        6        Analyzing Categorical Data TRUE   TRUE  TRUE
#> 7        7                       Correlation TRUE   TRUE  TRUE
#> 8        8                 Linear Regression TRUE   TRUE  TRUE
#> 9        9               Logistic Regression TRUE   TRUE  TRUE
#> 10      10            Reproducible Reporting TRUE  FALSE FALSE
#> 11      11                          Appendix TRUE  FALSE FALSE
#> 12      12  Downloading Installing R RStudio TRUE  FALSE FALSE
#> 13      13 Reproducible Reporting R Markdown TRUE  FALSE FALSE
#> 14      14                Package References TRUE  FALSE FALSE
#> 15      15                        References TRUE  FALSE FALSE
```
