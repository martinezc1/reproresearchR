# reproresearchR

[![CRANstatus](https://www.r-pkg.org/badges/version/reproresearchR)](https://CRAN.R-project.org/package=reproresearchR)
[![CRANdownloads](https://cranlogs.r-pkg.org/badges/grand-total/reproresearchR?color=blue)](https://r-pkg.org/pkg/reproresearchR)

`reproresearchR` is the companion R package for the open educational
resource *Reproducible Research Using R* by Christian Martinez. Version
**0.1.2**.

The package is designed to support learning and practicing reproducible
research in R by providing:

- teaching datasets used throughout the textbook
- complete chapter materials for working alongside the textbook
- helper and completed R scripts for each chapter
- editable Quarto chapter files for annotation and experimentation
- a bridge between learning R and applying reproducible workflows in
  real projects

This package is especially useful for students, instructors, and
self-learners who want a structured way to practice transparent,
organized, and repeatable analysis.

Users are encouraged to take the resources provided in this package and
edit, personalize, experiment with, and even break them!

> *Remember*: That’s why they put erasers on pencils!

## Companion Textbook

This package accompanies:

Martinez (2026), *Reproducible Research Using R*

<https://martinezc1-reproducible-research-using-r.share.connect.posit.cloud/>

<https://doi.org/10.5281/zenodo.19136755>

## Installation

Install the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("martinezc1/reproresearchR")
```

Install the released version from CRAN:

``` r

install.packages("reproresearchR")
```

Then load the package:

``` r

library(reproresearchR)
```

## What is included?

The package includes:

- teaching datasets for practice and examples
- editable Quarto (`.qmd`) files corresponding to textbook chapters
- helper R scripts designed for following along with chapter material
- completed R scripts that can be used as references
- functions for discovering and copying chapter materials into an R
  project

## Included datasets

The package includes teaching datasets used throughout the textbook,
such as:

- `exam_data`
- `infection_treatments`
- `pizza_prices`

## Working with chapter materials

The recommended way to use `reproresearchR` is to create an RStudio
Project for your work and load the package within that project.

To see the chapters available in the package:

``` r

list_chapters()
```

When you are ready to work through a chapter, use
[`chapter_materials()`](https://martinezc1.github.io/reproresearchR/reference/chapter_materials.md):

``` r

chapter_materials(2)
```

This creates a folder for the selected chapter in your current working
directory. For example:

``` text
chapter02/
├── 02-Introduction-To-tidyverse.qmd
├── chapter02_helper.R
└── chapter02_full.R
```

Each chapter folder contains:

- **Chapter:** an editable Quarto version of the textbook chapter that
  can be annotated, modified, and rendered locally.
- **Helper:** a scaffolded R script for following along and practicing
  the chapter material.
- **Full:** a completed R script that can be used as a reference.

Additional chapters can be added to the same project as you progress
through the textbook:

``` r

chapter_materials(3)
chapter_materials(4)
```

This allows you to gradually build an organized collection of chapter
materials within a single R project.

Users are encouraged to code alongside each chapter using the helper
script. If you get stuck, the full script is available as a reference.

## Why this package?

`reproresearchR` was built to make reproducible research more
approachable and practical. Rather than learning workflows in the
abstract, users work directly with datasets, code, and editable chapter
materials tied to a complete textbook and teaching framework.

The goal is to help learners build habits that support:

- organized project structure
- transparent analysis
- reproducible code
- clear documentation
- experimentation and annotation
- shareable research outputs

## Authors & Contributors

**Christian A. Martinez** 📧 <c.martinez0@outlook.com>  
GitHub: [@martinezc1](https://github.com/martinezc1)
