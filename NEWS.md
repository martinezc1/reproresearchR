# reproresearchR 0.1.2

## New features

* Added `chapter_materials()` to create a complete, editable set of materials for a selected textbook chapter in the user's working directory.
* Chapter materials now include:
  * the corresponding Quarto (`.qmd`) textbook chapter
  * a helper R script for coding alongside the chapter
  * a completed R script for reference
* Added editable Quarto chapter files to the package, allowing users to annotate, modify, experiment with, and render textbook chapters locally.
* Updated `list_chapters()` to reflect the chapter-based learning materials available through the package.

## Changes

* Deprecated `chapter_script()` in favor of `chapter_materials()`. The function remains available for backward compatibility.
* Shifted the recommended package workflow toward creating chapter-specific working folders within an R or RStudio Project.
* Updated package documentation and README to reflect the new chapter-materials workflow.

# reproresearchR 0.1.0

* Initial CRAN release.
* Added three teaching datasets used throughout the textbook:
  * `pizza_prices`
  * `exam_data`
  * `infection_treatments`
* Added `chapter_script()` to copy or run chapter scripts shipped with the package.
* Added `list_chapters()` to list available chapter scripts included in the package.
* Designed as a companion package to the open educational resource textbook
  *Reproducible Research Using R*.
