# Copy Chapter Materials

Copies all learning materials associated with a textbook chapter into a
local folder. This includes the chapter Quarto file, helper script, and
completed script.

## Usage

``` r
chapter_materials(chapter, path = ".", overwrite = FALSE)
```

## Arguments

- chapter:

  Chapter number.

- path:

  Directory where the chapter folder should be created. Defaults to the
  current working directory.

- overwrite:

  Logical. Should existing files be overwritten? Defaults to FALSE.

## Value

Invisibly returns the paths to the copied files.

## Examples

``` r
if (FALSE) { # \dontrun{
chapter_materials(1)
chapter_materials(4)
chapter_materials(4, path = "course-materials")
} # }
```
