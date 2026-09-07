# Copy or run a textbook chapter script

\`chapter_script()\` is deprecated. Use \[chapter_materials()\] instead
to copy the complete set of materials associated with a textbook
chapter.

This function is retained for backward compatibility with existing code.

## Usage

``` r
chapter_script(
  chapter,
  type = c("full", "helper"),
  dest,
  run = FALSE,
  overwrite = FALSE,
  open = FALSE
)
```

## Arguments

- chapter:

  Chapter number (e.g., 1, 2, 3).

- type:

  Script type: "full" or "helper".

- dest:

  Directory to copy the script into.

- run:

  If TRUE, source() the script after copying.

- overwrite:

  If TRUE, overwrite an existing file.

- open:

  If TRUE, attempt to open the copied script in RStudio.

## Value

Path to the copied script (invisibly if run = TRUE).

## Examples

``` r
if (FALSE) { # \dontrun{
tmp <- tempdir()
chapter_script(3, "helper", dest = tmp, overwrite = TRUE)
} # }
```
