#' List Available Chapters
#'
#' Lists textbook chapters and the learning materials available
#' for each chapter, including the Quarto chapter file, helper
#' script, and completed script.
#'
#' @return A data frame describing the available chapter materials.
#'
#' @examples
#' list_chapters()
#'
#' @export
list_chapters <- function() {

  chapters_dir <- system.file(
    "chapters",
    package = "reproresearchR"
  )

  scripts_dir <- system.file(
    "extdata",
    "scripts",
    package = "reproresearchR"
  )

  qmd_files <- list.files(
    chapters_dir,
    pattern = "^\\d{2}-.*\\.qmd$"
  )

  chapter_nums <- as.integer(
    sub("^([0-9]{2}).*$", "\\1", qmd_files)
  )

  titles <- sub(
    "^\\d{2}-",
    "",
    qmd_files
  )

  titles <- sub(
    "\\.qmd$",
    "",
    titles
  )

  titles <- gsub(
    "-",
    " ",
    titles
  )

  helper_exists <- file.exists(
    file.path(
      scripts_dir,
      sprintf("chapter%02d_helper.R", chapter_nums)
    )
  )

  full_exists <- file.exists(
    file.path(
      scripts_dir,
      sprintf("chapter%02d_full.R", chapter_nums)
    )
  )

  data.frame(
    chapter = chapter_nums,
    title = titles,
    qmd = TRUE,
    helper = helper_exists,
    full = full_exists,
    stringsAsFactors = FALSE
  )
}
