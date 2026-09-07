#' Copy Chapter Materials
#'
#' Copies all learning materials associated with a textbook chapter
#' into a local folder. This includes the chapter Quarto file,
#' helper script, and completed script.
#'
#' @param chapter Chapter number.
#' @param path Directory where the chapter folder should be created.
#'   Defaults to the current working directory.
#' @param overwrite Logical. Should existing files be overwritten?
#'   Defaults to FALSE.
#'
#' @return Invisibly returns the paths to the copied files.
#'
#' @examples
#' \dontrun{
#' chapter_materials(1)
#' chapter_materials(4)
#' chapter_materials(4, path = "course-materials")
#' }
#'
#' @export
chapter_materials <- function(
    chapter,
    path = ".",
    overwrite = FALSE
) {

  # Format chapter number as two digits
  chapter_num <- sprintf("%02d", as.integer(chapter))

  # ---- Locate chapter .qmd ----

  chapters_dir <- system.file(
    "chapters",
    package = "reproresearchR"
  )

  chapter_matches <- list.files(
    chapters_dir,
    pattern = paste0("^", chapter_num, "-.*\\.qmd$"),
    full.names = TRUE
  )

  if (length(chapter_matches) == 0) {
    stop(
      "No chapter file found for Chapter ",
      chapter_num,
      ".",
      call. = FALSE
    )
  }

  if (length(chapter_matches) > 1) {
    stop(
      "Multiple chapter files found for Chapter ",
      chapter_num,
      ".",
      call. = FALSE
    )
  }

  chapter_source <- chapter_matches[[1]]

  # ---- Locate helper and full scripts ----

  scripts_dir <- system.file(
    "extdata",
    "scripts",
    package = "reproresearchR"
  )

  helper_source <- file.path(
    scripts_dir,
    paste0("chapter", chapter_num, "_helper.R")
  )

  full_source <- file.path(
    scripts_dir,
    paste0("chapter", chapter_num, "_full.R")
  )

  if (!file.exists(helper_source)) {
    stop(
      "Helper script not found for Chapter ",
      chapter_num,
      ".",
      call. = FALSE
    )
  }

  if (!file.exists(full_source)) {
    stop(
      "Full script not found for Chapter ",
      chapter_num,
      ".",
      call. = FALSE
    )
  }

  # ---- Create destination folder ----

  destination <- file.path(
    path,
    paste0("chapter", chapter_num)
  )

  if (!dir.exists(destination)) {
    dir.create(
      destination,
      recursive = TRUE
    )
  }

  # ---- Copy files ----

  sources <- c(
    chapter = chapter_source,
    helper = helper_source,
    full = full_source
  )

  destinations <- file.path(
    destination,
    basename(sources)
  )

  copied <- file.copy(
    from = sources,
    to = destinations,
    overwrite = overwrite
  )

  # ---- Report result ----

  if (all(copied)) {

    message(
      "Chapter ",
      chapter_num,
      " materials copied to:\n",
      normalizePath(destination)
    )

  } else {

    warning(
      "Some files were not copied. ",
      "They may already exist. ",
      "Use overwrite = TRUE to replace them."
    )
  }

  invisible(destinations)
}
