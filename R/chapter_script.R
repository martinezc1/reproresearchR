#' Copy or run a textbook chapter script
#'
#' @param chapter Chapter number (e.g., 1, 2, 3).
#' @param type Script type: "full" or "helper".
#' @param dest Directory to copy the script into. Defaults to working directory.
#' @param run If TRUE, source() the script after copying.
#' @param overwrite If TRUE, overwrite an existing file.
#' @param open If TRUE, attempt to open the copied script in RStudio.
#'
#' @return Path to the copied script (invisibly if run = TRUE).
#' @examples
#' # Copy the Chapter 3 helper script into the working directory
#' \dontrun{
#' chapter_script(3, "helper")
#' }
#'
#' # Copy and open the Chapter 3 helper script in RStudio
#' \dontrun{
#' chapter_script(3, "helper", open = TRUE)
#' }
#' @export
chapter_script <- function(chapter,
                           type = c("full", "helper"),
                           dest = getwd(),
                           run = FALSE,
                           overwrite = FALSE,
                           open = FALSE) {

  type <- match.arg(type)

  if (!is.numeric(chapter) || length(chapter) != 1 || is.na(chapter)) {
    stop("`chapter` must be a single number (e.g., 1, 2, 3).", call. = FALSE)
  }

  chapter <- sprintf("%02d", as.integer(chapter))
  fname <- paste0("chapter", chapter, "_", type, ".R")

  pkg_path <- system.file("extdata", "scripts", fname, package = "reproresearchR")

  if (pkg_path == "") {
    stop(
      "Script not found: ", fname,
      "\nCheck that it exists in inst/extdata/scripts/.",
      call. = FALSE
    )
  }

  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  out_path <- file.path(dest, fname)

  if (file.exists(out_path) && !overwrite) {
    stop(
      "File already exists: ", out_path,
      "\nUse overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  ok <- file.copy(pkg_path, out_path, overwrite = overwrite)
  if (!ok) stop("Failed to copy script.", call. = FALSE)

  if (open) {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(out_path)
    } else {
      message(
        "`open = TRUE` requested, but RStudio is not available.\n",
        "File copied to: ", out_path
      )
    }
  }

  if (run) {
    source(out_path, local = new.env(parent = globalenv()))
    return(invisible(out_path))
  }

  out_path
}
