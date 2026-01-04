#' List available chapter scripts included with the package
#'
#' @return A data frame with columns: chapter, type, file.
#' @examples
#' list_chapters()
#' @export
list_chapters <- function() {

  scripts_dir <- system.file("extdata", "scripts", package = "reproresearchR")
  if (scripts_dir == "") {
    stop("Could not locate package scripts directory.", call. = FALSE)
  }

  files <- list.files(
    scripts_dir,
    pattern = "^chapter\\d{2}_(full|helper)\\.R$",
    full.names = FALSE
  )

  if (length(files) == 0) {
    return(data.frame(chapter = integer(), type = character(), file = character()))
  }

  chapter_num <- as.integer(sub("^chapter(\\d{2})_.*$", "\\1", files))
  type <- sub("^chapter\\d{2}_(full|helper)\\.R$", "\\1", files)

  out <- data.frame(
    chapter = chapter_num,
    type = type,
    file = files,
    stringsAsFactors = FALSE
  )

  out <- out[order(out$chapter, out$type), , drop = FALSE]
  rownames(out) <- NULL
  out
}
