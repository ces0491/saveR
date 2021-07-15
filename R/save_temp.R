#' save an R object to a temporary directory as an RDS file
#'
#' @param object an R object
#' @param file_name string indicating the name of the file to save as
#' @param temp_dir optional string indicating the temp directory. Defaults to \code{tempdir()}
#'
#' @export
#'
save_temp <- function(object, file_name, temp_dir = NULL) {

  #TODO: support for other file extensions

  if (is.null(temp_dir)) {
    temp_dir <- tempdir()
    message(glue::glue("save directory defaulting to {tempdir()}"))
  }

  file_ext <- paste0(file_name, ".rds")

  f <- file.path(tempdir(), file_ext)

  if (file.exists(f)) {
    file.remove(f)
  }

  # rds_files <- stringr::str_subset(list.files(temp_dir, full.names = TRUE), "rds")
  # do.call(file.remove, list(rds_files)) # remove any existing rds files in the temp directory

  saveRDS(object, f)

}
