#' @title [Internal: Create File Name of Archived File]
#'
#' @description This is an internal helper function of
#'   \code{\link[helfRlein]{save_rds_archive}} to create and return the archived
#'   file name depending on the parameters chosen by the user
#'
#' @param file The current file, passed from parent function
#' @param last_modified Logical - passed from parent function
#' @param with_time Logical - passed from parent function
#'
#' @seealso \code{\link[helfRlein]{save_rds_archive}}
#'
#' @keywords internal
#'
#' @author Lukas Feick

create_archived_file <- function(file, last_modified, with_time) {

  # create main suffix depending on type
  suffix_main <- ifelse(last_modified,
                        as.character(file.info(file)$mtime),
                        as.character(Sys.time()))

  if (with_time) {

    # create clean date-time suffix
    suffix <- gsub(pattern = " ", replacement = "_", x = suffix_main)
    suffix <- gsub(pattern = ":", replacement = "-", x = suffix)

    # add "at" between date and time
    suffix <- paste0(substr(suffix, 1, 10), "_at_", substr(suffix, 12, 19))

  } else {

    # create date suffix
    suffix <- substr(suffix_main, 1, 10)

  }

  # create info to paste depending on type
  type_info <- ifelse(last_modified,
                      "_MODIFIED_on_",
                      "_ARCHIVED_on_")

  # get file extension (could be any of "RDS", "Rds", "rds", etc.)
  ext <- paste0(".", tools::file_ext(file))

  # replace extension with suffix
  archived_file <- gsub(pattern = paste0(ext, "$"),
                        replacement = paste0(type_info,
                                             suffix,
                                             ext),
                        x = file)

  return(archived_file)

}

#' Save Datestamped RDS Files
#'
#' @param object an object to save as an RDS file
#' @param file string indicating the destination file path
#' @param archive logical indicating whether existing files should be archived - default to \code{TRUE}
#' @param last_modified logical indicating whether datestamp should be last modified time of original RDS or system time - default FALSE to system time
#' @param with_time logical indicating whether the time should be appended to the datestamp - default FALSE
#' @param archive_dir_path optional string specifying where to create the archive
#' @param ... Additional arguments passed to \code{\link[base]{saveRDS}}
#'
#' @return Nothing
#' @export
#'
save_rds <- function(object,
                     file = "",
                     archive = TRUE,
                     last_modified = FALSE,
                     with_time = FALSE,
                     archive_dir_path = NULL,
                     ...) {

  if (file == "" | !"character" %in% class(file)) {
    stop("'file' must be a non-empty character string")
  }

  if (!is.null(archive_dir_path) && archive_dir_path == "") {
    stop("must supply a directory name to 'archive_dir_path' if not NULL")
  }

  if (!is.logical(archive)) {
    archive <- TRUE
    warning("'archive' is not set to a boolean - will use default: ", archive)
  }

  if (!is.logical(with_time)) {
    with_time <- FALSE
    warning("'with_time' is not set to a boolean - will use default: ",
            with_time)
  }

  # IF ARCHIVE == TRUE --------------------------------------------------------

  if (archive) {

    # check if file exists
    if (file.exists(file)) {

      archived_file <- create_archived_file(file = file,
                                            last_modified = last_modified,
                                            with_time = with_time)

      if (!is.null(archive_dir_path)) {

        # get parent directory
        dname <- dirname(file)

        # create archive dir if it does not already exist
        if (!dir.exists(file.path(dname, archive_dir_path))) {
          dir.create(file.path(dname, archive_dir_path), recursive = TRUE)
          message("Created missing archive directory ",
                  sQuote(archive_dir_path))
        }

        # change path of archived file into 'archive' folder
        archived_file <- file.path(dirname(archived_file),
                                   archive_dir_path,
                                   basename(archived_file))

        # copy (rather than rename) file
        # rename sometimes does not work if the directory itself is changed
        # save return value of the file.copy function and wrap in tryCatch
        # set "overwrite" to T so an existing copy is overwritten (see details)

        if (file.exists(archived_file)) {
          warning("Archived copy already exists - will overwrite!")
        }

        temp <- tryCatch({
          file.copy(from = file,
                    to = archived_file,
                    overwrite = TRUE)
        },
        warning = function(e) {
          stop(e)
        })

      } else {

        if (file.exists(archived_file)) {
          warning("Archived copy already exists - will overwrite!")
        }

        # rename existing file with the new name
        # save return value of the file.rename function
        # (returns TRUE if successful) and wrap in tryCatch
        temp <- tryCatch({
          file.rename(from = file,
                      to = archived_file)
        },
        warning = function(e) {
          stop(e)
        })

      }

      # check return value and if archived file exists
      if (temp & file.exists(archived_file)) {
        # then save new file under specified name
        saveRDS(object = object, file = file, ...)
      }


    } else {

      warning("Nothing to overwrite - will use default saveRDS() behavior. ",
              "Additional arguments will be ignored!")

      # if file does not exist (but archive is set to TRUE anyways),
      # save new file under specified name
      saveRDS(object = object, file = file, ...)

    }

  } else {

    # OTHERWISE USE DEFAULT RDS -----------------------------------------------

    warning("'archive' is set to FALSE - will use default saveRDS() behavior. ",
            "Additional arguments will be ignored!")

    saveRDS(object = object, file = file, ...)

  }

}
