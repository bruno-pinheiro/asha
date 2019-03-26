#' @title Clean temporary files
#' @name list_tmp
#'
#' @description List files in \code{tempdir()}
#'
#' @param ext a string defining a file extension (defaul is NULL)
#' @param full a TRUE/FALSE value indicating if the function should return the
#'        full path of the files (default is TRUE).
#'
#' @details \code{list_tmp()} will check and print files in \code{tempdir()}.
#'          If any file extension is informed, will print all files in the root
#'          of temporary directory. If some file extension is informed will
#'          check and print all files matching that extension in
#'          \code{tempdir()}, including nested folders.
#'
#' @seealso \itemize{
#'          \item{\code{tempfile()}}
#'          \item{\code{tempdir()}}
#'          }
#'
#' @return list files in \code{tempdir()}
#'
#' @author Bruno Pinheiro
#'
#' @examples
#'
#' # to list files in tempdir() root
#' # list_tmp()
#'
#' # to list files of an extension
#' # list_tmp(".zip")
#'
#' @export
list_tmp <- function(ext = NULL, full = TRUE) {
  if (is.null(ext)) {
    f <- list.files(tempdir(), full.names = full)
  } else {
    f <- list.files(tempdir(), full.names = full)
    f <- f[grepl(ext, f, useBytes = TRUE)]
  }
  return(f)
}

#' @title Clean temporary files
#' @name clean_tmp
#'
#' @description Performs clean operations on temporary directory
#'
#' @param tmpdir the path to temporary directory
#'
#' @details \code{clean_tmp()} will delete all files stored in temporary
#'          directory. All extensioned files are remove, but not directories
#'          inside \code{tempdir()}.
#'
#' @seealso \itemize{
#'          \item{\code{tempfile()}}
#'          \item{\code{tempdir()}}
#'          }
#'
#' @return cleaned temporary directory
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' # clean_tmp()
#'
#' @export
clean_tmp <- function(tmpdir = tempdir()) {
  unlink(tmpdir, recursive = T)
  if (!file.exists(tempdir())) { dir.create(tempdir()) }
}
