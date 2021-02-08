
renv_files_list <- function(path         = getwd(),
                            pattern      = NULL,
                            all.files    = FALSE,
                            full.names   = FALSE,
                            recursive    = FALSE,
                            ignore.case  = FALSE,
                            include.dirs = FALSE)
{
  if (empty(path))
    return(character())

  # normalizePath('.') may fail on Windows, so fix up such paths now
  path <- if (identical(path, "."))
    getwd()
  else if (substring(path, 1L, 2L) %in% c("./", ".\\"))
    paste(getwd(), substring(path, 3L), sep = "/")
  else
    path

  children <- map(
    path,
    renv_files_list_impl,
    pattern      = pattern,
    all.files    = all.files,
    full.names   = full.names,
    recursive    = recursive,
    ignore.case  = ignore.case,
    include.dirs = include.dirs
  )

  unlist(children, recursive = TRUE, use.names = FALSE)
}

renv_files_list_impl <- function(path         = getwd(),
                                 pattern      = NULL,
                                 all.files    = FALSE,
                                 full.names   = FALSE,
                                 recursive    = FALSE,
                                 ignore.case  = FALSE,
                                 include.dirs = FALSE)
{
  children <- renv_files_list_impl_one(
    path         = path,
    all.files    = all.files,
    recursive    = recursive,
    ignore.case  = ignore.case
  )

  if (empty(children))
    return(character())

  if (!is.null(pattern)) {

    matched <- grep(
      pattern = pattern,
      x = basename(children),
      ignore.case = ignore.case
    )

    children <- children[matched]

  }

  if (recursive && !include.dirs) {
    paths <- paste(path, children, sep = "/")
    info <- file.info(paths, extra_cols = FALSE)
    children <- children[info$isdir %in% FALSE]
  }

  if (empty(children))
    children
  else if (full.names)
    paste(path, children, sep = "/")
  else
    children

}

renv_files_list_impl_one <- function(path        = getwd(),
                                     all.files   = FALSE,
                                     recursive   = FALSE,
                                     ignore.case = FALSE)
{
  # get filenames of children in requested path
  children <- renv_files_list_impl_one_exec(path)

  # always drop '.', '..'
  children <- setdiff(children, c(".", ".."))

  # drop files starting with a '.' if all.files is FALSE
  if (!all.files)
    children <- children[substring(children, 1L, 1L) != "."]

  # check and see if we're done now
  if (empty(children))
    return(character())

  # recurse if requested
  if (recursive) {

    # find the sub-directories
    childpaths <- paste(path, children, sep = "/")
    info <- file.info(childpaths, extra_cols = FALSE)
    subdirs <- children[info$isdir %in% TRUE]

    # list files in each sub-directory
    recursed <- uapply(subdirs, function(subdir) {

      # get filenames in sub-folder path
      subfiles <- renv_files_list_impl_one(
        path        = paste(path, subdir, sep = "/"),
        all.files   = all.files,
        recursive   = recursive,
        ignore.case = ignore.case
      )

      if (empty(subfiles))
        return(character())

      # return paths relative to root requested path
      paste(subdir, subfiles, sep = "/")

    })

    # add to whole file list
    children <- c(children, recursed)

  }

  # return listed files
  children

}

renv_files_list_impl_one_exec <- function(path) {
  renv_methods_error()
}

renv_files_list_impl_one_exec_unix <- function(path) {
  list.files(path, all.files = TRUE, no.. = TRUE)
}

# nocov start
renv_files_list_impl_one_exec_win32 <- function(path) {

  # first, try a plain list.files call, to see if we can get away with that
  # technically, the path must be translatable to the native encoding, but
  # easier to just only handle paths that are marked with 'unknown' encoding
  if (Encoding(path) == "unknown") {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (!any(grepl("?", files, fixed = TRUE)))
      return(files)
  }

  # check that the requested path is actually a directory
  info <- file.info(path, extra_cols = FALSE)
  if (!identical(info$isdir, TRUE))
    return(character())

  # otherwise, try some madness ...
  #
  # change working directory (done just to avoid encoding issues
  # when submitting path to cmd shell)
  owd <- setwd(path)
  on.exit(setwd(owd), add = TRUE)

  # NOTE: a sub-shell is required here in some contexts; e.g. when running
  # tests non-interactively or building in the RStudio pane
  command <- paste(comspec(), "/U /C dir /B")
  conn <- pipe(command, open = "rb", encoding = "native.enc")
  on.exit(close(conn), add = TRUE)

  # read binary output from connection
  output <- stack()

  while (TRUE) {

    data <- readBin(conn, what = "raw", n = 1024L)
    if (empty(data))
      break

    output$push(data)

  }

  # join into single raw vector
  encoded <- unlist(output$data(), recursive = FALSE, use.names = FALSE)
  if (is.null(encoded))
    return(character())

  # convert raw data (encoded as UTF-16LE) to UTF-8
  converted <- iconv(list(encoded), from = "UTF-16LE", to = "UTF-8")

  # split on (Windows) newlines
  paths <- strsplit(converted, "\r\n", fixed = TRUE)[[1]]

  # just in case?
  paths[nzchar(paths)]

}
# nocov end
