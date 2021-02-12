
renv_cache_migrate_init <- function() {

  # don't do anything during package build
  if (!is.na(Sys.getenv("R_INSTALL_PKG", unset = NA)))
    return()

  # don't do anything during a devtools::load_all
  calls <- sys.calls()
  for (call in calls)
    if (identical(call[[1L]], quote(load_all)) ||
        identical(call[[1L]], quote(devtools::load_all)))
      return()

  # if RENV_PATHS_PREFIX is set, nothing to do
  prefix <- Sys.getenv("RENV_PATHS_PREFIX", unset = NA)
  if (!is.na(prefix))
    return()

  # compute 'old' cache path
  old <- local({
    renv_scope_options(renv.paths.prefix = "")
    renv_paths_cache()
  })

  # compute 'new' cache path
  new <- local({
    renv_scope_options(renv.paths.prefix = NULL)
    renv_paths_cache()
  })

  # migrate only if the old cache path exists but the new does not
  migrate <- file.exists(old) && !file.exists(new)
  if (!migrate)
    return()

  # trim off platform-specific part of path
  old <- dirname(old)
  new <- dirname(new)

  # get symlink path (unix only)
  target <- new
  if (!renv_platform_windows()) {
    parts <- strsplit(new, "[/\\]")[[1L]]
    target <- paste(tail(parts, n = 2L), collapse = "/")
  }

  vprintf("Migrating cache ... ")
  ensure_parent_directory(new)
  renv_file_move(old, new)
  renv_file_link(target, old)
  vwritef("Done!")

  invisible(list(old = old, new = new))

}

