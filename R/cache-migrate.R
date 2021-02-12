
renv_cache_migrate_init <- function() {

  # don't do anything during package build
  if (!is.na(Sys.getenv("R_INSTALL_PKG", unset = NA)))
    return()

  # if the user doesn't want a prefix, respect that
  empty <- Sys.getenv("RENV_PATHS_PREFIX_EMPTY", unset = NA)
  if (empty %in% c("true", "True", "TRUE", "1"))
    return()

  # if RENV_PATHS_PREFIX is set, nothing to do
  prefix <- Sys.getenv("RENV_PATHS_PREFIX", unset = NA)
  if (!is.na(prefix))
    return()

  renv_cache_migrate(check = TRUE)

}

renv_cache_migrate <- function(check = FALSE) {

  # compute 'old' cache path
  old <- local({

    renv_scope_envvars(
      RENV_PATHS_PREFIX = NULL,
      RENV_PATHS_PREFIX_EMPTY = TRUE
    )

    renv_paths_cache()

  })

  # compute 'new' cache path
  new <- local({

    renv_scope_envvars(
      RENV_PATHS_PREFIX = NULL,
      RENV_PATHS_PREFIX_EMPTY = NULL
    )

    renv_paths_cache()

  })

  if (!check) {

    if (!file.exists(old)) {
      fmt <- "old cache location %s does not exist; cannot proceed"
      stopf(fmt, renv_path_pretty(old))
    }

    if (file.exists(new)) {
      fmt <- "new cache location %s already exists; cannot proceed"
      stopf(fmt, renv_path_pretty(new))
    }

  }

  # migrate only if the old cache path exists but the new does not
  migrate <- file.exists(old) && !file.exists(new)
  if (!migrate)
    return()

  paths <- c(
    paste("Old:", renv_path_pretty(old)),
    paste("New:", renv_path_pretty(new))
  )

  if (check) {

    renv_pretty_print(
      values    = paths,
      preamble  = "The default renv cache path has changed in this version of renv:",
      postamble = c(
        "Use `renv:::renv_cache_migrate()` to migrate the cache to the new location.",
        "Alternatively, set the RENV_PATHS_PREFIX_EMPTY environment variable,",
        "or manually migrate the cache folder from the old to the new path.",
        "See `utils::news(package = \"renv\")` for more details."
      ),
      wrap = FALSE

    )

    return(FALSE)

  }

  renv_pretty_print(
    values = paths,
    preamble = "The renv cache will be copied from the old location to the new location:",
    wrap = FALSE
  )

  if (interactive() && !proceed()) {
    message("* Operation aborted.")
    return(FALSE)
  }

  vprintf("Copying cache ... ")
  renv_file_copy(old, new)
  vwritef("Done!")

  renv_pretty_print(
    values    = paste("-", renv_path_pretty(old)),
    preamble  = "Consider removing the old cache, located at:",
    postamble = "The cache should only be removed after all renv projects on your system have been updated to 0.13.0 or newer.",
    wrap = FALSE
  )

  invisible(list(old = old, new = new))

}

