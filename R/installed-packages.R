
renv_installed_packages_base <- function() {

  # we can assume that the base set of installed packages won't change during
  # a session, so cache the result of installed.packages()
  renv_global("base.packages", {
    packages <- installed.packages(lib.loc = .Library, priority = "base")
    as.data.frame(packages, stringsAsFactors = FALSE)
  })

}

renv_installed_packages <- function(lib.loc = NULL,
                                    priority = NULL,
                                    ...)
{
  # installed.packages() will fail if lib.loc cannot be converted
  # to the native encoding, so we use our own homebrewed implementation here
  lib.loc <- lib.loc %||% renv_libpaths_all()
  bapply(lib.loc, function(library) {
    renv_installed_packages_impl(library, priority = priority, ...)
  })

}

renv_installed_packages_impl <- function(library  = NULL,
                                         priority = NULL,
                                         ...)
{
  # list files in library
  packages <- renv_files_list(library, full.names = TRUE)

  # keep only directories which have a DESCRIPTION file
  descpaths <- paste(packages, "DESCRIPTION", sep = "/")
  packages <- packages[file.exists(descpaths)]

  # read DESCRIPTION files
  descs <- map(packages, function(package) {

    # read DESCRIPTION file as character (so subsetting returns NA)
    desc <- renv_description_read(package)
    chr <- convert(desc, "character")

    # extract fields of interest (noting that we need to reset
    # names after extracting)
    fields <- renv_installed_packages_fields()
    entries <- chr[fields]
    names(entries) <- fields

    # keep only the first component of the built field
    built <- entries[["Built"]]
    index <- regexpr(";", built, useBytes = TRUE)
    if (!identical(c(index), -1L))
      entries[["Built"]] <- substring(built, 1L, index - 1L)

    # done
    entries

  })

  # tag with library path
  db <- bind_list(descs)
  db$LibPath <- library

  # done
  db

}

renv_installed_packages_fields <- function() {
  c(
    "Package", "Version", "Priority",
    "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
    "OS_type", "NeedsCompilation", "Built"
  )
}
