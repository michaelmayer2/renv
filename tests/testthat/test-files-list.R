
context("Files List")

test_that("we can list files in paths with UTF-8 characters", {

  # use 'demon' root
  root <- paste(tempdir(), "\u9b3c", sep = "/")
  renv_tests_scope(dir = root)

  # create some files
  entries <- list(

    list(
      path = "dir1/file1.R",
      contents = "library(bread)"
    ),

    list(
      path = "dir2/file2.R",
      contents = "library(oatmeal)"
    ),

    list(
      path = "\u{9b3c}/\u{9b3c}.R",
      contents = "library(toast)"
    )

  )

  map(entries, function(entry) {
    ensure_parent_directory(entry$path)
    writeLines(entry$contents, con = entry$path)
  })

  # try listing some files
  actual <- renv_files_list(recursive = TRUE)
  expected <- map_chr(entries, `[[`, "path")
  expect_setequal(setdiff(actual, "dependencies.R"), expected)

  # exercise renv a bit
  init()

  # check that the expected packages were installed
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))
  expect_true(renv_package_installed("oatmeal"))

})
