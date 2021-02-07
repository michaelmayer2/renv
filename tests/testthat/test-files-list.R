
context("Files List")

test_that("we can list files in paths with UTF-8 characters", {

  # use 'demon' root
  root <- paste(tempdir(), "\u9b3c", sep = "/")
  renv_tests_scope(dir = root)

  # create some files
  paths <- list(
    "dir1/file1.R"        = "library(bread)",
    "dir2/file2.R"        = "library(oatmeal)",
    "\u{9b3c}/\u{9b3c}.R" = "library(toast)"
  )

  enumerate(paths, function(path, contents) {
    ensure_parent_directory(path)
    writeLines(contents, con = path)
  })

  # try listing some files
  listed <- renv_files_list(recursive = TRUE)
  expect_setequal(names(paths), setdiff(listed, "dependencies.R"))

  # exercise renv a bit
  init()

  # check that the expected packages were installed
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))
  expect_true(renv_package_installed("oatmeal"))

})
