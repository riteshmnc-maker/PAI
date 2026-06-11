args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
app_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}

setwd(app_dir)

local_lib <- file.path(app_dir, "R_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

shiny::runApp(appDir = app_dir, host = "127.0.0.1", port = 3838, launch.browser = FALSE)
