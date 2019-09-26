
allowed_files <- c("hw4.Rmd",
                  "hw4.md",
                  "README.md",
                  "wercker.yml",
                  "hw4.Rproj",
                  "hw4_whitelist.R",
                  "nasa.rds",
                  "hw4.css",
                  ".gitignore")

files <- dir()
disallowed_files <- files[!(files %in% allowed_files)]

if (length(disallowed_files != 0)) {
  cat("Disallowed files found:\n")
  cat("  (remove the following files from your repo)\n\n")

  for (file in disallowed_files)
    cat("*", file, "\n")

  quit("no", 1, FALSE)
}
