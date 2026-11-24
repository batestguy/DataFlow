db <- tools::CRAN_package_db()
for (p in c("triageR", "TrialFlowR", "flowchart", "ggnext", "autocodebook", "consort")) cat("\n==", p, ":", gsub("[[:space:]]+", " ", db$Description[db$Package == p]), "\n")
d <- file.path(tempdir(), "sel"); dir.create(d)
f <- download.packages("selecta", d, repos = "https://cloud.r-project.org", type = "source", quiet = TRUE)[1, 2]
untar(f, exdir = d)
fs <- list.files(file.path(d, "selecta"), recursive = TRUE, full.names = TRUE, pattern = "[.](R|Rmd|md|Rd)$")
tt <- unlist(lapply(fs, readLines, warn = FALSE))
cat("\nselecta text lines:", length(tt), "\n")
for (w in c("alt text|alt_text|alternative text|accessib|screen reader", "cluster", "bracket", "TRIPOD", "training set|test set|train/test", "cross-validation|fold", "outcome event|events", "ggplot", "CONSORT 2025|CONSORT 2010", "PRISMA 2020", "STARD 2015")) {
  m <- grep(w, tt, ignore.case = TRUE, value = TRUE)
  cat("\n[", w, "]", length(m), "\n"); cat(head(trimws(m), 4), sep = "\n")
}
print(list.files(file.path(d, "selecta", "vignettes")))
cat(grep("export", readLines(file.path(d, "selecta", "NAMESPACE")), value = TRUE), sep = " ")
