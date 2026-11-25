# Which CRAN packages say they draw cross-validation or resampling schemes?
# Rscript --vanilla notes/fold-grid-scan.R > notes/fold-grid-scan-output.txt
# A search of titles and descriptions only: packages whose description does
# not use these words are missed (timetk, for example, may be).
options(repos = "https://cloud.r-project.org")
d <- tools::CRAN_package_db()
d <- d[!duplicated(d$Package), ]
t <- gsub("[[:space:]]+", " ", paste(d$Title, d$Description))
draw <- "(visuali[sz]|plot|draw|diagram)"
scheme <- "(fold|cross-?validation|resampl|rolling[- ]origin|backtest)"
hit <- grepl(paste0(draw, "[^.]{0,60}", scheme, "|", scheme, "[^.]{0,60}", draw), t,
             ignore.case = TRUE) &
  grepl("cross-?validation|rolling[- ]origin|backtest", t, ignore.case = TRUE)
cat("packages:", nrow(d), "; hits:", sum(hit), "\n\n")
for (i in which(hit)) cat(d$Package[i], d$Version[i], "|", substr(d$Title[i], 1, 90), "\n")

# triageR: is there any function that draws a participant flow?
.libPaths(c(normalizePath("dev/complib"), .libPaths()))
ns <- asNamespace("triageR")
cat("\ntriageR", as.character(utils::packageVersion("triageR")), "functions mentioning flow, participant or diagram:\n")
for (f in ls(ns, all.names = TRUE)) {
  g <- get(f, ns)
  if (!is.function(g)) next
  m <- grep("flow ?(chart|diagram)|participant|diagram", deparse(g), value = TRUE, ignore.case = TRUE)
  if (length(m)) cat(" ", f, ":", substr(trimws(m[1]), 1, 100), "\n")
}
cat("(end)\n")
for (p in c("timetk", "spatialsample", "mlr3spatiotempcv", "blockCV"))
  cat(p, as.character(utils::packageVersion(p)), ":",
      paste(grep("autoplot|plot_time_series_cv|cv_plot", c(getNamespaceExports(p), ls(asNamespace(p))), value = TRUE), collapse = " "), "\n")
