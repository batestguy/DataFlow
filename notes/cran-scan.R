db <- tools::CRAN_package_db()
tx <- paste(db$Title, db$Description)
h <- grepl("CONSORT|PRISMA|STARD|STROBE|TRIPOD", tx) |
  grepl("participant flow|patient flow|flow diagram|flowchart|study flow", tx, ignore.case = TRUE)
x <- db[h, ]
cat(nrow(db), "packages;", nrow(x), "hits\n")
for (i in seq_len(nrow(x))) {
  t <- paste(x$Title[i], x$Description[i])
  g <- unique(regmatches(t, gregexpr("CONSORT|PRISMA|STARD|STROBE|TRIPOD", t))[[1]])
  cat(x$Package[i], x$Version[i], x$Published[i], "|", gsub("[[:space:]]+", " ", x$Title[i]), "|", paste(g, collapse = ","), "\n")
}
s <- db[db$Package %in% c("selecta", "stard"), ]
print(s[, c("Package", "Version", "Published", "Description", "Imports")])
