# Grades for the 46 core textbook designs (milestone 6b, step 5).
# Run from the package folder: Rscript --vanilla notes/textbook-grades.R
# Reader answers: notes/textbook-blind-X.txt, -Y.txt (pictures), -T.txt (the
# written descriptions of the set X versions). Keys: notes/textbook-keys.csv.
# Grades were given by the author of the keys, comparing each answer with the
# key. T true, I incomplete, M misleading (a wrong unit, number or crossing).
# why: method = the picture does not say how units were assigned or chosen and
# the help page does.

codes <- utils::read.csv("notes/textbook-codes.csv")
designs <- utils::read.csv("notes/textbook-designs.csv", encoding = "UTF-8")

# pictures: everything not listed here is T
p <- list(
  X16 = c("I", "method"), X09 = c("I", "method"), X08 = c("I", "method"),
  X03 = c("I", "method"), X25 = c("I", "method"), X28 = c("I", "method"),
  X17 = c("I", "method"), X33 = c("I", "method"), X11 = c("I", "method"),
  X32 = c("I", "method"), X05 = c("I", "method"), X23 = c("I", "method"),
  X45 = c("I", "method"),
  X19 = c("M", "reader gave the material to boys; it was given to feet"),
  X20 = c("M", "columns of the Latin square are not shown: read as blocks by row"),
  X30 = c("M", "matching is not shown: read as two separate groups"),
  Y09 = c("I", "reader could not tell that each panellist tastes two"),
  X10 = c("I", "does not say that every pair of recipes meets equally often"),
  Y07 = c("I", "which combinations share a block is not shown"),
  X21 = c("I", "reader called education nested in sex and also crossed"),
  X04 = c("I", "number of nitrogen strips cannot be told"))
# descriptions: everything not listed here is T
t <- list(
  T19 = c("M", "reader gave the material to boys"),
  T20 = c("M", "columns not mentioned"),
  T29 = c("M", "n = 36 in each branch was read as 36 hills in all; there are 144"),
  T30 = c("M", "matching not mentioned"),
  T04 = c("I", "strip counts cannot be told"),
  T10 = c("I", "pair balance not said"))

g <- function(lst, code, j) vapply(code, function(k) if (is.null(lst[[k]])) c("T", "")[j] else lst[[k]][j], "")
codes$picture <- g(p, codes$code, 1)
codes$why_picture <- g(p, codes$code, 2)
tc <- ifelse(codes$set == "X", sub("X", "T", codes$code), NA)
codes$description <- ifelse(is.na(tc), NA, g(t, ifelse(is.na(tc), "none", tc), 1))
codes$why_description <- ifelse(is.na(tc), "", g(t, ifelse(is.na(tc), "none", tc), 2))
stopifnot(all(names(p) %in% codes$code), all(sub("T", "X", names(t)) %in% codes$code))
codes$predicted <- sub("[;,].*", "", designs$expect[match(codes$id, designs$id)])
codes <- codes[order(codes$id, codes$version), ]
utils::write.csv(codes, "notes/textbook-grades.csv", row.names = FALSE)

f <- function(x) table(factor(x, c("T", "I", "M")))
pl <- codes[codes$version == "p" | !(codes$id %in% codes$id[codes$version == "p"]), ]
rank <- c(T = 1, I = 2, M = 3)
best <- tapply(rank[codes$picture], codes$id, min)
cat("pictures drawn:", nrow(codes), "\n")
cat("plain version, 46 designs:\n"); print(f(pl$picture))
cat("best version of each design:\n"); print(f(names(rank)[best]))
cat("careful versions (14):\n"); print(f(codes$picture[codes$version == "c"]))
cat("plain versions of the same 14:\n"); print(f(pl$picture[pl$id %in% codes$id[codes$version == "c"]]))
cat("descriptions (46):\n"); print(f(codes$description[codes$set == "X"]))
cat("\nplain pictures, prediction (rows) against grade (columns):\n")
print(table(predicted = factor(pl$predicted, c("T", "I", "M")), grade = factor(pl$picture, c("T", "I", "M"))))
m <- unique(pl$id[pl$why_picture == "method"])
cat("\nthe only defect is the missing method:", length(m), "designs:", paste(m, collapse = " "), "\n")
cat("true if the method were shown (best version):", sum(best == 1) + sum(names(best)[best > 1] %in% m), "of 46\n")
cat("\nnot true in any version, other reasons:\n")
o <- codes[codes$picture != "T" & codes$id %in% names(best)[best > 1] & codes$why_picture != "method", ]
for (i in seq_len(nrow(o))) cat(" ", o$id[i], o$version[i], o$picture[i], o$why_picture[i], "\n")
cat("wider than 7 inches:", sum(codes$width > 7), "of", nrow(codes), "pictures\n")
