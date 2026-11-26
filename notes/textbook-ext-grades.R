# Grades for the 71 extended and edge textbook designs (milestone 6b, step 5).
# Run from the package folder:
#   Rscript --vanilla notes/textbook-ext-grades.R > notes/textbook-ext-grades-output.txt 2>&1
# Reader answers: notes/textbook-ext-blind-P.txt, -Q.txt (pictures, designs A
# to F), -R.txt, -S.txt (pictures, designs G to K), -U.txt (the written
# descriptions of set P, same number), -V.txt (descriptions of set R).
# Keys: notes/textbook-ext-keys-AF.csv, -GK.csv. Codes:
# notes/textbook-ext-codes-AF.csv, -GK.csv (version p plain, c careful words).
# Grades were given by a grader who wrote neither the keys nor the pictures,
# comparing each answer with the key and, where a grade was in doubt, looking
# at the picture. Same rules as notes/textbook-grades.R:
#   T true (all answers right), I incomplete (nothing wrong, something
#   missing; "cannot tell" where the picture really does not say), M
#   misleading (a reader is led to a wrong unit, number, crossing or error
#   term; one such answer makes it M).
#   "reader error": the picture plainly says otherwise; graded T, recorded.
#   When the key leaves something open, "not said" is right.
#   why "method" was kept from the core grades. No picture here lacks a
#   method the help page states. One design (H08) states it too precisely:
#   the random step is drawn under each clinic, the source does not say
#   whether randomisation was within clinic, and all three readers said it
#   was (cause "method too precise").
# Changed after the independent review (2026-11-26): P36, R15, S22 and V15
# from T to I.
# cause: likely cause of the fault. "7b units", "9 time", "7f shared groups",
# "crossing" (rows by columns, or factors drawn one under the other), "wording"
# (a careful version or a fuller label says it), "size", "description fault".

codes <- rbind(utils::read.csv("notes/textbook-ext-codes-AF.csv"),
               utils::read.csv("notes/textbook-ext-codes-GK.csv"))
designs <- utils::read.csv("notes/textbook-designs.csv", encoding = "UTF-8")
designs <- designs[designs$tier != "core", ]
stopifnot(nrow(designs) == 71, setequal(designs$id, codes$id))

# pictures: everything not listed here is T
p <- list(
  # set P (designs A to F)
  P02 = c("T", "tree draws the factors one under the other; reader still read a full crossing", ""),
  P04 = c("M", "rows and columns inside each block (Graeco-Latin) not shown: read as randomised complete blocks", "crossing"),
  P05 = c("I", "number of genotypes (35) not shown", "wording"),
  P09 = c("I", "that every pair of treatments shares a row once and a column once is not said", "crossing"),
  P26 = c("I", "number of factors (7), and that the two fractions together form a 2^(7-3), not said", "wording"),
  P30 = c("I", "origin crossed with boulder not shown: '3 per shore' under each origin leaves 12 or 24 boulders open; reader unsure whether a snail or a cage is measured", "crossing"),
  P31 = c("I", "pairs per pot (3, 3, 5, 4) not shown", "wording"),
  P36 = c("I", "number of varieties (45) not shown; reader wrote 135 plots, then 270 and 'unclear' (as U36)", "wording"),
  P39 = c("M", "split-plot (runs in whole plots) not shown: read as a completely randomised full factorial", "wording"),
  # set Q (designs A to F)
  Q03 = c("M", "n = 6 in '2 hills per sub-plot' boxes read as 6 hills per sub-plot: 576 hills, there are 192 (picture 49 inches wide)", "wording; size"),
  Q09 = c("I", "number of lines (13) and that every pair of lines meets once not shown", "wording"),
  Q11 = c("M", "columns of the Latin square not shown: read as blocks by row", "crossing"),
  Q14 = c("M", "number of treatments (15) not shown: read as complete blocks of 4 treatments", "wording"),
  Q20 = c("I", "reader unsure whether dose is nested in supplement or crossed (as core X21)", "crossing"),
  Q29 = c("M", "nitrogen drawn under each genotype: read as 54 nitrogen strips, there are 9", "crossing"),
  Q30 = c("T", "generators not shown; the key leaves them open (not on the help page)", ""),
  # set R (designs G to K)
  R07 = c("I", "firms not shown: number of firms and that workers share firms are missing", "7f shared groups"),
  R09 = c("M", "primary schools drawn inside secondary schools: read as a three-level hierarchy", "7f shared groups"),
  R14 = c("I", "number of trials (13,222) and the 278 missing trials not shown", "7b units"),
  R15 = c("I", "random step drawn under each clinic: reader says randomised within clinic; the source does not say", "method too precise"),
  R20 = c("M", "'72 trials per person' under each cue read as 72 per person in all (1,440 trials, there are 5,760); flanker not shown", "wording"),
  R30 = c("I", "number of measurements (26,796) and years per pupil not shown; reader error besides: 1,900 + 2,194 + 2,231 is 6,325, as drawn", "9 time"),
  # set S (designs G to K)
  S03 = c("I", "number of measurements (26,796) and years per pupil not shown", "9 time"),
  S07 = c("I", "reader called state, earner group and period nested and also crossed", "crossing"),
  S08 = c("I", "items (600) and number of trials not shown", "7b units"),
  S11 = c("M", "matching of day j of 1961 with day j of 1962 not shown: read as unpaired days", "wording"),
  S13 = c("M", "arms drawn as totals under all 13 trials: read as arms pooled across trials, not two arms inside each trial", "7b units"),
  S18 = c("I", "number of males (60) and the three experiments not shown", "7f shared groups"),
  S21 = c("I", "lecturers not shown: whether students and lecturers are crossed cannot be told", "7f shared groups"),
  S22 = c("I", "random step drawn under each clinic: reader says stratified by clinic; the source does not say", "method too precise"))

# descriptions: U01 to U39 describe P01 to P39, V01 to V32 describe R01 to R32;
# everything not listed here is T
d <- list(
  U04 = c("M", "rows and columns inside each block not said: read as randomised complete blocks", "crossing"),
  U05 = c("I", "number of genotypes (35) not said", "wording"),
  U09 = c("I", "reader could not tell whether the 4 x 4 replicates are Latin squares or the pair balance", "crossing"),
  U25 = c("I", "'Lots is split into Box 1 and Box 2': reader could not tell whether boxes sit inside lots; numbers of boxes and preparations not given", "description fault"),
  U26 = c("I", "factors not named or counted", "wording"),
  U30 = c("M", "read as 24 boulders (3 per shore per origin), there are 12; origin crossed with boulder not said", "crossing"),
  U31 = c("I", "pairs per pot not said", "wording"),
  U36 = c("I", "number of varieties (45) not said; reader could not tell whether varieties repeat across replicates", "wording"),
  U39 = c("M", "split-plot not said: read as a completely randomised full factorial", "wording"),
  V04 = c("I", "reader could not tell what one measurement is (counts per trial arm)", "description fault"),
  V07 = c("I", "firms not mentioned as units", "7f shared groups"),
  V09 = c("M", "primary schools described inside secondary schools", "7f shared groups"),
  V14 = c("I", "number of trials not given; person counts and item counts in one bracket", "description fault"),
  V15 = c("I", "'those in Clinic 1 are randomly assigned': reader says randomised within clinic; the source does not say", "method too precise"),
  V20 = c("I", "flanker not mentioned", "wording"),
  V30 = c("I", "number of measurements and years per pupil not stated", "9 time"),
  V31 = c("I", "whether the 5 hours lie inside each session is not clear", "description fault"))

stopifnot(all(names(p) %in% codes$code),
          all(sub("^U", "P", sub("^V", "R", names(d))) %in% codes$code))

g <- function(lst, code, j) vapply(code, function(k) if (is.null(lst[[k]])) c("T", "", "")[j] else lst[[k]][j], "", USE.NAMES = FALSE)
pics <- data.frame(id = codes$id, version = codes$version, set = codes$set,
                   code = codes$code, kind = "picture", width = codes$width)
pics$grade <- g(p, pics$code, 1); pics$why <- g(p, pics$code, 2); pics$cause <- g(p, pics$code, 3)
dc <- codes[codes$set %in% c("P", "R"), ]
desc <- data.frame(id = dc$id, version = dc$version,
                   set = ifelse(dc$set == "P", "U", "V"),
                   code = sub("^P", "U", sub("^R", "R", dc$code)), kind = "description", width = NA)
desc$code <- ifelse(desc$set == "V", sub("^R", "V", desc$code), desc$code)
desc$grade <- g(d, desc$code, 1); desc$why <- g(d, desc$code, 2); desc$cause <- g(d, desc$code, 3)
all <- rbind(pics, desc)
all$tier <- designs$tier[match(all$id, designs$id)]
all$predicted <- sub("[;,].*", "", designs$expect[match(all$id, designs$id)])
all <- all[order(all$kind != "picture", all$id, all$version), ]
out <- all[, c("id", "tier", "version", "set", "code", "kind", "grade", "why", "cause", "predicted")]
utils::write.csv(out, "notes/textbook-ext-grades.csv", row.names = FALSE)

f <- function(x) table(factor(x, c("T", "I", "M")))
rank <- c(T = 1, I = 2, M = 3)
pl <- pics[pics$version == "p" | !(pics$id %in% pics$id[pics$version == "p"]), ]
stopifnot(nrow(pl) == 71, !anyDuplicated(pl$id))
best <- tapply(rank[pics$grade], pics$id, min)
bestg <- names(rank)[best]; names(bestg) <- names(best)
two <- unique(pics$id[pics$version == "c" & pics$id %in% pics$id[pics$version == "p"]])
tier <- designs$tier[match(names(bestg), designs$id)]
pred <- sub("[;,].*", "", designs$expect[match(names(bestg), designs$id)])

cat("pictures drawn:", nrow(pics), "for", length(unique(pics$id)), "designs; none refused\n")
cat("only one version drawn:", sum(!(unique(pics$id) %in% two)), "designs (",
    sum(pics$version == "c" & !(pics$id %in% two)), "of them careful only )\n\n")
cat("plain picture of each design (71; careful where no plain was drawn):\n"); print(f(pl$grade))
cat("best version of each design (71):\n"); print(f(bestg))
cat("descriptions (71):\n"); print(f(desc$grade))
cat("\ndesigns with two versions (", length(two), "):\n", sep = "")
cat("  plain:\n"); print(f(pics$grade[pics$version == "p" & pics$id %in% two]))
cat("  careful:\n"); print(f(pics$grade[pics$version == "c" & pics$id %in% two]))
cat("\nby tier, best version:\n")
print(table(tier = tier, grade = factor(bestg, c("T", "I", "M"))))
cat("by tier, plain picture:\n")
print(table(tier = designs$tier[match(pl$id, designs$id)], grade = factor(pl$grade, c("T", "I", "M"))))
cat("by tier, descriptions:\n")
print(table(tier = designs$tier[match(desc$id, designs$id)], grade = factor(desc$grade, c("T", "I", "M"))))
cat("\nprediction from the list (rows) against best-version grade (columns):\n")
print(table(predicted = factor(pred, c("T", "I", "M", "R or M")), grade = factor(bestg, c("T", "I", "M"))))
cat("prediction against plain-picture grade:\n")
print(table(predicted = factor(sub("[;,].*", "", designs$expect[match(pl$id, designs$id)]), c("T", "I", "M", "R or M")),
            grade = factor(pl$grade, c("T", "I", "M"))))

cc <- function(x) { x <- x[x$grade != "T", ]; table(unlist(strsplit(x$cause, "; "))) }
cat("\ncauses of faults (a fault with two causes counts under both):\n")
cat("  all pictures (", sum(pics$grade != "T"), " not true):\n", sep = ""); print(cc(pics))
cat("  plain pictures (", sum(pl$grade != "T"), " not true):\n", sep = ""); print(cc(pl))
cat("  descriptions (", sum(desc$grade != "T"), " not true):\n", sep = ""); print(cc(desc))
cat("  misleading pictures and descriptions only:\n"); print(cc(all[all$grade == "M", ]))

cat("\nevery misleading grade:\n")
m <- all[all$grade == "M", ]
for (i in seq_len(nrow(m))) cat(sprintf("  %s %s %-11s %s: %s [%s]\n", m$id[i], m$version[i], m$kind[i], m$code[i], m$why[i], m$cause[i]))
cat("\nnot true in any picture version:\n")
for (k in names(bestg)[bestg != "T"]) {
  r <- pics[pics$id == k, ]
  cat(sprintf("  %s (%s): %s\n", k, bestg[[k]], paste(sprintf("%s %s %s", r$version, r$grade, r$why), collapse = " | ")))
}
cat("\nT with a note (reader error, or key left it open):\n")
n <- all[all$grade == "T" & all$why != "", ]
for (i in seq_len(nrow(n))) cat(sprintf("  %s %s %s: %s\n", n$id[i], n$version[i], n$code[i], n$why[i]))
cat("\nwider than 7 inches:", sum(pics$width > 7), "of", nrow(pics), "pictures; widest",
    pics$code[which.max(pics$width)], pics$id[which.max(pics$width)], max(pics$width), "inches\n")

# Faults in the written descriptions, found by the readers (sentences that are
# odd or wrong in themselves, whatever the grade). Checked against
# readers/{AF,GK}/text/descriptions.txt.
faults <- data.frame(
  code = c("V05", "V26", "V14", "U25", "V10", "U33", "U05", "U36", "V04", "V31"),
  says = c("'No limit (n = 71; )': an empty note leaves '; )'",
           "'Students (n = 2,972; )': the same empty note",
           "'Words (n = 20; up to 150 of the 300)': people and items counted in one bracket",
           "'Lots is split into Box 1 (n = 30) and Box 2 (n = 30)': every lot has both boxes; 'split' reads as dividing the lots",
           "the outcome sentence gives 'n = 1,721' (pupils) before '7,230 scores in all'",
           "'4 of the 15 in each block' does not say 15 of what",
           "the number of genotypes is never said",
           "the number of varieties (45) is never said",
           "what one recorded count belongs to (a trial arm) is not said",
           "whether the 5 hours lie inside each of the 3 sessions is not said"))
faults$id <- desc$id[match(faults$code, desc$code)]
faults$version <- desc$version[match(faults$code, desc$code)]
cat("\nfaults in the written descriptions found by the readers:\n")
for (i in seq_len(nrow(faults))) cat(sprintf("  %s %s (%s): %s\n", faults$id[i], faults$version[i], faults$code[i], faults$says[i]))
