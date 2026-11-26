# Draw the extended and edge textbook designs of families A to F (milestone
# 6b, step 5; 39 designs).
#
# Run from the package folder:
#   Rscript --vanilla notes/textbook-ext-AF.R <folder for the blind readers>
# Written after the answer keys (notes/textbook-ext-keys-AF.R), as
# notes/textbook-core.R was for the core designs. Version "p" is plain use of
# the verbs, now including flow_copy() where the same units go into every
# branch. Version "c" (only where wording could change what a reader
# understands) is careful wording with the same verbs. No package code is
# changed. The word "randomised" or a random method is used only where the
# help page states it (notes/textbook-ext-keys-AF.csv); a silent source gets
# neutral words ("received").
#
# Output: coded pictures and descriptions for the readers, and the list of
# codes in notes/textbook-ext-codes-AF.csv (never shown to a reader). Set P
# holds one version of every design; set Q the other version of those that
# have two; text/descriptions.txt the descriptions of the set-P versions, as
# U01.. (same number as the P code). flow_check() of every flow is printed.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
suppressMessages(pkgload::load_all(quiet = TRUE, export_all = FALSE))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_page <-", src) - 1)]))  # get_data()
out_dir <- commandArgs(TRUE)[1]
stopifnot(!is.na(out_dir))
for (s in c("P", "Q", "text")) dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)

D <- list()
# warnings are caught per design, so that they can be read with its id
catching <- function(id, expr) {
  withCallingHandlers(expr, warning = function(w) {
    cat(id, "warning:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  })
}
add <- function(id, p, c = NULL) {
  D[[id]] <<- list(p = catching(id, p), c = catching(id, c))
}
rng <- function(x) if (min(x) == max(x)) as.character(min(x)) else paste(min(x), "to", max(x))

# counts taken from the data, so that none is typed from memory ----------------
dm <- as.data.frame(get_data("agridat::darwin.maize"))
dm_pairs <- nrow(unique(dm[c("pot", "pair")]))
dm_pp <- table(unique(dm[c("pot", "pair")])$pot)
measurements <- as.data.frame(get_data("datasets::sleep"))
measurements$drug <- paste("Drug", measurements$group)
wr <- get_data("ISwR::wright")
ge <- get_data("MASS::gehan")
ge_n <- as.integer(table(ge$treat)[c("6-MP", "control")])
ge_ev <- as.integer(tapply(ge$cens == 1, ge$treat, sum)[c("6-MP", "control")])
db <- get_data("survival::diabetic")
db_pt <- db[!duplicated(db$id), ]
db_las <- as.integer(table(db_pt$laser)[c("xenon", "argon")])
db_ev <- tapply(db$status, list(db$laser, db$trt), sum)[c("xenon", "argon"), c("1", "0")]
an <- get_data("MASS::anorexia")
an_n <- as.integer(table(an$Treat)[c("CBT", "FT", "Cont")])
is <- get_data("datasets::InsectSprays")
is_n <- as.integer(table(is$spray))
ba <- get_data("carData::Baumann")
ba_n <- as.integer(table(ba$group)[c("Basal", "DRTA", "Strat")])
tg <- get_data("datasets::ToothGrowth")
tg_n <- table(tg$supp, tg$dose)[c("OJ", "VC"), ]
stopifnot(all(tg_n == 10))
fo <- get_data("HSAUR3::foster")
fo_t <- table(fo$litgen, fo$motgen)
ch <- get_data("daewr::chem")
stopifnot(nrow(unique(ch[c("A", "B", "C", "D")])) == 16)
ar <- as.data.frame(lapply(get_data("daewr::arso")[c("A", "B", "C", "D", "E", "F", "G")], function(v) as.numeric(as.character(v))))
stopifnot(with(ar, all(D == A * B, E == A * C, F == B * C, G == A * B * C)))
au <- get_data("daewr::augm")
au_n <- as.integer(table(au$fold)[c("original", "mirror")])
cf <- get_data("agridat::chinloy.fractionalfactorial")
stopifnot(with(cf, all((p + 2 * k + 2 * b + m) %% 3 == 0)), nrow(cf) == 81, all(table(cf$block) == 9))
ce <- get_data("daewr::cement")
ce_nz <- rowSums(abs(as.matrix(ce[c("x1", "x2", "x3")])) > 1e-8)
ce_kind <- table(ce$Block, factor(ce_nz, c(3, 1, 0)))
ce_alpha <- format(round(max(abs(as.matrix(ce[c("x1", "x2", "x3")]))), 2))
tr <- get_data("daewr::Treb")
tr_z <- rowSums(as.matrix(tr[c("x1", "x2", "x3")]) == 0)
tr_n <- c(sum(tr_z == 1), sum(tr_z == 3))
pe <- as.matrix(get_data("daewr::pest")[c("x1", "x2", "x3")])
pe_nz <- rowSums(pe > 1e-8)
pe_cen <- apply(abs(pe - 1 / 3) < 1e-3, 1, all)
pe_n <- c(sum(pe_nz == 1), sum(pe_nz == 2), sum(pe_cen), sum(pe_nz == 3 & !pe_cen))
stopifnot(sum(pe_n) == 13, all(abs(rowSums(pe) - 1) < 1e-3))
rc <- get_data("daewr::rcb")
rc_shots <- unique(as.integer(table(rc$id, rc$teehgt)))
stopifnot(length(rc_shots) == 1)
os <- get_data("datasets::OrchardSprays")
dv <- get_data("agridat::devries.pine")
bib <- get_data("agridat::cochran.bib")
pb <- get_data("SASmixed::PBIB")
pb_m <- unclass(table(pb$Block, pb$Treatment)) > 0
pb_cp <- crossprod(pb_m)[upper.tri(crossprod(pb_m))]
pb_met <- c(sum(pb_cp == 1), sum(pb_cp == 0))
stopifnot(sum(pb_cp > 1) == 0)
la <- get_data("agridat::cochran.lattice")
ja <- get_data("agridat::john.alpha")
km <- get_data("agridat::kempton.rowcol")
km_rep <- as.integer(table(km$rep))
km_once <- sum(table(km$gen) == 1)
# one genotype missing in each replicate, not the same one
stopifnot(km_rep == 34, km_once == 2, length(unique(names(which(table(km$gen) == 1)))) == 2,
          all(table(km$rep, km$gen)[, table(km$gen) == 1] %in% 0:1), all(colSums(table(km$rep, km$gen)[, table(km$gen) == 1]) == 1),
          all(rowSums(table(km$rep, km$gen)[, table(km$gen) == 1] == 0) == 1))
kl <- get_data("agridat::kling.augmented")
kl_t <- table(kl$gen)
kl_ck <- names(kl_t)[kl_t > 1]
kl_n <- c(sum(kl_t[kl_ck]), sum(kl_t == 1))
kl_pb <- as.integer(table(kl$block))
ss <- get_data("agridat::gomez.splitsplit")
ss_nit <- sort(unique(ss$nitro))
ss_man <- levels(factor(ss$management))
ss_gen <- levels(factor(ss$gen))
sp <- get_data("agridat::gomez.stripsplitplot")
sp_gen <- levels(factor(sp$gen))
sp_nit <- sort(unique(sp$nitro))
su <- get_data("agridat::gomez.splitplot.subsample")
su_man <- levels(factor(su$manage))
su_tim <- levels(factor(su$time))
gg <- get_data("agridat::gomez.groupsplit")
gg_vg <- unique(as.integer(table(unique(gg[c("group", "gen")])$group)))
stopifnot(length(gg_vg) == 1)
pl <- get_data("daewr::plasma")
sa <- get_data("daewr::sausage")
records <- as.data.frame(get_data("daewr::strung"))
records$cow <- paste(records$Diet, records$Cow)
records$Week <- paste("Week", records$week)
po <- get_data("daewr::polymer")
po_lots <- nlevels(factor(po$lot))
sn <- get_data("GAD::snails")
pn <- get_data("lme4::Penicillin")
ma <- get_data("nlme::Machines")
ma_runs <- unique(as.integer(table(ma$Worker, ma$Machine)))
jt <- get_data("AMCP::chapter_11_table_19")
f11 <- get_data("AMCP::chapter_10_table_5")
f11_cell <- unique(as.integer(table(f11$A, f11$B)))

# A ------------------------------------------------------------------------------
add("A06",
  flow_start("Pairs of seeds", n = dm_pairs, note = paste("in", length(dm_pp), "pots")) |>
    flow_copy("Parentage", c("Cross-fertilised", "Self-fertilised")) |>
    flow_outcome("Measured", "Height of the plant"),
  flow_start("Pots", n = length(dm_pp)) |>
    flow_apply("Pairs of seeds", paste(rng(dm_pp), "pairs per pot"), n = dm_pairs) |>
    flow_copy("Seed in each pair", c("Cross-fertilised:\none seed of every pair", "Self-fertilised:\nthe other seed")) |>
    flow_outcome("Measured", "Height of the plant", n = dm_pairs))
add("A07", flow_start("Patients", data = measurements, count = "ID") |>
  flow_copy("Drug given", by = "drug") |>
  flow_outcome("Measured", "Extra hours of sleep"))
add("A08", flow_start("Subjects", n = nrow(wr)) |>
  flow_copy("Peak-flow meter", c("Large Wright meter", "Mini Wright meter")) |>
  flow_outcome("Measured", "Peak expiratory flow"))
add("A09",
  flow_start("Patients with leukaemia", n = sum(ge_n), note = paste(nlevels(factor(ge$pair)), "matched pairs")) |>
    flow_split("Treatment received", c("6-MP", "Control"), n = ge_n) |>
    flow_outcome("Followed", "Remission ended", n = ge_n, events = ge_ev),
  flow_start("Matched pairs of patients", n = nlevels(factor(ge$pair))) |>
    flow_split("Within each pair", c("One patient: 6-MP", "The other patient:\ncontrol"), n = ge_n,
               note = "patients, one per pair") |>
    flow_outcome("Followed until either\nof the pair relapsed", "Remission ended", n = ge_n, events = ge_ev))
add("A10",
  flow_start("Patients with high-risk retinopathy in the study") |>
    flow_select("Sample", "Patients", n = nrow(db_pt), method = "random sample, half") |>
    flow_split("Laser type", c("Xenon", "Argon"), n = db_las) |>
    flow_copy("Eye, randomised within patient", c("Laser eye", "Untreated eye")) |>
    flow_outcome("Followed", "Visual loss", n = rep(db_las, each = 2), events = as.integer(t(db_ev))),
  flow_start("Patients with high-risk retinopathy in the study") |>
    flow_select("Sample", "Patients", n = nrow(db_pt), method = "random sample, half") |>
    flow_split("Laser type (how chosen not stated)", c("Xenon", "Argon"), n = db_las) |>
    flow_copy("Both eyes of every patient", c("One eye, chosen at\nrandom: laser", "The other eye:\nno treatment")) |>
    flow_outcome("Followed", "Visual loss of the eye", n = rep(db_las, each = 2), events = as.integer(t(db_ev))))
# B
add("B04", flow_start("Young women with anorexia", n = nrow(an)) |>
  flow_apply("Before", "Weight", n = nrow(an)) |>
  flow_split("Treatment received", c("Cognitive behavioural", "Family therapy", "Control"), n = an_n) |>
  flow_outcome("After", "Weight", n = an_n))
add("B06", flow_start("Experimental units", n = nrow(is)) |>
  flow_split("Spray received", paste("Spray", levels(is$spray)), n = is_n) |>
  flow_outcome("Measured", "Insect count"))
add("B07", flow_start("Students", n = nrow(ba)) |>
  flow_apply("Before", "2 pretests", n = nrow(ba)) |>
  flow_assign("Teaching method", c("Basal", "DRTA", "Strat"), n = ba_n) |>
  flow_outcome("After", "3 post-tests,\nthe same students", n = ba_n))
# C
add("C05",
  flow_start("Guinea pigs", n = nrow(tg)) |>
    flow_split("Supplement received", c("Orange juice", "Ascorbic acid"), n = as.integer(rowSums(tg_n))) |>
    flow_split("Dose received (mg/day)", c("0.5", "1", "2"), n = 10) |>
    flow_outcome("Measured", "Odontoblast length"),
  flow_start("Guinea pigs", n = nrow(tg)) |>
    flow_split("Supplement by dose (2 x 3, crossed)",
      paste0(rep(c("Orange juice,\n", "Ascorbic acid,\n"), each = 3), c("0.5", "1", "2"), " mg/day"), n = 10) |>
    flow_outcome("Measured", "Odontoblast length,\nonce per animal"))
add("C06",
  flow_start("Litters", n = nrow(fo)) |>
    flow_split("Genotype of the litter", rownames(fo_t), n = as.integer(rowSums(fo_t))) |>
    flow_split("Genotype of the foster mother", colnames(fo_t), n = lapply(1:4, function(i) as.integer(fo_t[i, ]))) |>
    flow_outcome("Measured", "Litter\nweight"),
  flow_start("Litters", n = nrow(fo), note = "unequal numbers in the 16 pairs of genotypes") |>
    flow_split("Genotype of the litter (observed)", paste("Litter", rownames(fo_t)), n = as.integer(rowSums(fo_t))) |>
    flow_split("Foster mother: the same 4 genotypes under every litter genotype (crossed)",
               paste("Mother", colnames(fo_t)), n = lapply(1:4, function(i) as.integer(fo_t[i, ]))) |>
    flow_outcome("Measured", "Litter\nweight"))
add("C07",
  flow_start("Runs", n = nrow(ch)) |>
    flow_split("Factor A", c("A low", "A high"), n = 8) |> flow_split("Factor B", c("B low", "B high"), n = 4) |>
    flow_split("Factor C", c("C low", "C high"), n = 2) |> flow_split("Factor D", c("D low", "D high"), n = 1) |>
    flow_outcome("Measured", "y"),
  flow_start("Runs", n = nrow(ch)) |>
    flow_apply("Combinations", "4 factors (A, B, C, D), 2 levels each,\ncrossed: all 16 combinations,\none run each (no repeats)", n = nrow(ch)) |>
    flow_outcome("Measured", "Response of the run", n = nrow(ch)))
add("C08",
  flow_start("Combinations of 7 two-level factors", n = 2^7) |>
    flow_select("Fraction", "Combinations run", n = nrow(ar), method = "1/16 fraction") |>
    flow_outcome("Measured", "Arsenic removed"),
  flow_start("Combinations of 7 two-level factors (A to G)", n = 2^7) |>
    flow_exclude("Not run", n = 2^7 - nrow(ar)) |>
    flow_apply("Runs", "8 combinations chosen by design:\nD = AB, E = AC, F = BC, G = ABC;\neach level of each factor in 4 runs", n = nrow(ar)) |>
    flow_outcome("Measured", "Arsenic removed,\nonce per run", n = nrow(ar)))
add("C09",
  flow_start("Runs", n = nrow(au)) |>
    flow_split("Fraction", c("Original fraction", "Mirror image"), n = au_n) |>
    flow_outcome("Measured", "Arsenic removed"),
  flow_start(c("First fraction:\n8 of the 128 combinations", "Mirror image, added to it:\nevery sign reversed"),
             n = au_n, stage = "Fractions") |>
    flow_pool("Combined", "16 runs, analysed together", n = nrow(au)) |>
    flow_outcome("Measured", "Arsenic removed,\nonce per run", n = nrow(au)))
add("C10",
  flow_start("Combinations of 5 three-level factors", n = 3^5) |>
    flow_select("Fraction", "Combinations grown", n = nrow(cf), method = "1/3 fraction") |>
    flow_split("Block", levels(factor(cf$block)), n = as.integer(table(cf$block))) |>
    flow_outcome("Measured", "Yield\nof the\nplot"),
  flow_start("Combinations of 5 manures, 3 levels each", n = 3^5) |>
    flow_exclude("Not grown", n = 3^5 - nrow(cf)) |>
    flow_apply("Plots", "81 combinations, one plot each:\nthose with p + 2k + 2b + m = 0 (mod 3)", n = nrow(cf)) |>
    flow_apply("Blocks", "9 blocks of 9 plots, each holding\na different ninth of the fraction", n = nrow(cf)) |>
    flow_outcome("Measured", "Yield of the plot", n = nrow(cf)))
add("C11",
  flow_start("Runs", n = nrow(ce)) |>
    flow_split("Block", c("Block 1", "Block 2"), n = as.integer(table(ce$Block))) |>
    flow_split("Kind of point", list(c("Corner", "Centre"), c("Axial", "Centre")),
               n = list(as.integer(ce_kind[1, c(1, 3)]), as.integer(ce_kind[2, c(2, 3)]))) |>
    flow_outcome("Measured", "Workability"),
  flow_start("Runs", n = nrow(ce), note = "3 factors; each run is one point") |>
    flow_split("Block", c("Block 1", "Block 2"), n = as.integer(table(ce$Block))) |>
    flow_split("Kind of point",
               list(c("Corners:\n8 points (+-1, +-1, +-1)", "Centre (0, 0, 0),\nrun 3 times"),
                    c(paste0("Axial:\n6 points, +-", ce_alpha, "\non one axis"), "Centre (0, 0, 0),\nrun 3 times")),
               n = list(as.integer(ce_kind[1, c(1, 3)]), as.integer(ce_kind[2, c(2, 3)]))) |>
    flow_outcome("Measured", "Workability"))
add("C12",
  flow_start("Runs", n = nrow(tr)) |>
    flow_split("Kind of point", c("Edge midpoints", "Centre"), n = tr_n) |>
    flow_outcome("Measured", "Response"),
  flow_start("Runs", n = nrow(tr), note = "3 factors at 3 levels; no corner points") |>
    flow_split("Kind of point", c("12 edge midpoints:\ntwo factors at +-1,\nthe third at 0", "Centre (0, 0, 0),\nrun 3 times"), n = tr_n) |>
    flow_outcome("Measured", "Response"))
add("C13",
  flow_start("Runs", n = nrow(pe)) |>
    flow_split("Kind of blend", c("Pure component", "Two-part blend", "Centroid", "Other 3-part blends"), n = pe_n) |>
    flow_outcome("Measured", "Response"),
  flow_start("Runs", n = nrow(pe), note = "each run is a blend of 3 components\nwhose parts add up to 1") |>
    flow_split("Kind of blend", c("Pure component:\n(1, 0, 0) and so on", "Two parts:\n(1/2, 1/2, 0) and so on",
                                  "Centroid:\n(1/3, 1/3, 1/3)", "Other 3-part blends,\ne.g. (0.8, 0.1, 0.1)"), n = pe_n) |>
    flow_outcome("Measured", "Response,\nonce per blend"))
# D
add("D04", flow_start("Golfers", n = nlevels(rc$id)) |>
  flow_copy("Tee height", paste("Height", levels(rc$teehgt))) |>
  flow_apply("Shots", paste(rc_shots, "per golfer at this height"), n = rc_shots * nlevels(rc$id)) |>
  flow_outcome("Measured", "Carry distance\nof the shot", n = rc_shots * nlevels(rc$id)))
add("D06",
  flow_start("Cells of dry comb", n = nrow(os)) |>
    flow_split("Row", paste("Row", 1:8), n = as.integer(table(os$rowpos))) |>
    flow_apply("Solution", "8 solutions,\none cell each") |>
    flow_outcome("Measured", "Decrease\nin volume"),
  flow_start("Cells of dry comb", n = nrow(os), note = "8 rows by 8 columns") |>
    flow_split("Solution (Latin square)", c("A: most\nlime sulphur", "B", "C", "D", "E", "F", "G: least\nlime sulphur", "H: none"),
               n = as.integer(table(os$treatment)), note = "once in every row,\nonce in every column") |>
    flow_outcome("Measured", "Decrease\nin volume"))
add("D07",
  flow_start("Plots", n = nrow(dv)) |>
    flow_split("Block", paste("Block", 1:4), n = as.integer(table(dv$block))) |>
    flow_apply("Spacing and thinning", "3 spacings x 3 thinnings,\none plot each") |>
    flow_outcome("Measured", "Stem\nvolume"),
  flow_start("Blocks", n = nlevels(factor(dv$block)), note = "each 3 rows by 3 columns of plots") |>
    flow_apply("Plots", "9 per block", n = nrow(dv)) |>
    flow_apply("Spacing and thinning (Graeco-Latin square in each block)",
               "Each spacing once in every row and\ncolumn of its block, and so is each\nthinning; each of the 9 pairs once\nper block", n = nrow(dv)) |>
    flow_outcome("Measured", "Stem volume of the plot", n = nrow(dv)))
add("D09",
  flow_start("Locations (blocks)", n = nlevels(factor(bib$loc))) |>
    flow_apply("Plots", "4 per location,\none line each", n = nrow(bib)) |>
    flow_outcome("Measured", "Yield of the plot", n = nrow(bib)),
  flow_start("Locations (blocks)", n = nlevels(factor(bib$loc))) |>
    flow_apply("Lines", "4 of the 13 lines at each location;\neach line at 4 locations;\nevery pair of lines together once", n = nrow(bib)) |>
    flow_outcome("Measured", "Yield of the plot", n = nrow(bib)))
add("D10",
  flow_start("Blocks", n = nlevels(pb$Block)) |>
    flow_apply("Plots", "4 per block,\none treatment each", n = nrow(pb)) |>
    flow_outcome("Measured", "Response", n = nrow(pb)),
  flow_start("Blocks", n = nlevels(pb$Block)) |>
    flow_apply("Treatments", paste0("4 of the 15 in each block;\neach treatment in 4 blocks;\n",
                                    pb_met[1], " pairs of treatments share a block\nonce, ", pb_met[2], " pairs never"), n = nrow(pb)) |>
    flow_outcome("Measured", "Response of the plot", n = nrow(pb)))
add("D11",
  flow_start("Replicates", n = nlevels(factor(la$rep))) |>
    flow_apply("Plots", "16 per replicate,\n4 rows by 4 columns", n = nrow(la)) |>
    flow_apply("Treatments", "All 16, once per replicate", n = nrow(la)) |>
    flow_outcome("Measured", "Buds attacked (%)", n = nrow(la)),
  flow_start("Replicates", n = nlevels(factor(la$rep))) |>
    flow_apply("Plots", "16 per replicate,\n4 rows by 4 columns", n = nrow(la)) |>
    flow_apply("Treatments", "All 16, once per replicate; rows and\ncolumns are rearranged in each replicate\nso that every pair of treatments shares\na row once and a column once", n = nrow(la)) |>
    flow_outcome("Measured", "Buds attacked (%)", n = nrow(la)))
add("D12",
  flow_start("Replicates", n = nlevels(factor(ja$rep))) |>
    flow_apply("Incomplete blocks", "6 per replicate", n = nrow(unique(ja[c("rep", "block")]))) |>
    flow_apply("Plots", "4 per block,\none variety each", n = nrow(ja)) |>
    flow_outcome("Measured", "Dry matter yield", n = nrow(ja)),
  flow_start("Replicates", n = nlevels(factor(ja$rep))) |>
    flow_apply("Incomplete blocks", "6 per replicate", n = nrow(unique(ja[c("rep", "block")]))) |>
    flow_apply("Plots", "4 per block, one variety each;\nall 24 varieties once per replicate;\ntwo varieties share a block at most once", n = nrow(ja)) |>
    flow_outcome("Measured", "Dry matter yield", n = nrow(ja)))
add("D13",
  flow_start("Plots", n = nrow(km)) |>
    flow_split("Replicate", c("Replicate 1", "Replicate 2"), n = km_rep) |>
    flow_apply("Genotypes", "35 genotypes in 5 rows\nby 7 columns, one plot each;\none plot missing") |>
    flow_outcome("Measured", "Yield"),
  flow_start("Plot positions", n = 2 * 35, note = "2 replicates, each 5 rows by 7 columns") |>
    flow_exclude("No plot in the data", n = 2 * 35 - nrow(km)) |>
    flow_split("Replicate", c("Replicate 1", "Replicate 2"), n = km_rep) |>
    flow_apply("Genotypes", "each genotype once per replicate,\nbut one missing in each;\nrows crossed with columns,\nboth inside the replicate") |>
    flow_outcome("Measured", "Yield"))
add("D14",
  flow_start("Plots", n = nrow(kl)) |>
    flow_split("Line", c("Check lines (3)", "New lines (50)"), n = kl_n) |>
    flow_apply("Blocks", c("each check once\nin each of 6 blocks", "each new line once,\nin one block")) |>
    flow_outcome("Measured", "Thousand\nseed weight"),
  flow_start("Blocks", n = nlevels(kl$block)) |>
    flow_apply("Plots", paste(rng(kl_pb), "per block"), n = nrow(kl)) |>
    flow_split("Line in the plot", c("3 checks: one plot each\nin every block", "50 new lines: one plot\neach, in one block"), n = kl_n) |>
    flow_outcome("Measured", "Thousand seed weight"))
# E
add("E03",
  flow_start("Blocks", n = 3) |> flow_apply("Whole plots", "5 per block", n = 15) |>
    flow_split("Nitrogen, applied to whole plots", paste(ss_nit, "kg/ha"), n = 3) |>
    flow_apply("Sub-plots", "3 per whole plot", n = 9) |>
    flow_split("Management, applied to sub-plots", ss_man, n = 3) |>
    flow_apply("Sub-sub-plots", "3 per sub-plot", n = 9) |>
    flow_split("Variety, applied to sub-sub-plots", ss_gen, n = 3) |>
    flow_outcome("Measured", "Yield"),
  flow_start("Blocks", n = 3) |> flow_apply("Whole plots", "5 per block", n = 15) |>
    flow_split("Nitrogen, applied to whole plots", paste(ss_nit, "kg/ha"), n = 3) |>
    flow_apply("Sub-plots", "3 per whole plot,\none management each:\nintensive, minimum, optimum", n = 9) |>
    flow_apply("Sub-sub-plots", "3 per sub-plot,\none variety each:\nV1, V2, V3", n = 27) |>
    flow_outcome("Measured", "Yield of the\nsub-sub-plot", n = 27))
add("E05",
  flow_start("Replicates", n = 3) |>
    flow_split("Genotype, horizontal strips", sp_gen, n = 3) |>
    flow_split("Nitrogen, vertical strips", paste(sp_nit, "kg"), n = 3) |>
    flow_split("Planting method, sub-plots", c("Broadcast", "Transplanted"), n = 3) |>
    flow_outcome("Measured", "Yield"),
  flow_start("Replicates", n = 3) |>
    flow_split("Strips, in each replicate", c("6 horizontal strips,\none genotype each", "3 vertical strips,\none nitrogen rate each"), n = c(18, 9)) |>
    flow_pool("Crossings", "Every genotype strip crosses every\nnitrogen strip of its replicate", n = 54) |>
    flow_apply("Sub-plots", "2 per crossing", n = 108) |>
    flow_split("Planting method, applied to sub-plots", c("Broadcast", "Transplanted"), n = 54) |>
    flow_outcome("Measured", "Yield of the sub-plot"))
add("E06",
  flow_start("Blocks", n = 3) |> flow_apply("Whole plots", "8 per block", n = 24) |>
    flow_split("Management, applied to whole plots", su_man, n = 3) |>
    flow_apply("Sub-plots", "4 per whole plot", n = 12) |>
    flow_split("Time of application, sub-plots", su_tim, n = 3) |>
    flow_apply("Samples", "2 hills per sub-plot", n = 6) |>
    flow_outcome("Measured", "Plant height\nof the hill"),
  flow_start("Blocks", n = 3) |> flow_apply("Whole plots", "8 per block", n = 24) |>
    flow_split("Management, applied to whole plots", su_man, n = 3) |>
    flow_apply("Sub-plots", "4 per whole\nplot, one time\nof application\neach", n = 12) |>
    flow_apply("Samples", "2 hills per\nsub-plot (not\nreplicates)", n = 24) |>
    flow_outcome("Measured", "Plant height\nof the hill"))
add("E07",
  flow_start("Replicates", n = 3) |> flow_apply("Whole plots", "2 per replicate", n = 6) |>
    flow_split("Fertiliser, whole plots", c("F1", "F2"), n = 3) |>
    flow_apply("Sub-plots", "3 per whole plot", n = 9) |>
    flow_split("Variety group, sub-plots", c("S1: under\n105 days", "S2: 105 to\n115 days", "S3: over\n115 days"), n = 3) |>
    flow_apply("Plots", paste(gg_vg, "per sub-plot,\none variety each"), n = 3 * gg_vg) |>
    flow_outcome("Measured", "Yield"),
  flow_start("Replicates", n = 3, note = "plot sizes read from the field positions,\nnot stated by the source") |>
    flow_apply("Whole plots", "2 per replicate", n = 6) |>
    flow_split("Fertiliser, whole plots", c("F1", "F2"), n = 3) |>
    flow_apply("Sub-plots", "3 per whole plot", n = 9) |>
    flow_split("Variety group, sub-plots", c("S1: under\n105 days", "S2: 105 to\n115 days", "S3: over\n115 days"), n = 3) |>
    flow_apply("Plots", paste(gg_vg, "per sub-plot, one\neach of the group's\nown", gg_vg, "varieties"), n = 3 * gg_vg) |>
    flow_outcome("Measured", "Yield"))
add("E08",
  flow_start("Runs", n = nrow(pl)) |>
    flow_apply("Combinations", "All 32 of 5 two-level\nfactors (A to E), one run each", n = nrow(pl)) |>
    flow_outcome("Measured", "Response", n = nrow(pl)),
  flow_start("Groups of runs (whole plots)", n = nrow(pl) / 4,
             note = "not marked in the data: in its row\norder every 4 runs share A, B and C") |>
    flow_apply("Runs", "4 per group: the 4 settings\nof D and E, one run each", n = nrow(pl)) |>
    flow_outcome("Measured", "Response, once per run", n = nrow(pl)))
ab <- c("A low, B low", "A high, B low", "A low, B high", "A high, B high")
add("E09",
  flow_start("Blocks", n = 2) |> flow_apply("Batches", "4 per block", n = 8) |>
    flow_split("A and B, applied to batches", ab, n = 2) |>
    flow_apply("Sub-units", "4 per batch", n = 8) |>
    flow_split("C and D, applied to sub-units", sub("B", "D", sub("A", "C", ab)), n = 2) |>
    flow_outcome("Measured", "Response"),
  flow_start("Blocks", n = 2) |> flow_apply("Batches (whole plots)", "4 per block", n = 8) |>
    flow_split("A and B, applied to batches", ab, n = 2) |>
    flow_apply("Sub-units", "4 per batch, one each\nof the 4 settings\nof C and D", n = 8) |>
    flow_outcome("Measured", "Response of the\nsub-unit", n = 8))
add("E10",
  flow_start("Cows", data = records, count = "cow") |>
    flow_split("Diet received", by = "Diet") |>
    flow_copy("Week", by = "Week") |>
    flow_outcome("Measured", "Milk protein"),
  flow_start("Cows", data = records, count = "cow", note = "cow numbers 1 to 10 repeat in\neach diet: 30 different cows") |>
    flow_split("Diet received", by = "Diet") |>
    flow_copy("Week (time, not assigned)", by = "Week") |>
    flow_outcome("Measured", "Milk protein"))
# F
add("F04",
  flow_start("Lots", n = po_lots) |>
    flow_split("Box", c("Box 1", "Box 2"), n = po_lots) |>
    flow_split("Preparation", list(c("Prep 1", "Prep 2"), "Prep 1"), n = po_lots) |>
    flow_split("Test", list(c("Test 1", "Test 2"), "Test 1", "Test 1"), n = po_lots) |>
    flow_outcome("Measured", "Strength"),
  flow_start("Lots", n = po_lots) |>
    flow_split("Box (2 from each lot)", c("Box 1 of\neach lot", "Box 2 of\neach lot"), n = po_lots, note = "boxes") |>
    flow_split("Preparation (staggered)", list(c("Prep 1 of\nbox 1", "Prep 2 of\nbox 1"), "Its only prep"), n = po_lots, note = "preparations") |>
    flow_split("Test (staggered)", list(c("Test 1", "Test 2"), "One test", "One test"), n = po_lots, note = "tests") |>
    flow_outcome("Measured", "Strength"))
add("F05",
  flow_start("Snails", n = nrow(sn)) |>
    flow_split("Origin", c("Origin 1", "Origin 2"), n = nrow(sn) / 2) |>
    flow_split("Shore", paste("Shore", 1:4), n = nrow(sn) / 8) |>
    flow_apply("Boulders", "3 per shore") |>
    flow_apply("Cages", "2 per boulder") |>
    flow_apply("Snails in cages", "5 per cage", n = nrow(sn) / 8) |>
    flow_outcome("Measured", "Growth"),
  flow_start("Shores", n = 4, note = "data are artificial") |>
    flow_apply("Boulders", "3 per shore", n = 12) |>
    flow_copy("Origin of the snails", c("Origin 1", "Origin 2")) |>
    flow_apply("Cages", "2 per boulder,\nfor this origin", n = 24) |>
    flow_apply("Snails", "5 per cage", n = nrow(sn) / 2) |>
    flow_outcome("Measured", "Growth of the snail", n = nrow(sn) / 2))
add("F07", flow_start("Plates", n = nlevels(pn$plate)) |>
  flow_copy("Penicillin sample", paste("Sample", levels(pn$sample))) |>
  flow_outcome("Measured", "Zone\ndiameter"))
add("F08", flow_start("Employees of the factory") |>
  flow_select("Workers", "Chosen", n = nlevels(ma$Worker), method = "random") |>
  flow_copy("Machine", paste("Machine", levels(ma$Machine))) |>
  flow_apply("Runs", paste(ma_runs, "per worker"), n = ma_runs * nlevels(ma$Worker)) |>
  flow_outcome("Measured", "Productivity score", n = ma_runs * nlevels(ma$Worker)))
add("F09", flow_start("Targets (people rated)", n = length(unique(jt$Subject)), note = "hypothetical data") |>
  flow_copy("Judge", paste("Judge", sort(unique(jt$Judge)))) |>
  flow_outcome("Measured", "Rating"))
add("F11",
  flow_start("Public high schools") |>
    flow_select("Schools", "Selected", n = 4, method = "random") |>
    flow_apply("Volunteers", "10 juniors per school", n = nrow(f11)) |>
    flow_split("School", paste("School", 1:4), n = 10) |>
    flow_assign("Study program", c("Computer-based", "Paper and pencil"), n = f11_cell) |>
    flow_outcome("Measured", "ACT score"),
  flow_start("Public high schools") |>
    flow_select("Schools", "Selected", n = 4, method = "random") |>
    flow_apply("Volunteers", "10 juniors per school", n = nrow(f11)) |>
    flow_assign("Study program, random within each school",
                c(paste0("Computer-based:\n", f11_cell, " per school"), paste0("Paper and pencil:\n", f11_cell, " per school")),
                n = nrow(f11) / 2) |>
    flow_outcome("Measured", "ACT score"))

# Checks -------------------------------------------------------------------------
keys <- utils::read.csv("notes/textbook-ext-keys-AF.csv", encoding = "UTF-8")
stopifnot(setequal(names(D), keys$id), all(keys$agrees))
cat("\n## flow_check() of every flow\n")
for (id in names(D)) for (v in c("p", "c")) {
  x <- D[[id]][[v]]
  if (is.null(x)) next
  ch <- flow_check(x)
  if (nrow(ch) == 0) {
    cat(id, v, ": clean\n")
  } else {
    for (i in seq_len(nrow(ch)))
      cat(id, v, ":", ch$level[i], "|", ch$stage[i], "|", gsub("\n", " ", ch$message[i]), "\n")
  }
}

# Codes ----------------------------------------------------------------------------
old_seed <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
set.seed(20261126)
two <- names(D)[!vapply(D, function(x) is.null(x$c), TRUE)]
first <- setNames(rep("p", length(D)), names(D))
first[sample(two, floor(length(two) / 2))] <- "c"
codes <- rbind(
  data.frame(id = names(D), version = first[names(D)], set = "P"),
  data.frame(id = two, version = ifelse(first[two] == "p", "c", "p"), set = "Q"))
codes <- codes[sample(nrow(codes)), ]
if (is.null(old_seed)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old_seed, globalenv())
codes <- codes[order(codes$set), ]
codes$code <- paste0(codes$set, sprintf("%02d", ave(seq_len(nrow(codes)), codes$set, FUN = seq_along)))
codes$width <- codes$height <- NA_real_
for (i in seq_len(nrow(codes))) {
  x <- D[[codes$id[i]]][[codes$version[i]]]
  catching(codes$id[i], flow_save(x, file.path(out_dir, codes$set[i], paste0(codes$code[i], ".png")), dpi = 100))
  s <- flow_layout(x)$size
  codes$width[i] <- round(s[["width"]], 1); codes$height[i] <- round(s[["height"]], 1)
}
# descriptions: set P's version of every design, numbered as in set P
t <- codes[codes$set == "P", ]
txt <- unlist(lapply(seq_len(nrow(t)), function(i)
  c(paste0("### U", substr(t$code[i], 2, 3)), unclass(flow_describe(D[[t$id[i]]][[t$version[i]]])), "")))
writeLines(txt, file.path(out_dir, "text", "descriptions.txt"))
rownames(codes) <- NULL
utils::write.csv(codes[c("id", "version", "set", "code", "height", "width")], "notes/textbook-ext-codes-AF.csv", row.names = FALSE)
cat("\ndesigns", length(D), "; with two versions", length(two), "; pictures", nrow(codes), "\n")
w7 <- codes$width > 7
cat("wider than 7 inches:", paste(codes$id[w7], codes$version[w7], codes$width[w7], collapse = "; "), "\n")
