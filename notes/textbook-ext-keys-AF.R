# Answer keys for the extended and edge textbook designs of families A to F
# (milestone 6b, step 4; 39 designs).
#
# Run from the package folder:
#   Rscript --vanilla notes/textbook-ext-keys-AF.R > notes/textbook-ext-keys-AF-output.txt 2>&1
# Written BEFORE any of these designs was drawn. Writes
# notes/textbook-ext-keys-AF.csv (same columns as notes/textbook-keys.csv) and
# prints every check. Method as in notes/textbook-keys.R:
#   source    what the help page says about how units were assigned or chosen:
#             "stated", "partly" or "silent" (the books were not read)
#   expect    numbers the structure implies, worked out by arithmetic from the
#             numbers of units: error degrees of freedom, or, where those do
#             not make sense (fractions, response surfaces, mixtures), counts
#             of runs, points, blocks and which combinations are present
#   check     the same numbers computed from the data by a separate route
# A key is only accepted when expect and check agree. Agreement shows the key
# fits the data as coded. It does NOT show that the study was run that way.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_page <-", src) - 1)]))  # get_data()

# error df of every stratum of an aov with Error()
strata <- function(formula, data) {
  s <- summary(stats::aov(formula, data))
  out <- vapply(s, function(x) {
    t <- x[[1]]
    i <- grep("Residuals", rownames(t))
    if (length(i)) as.integer(round(t$Df[i])) else NA_integer_
  }, 1L)
  unname(out[!is.na(out)])
}
fac <- function(d, ...) {
  d <- as.data.frame(d)
  for (v in c(...)) d[[v]] <- factor(d[[v]])
  d
}
n_of <- function(d, ...) nrow(unique(as.data.frame(d)[c(...)]))
# how often each pair of treatments shares a block
pairs_met <- function(block, trt) {
  m <- unclass(table(block, trt)) > 0
  cp <- crossprod(m)
  cp[upper.tri(cp)]
}
# words of the defining relation of a two-level design (columns coded -1, 1):
# the sets of factors whose product is the same in every run
words2 <- function(d, cols) {
  X <- sapply(as.data.frame(d)[cols], function(v) as.numeric(as.character(v)))
  k <- length(cols)
  out <- integer()
  for (s in seq_len(2^k - 1)) {
    use <- bitwAnd(s, 2^(seq_len(k) - 1)) > 0
    p <- apply(X[, use, drop = FALSE], 1, prod)
    if (length(unique(p)) == 1) out <- c(out, sum(use))
  }
  out
}
# contrasts a.x (mod 3) of a three-level design that are constant over all
# runs (`within` NULL) or within every level of `within`
words3 <- function(X, within = NULL) {
  k <- ncol(X)
  A <- as.matrix(expand.grid(rep(list(0:2), k)))[-1, , drop = FALSE]
  g <- if (is.null(within)) rep(1, nrow(X)) else within
  keep <- apply(A, 1, function(a) {
    v <- (X %*% a) %% 3
    all(tapply(v, g, function(z) length(unique(z))) == 1)
  })
  A[keep, , drop = FALSE]
}

K <- list()
key <- function(id, data, source, assigned, measured, nested, crossed, repeats,
                limits, open, expect, check) {
  K[[length(K) + 1]] <<- list(
    id = id, data = data, source = source, assigned = assigned,
    measured = measured, nested = nested, crossed = crossed, repeats = repeats,
    limits = limits, open = open, expect = expect, check = substitute(check))
}
no <- "none"

# A. One and two groups ------------------------------------------------------------
key("A06", "agridat::darwin.maize", "partly", "nothing random stated; pairs of seeds, one cross- and one self-fertilised, planted in pots", "plant", "plants in pairs, pairs in pots", "type by pair", no,
    "15 pairs in 4 pots (3, 3, 5, 4 pairs)", "The help page says the pair factor has 12 levels; the data hold 15 (a to o). Whether the seed of each type within a pair was placed at random is not said",
    c(pairs = 15, pairs_one_of_each = 15, pair_error = 14),
    { t <- table(d$pair, d$type)
      c(nrow(t), sum(t[, "cross"] == 1 & t[, "self"] == 1), strata(height ~ type + Error(pair), fac(d, "pair", "type"))[2]) })
key("A07", "datasets::sleep", "partly", "both drugs given to each patient", "patient-drug", no, "patient by drug", "each patient measured under both drugs",
    "10 patients, not two groups of 10 (the help page says so)", "order of the drugs; how the patients were found",
    c(patients_with_both = 10, error = 9),
    { t <- table(d$ID, d$group); c(sum(rowSums(t > 0) == 2), strata(extra ~ group + Error(ID), d)[2]) })
key("A08", "ISwR::wright", "partly", "nothing; every subject measured with both meters", "subject-meter", no, "subject by meter", "each subject measured twice",
    "17 subjects", "order of the meters; how the subjects were found",
    c(subjects = 17, difference_error = 16),
    c(sum(stats::complete.cases(d)), stats::df.residual(stats::lm(I(std.wright - mini.wright) ~ 1, d))))
key("A09", "MASS::gehan", "partly", "6-MP or control to patient, within matched pairs", "patient", "patients in pairs", no, no,
    "21 pairs, one of each treatment; both withdrawn when either came out of remission", "whether the treatment within a pair was random; what the pairs were matched on",
    c(pairs = 21, pairs_one_of_each = 21, within_pair_error = 20),
    { t <- table(d$pair, d$treat)
      c(nrow(t), sum(t[, "6-MP"] == 1 & t[, "control"] == 1), strata(time ~ treat + Error(factor(pair)), d)[2]) })
key("A10", "survival::diabetic", "stated", "patients a 50% random sample; one eye of each randomised to laser, the other untreated", "eye", "eyes in patients", "treatment by patient", "both eyes of every patient followed",
    "197 patients, 394 eyes", "how the laser type (xenon or argon) was chosen for a patient",
    c(patients = 197, one_treated_one_not = 197, eyes = 394, laser_types_per_patient = 1, within_error = 196),
    { t <- table(d$id, d$trt)
      c(nrow(t), sum(t[, "0"] == 1 & t[, "1"] == 1), nrow(d), max(tapply(d$laser, d$id, function(v) length(unique(v)))),
        strata(time ~ factor(trt) + Error(factor(id)), d)[2]) })
# B. One factor, separate groups -------------------------------------------------
key("B04", "MASS::anorexia", "silent", "control, CBT or family therapy to patient", "patient, before and after", no, no,
    "each patient weighed twice", "unequal groups", "whether random; how patients were found",
    c(ancova_error = 68), c(stats::df.residual(stats::lm(Postwt ~ Prewt + Treat, d))))
key("B06", "datasets::InsectSprays", "silent", "spray to agricultural experimental unit", "unit", no, no, no,
    "12 units per spray", "whether random; what a unit was",
    c(error = 66), c(stats::df.residual(stats::lm(count ~ spray, d))))
key("B07", "carData::Baumann", "stated", "teaching method to student, random", "student, several tests", no, no,
    "each student takes 2 pretests and 3 post-tests", "22 per group", "",
    c(error = 63, smallest_group = 22, largest_group = 22, tests = 5),
    c(stats::df.residual(stats::lm(post.test.1 ~ group, d)), min(table(d$group)), max(table(d$group)), length(grep("test", names(d)))))
# C. Factorial ---------------------------------------------------------------------
key("C05", "datasets::ToothGrowth", "silent", "one of 3 doses by one of 2 supplements to each guinea pig", "animal", no, "supplement by dose, full", no,
    "10 per cell; doses ordered", "whether random",
    c(error = 54), c(stats::df.residual(stats::lm(len ~ supp * factor(dose), d))))
key("C06", "HSAUR3::foster", "silent", "litter to foster mother (genotypes of both observed)", "litter", no, "litter genotype by mother genotype", no,
    "unequal cells, 2 to 5", "how litters were given to mothers",
    c(cells = 16, error = 45),
    c(n_of(d, "litgen", "motgen"), stats::df.residual(stats::lm(weight ~ litgen * motgen, d))))
key("C07", "daewr::chem", "silent", "16 combinations of 4 two-level factors to runs", "run", no, "A by B by C by D, full", no,
    "one run per combination", "run order; whether it was random",
    c(distinct = 16, residual = 0),
    c(n_of(d, "A", "B", "C", "D"), stats::df.residual(stats::lm(y ~ A * B * C * D, d))))
key("C08", "daewr::arso", "partly", "8 combinations of 7 two-level factors chosen by design", "run", no, "7 factors, fraction", no,
    "1/16 of 128 combinations; each level of each factor in 4 runs",
    "The help page title says 2^(7-3) (16 runs), but the data hold 8 runs, which is 2^(7-4); the generators are not given on the page",
    c(runs = 8, distinct = 8, words = 15, shortest_word = 3, per_level = 4),
    { w <- words2(d, c("A", "B", "C", "D", "E", "F", "G"))
      c(nrow(d), n_of(d, "A", "B", "C", "D", "E", "F", "G"), length(w), min(w), unique(unlist(lapply(d[c("A", "B", "C", "D", "E", "F", "G")], table)))) })
key("C09", "daewr::augm", "partly", "a fraction, then its mirror image (every sign reversed)", "run", "runs in fractions", "7 factors, fraction", no,
    "8 + 8 runs; together a 2^(7-3)", "whether the mirror was run later (the page says augmented)",
    c(runs = 16, mirror_is_reversed = 1, words_first = 15, words_both = 7, shortest_word_both = 4),
    { f <- c("A", "B", "C", "D", "E", "F", "G")
      X <- sapply(d[f], function(v) as.numeric(as.character(v)))
      o <- X[d$fold == "original", ]; m <- X[d$fold == "mirror", ]
      same <- setequal(apply(-o, 1, paste, collapse = " "), apply(m, 1, paste, collapse = " "))
      w1 <- words2(d[d$fold == "original", ], f); w2 <- words2(d, f)
      c(nrow(d), as.integer(same), length(w1), length(w2), min(w2)) })
key("C10", "agridat::chinloy.fractionalfactorial", "partly", "81 of the 243 combinations of 5 three-level factors to plots, in 9 blocks", "plot", "plots in blocks", "n by p by k by b by m, a third", no,
    "1/3 fraction; 9 blocks of 9",
    "The page names neither the defining contrast nor the block contrasts. The key was first written expecting a five-letter defining word (resolution V) and disagreed; the data give p + 2k + 2b + m = 0 (mod 3), four letters, nitrogen not in it, and one component of the p by k interaction confounded with blocks. Corrected after the first run. Randomisation not on the page",
    # first run: shortest_defining = 5 expected, 4 found (see open)
    c(plots = 81, defining = 2, shortest_defining = 4, block_contrasts = 26, per_level = 27, per_block = 9),
    { X <- as.matrix(d[c("n", "p", "k", "b", "m")])
      w <- words3(X); wb <- words3(X, d$block)
      c(nrow(d), nrow(w), min(rowSums(w > 0)), nrow(wb), unique(unlist(lapply(d[c("n", "p", "k", "b", "m")], table))), unique(as.integer(table(d$block)))) })
key("C11", "daewr::cement", "partly", "points of a central composite design to runs, in 2 blocks", "run", "runs in blocks", "x1 by x2 by x3 at the corners", "centre point run 6 times",
    "corners and 3 centres in block 1; axial points and 3 centres in block 2", "run order",
    c(corners = 8, axial = 6, centre = 6, points = 15, block1 = 11, block2 = 9, alpha_x1000 = 1682),
    { X <- as.matrix(d[c("x1", "x2", "x3")]); nz <- rowSums(abs(X) > 1e-8)
      c(sum(nz == 3), sum(nz == 1), sum(nz == 0), n_of(round(X, 6), "x1", "x2", "x3"), sum(d$Block == 1), sum(d$Block == 2), round(1000 * max(abs(X)))) })
key("C12", "daewr::Treb", "partly", "points of a Box-Behnken design to runs", "run", no, no, "centre point run 3 times",
    "12 edge midpoints, 3 centres", "run order",
    c(edge = 12, centre = 3, points = 13, edge_points_one_zero = 12),
    { X <- as.matrix(d[c("x1", "x2", "x3")]); z <- rowSums(X == 0)
      c(sum(z < 3), sum(z == 3), n_of(d, "x1", "x2", "x3"), sum(z == 1)) })
key("C13", "daewr::pest", "silent", "13 blends of 3 components to runs", "run", no, no, no,
    "parts add up to 1 in every run", "The list calls it constrained, but every component runs from 0 to 1 in the data: the whole simplex is covered. Run order",
    c(runs = 13, sum_to_one = 13, pure = 3, two_part = 3, centroid = 1, other = 6, lowest = 0, highest = 1),
    { X <- as.matrix(d[c("x1", "x2", "x3")]); nz <- rowSums(X > 1e-8)
      cen <- apply(abs(X - 1 / 3) < 1e-3, 1, all)
      c(nrow(d), sum(abs(rowSums(X) - 1) < 1e-3), sum(nz == 1), sum(nz == 2), sum(cen), sum(nz == 3 & !cen),
        max(apply(X, 2, min)), min(apply(X, 2, max))) })
# D. Blocks ------------------------------------------------------------------------
key("D04", "daewr::rcb", "partly", "tee height to shot, within golfer", "shot", "shots in golfer-height cells", "golfer by tee height", "each golfer at every height, 5 shots each",
    "golfer is the block", "order of shots and heights",
    c(golfer_by_height = 16, shot = 108),
    { x <- fac(d, "id", "teehgt"); utils::tail(strata(cdistance ~ teehgt + Error(id / teehgt), x), 2) })
key("D06", "datasets::OrchardSprays", "partly", "solution to cell of comb, Latin square", "cell", no, "rows by columns", no,
    "8 x 8 Latin square", "how the square was chosen; whether a row or column was a chamber",
    c(error = 42, row_treatment_once = 64, column_treatment_once = 64),
    c(stats::df.residual(stats::lm(decrease ~ factor(rowpos) + factor(colpos) + treatment, d)),
      sum(table(d$rowpos, d$treatment) == 1), sum(table(d$colpos, d$treatment) == 1)))
key("D07", "agridat::devries.pine", "partly", "spacing and thinning to plot, Graeco-Latin square in each block", "plot", "plots in blocks", "rows by columns within block; spacing by thinning", no,
    "4 blocks (squares) of 3 x 3", "how the squares were chosen",
    c(error = 12, spacing_row = 36, spacing_col = 36, thinning_row = 36, thinning_col = 36, pair_per_block = 36),
    { x <- fac(d, "block", "row", "col")
      c(stats::df.residual(stats::lm(growth ~ block + block:row + col + spacing + thinning, x)),
        sum(table(interaction(x$block, x$row), x$spacing) == 1), sum(table(x$col, x$spacing) == 1),
        sum(table(interaction(x$block, x$row), x$thinning) == 1), sum(table(x$col, x$thinning) == 1),
        sum(table(x$block, interaction(x$spacing, x$thinning)) == 1)) })
key("D09", "agridat::cochran.bib", "partly", "4 of 13 lines to each location (block)", "plot", "plots in locations", "line by location, incomplete", no,
    "each line at 4 locations; every pair of lines together once", "randomisation within location",
    c(error = 27, pair_min = 1, pair_max = 1),
    { p <- pairs_met(d$loc, d$gen); c(stats::df.residual(stats::lm(yield ~ loc + gen, d)), min(p), max(p)) })
key("D10", "SASmixed::PBIB", "partly", "4 of 15 treatments to each block", "plot", "plots in blocks", "treatment by block, incomplete", no,
    "each treatment in 4 blocks; pairs meet once or never", "randomisation; the association scheme is not on the page",
    c(error = 31, pair_max = 1, partners_min = 12, partners_max = 12),
    { p <- pairs_met(d$Block, d$Treatment); m <- unclass(table(d$Block, d$Treatment)) > 0; cp <- crossprod(m); diag(cp) <- 0
      c(stats::df.residual(stats::lm(response ~ Block + Treatment, d)), max(p), min(rowSums(cp > 0)), max(rowSums(cp > 0))) })
key("D11", "agridat::cochran.lattice", "partly", "16 treatments to plots, 4 x 4 in each of 5 replicates", "plot", "plots in replicates", "rows by columns within replicate", no,
    "every pair of treatments shares a row once and a column once", "randomisation; plot orientation (the page says it is not clear)",
    c(error = 30, row_pair_min = 1, row_pair_max = 1, col_pair_min = 1, col_pair_max = 1),
    { x <- fac(d, "rep", "row", "col")
      pr <- pairs_met(interaction(x$rep, x$row), x$trt); pc <- pairs_met(interaction(x$rep, x$col), x$trt)
      c(stats::df.residual(stats::lm(y ~ rep + rep:row + rep:col + trt, x)), min(pr), max(pr), min(pc), max(pc)) })
key("D12", "agridat::john.alpha", "partly", "24 varieties to plots, 6 incomplete blocks of 4 in each of 3 replicates", "plot", "plots in blocks in replicates", "variety by block, incomplete", no,
    "each variety once per replicate", "randomisation; the plots lay in one line (help page)",
    c(error = 31, once_per_rep = 72, pair_max = 1),
    { b <- interaction(d$rep, d$block)
      c(stats::df.residual(stats::lm(yield ~ rep + b + gen, cbind(d, b = b))), sum(table(d$rep, d$gen) == 1), max(pairs_met(b, d$gen))) })
key("D13", "agridat::kempton.rowcol", "partly", "35 genotypes to plots, rows by columns in each of 2 replicates", "plot", "plots in replicates", "rows by columns within replicate", no,
    "5 rows by 7 columns per replicate; 2 plots missing", "randomisation; why 2 plots are missing",
    c(error = 12, slots_per_rep = 35, missing = 2, seen_twice = 33, seen_once = 2),
    { x <- fac(d, "rep", "row", "col"); t <- table(d$gen)
      s <- n_of(d[d$rep == "R1", ], "row") * n_of(d[d$rep == "R1", ], "col")
      c(stats::df.residual(stats::lm(yield ~ rep + rep:row + rep:col + gen, x)), s, n_of(d, "rep") * s - nrow(d), sum(t == 2), sum(t == 1)) })
key("D14", "agridat::kling.augmented", "partly", "3 checks in every block, 50 new lines once each", "plot", "plots in blocks", "checks by block", "checks repeated",
    "6 blocks; checks have 6 plots", "randomisation within block",
    c(error = 10, checks = 3, check_plots = 18, new_once = 50, check_once_per_block = 18),
    { t <- table(d$gen); ck <- names(t)[t > 1]; cd <- droplevels(d[d$gen %in% ck, ])
      c(stats::df.residual(stats::lm(tsw ~ block + gen, d)), length(ck), sum(t[ck]), sum(t == 1), sum(table(cd$block, cd$gen) == 1)) })
# E. Split units -------------------------------------------------------------------
key("E03", "agridat::gomez.splitsplit", "partly", "nitrogen to whole plot; management to sub-plot; variety to sub-sub-plot", "sub-sub-plot", "sub-sub-plots in sub-plots in whole plots in blocks", "nitrogen by management by variety", no,
    "three sizes of unit", "randomisation at each level (not on the page)",
    c(whole = 8, sub = 20, subsub = 60),
    { x <- fac(d, "rep", "nitro", "management", "gen"); utils::tail(strata(yield ~ nitro * management * gen + Error(rep / nitro / management), x), 3) })
key("E05", "agridat::gomez.stripsplitplot", "partly", "genotype to horizontal strip; nitrogen to vertical strip; planting method to sub-plot of each crossing", "sub-plot", "strips in replicates; sub-plots in crossings", "genotype strips by nitrogen strips", no,
    "neither strip factor inside the other", "randomisation (not on the page)",
    c(gen_strip = 10, nitro_strip = 4, crossing = 20, sub = 36),
    { x <- fac(d, "rep", "gen", "nitro", "planting"); utils::tail(strata(yield ~ gen * nitro * planting + Error(rep / (gen * nitro)), x), 4) })
key("E06", "agridat::gomez.splitplot.subsample", "partly", "management to whole plot; time of application to sub-plot", "hill sample", "samples in sub-plots in whole plots in blocks", "management by time", no,
    "2 hills sampled per sub-plot",
    "The help page says 186 rows and managements M1 to M6; the data hold 192 rows and M1 to M8 (3 x 8 x 4 x 2 = 192). Randomisation not on the page",
    c(whole = 14, sub = 48, sample = 96),
    { x <- fac(d, "rep", "manage", "time"); utils::tail(strata(height ~ manage * time + Error(rep / manage / time), x), 3) })
key("E07", "agridat::gomez.groupsplit", "partly", "fertiliser to whole plot; variety group to sub-plot; variety to sub-sub-plot", "plot", "varieties in groups in fertiliser halves in replicates", "fertiliser by variety", no,
    "in the data each fertiliser takes half the rows of a replicate and each group a column of 15 plots in it",
    "The help page names the design only; which factor is on which size of plot is read from the row and column numbers, not stated. Randomisation not on the page",
    c(whole = 2, group = 8, variety = 168, columns_per_group_plot = 1, rows_per_group_plot = 15),
    { x <- fac(d, "rep", "fert", "group", "gen"); g <- interaction(x$rep, x$fert, x$group, drop = TRUE)
      c(utils::tail(strata(yield ~ fert * gen + Error(rep / fert / group), x), 3),
        max(tapply(x$col, g, function(v) length(unique(v)))), min(tapply(x$row, g, function(v) length(unique(v))))) })
key("E08", "daewr::plasma", "partly", "32 combinations of 5 two-level factors to runs, split-plot", "run", "runs in whole plots (not marked)", "A by B by C by D by E, full", no,
    "unreplicated; in the data order the runs come in 8 groups of 4 that share A, B, C and hold all 4 settings of D and E",
    "Which factors were hard to change is not on the page and whole plots are not marked; the grouping is read from the row order",
    c(combinations = 32, residual = 0, groups_same_ABC = 8, groups_all_DE = 8),
    { g <- rep(1:8, each = 4)
      c(n_of(d, "A", "B", "C", "D", "E"), stats::df.residual(stats::lm(y ~ A * B * C * D * E, d)),
        sum(tapply(paste(d$A, d$B, d$C), g, function(v) length(unique(v))) == 1),
        sum(tapply(paste(d$D, d$E), g, function(v) length(unique(v))) == 4)) })
key("E09", "daewr::sausage", "partly", "A and B to batch (whole plot), within block; C and D to sub-unit", "sub-unit", "sub-units in batches in blocks", "A by B by C by D", no,
    "8 batches, 4 sub-units each", "randomisation (not on the page)",
    c(batch = 3, sub = 12, batches_constant_AB = 8),
    { x <- d; x$batch <- interaction(x$Block, x$Gbatch)
      c(strata(ys ~ Block + A * B * C * D + Error(batch), x), sum(tapply(paste(x$A, x$B), x$batch, function(v) length(unique(v))) == 1)) })
key("E10", "daewr::strung", "partly", "diet to cow", "cow-week", "weeks in cows, cows in diets", "diet by week", "each cow measured in 4 weeks",
    "30 cows although numbered 1 to 10", "whether diets were random; weeks are time, not assigned",
    c(cow = 27, within = 81),
    { x <- fac(d, "Diet", "week"); x$cow <- interaction(x$Diet, x$Cow); strata(protein ~ Diet * week + Error(cow), x) })
# F. Nested and crossed random factors -----------------------------------------------
key("F04", "daewr::polymer", "silent", "nothing; lots, boxes, preparations, tests", "test", "tests in preparations in boxes in lots", no, no,
    "staggered: box 1 has 2 preparations (2 tests and 1), box 2 has 1 preparation with 1 test", "how lots were sampled; the staggered plan is in the data only",
    c(lot = 29, box = 30, prep = 30, test = 30),
    { a <- stats::anova(stats::lm(strength ~ lot / box / prep, d)); a$Df })
key("F05", "GAD::snails", "partly", "snails of each origin transplanted into cages on boulders of each shore", "snail", "boulders in shores; cages in boulder by origin; snails in cages", "origin by shore; origin by boulder", no,
    "every boulder holds cages of both origins", "The data are artificial (help page). All four factors are called random",
    c(origin = 1, shore = 3, origin_shore = 3, boulder = 8, origin_boulder = 8, cage = 24, snail = 192),
    { a <- stats::anova(stats::lm(growth ~ origin * shore + shore:boulder + origin:shore:boulder + origin:shore:boulder:cage, d)); a$Df })
key("F07", "lme4::Penicillin", "partly", "nothing; each of 6 samples tested on each of 24 plates", "plate-sample", no, "plate by sample", "every sample on every plate",
    "one result per plate and sample", "how samples and plates were chosen; position of the samples on a plate",
    c(plate = 23, sample = 5, error = 115),
    { a <- stats::anova(stats::lm(diameter ~ plate + sample, d)); a$Df })
key("F08", "nlme::Machines", "stated", "nothing assigned; 6 workers chosen at random, each operates every machine 3 times", "run", "runs in worker-machine cells", "worker by machine", "each worker on all 3 machines",
    "3 runs per worker and machine", "order of the runs",
    c(worker = 5, machine = 2, both = 10, error = 36),
    { a <- stats::anova(stats::lm(score ~ factor(Worker, ordered = FALSE) * Machine, d)); a$Df })
key("F09", "AMCP::chapter_11_table_19", "partly", "nothing; every judge rates every target", "rating", no, "target by judge", "each target rated 4 times",
    "hypothetical data", "The help page says both four judges and 'Judge: judge number; of 3'; the data hold 4. How judges and targets were chosen",
    c(target = 5, judge = 3, error = 15),
    { a <- stats::anova(stats::lm(Rating ~ factor(Subject) + factor(Judge), d)); a$Df })
key("F11", "AMCP::chapter_10_table_5", "partly", "4 schools at random; within each school, students assigned at random to one of 2 programs", "student", "students in school-program cells", "program by school", no,
    "5 students per school and program",
    "The help page contradicts itself: one paragraph assigns students within each school, another assigns two schools to each program. The data have both programs in every school; the key follows the data",
    c(A = 1, B = 3, AB = 3, within = 32),
    { a <- stats::anova(stats::lm(ACT ~ factor(A) * factor(B), d)); a$Df })

# Check and write ----------------------------------------------------------------
want <- strsplit("A06 A07 A08 A09 A10 B04 B06 B07 C05 C06 C07 C08 C09 C10 C11 C12 C13 D04 D06 D07 D09 D10 D11 D12 D13 D14 E03 E05 E06 E07 E08 E09 E10 F04 F05 F07 F08 F09 F11", " ")[[1]]
designs <- utils::read.csv("notes/textbook-designs.csv", encoding = "UTF-8")
ids <- vapply(K, `[[`, "", "id")
stopifnot(length(want) == 39, !anyDuplicated(ids), setequal(ids, want),
          all(designs$tier[match(ids, designs$id)] %in% c("extended", "edge")),
          all(substr(ids, 1, 1) %in% LETTERS[1:6]))
data_names <- vapply(K, `[[`, "", "data")
stopifnot(identical(data_names, designs$data[match(ids, designs$id)]))
cat("tiers in the list:\n")
print(table(designs$tier[match(ids, designs$id)]))

rows <- lapply(K, function(k) {
  got <- tryCatch(eval(k$check, list(d = as.data.frame(get_data(k$data))), globalenv()),
                  error = function(e) e)
  ok <- !inherits(got, "error") && length(got) == length(k$expect) &&
    all(as.integer(got) == as.integer(k$expect))
  cat(sprintf("%-4s %-9s %s%s\n", k$id, if (ok) "agrees" else "DISAGREES",
              paste(names(k$expect), k$expect, sep = "=", collapse = " "),
              if (ok) "" else paste(" | got:", if (inherits(got, "error")) conditionMessage(got) else paste(got, collapse = " "))))
  data.frame(k[c("id", "data", "source", "assigned", "measured", "nested",
                 "crossed", "repeats", "limits", "open")],
             implied = paste(names(k$expect), k$expect, sep = " = ", collapse = "; "),
             agrees = ok, stringsAsFactors = FALSE)
})
out <- do.call(rbind, rows)
con <- file("notes/textbook-ext-keys-AF.csv", "w", encoding = "UTF-8")
utils::write.csv(out, con, row.names = FALSE)
close(con)
cat("\nkeys:", nrow(out), "; agree:", sum(out$agrees), "\n")
cat("what the help page says about assignment or selection:\n")
print(table(factor(out$source, c("stated", "partly", "silent"))))
cat("keys with something left open:", sum(nzchar(out$open)), "\n")
cat("\nR", R.version.string, "\n")
for (p in unique(sub("::.*", "", out$data))) cat(p, as.character(utils::packageVersion(p)), "\n")
