# Textbook designs to test studydesignflow with (milestone 6b).
#
# Run from the package folder:  Rscript --vanilla notes/textbook-designs.R
# Needs the data packages in a separate library (never the renv library):
#   dev/complib, installed from CRAN; versions are written to the output.
# Writes notes/textbook-designs.csv (the list, with facts computed from the
# data and the source as given on the data's help page) and prints a summary.
#
# One entry:
#   id, family, tier ("core" = in a vignette, "extended" = tested only,
#   "edge" = expected to be outside what a top-down flow can say)
#   design    the design, in textbook words
#   data      package::dataset
#   taught    analyses this design is used to teach (by the author of this
#             list, general knowledge; not quoted from the book)
#   assigned  unit that received the treatment, and how; "nothing" = observed
#   measured  unit one measurement belongs to
#   must_say  what a reader must be able to tell from the picture alone
#   expect    prediction made BEFORE testing: T true, I incomplete,
#             M misleading, R refused; and the planned part that would fix it
#   facts     expression computing the counts from the data

.libPaths(c(normalizePath("dev/complib"), .libPaths()))

get_data <- function(s) {
  e <- new.env()
  file <- c("survey::apistrat" = "api", "survey::apiclus1" = "api",
            "survey::apiclus2" = "api", "survey::apisrs" = "api",
            "survey::apipop" = "api",
            "survival::veteran" = "cancer", "survival::diabetic" = "diabetic")
  p <- sub("::.*", "", s)
  x <- sub(".*::", "", s)
  f <- if (s %in% names(file)) file[[s]] else x
  suppressWarnings(utils::data(list = f, package = p, envir = e))
  if (exists(x, envir = e, inherits = FALSE)) return(get(x, envir = e))
  getExportedValue(p, x)
}
help_page <- function(s) {
  h <- utils::help((sub(".*::", "", s)), package = (sub("::.*", "", s)))
  if (!length(h)) return(character())
  t <- utils::capture.output(tools::Rd2txt(
    utils:::.getHelpFile(h), options = list(underline_titles = FALSE)))
  gsub("_\b", "", t)
}
help_part <- function(t, part) {
  i <- grep(paste0("^", part, ":"), t)
  if (!length(i)) return("")
  j <- grep("^[A-Z][^:]*:$", t)
  j <- c(j[j > i[1]], length(t) + 1)[1]
  trimws(gsub("\\s+", " ", paste(t[(i[1] + 1):(j - 1)], collapse = " ")))
}
# distinct combinations of columns
u <- function(d, ...) nrow(unique(as.data.frame(d)[c(...)]))
# smallest and largest number of rows per combination, empty ones left out
per <- function(d, ...) {
  k <- table(do.call(paste, c(as.data.frame(d)[c(...)], sep = "|")))
  if (min(k) == max(k)) as.character(min(k)) else paste0(min(k), "-", max(k))
}
# number of distinct `what` per combination of the other columns
within <- function(d, what, ...) per(unique(as.data.frame(d)[c(..., what)]), ...)

L <- list()
add <- function(id, family, tier, design, data, taught, assigned, measured,
                must_say, expect, facts) {
  L[[length(L) + 1]] <<- list(
    id = id, family = family, tier = tier, design = design, data = data,
    taught = taught, assigned = assigned, measured = measured,
    must_say = must_say, expect = expect, facts = substitute(facts))
}

# A. One and two groups -----------------------------------------------------
A <- "A. One and two groups"
add("A01", A, "core", "One sample against a known value", "AMCP::chapter_3_table_1",
    "one-sample t; Wilcoxon signed-rank", "nothing", "child",
    "One group, how it was chosen, compared with a fixed value",
    "T", c(children = nrow(d)))
add("A02", A, "core", "Two independent observed groups, unequal sizes", "ISwR::energy",
    "two-sample t; Welch t; Wilcoxon rank-sum", "nothing (groups observed)", "woman",
    "The groups were not assigned; sizes differ",
    "T", c(women = nrow(d), lean = sum(d$stature == "lean"), obese = sum(d$stature == "obese")))
add("A03", A, "core", "Two randomised groups, yes/no outcome", "medicaldata::strep_tb",
    "two-proportion test; chi-squared; Fisher exact; risk ratio", "patient, random", "patient",
    "Randomised; arm sizes; outcome per arm",
    "T", c(patients = nrow(d), arms = u(d, "arm"), per_arm = per(d, "arm")))
add("A04", A, "core", "Paired: same person before and after", "ISwR::intake",
    "paired t; Wilcoxon signed-rank", "nothing", "person-occasion",
    "11 people measured twice, not 22 people",
    "M; 7a copies, 9 time", c(people = nrow(d), measurements = 2 * nrow(d)))
add("A05", A, "core", "Paired: both treatments inside one person", "MASS::shoes",
    "paired t; randomisation test", "foot, within boy", "foot",
    "Each boy wears both materials; the boy is the block",
    "M; 7a copies", c(boys = length(d$A), feet = length(unlist(d))))
add("A06", A, "extended", "Matched pairs of different units", "agridat::darwin.maize",
    "paired t; sign test; Fisher's randomisation test", "plant, within pair", "plant",
    "Pairs sit inside pots; each pair holds one of each type",
    "I; pairing not drawable except as a block row",
    c(plants = nrow(d), pairs = u(d, "pair"), pots = u(d, "pot"), pairs_per_pot = within(d, "pair", "pot")))
add("A07", A, "extended", "Same people, two drugs (often misread as two groups)", "datasets::sleep",
    "paired t", "drug, within patient", "patient-drug",
    "10 patients, not two groups of 10",
    "M; 7a copies", c(patients = u(d, "ID"), rows = nrow(d), rows_per_patient = per(d, "ID")))
add("A08", A, "extended", "Two methods on the same subjects", "ISwR::wright",
    "limits of agreement (Bland-Altman)", "nothing", "subject-meter",
    "Every subject measured by both meters",
    "M; 7a copies", c(subjects = nrow(d), meters = ncol(d)))
add("A09", A, "extended", "Matched pairs, time to event", "MASS::gehan",
    "stratified log-rank; paired survival", "patient, within pair", "patient",
    "21 pairs, one of each treatment in a pair",
    "I; pairing", c(patients = nrow(d), pairs = u(d, "pair"), per_pair = per(d, "pair")))
add("A10", A, "extended", "One eye treated, the other is the control", "survival::diabetic",
    "paired survival; frailty model", "eye, within patient", "eye",
    "Treatment is assigned to eyes, both eyes are followed",
    "M; 7a copies", c(patients = u(d, "id"), eyes = nrow(d), eyes_per_patient = per(d, "id")))

# B. One-way designs --------------------------------------------------------
B <- "B. One factor, separate groups"
add("B01", B, "core", "Completely randomised, balanced", "datasets::PlantGrowth",
    "one-way ANOVA; Tukey; Dunnett; Kruskal-Wallis", "plant, random", "plant",
    "Three groups of equal size",
    "T", c(plants = nrow(d), groups = u(d, "group"), per_group = per(d, "group")))
add("B02", B, "core", "Completely randomised, quantitative levels", "AMCP::chapter_6_table_1",
    "trend (polynomial) contrasts", "child, random", "child",
    "The levels are ordered amounts",
    "T", c(children = nrow(d), levels = u(d, "Minutes"), per_level = per(d, "Minutes")))
add("B03", B, "core", "Randomised groups with a measurement before", "AMCP::chapter_9_table_7",
    "ANCOVA; gain scores", "person, random", "person, twice",
    "Measured before and after; randomised in between",
    "I; 9 time", c(people = nrow(d), conditions = u(d, "Condition"), per_condition = per(d, "Condition")))
add("B04", B, "extended", "Unequal groups with before and after", "MASS::anorexia",
    "ANCOVA; unequal n", "person", "person, twice",
    "Group sizes differ",
    "I; 9 time", c(people = nrow(d), groups = u(d, "Treat"), per_group = per(d, "Treat")))
add("B05", B, "core", "One random factor", "lme4::Dyestuff",
    "variance components; random-effects one-way ANOVA; ICC", "nothing (batches sampled)", "preparation",
    "Batches were sampled, not treatments assigned",
    "T", c(batches = u(d, "Batch"), preparations = nrow(d), per_batch = per(d, "Batch")))
add("B06", B, "extended", "Randomised groups, counts", "datasets::InsectSprays",
    "one-way ANOVA after transformation; Poisson GLM", "plot", "plot",
    "Units and group sizes",
    "T", c(plots = nrow(d), sprays = u(d, "spray"), per_spray = per(d, "spray")))
add("B07", B, "extended", "Randomised groups, several outcomes", "carData::Baumann",
    "MANOVA; planned contrasts; ANCOVA", "student, random", "student, several tests",
    "Same students take every test",
    "I; 7a copies", c(students = nrow(d), groups = u(d, "group"), per_group = per(d, "group"), tests = ncol(d) - 1))

# C. Factorial --------------------------------------------------------------
C <- "C. Factorial"
add("C01", C, "core", "Two-way factorial, balanced", "AMCP::chapter_7_table_5",
    "two-way ANOVA; simple effects", "person, random to cells", "person",
    "Both factors fully crossed; n per cell; margins",
    "I; 7f shared groups for margins",
    c(people = nrow(d), A = u(d, "Feedback"), B = u(d, "Drug"), per_cell = per(d, "Feedback", "Drug")))
add("C02", C, "core", "Two-way, unbalanced and only observed", "AMCP::chapter_7_table_16",
    "Type I, II, III sums of squares", "nothing", "person",
    "Nothing was assigned; unequal cells",
    "T", c(people = nrow(d), cells = u(d, "Sex", "Education"), per_cell = per(d, "Sex", "Education")))
add("C03", C, "core", "One assigned factor by one observed factor", "AMCP::chapter_7_table_24",
    "treatment by blocking-factor ANOVA", "person, to therapy only", "person",
    "Therapy was assigned; severity was classified afterwards (help page)",
    "T", c(people = nrow(d), therapy = u(d, "Therapy"), severity = u(d, "Severity"),
           per_cell = per(d, "Therapy", "Severity")))
add("C04", C, "core", "Three-way factorial", "AMCP::chapter_8_table_12",
    "three-way ANOVA; three-way interaction", "person, random to cells", "person",
    "All cells, n per cell, readable at page width",
    "I; size", c(people = nrow(d), drug = u(d, "Drug"), biofeedback = u(d, "Biofeedback"), diet = u(d, "Diet"),
                 per_cell = per(d, "Drug", "Biofeedback", "Diet")))
add("C05", C, "extended", "Two-way with quantitative factor", "datasets::ToothGrowth",
    "two-way ANOVA; trend by group", "animal", "animal",
    "Ordered doses crossed with supplement",
    "T", c(animals = nrow(d), supp = u(d, "supp"), dose = u(d, "dose"), per_cell = per(d, "supp", "dose")))
add("C06", C, "extended", "Two-way, two crossed observed origins", "HSAUR3::foster",
    "unbalanced two-way ANOVA", "litter, to foster mother", "litter",
    "Genotype of litter crossed with genotype of mother; unequal cells",
    "T", c(litters = nrow(d), cells = u(d, "litgen", "motgen"), per_cell = per(d, "litgen", "motgen")))
add("C07", C, "extended", "Two-level factorial, unreplicated", "daewr::chem",
    "half-normal plot; Lenth", "run", "run",
    "One run per combination: no replication",
    "I; size", c(runs = nrow(d), combinations = u(d, "A", "B", "C", "D"), per_combination = per(d, "A", "B", "C", "D")))
add("C08", C, "edge", "Two-level fractional factorial", "daewr::arso",
    "aliasing; resolution", "run", "run",
    "Which combinations were left out, and why",
    "I; a tree cannot show a fraction well", c(runs = nrow(d), factors = 7L, full = 2L^7L))
add("C09", C, "edge", "Fraction followed by its mirror image", "daewr::augm",
    "fold-over; sequential experimentation", "run", "run",
    "Second fraction added after the first",
    "I; 9 time", c(runs = nrow(d), fractions = u(d, "fold"), per_fraction = per(d, "fold")))
add("C10", C, "edge", "Fractional factorial, three levels, field", "agridat::chinloy.fractionalfactorial",
    "confounding in fractions", "plot", "plot",
    "Fraction of the full set, in blocks",
    "I; fraction", c(plots = nrow(d), blocks = u(d, "block"), combinations = u(d, "n", "p", "k", "b", "m"), full = 3L^5L))
add("C11", C, "edge", "Central composite design in blocks", "daewr::cement",
    "response surface", "run", "run",
    "Corner, axial and centre runs; two blocks",
    "I; levels are points, not groups", c(runs = nrow(d), blocks = u(d, "Block"), points = u(d, "x1", "x2", "x3")))
add("C12", C, "edge", "Box-Behnken design", "daewr::Treb",
    "response surface", "run", "run",
    "Edge points and centre runs", "I", c(runs = nrow(d), points = u(d, "x1", "x2", "x3")))
add("C13", C, "edge", "Constrained mixture", "daewr::pest",
    "Scheffe mixture model", "run", "run",
    "Parts add up to a whole", "I", c(runs = nrow(d), points = u(d, "x1", "x2", "x3")))

# D. Blocking ---------------------------------------------------------------
D <- "D. Blocks"
add("D01", D, "core", "Randomised complete blocks", "agridat::gomez.seedrate",
    "two-way ANOVA without interaction; Friedman", "plot, random within block", "plot",
    "Every treatment once in every block",
    "T", c(plots = nrow(d), blocks = u(d, "rep"), treatments = u(d, "rate"), per_block = per(d, "rep"), per_cell = per(d, "rep", "rate")))
add("D02", D, "core", "Person as the complete block", "nlme::ergoStool",
    "RCB with random subjects; repeated measures", "stool, within person", "person-stool",
    "Each person tries every stool",
    "M; 7a copies", c(people = u(d, "Subject"), stools = u(d, "Type"), rows = nrow(d), per_cell = per(d, "Subject", "Type")))
add("D03", D, "core", "Complete blocks with subsamples", "agridat::grover.rcb.subsample",
    "ANOVA with sampling error; pseudoreplication", "plot", "hill within plot",
    "Plots are the replicates, hills are not",
    "T", c(blocks = u(d, "block"), plots = u(d, "block", "trt"), hills = nrow(d), hills_per_plot = per(d, "block", "trt")))
add("D04", D, "extended", "Generalised blocks (replicates inside block)", "daewr::rcb",
    "block by treatment interaction as error", "shot, within golfer", "shot",
    "Golfer is the block; several shots per cell",
    "M; 7a copies", c(golfers = u(d, "id"), heights = u(d, "teehgt"), shots = nrow(d), per_cell = per(d, "id", "teehgt")))
add("D05", D, "core", "Latin square, field", "agridat::cochran.wireworms",
    "Latin square ANOVA", "plot, random within row and column", "plot",
    "Two blockings at once; each treatment once per row and column",
    "M; a tree nests rows over columns",
    c(plots = nrow(d), rows = u(d, "row"), cols = u(d, "col"), treatments = u(d, "trt"),
      per_row_trt = per(d, "row", "trt"), per_col_trt = per(d, "col", "trt")))
add("D06", D, "extended", "Latin square, laboratory", "datasets::OrchardSprays",
    "Latin square ANOVA", "cell", "cell", "As D05, larger",
    "M", c(cells = nrow(d), rows = u(d, "rowpos"), cols = u(d, "colpos"), treatments = u(d, "treatment"), per_row_trt = per(d, "rowpos", "treatment")))
add("D07", D, "edge", "Graeco-Latin square, replicated", "agridat::devries.pine",
    "Graeco-Latin ANOVA", "plot", "plot",
    "Two treatments each balanced over rows and columns",
    "M", c(plots = nrow(d), squares = u(d, "block"), spacing = u(d, "spacing"), thinning = u(d, "thinning"), plots_per_square = per(d, "block")))
add("D08", D, "core", "Balanced incomplete blocks, tasting", "daewr::taste",
    "intra-block analysis", "recipe, to panellist", "panellist-recipe",
    "Each panellist tastes only some recipes",
    "M; 7a copies to a subset", c(panellists = u(d, "panelist"), recipes = u(d, "recipe"), rows = nrow(d),
                                 recipes_per_panellist = per(d, "panelist"), panellists_per_recipe = per(d, "recipe")))
add("D09", D, "extended", "Balanced incomplete blocks, field", "agridat::cochran.bib",
    "intra- and inter-block analysis", "plot", "plot",
    "13 lines, 4 per block", "I", c(blocks = u(d, "loc"), lines = u(d, "gen"), plots = nrow(d), per_block = per(d, "loc"), per_line = per(d, "gen")))
add("D10", D, "edge", "Partially balanced incomplete blocks", "SASmixed::PBIB",
    "mixed model with random blocks", "plot", "plot", "Which treatments meet in a block",
    "I", c(blocks = u(d, "Block"), treatments = u(d, "Treatment"), plots = nrow(d), per_block = per(d, "Block")))
add("D11", D, "edge", "Balanced lattice square", "agridat::cochran.lattice",
    "lattice analysis", "plot", "plot", "Replicates, rows and columns",
    "I", c(reps = u(d, "rep"), plots = nrow(d), treatments = u(d, "trt"), per_rep = per(d, "rep")))
add("D12", D, "extended", "Resolvable incomplete blocks (alpha)", "agridat::john.alpha",
    "mixed model, blocks within replicates", "plot", "plot",
    "Incomplete blocks nested in complete replicates",
    "I", c(reps = u(d, "rep"), blocks = u(d, "rep", "block"), plots = nrow(d), varieties = u(d, "gen"), per_block = per(d, "rep", "block")))
add("D13", D, "edge", "Row-column design", "agridat::kempton.rowcol",
    "REML row-column analysis", "plot", "plot", "Rows and columns inside replicates",
    "M", c(reps = u(d, "rep"), rows = u(d, "rep", "row"), cols = u(d, "rep", "col"), plots = nrow(d), lines = u(d, "gen")))
add("D14", D, "extended", "Augmented: repeated checks, single new lines", "agridat::kling.augmented",
    "augmented-design ANOVA", "plot", "plot",
    "Checks in every block, new lines once",
    "T", c(blocks = u(d, "block"), plots = nrow(d), lines = u(d, "gen"),
           lines_seen_once = sum(table(d$gen) == 1), most_plots_for_a_line = max(table(d$gen))))
add("D15", D, "core", "Factorial confounded in blocks", "datasets::npk",
    "confounding; intra-block analysis", "plot, random within block", "plot",
    "Each block holds half the combinations",
    "I", c(plots = nrow(d), blocks = u(d, "block"), combinations = u(d, "N", "P", "K"), per_block = per(d, "block")))

# E. Split units ------------------------------------------------------------
E <- "E. Split units"
add("E01", E, "core", "Split-plot in blocks", "agridat::yates.oats",
    "split-plot ANOVA with two error terms", "variety to whole plot; nitrogen to sub-plot", "sub-plot",
    "Two sizes of unit, each with its own randomisation",
    "T", c(blocks = u(d, "block"), whole_plots = u(d, "block", "gen"), sub_plots = nrow(d),
           varieties = u(d, "gen"), nitrogen = u(d, "nitro"), per_whole_plot = per(d, "block", "gen")))
add("E02", E, "core", "Split-plot without blocks (batches)", "lme4::cake",
    "split-plot ANOVA; mixed model", "recipe to batch; temperature to portion", "cake",
    "Batch numbers repeat across recipes but are different batches",
    "T", c(recipes = u(d, "recipe"), batch_labels = u(d, "replicate"), batches = u(d, "recipe", "replicate"), cakes = nrow(d), per_batch = per(d, "recipe", "replicate")))
add("E03", E, "extended", "Split-split-plot", "agridat::gomez.splitsplit",
    "three error terms", "three nested sizes of plot", "sub-sub-plot",
    "Three sizes of unit",
    "I; size", c(blocks = u(d, "rep"), whole = u(d, "rep", "nitro"), sub = u(d, "rep", "nitro", "management"), subsub = nrow(d)))
add("E04", E, "core", "Strip-plot (criss-cross)", "agridat::gomez.stripplot",
    "strip-plot ANOVA, three error terms", "each factor to its own strip", "crossing of two strips",
    "Neither factor is nested in the other",
    "M; a tree forces one inside the other", c(blocks = u(d, "rep"), variety_strips = u(d, "rep", "gen"), nitrogen_strips = u(d, "rep", "nitro"), plots = nrow(d)))
add("E05", E, "edge", "Strip-split-plot", "agridat::gomez.stripsplitplot",
    "four error terms", "two strips, then split", "sub-plot", "As E04, then split",
    "M", c(blocks = u(d, "rep"), crossings = u(d, "rep", "gen", "nitro"), plots = nrow(d)))
add("E06", E, "extended", "Split-plot with subsamples", "agridat::gomez.splitplot.subsample",
    "split-plot with sampling error", "management to whole plot; time to sub-plot", "hill sample",
    "Samples are not replicates",
    "T", c(blocks = u(d, "rep"), whole = u(d, "rep", "manage"), sub = u(d, "rep", "manage", "time"), samples = nrow(d)))
add("E07", E, "extended", "Group-balanced split-plot", "agridat::gomez.groupsplit",
    "group-balanced ANOVA", "fertiliser; variety group; variety", "plot",
    "Varieties differ between groups",
    "I; size", c(blocks = u(d, "rep"), fert = u(d, "fert"), groups = u(d, "group"), varieties = u(d, "gen"), plots = nrow(d), varieties_per_group = within(d, "gen", "group")))
add("E08", E, "extended", "Split-plot, industrial, unreplicated", "daewr::plasma",
    "two half-normal plots", "hard-to-change factors to the larger unit", "run",
    "Some factors are reset less often; the data do not mark the whole plots",
    "I", c(runs = nrow(d), combinations = u(d, "A", "B", "C", "D", "E")))
add("E09", E, "extended", "Split-plot with whole plots in blocks, industrial", "daewr::sausage",
    "split-plot mixed model", "two factors to batch; two to sub-unit", "sub-unit",
    "Batch is the whole plot",
    "T", c(blocks = u(d, "Block"), batches = u(d, "Block", "Gbatch"), whole_combinations = u(d, "A", "B"), runs = nrow(d)))
add("E10", E, "extended", "Split-plot in time", "daewr::strung",
    "split-plot ANOVA; mixed model for repeated measures", "diet to cow", "cow-week",
    "Cow numbers repeat across diets; weeks are not randomised",
    "M; 7a copies, 9 time", c(diets = u(d, "Diet"), cow_labels = u(d, "Cow"), cows = u(d, "Diet", "Cow"), rows = nrow(d), weeks = u(d, "week")))

# F. Nested and crossed random factors -------------------------------------
G <- "F. Nested and crossed random factors"
add("F01", G, "core", "Two-level nesting", "lme4::Pastes",
    "nested ANOVA; variance components", "nothing (sampled)", "analysis of a cask sample",
    "Cask a in batch A is not cask a in batch B",
    "T", c(batches = u(d, "batch"), cask_labels = u(d, "cask"), casks = u(d, "batch", "cask"), analyses = nrow(d), per_cask = per(d, "batch", "cask")))
add("F02", G, "core", "Treatment, animals, preparations, readings", "GAD::rats",
    "mixed nested ANOVA", "treatment to rat", "reading",
    "The rat is the replicate; its label repeats across treatments",
    "T", c(treatments = u(d, "treat"), rat_labels = u(d, "rat"), rats = u(d, "treat", "rat"), preparations = u(d, "treat", "rat", "liver"), readings = nrow(d)))
add("F03", G, "core", "Three-level passive sampling", "nlme::Oxide",
    "variance components", "nothing (passive)", "site on wafer",
    "Lot, wafer, site",
    "T", c(lots = u(d, "Lot"), wafers = u(d, "Lot", "Wafer"), sites = nrow(d)))
add("F04", G, "extended", "Unequal (staggered) nesting", "daewr::polymer",
    "staggered nested variance components", "nothing (sampled)", "test",
    "Branches are deliberately unequal",
    "T", c(lots = u(d, "lot"), boxes = u(d, "lot", "box"), preparations = u(d, "lot", "box", "prep"), tests = nrow(d), tests_per_box = per(d, "lot", "box")))
add("F05", G, "extended", "Crossed and nested together", "GAD::snails",
    "mixed ANOVA with nested and orthogonal random factors", "snails to boulders and cages", "snail",
    "Origin crossed with shore; boulders inside shores; cages inside both",
    "M; a tree cannot cross", c(origins = u(d, "origin"), shores = u(d, "shore"), boulders = u(d, "shore", "boulder"), cages = u(d, "origin", "shore", "boulder", "cage"), snails = nrow(d)))
add("F06", G, "core", "Crossed random factors: gauge study", "daewr::gagerr",
    "gauge R and R", "nothing", "measurement",
    "Every operator measures every part",
    "M; 7a copies", c(parts = u(d, "part"), operators = u(d, "oper"), measurements = nrow(d), per_cell = per(d, "part", "oper")))
add("F07", G, "extended", "Crossed random factors, no replicate", "lme4::Penicillin",
    "crossed random effects", "nothing", "plate-sample",
    "Every sample on every plate",
    "M; 7a copies", c(plates = u(d, "plate"), samples = u(d, "sample"), rows = nrow(d)))
add("F08", G, "extended", "Workers crossed with machines, replicated", "nlme::Machines",
    "mixed model with interaction", "machine, within worker", "run",
    "Every worker uses every machine several times",
    "M; 7a copies", c(workers = u(d, "Worker"), machines = u(d, "Machine"), runs = nrow(d), per_cell = per(d, "Worker", "Machine")))
add("F09", G, "extended", "Judges and targets", "AMCP::chapter_11_table_19",
    "intraclass correlation", "nothing", "rating",
    "Every judge rates every target",
    "M; 7a copies", c(targets = u(d, "Subject"), judges = u(d, "Judge"), ratings = nrow(d)))

add("F10", G, "core", "Random therapists nested in an observed group", "AMCP::chapter_10_table_9",
    "nested mixed ANOVA", "clients to trainee, random; gender observed", "client rating",
    "Trainees belong to one gender group; trainee numbers may repeat",
    "T", c(genders = u(d, "Gender"), trainee_labels = u(d, "Trainee"), trainees = u(d, "Gender", "Trainee"), ratings = nrow(d), per_trainee = per(d, "Gender", "Trainee")))
add("F11", G, "extended", "Fixed factor crossed with a random factor", "AMCP::chapter_10_table_5",
    "two-way mixed ANOVA", "program (A) inside each school; schools (B) are random", "student",
    "Every school runs both programs; different students in each cell",
    "T", c(students = nrow(d), A = u(d, "A"), B = u(d, "B"), per_cell = per(d, "A", "B")))

# G. Within subjects --------------------------------------------------------
H <- "G. Within subjects and crossover"
add("G01", H, "core", "One-way repeated measures", "AMCP::chapter_11_table_5",
    "repeated-measures ANOVA; sphericity; MANOVA approach", "nothing (age)", "child-age",
    "12 children at 4 ages, not 48 children",
    "M; 9 time", c(children = nrow(d), ages = ncol(d)))
add("G02", H, "core", "Two-way within subjects", "AMCP::chapter_12_table_1",
    "two-way repeated-measures ANOVA", "conditions within person", "person-condition",
    "Every person in all conditions",
    "M; 7a copies", c(people = nrow(d), conditions = ncol(d)))
add("G03", H, "core", "One between, one within (mixed design)", "AMCP::chapter_14_table_9",
    "mixed-design ANOVA", "nothing between (age group); angle within", "person-angle",
    "Groups hold different people; angles hold the same people",
    "M; 7a copies", c(people = nrow(d), groups = u(d, "Group"), per_group = per(d, "Group"), angles = 3L))
add("G04", H, "extended", "Two between, two within", "carData::OBrienKaiser",
    "mixed-design ANOVA; MANOVA", "treatment between; phase and hour within", "person-phase-hour",
    "16 people, 15 measurements each, unequal groups",
    "M; 7a copies, 9 time", c(people = nrow(d), treatments = u(d, "treatment"), per_group = per(d, "treatment", "gender"), occasions = ncol(d) - 2L))
add("G05", H, "core", "Two-period crossover", "daewr::antifungal",
    "crossover analysis with sequence, period, carry-over", "person to sequence", "person-period",
    "Sequences were assigned, everyone gets both drugs; order",
    "M; 7a copies, 9 time", c(people = u(d, "Group", "Subject"), subject_labels = u(d, "Subject"), groups = u(d, "Group"), people_per_group = within(d, "Subject", "Group"), periods = u(d, "Period"), rows = nrow(d)))
add("G06", H, "extended", "Extra-period crossover", "daewr::bioequiv",
    "carry-over separated from treatment", "person to sequence", "person-period",
    "Third period repeats the second treatment",
    "M; 9 time", c(people = u(d, "Group", "Subject"), periods = u(d, "Period"), rows = nrow(d), sequences = length(unique(tapply(paste(d$Treat), paste(d$Group, d$Subject), paste, collapse = "")))))
add("G07", H, "extended", "Williams square crossover", "daewr::chipman",
    "balanced carry-over", "person to sequence", "person-period",
    "Squares, groups, periods",
    "M; 9 time", c(people = u(d, "Square", "Group", "Subject"), subject_labels = u(d, "Subject"), squares = u(d, "Square"), periods = u(d, "Period"), rows = nrow(d)))
add("G08", H, "extended", "Repeated Latin square for order", "AMCP::chapter_12_table_29",
    "Latin square repeated measures", "order, within animal", "animal-time",
    "Order is balanced over squares",
    "M; 9 time", c(animals = u(d, "Square", "Subject"), subject_labels = u(d, "Subject"), squares = u(d, "Square"), times = u(d, "Time"), rows = nrow(d)))
add("G09", H, "extended", "Subjects and items crossed, many trials", "afex::fhch2010",
    "crossed random effects of subjects and items", "task between; item type within", "trial",
    "Task differs between people; items are shared within a task",
    "M; 7a copies", c(people = u(d, "id"), items = u(d, "item"), trials = nrow(d), people_per_task = within(d, "id", "task"), items_per_person = within(d, "item", "id")))
add("G10", H, "extended", "Trials inside blocks inside people, between groups", "ez::ANT",
    "repeated-measures ANOVA on cell means; mixed model", "group between; cue and flank within", "trial",
    "How many trials feed each cell mean",
    "M; 7a copies", c(people = u(d, "subnum"), groups = u(d, "group"), blocks_per_person = within(d, "block", "subnum"), trials_per_person = per(d, "subnum"), trials = nrow(d), cells = u(d, "cue", "flank")))
add("G11", H, "extended", "Persons crossed with items, questionnaire", "lme4::VerbAgg",
    "item response models", "nothing", "answer",
    "Everyone answers every item; items are a 2 by 2 by 3 set",
    "M; 7a copies", c(people = u(d, "id"), items = u(d, "item"), answers = nrow(d), answers_per_person = per(d, "id")))
add("G12", H, "extended", "Two within factors, few animals", "WWGbook::rat.brain",
    "mixed model, two within factors", "drug and region, within rat", "rat-region-drug",
    "5 rats, 6 measurements each",
    "M; 7a copies", c(rats = u(d, "animal"), rows = nrow(d), per_rat = per(d, "animal")))

# H. Multilevel and longitudinal -------------------------------------------
M <- "H. Multilevel and longitudinal"
add("H01", M, "core", "Two levels, observed: pupils in schools", "mlmRev::Hsb82",
    "random intercept and slope models", "nothing", "pupil",
    "Schools sampled, pupils inside; sector belongs to the school",
    "T", c(schools = u(d, "school"), pupils = nrow(d), pupils_per_school = per(d, "school"), sectors = u(d, "sector")))
add("H02", M, "core", "Three levels: pupils, classes, schools", "WWGbook::classroom",
    "three-level model", "nothing", "pupil",
    "Numbers at each level, and how uneven they are",
    "T", c(schools = u(d, "schoolid"), classes = u(d, "schoolid", "classid"), pupils = nrow(d), classes_per_school = within(d, "classid", "schoolid"), pupils_per_class = per(d, "schoolid", "classid")))
add("H03", M, "core", "Cluster-randomised: dose to dam, pups measured", "WWGbook::ratpup",
    "mixed model, heterogeneous variances", "dose to dam, random", "pup",
    "Litter is the assigned unit; the help page says 30 dams",
    "T", c(litters = u(d, "litter"), pups = nrow(d), litters_per_dose = within(d, "litter", "treatment"), pups_per_litter = per(d, "litter")))
add("H04", M, "core", "Cluster-assigned: condition to room", "AMCP::chapter_16_table_4",
    "multilevel model against pooled ANOVA", "condition to room", "child",
    "Rooms, not children, received the condition; room labels repeat. The help page says both that each classroom held both conditions and that they were separate: check the book",
    "T", c(room_labels = u(d, "Room"), rooms = u(d, "Condition", "Room"), children = nrow(d), rooms_per_condition = within(d, "Room", "Condition"), children_per_room = per(d, "Condition", "Room")))
add("H05", M, "core", "Randomised people followed over time", "AMCP::chapter_14_table_13",
    "longitudinal mixed model; GEE", "person, random", "person-time",
    "Arms, then repeated visits of the same people",
    "I; 9 time", c(people = u(d, "id1"), people_per_arm = within(d, "id1", "treatment"), times = u(d, "time"), rows = nrow(d)))
add("H06", M, "core", "Growth of observed groups", "nlme::Orthodont",
    "random coefficient growth model", "nothing", "child-age",
    "Children by sex, four ages each",
    "I; 9 time", c(children = u(d, "Subject"), children_per_sex = within(d, "Subject", "Sex"), ages = u(d, "age"), rows = nrow(d)))
add("H07", M, "core", "Follow-up with missed visits", "WWGbook::autism",
    "growth model with unbalanced data", "nothing", "child-age",
    "Who was seen at each age; the help page says 214 children",
    "I; 9 time", c(children = u(d, "childid"), rows = nrow(d), visits_per_child = per(d, "childid"), seen_at_each_age = paste(table(d$age), collapse = "/")))
add("H08", M, "extended", "Trial with visits in two clinics (repeated labels)", "geepack::respiratory",
    "GEE; GLMM, binary outcome", "patient, random", "patient-visit",
    "Patient numbers repeat across clinics",
    "I; 9 time", c(clinics = u(d, "center"), patient_labels = u(d, "id"), patients = u(d, "center", "id"), rows = nrow(d), visits = u(d, "visit")))
add("H09", M, "extended", "Trial with missed visits", "HSAUR3::toenail",
    "GLMM against GEE, dropout", "patient, random", "patient-visit",
    "Patients at each visit; the help page says 378 were randomised",
    "I; 9 time", c(patients = u(d, "patientID"), per_arm = within(d, "patientID", "treatment"), rows = nrow(d), seen_at_each_visit = paste(table(d$visit), collapse = "/")))
add("H10", M, "extended", "Occasions in pupils in schools", "mlmRev::egsingle",
    "three-level growth model", "nothing", "pupil-year",
    "Schools, pupils, occasions",
    "I; 9 time", c(schools = u(d, "schoolid"), pupils = u(d, "childid"), rows = nrow(d), occasions_per_pupil = per(d, "childid")))
add("H11", M, "extended", "Clustered and longitudinal: teeth in patients", "WWGbook::veneer",
    "clustered longitudinal model", "nothing", "tooth-time",
    "Teeth inside patients, each seen at each time",
    "I; 9 time", c(patients = u(d, "patient"), teeth = u(d, "patient", "tooth"), rows = nrow(d), times = u(d, "time"), teeth_per_patient = within(d, "tooth", "patient")))
add("H12", M, "extended", "Children in mothers in communities", "mlmRev::guImmun",
    "three-level logistic model", "nothing (survey)", "child",
    "Mostly one child per mother",
    "T", c(communities = u(d, "comm"), mothers = u(d, "mom"), children = nrow(d), children_per_mother = per(d, "mom")))
add("H13", M, "edge", "Cross-classified: primary by secondary school", "mlmRev::ScotsSec",
    "cross-classified model", "nothing", "pupil",
    "Primary schools are not inside secondary schools",
    "M; 7f shared groups", c(primary = u(d, "primary"), secondary = u(d, "second"), pupils = nrow(d), pairs_seen = u(d, "primary", "second"), secondaries_per_primary = within(d, "second", "primary")))
add("H14", M, "edge", "Partly crossed: students rate lecturers", "lme4::InstEval",
    "crossed random effects, large", "nothing", "rating",
    "Each student rates some lecturers",
    "M; 7f", c(students = u(d, "s"), lecturers = u(d, "d"), ratings = nrow(d), ratings_per_student = per(d, "s")))
add("H15", M, "edge", "Multiple membership: workers in several firms", "R2MLwiN::wage1",
    "multiple-membership model", "nothing", "worker",
    "One worker counts in several firms",
    "M; 7f", c(workers = nrow(d), first_firms = u(d, "company"), workers_with_2nd = sum(d$company2 != 0), with_3rd = sum(d$company3 != 0), with_4th = sum(d$company4 != 0)))
add("H16", M, "edge", "Two sets of individuals crossed in pairs", "lme4::salamander",
    "GLMM with crossed random effects", "pairings by design", "mating trial",
    "Each female meets several males and the reverse",
    "M; 7f", c(females = u(d, "Female"), males = u(d, "Male"), trials = nrow(d), trials_per_female = per(d, "Female")))
add("H17", M, "extended", "Multisite randomised experiment over four years", "mlmRev::star",
    "multilevel and cross-classified models", "class type, within school", "pupil-year",
    "Pupils enter, leave and move; randomised inside schools",
    "I; 9 time", c(schools = u(d, "sch"), pupils = u(d, "id"), rows = nrow(d), years_per_pupil = per(d, "id"), pupils_by_grade = paste(table(d$gr), collapse = "/")))
add("H18", M, "extended", "Several sites, blocks at each site", "SASmixed::Multilocation",
    "multilocation mixed model", "plot, within block", "plot",
    "Block numbers repeat across sites",
    "T", c(sites = u(d, "Location"), block_labels = u(d, "Block"), blocks = u(d, "Location", "Block"), plots = nrow(d), treatments = u(d, "Trt")))
add("H19", M, "extended", "Meta-analysis: trials as units", "HSAUR3::BCG",
    "fixed and random-effects meta-analysis", "people, within trial", "trial",
    "Trials pooled; two arms in each",
    "T", c(trials = nrow(d), vaccinated = sum(d$BCGVacc), not_vaccinated = sum(d$NoVacc)))
add("H20", M, "extended", "Groups followed, changing sizes", "lme4::cbpp",
    "binomial GLMM", "nothing", "herd-period",
    "Herds, not animals, are the rows",
    "I; 9 time", c(herds = u(d, "herd"), rows = nrow(d), periods = u(d, "period"), periods_per_herd = per(d, "herd")))

# I. Observation, categories, time to event --------------------------------
I <- "I. Observational, categorical, time to event"
add("I01", I, "core", "Cross-section, one sample classified two ways", "datasets::HairEyeColor",
    "chi-squared test; log-linear model", "nothing", "student",
    "Only the total was fixed in advance",
    "T", c(students = sum(d), hair = dim(d)[1], eye = dim(d)[2], sex = dim(d)[3]))
add("I02", I, "core", "Case-control, unmatched", "datasets::esoph",
    "odds ratio; logistic regression", "nothing; sampled by outcome", "person",
    "Cases and controls were sampled separately",
    "T", c(cases = sum(d$ncases), controls = sum(d$ncontrols), rows_are_groups = nrow(d)))
add("I03", I, "core", "Case-control, matched sets", "datasets::infert",
    "conditional logistic regression", "nothing; matched", "woman",
    "Sets of one case and its controls; one set is short",
    "I; pairing", c(women = nrow(d), cases = sum(d$case == 1), sets = u(d, "stratum"), women_per_set = per(d, "stratum")))
add("I04", I, "extended", "Stratified two-way table", "datasets::UCBAdmissions",
    "Cochran-Mantel-Haenszel; Simpson's paradox", "nothing", "applicant",
    "Departments as strata",
    "T", c(applicants = sum(d), departments = dim(d)[3]))
add("I05", I, "extended", "Cohort, binary outcome", "MASS::birthwt",
    "logistic regression", "nothing", "birth",
    "Observed exposure", "T", c(births = nrow(d), low = sum(d$low), smokers = sum(d$smoke)))
add("I06", I, "core", "Randomised, time to event, censored", "survival::veteran",
    "Kaplan-Meier; log-rank; Cox", "patient, random", "patient",
    "Arms, events and censored per arm",
    "T", c(patients = nrow(d), per_arm = per(d, "trt"), events = sum(d$status == 1)))
add("I07", I, "extended", "Two-phase (case-cohort) sampling", "survival::nwtco",
    "case-cohort Cox model", "nothing; second phase sampled", "patient",
    "Subcohort plus all cases; they overlap",
    "R or M; 7f shared groups", c(patients = nrow(d), subcohort = sum(d$in.subcohort), relapsed = sum(d$rel), both = sum(d$in.subcohort & d$rel)))
add("I08", I, "extended", "Interrupted time series", "datasets::Seatbelts",
    "segmented regression; intervention model", "nothing; law changed", "month",
    "Months before and after the law",
    "I; 9 time", c(months = nrow(d), before = sum(d[, "law"] == 0), after = sum(d[, "law"] == 1)))
add("I09", I, "extended", "Before and after with a comparison group", "wooldridge::injury",
    "difference in differences", "nothing; policy", "claim",
    "Two states, two earner groups, two periods",
    "T", c(claims = nrow(d), states = u(d, "ky"), cells = u(d, "ky", "highearn", "afchnge"), per_cell = per(d, "ky", "highearn", "afchnge")))
add("I10", I, "extended", "Matched days, speed limit on or off", "MASS::Traffic",
    "Poisson GLM", "speed limit to days, in runs; randomisation not stated", "day",
    "Days paired across two years",
    "I; pairing", c(days = nrow(d), pairs = u(d, "day"), with_limit = sum(d$limit == "yes")))

# J. Sampling ---------------------------------------------------------------
J <- "J. Sample surveys"
add("J01", J, "core", "Simple random sample", "SDAResources::agsrs",
    "SRS estimation", "nothing; sampled", "county",
    "Population and sample size", "T", c(counties = nrow(d)))
add("J02", J, "core", "Stratified random sample", "survey::apistrat",
    "stratified estimation", "nothing; sampled within strata", "school",
    "Sampled separately in each stratum",
    "T", c(schools = nrow(d), strata = u(d, "stype"), per_stratum = paste(table(d$stype), collapse = "/")))
add("J03", J, "core", "One-stage cluster sample", "survey::apiclus1",
    "cluster estimation", "nothing; districts sampled", "school",
    "Every school in a chosen district",
    "T", c(districts = u(d, "dnum"), schools = nrow(d), per_district = per(d, "dnum")))
add("J04", J, "core", "Two-stage cluster sample", "survey::apiclus2",
    "two-stage estimation", "nothing; two stages", "school",
    "Some schools in a chosen district",
    "T", c(districts = u(d, "dnum"), schools = nrow(d), per_district = per(d, "dnum")))
add("J05", J, "extended", "Cluster sample with unequal sizes", "SDAResources::algebra",
    "ratio estimation for clusters", "nothing; classes sampled", "student",
    "12 of 187 classes",
    "T", c(classes = u(d, "class"), students = nrow(d), per_class = per(d, "class")))
add("J06", J, "extended", "Two-stage, two units per cluster", "SDAResources::coots",
    "two-stage unequal probabilities", "nothing; eggs sampled", "egg",
    "Two eggs from each clutch",
    "T", c(clutches = u(d, "clutch"), eggs = nrow(d), per_clutch = per(d, "clutch")))

# K. Breeding and long-term field designs ----------------------------------
K <- "K. Special field designs"
add("K01", K, "edge", "Diallel cross", "agridat::hayman.tobacco",
    "diallel analysis", "crosses by design", "plot",
    "Every line as male and as female",
    "M; crossing", c(years = u(d, "year"), males = u(d, "male"), females = u(d, "female"), plots = nrow(d)))
add("K02", K, "edge", "Long-term rotation", "agridat::payne.wheat",
    "mixed model for rotations", "plots, fixed for years", "plot-year",
    "The same plots for many years",
    "I; 9 time", c(years = u(d, "year"), rotations = u(d, "rotation"), nitrogen = u(d, "nitro"), rows = nrow(d)))
add("K03", K, "edge", "Unreplicated lines with a repeated check", "agridat::burgueno.unreplicated",
    "spatial mixed model", "plot", "plot",
    "One check repeated, new lines once",
    "T", c(plots = nrow(d), lines = u(d, "gen"), lines_seen_once = sum(table(d$gen) == 1), check_plots = max(table(d$gen))))

# Compute, check, write -----------------------------------------------------
stopifnot(!anyDuplicated(vapply(L, `[[`, "", "id")))
rows <- lapply(L, function(x) {
  d <- tryCatch(get_data(x$data), error = function(e) e)
  f <- if (inherits(d, "error")) d else
    tryCatch(eval(x$facts, list(d = d), globalenv()), error = function(e) e)
  ok <- !inherits(f, "error") && !anyNA(f)
  t <- help_page(x$data)
  src <- paste(help_part(t, "Source"), help_part(t, "References"))
  src <- trimws(src)
  data.frame(
    id = x$id, family = x$family, tier = x$tier, design = x$design,
    data = x$data, taught = x$taught, assigned = x$assigned,
    measured = x$measured, must_say = x$must_say, expect = x$expect,
    facts = if (ok) paste(names(f), f, sep = " = ", collapse = "; ")
            else paste("ERROR:", if (inherits(f, "error")) conditionMessage(f) else "NA"),
    help_page_source = if (nzchar(src)) src else "(help page names no source)",
    stringsAsFactors = FALSE)
})
out <- do.call(rbind, rows)
con <- file("notes/textbook-designs.csv", "w", encoding = "UTF-8")
utils::write.csv(out, con, row.names = FALSE)
close(con)

# a readable list, one table per family
md <- c("# Textbook designs: the list", "",
        "Written by `notes/textbook-designs.R`; do not edit by hand. Method and",
        "rules: `notes/textbook-designs.md`. Counts are computed from the data.",
        "The columns taught, assigned, must say and prediction are the list",
        "author's reading, not quotations, and are checked against the book",
        "before a design is tested.", "")
cell <- function(z) gsub("|", "/", z, fixed = TRUE)
for (f in unique(out$family)) {
  o <- out[out$family == f, ]
  md <- c(md, paste("##", f), "",
          "| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |",
          "|---|---|---|---|---|---|---|---|---|---|",
          sprintf("| %s | %s | %s | `%s` | %s | %s | %s | %s | %s | %s |",
                  o$id, o$tier, cell(o$design), o$data, cell(o$taught), cell(o$assigned),
                  cell(o$measured), cell(o$must_say), cell(o$facts), cell(o$expect)), "")
}
con <- file("notes/textbook-designs-list.md", "wb")
writeLines(md, con, useBytes = TRUE)
close(con)

cat("designs:", nrow(out), "\n")
cat("data sets:", length(unique(out$data)), "\n")
cat("tiers:", paste(names(table(out$tier)), table(out$tier), collapse = ", "), "\n")
cat("failed facts:", sum(grepl("^ERROR", out$facts)), "\n")
cat("help page names no source:",
    paste(out$id[out$help_page_source == "(help page names no source)"], collapse = " "), "\n\n")
print(table(out$family, factor(out$tier, c("core", "extended", "edge"))))
cat("\nprediction (first letter):\n")
print(table(substr(out$expect, 1, 1), factor(out$tier, c("core", "extended", "edge"))))
cat("\n")
for (i in seq_len(nrow(out))) cat(out$id[i], out$data[i], "|", out$facts[i], "\n")
pk <- sort(unique(sub("::.*", "", out$data)))
cat("\npackages:", paste(pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), sep = "; ")
cat("\n", R.version.string, "\n")
