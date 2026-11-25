# Answer keys for the 46 core textbook designs (milestone 6b, step 4).
#
# Run from the package folder:  Rscript --vanilla notes/textbook-keys.R
# Written BEFORE any design was drawn. Writes notes/textbook-keys.csv and
# prints every check.
#
# For each design:
#   source    what the help page says about how units were assigned or chosen:
#             "stated", "partly" or "silent" (the books were not read)
#   assigned  what was assigned, to what unit, how ("nothing" = observed)
#   measured  what one measurement belongs to
#   nested / crossed / repeats / limits   the structure a reader must see
#   open      what the source leaves unsettled
#   expect    the degrees of freedom (or counts) the structure implies, worked
#             out by arithmetic from the numbers of units
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
long <- function(d, cols, id = NULL) {
  m <- as.data.frame(d)[cols]
  data.frame(id = factor(rep(seq_len(nrow(m)), length(cols))),
             cond = factor(rep(cols, each = nrow(m)), levels = cols),
             y = unlist(m, use.names = FALSE))
}
n_of <- function(d, ...) nrow(unique(as.data.frame(d)[c(...)]))

K <- list()
key <- function(id, data, source, assigned, measured, nested, crossed, repeats,
                limits, open, expect, check) {
  K[[length(K) + 1]] <<- list(
    id = id, data = data, source = source, assigned = assigned,
    measured = measured, nested = nested, crossed = crossed, repeats = repeats,
    limits = limits, open = open, expect = expect, check = substitute(check))
}
no <- "none"

key("A01", "AMCP::chapter_3_table_1", "stated", "nothing; files of 6 hyperactive children drawn at random", "child", no, no, no,
    "one group against the value 98", "", c(error = 5),
    c(stats::df.residual(stats::lm(IQ ~ 1, d))))
key("A02", "ISwR::energy", "silent", "nothing; lean and obese women", "woman", no, no, no,
    "13 and 9", "how the women were found", c(error = 20),
    c(stats::df.residual(stats::lm(expend ~ stature, d))))
key("A03", "medicaldata::strep_tb", "partly", "patient to streptomycin or control, random", "patient", no, no, no,
    "55 and 52; 55 of 107 improved",
    "The help page says placebo-controlled, but its own arm is called Control; secondary sources found by search say bed rest, no placebo. The 1948 paper could not be opened. Draw it as Control, never as placebo",
    c(strep = 55, control = 52, improved = 55),
    c(sum(d$arm == "Streptomycin"), sum(d$arm == "Control"), sum(d$improved)))
key("A04", "ISwR::intake", "silent", "nothing; pre- and postmenstrual days of the same woman", "woman-occasion", no, no,
    "each woman measured twice", "", "how the women were found", c(error = 10),
    { x <- long(d, c("pre", "post")); strata(y ~ cond + Error(id), x)[2] })
key("A05", "MASS::shoes", "partly", "material to foot, within boy; whether left or right was random is not said on the help page", "foot", "feet in boys", no,
    "each boy wears both materials", "boy is the block", "randomisation of feet", c(error = 9),
    { x <- long(data.frame(A = d$A, B = d$B), c("A", "B")); strata(y ~ cond + Error(id), x)[2] })
key("B01", "datasets::PlantGrowth", "silent", "control or one of two treatments to plant", "plant", no, no, no,
    "10 each", "whether random", c(error = 27), c(stats::df.residual(stats::lm(weight ~ group, d))))
key("B02", "AMCP::chapter_6_table_1", "stated", "study time (1 to 4 minutes) to child, random", "child", no, no, no,
    "ordered amounts; 6 each", "", c(error = 20), c(stats::df.residual(stats::lm(Recall ~ factor(Minutes), d))))
key("B03", "AMCP::chapter_9_table_7", "stated", "SSRI, placebo or wait list to person, random", "person, before and after", no, no,
    "each person measured twice", "10 each", "", c(ancova_error = 26),
    c(stats::df.residual(stats::lm(Post ~ Pre + factor(Condition), d))))
key("B05", "lme4::Dyestuff", "stated", "nothing; 6 batches of works manufacture", "preparation", "preparations in batches", no, no,
    "5 each", "whether the batches were a random sample", c(batches = 5, within = 24),
    { a <- stats::anova(stats::lm(Yield ~ Batch, d)); a$Df })
key("C01", "AMCP::chapter_7_table_5", "partly", "biofeedback (yes, no) and drug (X, Y, Z) to person", "person", no, "biofeedback by drug, full", no,
    "5 per cell", "random assignment is not stated on this page", c(error = 24),
    c(stats::df.residual(stats::lm(Score ~ factor(Feedback) * factor(Drug), d))))
key("C02", "AMCP::chapter_7_table_16", "stated", "nothing; newly hired employees", "person", no, "sex by degree", no,
    "3 to 8 per cell", "", c(error = 18), c(stats::df.residual(stats::lm(Salary ~ factor(Sex) * factor(Education), d))))
key("C03", "AMCP::chapter_7_table_24", "stated", "therapy to person, random, 15 each; severity classified afterwards", "person", no, "therapy by severity", no,
    "3 to 7 per cell", "", c(error = 36), c(stats::df.residual(stats::lm(Score ~ factor(Therapy) * factor(Severity), d))))
key("C04", "AMCP::chapter_8_table_12", "partly", "biofeedback, drug and diet to person", "person", no, "2 by 3 by 2, full", no,
    "6 per cell, 12 cells", "random assignment is not stated on this page", c(error = 60),
    c(stats::df.residual(stats::lm(BP ~ factor(Drug) * factor(Biofeedback) * factor(Diet), d))))
key("D01", "agridat::gomez.seedrate", "stated", "seed rate to plot, random within block", "plot", "plots in blocks", "rate by block", no,
    "every rate once per block", "", c(error = 15), c(stats::df.residual(stats::lm(yield ~ factor(rate) + factor(rep), d))))
key("D02", "nlme::ergoStool", "silent", "stool type to person-trial", "person-stool", no, "person by stool", "each person tries all 4 stools",
    "person is the block", "order of stools", c(error = 24), { x <- fac(d, "Subject", "Type"); strata(effort ~ Type + Error(Subject), x)[2] })
key("D03", "agridat::grover.rcb.subsample", "partly", "fertiliser to plot, within block", "hill", "hills in plots, plots in blocks", "fertiliser by block", no,
    "4 hills per plot are samples, not replicates", "the word random is not on the page", c(plot_error = 24, hill = 108),
    { x <- fac(d, "trt", "block"); x$plot <- interaction(x$trt, x$block); s <- strata(tiller ~ trt + block + Error(plot), x); s })
key("D05", "agridat::cochran.wireworms", "partly", "fumigant to plot, once in each row and column", "plot", no, "rows by columns", no,
    "Latin square", "how the square was chosen", c(error = 12), c(stats::df.residual(stats::lm(worms ~ factor(row) + factor(col) + trt, d))))
key("D08", "daewr::taste", "silent", "2 of 4 recipes to each panellist", "panellist-recipe", no, "panellist by recipe, incomplete", "each panellist tastes 2",
    "each recipe tasted by 6; every pair of recipes met equally often", "order of tasting; how pairs were given out", c(error = 9, pair_min = 2, pair_max = 2),
    { m <- table(d$panelist, d$recipe); cp <- crossprod(m); pr <- cp[upper.tri(cp)]
      c(stats::df.residual(stats::lm(score ~ panelist + recipe, d)), min(pr), max(pr)) })
key("D15", "datasets::npk", "stated", "N, P, K combination to plot, within block", "plot", "plots in blocks", "N by P by K", no,
    "each block holds 4 of 8 combinations; NPK is confounded with blocks", "the word random is not on the page", c(error = 12),
    { strata(yield ~ N * P * K + Error(block), d)[2] })
key("E01", "agridat::yates.oats", "partly", "variety to whole plot; nitrogen to sub-plot", "sub-plot", "sub-plots in whole plots in blocks", "variety by nitrogen", no,
    "two sizes of unit", "randomisation: the Rothamsted report page opened without its text", c(whole_error = 10, sub_error = 45),
    { x <- fac(d, "block", "gen", "nitro"); s <- strata(yield ~ gen * nitro + Error(block / gen), x); utils::tail(s, 2) })
key("E02", "lme4::cake", "partly", "recipe to batch; temperature to portion of batch", "cake", "cakes in batches, batches in recipes", "recipe by temperature", no,
    "45 batches although numbered 1 to 15",
    "The page nests replicates in recipe but says the numbers suggest time order; if replicate 1 of all recipes was one day, day is a block. Cochran and Cox 1957 p. 300 not read",
    c(batch_error = 42, cake_error = 210),
    { x <- fac(d, "replicate", "recipe", "temperature"); x$temperature <- factor(x$temperature, ordered = FALSE)
      x$batch <- interaction(x$recipe, x$replicate); strata(angle ~ recipe * temperature + Error(batch), x) })
key("E04", "agridat::gomez.stripplot", "stated", "variety to horizontal strip; nitrogen to vertical strip", "crossing of two strips", "strips in replicates", "variety strips by nitrogen strips", no,
    "neither factor inside the other", "randomisation within replicates", c(variety_error = 10, nitrogen_error = 4, crossing_error = 20),
    { x <- fac(d, "rep", "gen", "nitro"); s <- strata(yield ~ gen * nitro + Error(rep / (gen * nitro)), x); utils::tail(s, 3) })
key("F01", "lme4::Pastes", "stated", "nothing; 10 deliveries at random, 3 casks at random in each", "analysis", "analyses in casks in deliveries", no,
    "2 analyses per cask sample", "30 casks although labelled a to c", "", c(batch = 9, cask = 20, analysis = 30),
    { a <- stats::anova(stats::lm(strength ~ batch / cask, d)); a$Df })
key("F02", "GAD::rats", "partly", "treatment to rat", "reading", "readings in preparations in rats in treatments", no,
    "2 readings per preparation", "6 rats although labelled 1 and 2", "whether random", c(rat = 3, preparation = 12, reading = 18),
    { x <- fac(d, "treat", "rat", "liver"); a <- stats::anova(stats::lm(glycog ~ treat / rat / liver, x)); a$Df[-1] })
key("F03", "nlme::Oxide", "stated", "nothing; 8 lots sampled", "site on wafer", "sites in wafers in lots", no, no,
    "passive data collection; 4 lots from each of 2 sources", "", c(lot = 7, wafer = 16, site = 48),
    { x <- as.data.frame(d); x$Lot <- factor(as.character(x$Lot)); x$w <- factor(paste(x$Lot, x$Wafer))
      c(nlevels(x$Lot) - 1, nlevels(x$w) - nlevels(x$Lot), nrow(x) - nlevels(x$w)) })
key("F06", "daewr::gagerr", "silent", "nothing", "measurement", no, "part by operator", "every operator measures every part twice",
    "both random", "how parts were chosen", c(part = 9, operator = 2, both = 18, repeat_ = 30),
    { a <- stats::anova(stats::lm(y ~ part * oper, d)); a$Df })
key("F10", "AMCP::chapter_10_table_9", "stated", "4 clients to each trainee, random; 3 male and 3 female trainees selected at random", "client rating", "clients in trainees, trainees in gender", no, no,
    "gender is observed; 6 trainees although numbered 1 to 3", "", c(trainee = 4, client = 18),
    { x <- fac(d, "Gender", "Trainee"); a <- stats::anova(stats::lm(Severity ~ Gender / Trainee, x)); a$Df[-1] })
key("G01", "AMCP::chapter_11_table_5", "stated", "nothing; ages 30, 36, 42, 48 months", "child-age", no, "child by age", "each child at 4 ages",
    "order is time", "", c(error = 33), { x <- long(d, names(d)); strata(y ~ cond + Error(id), x)[2] })
key("G02", "AMCP::chapter_12_table_1", "partly", "6 conditions to each person", "person-condition", no, "person by noise by angle", "each person in all 6",
    "", "order of trials", c(angle = 18, noise = 9, both = 18),
    { x <- long(d, names(d)); x$noise <- factor(sub("[0-9]", "", x$cond)); x$angle <- factor(sub("[A-Za-z]+", "", x$cond))
      s <- strata(y ~ noise * angle + Error(id / (noise * angle)), x); c(s[3], s[2], s[4]) })
key("G03", "AMCP::chapter_14_table_9", "partly", "nothing between (young, old); 3 angles to each person", "person-angle", "people in age groups", "angle by group", "each person at 3 angles",
    "10 each", "order of trials; scores are means over trials", c(between = 18, within = 36),
    { x <- long(d, c("Angle0", "Angle4", "Angle8")); x$g <- factor(rep(d$Group, 3)); strata(y ~ g * cond + Error(id), x) })
key("G05", "daewr::antifungal", "silent", "person to sequence (two groups)", "person-period", "people in sequences", "person by period", "each person gets both drugs",
    "8 and 9 people; order matters", "whether sequence was random; person 13 is missing from the numbering, reason not given", c(person = 15, within = 15),
    { x <- fac(d, "Group", "Subject", "Period", "Treat"); strata(pl ~ Group + Period + Treat + Error(Subject), x) })
key("H01", "mlmRev::Hsb82", "silent", "nothing; pupils in public and Catholic schools", "pupil", "pupils in schools, schools in sector", no, no,
    "14 to 67 per school; sector belongs to school", "how schools and pupils were sampled", c(school = 158, pupil = 7025),
    c(n_of(d, "school") - n_of(d, "sector"), nrow(d) - n_of(d, "school")))
key("H02", "WWGbook::classroom", "partly", "nothing; randomly selected classrooms", "pupil", "pupils in classes in schools", no, no,
    "uneven: 1 to 9 classes, 1 to 10 pupils", "how schools and pupils were selected", c(school = 106, class = 205, pupil = 878),
    c(n_of(d, "schoolid") - 1, n_of(d, "schoolid", "classid") - n_of(d, "schoolid"), nrow(d) - n_of(d, "schoolid", "classid")))
key("H03", "WWGbook::ratpup", "stated", "dose to dam, random", "pup", "pups in litters, litters in doses", no, no,
    "30 dams assigned, 27 litters in the data", "what happened to 3 dams", c(litter = 24, pup = 295),
    c(n_of(d, "litter") - n_of(d, "treatment"), nrow(d) - n_of(d, "litter")))
key("H04", "AMCP::chapter_16_table_4", "partly", "condition to classroom", "child", "children in classrooms, classrooms in conditions", no, no,
    "6 classrooms although numbered 1 to 3; 15 and 14 children", "The page contradicts itself: one sentence puts both conditions in each classroom. Key follows the data and the other sentences", c(room = 4, child = 23, control = 15, treated = 14),
    c(n_of(d, "Condition", "Room") - 2, nrow(d) - n_of(d, "Condition", "Room"), sum(d$Condition == 0), sum(d$Condition == 1)))
key("H05", "AMCP::chapter_14_table_13", "stated", "person to treatment (44) or control (56), random", "person-time", "people in arms", "time by arm", "each person at times 0, 4, 8",
    "time 0 is before treatment", "", c(person = 98, within = 196),
    { x <- fac(d, "id1", "treatment", "time"); strata(Y ~ treatment * time + Error(id1), x) })
key("H06", "nlme::Orthodont", "stated", "nothing; boys and girls", "child-age", "children in sex", "age by sex", "each child at 8, 10, 12, 14 years",
    "16 boys, 11 girls", "how children were found", c(person = 25, within = 75),
    { x <- as.data.frame(d); x$Subject <- factor(as.character(x$Subject)); x$age <- factor(x$age); strata(distance ~ Sex * age + Error(Subject), x) })
key("H07", "WWGbook::autism", "partly", "nothing; language group at age 2", "child-age", "children in language groups", "age by group", "1 to 5 ages per child",
    "214 children in the study, 158 in the data; missed visits", "why 56 children are not in the data", c(child = 157, visit = 454),
    c(n_of(d, "childid") - 1, nrow(d) - n_of(d, "childid")))
key("I01", "datasets::HairEyeColor", "stated", "nothing; a survey of students", "student", no, "hair by eye", no,
    "only the total of 592 was fixed", "the split by sex was added later for teaching, so sex is not data", c(total = 592, chisq_df = 9),
    { t <- margin.table(d, c(1, 2)); c(sum(t), suppressWarnings(stats::chisq.test(t))$parameter) })
key("I02", "datasets::esoph", "partly", "nothing; sampled by outcome", "person", no, no, no,
    "200 cases and 775 controls, sampled separately", "how controls were chosen", c(cases = 200, controls = 775),
    c(sum(d$ncases), sum(d$ncontrols)))
key("I03", "datasets::infert", "stated", "nothing; matched sets", "woman", "women in sets", no, no,
    "83 sets: 82 of one case and two controls, one of one and one", "what was matched on; one case is omitted (help page note)", c(sets = 83, three = 82, two = 1, cases = 83),
    { t <- table(d$stratum); c(length(t), sum(t == 3), sum(t == 2), sum(d$case)) })
key("I06", "survival::veteran", "stated", "patient to standard or test, random", "patient", no, no, no,
    "69 and 68; 128 events, 9 censored", "", c(standard = 69, test = 68, events = 128, censored = 9),
    c(sum(d$trt == 1), sum(d$trt == 2), sum(d$status == 1), sum(d$status == 0)))
key("J01", "SDAResources::agsrs", "stated", "nothing; simple random sample of counties", "county", no, no, no,
    "300 of the counties in agpop", "", c(sample = 300, population = 3078),
    c(nrow(d), nrow(get_data("SDAResources::agpop"))))
key("J02", "survey::apistrat", "stated", "nothing; sampled within school type", "school", "schools in types", no, no,
    "100, 50, 50 of 4421, 755, 1018 (E, H, M)", "", c(E = 100, H = 50, M = 50, NE = 4421, NH = 755, NM = 1018),
    { p <- get_data("survey::apipop"); c(table(d$stype), table(p$stype)) })
key("J03", "survey::apiclus1", "stated", "nothing; districts sampled, all their schools taken", "school", "schools in districts", no, no,
    "15 of 757 districts, 183 schools", "the help page says the weights in the data are wrong", c(districts = 15, all_districts = 757, schools = 183),
    c(n_of(d, "dnum"), n_of(get_data("survey::apipop"), "dnum"), nrow(d)))
key("J04", "survey::apiclus2", "stated", "nothing; districts sampled, then schools", "school", "schools in districts", no, no,
    "40 of 757 districts, 126 schools, 1 to 5 each", "", c(districts = 40, all_districts = 757, schools = 126),
    c(n_of(d, "dnum"), n_of(get_data("survey::apipop"), "dnum"), nrow(d)))

# Check and write ----------------------------------------------------------------
designs <- utils::read.csv("notes/textbook-designs.csv", encoding = "UTF-8")
core <- designs$id[designs$tier == "core"]
ids <- vapply(K, `[[`, "", "id")
stopifnot(!anyDuplicated(ids), setequal(ids, core))
data_names <- vapply(K, `[[`, "", "data")
stopifnot(identical(data_names, designs$data[match(ids, designs$id)]))

rows <- lapply(K, function(k) {
  got <- tryCatch(eval(k$check, list(d = get_data(k$data)), globalenv()),
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
con <- file("notes/textbook-keys.csv", "w", encoding = "UTF-8")
utils::write.csv(out, con, row.names = FALSE)
close(con)
cat("\nkeys:", nrow(out), "; agree:", sum(out$agrees), "\n")
cat("what the help page says about assignment or selection:\n")
print(table(factor(out$source, c("stated", "partly", "silent"))))
cat("keys with something left open:", sum(nzchar(out$open)), "\n")
