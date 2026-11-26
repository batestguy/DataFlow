# Answer keys for the extended and edge textbook designs of families G to K
# (milestone 6b, step 4; 32 designs).
#
# Run from the package folder:
#   Rscript --vanilla notes/textbook-ext-keys-GK.R > notes/textbook-ext-keys-GK-output.txt 2>&1
# Written BEFORE any of these designs was drawn. Writes
# notes/textbook-ext-keys-GK.csv (same columns as notes/textbook-keys.csv) and
# prints every check. Method as in notes/textbook-keys.R:
#   source    what the help page says about how units were assigned or chosen:
#             "stated", "partly" or "silent" (the books were not read)
#   assigned  what was assigned, to what unit, how ("nothing" = observed)
#   measured  what one measurement belongs to
#   nested / crossed / repeats / limits   the structure a reader must see
#   open      what the source leaves unsettled, and where the help page and
#             the list (notes/textbook-designs-list.md) disagree
#   expect    the degrees of freedom (or counts) the structure implies, worked
#             out by arithmetic from the numbers of units on the help page or
#             in the list
#   check     the same numbers computed from the data by a separate route
# A key is only accepted when expect and check agree. Agreement shows the key
# fits the data as coded. It does NOT show that the study was run that way.
# For the largest data (G09, H14, H17) counts are checked, not models.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_page <-", src) - 1)]))  # get_data()

# error df of every stratum of an aov with Error()
strata <- function(formula, data) {
  s <- suppressWarnings(summary(stats::aov(formula, data)))
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

K <- list()
key <- function(id, data, source, assigned, measured, nested, crossed, repeats,
                limits, open, expect, check) {
  K[[length(K) + 1]] <<- list(
    id = id, data = data, source = source, assigned = assigned,
    measured = measured, nested = nested, crossed = crossed, repeats = repeats,
    limits = limits, open = open, expect = expect, check = substitute(check))
}
no <- "none"

# G. Within subjects and crossover -------------------------------------------------
# G04: 16 people in 6 treatment-by-gender cells; 3 phases x 5 hours within.
# between = 16 - 6; phase = 10 x 2; hour = 10 x 4; phase by hour = 10 x 8
key("G04", "carData::OBrienKaiser", "silent", "treatment (control, A, B) between; gender observed; phase and hour within",
    "person-phase-hour", "people in treatment-by-gender cells", "phase by hour, within every person",
    "each person measured 15 times (3 phases x 5 hours)", "2 to 4 people per cell; contrived data",
    "how the 16 were divided into treatments ('an imaginary study')",
    c(between = 10, phase = 20, hour = 40, phase_hour = 80),
    { x <- as.data.frame(d); m <- x[, 3:17]
      l <- data.frame(id = factor(rep(seq_len(nrow(x)), 15)), trt = rep(x$treatment, 15), sex = rep(x$gender, 15),
                      phase = factor(rep(sub("\\..*", "", names(m)), each = nrow(x))),
                      hour = factor(rep(sub(".*\\.", "", names(m)), each = nrow(x))), y = unlist(m))
      strata(y ~ trt * sex * phase * hour + Error(id / (phase * hour)), l) })
# G06: 36 people, 2 sequences (ABB, BAA), 3 periods. person = 36 - 2;
# within = 36 x 2 - period 2 - treatment 1 - carry-over 1
key("G06", "daewr::bioequiv", "silent", "person to sequence ABB or BAA", "person-period", "people in sequences",
    "person by period", "each person in all 3 periods; period 3 repeats the period 2 drug",
    "18 per sequence; subject numbers have gaps (to 129)", "whether sequence was random; why numbers are missing",
    c(person = 34, within = 68, per_sequence_min = 18, per_sequence_max = 18),
    { x <- fac(d, "Subject"); s <- tapply(paste(x$Treat), x$Subject, paste, collapse = "")
      c(strata(y ~ Group + Period + Treat + Carry + Error(Subject), x), range(table(s))) })
# G07: 12 people = 2 squares x 3 sequence groups x 2; 3 periods, 3 treatments.
# error = 35 - person 11 - period 2 - treatment 2 - carry-over 2
key("G07", "daewr::chipman", "silent", "person to sequence (6 sequences, Williams squares)", "person-period",
    "people in sequence groups in squares", "person by period", "each person gets all 3 treatments, one per period",
    "2 per sequence; every treatment follows every other once per square", "whether allocation to sequences was random",
    c(error = 18, sequences = 6, per_sequence = 2),
    { x <- fac(d, "Subject", "Period", "Treat", "Carry"); s <- tapply(paste(x$Treat), x$Subject, paste, collapse = "")
      c(stats::df.residual(stats::lm(Time ~ Subject + Period + Treat + Carry, x)), length(unique(s)), unique(table(s))) })
# G08: 6 subjects in 2 Latin squares of 3; 3 times, 3 drugs.
# error = 17 - subject 5 - time 2 - drug 2
key("G08", "AMCP::chapter_12_table_29", "stated", "subjects randomly to 2 squares and to rows (orders) of the square",
    "subject-time", "subjects in squares", "subject by time; drug balanced over time within each square",
    "each subject gets all 3 drugs, one per time", "each drug once per time in each square",
    "The page calls a subject 'a pair of monkeys' and also speaks of '6 monkeys': whether there are 6 or 12 animals is not settled; the list says animals = 6",
    c(error = 8, orders = 6),
    { x <- fac(d, "Subject", "Time", "Condition"); s <- tapply(x$Condition[order(x$Subject, x$Time)], sort(x$Subject), paste, collapse = "")
      c(stats::df.residual(stats::lm(DV ~ Subject + Time + Condition, x)), length(unique(s))) })
# G09: 20 naming + 25 lexical decision people (help page); 300 words + 300
# nonwords; at most 300 items per person; full = 45 x 300 = 13500, data 13222
key("G09", "afex::fhch2010", "partly", "task between people (how is not said); item type within",
    "person-item trial", "people in tasks", "people by items, partly: each person sees about half the items; both tasks use all 600",
    "each person gives up to 300 trials", "items are fixed sets of words and nonwords",
    "how people were allotted to tasks and to item halves; the list says items are shared within a task, the data show both tasks used all 600 items; 278 trials missing (incorrect naming responses are not in the data, help page)",
    c(people = 45, naming = 20, lexdec = 25, items = 600, max_items_per_person = 300, missing = 278),
    { tk <- unique(d[c("id", "task")])$task; m <- table(d$id, d$item) > 0
      c(n_of(d, "id"), sum(tk == "naming"), sum(tk == "lexdec"), n_of(d, "item"), max(rowSums(m)), 45L * 300L - sum(m)) })
# G10: 20 people, 10 per group; 6 blocks x 48 = 288 trials; 12 cells of 24.
# on cell means: between = 20 - 2; cue = 18 x 3; flank = 18 x 2; both = 18 x 6
key("G10", "ez::ANT", "silent", "group between; cue and flank within (simulated data)", "trial",
    "trials in blocks in people, people in groups", "cue by flank within every person",
    "each person gives 288 trials, 24 per cue-flank cell", "simulated, not a real study",
    "how people were put into groups",
    c(between = 18, cue = 54, flank = 36, cue_flank = 108, per_cell = 24),
    { a <- stats::aggregate(rt ~ subnum + group + cue + flank, d, mean)
      c(strata(rt ~ group * cue * flank + Error(subnum / (cue * flank)), a), unique(as.vector(table(d$subnum, d$cue, d$flank)))) })
# G11: 316 people x 24 items; items = 2 modes x 2 blame types x 2 situations x 3 behaviours.
# answers = 316 x 24; additive error = 315 x 23
key("G11", "lme4::VerbAgg", "silent", "nothing; every person answers every item", "person-item answer", no,
    "people by items, full; items are 2 x 2 x 2 x 3", "each person answers all 24", "",
    "how the 316 people were found",
    c(answers = 7584, error = 7245, items = 24),
    { x <- fac(d, "id", "item")
      c(nrow(x), stats::df.residual(stats::lm(as.numeric(resp) ~ id + item, x)), nrow(unique(x[c("btype", "situ", "mode", "item")]))) })
# G12: 5 rats x 2 drugs x 3 regions. rat = 5 - 1; treatment = 4 x 1;
# region = 4 x 2; both = 4 x 2. (First run, 2026-11-26: the key left out the
# between-rat stratum and DISAGREED, got 4 4 8 8; the slip was in the key's
# arithmetic, not in the data, and was corrected before any drawing.)
key("G12", "WWGbook::rat.brain", "silent", "drug (basal, carbachol) and region, both within rat", "rat-region-drug",
    no, "rat by drug by region, full", "each rat measured 6 times", "",
    "The help page speaks of seven brain nuclei; the data hold 3 regions",
    c(rat = 4, treatment = 4, region = 8, both = 8),
    { x <- fac(d, "animal", "treatment", "region"); strata(activate ~ treatment * region + Error(animal / (treatment * region)), x) })

# H. Multilevel and longitudinal -----------------------------------------------------
# H08: 56 + 55 = 111 patients (help page), 4 visits. patient = 111 - 4 centre-arm cells
key("H08", "geepack::respiratory", "stated", "patient to placebo or active, random (help page)", "patient-visit",
    "patients in clinics", "visit by arm", "each patient at 4 visits; baseline is a column, not a visit",
    "patient numbers restart in each clinic", "source not named on the help page or in the package documents; randomisation within clinic or overall not said",
    c(patients = 111, patient_labels = 56, rows = 444, patient_error = 107),
    { x <- as.data.frame(d); x$pid <- factor(paste(x$center, x$id))
      c(n_of(x, "pid"), n_of(x, "id"), nrow(x), strata(outcome ~ factor(center) * treat + factor(visit) + Error(pid), x)[1]) })
# H09: 146 + 148 = 294 in the data; 7 planned visits; 294 x 7 - 1908 = 150 missed
key("H09", "HSAUR3::toenail", "stated", "patient to terbinafine or itraconazole, random (378 randomised, help page)", "patient-visit",
    "patients in arms", "visit by arm", "up to 7 visits per patient", "visits not at exactly the planned weeks",
    "Help page: 378 randomised; data: 294 patients. The 84 are not explained in the data",
    c(patients = 294, itraconazole = 146, terbinafine = 148, missed_visits = 150),
    { t <- table(d$patientID, d$visit); a <- table(unique(d[c("patientID", "treatment")])$treatment)
      c(nrow(t), a[["itraconazole"]], a[["terbinafine"]], sum(t == 0)) })
# H10: 60 schools, 1721 pupils (help page), 7230 rows; pupils in one school each
key("H10", "mlmRev::egsingle", "silent", "nothing; a subset of a study", "pupil-year", "years in pupils in schools", no,
    "2 to 6 years per pupil", "", "how schools and pupils were chosen for the subset",
    c(school = 59, pupil = 1661, occasion = 5509, pupils_in_two_schools = 0),
    c(n_of(d, "schoolid") - 1, n_of(d, "schoolid", "childid") - n_of(d, "schoolid"), nrow(d) - n_of(d, "childid"),
      n_of(d, "schoolid", "childid") - n_of(d, "childid")))
# H11: 12 patients, 55 teeth, 2 times. patient 11; tooth 55 - 12; within 55 - time 1
key("H11", "WWGbook::veneer", "silent", "nothing; veneers placed, teeth followed", "tooth-time", "teeth in patients", "time within tooth",
    "each tooth at 3 and 6 months", "tooth numbers repeat across patients (6 labels, 55 teeth)",
    "how patients and teeth were chosen",
    c(patient = 11, tooth = 43, within = 54, tooth_labels = 6),
    { x <- as.data.frame(d); x$t <- factor(paste(x$patient, x$tooth)); x$patient <- factor(x$patient)
      c(strata(gcf ~ factor(time) + Error(patient / t), x), n_of(x, "tooth")) })
# H12: 161 communities, 1595 mothers, 2159 children; 1063 + 2 x 500 + 3 x 32 = 2159
key("H12", "mlmRev::guImmun", "silent", "nothing; a survey", "child", "children in mothers in communities", no, no,
    "mostly one child per mother", "how communities and families were sampled",
    c(community = 160, mother = 1434, child = 564, children = 2159),
    c(n_of(d, "comm") - 1, n_of(d, "comm", "mom") - n_of(d, "comm"), nrow(d) - n_of(d, "comm", "mom"),
      sum(seq_along(table(table(d$mom))) * table(table(d$mom)))))
# H13: 148 primary by 19 secondary schools, 303 pairs seen. Extra memberships
# beyond one secondary per primary = 303 - 148; additive crossed error =
# 3435 - 1 - 147 - 18 (if the pairs connect all schools)
key("H13", "mlmRev::ScotsSec", "silent", "nothing", "pupil", no, "primary by secondary school, partly (303 of 2812 pairs)", no,
    "a primary school sends pupils to 1 to 6 secondaries", "how pupils were chosen",
    c(pairs = 303, extra = 155, error = 3269),
    { p <- unique(d[c("primary", "second")])
      c(nrow(p), sum(table(p$primary) - 1), stats::df.residual(stats::lm(attain ~ primary + second, d))) })
# H14: counts only (73421 rows). Each pair of student and lecturer at most once
key("H14", "lme4::InstEval", "silent", "nothing", "rating", "lecturers in departments", "students by lecturers, partly",
    "a student rates 1 to 92 lectures", "",
    "Source not named on the help page or in the package documents; how students came to rate is not said; data anonymised and simplified on purpose",
    c(students = 2972, lecturers = 1128, ratings = 73421, repeated_pairs = 0, lecturers_in_two_depts = 0),
    c(n_of(d, "s"), n_of(d, "d"), nrow(d), nrow(d) - n_of(d, "s", "d"), n_of(d, "d", "dept") - n_of(d, "d")))
# H15: 3022 workers; memberships 2496 x 1 + 472 x 2 + 52 x 3 + 2 x 4 = 3604
key("H15", "R2MLwiN::wage1", "silent", "nothing (simulated)", "worker", no, "workers belong to 1 to 4 firms", no,
    "weights give the share of the year in each firm", "simulated data",
    c(workers = 3022, memberships = 3604, with_2nd = 526, with_3rd = 54, with_4th = 2),
    c(nrow(d), sum(d$company > 0, d$company2 > 0, d$company3 > 0, d$company4 > 0),
      sum(d$company2 > 0), sum(d$company3 > 0), sum(d$company4 > 0)))
# H16: 3 experiments x 20 females x 6 trials = 360; 120 per experiment / 4 crosses = 30
key("H16", "lme4::salamander", "partly", "pairings set by the experimenters; each animal mates several partners", "mating trial",
    "animals in experiments", "females by males, partly, inside each experiment", "each female 6 trials, each male 6",
    "30 trials per cross type per experiment", "how partners were chosen; whether experiments 2 and 3 reused animals (the data number them apart)",
    c(trials = 360, per_female = 6, per_male = 6, per_cross_experiment = 30, repeated_pairs = 0),
    c(nrow(d), unique(as.vector(table(d$Female))), unique(as.vector(table(d$Male))),
      unique(as.vector(table(d$Cross, d$Experiment))), nrow(d) - n_of(d, "Experiment", "Female", "Male")))
# H17: counts only. New pupils per grade 6325 + 2313 + 1679 + 1281 = 11598; rows
# 1 x 4314 + 2 x 2455 + 3 x 1744 + 4 x 3085 = 26796. (Corrected 2026-11-26,
# after drawing began: the first version of the limits text said 456 and
# 2745 pupils; those are extra pupil-school and pupil-type pairs. A data-mode
# warning while drawing showed it. The checked numbers did not change.)
key("H17", "mlmRev::star", "silent", "class type (small, regular, regular with aide) to class within school, as the list reads it; not on the help page",
    "pupil-year", "years in pupils; pupils in classes in schools, changing", "class type by school", "1 to 4 years per pupil; pupils enter in later grades",
    "446 pupils in more than one school, 2610 in more than one class type",
    "The list says randomised inside schools; the help page and the package vignette do not say so. Teacher ids were built from teacher traits (vignette)",
    c(pupils = 11598, rows = 26796, schools = 80),
    { f <- tapply(as.integer(d$gr), d$id, min); y <- table(table(d$id))
      c(length(f), sum(as.integer(names(y)) * y), n_of(d, "sch")) })
# H18: 9 sites x 3 blocks x 4 treatments. error = 9 x 2 x 3; blocks 27
key("H18", "SASmixed::Multilocation", "silent", "treatment to plot within block", "plot", "plots in blocks in sites",
    "treatment by site", no, "every treatment once in every block; block numbers repeat across sites",
    "whether random within block",
    c(blocks = 27, error = 54),
    c(n_of(d, "Location", "Block"), stats::df.residual(stats::lm(Adj ~ Location * Trt + Location:Block, d))))
# H19: 13 trials; between-trial df 12
key("H19", "HSAUR3::BCG", "partly", "vaccine within each trial (how is not said)", "trial arm (counts)", "people in arms in trials", no, no,
    "one row per trial", "how each trial assigned vaccine; trial 8 has equal arm sizes (88391)",
    c(trials = 13, q_df = 12, vaccinated = 191064, not_vaccinated = 166283),
    { lor <- log(d$BCGTB / (d$BCGVacc - d$BCGTB)) - log(d$NoVaccTB / (d$NoVacc - d$NoVaccTB))
      c(nrow(d), stats::df.residual(stats::lm(lor ~ 1)), sum(d$BCGVacc), sum(d$NoVacc)) })
# H20: 15 herds x 4 periods = 60 planned, 56 seen
key("H20", "lme4::cbpp", "partly", "nothing; a follow-up survey of 15 herds, all animals sampled quarterly", "herd-period (counts)",
    "periods in herds", no, "1 to 4 periods per herd", "rows are herds, not animals; herd size changes",
    "how the 15 herds were chosen; why 4 herd-periods are missing ('lost to follow-up')",
    c(herds = 15, rows = 56, missing = 4),
    c(n_of(d, "herd"), nrow(d), sum(table(d$herd, d$period) == 0)))

# I. Observational, categorical, time to event ------------------------------------------
# I04: 2691 + 1835 = 4526 applicants (help page); 6 departments
key("I04", "datasets::UCBAdmissions", "stated", "nothing; applicants to the six largest departments, 1973", "applicant", "applicants in departments",
    "sex by admission, within department", no, "", "",
    c(applicants = 4526, male = 2691, female = 1835, male_admitted = 1198, female_admitted = 557, departments = 6),
    { m <- margin.table(d, c(1, 2)); c(sum(d), sum(m[, "Male"]), sum(m[, "Female"]), m["Admitted", "Male"], m["Admitted", "Female"], dim(d)[3]) })
# I05: 189 births; one exposure, residual df 187
key("I05", "MASS::birthwt", "silent", "nothing; births at one hospital, 1986", "birth", no, no, no,
    "", "how the 189 were chosen (whether all births)",
    c(births = 189, error = 187),
    c(nrow(d), stats::df.residual(stats::glm(low ~ smoke, binomial, d))))
# I07: phase 2 = subcohort 668 + cases 571 - both 85 = 1154; rest 4028 - 1154 = 2874
key("I07", "survival::nwtco", "partly", "nothing; a subcohort plus all relapses (case-cohort)", "patient", no, "subcohort and cases overlap", no,
    "85 are in both", "how the subcohort was sampled ('for the example in the paper')",
    c(cohort = 4028, subcohort = 668, cases = 571, both = 85, phase2 = 1154, rest = 2874),
    c(nrow(d), sum(d$in.subcohort), sum(d$rel), sum(d$in.subcohort & d$rel == 1),
      sum(d$in.subcohort | d$rel == 1), sum(!d$in.subcohort & d$rel == 0)))
# I08: Jan 1969 to Dec 1984 = 16 x 12; law from Feb 1983: 14 x 12 + 1 before
key("I08", "datasets::Seatbelts", "stated", "nothing; a law in force from 31 Jan 1983", "month", no, no, no,
    "", "",
    c(months = 192, before = 169, after = 23),
    c(nrow(d), sum(d[, "law"] == 0), sum(d[, "law"] == 1)))
# I09: 2 states x 2 earner groups x 2 periods = 8 cells; error 7150 - 8
key("I09", "wooldridge::injury", "partly", "nothing; a change in benefits for high earners (natural experiment, in the source title)", "claim",
    no, "state by earner group by period", no, "different claims before and after", "who is in the data",
    c(claims = 7150, cells = 8, error = 7142),
    c(nrow(d), n_of(d, "ky", "highearn", "afchnge"), stats::df.residual(stats::lm(ldurat ~ factor(ky) * factor(highearn) * factor(afchnge), d))))
# I10: 92 days in each of 2 years = 184; limit 21 + 48 = 69 days. error 184 - 92 - 1 - 1
key("I10", "MASS::Traffic", "partly", "speed limit on some days, in runs; how is not said", "day", "days in matched pairs", "year by limit", no,
    "day j of 1961 matched with day j of 1962", "how the limit days were chosen (not random as far as the page says)",
    c(days = 184, pairs = 92, with_limit = 69, error = 90),
    c(nrow(d), n_of(d, "day"), sum(d$limit == "yes"), stats::df.residual(stats::lm(y ~ factor(day) + factor(year) + limit, d))))

# J. Sample surveys ---------------------------------------------------------------------
# J05: 12 of 187 classes (help page); every student of a chosen class, so
# students = sum of class sizes
key("J05", "SDAResources::algebra", "stated", "nothing; simple random sample of 12 of 187 classes", "student", "students in classes", no, no,
    "all students of each chosen class", "fictional data",
    c(classes = 12, students = 299, classes_all_taken = 12),
    c(n_of(d, "class"), nrow(d), sum(table(d$class) == unique(d[c("class", "Mi")])$Mi[match(names(table(d$class)), unique(d[c("class", "Mi")])$class)])))
# J06: 184 clutches x 2 eggs
key("J06", "SDAResources::coots", "partly", "nothing; 2 eggs subsampled from each clutch; some clutches had extra feeding", "egg", "eggs in clutches", no, no,
    "clutches of 5 to 13 eggs", "how clutches and eggs were chosen; how feeding was given (53 of 184 clutches fed; not in the list)",
    c(clutches = 184, eggs = 368),
    c(n_of(d, "clutch"), nrow(d)))

# K. Special field designs ---------------------------------------------------------------
# K01: 8 x 8 crosses (with selfs and reciprocals) x 2 blocks x 2 years = 256;
# per year 128 plots - mean 1 - block 1 - crosses 63 = 63
key("K01", "agridat::hayman.tobacco", "partly", "crosses made by design; each cross in 2 plots of 5 plants", "plot (mean of 5 plants)",
    "plots in blocks in years", "male by female parent, full (8 x 8)", "every line as male and as female", "",
    "randomisation of plots within blocks",
    c(plots = 256, crosses = 64, error_1951 = 63, error_1952 = 63),
    { x <- fac(d, "male", "female", "block")
      c(nrow(x), n_of(x, "male", "female"), sapply(c(1951, 1952), function(y)
        stats::df.residual(stats::lm(day ~ block + male:female, x[x$year == y, ])))) })
# K02: 20 years x 6 rotations x 4 nitrogen rates; one row per combination
key("K02", "agridat::payne.wheat", "silent", "rotation and nitrogen to plots (as the list reads it)", "rotation-nitrogen-year",
    no, "rotation by nitrogen by year", "the same treatments every year", "one row per combination, a subset of the published data",
    "number of plots, blocks and whether rows are plot values or means is not said",
    c(rows = 480, per_cell = 1),
    c(nrow(d), unique(as.vector(table(d$rotation, d$nitro, d$year)))))
# K03: 31 columns x 14 rows = 434 plots; check in columns 1, 4, ..., 31: 11 x 14 = 154;
# new lines 434 - 154 = 280
key("K03", "agridat::burgueno.unreplicated", "partly", "new lines once each; one check line in fixed columns", "plot", no, "rows by columns", no,
    "280 new lines once, the check 154 times",
    "The help page says the check is in every 4th column; the data have it in every 3rd (columns 1, 4, ..., 31). Placement of new lines not said",
    c(plots = 434, check_plots = 154, new_once = 280, check_columns = 11),
    c(nrow(d), sum(d$gen == "G000"), sum(table(d$gen[d$gen != "G000"]) == 1), n_of(d[d$gen == "G000", ], "col")))

# Check and write ----------------------------------------------------------------
designs <- utils::read.csv("notes/textbook-designs.csv", encoding = "UTF-8")
want <- c("G04", "G06", "G07", "G08", "G09", "G10", "G11", "G12",
          sprintf("H%02d", 8:20), "I04", "I05", "I07", "I08", "I09", "I10",
          "J05", "J06", "K01", "K02", "K03")
ids <- vapply(K, `[[`, "", "id")
stopifnot(length(want) == 32, !anyDuplicated(ids), setequal(ids, want),
          all(designs$tier[match(ids, designs$id)] %in% c("extended", "edge")))
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
con <- file("notes/textbook-ext-keys-GK.csv", "w", encoding = "UTF-8")
utils::write.csv(out, con, row.names = FALSE)
close(con)
cat("\nkeys:", nrow(out), "; agree:", sum(out$agrees), "\n")
cat("what the help page says about assignment or selection:\n")
print(table(factor(out$source, c("stated", "partly", "silent"))))
cat("keys with something left open:", sum(nzchar(out$open)), "\n")
cat("\nR", R.version.string, "\n")
for (p in unique(sub("::.*", "", out$data))) cat(p, as.character(utils::packageVersion(p)), "\n")
