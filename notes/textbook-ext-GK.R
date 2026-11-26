# Draw the extended and edge textbook designs of families G to K (milestone
# 6b, step 5; 32 designs).
#
# Run from the package folder:
#   Rscript --vanilla notes/textbook-ext-GK.R <folder for the blind readers>
# As notes/textbook-core.R: version "p" is plain use of the verbs (now with
# flow_copy() where the same units go into every branch); version "c" (only
# where wording could change what a reader understands) is careful wording
# with the same verbs. No package code is changed. The word "randomised" or a
# random method is used only where the help page states it
# (notes/textbook-ext-keys-GK.csv, column source). Counts are computed here
# from the data, or read from the help page text, never typed.
#
# Output: coded pictures and descriptions for the readers, and the list of
# codes in notes/textbook-ext-codes-GK.csv (never shown to a reader). Set R
# holds one version of every design, set S the other version of those that
# have two; text/descriptions.txt holds the descriptions of the set R
# versions as V01.. (same number as the R code). Printed: flow_check() of
# every flow, the data-mode counting traps, and the widths.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
suppressMessages(pkgload::load_all(quiet = TRUE, export_all = FALSE))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_part <-", src) - 1)]))  # get_data(), help_page()
out_dir <- commandArgs(TRUE)[1]
stopifnot(!is.na(out_dir))
for (s in c("R", "S", "text")) dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)

D <- list()
add <- function(id, p, c = NULL) D[[id]] <<- list(p = p, c = c)
# a number written on a help page, found by a pattern with one group
help_num <- function(s, pattern) {
  t <- gsub("\\s+", " ", paste(help_page(s), collapse = " "))
  stopifnot(grepl(pattern, t))
  as.integer(sub(paste0(".*", pattern, ".*"), "\\1", t))
}
rng <- function(x) paste(min(x), "to", max(x))
fmt <- function(x) format(x, big.mark = ",")
ids_of <- function(x, stage, labels) {
  nd <- x$nodes; st <- x$stages$id[x$stages$name == stage]
  nd$id[nd$stage == st & nd$label %in% labels]
}

# G ---------------------------------------------------------------------------------
ok <- get_data("carData::OBrienKaiser")
t4 <- table(ok$treatment, ok$gender)
add("G04",
  flow_start("People", n = nrow(ok)) |>
    flow_split("Treatment received", c("Control", "A", "B"), n = as.integer(rowSums(t4))) |>
    flow_split("Gender", c("Female", "Male"), n = lapply(1:3, function(i) as.integer(t4[i, ]))) |>
    flow_copy("Session", c("Pretest", "Posttest", "Follow-up")) |>
    flow_outcome("Measured", "Score at\nhours 1 to 5"),
  flow_start("People", n = nrow(ok)) |>
    flow_split("Treatment received", c("Control", "A", "B"), n = as.integer(rowSums(t4)),
               note = sprintf("%d women, %d men", t4[, "F"], t4[, "M"])) |>
    flow_copy("Session: every person in all 3", c("Pretest", "Posttest", "Follow-up")) |>
    flow_outcome("Measured", "5 scores, one\nper hour"))

bq <- get_data("daewr::bioequiv")
seq6 <- tapply(paste(bq$Treat), droplevels(bq$Subject), paste, collapse = "")
g6 <- as.integer(table(factor(seq6, c("ABB", "BAA"))))
add("G06",
  flow_start("People", n = length(seq6)) |>
    flow_split("Sequence received", c("A, B, B", "B, A, A"), n = g6) |>
    flow_apply("Period 1", c("Drug A", "Drug B"), n = g6) |>
    flow_apply("Period 2", c("Drug B", "Drug A"), n = g6) |>
    flow_apply("Period 3", c("Drug B", "Drug A"), n = g6) |>
    flow_outcome("Measured", "Response in each period"),
  flow_start("People", n = length(seq6)) |>
    flow_split("Sequence received", c("A, then B, then B", "B, then A, then A"), n = g6) |>
    flow_apply("Period 1", c("Drug A", "Drug B"), n = g6, note = "the same people\nin every period") |>
    flow_apply("Period 2", c("Drug B", "Drug A"), n = g6) |>
    flow_apply("Period 3 (extra period)", c("Drug B again", "Drug A again"), n = g6) |>
    flow_outcome("Measured", "Response, once\nper period"))

ch <- get_data("daewr::chipman")
ch <- ch[order(ch$Square, ch$Group, ch$Subject, ch$Period), ]
o7 <- unique(data.frame(sq = ch$Square, g = ch$Group,
  s = ave(as.character(ch$Treat), ch$Subject, FUN = function(v) paste(v, collapse = ", "))))
n7 <- as.integer(table(unique(ch[c("Square", "Group", "Subject")])[c("Square", "Group")]))
stopifnot(length(unique(n7)) == 1)
per7 <- lapply(split(o7$s, o7$sq), function(v) paste("Treatments", v))
trt7 <- function(k) paste("Treatment", sapply(strsplit(o7$s, ", "), `[`, k))
add("G07",
  flow_start("People", n = nlevels(ch$Subject)) |>
    flow_split("Square", c("Square 1", "Square 2"), n = as.integer(table(unique(ch[c("Square", "Subject")])$Square))) |>
    flow_split("Sequence received", lapply(split(o7$s, o7$sq), function(v) paste("Order", v)), n = n7[1]) |>
    flow_apply("Period 1", trt7(1), n = n7[1]) |>
    flow_apply("Period 2", trt7(2), n = n7[1]) |>
    flow_apply("Period 3", trt7(3), n = n7[1]) |>
    flow_outcome("Measured", "Sprint\ntime"),
  flow_start("People", n = nlevels(ch$Subject)) |>
    flow_split("Williams square", c("Square 1", "Square 2"), n = as.integer(table(unique(ch[c("Square", "Subject")])$Square)),
               note = "each treatment follows\nevery other once") |>
    flow_split("Order of treatments received", per7, n = n7[1]) |>
    flow_outcome("Measured", "Sprint time,\nin each of\n3 periods"))

am <- get_data("AMCP::chapter_12_table_29")
am <- am[order(am$Square, am$Subject, am$Time), ]
o8 <- tapply(am$Condition, am$Subject, paste, collapse = ", ")
sq8 <- tapply(am$Square, am$Subject, unique)
add("G08",
  flow_start("Subjects", n = length(o8)) |>
    flow_assign("Latin square", c("Square 1", "Square 2"), n = as.integer(table(sq8)), method = "random") |>
    flow_assign("Order (row of the square)", lapply(split(o8, sq8), function(v) paste("Drugs", v)), n = 1, method = "random") |>
    flow_outcome("Measured", "Aggressive acts\nat times 1, 2, 3"),
  flow_start("Subjects (each a pair of monkeys)", n = length(o8)) |>
    flow_assign("Latin square", c("Square 1", "Square 2"), n = as.integer(table(sq8)), method = "random",
                note = "each drug once\nat each time") |>
    flow_assign("Order of drugs (row of the square)", lapply(split(o8, sq8), function(v) paste("Drugs", v)), n = 1,
                method = "random") |>
    flow_outcome("Measured", "Aggressive acts in\neach of 3 times"))

fh <- get_data("afex::fhch2010")
fh$Task <- factor(fh$task, c("naming", "lexdec"), c("Naming aloud", "Lexical decision"))
fh$Stimulus <- factor(fh$stimulus, c("word", "nonword"), c("Words", "Nonwords"))
t9 <- table(unique(fh[c("id", "Task")])$Task)
trials <- fh  # the description names the data frame; keep the data set's name out of it
seen9 <- tapply(fh$item, list(fh$id, fh$Stimulus), function(v) length(unique(v)))
it9 <- table(unique(fh[c("item", "Stimulus")])$Stimulus)
add("G09",
  flow_start("People", data = trials, count = "id") |>
    flow_split("Task", by = "Task") |>
    flow_copy("Stimulus", by = "Stimulus") |>
    flow_outcome("Measured", "Response time\nper trial"),
  flow_start("People", n = sum(t9)) |>
    flow_split("Task, between people", names(t9), n = as.integer(t9)) |>
    flow_copy("Items, within people", c("Words", "Nonwords"),
              note = sprintf("up to %d of the %d", max(seen9), it9[[1]])) |>
    flow_outcome("Measured", "Response time per trial",
                 note = sprintf("the same %d items\nin both tasks", sum(it9))))

ant <- get_data("ez::ANT")
ant$Cue <- ant$cue
trials10 <- ant  # the description names the data frame; keep the data set's name out of it
tr10 <- unique(as.vector(table(ant$subnum)))
nc10 <- nlevels(ant$cue); nf10 <- nlevels(ant$flank); nb10 <- length(unique(ant$block))
add("G10",
  flow_start("People", data = trials10, count = "subnum") |>
    flow_split("Group", by = "group") |>
    flow_copy("Cue", by = "Cue") |>
    flow_outcome("Measured", sprintf("Response time,\n%d trials per person", tr10 / nc10)),
  flow_start("People", n = nlevels(ant$subnum)) |>
    flow_split("Group", levels(ant$group), n = as.integer(table(unique(ant[c("subnum", "group")])$group))) |>
    flow_apply("Trials", sprintf("%d trials per person:\n%d blocks of %d", tr10, nb10, tr10 / nb10),
               n = as.integer(table(unique(ant[c("subnum", "group")])$group))) |>
    flow_outcome("Analysed", sprintf("Mean response time per person\nin each of %d cells (%d cues x %d\nflankers, %d trials in each)",
                                     nc10 * nf10, nc10, nf10, tr10 / (nc10 * nf10)),
                 n = as.integer(table(unique(ant[c("subnum", "group")])$group))))

va <- get_data("lme4::VerbAgg")
g11 <- table(unique(va[c("id", "Gender")])$Gender)
b11 <- table(unique(va[c("item", "btype")])$btype)
stopifnot(length(unique(b11)) == 1)
add("G11",
  flow_start("People", n = nlevels(va$id)) |>
    flow_split("Gender", c("Female", "Male"), n = as.integer(g11[c("F", "M")])) |>
    flow_copy("Behaviour asked about", c("Curse", "Scold", "Shout")) |>
    flow_outcome("Measured", sprintf("Answers to %d items:\nwant or do, 4 situations", b11[[1]])),
  flow_start("People", n = nlevels(va$id), note = sprintf("%d women, %d men", g11[["F"]], g11[["M"]])) |>
    flow_copy(sprintf("Items: every person answers all %d", nlevels(va$item)), c("Curse", "Scold", "Shout"),
              note = sprintf("%d items each", b11[[1]])) |>
    flow_outcome("Measured", "No, perhaps or yes,\nper person and item"))

rb <- get_data("WWGbook::rat.brain")
add("G12",
  flow_start("Rats", n = nlevels(rb$animal)) |>
    flow_copy("Drug", c("Basal", "Carbachol")) |>
    flow_copy("Brain region", c("BST", "LS", "VDB")) |>
    flow_outcome("Measured", "Nucleotide\nactivation"))

# H ---------------------------------------------------------------------------------
rs <- get_data("geepack::respiratory")
pc <- table(unique(rs[c("center", "id", "treat")])[c("center", "treat")])
add("H08",
  flow_start("Patients", n = sum(pc)) |>
    flow_split("Clinic", c("Clinic 1", "Clinic 2"), n = as.integer(rowSums(pc))) |>
    flow_assign("Treatment", c("Active", "Placebo"), n = lapply(1:2, function(i) as.integer(pc[i, c("A", "P")])), method = "random") |>
    flow_apply("Baseline", "Breathing status", n = as.integer(t(pc[, c("A", "P")]))) |>
    flow_outcome("Visits 1 to 4", "Breathing status\nat each visit", n = as.integer(t(pc[, c("A", "P")]))),
  flow_start("Patients", n = sum(pc)) |>
    flow_split("Clinic", c("Clinic 1", "Clinic 2"), n = as.integer(rowSums(pc)),
               note = "patients numbered\nfrom 1 in each clinic") |>
    flow_assign("Treatment", c("Active", "Placebo"), n = lapply(1:2, function(i) as.integer(pc[i, c("A", "P")])), method = "random") |>
    flow_apply("Baseline", "Breathing status", n = as.integer(t(pc[, c("A", "P")]))) |>
    flow_outcome("Visits 1 to 4", "Breathing status at\neach of 4 visits,\nthe same patients", n = as.integer(t(pc[, c("A", "P")]))))

te <- get_data("HSAUR3::toenail")
n378 <- help_num("HSAUR3::toenail", "A total of ([0-9]+) patients")
arm9 <- table(unique(te[c("patientID", "treatment")])$treatment)
vt9 <- table(te$visit, te$treatment)
wk9 <- c(0, 4, 8, 12, 24, 36, 48)
stopifnot(grepl("weeks 0, 4, 8, 12, 24, 36, and 48", paste(help_page("HSAUR3::toenail"), collapse = " ")))
h09 <- function(careful) {
  x <- flow_start("Patients randomised", n = n378) |>
    flow_exclude(if (careful) "Not in the data\n(reason not given)" else "Not in the data", n = n378 - sum(arm9)) |>
    flow_assign("Treatment", c("Itraconazole", "Terbinafine"), n = as.integer(arm9), method = "random",
                note = if (careful) sprintf("of the %d randomised", n378))
  for (v in 1:7) {
    f <- if (v < 7) flow_apply else flow_outcome
    x <- f(x, sprintf("Visit %d (week %d)", v, wk9[v]), if (careful) "Seen" else "Nail assessed", n = as.integer(vt9[v, ]))
  }
  x
}
add("H09", h09(FALSE), h09(TRUE))

eg <- get_data("mlmRev::egsingle")
add("H10",
  flow_start("Schools", n = nlevels(eg$schoolid)) |>
    flow_apply("Pupils", paste(rng(table(unique(eg[c("schoolid", "childid")])$schoolid)), "per school"), n = nlevels(eg$childid)) |>
    flow_outcome("Measured", paste0("Maths score, ", rng(table(eg$childid)), "\nyears per pupil"), n = nrow(eg)),
  flow_start("Schools", n = nlevels(eg$schoolid)) |>
    flow_apply("Pupils", paste(rng(table(unique(eg[c("schoolid", "childid")])$schoolid)), "per school"), n = nlevels(eg$childid)) |>
    flow_outcome("Measured", paste0("Maths score once a year,\n", rng(table(eg$childid)), " years per pupil"),
                 n = nlevels(eg$childid), note = paste(fmt(nrow(eg)), "scores in all")))

vn <- get_data("WWGbook::veneer")
add("H11",
  flow_start("Patients", n = length(unique(vn$patient))) |>
    flow_apply("Teeth", paste(rng(table(unique(vn[c("patient", "tooth")])$patient)), "per patient"), n = nrow(unique(vn[c("patient", "tooth")]))) |>
    flow_copy("Time after placement", paste(sort(unique(vn$time)), "months")) |>
    flow_outcome("Measured", "Gum fluid next\nto the tooth"))

gu <- get_data("mlmRev::guImmun")
h12 <- function(careful) flow_start("Communities", n = nlevels(gu$comm)) |>
  flow_apply("Mothers", paste(rng(table(unique(gu[c("comm", "mom")])$comm)), "per community"), n = nlevels(gu$mom)) |>
  flow_apply("Children", paste(rng(table(gu$mom)), "per mother"), n = nrow(gu),
             note = if (careful) "only children with at\nleast one immunisation") |>
  flow_outcome("Recorded", "Complete set of\nimmunisations or not")
add("H12", h12(FALSE), h12(TRUE))

sc <- get_data("mlmRev::ScotsSec")
pp13 <- unique(sc[c("primary", "second")])
add("H13",
  flow_start("Secondary schools", n = nlevels(sc$second)) |>
    flow_apply("Primary schools", "Primary schools", n = nlevels(sc$primary)) |>
    flow_apply("Pupils", "Pupils", n = nrow(sc)) |>
    flow_outcome("Measured", "Attainment at 16"),
  flow_start(c("Primary schools", "Secondary schools"), n = c(nlevels(sc$primary), nlevels(sc$second))) |>
    flow_pool("Pupils", "Pupils, each from one primary\nand in one secondary school", n = nrow(sc),
              note = sprintf("%d primary-secondary pairs;\na primary sends pupils to\n%s secondaries",
                             nrow(pp13), rng(table(pp13$primary)))) |>
    flow_outcome("Measured", "Attainment at 16"))

ie <- get_data("lme4::InstEval")
add("H14",
  flow_start("Students", n = nlevels(ie$s)) |>
    flow_apply("Ratings", paste(rng(table(ie$s)), "lectures\nrated per student"), n = nrow(ie)) |>
    flow_outcome("Measured", "Rating, 1 to 5"),
  flow_start(c("Students", "Lecturers"), n = c(nlevels(ie$s), nlevels(ie$d)),
             note = c("", sprintf("in %d departments", nlevels(ie$dept)))) |>
    flow_pool("Ratings", "Ratings: a student rates the\nlectures of some lecturers", n = nrow(ie),
              note = "each student and lecturer\nat most once") |>
    flow_outcome("Measured", "Rating, 1 to 5"))

wg <- get_data("R2MLwiN::wage1")
nj <- as.integer(table(factor(wg$numjobs, 1:4)))
nf15 <- length(unique(c(wg$company, wg$company2, wg$company3, wg$company4)[c(wg$company, wg$company2, wg$company3, wg$company4) > 0]))
firms <- c("1 firm", "2 firms", "3 firms", "4 firms")
add("H15",
  flow_start("Workers", n = nrow(wg)) |>
    flow_split("Firms worked for in the year", firms, n = nj) |>
    flow_outcome("Measured", "Earnings"),
  flow_start("Workers", n = nrow(wg)) |>
    flow_split("Firms worked for in the year", firms, n = nj) |>
    flow_apply("Memberships", "Worker-firm\nmemberships", n = nj * 1:4) |>
    flow_pool("Firms", sprintf("Memberships in %d firms", nf15), n = sum(nj * 1:4),
              note = "earnings are per worker,\nshared out by time\nin each firm"))

sl <- get_data("lme4::salamander")
pf <- unique(as.vector(table(sl$Female))); pm <- unique(as.vector(table(sl$Male)))
ex16 <- length(unique(sl$Experiment)); fe16 <- unique(as.vector(table(unique(sl[c("Experiment", "Female")])$Experiment)))
cr16 <- unique(as.vector(table(sl$Cross, sl$Experiment)))
nF <- length(unique(sl$Female)); nM <- length(unique(sl$Male))
tf <- as.integer(table(unique(sl[c("Female", "TypeF")])$TypeF))
add("H16",
  flow_start("Females", n = nF) |>
    flow_split("Type of female", c("Rough butt", "White side"), n = tf) |>
    flow_apply("Mating trials", sprintf("%d trials per female,\neach with a different male", pf), n = tf * pf) |>
    flow_outcome("Recorded", "Mated or not"),
  flow_start(c("Females", "Males"), n = c(nF, nM)) |>
    flow_split("Type", c("Rough butt", "White side"),
               n = list(tf, as.integer(table(unique(sl[c("Male", "TypeM")])$TypeM)))) |>
    flow_pool("Mating trials", "Mating trials, each a\nfemale with a male", n = nrow(sl),
              note = sprintf("every female meets %d males,\nevery male %d females;\n%d experiments of %d + %d;\n%d trials of each cross\ntype per experiment",
                             pf, pm, ex16, fe16, fe16, cr16)) |>
    flow_outcome("Recorded", "Mated or not"))

st <- get_data("mlmRev::star")
st <- st[order(st$id, st$gr), ]
ent <- st[!duplicated(st$id), ]
te17 <- table(ent$gr, ent$cltype)
moved_sch <- sum(tapply(st$sch, st$id, function(v) length(unique(v))) > 1)
moved_typ <- sum(tapply(st$cltype, st$id, function(v) length(unique(v))) > 1)
grades <- c("Kindergarten", "Grade 1", "Grade 2", "Grade 3")
types <- c("Small", "Regular", "Regular + aide")
h17 <- function(careful) flow_start("Pupils", n = nlevels(st$id),
    note = if (careful) sprintf("%d schools; followed to grade 3\nor until they left; %s changed\nschool, %s changed class type", nlevels(st$sch), fmt(moved_sch), fmt(moved_typ))
           else sprintf("%d schools", nlevels(st$sch))) |>
  flow_split("Grade at entry", grades, n = as.integer(rowSums(te17))) |>
  flow_split("Class type received at entry", types, n = lapply(1:4, function(i) as.integer(te17[i, ]))) |>
  flow_outcome("Measured", "Reading and\nmaths each year")
add("H17", h17(FALSE), h17(TRUE))

ml <- get_data("SASmixed::Multilocation")
nb18 <- nrow(unique(ml[c("Location", "Block")]))
add("H18",
  flow_start("Sites", n = nlevels(ml$Location)) |>
    flow_apply("Blocks", paste(nlevels(ml$Block), "per site"), n = nb18) |>
    flow_copy("Treatment", paste("Treatment", levels(ml$Trt))) |>
    flow_outcome("Measured", "Response of\nthe plot"),
  flow_start("Sites", n = nlevels(ml$Location)) |>
    flow_apply("Blocks", sprintf("%d blocks at each site\n(numbered 1 to %d at every site)", nlevels(ml$Block), nlevels(ml$Block)), n = nb18) |>
    flow_apply("Plots", sprintf("%d per block, one\nfor each treatment", nlevels(ml$Trt)), n = nrow(ml)) |>
    flow_outcome("Measured", "Response of the plot", n = nrow(ml)))

bc <- get_data("HSAUR3::BCG")
add("H19",
  flow_start("Trials", n = nrow(bc)) |>
    flow_split("Arm", c("Vaccinated", "Not vaccinated"), n = c(sum(bc$BCGVacc), sum(bc$NoVacc))) |>
    flow_outcome("Recorded", "Tuberculosis cases", n = c(sum(bc$BCGTB), sum(bc$NoVaccTB))),
  flow_start("Trials", n = nrow(bc)) |>
    flow_copy("Arms, in every trial", c("Vaccinated", "Not vaccinated")) |>
    flow_apply("People", "People", n = c(sum(bc$BCGVacc), sum(bc$NoVacc))) |>
    flow_outcome("Recorded", "Tuberculosis cases", n = c(sum(bc$BCGTB), sum(bc$NoVaccTB))))

cb <- get_data("lme4::cbpp")
per20 <- as.integer(table(cb$period))
add("H20",
  flow_start("Herds", n = nlevels(cb$herd)) |>
    flow_copy("Period", paste("Period", levels(cb$period)), n = per20) |>
    flow_outcome("Recorded", "New cases\nin the herd"),
  flow_start("Herds", n = nlevels(cb$herd), note = "all animals sampled\nevery quarter") |>
    flow_copy("Period", paste("Period", levels(cb$period)), n = per20,
              note = sprintf("%d animals", as.integer(tapply(cb$size, cb$period, sum)))) |>
    flow_outcome("Recorded", "New cases\nin the herd"))

# I ---------------------------------------------------------------------------------
ucb <- get_data("datasets::UCBAdmissions")
dg <- margin.table(ucb, c(3, 2))
add("I04",
  flow_start("Applicants", n = sum(ucb)) |>
    flow_split("Department applied to", paste("Dept", dimnames(ucb)$Dept), n = as.integer(rowSums(dg))) |>
    flow_split("Sex", c("Men", "Women"), n = lapply(seq_len(nrow(dg)), function(i) as.integer(dg[i, ]))) |>
    flow_outcome("Decision", "Admitted", n = as.integer(ucb["Admitted", , ])))

bw <- get_data("MASS::birthwt")
add("I05",
  flow_start("Births", n = nrow(bw), note = "one hospital, 1986") |>
    flow_split("Mother smoked in pregnancy", c("Yes", "No"), n = c(sum(bw$smoke == 1), sum(bw$smoke == 0))) |>
    flow_outcome("Recorded", "Birth weight\nunder 2.5 kg", n = c(sum(bw$low[bw$smoke == 1]), sum(bw$low[bw$smoke == 0]))))

nw <- get_data("survival::nwtco")
t7 <- table(nw$in.subcohort, nw$rel)
i07 <- flow_start("Patients", n = nrow(nw)) |>
  flow_split("Subcohort", c("In the subcohort", "Not in it"), n = c(sum(t7["TRUE", ]), sum(t7["FALSE", ]))) |>
  flow_split("Relapse", c("Relapsed", "No relapse"), n = list(as.integer(t7["TRUE", c("1", "0")]), as.integer(t7["FALSE", c("1", "0")])))
rel_ids <- i07$nodes$id[i07$nodes$stage == max(i07$stages$id)][1:3]
add("I07", i07,
  i07 |> flow_pool("Case-cohort sample", "Subcohort plus\nall relapses", n = sum(nw$in.subcohort | nw$rel == 1), from = rel_ids))

sb <- get_data("datasets::Seatbelts")
law <- sb[, "law"]
mon <- function(i) format(as.Date(sprintf("%d-%02d-01", floor(time(sb)[i] + 1e-6), cycle(sb)[i])), "%b %Y")
first_law <- which(law == 1)[1]
add("I08",
  flow_start("Months", n = length(law), note = paste(mon(1), "to", mon(length(law)))) |>
    flow_split("Seat belt law", c("Before the law", "Law in force"), n = c(sum(law == 0), sum(law == 1)),
               note = c(paste(mon(1), "to", mon(first_law - 1)), paste(mon(first_law), "to", mon(length(law))))) |>
    flow_outcome("Recorded", "Drivers killed or\nseriously injured"))

ij <- get_data("wooldridge::injury")
ij$st <- factor(ij$ky, 1:0, c("Kentucky", "Michigan"))
ij$hi <- factor(ij$highearn, 1:0, c("High", "Low"))
ij$af <- factor(ij$afchnge, 0:1)
c9 <- table(ij$st, ij$hi, ij$af)
n9a <- lapply(1:2, function(s) as.integer(rowSums(c9[s, , ])))
n9b <- lapply(1:4, function(k) { s <- (k - 1) %/% 2 + 1; h <- (k - 1) %% 2 + 1; as.integer(c9[s, h, ]) })
add("I09",
  flow_start("Claims", n = nrow(ij)) |>
    flow_split("State", levels(ij$st), n = as.integer(table(ij$st))) |>
    flow_split("Earnings", levels(ij$hi), n = n9a) |>
    flow_split("Injured", c("Before the change", "After the change"), n = n9b) |>
    flow_outcome("Measured", "Duration of\nbenefits"),
  flow_start("Compensation claims", n = nrow(ij), note = "different claims before\nand after the change") |>
    flow_split("State", levels(ij$st), n = as.integer(table(ij$st))) |>
    flow_split("Earnings", levels(ij$hi), n = n9a) |>
    flow_split("Claim made before or after the change in benefits", c("Before", "After"), n = n9b) |>
    flow_outcome("Measured", "Duration of\nbenefits"))

tf10 <- get_data("MASS::Traffic")
l10 <- table(tf10$year, tf10$limit)
add("I10",
  flow_start("Days", n = nrow(tf10)) |>
    flow_split("Year", rownames(l10), n = as.integer(rowSums(l10))) |>
    flow_split("Speed limit", c("No limit", "Limit"), n = lapply(1:2, function(i) as.integer(l10[i, c("no", "yes")]))) |>
    flow_outcome("Recorded", "Accidents on the\nmotorways that day"),
  flow_start("Matched days", n = length(unique(tf10$day)), note = "day j of 1961 matched\nwith day j of 1962") |>
    flow_copy("Year", rownames(l10)) |>
    flow_split("Speed limit received", c("No limit", "Limit"), n = lapply(1:2, function(i) as.integer(l10[i, c("no", "yes")])),
               note = c("", "limit days in runs")) |>
    flow_outcome("Recorded", "Accidents on the\nmotorways that day"))

# J ---------------------------------------------------------------------------------
al <- get_data("SDAResources::algebra")
n187 <- help_num("SDAResources::algebra", "population of ([0-9]+) classes")
add("J05",
  flow_start("Maths classes in a city", n = n187) |>
    flow_select("Sample", "Classes", n = length(unique(al$class)), method = "simple random sampling") |>
    flow_apply("Students", "All students in\neach chosen class", n = nrow(al)) |>
    flow_outcome("Measured", "Test score"))

co <- get_data("SDAResources::coots")
cl6 <- unique(co[c("clutch", "tmt")])
e6 <- unique(as.vector(table(co$clutch)))
add("J06",
  flow_start("Clutches", n = nrow(cl6), note = paste0("from a larger study;\n", rng(co$csize), " eggs each")) |>
    flow_split("Supplemental feeding", c("Fed", "Not fed"), n = c(sum(cl6$tmt == 1), sum(cl6$tmt == 0))) |>
    flow_select("Eggs", paste(e6, "eggs per clutch"), n = e6 * c(sum(cl6$tmt == 1), sum(cl6$tmt == 0))) |>
    flow_outcome("Measured", "Length and breadth\nof the egg"))

# K ---------------------------------------------------------------------------------
ht <- get_data("agridat::hayman.tobacco")
nv <- nlevels(ht$male)
stopifnot(identical(levels(ht$male), levels(ht$female)))
yrs <- sort(unique(ht$year)); blk <- levels(ht$block)
pl <- help_num("agridat::hayman.tobacco", "two plots of ([0-9]+) plants")
add("K01",
  flow_start("Crosses", n = nv^2, note = sprintf("%d x %d varieties: each as\nmale and as female, selfs too", nv, nv)) |>
    flow_copy("Year", as.character(yrs)) |>
    flow_copy("Block", paste("Block", seq_along(blk))) |>
    flow_outcome("Measured", sprintf("Mean flowering time\nof a plot of %d plants", pl)),
  flow_start(c("Varieties as male parent", "Varieties as female parent"), n = nv,
             note = "the same varieties") |>
    flow_pool("Crosses", "Every male with every\nfemale, selfs included", n = nv^2) |>
    flow_copy("Year", as.character(yrs)) |>
    flow_copy("Block", paste("Block", seq_along(blk))) |>
    flow_outcome("Measured", sprintf("Mean flowering time\nof a plot of %d plants", pl)))

pw <- get_data("agridat::payne.wheat")
nr <- nlevels(pw$rotation); nn <- length(unique(pw$nitro)); yr <- range(pw$year)
add("K02",
  flow_start("Treatments", n = nr * nn, note = sprintf("%d rotations x %d\nnitrogen rates", nr, nn)) |>
    flow_apply("Years", sprintf("Every year,\n%d to %d", yr[1], yr[2]), n = nrow(pw)) |>
    flow_outcome("Measured", "Grain yield"),
  flow_start("Rotation x nitrogen\ncombinations", n = nr * nn,
             note = sprintf("%d rotations x %d nitrogen rates;\nplots and blocks not given", nr, nn)) |>
    flow_apply("Yields recorded", sprintf("One yield per combination\nand year, %d to %d", yr[1], yr[2]), n = nrow(pw),
               note = sprintf("%d years", length(unique(pw$year)))) |>
    flow_outcome("Measured", "Grain yield, t/ha"))

bu <- get_data("agridat::burgueno.unreplicated")
chk <- bu$gen == "G000"
ccol <- sort(unique(bu$col[chk]))
add("K03",
  flow_start("Plots", n = nrow(bu), note = sprintf("%d rows x %d columns", max(bu$row), max(bu$col))) |>
    flow_split("Line planted", c("Check line", "New lines"), n = c(sum(chk), sum(!chk))) |>
    flow_outcome("Measured", "Yield"),
  flow_start("Plots", n = nrow(bu), note = sprintf("%d rows x %d columns", max(bu$row), max(bu$col))) |>
    flow_split("Line planted", c("Check line", "New lines"), n = c(sum(chk), sum(!chk)),
               note = c(sprintf("one line, in columns\n%d, %d, %d, ..., %d", ccol[1], ccol[2], ccol[3], max(ccol)),
                        sprintf("%d lines,\none plot each", length(unique(bu$gen[!chk]))))) |>
    flow_outcome("Measured", "Yield"))

# Number check of every flow --------------------------------------------------------
cat("== flow_check() of every flow\n")
for (id in names(D)) for (v in c("p", "c")) {
  x <- D[[id]][[v]]
  if (is.null(x)) next
  r <- flow_check(x)
  if (nrow(r)) {
    cat(sprintf("%s %s: %d problem(s), %d note(s)\n", id, v, sum(r$level == "problem"), sum(r$level == "note")))
    for (i in seq_len(nrow(r))) cat("   ", r$level[i], "|", r$stage[i], "|", gsub("\\s+", " ", r$message[i]), "\n")
  } else cat(sprintf("%s %s: clean\n", id, v))
}

# Counting traps and copies in data mode ---------------------------------------------
cat("\n== data mode\n")
warned <- function(expr) {
  w <- character()
  x <- withCallingHandlers(expr, warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  list(x = x, warn = w)
}
trap <- function(id, what, data, by, label, unique_label, real) {
  go <- function(count) warned(flow_split(flow_start(what, data = data, count = count), "Group", by = by))
  a <- go(label); b <- go(unique_label)
  na <- flow_nodes(a$x)$n; nb <- flow_nodes(b$x)$n
  cat(sprintf("%s real units %s | counting %s: top %s, groups %s%s | counting a unique label: top %s, groups %s%s\n",
              id, real, label, na[1], paste(na[-1], collapse = " "), if (length(a$warn)) " [WARNED]" else " [silent]",
              nb[1], paste(nb[-1], collapse = " "), if (length(b$warn)) " [WARNED]" else " [silent]"))
  if (length(a$warn)) cat("   warning:", gsub("\\s+", " ", a$warn[1]), "\n")
  if (length(b$warn)) cat("   warning (unique label):", gsub("\\s+", " ", b$warn[1]), "\n")
}
d <- as.data.frame(rs); d$u <- paste(d$center, d$id)
trap("H08", "Patients", d, "center", "id", "u", nrow(unique(d["u"])))
d <- as.data.frame(ml); d$u <- paste(d$Location, d$Block)
trap("H18", "Blocks", d, "Location", "Block", "u", nb18)
d <- as.data.frame(vn); d$u <- paste(d$patient, d$tooth)
trap("H11", "Teeth", d, "patient", "tooth", "u", nrow(unique(d["u"])))
# a real overlap, not a repeated label: pupils who changed school
d <- as.data.frame(st)
r <- warned(flow_split(flow_start("Pupils", data = d, count = "id"), "School", by = "sch"))
cat(sprintf("H17 pupils %d; sum of the %d school boxes %d (%d pupils in more than one school)%s\n",
            nlevels(st$id), nlevels(st$sch), sum(as.integer(flow_nodes(r$x)$n[-1])), moved_sch,
            if (length(r$warn)) " [WARNED]" else " [silent]"))
if (length(r$warn)) cat("   warning:", gsub("\\s+", " ", r$warn[1]), "\n")
# copies from data: one row per unit and condition
for (id in c("G09", "G10")) {
  x <- D[[id]]$p
  cat(id, "p (data mode) boxes:", paste(flow_nodes(x)$label, flow_nodes(x)$n, sep = " = ", collapse = "; "), "\n")
}
r <- warned(flow_start("Rats", data = as.data.frame(rb), count = "animal") |>
  flow_copy("Drug", by = "treatment") |> flow_copy("Brain region", by = "region"))
cat("G12 from data: counts", paste(flow_nodes(r$x)$n, collapse = " "), if (length(r$warn)) "[WARNED]" else "[silent]",
    "; flow_check rows:", nrow(flow_check(r$x)), "\n")
d <- as.data.frame(vn); d$u <- paste(d$patient, d$tooth)
r <- warned(flow_start("Teeth", data = d, count = "u") |> flow_copy("Time", by = "time"))
cat("H11 from data: counts", paste(flow_nodes(r$x)$n, collapse = " "), if (length(r$warn)) "[WARNED]" else "[silent]", "\n")
r <- warned(flow_start("Teeth", data = d, count = "tooth") |> flow_copy("Time", by = "time"))
cat("H11 from data counting tooth labels: counts", paste(flow_nodes(r$x)$n, collapse = " "),
    if (length(r$warn)) "[WARNED]" else "[silent]", "\n")

# Codes ----------------------------------------------------------------------------
keys <- utils::read.csv("notes/textbook-ext-keys-GK.csv", encoding = "UTF-8")
stopifnot(setequal(names(D), keys$id))
old_seed <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
set.seed(20261126)
two <- names(D)[!vapply(D, function(x) is.null(x$c), TRUE)]
first <- setNames(rep("p", length(D)), names(D))
first[sample(two, floor(length(two) / 2))] <- "c"
codes <- rbind(
  data.frame(id = names(D), version = first[names(D)], set = "R"),
  data.frame(id = two, version = ifelse(first[two] == "p", "c", "p"), set = "S"))
codes <- codes[sample(nrow(codes)), ]
if (is.null(old_seed)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old_seed, globalenv())
codes <- codes[order(codes$set), ]
codes$code <- paste0(codes$set, sprintf("%02d", ave(seq_len(nrow(codes)), codes$set, FUN = seq_along)))
codes$width <- codes$height <- NA_real_
for (i in seq_len(nrow(codes))) {
  x <- D[[codes$id[i]]][[codes$version[i]]]
  flow_save(x, file.path(out_dir, codes$set[i], paste0(codes$code[i], ".png")), dpi = 100)
  s <- flow_layout(x)$size
  codes$width[i] <- round(s[["width"]], 1); codes$height[i] <- round(s[["height"]], 1)
}
t <- codes[codes$set == "R", ]
txt <- unlist(lapply(seq_len(nrow(t)), function(i)
  c(paste0("### V", substr(t$code[i], 2, 3)), unclass(flow_describe(D[[t$id[i]]][[t$version[i]]])), "")))
writeLines(txt, file.path(out_dir, "text", "descriptions.txt"))
utils::write.csv(codes, "notes/textbook-ext-codes-GK.csv", row.names = FALSE)

# Nothing a reader sees may name the design or the data set
seen <- c(txt, unlist(lapply(seq_len(nrow(codes)), function(i) {
  x <- D[[codes$id[i]]][[codes$version[i]]]
  c(x$nodes$label, x$nodes$note, x$stages$name, x$stages$row_label, x$side$label)
})))
bare <- sub(".*::", "", keys$data)
words <- unique(c(bare, "tobacco", "wheat", "burgueno", "hayman", "payne", "lawson", "STAR"))
hits <- words[vapply(words, function(w) any(grepl(paste0("\\b", gsub(".", "\\.", w, fixed = TRUE), "\\b"), seen, ignore.case = TRUE)), NA)]
ids_hit <- any(grepl("\\b[A-K][0-9]{2}\\b", seen))
cat("\nnames found in what readers see:", if (length(hits)) paste(hits, collapse = ", ") else "none",
    "; design ids found:", ids_hit, "\n")
cat("designs", length(D), "; with two versions", length(two), "; pictures", nrow(codes), "\n")
w7 <- codes$width > 7
cat("wider than 7 inches:", paste(codes$id[w7], codes$version[w7], codes$width[w7], collapse = "; "), "\n")
