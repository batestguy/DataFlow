# Side-by-side test with other packages (milestone 6).
#
# Run from the package folder:  Rscript --vanilla notes/comparison.R
# Other packages come from dev/complib (never the renv library). Pictures go to
# dev/m6/ (git-ignored). Prints, for every design and tool: drawn / refused,
# warnings, and the number of calls to the tool's own functions.
#
# The same eight designs, same invented numbers, in every tool that has a way
# to attempt them. The "key" is what a reader must be able to tell; it was
# written before drawing.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
suppressMessages({
  pkgload::load_all(quiet = TRUE, export_all = FALSE)
  library(dplyr, warn.conflicts = FALSE)
})
dir.create("dev/m6", showWarnings = FALSE)
options(dtrackr.verbose = FALSE)

key <- c(
  D1 = "Parallel trial: 500 assessed, 100 excluded (60, 40), 400 randomised 200/200, lost 12 and 15 with reasons, analysed 188/185; three stage names",
  D2 = "Review, two streams kept apart: databases 1500 - 300 = 1200 screened - 1000 = 200 - 150 = 50; other 100 - 30 = 70 - 50 = 20; included 70",
  D3 = "2 x 2 factorial: 240, A/no A 120 each, B/no B 60 each cell, then pooled by A (120, 120); the reader sees that cells of both B levels go into each pool",
  D4 = "Cluster survey: 9200 areas, urban/rural, 20 and 30 areas selected (PPS), 25 households per area (500, 750), 1250 households, 150 not interviewed, 1100; 1300 women: the unit changes three times and the sampling method is said",
  D5 = "Lab design of the reference photo: 100 cultures, 2 experiments (50), type, 2 batches (25), compositions A B C, one bracket per experiment to 9 conditions, 2 tasks, 600 trials",
  D6 = "Prediction model: 900 development patients (135 events) split by hospital 600/150/150 (90/22/23), and a separate external set 400 (52)",
  D7 = "Scraping: 40 sites gave 12400 pages, 1 interface gave 58000 posts; removed 1200 and 4100; 65100 records; 9100 removed; 56000; 2000 sampled; 1400/300/300. Units change",
  D8 = "Two annotators EACH label all 2000 records; 1700 agree, 300 disagree and go to a senior reviewer; 2000 gold labels"
)

# Data for the tools that need rows ------------------------------------------
trial <- data.frame(id = 1:500, arm = NA_character_, excl = NA_character_, lost = NA_character_)
trial$excl[1:60] <- "Not eligible"
trial$excl[61:100] <- "Declined"
trial$arm[101:300] <- "Drug"
trial$arm[301:500] <- "Placebo"
trial$lost[101:112] <- rep(c("Moved", "Withdrew"), c(7, 5))
trial$lost[301:315] <- rep(c("Moved", "Withdrew"), c(6, 9))
trial$rand <- ifelse(is.na(trial$arm), NA, "yes")
trial$done <- ifelse(is.na(trial$arm) | !is.na(trial$lost), NA, "yes")
fact <- expand.grid(i = 1:60, B = c("B", "no B"), A = c("A", "no A"))
fact$id <- seq_len(nrow(fact))
lab <- expand.grid(batch = c("Batch 1", "Batch 2"), type = c("Type A", "Type B"), i = 1:25)
lab$exp <- ifelse(lab$type == "Type A", "Ex. 1", "Ex. 2")
lab$comp <- rep(c("A", "B", "C"), length.out = nrow(lab))
lab$id <- seq_len(nrow(lab))
dev_set <- data.frame(id = 1:900, set = rep(c("Training", "Validation", "Test"), c(600, 150, 150)),
                      event = c(rep(1:0, c(90, 510)), rep(1:0, c(22, 128)), rep(1:0, c(23, 127))))
ext_set <- data.frame(id = 1001:1400, set = "External validation", event = rep(1:0, c(52, 348)))
recs <- data.frame(id = 1:2000)
a1 <- data.frame(id = 1:2000, lab1 = "x")
a2 <- data.frame(id = 1:2000, lab2 = rep(c("x", "y"), c(1700, 300)))

# Saving -----------------------------------------------------------------------
save_as <- function(tool, id, x) {
  f <- file.path("dev/m6", paste0(id, "_", tool, ".png"))
  if (tool == "ours") {
    flow_save(x, f, dpi = 110)
  } else if (tool == "selecta") {
    suppressMessages(selecta::flowsave(x, f, dpi = 110, quiet = TRUE))
  } else if (tool == "flowchart") {
    grDevices::png(f, 1200, 1000, res = 110)
    on.exit(grDevices::dev.off())
    invisible(flowchart::fc_draw(x, big.mark = ","))
  } else if (tool == "consort") {
    grDevices::png(f, 1300, 1000, res = 110)
    on.exit(grDevices::dev.off())
    plot(x)
  } else if (tool == "PRISMA2020") {
    PRISMA2020::PRISMA_save(x, f, overwrite = TRUE)
  } else if (tool == "dtrackr") {
    svg <- dtrackr::dot2svg(dtrackr::p_get_as_dot(x))
    rsvg::rsvg_png(charToRaw(svg), f, width = 1100)
  }
  f
}
own <- c(ours = "studydesignflow", selecta = "selecta", flowchart = "flowchart",
         consort = "consort", dtrackr = "dtrackr", PRISMA2020 = "PRISMA2020", TrialFlowR = "TrialFlowR")
res <- list()
try_it <- function(id, tool, expr, note = "") {
  e <- substitute(expr)
  ex <- getNamespaceExports(own[[tool]])
  calls <- sum(all.names(e) %in% setdiff(ex, c("%>%", "bind_rows")))
  w <- character()
  r <- withCallingHandlers(
    tryCatch({
      x <- eval(e, list2env(as.list(asNamespace(own[[tool]])), parent = globalenv()))
      if (tool == "TrialFlowR") "no drawing function" else { save_as(tool, id, x); "drawn" }
    }, error = function(err) paste("REFUSED:", conditionMessage(err))),
    warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") },
    message = function(c) invokeRestart("muffleMessage"))
  r <- gsub("\\s+", " ", r)
  res[[length(res) + 1]] <<- data.frame(id = id, tool = tool, result = r, calls = calls,
                                        warnings = paste(unique(gsub("\\s+", " ", w)), collapse = " / "), note = note)
}

# D1 Parallel trial --------------------------------------------------------------
try_it("D1", "ours", flow_start("Assessed for eligibility", n = 500, stage = "Enrolment") |>
  flow_exclude("Excluded", n = 100, reasons = c("Not eligible" = 60, "Declined" = 40)) |>
  flow_split("Randomised", "Randomised", n = 400, row_label = "") |>
  flow_assign("Allocation", c("Drug", "Placebo"), n = 200) |>
  flow_exclude("Lost to follow-up", n = c(12, 15), stage = "Follow-up",
               reasons = list(c("Moved" = 7, "Withdrew" = 5), c("Moved" = 6, "Withdrew" = 9))) |>
  flow_outcome("Analysis", "Analysed", n = c(188, 185)))
try_it("D1", "selecta", enroll(n = 500, label = "Assessed for eligibility") |>
  phase("Enrolment") |>
  exclude("Excluded", n = 100, reasons = c("Not eligible" = 60, "Declined" = 40)) |>
  phase("Allocation") |>
  allocate(labels = c("Drug", "Placebo"), n = c(200, 200)) |>
  phase("Follow-up") |>
  exclude("Lost to follow-up", n = c(12, 15),
          reasons = list(c("Moved" = 7, "Withdrew" = 5), c("Moved" = 6, "Withdrew" = 9))) |>
  phase("Analysis") |>
  endpoint("Analysed"))
try_it("D1", "flowchart", as_fc(trial, label = "Assessed for eligibility") |>
  fc_filter(!is.na(arm), label = "Randomised", show_exc = TRUE,
            label_exc = "Excluded\nNot eligible: 60\nDeclined: 40") |>
  fc_split(arm, title = "Allocation") |>
  fc_filter(is.na(lost), label = "Analysed", show_exc = TRUE, label_exc = "Lost to follow-up", title = "Follow-up"),
  "reasons typed by hand")
try_it("D1", "consort", consort_plot(trial,
  orders = c(id = "Assessed for eligibility", excl = "Excluded", rand = "Randomised", arm = "Allocated",
             lost = "Lost to follow-up", done = "Analysed"),
  side_box = c("excl", "lost"), allocation = "arm", labels = c("1" = "Enrolment", "2" = "Allocation", "3" = "Follow-up")))
try_it("D1", "dtrackr", trial |> track() |> comment("{.count} assessed for eligibility") |>
  exclude_all(excl == "Not eligible" ~ "{.excluded} not eligible", excl == "Declined" ~ "{.excluded} declined") |>
  comment("{.count} randomised") |>
  group_by(arm, .messages = "allocated at random") |> comment("{.count} allocated") |>
  exclude_all(lost == "Moved" ~ "{.excluded} moved", lost == "Withdrew" ~ "{.excluded} withdrew") |>
  comment("{.count} analysed"))
try_it("D1", "TrialFlowR", trial_flow(transform(trial, stage = "Randomised"), id = "id", stage = "stage", group = "arm"))

# D2 Review ----------------------------------------------------------------------
try_it("D2", "ours", flow_start(c("Databases", "Other sources"), n = c(1500, 100), stage = "Identification") |>
  flow_exclude("Removed before screening", n = 300, reasons = c("Duplicates" = 300), from = 1) |>
  flow_apply("Screening", "Records screened", n = 1200, from = 1) |>
  flow_exclude("Records excluded", n = 1000, from = "Records screened") |>
  flow_apply("Retrieval", "Reports sought", n = 100, from = 2) |>
  flow_exclude("Not retrieved", n = 30, from = "Reports sought") |>
  flow_apply("Eligibility", "Reports assessed", n = c(200, 70), from = c("Records screened", "Reports sought")) |>
  flow_exclude("Reports excluded", n = c(150, 50)) |>
  flow_pool("Included", "Studies included", n = 70))
try_it("D2", "selecta", sources(databases = c("Databases" = 1500), other = c("Other sources" = 100),
                                headers = c(databases = "Databases", other = "Other methods")) |>
  exclude("Removed", n = c(300, 0)) |>
  exclude("Not screened in / not retrieved", n = c(1000, 30), included_label = "Reports assessed") |>
  exclude("Reports excluded", n = c(150, 50)) |>
  combine("Studies included") |> endpoint("Review"), "streams kept apart, as the key asks")
try_it("D2b", "selecta", sources(databases = c("Databases" = 1500), other = c("Other sources" = 100),
                                 headers = c(databases = "Databases", other = "Other methods")) |>
  phase("Identification") |> combine("Records identified") |>
  exclude("Duplicates", n = 300, included_label = "Records screened") |>
  phase("Screening") |> exclude("Excluded or not retrieved", n = 1030, included_label = "Reports assessed") |>
  exclude("Reports excluded", n = 200) |> phase("Included") |> endpoint("Studies included"),
  "its own documented way: streams pooled first")
try_it("D2c", "selecta", enroll(n = 1600, label = "Identified") |>
  stratify(labels = c("Databases", "Other sources"), n = c(1500, 100), label = "Source") |>
  exclude(c("Duplicates", "Not retrieved"), n = c(300, 30), included_label = c("Records screened", "Reports assessed")) |>
  exclude("Excluded", n = c(1150, 50), reasons = list(c("Records" = 1000, "Reports" = 150), c("Reports" = 50))) |>
  combine("Studies included") |> endpoint("Review"), "streams kept apart by starting from one box")
try_it("D2", "flowchart", fc_merge(list(
  as_fc(N = 1500, label = "Databases") |> fc_filter(N = 1200, label = "Records screened", show_exc = TRUE, label_exc = "Duplicates") |>
    fc_filter(N = 200, label = "Reports assessed", show_exc = TRUE) |> fc_filter(N = 50, label = "Included", show_exc = TRUE),
  as_fc(N = 100, label = "Other sources") |> fc_filter(N = 70, label = "Reports assessed", show_exc = TRUE, label_exc = "Not retrieved") |>
    fc_filter(N = 20, label = "Included", show_exc = TRUE))))
try_it("D2", "dtrackr", {
  db <- data.frame(id = 1:1500, why = rep(c("dup", "irr", "full", NA), c(300, 1000, 150, 50)))
  ot <- data.frame(id = 1501:1600, why = rep(c("ret", "full", NA), c(30, 50, 20)))
  x <- db |> track(.messages = "{.count} from databases") |> exclude_all(why == "dup" ~ "{.excluded} duplicates") |>
    comment("{.count} screened") |> exclude_all(why == "irr" ~ "{.excluded} excluded") |>
    comment("{.count} reports assessed") |> exclude_all(why == "full" ~ "{.excluded} reports excluded")
  y <- ot |> track(.messages = "{.count} from other sources") |> exclude_all(why == "ret" ~ "{.excluded} not retrieved") |>
    comment("{.count} reports assessed") |> exclude_all(why == "full" ~ "{.excluded} reports excluded")
  bind_rows(x, y, .messages = "{.count.out} studies included")
})
try_it("D2", "PRISMA2020", {
  d <- utils::read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
  set <- c(database_results = 1500, register_results = 0, website_results = 60, organisation_results = 0,
           citations_results = 40, duplicates = 300, excluded_automatic = 0, excluded_other = 0,
           records_screened = 1200, records_excluded = 1000, dbr_sought_reports = 200, dbr_notretrieved_reports = 0,
           other_sought_reports = 100, other_notretrieved_reports = 30, dbr_assessed = 200, dbr_excluded = "Reason, 150",
           other_assessed = 70, other_excluded = "Reason, 50", new_studies = 70, new_reports = 70)
  i <- match(names(set), d$data); stopifnot(!anyNA(i)); d$n[i] <- set
  PRISMA_flowdiagram(PRISMA_data(d), previous = FALSE, other = TRUE, interactive = FALSE)
})

# D3 Factorial ---------------------------------------------------------------------
try_it("D3", "ours", flow_start("Randomised", n = 240) |>
  flow_assign("Factor A", c("A", "no A"), n = 120) |>
  flow_assign("Factor B", c("B", "no B"), n = 60) |>
  flow_pool("Analysis of A", "Analysed", over = "Factor A", n = 120))
try_it("D3", "selecta", enroll(n = 240, label = "Randomised") |>
  allocate(labels = c("A", "no A"), n = c(120, 120), label = "Factor A") |>
  allocate(labels = c("B", "no B"), n = c(60, 60, 60, 60), label = "Factor B") |>
  endpoint("Analysed"), "without pooling")
try_it("D3b", "selecta", enroll(n = 240, label = "Randomised") |>
  allocate(labels = c("A", "no A"), n = c(120, 120), label = "Factor A") |>
  allocate(labels = c("B", "no B"), n = c(60, 60, 60, 60), label = "Factor B") |>
  combine("Analysed by A") |> endpoint("Analysis"), "combine releases one level, so this pools by A")
try_it("D3", "flowchart", as_fc(fact, label = "Randomised") |> fc_split(A) |> fc_split(B))
try_it("D3", "consort", consort_plot(transform(fact, A = as.character(A), B = as.character(B), done = "yes", out = NA_character_),
  orders = list(c(id = "Randomised"), c(out = "Excluded"), c(A = "Factor A"), c(B = "Factor B"), c(done = "Analysed")),
  side_box = "out", allocation = c("A", "B")), "needs an exclusion column, so an empty one is given")
try_it("D3", "dtrackr", fact |> track(.messages = "{.count} randomised") |> group_by(A, .messages = "factor A") |>
  comment("{.count}") |> group_by(A, B, .messages = "factor B") |> comment("{.count}") |>
  group_by(A, .messages = "pooled over B") |> comment("{.count} analysed"))

# D4 Survey --------------------------------------------------------------------------
try_it("D4", "ours", flow_start("Enumeration areas", n = 9200, stage = "Frame") |>
  flow_split("Stratum", c("Urban", "Rural"), n = c(3500, 5700)) |>
  flow_cluster("Clusters", c(20, 30), summary = "{k} areas selected", method = "probability proportional to size") |>
  flow_select("Households", "25 per area", n = c(500, 750), method = "systematic sampling") |>
  flow_pool("Pooled", "Households selected", n = 1250, row_label = "") |>
  flow_exclude("Not interviewed", n = 150) |>
  flow_apply("Interviews", "Households interviewed", n = 1100) |>
  flow_outcome("Women", "Women interviewed", n = 1300))
try_it("D4", "selecta", enroll(n = 9200, label = "Enumeration areas") |>
  stratify(labels = c("Urban", "Rural"), n = c(3500, 5700), label = "Stratum") |>
  exclude("Areas not selected", n = c(3480, 5670), included_label = "Areas selected (PPS)") |>
  combine("Households selected", n = 1250) |>
  exclude("Not interviewed", n = 150, included_label = "Households interviewed") |>
  endpoint("Women interviewed"), "cannot type 1300 women at the end")
try_it("D4", "flowchart", as_fc(N = 9200, label = "Enumeration areas") |>
  fc_split(N = c(3500, 5700), label = c("Urban", "Rural")) |>
  fc_filter(N = c(20, 30), label = "Areas selected (PPS)") |>
  fc_filter(N = c(500, 750), label = "Households (25 per area)"), "more households than areas")
try_it("D4", "consort", add_box(txt = "Enumeration areas (n=9200)") |>
  add_split(txt = c("Urban (n=3500)", "Rural (n=5700)")) |>
  add_box(txt = c("20 areas selected (PPS)", "30 areas selected (PPS)")) |>
  add_box(txt = c("Households, 25 per area (n=500)", "Households, 25 per area (n=750)")) |>
  add_box(txt = "Households selected (n=1250)") |>
  add_side_box(txt = "Not interviewed (n=150)") |>
  add_box(txt = "Households interviewed (n=1100)") |>
  add_box(txt = "Women interviewed (n=1300)"), "all text typed")

# D5 Lab -----------------------------------------------------------------------------
try_it("D5", "ours", flow_start("Cultures", n = 100) |>
  flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = c(50, 50)) |>
  flow_split("Culture type", list("Type A", "Type B"), n = 50) |>
  flow_split("Batch", c("Batch 1", "Batch 2"), n = 25) |>
  flow_split("Cell culture composition", c("A", "B", "C")) |>
  flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
  flow_apply("Task", "2 tasks each") |> flow_outcome("Number of trials", "600 each"))
try_it("D5", "selecta", enroll(n = 100, label = "Cultures") |>
  stratify(labels = c("Ex. 1, Type A", "Ex. 2, Type B"), n = c(50, 50), label = "Experiment") |>
  stratify(labels = c("Batch 1", "Batch 2"), n = c(25, 25, 25, 25), label = "Batch") |>
  stratify(labels = c("A", "B", "C"), n = rep(8, 12), label = "Composition") |>
  endpoint("9 conditions, 2 tasks, 600 trials each"))
try_it("D5b", "selecta", enroll(n = 100, label = "Cultures") |>
  stratify(labels = c("Ex. 1, Type A, Batch 1", "Ex. 1, Type A, Batch 2", "Ex. 2, Type B, Batch 1", "Ex. 2, Type B, Batch 2"),
           n = rep(25, 4), label = "Experiment, type, batch") |>
  stratify(labels = c("A", "B", "C"), n = rep(c(9, 8, 8), 4), label = "Composition") |>
  combine("9 conditions, 2 tasks, 600 trials each") |> endpoint("Trials"),
  "flattened to two levels; needs invented counts per composition; pools per batch, not per experiment")
try_it("D5", "flowchart", as_fc(lab, label = "Cultures") |> fc_split(exp) |> fc_split(batch) |> fc_split(comp))
try_it("D5", "consort", add_box(txt = "Cultures (n=100)") |>
  add_split(txt = c("Ex. 1, Type A (n=50)", "Ex. 2, Type B (n=50)")) |>
  add_split(txt = list(c("Batch 1 (25)", "Batch 2 (25)"), c("Batch 1 (25)", "Batch 2 (25)"))) |>
  add_split(txt = list(c("A", "B", "C"), c("A", "B", "C"), c("A", "B", "C"), c("A", "B", "C"))))
try_it("D5", "dtrackr", lab |> track(.messages = "{.count} cultures") |> group_by(exp) |> comment("{.count}") |>
  group_by(exp, batch) |> comment("{.count}") |> group_by(exp, batch, comp) |> comment("{.count}") |>
  group_by(exp, .messages = "9 conditions each") |> comment("2 tasks each; 600 trials each"))

# D6 Prediction model ---------------------------------------------------------------
try_it("D6", "ours", flow_start(c("Development", "External"), n = c(900, 400), events = c(135, 52), stage = "Source") |>
  flow_partition("Split by hospital", c("Training", "Validation", "Test"), n = c(600, 150, 150),
                 events = c(90, 22, 23), group = "hospital", from = "Development") |>
  flow_apply("External validation", "Validated", n = 400, events = 52, from = "External"))
try_it("D6", "selecta", enroll(n = 1300, label = "Patients") |>
  stratify(labels = c("Development", "External"), n = c(900, 400), label = "Source") |>
  stratify(labels = list(c("Training", "Validation", "Test"), "External validation"), n = c(600, 150, 150, 400)) |>
  endpoint("Events", breakdown = list(c("Events" = 90), c("Events" = 22), c("Events" = 23), c("Events" = 52))))
try_it("D6b", "selecta", enroll(n = 900, label = "Development patients") |>
  stratify(labels = c("Training", "Validation", "Test"), n = c(600, 150, 150), label = "Split by hospital") |>
  endpoint("Set", breakdown = list(c("Event" = 90, "No event" = 510), c("Event" = 22, "No event" = 128), c("Event" = 23, "No event" = 127))),
  "external set left out")
try_it("D6c", "selecta", enroll(n = 1300, label = "Patients") |>
  stratify(labels = c("Training", "Validation", "Test", "External"), n = c(600, 150, 150, 400), label = "Set") |>
  endpoint("Set", breakdown = list(c("Event" = 90), c("Event" = 22), c("Event" = 23), c("Event" = 52))),
  "external set drawn as one more split")
try_it("D6", "flowchart", fc_merge(list(
  as_fc(dev_set, label = "Development (135 events)") |> fc_split(set),
  as_fc(ext_set, label = "External validation (52 events)"))))
try_it("D6", "dtrackr", {
  x <- dev_set |> track(.messages = "Development: {.count} patients") |> group_by(set) |> status(ev = sum(event), .messages = "{.count}, {ev} events")
  y <- ext_set |> track(.messages = "External: {.count} patients") |> group_by(set) |> status(ev = sum(event), .messages = "{.count}, {ev} events")
  bind_rows(x, y, .messages = "{.count.out} in all")
})

# D7 Scraping -----------------------------------------------------------------------
try_it("D7", "ours", flow_start(c("News sites", "Social media interface"), n = c(40, 1), stage = "Sources") |>
  flow_apply("Collection", c("Pages fetched", "Posts fetched"), n = c(12400, 58000)) |>
  flow_exclude("Not usable", n = c(1200, 4100)) |>
  flow_pool("Merged", "Records", n = 65100) |>
  flow_exclude("Cleaning", n = 9100, reasons = c("Duplicates" = 7000, "Wrong language" = 2100)) |>
  flow_apply("Corpus", "Clean records", n = 56000) |>
  flow_select("Sample", "Sampled for labelling", n = 2000, method = "simple random sampling") |>
  flow_partition(labels = c("Training", "Validation", "Test"), n = c(1400, 300, 300)))
try_it("D7", "selecta", sources(web = c("Pages (40 sites)" = 12400), api = c("Posts (1 interface)" = 58000)) |>
  phase("Collection") |> exclude("Not usable", n = c(1200, 4100)) |> combine("Records") |>
  phase("Cleaning") |> exclude("Removed", n = 9100, reasons = c("Duplicates" = 7000, "Wrong language" = 2100), included_label = "Clean records") |>
  exclude("Not sampled", n = 54000, included_label = "Sampled for labelling") |>
  stratify(labels = c("Training", "Validation", "Test"), n = c(1400, 300, 300), label = "Split") |> endpoint("Set"))
try_it("D7b", "selecta", sources(web = c("Pages (40 sites)" = 12400), api = c("Posts (1 interface)" = 58000)) |>
  phase("Collection") |> combine("Fetched") |>
  exclude("Not usable", n = 5300, reasons = c("Pages" = 1200, "Posts" = 4100), included_label = "Records") |>
  phase("Cleaning") |> exclude("Removed", n = 9100, reasons = c("Duplicates" = 7000, "Wrong language" = 2100), included_label = "Clean records") |>
  exclude("Not sampled", n = 54000, included_label = "Sampled for labelling") |>
  stratify(labels = c("Training", "Validation", "Test"), n = c(1400, 300, 300), label = "Split") |> endpoint("Set"),
  "streams pooled before removal")
try_it("D7c", "selecta", enroll(n = 70400, label = "Fetched") |>
  stratify(labels = c("Pages (40 sites)", "Posts (1 interface)"), n = c(12400, 58000), label = "Source") |>
  exclude("Not usable", n = c(1200, 4100)) |> combine("Records") |>
  exclude("Removed", n = 9100, reasons = c("Duplicates" = 7000, "Wrong language" = 2100), included_label = "Clean records") |>
  exclude("Not sampled", n = 54000, included_label = "Sampled for labelling") |>
  stratify(labels = c("Training", "Validation", "Test"), n = c(1400, 300, 300), label = "Split") |> endpoint("Set"),
  "streams kept apart by starting from one box")
try_it("D7", "dtrackr", {
  p <- data.frame(id = 1:12400, bad = rep(c(TRUE, FALSE), c(1200, 11200)))
  s <- data.frame(id = 12401:70400, bad = rep(c(TRUE, FALSE), c(4100, 53900)))
  m <- bind_rows(p |> track(.messages = "40 sites: {.count} pages") |> exclude_all(bad ~ "{.excluded} not usable"),
                 s |> track(.messages = "1 interface: {.count} posts") |> exclude_all(bad ~ "{.excluded} not usable"),
                 .messages = "{.count.out} records")
  m <- m |> mutate(why = rep(c("dup", "lang", NA), c(7000, 2100, 56000)), .messages = NULL) |>
    exclude_all(why == "dup" ~ "{.excluded} duplicates", why == "lang" ~ "{.excluded} wrong language") |>
    comment("{.count} clean records") |> slice_head(n = 2000, .messages = "{.count.out} sampled for labelling") |>
    mutate(set = rep(c("Training", "Validation", "Test"), c(1400, 300, 300)), .messages = NULL) |> group_by(set) |> comment("{.count}")
  m
})

# D8 Two annotators -----------------------------------------------------------------
try_it("D8", "ours", flow_start("Records to label", n = 2000) |>
  flow_split("Annotator", c("Annotator 1", "Annotator 2"), n = 2000) |>
  flow_pool("Agreement", "Labels compared", n = 2000) |>
  flow_split("Result", c("Agree", "Disagree"), n = c(1700, 300)) |>
  flow_apply("Adjudication", "Senior reviewer", n = 300, from = "Disagree") |>
  flow_pool("Final", "Gold labels", n = 2000))
try_it("D8", "selecta", enroll(n = 2000, label = "Records to label") |>
  stratify(labels = c("Annotator 1", "Annotator 2"), n = c(2000, 2000), label = "Each labels all records") |>
  combine("Labels compared", n = 2000) |>
  stratify(labels = c("Agree", "Disagree"), n = c(1700, 300), label = "Result") |>
  combine("Gold labels", n = 2000) |> endpoint("Data set"))
try_it("D8", "flowchart", as_fc(N = 2000, label = "Records to label") |>
  fc_split(N = c(2000, 2000), label = c("Annotator 1", "Annotator 2")))
try_it("D8", "dtrackr", {
  r <- recs |> track(.messages = "{.count} records to label")
  x <- r |> left_join(a1, by = "id", .messages = NULL) |> comment("Annotator 1 labels all {.count}")
  y <- r |> left_join(a2, by = "id", .messages = NULL) |> comment("Annotator 2 labels all {.count}")
  z <- inner_join(x, y, by = "id", .messages = "{.count.out} records, labels compared") |>
    mutate(res = ifelse(lab1 == lab2, "agree", "disagree"), .messages = NULL) |> group_by(res) |> comment("{.count}")
  z |> ungroup(.messages = "{.count} gold labels")
})

# Second, fairer tries -----------------------------------------------------------------
try_it("D3b", "consort", add_box(txt = "Randomised (n=240)") |>
  add_split(txt = c("A (n=120)", "no A (n=120)")) |>
  add_split(txt = list(c("B (n=60)", "no B (n=60)"), c("B (n=60)", "no B (n=60)"))) |>
  add_box(txt = c("Analysed (n=120)", "Analysed (n=120)")), "all text typed")
try_it("D2", "consort", add_split(txt = c("Databases (n=1500)", "Other sources (n=100)")) |>
  add_side_box(txt = c("Duplicates (n=300)", "Not retrieved (n=30)")) |>
  add_box(txt = c("Records screened (n=1200)", "Reports assessed (n=70)")) |>
  add_side_box(txt = c("Excluded (n=1150)", "Excluded (n=50)")) |>
  add_box(txt = "Studies included (n=70)"), "two starting boxes, all text typed")
try_it("D2d", "consort", add_box(txt = "Identified (n=1600)") |>
  add_split(txt = c("Databases (n=1500)", "Other sources (n=100)")) |>
  add_side_box(txt = c("Duplicates (n=300)", "Not retrieved (n=30)")) |>
  add_box(txt = c("Records screened (n=1200)", "Reports assessed (n=70)")) |>
  add_side_box(txt = c("Excluded (n=1150)", "Excluded (n=50)")) |>
  add_box(txt = "Studies included (n=70)"), "one starting box, all text typed")
try_it("D6", "consort", add_box(txt = "Patients (n=1300)") |>
  add_split(txt = c("Development (n=900; 135 events)", "External (n=400; 52 events)")) |>
  add_split(txt = list(c("Training (600; 90)", "Validation (150; 22)", "Test (150; 23)"), "External validation (400; 52)")),
  "one starting box, all text typed")
try_it("D8", "consort", add_box(txt = "Records to label (n=2000)") |>
  add_split(txt = c("Annotator 1 labels all 2000", "Annotator 2 labels all 2000")) |>
  add_box(txt = "Labels compared (n=2000)") |>
  add_split(txt = c("Agree (n=1700)", "Disagree (n=300)")) |>
  add_box(txt = c("", "Senior reviewer (n=300)")) |>
  add_box(txt = "Gold labels (n=2000)"), "all text typed")
try_it("D6b", "dtrackr", {
  x <- dev_set |> track(.messages = "Development: {.count} patients") |> group_by(set, .messages = "split by hospital") |>
    status(ev = sum(event), .messages = "{.count}, {ev} events")
  y <- ext_set |> track(.messages = "External: {.count} patients") |> comment("external validation")
  bind_rows(x |> ungroup(.messages = "development total"), y, .messages = "{.count.out} in all")
}, "needs an invented last box to join the two")
try_it("D8b", "dtrackr", {
  r <- recs |> track(.messages = "{.count} records to label")
  x <- r |> comment("Annotator 1 labels all {.count}") |> mutate(lab1 = "x", .messages = NULL, .headline = NULL)
  y <- r |> comment("Annotator 2 labels all {.count}") |> mutate(lab2 = rep(c("x", "y"), c(1700, 300)), .messages = NULL, .headline = NULL)
  inner_join(x, y, by = "id", .messages = "{.count.out} records, labels compared") |>
    mutate(res = ifelse(lab1 == lab2, "agree", "disagree: senior reviewer"), .messages = NULL) |>
    group_by(res, .messages = "compared") |> comment("{.count}") |> ungroup(.messages = "{.count} gold labels")
})

try_it("D8b", "ours", flow_start("Records to label", n = 2000) |>
  flow_split("Annotator", c("Annotator 1 labels all 2,000", "Annotator 2 labels all 2,000")) |>
  flow_pool("Agreement", "Labels compared", n = 2000) |>
  flow_split("Result", c("Agree", "Disagree"), n = c(1700, 300)) |>
  flow_apply("Adjudication", "Senior reviewer", n = 300, from = "Disagree") |>
  flow_pool("Final", "Gold labels", n = 2000), "same wording as the others")
try_it("D4b", "ours", flow_start("Enumeration areas", n = 9200, stage = "Frame") |>
  flow_split("Stratum", c("Urban", "Rural"), n = c(3500, 5700)) |>
  flow_cluster("Clusters", c(20, 30), summary = "{k} areas selected (PPS)", method = "probability proportional to size") |>
  flow_select("Households", "25 per area, systematic", n = c(500, 750), method = "systematic sampling") |>
  flow_pool("Pooled", "Households selected", n = 1250, row_label = "") |>
  flow_exclude("Not interviewed", n = 150) |>
  flow_apply("Interviews", "Households interviewed", n = 1100) |>
  flow_outcome("Women", "Women interviewed", n = 1300), "method typed into the labels")

# selecta: an exclusion between two sources and their merge
try_it("S1", "selecta", sources(a = c("X" = 1500), b = c("Y" = 100)) |> exclude("R", n = c(300, 10)) |>
  combine("All") |> endpoint("E"), "look at the picture: the 310 exclusions are missing and All says 1,600")
try_it("S2", "selecta", sources(a = c("X" = 1500), b = c("Y" = 100)) |> phase("P") |> exclude("R", n = c(300, 10)) |>
  combine("All") |> endpoint("E"), "the same with a phase label")

# Planted error: 400 randomised typed as 200 + 190; reasons 60 + 30 for 100 -----------
try_it("P1", "ours", flow_start("Assessed", n = 500) |>
  flow_exclude("Excluded", n = 100, reasons = c("Not eligible" = 60, "Declined" = 30)) |>
  flow_split("R", "Randomised", n = 400) |> flow_assign("Allocation", c("Drug", "Placebo"), n = c(200, 190)))
try_it("P1", "selecta", enroll(n = 500, label = "Assessed") |>
  exclude("Excluded", n = 100, reasons = c("Not eligible" = 60, "Declined" = 30)) |>
  allocate(labels = c("Drug", "Placebo"), n = c(200, 190)) |> endpoint("Analysed"))
try_it("P1", "flowchart", as_fc(N = 500, label = "Assessed") |>
  fc_filter(N = 400, label = "Randomised", show_exc = TRUE) |> fc_split(N = c(200, 190), label = c("Drug", "Placebo")))
try_it("P1", "consort", add_box(txt = "Assessed (n=500)") |> add_side_box(txt = "Excluded (n=100): 60, 30") |>
  add_box(txt = "Randomised (n=400)") |> add_split(txt = c("Drug (n=200)", "Placebo (n=190)")))

out <- do.call(rbind, res)
for (i in seq_len(nrow(out))) {
  cat(sprintf("%-4s %-10s %-3d calls | %s%s%s\n", out$id[i], out$tool[i], out$calls[i], out$result[i],
              if (nzchar(out$warnings[i])) paste0(" | WARNED: ", out$warnings[i]) else "",
              if (nzchar(out$note[i])) paste0(" | note: ", out$note[i]) else ""))
}
cat("
our description of D8b:
", unclass(flow_describe(
  flow_start("Records to label", n = 2000) |>
    flow_split("Annotator", c("Annotator 1 labels all 2,000", "Annotator 2 labels all 2,000")))), "
")
cat("\nclass of what each tool returns / draws with:\n")
cat("ours:", class(ggplot2::autoplot(flow_start("a", n = 1)))[1], "\n")
cat("selecta:", class(selecta::enroll(n = 1)), "; flowchart() returns", class(suppressMessages(tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off()); selecta::flowchart(selecta::enroll(n = 10) |> selecta::endpoint("x"))
}, error = function(e) conditionMessage(e))))[1], "\n")
cat("consort:", class(consort::add_box(txt = "a")), "\n")
cat("flowchart:", class(flowchart::as_fc(N = 1)), "\n")
cat("\nlines of R code that mention alternative text (whole words; matches printed):\n")
for (p in c("selecta", "flowchart", "consort", "dtrackr", "PRISMA2020", "TrialFlowR", "studydesignflow")) {
  src <- unlist(lapply(ls(asNamespace(p), all.names = TRUE), function(f) {
    g <- get(f, asNamespace(p)); if (is.function(g)) deparse(g) else NULL }))
  m <- grep("\\balt[ _.-]?text\\b|\\baria-|screen.?reader|\\baccessib|\\balt *=|<desc|<title", src, value = TRUE, perl = TRUE)
  cat(" ", p, as.character(utils::packageVersion(p)), ":", length(m), "lines", if (length(m)) paste0("e.g. ", trimws(m[1])), "\n")
}
cat("ours", as.character(utils::packageVersion("studydesignflow")), "; ", R.version.string, "\n")
