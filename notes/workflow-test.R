# Can studydesignflow describe these data-collection workflows today? All numbers invented.
# Run from the package folder: Rscript notes/workflow-test.R (pictures go to dev/, git-ignored).
suppressMessages(devtools::load_all(quiet = TRUE))
try_one <- function(nm, expr) {
  r <- tryCatch({
    x <- force(expr)
    flow_save(x, file.path("dev", paste0("goal_", nm, ".png")), dpi = 110)
    s <- if (inherits(x, "studydesignflow")) flow_layout(x)$size else grid_layout(x)$size
    sprintf("drawn (%.1f x %.1f in)", s[[1]], s[[2]])
  }, error = function(e) paste("FAILED:", conditionMessage(e)))
  cat(nm, "->", r, "\n")
}
try_one("scraping", flow_start(c("News sites", "Social media API"), n = c(40, 1), stage = "Sources") |>
  flow_apply("Collection", c("Pages fetched"), n = c(12400, 58000)) |>
  flow_exclude("Not collected", reasons = list(c("Blocked by robots.txt" = 900, "Timeout" = 300), c("Rate limit" = 4100)), n = c(1200, 4100)) |>
  flow_pool("Merged", "Records", n = 65100) |>
  flow_exclude("Cleaning", n = 9100, reasons = c("Duplicates" = 7000, "Wrong language" = 2100)) |>
  flow_apply("Corpus", "Clean records", n = 56000) |>
  flow_select("Annotated sample", "Random sample", n = 2000, method = "simple random sampling") |>
  flow_partition(labels = c("Training", "Validation", "Test"), n = c(1400, 300, 300)))
# 3 annotators label the SAME records, then adjudication
try_one("annotation", flow_start("Records to label", n = 2000) |>
  flow_split("Annotator", c("Annotator 1", "Annotator 2"), n = c(2000, 2000)) |>
  flow_pool("Agreement", "Labels compared", n = 2000) |>
  flow_split("Result", c("Agree", "Disagree"), n = c(1700, 300)) |>
  flow_apply("Adjudication", "Senior reviewer", n = 300, from = "Disagree") |>
  flow_pool("Final", "Gold labels", n = 2000))
# crossover: same people, order swapped, washout
try_one("crossover", flow_start("Randomised", n = 60) |>
  flow_assign("Sequence", c("A then B", "B then A"), n = 30) |>
  flow_apply("Period 1", c("Drug A", "Drug B"), n = c(30, 30)) |>
  flow_apply("Washout", "2 weeks", n = c(29, 30)) |>
  flow_apply("Period 2", c("Drug B", "Drug A"), n = c(28, 30)) |>
  flow_outcome("Analysis", "Analysed", n = c(28, 30)))
# linkage of two registers: matched / unmatched from each side
try_one("linkage", flow_start(c("Hospital register", "Death register"), n = c(5000, 900)) |>
  flow_exclude("Not linked", n = c(4200, 100)) |>
  flow_pool("Linked", "Linked records", n = 800))
# one box feeding two later analyses
try_one("overlap", flow_start("Cells", n = 100) |> flow_split("Group", c("a", "b", "c"), n = c(30, 30, 40)) |>
  flow_pool("Analysis", "Pair", groups = list(c("a", "b"), c("b", "c"))))
# repeat until done (loop): no verb for it
try_one("loop", flow_start("Prompt set", n = 200) |> flow_apply("Model run", "Responses", n = 1000) |>
  flow_split("Check", c("Pass", "Fail"), n = c(900, 100)) |>
  flow_apply("Retry", "Responses", n = 100, from = "Fail") |> flow_pool("Scored", "All responses", n = 1000))
# panel survey: three waves, refreshment sample joins at wave 2
try_one("panel", flow_start(c("Baseline sample", "Refreshment sample"), n = c(1000, 200)) |>
  flow_apply("Wave 1", "Interviewed", n = 900, from = "Baseline sample") |>
  flow_exclude("Lost", n = 150, from = "Interviewed") |>
  flow_pool("Wave 2", "Interviewed at wave 2", n = 950) |>
  flow_exclude("Lost after wave 2", n = 120) |> flow_outcome("Wave 3", "Interviewed at wave 3", n = 830))
# A/B test with staged rollout
try_one("abtest", flow_start("Site visitors", n = 100000) |> flow_exclude("Ineligible", n = 5000, reasons = c("Bots" = 4000, "Opt-out" = 1000)) |>
  flow_assign("Variant", c("Control", "New page"), n = c(47500, 47500), method = "hash of user id") |>
  flow_outcome("Conversion", "Converted", n = c(1900, 2100)))
# software testing: suites x environments
try_one("testing", flow_start("Test cases", n = 480) |> flow_split("Suite", c("Unit", "Integration", "End to end"), n = c(300, 120, 60)) |>
  flow_apply("Environments", "3 browsers each", n = NA_character_) |> flow_split("Result", c("Pass", "Fail")))
# qualitative: interviews to themes
try_one("qualitative", flow_start("Invited", n = 45) |> flow_exclude("Declined", n = 15) |>
  flow_split("Method", c("Interviews", "Focus groups"), n = c(18, 12)) |> flow_apply("Transcribed", "Transcripts", n = c(18, 3)) |>
  flow_pool("Coding", "Coded extracts", n = 640) |> flow_apply("Themes", "Themes", n = 7))
