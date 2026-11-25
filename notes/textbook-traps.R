# Counting traps in data mode (milestone 6b): what does the package count, and
# does it say anything, when unit labels repeat?
# Run from the package folder: Rscript --vanilla notes/textbook-traps.R
.libPaths(c(normalizePath("dev/complib"), .libPaths()))
suppressMessages(pkgload::load_all(quiet = TRUE, export_all = FALSE))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_page <-", src) - 1)]))

run <- function(id, what, data, by, label, unique_label, real) {
  go <- function(count) {
    w <- character()
    x <- withCallingHandlers(
      do.call(flow_split, list(flow_start(what, data = data, count = count), "Group", by = by)),
      warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
    n <- flow_nodes(x)
    list(n = n$n, warn = w)
  }
  a <- go(label); b <- go(unique_label)
  cat(sprintf("%s real units %d | counting %s: top %s, groups %s%s | counting a unique label: top %s, groups %s%s\n",
              id, real, label, a$n[1], paste(a$n[-1], collapse = " "),
              if (length(a$warn)) " [WARNED]" else " [silent]",
              b$n[1], paste(b$n[-1], collapse = " "), if (length(b$warn)) " [WARNED]" else " [silent]"))
  if (length(a$warn)) cat("   warning:", gsub("\\s+", " ", a$warn[1]), "\n")
}
d <- as.data.frame(get_data("lme4::cake")); d$u <- paste(d$recipe, d$replicate)
run("E02", "Batches", d, "recipe", "replicate", "u", 45)
d <- as.data.frame(get_data("GAD::rats")); d$u <- paste(d$treat, d$rat)
run("F02", "Rats", d, "treat", "rat", "u", 6)
d <- as.data.frame(get_data("AMCP::chapter_10_table_9")); d$u <- paste(d$Gender, d$Trainee)
run("F10", "Trainees", d, "Gender", "Trainee", "u", 6)
d <- as.data.frame(get_data("AMCP::chapter_16_table_4")); d$u <- paste(d$Condition, d$Room)
run("H04", "Rooms", d, "Condition", "Room", "u", 6)
d <- as.data.frame(get_data("lme4::Pastes")); d$u <- paste(d$batch, d$cask)
run("F01", "Casks", d, "batch", "cask", "u", 30)
