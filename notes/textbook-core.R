# Draw the 46 core textbook designs (milestone 6b, step 5).
#
# Run from the package folder:
#   Rscript --vanilla notes/textbook-core.R <folder for the blind readers>
# Version "p" is plain use of the verbs. Version "c" (only where wording could
# change what a reader understands) is careful wording with the same verbs.
# No package code is changed. The word "randomised" or a random method is used
# only where the help page states it (notes/textbook-keys.csv).
#
# Output: coded pictures and descriptions for the readers, and the list of
# codes in notes/textbook-codes.csv (never shown to a reader). Set X holds one
# version of every design; set Y the other version of those that have two.

.libPaths(c(normalizePath("dev/complib"), .libPaths()))
suppressMessages(pkgload::load_all(quiet = TRUE, export_all = FALSE))
src <- readLines("notes/textbook-designs.R")
eval(parse(text = src[seq_len(grep("^help_page <-", src) - 1)]))  # get_data()
out_dir <- commandArgs(TRUE)[1]
stopifnot(!is.na(out_dir))
for (s in c("X", "Y")) dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "text"), showWarnings = FALSE)

D <- list()
add <- function(id, p, c = NULL) D[[id]] <<- list(p = p, c = c)
cnt <- function(...) as.integer(table(...))

# counts taken from the data, so that none is typed from memory
tb <- get_data("medicaldata::strep_tb")
imp <- tapply(tb$improved, tb$arm, sum)[c("Streptomycin", "Control")]
c2 <- get_data("AMCP::chapter_7_table_16")
t2 <- table(c2$Sex, c2$Education)
# coded 1 and 2; the help page says 12 females and 10 males, and that most of
# the females are graduates, which fixes the labels
stopifnot(rowSums(t2) == c(12, 10), t2[1, 1] > t2[1, 2])
dimnames(t2) <- list(c("Female", "Male"), c("Degree", "No degree"))
c3 <- get_data("AMCP::chapter_7_table_24")
t3 <- table(c3$Therapy, c3$Severity)
rate <- levels(factor(get_data("agridat::gomez.seedrate")$rate))
npk <- get_data("datasets::npk")
npk$half <- (as.integer(npk$N) + as.integer(npk$P) + as.integer(npk$K)) %% 2
stopifnot(all(rowSums(table(npk$block, npk$half) > 0) == 1), all(colSums(table(npk$block, npk$half) > 0) == 3))
af <- get_data("daewr::antifungal")
seqs <- tapply(as.character(af$Treat), list(af$Subject), paste, collapse = "")
gsz <- table(factor(seqs, c("AB", "BA")))
hs <- get_data("mlmRev::Hsb82")
hs_s <- c(table(unique(as.data.frame(hs)[c("school", "sector")])$sector))
hs_p <- c(table(hs$sector))
rp <- get_data("WWGbook::ratpup")
rp_l <- table(unique(rp[c("litter", "treatment")])$treatment)[c("Control", "Low", "High")]
rp_p <- table(rp$treatment)[c("Control", "Low", "High")]
he <- margin.table(get_data("datasets::HairEyeColor"), c(1, 2))
vt <- get_data("survival::veteran")
dead <- tapply(vt$status == 1, vt$trt, sum)
ap <- table(get_data("survey::apipop")$stype)[c("E", "M", "H")]
as_n <- table(get_data("survey::apistrat")$stype)[c("E", "M", "H")]
au <- table(get_data("WWGbook::autism")$age)
oat <- get_data("agridat::yates.oats")
gen <- levels(factor(oat$gen)); nit <- levels(factor(oat$nitro))
tmp <- levels(factor(get_data("lme4::cake")$temp))
sp <- get_data("agridat::gomez.stripplot")
fum <- levels(factor(get_data("agridat::cochran.wireworms")$trt))

# A ------------------------------------------------------------------------------
add("A01", flow_start("Files of hyperactive children", stage = "School district") |>
  flow_select("Sample", "Children", n = 6, method = "simple random sampling") |>
  flow_outcome("Measured", "IQ score,\ncompared with the value 98"))
add("A02", flow_start("Women", n = 22) |> flow_split("Build", c("Lean", "Obese"), n = c(13, 9)) |>
  flow_outcome("Measured", "Energy expenditure, 24 hours"))
add("A03", flow_start("Patients", n = 107) |>
  flow_assign("Allocation", c("Streptomycin", "Control"), n = c(55, 52)) |>
  flow_outcome("At 6 months", "Improved", n = as.integer(imp)))
add("A04", flow_start("Women", n = 11) |> flow_split("Occasion", c("Premenstrual", "Postmenstrual"), n = 11) |>
  flow_outcome("Measured", "Energy intake"),
  flow_start("Women", n = 11) |>
    flow_split("Occasion", c("Premenstrual:\nall 11 women", "Postmenstrual:\nthe same 11 women")) |>
    flow_outcome("Measured", "Energy intake", n = 11))
add("A05", flow_start("Boys", n = 10) |> flow_assign("Shoe material", c("Material A", "Material B"), n = 10, method = NULL) |>
  flow_outcome("Measured", "Wear"),
  flow_start("Boys", n = 10, note = "each boy is a pair of feet") |>
    flow_split("Foot, within each boy", c("One foot of every boy:\nmaterial A", "His other foot:\nmaterial B")) |>
    flow_outcome("Measured", "Wear of the foot's shoe", n = 10))
# B
add("B01", flow_start("Plants", n = 30) |> flow_split("Treatment received", c("Control", "Treatment 1", "Treatment 2"), n = 10) |>
  flow_outcome("Measured", "Dried weight"))
add("B02", flow_start("Children", n = 24) |> flow_assign("Study time", c("1 minute", "2 minutes", "3 minutes", "4 minutes"), n = 6) |>
  flow_outcome("Measured", "Words recalled"))
add("B03", flow_start("People with depression", n = 30) |> flow_apply("Before", "Depression score", n = 30) |>
  flow_assign("Condition", c("SSRI", "Placebo", "Wait list"), n = 10) |> flow_outcome("After", "Depression score", n = 10))
add("B05", flow_start("Batches of intermediate", n = 6) |> flow_apply("Preparations", "5 per batch", n = 30) |>
  flow_outcome("Measured", "Yield of the preparation", n = 30))
# C
add("C01", flow_start("People", n = 30) |> flow_split("Biofeedback received", c("Present", "Absent"), n = 15) |>
  flow_split("Drug received", c("X", "Y", "Z"), n = 5) |> flow_outcome("Measured", "Blood pressure"),
  flow_start("People", n = 30) |> flow_split("Biofeedback by drug (2 x 3, crossed)",
    c("Biofeedback + X", "Biofeedback + Y", "Biofeedback + Z", "X only", "Y only", "Z only"), n = 5) |>
    flow_outcome("Measured", "Blood pressure, once per person"))
add("C02", flow_start("New employees", n = 22) |> flow_split("Sex", rownames(t2), n = rowSums(t2)) |>
  flow_split("Education", colnames(t2), n = lapply(seq_len(nrow(t2)), function(i) as.integer(t2[i, ]))) |>
  flow_outcome("Measured", "Salary"))
add("C03", flow_start("People with depression", n = 45) |>
  flow_assign("Therapy", c("Cognitive-behavioural", "Rogerian", "Assertiveness training"), n = 15) |>
  flow_split("Severity, classified afterwards", c("1", "2", "3"), n = lapply(1:3, function(i) as.integer(t3[i, ]))) |>
  flow_outcome("Measured", "MMPI"))
add("C04", flow_start("People", n = 72) |> flow_split("Biofeedback received", c("Present", "Absent"), n = 36) |>
  flow_split("Drug received", c("X", "Y", "Z"), n = 12) |> flow_split("Diet received", c("Diet", "None"), n = 6) |>
  flow_outcome("Measured", "Blood\npressure"),
  flow_start("People", n = 72) |>
    flow_apply("Treatments received", "2 biofeedback x 3 drugs x 2 diets, crossed:\n12 combinations, one per person,\n6 people in each", n = 72) |>
    flow_outcome("Measured", "Blood pressure, once per person", n = 72))
# D
add("D01", flow_start("Field") |> flow_split("Block", paste("Block", 1:4, "\n6 plots")) |>
  flow_assign("Seed rate, random within block", as.list(rep("6 rates,\none plot each", 4))) |>
  flow_outcome("Measured", "Yield of\nthe plot"))
add("D02", flow_start("People", n = 9) |> flow_split("Stool", c("T1", "T2", "T3", "T4"), n = 9) |> flow_outcome("Measured", "Effort"),
  flow_start("People", n = 9) |> flow_apply("Stools", "Each person tries all 4 stools", n = 36) |>
    flow_outcome("Measured", "Effort, per person and stool", n = 36))
add("D03", flow_start("Field") |> flow_split("Block", paste("Block", 1:4, "\n9 plots")) |>
  flow_apply("Fertiliser", "9 fertilisers,\none plot each") |> flow_apply("Samples", "4 hills\nper plot") |>
  flow_outcome("Measured", "Tillers of\nthe hill", n = 36))
add("D05", flow_start("Plots", n = 25) |> flow_split("Row", paste("Row", 1:5), n = 5) |>
  flow_apply("Fumigant", "5 fumigants,\none plot each") |> flow_outcome("Measured", "Wireworms\nin the plot"),
  flow_start("Plots", n = 25, note = "5 rows by 5 columns") |>
    flow_split("Fumigant (Latin square)", paste0("Fumigant ", fum), n = 5, note = "once in every row,\nonce in every column") |>
    flow_outcome("Measured", "Wireworms\nin the plot"))
add("D08", flow_start("Panellists", n = 12) |> flow_split("Recipe", c("A", "B", "C", "D"), n = 6) |> flow_outcome("Measured", "Score"),
  flow_start("Panellists", n = 12) |> flow_apply("Tastings", "Each tastes 2 of the 4 recipes", n = 24) |>
    flow_split("Recipe", c("A", "B", "C", "D"), n = 6, note = "by 6 of the\n12 panellists") |> flow_outcome("Measured", "Score"))
add("D15", flow_start("Plots", n = 24) |> flow_split("Block", paste("Block", 1:6), n = 4) |>
  flow_apply("N, P, K", "4 of the 8\ncombinations") |> flow_outcome("Measured", "Yield of\nthe plot"),
  flow_start("Blocks of 4 plots", n = 6) |>
    flow_split("Half of the 8 combinations of N, P, K", c("N, P, K, NPK", "none, NP, NK, PK"), n = 3, note = "blocks") |>
    flow_apply("Plots", "4 per block, each with a different\ncombination from its half", n = 12) |>
    flow_outcome("Measured", "Yield of the plot", n = 12))
# E
add("E01", flow_start("Blocks", n = 6) |> flow_apply("Whole plots", "3 per block", n = 18) |>
  flow_split("Variety, applied to whole plots", gen, n = 6) |> flow_apply("Sub-plots", "4 per whole plot", n = 24) |>
  flow_split("Nitrogen, applied to sub-plots", nit, n = 6) |> flow_outcome("Measured", "Yield"))
add("E02", flow_start("Batches of cake mix", n = 45) |> flow_split("Recipe, applied to batches", c("A", "B", "C"), n = 15) |>
  flow_apply("Portions", "6 per batch", n = 90) |> flow_split("Baking temperature, applied to portions", tmp, n = 15) |>
  flow_outcome("Measured", "Angle"))
add("E04", flow_start("Replicates", n = 3) |> flow_split("Variety, horizontal strips", levels(factor(sp$gen)), n = 3) |>
  flow_split("Nitrogen, vertical strips", levels(factor(sp$nitro)), n = 3) |> flow_outcome("Measured", "Yield"),
  flow_start("Replicates", n = 3) |>
    flow_split("Strips, in each replicate", c("6 horizontal strips,\none variety each", "3 vertical strips,\none nitrogen rate each"), n = c(18, 9)) |>
    flow_pool("Crossings", "Every variety strip crosses every\nnitrogen strip of its replicate", n = 54) |>
    flow_outcome("Measured", "Yield of the crossing", n = 54))
# F
add("F01", flow_start("Deliveries of paste") |> flow_select("Deliveries", "Sampled", n = 10, method = "simple random sampling") |>
  flow_select("Casks", "3 per delivery", n = 30, method = "simple random sampling") |>
  flow_apply("Analyses", "2 per cask sample", n = 60) |> flow_outcome("Measured", "Paste strength", n = 60))
add("F02", flow_start("Rats", n = 6) |> flow_split("Treatment received", paste("Treatment", 1:3), n = 2) |>
  flow_apply("Liver preparations", "3 per rat", n = 6) |> flow_apply("Readings", "2 per preparation", n = 12) |>
  flow_outcome("Measured", "Glycogen"))
add("F03", flow_start("Lots", n = 8) |> flow_split("Source", c("Source 1", "Source 2"), n = 4) |>
  flow_select("Wafers", "3 per lot", n = 12) |> flow_select("Sites", "3 per wafer", n = 36) |>
  flow_outcome("Measured", "Oxide thickness"))
add("F06", flow_start("Parts", n = 10) |> flow_split("Operator", paste("Operator", 1:3), n = 10) |>
  flow_apply("Repeats", "2 measurements each", n = 20) |> flow_outcome("Measured", "Size"),
  flow_start("Parts", n = 10) |> flow_apply("Operators", "The same 3 operators\nmeasure every part") |>
    flow_apply("Repeats", "Each operator measures\neach part twice", n = 60) |> flow_outcome("Measured", "Measurement", n = 60))
add("F10", flow_start("Clinical trainees", n = 6) |> flow_split("Gender of trainee", c("Male", "Female"), n = 3) |>
  flow_assign("Clients", as.list(rep("4 clients per trainee", 2)), n = 12) |> flow_outcome("Measured", "Severity rating\nof the client"))
# G
add("G01", flow_start("Children", n = 12) |> flow_split("Age in months", c("30", "36", "42", "48"), n = 12) |> flow_outcome("Measured", "Score"),
  flow_start("Children", n = 12) |> flow_apply("30 months", "Score", n = 12) |> flow_apply("36 months", "Score", n = 12) |>
    flow_apply("42 months", "Score", n = 12) |> flow_outcome("48 months", "Score", n = 12))
add("G02", flow_start("People", n = 10) |> flow_split("Noise", c("Absent", "Present"), n = 10) |>
  flow_split("Angle", c("0", "4", "8"), n = 10) |> flow_outcome("Measured", "Reaction\ntime"),
  flow_start("People", n = 10) |>
    flow_apply("Conditions", "Each person does all 6:\nnoise (absent, present)\nby angle (0, 4, 8), crossed", n = 60) |>
    flow_outcome("Measured", "Reaction time,\nper person and condition", n = 60))
add("G03", flow_start("People", n = 20) |> flow_split("Age group", c("Young", "Old"), n = 10) |>
  flow_split("Angle", c("0", "4", "8"), n = 10) |> flow_outcome("Measured", "Reaction\ntime"),
  flow_start("People", n = 20) |> flow_split("Age group", c("Young", "Old"), n = 10) |>
    flow_apply("Angles", "Each person does all 3\nangles (0, 4, 8)", n = 30) |>
    flow_outcome("Measured", "Reaction time,\nper person and angle", n = 30))
add("G05", flow_start("People", n = 17) |>
  flow_split("Sequence received", c("A then B", "B then A"), n = as.integer(gsz)) |>
  flow_apply("Period 1", c("Drug A", "Drug B"), n = as.integer(gsz)) |>
  flow_apply("Period 2", c("Drug B", "Drug A"), n = as.integer(gsz)) |>
  flow_outcome("Measured", "Response in each period"))
# H
add("H01", flow_start("Schools", n = 160) |> flow_split("Sector", names(hs_s), n = as.integer(hs_s)) |>
  flow_apply("Pupils", "14 to 67 per school", n = as.integer(hs_p[names(hs_s)])) |> flow_outcome("Measured", "Maths score\nof the pupil"))
add("H02", flow_start("Schools", n = 107) |> flow_select("Classrooms", "1 to 9 per school", n = 312, method = "random selection") |>
  flow_apply("Pupils", "1 to 10 per classroom", n = 1190) |> flow_outcome("Measured", "Maths gain of the pupil", n = 1190))
add("H03", flow_start("Dams", n = 30) |> flow_assign("Dose", c("Control", "Low", "High")) |>
  flow_apply("Litters in the data", "Litters", n = as.integer(rp_l)) |> flow_apply("Pups", "Pups", n = as.integer(rp_p)) |>
  flow_outcome("Measured", "Birth weight\nof the pup"))
add("H04", flow_start("Classrooms", n = 6) |> flow_split("Condition received", c("Control", "Treatment"), n = 3) |>
  flow_apply("Children", "Children", n = c(15, 14)) |> flow_outcome("Measured", "Reasoning score\nof the child"))
add("H05", flow_start("People", n = 100) |> flow_assign("Arm", c("Treatment", "Control"), n = c(44, 56)) |>
  flow_apply("Time 0", "Measured", n = c(44, 56)) |> flow_apply("Time 4", "Measured", n = c(44, 56)) |>
  flow_outcome("Time 8", "Measured", n = c(44, 56)))
add("H06", flow_start("Children", n = 27) |> flow_split("Sex", c("Male", "Female"), n = c(16, 11)) |>
  flow_apply("Age 8", "X-ray", n = c(16, 11)) |> flow_apply("Age 10", "X-ray", n = c(16, 11)) |>
  flow_apply("Age 12", "X-ray", n = c(16, 11)) |> flow_outcome("Age 14", "X-ray", n = c(16, 11)))
add("H07", flow_start("Children in the study", n = 214) |> flow_exclude("Not in the data", n = 56) |>
  flow_apply("Data", "Children", n = 158) |> flow_apply("Age 2", "Seen", n = au[["2"]]) |> flow_apply("Age 3", "Seen", n = au[["3"]]) |>
  flow_apply("Age 5", "Seen", n = au[["5"]]) |> flow_apply("Age 9", "Seen", n = au[["9"]]) |> flow_outcome("Age 13", "Seen", n = au[["13"]]))
# I
add("I01", flow_start("Students surveyed", n = 592) |> flow_split("Hair", rownames(he), n = as.integer(rowSums(he))) |>
  flow_split("Eyes", colnames(he), n = lapply(1:4, function(i) as.integer(he[i, ]))))
add("I02", flow_start(c("Cases", "Controls"), n = c(200, 775), stage = "Sampled separately") |>
  flow_outcome("Recorded", "Alcohol, tobacco, age group"))
add("I03", flow_start(c("Cases", "Controls"), n = c(83, 165)) |> flow_outcome("Recorded", "Earlier abortions"),
  flow_start("Matched sets", n = 83) |> flow_split("Women in the set", c("1 case, 2 controls", "1 case, 1 control"), n = c(82, 1), note = "sets") |>
    flow_apply("Women", "Women", n = c(246, 2)) |> flow_outcome("Recorded", "Earlier abortions"))
add("I06", flow_start("Patients", n = 137) |> flow_assign("Regimen", c("Standard", "Test"), n = c(69, 68)) |>
  flow_split("End of follow-up", c("Died", "Censored"), n = list(c(dead[[1]], 69 - dead[[1]]), c(dead[[2]], 68 - dead[[2]]))))
# J
add("J01", flow_start("Counties", n = 3078) |> flow_select("Sample", "Counties", n = 300, method = "simple random sampling") |>
  flow_outcome("Measured", "Acres of the county", n = 300))
add("J02", flow_start("Schools", n = sum(ap)) |> flow_split("School type", c("Elementary", "Middle", "High"), n = as.integer(ap)) |>
  flow_select("Sample", "Schools", n = as.integer(as_n), method = "simple random sampling within type") |>
  flow_outcome("Measured", "Performance\nof the school"))
add("J03", flow_start("School districts", n = 757) |> flow_select("Districts", "Sampled", n = 15) |>
  flow_apply("Schools", "All schools in them", n = 183) |> flow_outcome("Measured", "Performance of the school", n = 183))
add("J04", flow_start("School districts", n = 757) |> flow_select("Districts", "Sampled", n = 40) |>
  flow_select("Schools", "1 to 5 per district", n = 126) |> flow_outcome("Measured", "Performance of the school", n = 126))

# Codes ----------------------------------------------------------------------------
keys <- utils::read.csv("notes/textbook-keys.csv", encoding = "UTF-8")
stopifnot(setequal(names(D), keys$id))
set.seed(20261125)
two <- names(D)[!vapply(D, function(x) is.null(x$c), TRUE)]
first <- setNames(rep("p", length(D)), names(D))
first[sample(two, floor(length(two) / 2))] <- "c"
codes <- rbind(
  data.frame(id = names(D), version = first[names(D)], set = "X"),
  data.frame(id = two, version = ifelse(first[two] == "p", "c", "p"), set = "Y"))
codes <- codes[sample(nrow(codes)), ]
codes <- codes[order(codes$set), ]
codes$code <- paste0(codes$set, sprintf("%02d", ave(seq_len(nrow(codes)), codes$set, FUN = seq_along)))
codes$width <- codes$height <- NA_real_
for (i in seq_len(nrow(codes))) {
  x <- D[[codes$id[i]]][[codes$version[i]]]
  flow_save(x, file.path(out_dir, codes$set[i], paste0(codes$code[i], ".png")), dpi = 100)
  s <- flow_layout(x)$size
  codes$width[i] <- round(s[["width"]], 1); codes$height[i] <- round(s[["height"]], 1)
}
# descriptions: set X's version of every design, for the third reader
t <- codes[codes$set == "X", ]
txt <- unlist(lapply(seq_len(nrow(t)), function(i)
  c(paste0("### T", substr(t$code[i], 2, 3)), unclass(flow_describe(D[[t$id[i]]][[t$version[i]]])), "")))
writeLines(txt, file.path(out_dir, "text", "descriptions.txt"))
utils::write.csv(codes, "notes/textbook-codes.csv", row.names = FALSE)
cat("designs", length(D), "; with two versions", length(two), "; pictures", nrow(codes), "\n")
cat("wider than 7 inches:", paste(codes$id[codes$width > 7], codes$version[codes$width > 7], codes$width[codes$width > 7], collapse = "; "), "\n")
