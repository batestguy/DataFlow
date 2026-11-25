# Side-by-side test with other packages, 2026-11-25

Milestone 6. Script: `notes/comparison.R`; output with versions:
`notes/comparison-output.txt`; pictures: `dev/m6/` (git-ignored, remade by the
script). Eight designs, the same invented numbers in each tool.

Tested by running: `selecta` 0.6.1, `flowchart` 1.0.1, `dtrackr` 0.5.0,
`consort` 1.2.4, `PRISMA2020` 1.1.5 (the review only), `TrialFlowR` 1.0.0
(counts participants; its exports include no drawing function).

## Read this first: limits of the test

- The grader built studydesignflow, chose the eight designs, and knows its
  steps far better than the others'. Designs were fixed in the plan before
  the test, several because studydesignflow was made for them (D5, D6). **This
  favours us.**
- Other tools got at most three tries, from their help pages. "Not managed"
  means by this author, not impossible.
- One reader, not blind. Grades are judgements from looking at the pictures.
- dtrackr text running out of its boxes is probably the font on this machine;
  it is not held against it.

## Grades

True = a reader gets everything in the key. Part = nothing wrong, something
missing. Wrong = a number, unit or grouping the reader would get wrong.
No = refused or not managed. "-" = not tried. Best try of each tool.

| Design | ours | selecta | consort | dtrackr | flowchart | PRISMA2020 |
|---|---|---|---|---|---|---|
| D1 parallel trial | True | True | True | True | Part: reasons | - |
| D2 review, two streams | True | Part: invented top box | Part: invented top box | True | Part: streams never join | True |
| D3 factorial, pooled by A | True | True | No | Part: which cells pool | Part: no pooling | - |
| D4 cluster survey | True, method typed in labels | Wrong: 1,100 women for 1,300 | True, all text typed | - | No: more households than areas | - |
| D5 lab design of the photo | True | Wrong: groups per batch | Part: no joining | Part: which groups join | Part: boxes overlap | - |
| D6 prediction model, external set | True | Wrong: external set looks like part of the split | Part: invented top box | Part: invented end boxes | Part: events missing | - |
| D7 scraping to labelled set | True | Part: invented top box | - | True | - | - |
| D8 two annotators | True only when the labels say "all 2,000" | Wrong: says 4,000 | True, all text typed | True | No: numbers must add up | - |
| **True** | **8** | **2** | **3** | **4** | **0** | **1 of 1** |

Read the eight with care: three of the designs (D5, D6 and the mix of D4) are
ours by birth, two needed wording chosen after seeing what the others did (D4,
D8), and consort's two survey and annotator pictures are free text, checked by
nothing, as ours are.

## Other points

| Point | ours | selecta | consort | dtrackr | flowchart |
|---|---|---|---|---|---|
| Planted wrong numbers (split 390 for 400; reasons 90 for 100) | **drawn, no word** | both warned | drawn, no word (typed boxes) | not possible: counts come from data | refused (the split) |
| Counts from data | yes | yes | yes | always; the only way | yes |
| Rows behind each step | yes | yes (`cohorts()`) | not found | yes (excluded rows) | yes (data kept) |
| Two starting points | yes | yes, but exclusions before the merge are lost (S1, S2) | not managed | yes | side by side only |
| Records joined by key | no | no | no | **yes** | no |
| Same records down two branches | no (wording only) | no | wording only | **yes** | no |
| Change after drawing | `flow_edit()` | not found | `move_box()` | not found | `fc_modify()` |
| Result is a ggplot | yes | no (grid) | no (grid) | no (Graphviz) | no (grid) |
| Alternative text in the code (search of every function) | yes | none found | none found | none found | none found |
| Sample method shown in the picture | **no, description only** | typed label | typed label | typed label | typed label |

Two more observations, on this machine: selecta cut off side boxes at the
right edge in one picture (D7b). An exclusion placed between two sources and
their merge was left out of the picture, with the total unchanged (S1), or
stopped with an error (S2, D2, D7); the drawing gives an internal warning only.
Its help does not say this is allowed. Worth reporting to its author after a
check on another machine (not done). flowchart overlapped twelve boxes at nearly 11
inches wide (D5), with its default box widths.

## What we got wrong, found by this test

1. **Wrong typed numbers pass without a word.** selecta warns by default. This
   is our clearest weakness.
2. **The method never appears in the picture** (`method =` of cluster, select
   and assign goes only into the description). A reader of the figure cannot
   tell random from convenient unless the author types it in a label.
3. **Copies.** With plain wording the picture of D8 misleads; with careful
   wording the picture is right, but our description still says "Records to
   label is split into", which is false. So the fault is in what the package
   knows, not only in how it looks: data mode and any number check would be
   wrong too.
4. Earlier claims corrected: selecta's `combine()` does pool one factor at a
   time, so pooling a factorial is not ours alone; dtrackr already has copies
   and joins for data in R.

## Open questions from the research note

- **triageR** (0.2.0): no function mentions a flow chart, participants or a
  diagram (`notes/fold-grid-scan-output.txt`). It does not draw a TRIPOD flow.
- **Fold grids:** others draw them. `timetk` plots time-series plans (reads
  rsample objects), `spatialsample`, `blockCV` and `mlr3spatiotempcv` have
  plots of folds. Not run. Our grids differ in taking settings or
  the user's own assignments without fitting tools, counts and gaps as text,
  greyscale, and a description; the claim that fold grids are ours alone must
  not be made. The title search of CRAN (30 hits, mostly not about this)
  found blockCV but not timetk, so it is a poor search.

## Does another package already do most of milestone 7?

| Part | Found elsewhere |
|---|---|
| 7a copies | dtrackr, for data in R pipelines; none for typed counts |
| 7b units | none has units as such; text can say them |
| 7c step details | none (tool, who, dates) |
| 7d physical and data steps | none of these six |
| 7e linking | dtrackr joins; unmatched from each side not tried |
| 7f shared groups | none found |
| 7g loops | none found |
| 7h planned and actual | none found |

No. But dtrackr is the nearest neighbour for data work, and anyone whose whole
story is an R data pipeline should use it. For standard clinical flows with
number checking, selecta is as good as ours and better at checking. For
PRISMA 2020, use PRISMA2020.

## Recommendation

Keep milestones 7 to 12, with four changes:

1. **Move `flow_check()` from milestone 8 to the front of milestone 7** (D1 is
   yes). It is small and closes our clearest weakness. It must know copies and
   changes of unit, or it will cry wolf, so it is designed with 7a and 7b.
2. **Show the method in the picture** (new part 7i, small): an option on
   assign, cluster and select, default decided with the user because it
   changes existing pictures.
3. **7a copies before everything else in 7**, and the description and the data
   mode fixed with the picture. The textbook list says the same: copies are
   the main predicted cause of failure there.
4. **Point people elsewhere** in the README now, not at release: PRISMA2020,
   selecta, dtrackr, and `timetk` or `spatialsample` for plots of resampling
   objects.

What is ours after this test: designs with many levels that rejoin, several
kinds of source that are never pooled, outcome events, sampling stages, one
picture type across fields, a ggplot that can be restyled, and a written
description. That is a narrower claim than before, and it is tested.
