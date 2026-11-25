# Textbook designs: what to test, and how

Requested by the user on 2026-11-25: a rigorous list of designs taught in
applied classical-statistics textbooks (t-tests, all kinds of ANOVA,
multilevel models and so on), for the vignettes, to be done **before** any
design is redrawn from a published paper. It is milestone 6b of `ROADMAP.md`.

| File | What it is |
|---|---|
| `textbook-designs-list.md` | the list, one table per family (generated) |
| `textbook-designs.csv` | the same, with the source named on each help page (generated) |
| `textbook-designs.R` | the only place to edit the list |
| `textbook-keys.R`, `.csv`, `-output.txt` | answer keys for the 46 core designs, written before drawing and checked against the data |
| `textbook-designs-output.txt` | the run the numbers below come from, with package versions |

## What has and has not been done

- **Done:** 117 designs listed, each tied to a data set that comes with an R
  package. Every count in the list is computed from the data (0 failed). 115
  help pages name a source; H08 and H14 name none and need one found. Sources
  are textbooks, data collections used in teaching, or the original papers the
  textbooks take the data from; the CSV shows which.
- **Not done:** no design has been drawn. The books themselves were not read.
  The columns "taught with it", "assigned to", "one measurement is a", "the
  picture must say" and "prediction" are the list author's reading. A help
  page is often too short to settle what was randomised, so step 1 below is
  to check it.
- **Update 2026-11-25:** the help page of every core design has been read in
  full and its answer key written. 23 pages state how units were assigned or
  chosen, 15 partly, 8 not at all; what is left open is in the key. A design
  whose source is silent is drawn with neutral words ("received"), never with
  "randomised".
- A data set's help page may follow another edition than the one in hand.

## What counts as a different design

Tests are not designs. A t-test, a Welch test and a rank-sum test can all be
done on the same two groups, and need one picture. Two designs are different
here when a reader would have to draw them differently:

1. **What was assigned, and to what unit** (or nothing: only observed).
2. **What one measurement belongs to**, and whether that unit is smaller than
   the assigned one (subsamples, pupils in classes, sub-plots).
3. **Nested or crossed**: does each lower unit belong to one upper unit, or
   does every unit meet every level?
4. **Same or different units** across conditions (within or between).
5. **Restrictions**: blocks, pairs, rows and columns, strips, sequences.
6. **How much of the full set was run**: complete, incomplete, fraction.
7. **Time**: order, periods, visits, a change at a date.
8. **How units were chosen**: volunteers, random, stratified, by cluster, by
   outcome.
9. **Balance and loss**: equal, unequal, missing.

## The list in numbers

From `textbook-designs-output.txt`.

| Family | Core | Extended | Edge |
|---|---|---|---|
| A. One and two groups | 5 | 5 | 0 |
| B. One factor, separate groups | 4 | 3 | 0 |
| C. Factorial | 4 | 3 | 6 |
| D. Blocks | 6 | 5 | 4 |
| E. Split units | 3 | 6 | 1 |
| F. Nested and crossed random factors | 5 | 6 | 0 |
| G. Within subjects and crossover | 4 | 8 | 0 |
| H. Multilevel and longitudinal | 7 | 9 | 4 |
| I. Observational, categorical, time to event | 4 | 6 | 0 |
| J. Sample surveys | 4 | 2 | 0 |
| K. Special field designs | 0 | 0 | 3 |
| **All** | **46** | **53** | **18** |

- **Core**: in a vignette. **Extended**: drawn and graded, not in a vignette.
- **Edge**: expected to lie outside what a top-down flow can say (fractions,
  response surfaces, lattices, crossed memberships). Tested so the published
  list of unsupported designs rests on evidence, not opinion.

## Predictions, written before testing

Of the 46 core designs, 27 are predicted to draw truthfully today, 8
incompletely and 11 misleadingly. The predictions are not results; they are
here so that testing can prove them wrong.

Three causes account for most predicted failures:

1. **The same units in several conditions** (paired, repeated measures,
   crossover, judges and parts). A split draws one person as several. Planned
   part 7a (copies). The list shows 7a matters far beyond labelling, the case
   it was first proposed for.
2. **Crossing** (Latin square, strip-plot, cross-classified). A tree puts one
   factor inside the other, which is false. Part 7f covers shared groups;
   **nothing planned covers rows by columns**. Decide after the test: add a
   small third kind of picture (a grid of plots), or publish as unsupported.
3. **Time** (visits, periods, carry-over). Milestone 9.

## Known traps, found in the data

Counting labels gives the wrong number of units in these; the picture and the
description must give the right one. They are the first tests for data mode.

| Design | Labels seen | Real units |
|---|---|---|
| E02 cake | 15 batch labels | 45 batches (the help page nests them in recipe; check against the book) |
| E10 cows | 10 cow labels | 30 cows |
| F01 paste | 3 cask labels | 30 casks |
| F02 rats | 2 rat labels | 6 rats |
| F10 trainees | 3 trainee labels | 6 trainees |
| H04 classrooms | 3 room labels | 6 rooms |
| H08 trial in two clinics | 56 patient labels | 111 patients |
| H18 sites | 3 block labels | 27 blocks |

Others: the help page of H03 says 30 dams but the data hold 27 litters; H07,
214 children against 158; H09, 378 randomised against 294 (the diagram must
be able to show both numbers and say the difference is not explained in the
data). The help page of H04 contradicts itself on whether a classroom held
both conditions.

## How each design is tested

1. **Check the source.** Read the book's account (or the original paper) of
   what was assigned, to what, and how. Correct the list if needed. If the
   source cannot be opened, mark the design "source not read".
2. **Write the answer key first**, before drawing: the assigned unit; the
   measured unit; the number at every level; what is nested and what crossed;
   which units repeat; the error term for each effect (or the random part of
   the model).
3. **Draw it** with the current package, from typed counts and, where a data
   set allows, from the data. Both must agree.
4. **Blind reading.** Someone who has seen neither the key nor the code gets
   the picture only, and separately the written description only, and answers
   the questions in step 2.
5. **Grade**: *true* (all answers right from both), *incomplete* (nothing
   wrong, something missing), *misleading* (a reader is led to a wrong unit,
   number or error term), *refused*, or *source not read*. One wrong answer
   about units or crossing makes it misleading, however good it looks.
6. **Record** grade, code, picture, lines of code, size at page width, and
   the prediction beside the result.

The blind reader is a fresh session with no access to the list, then the user
for the core designs. A reader of the same model family as the builder is a
weak check; a statistician outside the project is better, and one is needed
before release.

## Rules for the vignettes

- Ten vignettes, one per family A to J. K has none.
- A core design enters a vignette only when graded *true*. Others are listed
  at the end of the vignette as not supported, with the reason.
- The vignette states the assigned unit, the measured unit and what the
  picture does **not** say (it never names an analysis as the right one).
- Data are used directly only from packages that come with R (`datasets`,
  `MASS`, `nlme`, `survival`). For the rest the counts are typed, with the
  source cited: no textbook data are copied into the package, and no
  dependency is added.
- Every number in a vignette is in the CSV or computed in the vignette.

## Designs wanted, no data set found yet

Not searched for exhaustively. Each needs a textbook source and typed counts:
Youden square; Solomon four-group; stepped-wedge and cluster-crossover
trials; cluster-randomised trial of people (not litters or classrooms);
partially nested trial (therapy groups in one arm); group-sequential trial;
multi-arm multi-stage and platform trials; SMART; single-person (N-of-1)
trials; dose-escalation; regression discontinuity; rotating panel;
capture-recapture; matched cohort (propensity score).
