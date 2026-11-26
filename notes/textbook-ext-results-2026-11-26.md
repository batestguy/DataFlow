# Extended and edge textbook designs: drawn, read blind, graded (2026-11-26)

Milestone 6b, second part: the 71 designs not done with the core (53
extended, 18 edge). Package as on `main` after 7a (copies, method shown by
default, `flow_check()`); no package code changed.

| File | What |
|---|---|
| `textbook-ext-keys-AF.R`, `-GK.R` (`.csv`, `-output.txt`) | answer keys, written before drawing, checked against the data |
| `textbook-ext-AF.R`, `textbook-ext-GK.R` | the code for all 126 pictures |
| `textbook-ext-codes-AF.csv`, `-GK.csv` | which shuffled code was which design |
| `textbook-ext-blind-P.txt`, `-Q`, `-R`, `-S` | picture readers' answers, word for word |
| `textbook-ext-blind-U.txt`, `-V` | description readers' answers |
| `textbook-ext-grades.R`, `.csv`, `-output.txt` | grades, reasons, totals (every number below) |

## How

As for the core (`textbook-designs.md`). Two builders, one per half (A to F,
G to K), read every help page and wrote the keys before any drawing code;
then drew a plain version of every design and a careful version of 55. Six
fresh readers, each seeing one folder only: plain-or-only pictures of each
half, second versions of each half, the descriptions of each half. A grader
who wrote neither keys nor pictures graded; a reviewer re-graded 46 pictures
and 17 descriptions, re-ran both key scripts and scanned the reader folders.

## Limits, before the numbers

- Readers, builders, grader and reviewer are all the same kind of AI. People
  will read less carefully: expect them to do worse.
- Keys checked against the data by the same author who wrote them; two keys
  (C10, G12) agreed only after one correction, made before drawing (file
  times support this). H17's limits text was corrected after drawing began;
  no checked number changed.
- Grades are against the help pages; books and original papers not read.
  H08 and H14 name no source.
- The reviewer re-graded about a third; the rest rest on one grader.
- Description codes match picture codes (U01 describes P01). The readers
  were separate, but a reader who saw both would not be blind.
- Three descriptions name the data frame (`measurements`, `records`,
  `trials10`) and its row count. Content words (for example "extra hours of
  sleep") identify some well-known data sets anyway.

## Results

| | True | Incomplete | Misleading |
|---|---|---|---|
| Plain pictures (71) | 47 | 14 | 10 |
| Best version of each design (71) | 68 | 3 | 0 |
| Descriptions (71) | 54 | 13 | 4 |
| The 55 with two versions, plain | 31 | 14 | 10 |
| The same 55, careful | 51 | 4 | 0 |
| Edge (18), plain | 9 | 5 | 4 |
| Edge (18), best version | 18 | 0 | 0 |

**1. Showing the method worked.** In the core, 13 designs failed only
because the picture did not say how units were assigned or chosen. Here no
picture lacks a method that the help page states. One design goes the other
way: H08 draws the random step under each clinic, the source does not say
whether randomisation was within clinic, and all three readers said it was.
Fix in the drawing, not the package: say "within clinic or overall not said".

**2. Wording is the largest cause of plain pictures going wrong** (9 of 24
faults), and the careful version fixed it every time. Examples: "72 trials
per person" under each cue read as 72 in all; "4 per block" without saying
there are 15 treatments; matched days not said to be matched. Advice in the
vignettes is again worth as much as new features.

**3. Crossing is the main fault that words only work around** (7 of 24):
Latin and Graeco-Latin squares, strips, a factor drawn under another it is
crossed with. Nothing planned draws rows by columns.

**4. "Best version 18 of 18 edge designs true" needs reading with care.**
Nine edge designs are true in their plain picture. Of the nine that needed the
careful version:
- six are true because of structure the package draws: separate top boxes
  joined into one (C09, E05, H13, H14, H15, H16);
- three are true only because a sentence in a box says it (D07 Graeco-Latin,
  D10, D11 lattice balance). The reader copied the answer from the sentence.
  That shows the package can carry words; it does not show it can draw rows
  by columns. These stay on the list of designs not supported in a picture.
Which of the nine plain-true edge designs rely on words in the same way was
not separated.

**5. Not true in any version:** G09 and H17 (number of measurements not
shown: trials, pupil-years; 7b units and 9 time), and H08 (point 1).

**6. Predictions were too gloomy again.** Of 26 designs predicted misleading,
25 have a true version and 16 a true plain picture: readers inferred repeated
units from copies ("the same 9 as above"). All 17 predicted true have a true
version.

**7. Size.** 43 of 126 pictures are wider than 7 inches; the widest 49
(E06 plain, a naive full tree), which a reader misread. Careful versions are
narrower.

**8. Data mode passes the counting traps** tried here (H08, H18, and H11,
a trap not in the list: 6 tooth labels, 55 teeth): counting the repeated label
warned, the unique label was right and silent.

## Contradictions found between help pages and data

A06 pairs (12 on the page, 15 in data); C08 called 2^(7-3), data a 2^(7-4);
C13 called constrained, data cover the whole simplex; E06 186 rows and M1-M6
on the page, 192 and M1-M8 in data; F09 "of 3" judges, data 4; F11 the page
contradicts itself on how programs were assigned (careful picture says
"random within each school": add "the source contradicts itself"); G12 seven
nuclei on the page, 3 regions in data; G08 6 or 12 monkeys; G09 both tasks
used all 600 items; H09 378 randomised, 294 in data; H17 the list says
randomised, the page does not; J06 an extra-feeding treatment not in the list;
K03 check every 4th column on the page, every 3rd in data.

## Package faults found (not fixed)

| Fault | Seen in |
|---|---|
| `flow_check()` false problem when a split changes unit (strips: 6 + 3 boxes against 3 replicates) | E05 c, as core E04 (needs 7b) |
| Three or more note lines creep up until they touch the count line | H16 c |
| Empty note prints as "(n = 71; )" in the description | I10 c, H14 c |
| A method with its own brackets prints doubled: "(random sample (50%))" | A10 (worked around) |
| Description names the data frame passed as `data =` | three data-mode flows |
| Row label wraps badly: "Visit 1 (week\n0)" | H09 |
| Check messages chain repeated parent names seven deep | H09 |
| A chain of visits implies each is a subset of the one above (117 then 133) | H09 (9 time) |

## What this changes

1. 7b units next, as planned: G09, H17, H19 and the `flow_check()` false
   problem all need it.
2. Rows by columns: the evidence now rests on 7 designs (core D05, E04; here
   D06, D07, D11, E05, and D13 in words). Decide before the examples page:
   a small grid picture, or publish as not supported. Recommendation: publish
   as not supported for now; revisit after 7b-7h.
3. Fix the small faults above (empty note, doubled brackets, note spacing)
   with 7b; they are small and certain.
4. Vignettes: use the careful versions; H08 redrawn without "within clinic".

## Still to do in 6b

The user looks at a sample of pictures; a person who is not the author as
reader; a cross-family review before anything is published.
