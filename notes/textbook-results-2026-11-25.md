# Core textbook designs: drawn, read blind, graded (2026-11-25)

Milestone 6b, core part. Package as it is; no code changed.

| File | What |
|---|---|
| `textbook-keys.csv` | answer keys, written before drawing |
| `textbook-core.R` | the code for all 60 pictures |
| `textbook-codes.csv` | which shuffled code was which design |
| `textbook-blind-X.txt`, `-Y.txt`, `-T.txt` | the readers' answers, word for word |
| `textbook-grades.R`, `.csv`, `-output.txt` | grades, reasons, totals (every number below) |
| `textbook-traps.R`, `-output.txt` | counting traps in data mode |

## How

46 designs; 14 were drawn twice, in plain words and in careful words, with the
same verbs. Pictures had shuffled code names. Three fresh readers, each with
no access to keys, code or each other: one read 46 pictures (one version of
each design), one read the other 14, one read the 46 written descriptions and
no pictures. The logs show they opened nothing else. "Cannot tell" was offered
as a good answer.

## Limits, before the numbers

- The readers are the same kind of AI as the builder. They are patient, check
  every sum, and were asked directly whether units repeat. **People will not
  read this way; expect people to do worse.**
- The readers may have been shown project instructions automatically; they
  were told to ignore them.
- Keys, pictures and grades are by one author. Grades have not been checked.
- The questions do not ask about everything: order in time, for example (H05:
  the picture puts time 0 after assignment, and nobody was asked).
- Grades are against the help page. The books were not read.

## Results

| | True | Incomplete | Misleading |
|---|---|---|---|
| Plain pictures (46) | 26 | 17 | 3 |
| Best version of each design (46) | 31 | 15 | 0 |
| Descriptions (46) | 40 | 2 | 4 |
| The 14 in plain words | 8 | 3 | 3 |
| The same 14 in careful words | 13 | 1 | 0 |

**1. The missing method is the main defect, not copies.** In 13 designs the
only fault of the picture is that it does not say units were randomised or
randomly sampled, though the author told the package so. The readers of the
descriptions, where the method is written, got those right. If the method were
shown, and nothing else went wrong, 44 of 46 would have a true version.

**2. My predictions were wrong in both directions.** Of 27 plain pictures
predicted true, 12 were incomplete (11 for the method, which I had not thought
of). Of 11 predicted misleading, 7 were read correctly: the readers saw the
same n repeated and concluded that the same people were in every branch. That
is the kind of inference a hurried person may not make.

**3. Copies still matter**, for three reasons: A05 (material given to a boy
or to a foot), D08 (cannot tell) and the descriptions, which say "split"; and
the number check must know about them.

**4. Careful words carry a lot.** Wrong or incomplete pictures of the 14 went
from six to one with wording alone (paired, Latin square, strip-plot, matched
sets). Advice in the vignettes is worth as much as new features.

**5. Not truthful in any version yet, apart from the method:** C02 (a tree of
two crossed, unequal factors was called nested), D08 (that every pair of
recipes meets equally often is not said).

**6. Size.** 10 of 60 pictures are wider than 7 inches; E02 is 17 inches. They
were legible at full size only.

**7. Data mode passes the counting traps.** In the five tried (E02, F01, F02,
F10, H04) counting a repeated label gives the wrong total, and the package
warns in all five; counting a unique label is right and silent.

## Faults in the descriptions (found by the readers)

| Where | Says | Should |
|---|---|---|
| B03 | "Depression score is randomly assigned to SSRI" | name the people, not the last box |
| D03 | "Tillers of the hill (n = 36), for each of the 4 boxes" read as 36 in all | give the total, or say "each" |
| D08 | "by 6 of the, 12 panellists" | a line break in a note becomes a stray comma |
| E04 | "Replicates (n = 3) is split into G1 (n = 3) ..." | copies (7a) |
| I02, I03 | refers to a "Start" or "Sampled separately" row not named before | name the first row |

## What this changes

1. **Do 7i (method in the picture) before 7a (copies).** Evidence: 13 designs
   against 2 or 3. 7-0 (number check) stays first.
2. Fix the description faults above with 7-0: small and certain.
3. 7a stays: shown, described and counted as copies.
4. Rows by columns and strips can be said in words today (Y02, Y10 read
   correctly), so no new picture type is needed for the vignettes; revisit if
   a person misreads them.
5. Vignettes: use the careful versions; say what wording prevents.

## Still to do in 6b

The user looks at the pictures; the 53 extended and 18 edge designs; a person
who is not the author as reader; someone to check the grades.
