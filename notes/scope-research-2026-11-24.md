# Scope research, 2026-11-24

Why the plan changed after milestone 5, what was read, and what was not. The
plan itself is in `ROADMAP.md`; this file is the evidence behind it. Web pages
were accessed on 2026-11-24.

## The goal (user, 2026-11-24)

> Everyone can describe their data collection process using figures, from lab
> experiments to survey and ML scraping or testing, capturing all workflows.

Working sentence: **DataFlow shows how any dataset came to be.** "All" cannot
be proven. What can be promised is a published list of workflows that draw
truthfully, and a list of those that do not.

## Evidence levels used below

- **[read]** the relevant part of the source itself was read.
- **[summary]** only a search-engine summary was seen. Read the source before
  building on it or citing it anywhere public.
- **[unread]** could not be opened.

## 1. Where DataFlow stands: ten workflows

`notes/workflow-test.R` draws ten workflows that were not among the package
examples (invented numbers). Output: `notes/workflow-test-output.txt` (commit
a107536). Nine draw without an error, one is refused. The pictures of five
(scraping, annotation, panel, testing, crossover) were looked at; the others
were judged from what the verbs can express, so re-view them in milestone 7.

| Workflow | Today | What is missing |
|---|---|---|
| scraping | draws well | units: "n" is sites, then pages, then records |
| abtest | draws well | nothing found (picture not viewed) |
| qualitative | draws well | units: people, transcripts, extracts, themes (picture not viewed) |
| panel | draws well as a flow | no time axis |
| testing | draws | results repeat under every suite |
| crossover | draws | not a timeline |
| annotation | **misleading** | 2,000 records look divided between two annotators; each labelled all 2,000 |
| linkage | draws | cannot show matched and unmatched from each side |
| loop | draws as extra steps | no arrow back to an earlier step |
| overlap | **refused** | a box can feed only one pool per call |

## 2. Other tools

### CRAN scan

`notes/cran-scan.R` searches titles and descriptions of every CRAN package.
Output: `notes/cran-scan-output.txt` (25,848 packages, 22 hits). It finds only
packages whose title or description uses the searched words: `dtrackr`,
`Gmisc`, `visR` and `hassediagrams`, for example, are not among the hits.

| Guideline | Found on CRAN or GitHub |
|---|---|
| CONSORT | `consort`, `flowchart`, `TrialFlowR` (says crossover and cluster trials too), `selecta`, `autocodebook`, `ggnext` |
| PRISMA | `PRISMA2020`, `selecta`, `prismadiagramR`, `metagear` |
| STARD | `selecta`; `stard` (GitHub, beta) [summary] |
| STROBE | `selecta`, `autocodebook` |
| TRIPOD+AI | none found that draws the flow; `triageR` writes TRIPOD+AI-aligned reports, not checked whether it draws one |

Consequence: four of the five templates planned as milestone 6 already exist,
so that milestone was dropped.

### selecta (the closest competitor)

Version 0.6.1 on CRAN, published 2026-08-22; missed by the 2026-11-22
positioning note. Its CRAN description lists CONSORT, STROBE, STARD, PRISMA
and MOOSE layouts, several sources that converge, split and recombine,
factorial designs, typed or computed counts and a pipe-friendly interface; its
README adds arithmetic checking and extraction of the analysis data.

`notes/selecta-text-search.R` searched the package source (22,015 lines;
output in `notes/selecta-text-search-output.txt`): no match for TRIPOD,
training or test sets, outcome events or alternative text; `ggplot2` appears
only in comparisons with `ggsave()`; cluster-randomised layouts appear in a
table of possible extensions. **A text search, not a trial of the package.**
The side-by-side test (milestone 6) settles this.

### Others, each covering part of the range

| Tool | What it does | Level |
|---|---|---|
| `dtrackr` | flowchart from a dplyr pipeline, with exclusions | [summary] |
| `smallsets` | shows sample rows changing through preprocessing | [summary] |
| `edibble` | grammar of experimental designs; makes design tables | [summary] |
| `hassediagrams` | Hasse diagrams of experimental structure | [summary] |
| `swcrt` (GitHub), `steppedwedge` | stepped-wedge design pictures | [summary] |
| NC3Rs Experimental Design Assistant | web tool for animal studies; diagram plus rule-based feedback | [read] |
| `PRISMA2020` | has a web app and CSV template for people who do not code | [summary] |

No tool covering the whole range was found. That is not proof there is none.
Not checked: whether another package draws cross-validation and forecasting
grids like `flow_folds()`; Python, Stata and web tools beyond those named.

## 3. How well collection is reported today

- Trial reports: 263 of 469 (56%) had a CONSORT flow diagram and information
  was often missing (Hopewell et al.,
  <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3260171/>) [summary].
- Periodontology and implantology trials: 44% of flow diagrams lacked at least
  one point of the template
  (<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10134555/>) [summary].
- Annotation: of 298 publications with human annotators, 68 (printed as 22%;
  68/298 is 23%) reported an iterative refinement loop; in 46% of labelling
  datasets the adjudication method was unclear or missing (Klie, Eckart de
  Castilho and Gurevych, <https://arxiv.org/pdf/2307.08153>) [read].
- No study was found that measures how often flow-diagram numbers fail to add
  up. Do not claim that they often do.

## 4. What fields ask people to report

- **Datasheets for Datasets** (Gebru et al.,
  <https://arxiv.org/pdf/1803.09010>, sections 3.3 and 3.4) [read]: how each
  instance was acquired and validated; mechanisms (sensors, manual curation,
  software, APIs); sampling strategy; who collected and how they were paid;
  time frame, and whether it matches when the data were created; ethical
  review; cleaning and labelling; whether raw data were kept; whether the
  software is available.
- **Croissant RAI 1.0**
  (<https://docs.mlcommons.org/croissant/docs/croissant-rai-spec.html>)
  [read]: `dataCollection`, `dataCollectionType` (surveys, web scraping, web
  API, experiments, manual curation...), `dataCollectionMissingData`,
  `dataCollectionRawData`, `dataCollectionTimeframe`,
  `dataPreprocessingProtocol`, `dataManipulationProtocol`,
  `dataImputationProtocol`, `dataAnnotationProtocol`,
  `dataAnnotationPlatform`, `dataAnnotationAnalysis`, `annotationsPerItem`,
  `annotatorDemographics`, `machineAnnotationTools`. The `mlcroissant` page
  uses some different names (for example `dataAnnotationPerItem`): check
  which set validates before exporting.
- **W3C PROV** (<https://www.w3.org/TR/prov-primer/>) [read]: entities,
  activities and agents; used, wasGeneratedBy, wasAssociatedWith,
  wasAttributedTo, wasDerivedFrom.
- **NC3Rs EDA** (<https://eda.nc3rs.org.uk/guide-diagram>,
  `/experimental-design`, `/about`) [read]: nodes carry roles (experiment,
  group, experimental unit, inclusion and exclusion, allocation, blinding,
  intervention, measurement, analysis); solid lines are physical processes,
  dashed lines are data processing.
- **W3C complex images**
  (<https://www.w3.org/WAI/tutorials/images/complex/>) [read]: a short
  description that says where the long one is, and a long description visible
  in the page. `aria-describedby` flattens headings and tables. The nested
  list for flow charts comes from a [summary] of the same tutorial; the page
  as read gave no flow-chart-specific steps.
- **Web scraping**: researcher checklist
  (<https://arxiv.org/html/2410.23432v2>), PRITES framework
  (<https://arxiv.org/pdf/2511.13773>), UK Data Service guidance
  (<https://ukdataservice.ac.uk/learning-hub/data-producer-support/special-contexts-and-additional-considerations/platformandweb-deriveddata/>),
  Kostygina et al. 2023
  (<https://pmc.ncbi.nlm.nih.gov/articles/PMC10795517>) [summary].
- **Surveys**: AAPOR Standard Definitions, 10th edition, final disposition
  codes by mode
  (<https://aapor.org/wp-content/uploads/2023/05/Standards-Definitions-10th-edition.pdf>)
  [summary]. Statistical-office process model (GSBPM): not found, not read.
- **Ecology**: Humboldt Extension to Darwin Core, nested sampling events and
  effort (<https://docs.gbif.org/survey-monitoring-quick-start/en/>,
  <https://eco.tdwg.org/hierarchy/>) [summary].
- **Record linkage**: GUILD (doi:10.1093/pubmed/fdx037) [unread]. Read it
  before building the linking step.
- **Mixed methods**: Ivankova, Creswell and Stick 2006
  (<https://studysites.sagepub.com/foundations/includes/Appendix%20A.Ivankova,%20et%20al.2006.pdf>)
  [summary]; notation rules not found.
- **Longitudinal study windows**: Schneeweiss et al. 2019, Wang and
  Schneeweiss 2022 (<https://pubmed.ncbi.nlm.nih.gov/35520277/>) [summary];
  no R package found that draws them.
- **Language models and ML**: TRIPOD-LLM
  (<https://pubmed.ncbi.nlm.nih.gov/39779929/>) and REFORMS
  (<https://reforms.cs.princeton.edu/>) [summary]. TRIPOD+AI item 20a asks for
  participants with and without the outcome, item 21 for participants and
  events in each analysis [summary].

The common core, used to design milestone 7, is five questions about every
step: **what went in (and in what unit), what was done, who or what did it,
when, and what came out or was lost.**

## 5. No-code route

A spreadsheet template first, then a web form. Shinylive runs R in the browser
and can be hosted free on GitHub Pages; everything is sent to the visitor's
browser, so nothing is uploaded, but no secrets can be kept and large data are
slow (<https://www.datanovia.com/learn/programming/shiny/deployment/shinylive>)
[summary]. A report of ggplot2 failing to load for lack of `munsell` has a
known workaround; test the deployed page.

## 6. Not verified, to do

1. Run `selecta`, `flowchart`, `dtrackr`, `TrialFlowR` (milestone 6).
2. `triageR`: does it draw a TRIPOD+AI flow?
3. Packages that draw fold grids.
4. Read: GUILD, the scraping checklist and PRITES, AAPOR dispositions,
   Schneeweiss 2019, the CONSORT stepped-wedge extension, the mixed-methods
   notation, TRIPOD-LLM.
5. Croissant field names (two sets in use).
6. Other languages' tools (Python, Stata, web).
