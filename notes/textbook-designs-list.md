# Textbook designs: the list

Written by `notes/textbook-designs.R`; do not edit by hand. Method and
rules: `notes/textbook-designs.md`. Counts are computed from the data.
The columns taught, assigned, must say and prediction are the list
author's reading, not quotations, and are checked against the book
before a design is tested.

## A. One and two groups

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| A01 | core | One sample against a known value | `AMCP::chapter_3_table_1` | one-sample t; Wilcoxon signed-rank | nothing | child | One group, how it was chosen, compared with a fixed value | children = 6 | T |
| A02 | core | Two independent observed groups, unequal sizes | `ISwR::energy` | two-sample t; Welch t; Wilcoxon rank-sum | nothing (groups observed) | woman | The groups were not assigned; sizes differ | women = 22; lean = 13; obese = 9 | T |
| A03 | core | Two randomised groups, yes/no outcome | `medicaldata::strep_tb` | two-proportion test; chi-squared; Fisher exact; risk ratio | patient, random | patient | Randomised; arm sizes; outcome per arm | patients = 107; arms = 2; per_arm = 52-55 | T |
| A04 | core | Paired: same person before and after | `ISwR::intake` | paired t; Wilcoxon signed-rank | nothing | person-occasion | 11 people measured twice, not 22 people | people = 11; measurements = 22 | M; 7a copies, 9 time |
| A05 | core | Paired: both treatments inside one person | `MASS::shoes` | paired t; randomisation test | foot, within boy | foot | Each boy wears both materials; the boy is the block | boys = 10; feet = 20 | M; 7a copies |
| A06 | extended | Matched pairs of different units | `agridat::darwin.maize` | paired t; sign test; Fisher's randomisation test | plant, within pair | plant | Pairs sit inside pots; each pair holds one of each type | plants = 30; pairs = 15; pots = 4; pairs_per_pot = 3-5 | I; pairing not drawable except as a block row |
| A07 | extended | Same people, two drugs (often misread as two groups) | `datasets::sleep` | paired t | drug, within patient | patient-drug | 10 patients, not two groups of 10 | patients = 10; rows = 20; rows_per_patient = 2 | M; 7a copies |
| A08 | extended | Two methods on the same subjects | `ISwR::wright` | limits of agreement (Bland-Altman) | nothing | subject-meter | Every subject measured by both meters | subjects = 17; meters = 2 | M; 7a copies |
| A09 | extended | Matched pairs, time to event | `MASS::gehan` | stratified log-rank; paired survival | patient, within pair | patient | 21 pairs, one of each treatment in a pair | patients = 42; pairs = 21; per_pair = 2 | I; pairing |
| A10 | extended | One eye treated, the other is the control | `survival::diabetic` | paired survival; frailty model | eye, within patient | eye | Treatment is assigned to eyes, both eyes are followed | patients = 197; eyes = 394; eyes_per_patient = 2 | M; 7a copies |

## B. One factor, separate groups

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| B01 | core | Completely randomised, balanced | `datasets::PlantGrowth` | one-way ANOVA; Tukey; Dunnett; Kruskal-Wallis | plant, random | plant | Three groups of equal size | plants = 30; groups = 3; per_group = 10 | T |
| B02 | core | Completely randomised, quantitative levels | `AMCP::chapter_6_table_1` | trend (polynomial) contrasts | child, random | child | The levels are ordered amounts | children = 24; levels = 4; per_level = 6 | T |
| B03 | core | Randomised groups with a measurement before | `AMCP::chapter_9_table_7` | ANCOVA; gain scores | person, random | person, twice | Measured before and after; randomised in between | people = 30; conditions = 3; per_condition = 10 | I; 9 time |
| B04 | extended | Unequal groups with before and after | `MASS::anorexia` | ANCOVA; unequal n | person | person, twice | Group sizes differ | people = 72; groups = 3; per_group = 17-29 | I; 9 time |
| B05 | core | One random factor | `lme4::Dyestuff` | variance components; random-effects one-way ANOVA; ICC | nothing (batches sampled) | preparation | Batches were sampled, not treatments assigned | batches = 6; preparations = 30; per_batch = 5 | T |
| B06 | extended | Randomised groups, counts | `datasets::InsectSprays` | one-way ANOVA after transformation; Poisson GLM | plot | plot | Units and group sizes | plots = 72; sprays = 6; per_spray = 12 | T |
| B07 | extended | Randomised groups, several outcomes | `carData::Baumann` | MANOVA; planned contrasts; ANCOVA | student, random | student, several tests | Same students take every test | students = 66; groups = 3; per_group = 22; tests = 5 | I; 7a copies |

## C. Factorial

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| C01 | core | Two-way factorial, balanced | `AMCP::chapter_7_table_5` | two-way ANOVA; simple effects | person, random to cells | person | Both factors fully crossed; n per cell; margins | people = 30; A = 2; B = 3; per_cell = 5 | I; 7f shared groups for margins |
| C02 | core | Two-way, unbalanced and only observed | `AMCP::chapter_7_table_16` | Type I, II, III sums of squares | nothing | person | Nothing was assigned; unequal cells | people = 22; cells = 4; per_cell = 3-8 | T |
| C03 | core | One assigned factor by one observed factor | `AMCP::chapter_7_table_24` | treatment by blocking-factor ANOVA | person, to therapy only | person | Therapy was assigned; severity was classified afterwards (help page) | people = 45; therapy = 3; severity = 3; per_cell = 3-7 | T |
| C04 | core | Three-way factorial | `AMCP::chapter_8_table_12` | three-way ANOVA; three-way interaction | person, random to cells | person | All cells, n per cell, readable at page width | people = 72; drug = 3; biofeedback = 2; diet = 2; per_cell = 6 | I; size |
| C05 | extended | Two-way with quantitative factor | `datasets::ToothGrowth` | two-way ANOVA; trend by group | animal | animal | Ordered doses crossed with supplement | animals = 60; supp = 2; dose = 3; per_cell = 10 | T |
| C06 | extended | Two-way, two crossed observed origins | `HSAUR3::foster` | unbalanced two-way ANOVA | litter, to foster mother | litter | Genotype of litter crossed with genotype of mother; unequal cells | litters = 61; cells = 16; per_cell = 2-5 | T |
| C07 | extended | Two-level factorial, unreplicated | `daewr::chem` | half-normal plot; Lenth | run | run | One run per combination: no replication | runs = 16; combinations = 16; per_combination = 1 | I; size |
| C08 | edge | Two-level fractional factorial | `daewr::arso` | aliasing; resolution | run | run | Which combinations were left out, and why | runs = 8; factors = 7; full = 128 | I; a tree cannot show a fraction well |
| C09 | edge | Fraction followed by its mirror image | `daewr::augm` | fold-over; sequential experimentation | run | run | Second fraction added after the first | runs = 16; fractions = 2; per_fraction = 8 | I; 9 time |
| C10 | edge | Fractional factorial, three levels, field | `agridat::chinloy.fractionalfactorial` | confounding in fractions | plot | plot | Fraction of the full set, in blocks | plots = 81; blocks = 9; combinations = 81; full = 243 | I; fraction |
| C11 | edge | Central composite design in blocks | `daewr::cement` | response surface | run | run | Corner, axial and centre runs; two blocks | runs = 20; blocks = 2; points = 15 | I; levels are points, not groups |
| C12 | edge | Box-Behnken design | `daewr::Treb` | response surface | run | run | Edge points and centre runs | runs = 15; points = 13 | I |
| C13 | edge | Constrained mixture | `daewr::pest` | Scheffe mixture model | run | run | Parts add up to a whole | runs = 13; points = 13 | I |

## D. Blocks

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| D01 | core | Randomised complete blocks | `agridat::gomez.seedrate` | two-way ANOVA without interaction; Friedman | plot, random within block | plot | Every treatment once in every block | plots = 24; blocks = 4; treatments = 6; per_block = 6; per_cell = 1 | T |
| D02 | core | Person as the complete block | `nlme::ergoStool` | RCB with random subjects; repeated measures | stool, within person | person-stool | Each person tries every stool | people = 9; stools = 4; rows = 36; per_cell = 1 | M; 7a copies |
| D03 | core | Complete blocks with subsamples | `agridat::grover.rcb.subsample` | ANOVA with sampling error; pseudoreplication | plot | hill within plot | Plots are the replicates, hills are not | blocks = 4; plots = 36; hills = 144; hills_per_plot = 4 | T |
| D04 | extended | Generalised blocks (replicates inside block) | `daewr::rcb` | block by treatment interaction as error | shot, within golfer | shot | Golfer is the block; several shots per cell | golfers = 9; heights = 3; shots = 135; per_cell = 5 | M; 7a copies |
| D05 | core | Latin square, field | `agridat::cochran.wireworms` | Latin square ANOVA | plot, random within row and column | plot | Two blockings at once; each treatment once per row and column | plots = 25; rows = 5; cols = 5; treatments = 5; per_row_trt = 1; per_col_trt = 1 | M; a tree nests rows over columns |
| D06 | extended | Latin square, laboratory | `datasets::OrchardSprays` | Latin square ANOVA | cell | cell | As D05, larger | cells = 64; rows = 8; cols = 8; treatments = 8; per_row_trt = 1 | M |
| D07 | edge | Graeco-Latin square, replicated | `agridat::devries.pine` | Graeco-Latin ANOVA | plot | plot | Two treatments each balanced over rows and columns | plots = 36; squares = 4; spacing = 3; thinning = 3; plots_per_square = 9 | M |
| D08 | core | Balanced incomplete blocks, tasting | `daewr::taste` | intra-block analysis | recipe, to panellist | panellist-recipe | Each panellist tastes only some recipes | panellists = 12; recipes = 4; rows = 24; recipes_per_panellist = 2; panellists_per_recipe = 6 | M; 7a copies to a subset |
| D09 | extended | Balanced incomplete blocks, field | `agridat::cochran.bib` | intra- and inter-block analysis | plot | plot | 13 lines, 4 per block | blocks = 13; lines = 13; plots = 52; per_block = 4; per_line = 4 | I |
| D10 | edge | Partially balanced incomplete blocks | `SASmixed::PBIB` | mixed model with random blocks | plot | plot | Which treatments meet in a block | blocks = 15; treatments = 15; plots = 60; per_block = 4 | I |
| D11 | edge | Balanced lattice square | `agridat::cochran.lattice` | lattice analysis | plot | plot | Replicates, rows and columns | reps = 5; plots = 80; treatments = 16; per_rep = 16 | I |
| D12 | extended | Resolvable incomplete blocks (alpha) | `agridat::john.alpha` | mixed model, blocks within replicates | plot | plot | Incomplete blocks nested in complete replicates | reps = 3; blocks = 18; plots = 72; varieties = 24; per_block = 4 | I |
| D13 | edge | Row-column design | `agridat::kempton.rowcol` | REML row-column analysis | plot | plot | Rows and columns inside replicates | reps = 2; rows = 10; cols = 14; plots = 68; lines = 35 | M |
| D14 | extended | Augmented: repeated checks, single new lines | `agridat::kling.augmented` | augmented-design ANOVA | plot | plot | Checks in every block, new lines once | blocks = 6; plots = 68; lines = 53; lines_seen_once = 50; most_plots_for_a_line = 6 | T |
| D15 | core | Factorial confounded in blocks | `datasets::npk` | confounding; intra-block analysis | plot, random within block | plot | Each block holds half the combinations | plots = 24; blocks = 6; combinations = 8; per_block = 4 | I |

## E. Split units

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| E01 | core | Split-plot in blocks | `agridat::yates.oats` | split-plot ANOVA with two error terms | variety to whole plot; nitrogen to sub-plot | sub-plot | Two sizes of unit, each with its own randomisation | blocks = 6; whole_plots = 18; sub_plots = 72; varieties = 3; nitrogen = 4; per_whole_plot = 4 | T |
| E02 | core | Split-plot without blocks (batches) | `lme4::cake` | split-plot ANOVA; mixed model | recipe to batch; temperature to portion | cake | Batch numbers repeat across recipes but are different batches | recipes = 3; batch_labels = 15; batches = 45; cakes = 270; per_batch = 6 | T |
| E03 | extended | Split-split-plot | `agridat::gomez.splitsplit` | three error terms | three nested sizes of plot | sub-sub-plot | Three sizes of unit | blocks = 3; whole = 15; sub = 45; subsub = 135 | I; size |
| E04 | core | Strip-plot (criss-cross) | `agridat::gomez.stripplot` | strip-plot ANOVA, three error terms | each factor to its own strip | crossing of two strips | Neither factor is nested in the other | blocks = 3; variety_strips = 18; nitrogen_strips = 9; plots = 54 | M; a tree forces one inside the other |
| E05 | edge | Strip-split-plot | `agridat::gomez.stripsplitplot` | four error terms | two strips, then split | sub-plot | As E04, then split | blocks = 3; crossings = 54; plots = 108 | M |
| E06 | extended | Split-plot with subsamples | `agridat::gomez.splitplot.subsample` | split-plot with sampling error | management to whole plot; time to sub-plot | hill sample | Samples are not replicates | blocks = 3; whole = 24; sub = 96; samples = 192 | T |
| E07 | extended | Group-balanced split-plot | `agridat::gomez.groupsplit` | group-balanced ANOVA | fertiliser; variety group; variety | plot | Varieties differ between groups | blocks = 3; fert = 2; groups = 3; varieties = 45; plots = 270; varieties_per_group = 15 | I; size |
| E08 | extended | Split-plot, industrial, unreplicated | `daewr::plasma` | two half-normal plots | hard-to-change factors to the larger unit | run | Some factors are reset less often; the data do not mark the whole plots | runs = 32; combinations = 32 | I |
| E09 | extended | Split-plot with whole plots in blocks, industrial | `daewr::sausage` | split-plot mixed model | two factors to batch; two to sub-unit | sub-unit | Batch is the whole plot | blocks = 2; batches = 8; whole_combinations = 4; runs = 32 | T |
| E10 | extended | Split-plot in time | `daewr::strung` | split-plot ANOVA; mixed model for repeated measures | diet to cow | cow-week | Cow numbers repeat across diets; weeks are not randomised | diets = 3; cow_labels = 10; cows = 30; rows = 120; weeks = 4 | M; 7a copies, 9 time |

## F. Nested and crossed random factors

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| F01 | core | Two-level nesting | `lme4::Pastes` | nested ANOVA; variance components | nothing (sampled) | analysis of a cask sample | Cask a in batch A is not cask a in batch B | batches = 10; cask_labels = 3; casks = 30; analyses = 60; per_cask = 2 | T |
| F02 | core | Treatment, animals, preparations, readings | `GAD::rats` | mixed nested ANOVA | treatment to rat | reading | The rat is the replicate; its label repeats across treatments | treatments = 3; rat_labels = 2; rats = 6; preparations = 18; readings = 36 | T |
| F03 | core | Three-level passive sampling | `nlme::Oxide` | variance components | nothing (passive) | site on wafer | Lot, wafer, site | lots = 8; wafers = 24; sites = 72 | T |
| F04 | extended | Unequal (staggered) nesting | `daewr::polymer` | staggered nested variance components | nothing (sampled) | test | Branches are deliberately unequal | lots = 30; boxes = 60; preparations = 90; tests = 120; tests_per_box = 1-3 | T |
| F05 | extended | Crossed and nested together | `GAD::snails` | mixed ANOVA with nested and orthogonal random factors | snails to boulders and cages | snail | Origin crossed with shore; boulders inside shores; cages inside both | origins = 2; shores = 4; boulders = 12; cages = 48; snails = 240 | M; a tree cannot cross |
| F06 | core | Crossed random factors: gauge study | `daewr::gagerr` | gauge R and R | nothing | measurement | Every operator measures every part | parts = 10; operators = 3; measurements = 60; per_cell = 2 | M; 7a copies |
| F07 | extended | Crossed random factors, no replicate | `lme4::Penicillin` | crossed random effects | nothing | plate-sample | Every sample on every plate | plates = 24; samples = 6; rows = 144 | M; 7a copies |
| F08 | extended | Workers crossed with machines, replicated | `nlme::Machines` | mixed model with interaction | machine, within worker | run | Every worker uses every machine several times | workers = 6; machines = 3; runs = 54; per_cell = 3 | M; 7a copies |
| F09 | extended | Judges and targets | `AMCP::chapter_11_table_19` | intraclass correlation | nothing | rating | Every judge rates every target | targets = 6; judges = 4; ratings = 24 | M; 7a copies |
| F10 | core | Random therapists nested in an observed group | `AMCP::chapter_10_table_9` | nested mixed ANOVA | clients to trainee, random; gender observed | client rating | Trainees belong to one gender group; trainee numbers may repeat | genders = 2; trainee_labels = 3; trainees = 6; ratings = 24; per_trainee = 4 | T |
| F11 | extended | Fixed factor crossed with a random factor | `AMCP::chapter_10_table_5` | two-way mixed ANOVA | program (A) inside each school; schools (B) are random | student | Every school runs both programs; different students in each cell | students = 40; A = 2; B = 4; per_cell = 5 | T |

## G. Within subjects and crossover

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| G01 | core | One-way repeated measures | `AMCP::chapter_11_table_5` | repeated-measures ANOVA; sphericity; MANOVA approach | nothing (age) | child-age | 12 children at 4 ages, not 48 children | children = 12; ages = 4 | M; 9 time |
| G02 | core | Two-way within subjects | `AMCP::chapter_12_table_1` | two-way repeated-measures ANOVA | conditions within person | person-condition | Every person in all conditions | people = 10; conditions = 6 | M; 7a copies |
| G03 | core | One between, one within (mixed design) | `AMCP::chapter_14_table_9` | mixed-design ANOVA | nothing between (age group); angle within | person-angle | Groups hold different people; angles hold the same people | people = 20; groups = 2; per_group = 10; angles = 3 | M; 7a copies |
| G04 | extended | Two between, two within | `carData::OBrienKaiser` | mixed-design ANOVA; MANOVA | treatment between; phase and hour within | person-phase-hour | 16 people, 15 measurements each, unequal groups | people = 16; treatments = 3; per_group = 2-4; occasions = 15 | M; 7a copies, 9 time |
| G05 | core | Two-period crossover | `daewr::antifungal` | crossover analysis with sequence, period, carry-over | person to sequence | person-period | Sequences were assigned, everyone gets both drugs; order | people = 17; subject_labels = 17; groups = 2; people_per_group = 8-9; periods = 2; rows = 34 | M; 7a copies, 9 time |
| G06 | extended | Extra-period crossover | `daewr::bioequiv` | carry-over separated from treatment | person to sequence | person-period | Third period repeats the second treatment | people = 36; periods = 3; rows = 108; sequences = 2 | M; 9 time |
| G07 | extended | Williams square crossover | `daewr::chipman` | balanced carry-over | person to sequence | person-period | Squares, groups, periods | people = 12; subject_labels = 12; squares = 2; periods = 3; rows = 36 | M; 9 time |
| G08 | extended | Repeated Latin square for order | `AMCP::chapter_12_table_29` | Latin square repeated measures | order, within animal | animal-time | Order is balanced over squares | animals = 6; subject_labels = 6; squares = 2; times = 3; rows = 18 | M; 9 time |
| G09 | extended | Subjects and items crossed, many trials | `afex::fhch2010` | crossed random effects of subjects and items | task between; item type within | trial | Task differs between people; items are shared within a task | people = 45; items = 600; trials = 13222; people_per_task = 20-25; items_per_person = 274-300 | M; 7a copies |
| G10 | extended | Trials inside blocks inside people, between groups | `ez::ANT` | repeated-measures ANOVA on cell means; mixed model | group between; cue and flank within | trial | How many trials feed each cell mean | people = 20; groups = 2; blocks_per_person = 6; trials_per_person = 288; trials = 5760; cells = 12 | M; 7a copies |
| G11 | extended | Persons crossed with items, questionnaire | `lme4::VerbAgg` | item response models | nothing | answer | Everyone answers every item; items are a 2 by 2 by 3 set | people = 316; items = 24; answers = 7584; answers_per_person = 24 | M; 7a copies |
| G12 | extended | Two within factors, few animals | `WWGbook::rat.brain` | mixed model, two within factors | drug and region, within rat | rat-region-drug | 5 rats, 6 measurements each | rats = 5; rows = 30; per_rat = 6 | M; 7a copies |

## H. Multilevel and longitudinal

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| H01 | core | Two levels, observed: pupils in schools | `mlmRev::Hsb82` | random intercept and slope models | nothing | pupil | Schools sampled, pupils inside; sector belongs to the school | schools = 160; pupils = 7185; pupils_per_school = 14-67; sectors = 2 | T |
| H02 | core | Three levels: pupils, classes, schools | `WWGbook::classroom` | three-level model | nothing | pupil | Numbers at each level, and how uneven they are | schools = 107; classes = 312; pupils = 1190; classes_per_school = 1-9; pupils_per_class = 1-10 | T |
| H03 | core | Cluster-randomised: dose to dam, pups measured | `WWGbook::ratpup` | mixed model, heterogeneous variances | dose to dam, random | pup | Litter is the assigned unit; the help page says 30 dams | litters = 27; pups = 322; litters_per_dose = 7-10; pups_per_litter = 2-18 | T |
| H04 | core | Cluster-assigned: condition to room | `AMCP::chapter_16_table_4` | multilevel model against pooled ANOVA | condition to room | child | Rooms, not children, received the condition; room labels repeat. The help page says both that each classroom held both conditions and that they were separate: check the book | room_labels = 3; rooms = 6; children = 29; rooms_per_condition = 3; children_per_room = 3-6 | T |
| H05 | core | Randomised people followed over time | `AMCP::chapter_14_table_13` | longitudinal mixed model; GEE | person, random | person-time | Arms, then repeated visits of the same people | people = 100; people_per_arm = 44-56; times = 3; rows = 300 | I; 9 time |
| H06 | core | Growth of observed groups | `nlme::Orthodont` | random coefficient growth model | nothing | child-age | Children by sex, four ages each | children = 27; children_per_sex = 11-16; ages = 4; rows = 108 | I; 9 time |
| H07 | core | Follow-up with missed visits | `WWGbook::autism` | growth model with unbalanced data | nothing | child-age | Who was seen at each age; the help page says 214 children | children = 158; rows = 612; visits_per_child = 1-5; seen_at_each_age = 156/150/91/120/95 | I; 9 time |
| H08 | extended | Trial with visits in two clinics (repeated labels) | `geepack::respiratory` | GEE; GLMM, binary outcome | patient, random | patient-visit | Patient numbers repeat across clinics | clinics = 2; patient_labels = 56; patients = 111; rows = 444; visits = 4 | I; 9 time |
| H09 | extended | Trial with missed visits | `HSAUR3::toenail` | GLMM against GEE, dropout | patient, random | patient-visit | Patients at each visit; the help page says 378 were randomised | patients = 294; per_arm = 146-148; rows = 1908; seen_at_each_visit = 294/288/283/272/263/244/264 | I; 9 time |
| H10 | extended | Occasions in pupils in schools | `mlmRev::egsingle` | three-level growth model | nothing | pupil-year | Schools, pupils, occasions | schools = 60; pupils = 1721; rows = 7230; occasions_per_pupil = 2-6 | I; 9 time |
| H11 | extended | Clustered and longitudinal: teeth in patients | `WWGbook::veneer` | clustered longitudinal model | nothing | tooth-time | Teeth inside patients, each seen at each time | patients = 12; teeth = 55; rows = 110; times = 2; teeth_per_patient = 1-6 | I; 9 time |
| H12 | extended | Children in mothers in communities | `mlmRev::guImmun` | three-level logistic model | nothing (survey) | child | Mostly one child per mother | communities = 161; mothers = 1595; children = 2159; children_per_mother = 1-3 | T |
| H13 | edge | Cross-classified: primary by secondary school | `mlmRev::ScotsSec` | cross-classified model | nothing | pupil | Primary schools are not inside secondary schools | primary = 148; secondary = 19; pupils = 3435; pairs_seen = 303; secondaries_per_primary = 1-6 | M; 7f shared groups |
| H14 | edge | Partly crossed: students rate lecturers | `lme4::InstEval` | crossed random effects, large | nothing | rating | Each student rates some lecturers | students = 2972; lecturers = 1128; ratings = 73421; ratings_per_student = 1-92 | M; 7f |
| H15 | edge | Multiple membership: workers in several firms | `R2MLwiN::wage1` | multiple-membership model | nothing | worker | One worker counts in several firms | workers = 3022; first_firms = 141; workers_with_2nd = 526; with_3rd = 54; with_4th = 2 | M; 7f |
| H16 | edge | Two sets of individuals crossed in pairs | `lme4::salamander` | GLMM with crossed random effects | pairings by design | mating trial | Each female meets several males and the reverse | females = 60; males = 60; trials = 360; trials_per_female = 6 | M; 7f |
| H17 | extended | Multisite randomised experiment over four years | `mlmRev::star` | multilevel and cross-classified models | class type, within school | pupil-year | Pupils enter, leave and move; randomised inside schools | schools = 80; pupils = 11598; rows = 26796; years_per_pupil = 1-4; pupils_by_grade = 6325/6829/6840/6802 | I; 9 time |
| H18 | extended | Several sites, blocks at each site | `SASmixed::Multilocation` | multilocation mixed model | plot, within block | plot | Block numbers repeat across sites | sites = 9; block_labels = 3; blocks = 27; plots = 108; treatments = 4 | T |
| H19 | extended | Meta-analysis: trials as units | `HSAUR3::BCG` | fixed and random-effects meta-analysis | people, within trial | trial | Trials pooled; two arms in each | trials = 13; vaccinated = 191064; not_vaccinated = 166283 | T |
| H20 | extended | Groups followed, changing sizes | `lme4::cbpp` | binomial GLMM | nothing | herd-period | Herds, not animals, are the rows | herds = 15; rows = 56; periods = 4; periods_per_herd = 1-4 | I; 9 time |

## I. Observational, categorical, time to event

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| I01 | core | Cross-section, one sample classified two ways | `datasets::HairEyeColor` | chi-squared test; log-linear model | nothing | student | Only the total was fixed in advance | students = 592; hair = 4; eye = 4; sex = 2 | T |
| I02 | core | Case-control, unmatched | `datasets::esoph` | odds ratio; logistic regression | nothing; sampled by outcome | person | Cases and controls were sampled separately | cases = 200; controls = 775; rows_are_groups = 88 | T |
| I03 | core | Case-control, matched sets | `datasets::infert` | conditional logistic regression | nothing; matched | woman | Sets of one case and its controls; one set is short | women = 248; cases = 83; sets = 83; women_per_set = 2-3 | I; pairing |
| I04 | extended | Stratified two-way table | `datasets::UCBAdmissions` | Cochran-Mantel-Haenszel; Simpson's paradox | nothing | applicant | Departments as strata | applicants = 4526; departments = 6 | T |
| I05 | extended | Cohort, binary outcome | `MASS::birthwt` | logistic regression | nothing | birth | Observed exposure | births = 189; low = 59; smokers = 74 | T |
| I06 | core | Randomised, time to event, censored | `survival::veteran` | Kaplan-Meier; log-rank; Cox | patient, random | patient | Arms, events and censored per arm | patients = 137; per_arm = 68-69; events = 128 | T |
| I07 | extended | Two-phase (case-cohort) sampling | `survival::nwtco` | case-cohort Cox model | nothing; second phase sampled | patient | Subcohort plus all cases; they overlap | patients = 4028; subcohort = 668; relapsed = 571; both = 85 | R or M; 7f shared groups |
| I08 | extended | Interrupted time series | `datasets::Seatbelts` | segmented regression; intervention model | nothing; law changed | month | Months before and after the law | months = 192; before = 169; after = 23 | I; 9 time |
| I09 | extended | Before and after with a comparison group | `wooldridge::injury` | difference in differences | nothing; policy | claim | Two states, two earner groups, two periods | claims = 7150; states = 2; cells = 8; per_cell = 219-1705 | T |
| I10 | extended | Matched days, speed limit on or off | `MASS::Traffic` | Poisson GLM | speed limit to days, in runs; randomisation not stated | day | Days paired across two years | days = 184; pairs = 92; with_limit = 69 | I; pairing |

## J. Sample surveys

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| J01 | core | Simple random sample | `SDAResources::agsrs` | SRS estimation | nothing; sampled | county | Population and sample size | counties = 300 | T |
| J02 | core | Stratified random sample | `survey::apistrat` | stratified estimation | nothing; sampled within strata | school | Sampled separately in each stratum | schools = 200; strata = 3; per_stratum = 100/50/50 | T |
| J03 | core | One-stage cluster sample | `survey::apiclus1` | cluster estimation | nothing; districts sampled | school | Every school in a chosen district | districts = 15; schools = 183; per_district = 1-37 | T |
| J04 | core | Two-stage cluster sample | `survey::apiclus2` | two-stage estimation | nothing; two stages | school | Some schools in a chosen district | districts = 40; schools = 126; per_district = 1-5 | T |
| J05 | extended | Cluster sample with unequal sizes | `SDAResources::algebra` | ratio estimation for clusters | nothing; classes sampled | student | 12 of 187 classes | classes = 12; students = 299; per_class = 17-34 | T |
| J06 | extended | Two-stage, two units per cluster | `SDAResources::coots` | two-stage unequal probabilities | nothing; eggs sampled | egg | Two eggs from each clutch | clutches = 184; eggs = 368; per_clutch = 2 | T |

## K. Special field designs

| id | tier | design | data | taught with it | assigned to | one measurement is a | the picture must say | facts from the data | prediction |
|---|---|---|---|---|---|---|---|---|---|
| K01 | edge | Diallel cross | `agridat::hayman.tobacco` | diallel analysis | crosses by design | plot | Every line as male and as female | years = 2; males = 8; females = 8; plots = 256 | M; crossing |
| K02 | edge | Long-term rotation | `agridat::payne.wheat` | mixed model for rotations | plots, fixed for years | plot-year | The same plots for many years | years = 20; rotations = 6; nitrogen = 4; rows = 480 | I; 9 time |
| K03 | edge | Unreplicated lines with a repeated check | `agridat::burgueno.unreplicated` | spatial mixed model | plot | plot | One check repeated, new lines once | plots = 434; lines = 281; lines_seen_once = 280; check_plots = 154 | T |

