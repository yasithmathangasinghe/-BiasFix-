# BiasFix

**Adjusting assessment marks for assessor leniency, without specialist statistics.**

In vivas, OSCEs and other assessments marked by people, some assessors mark more generously
than others ("doves" and "hawks"). BiasFix places every assessor's marks on the scale of the
whole cohort so that a student's mark no longer depends on which assessor they happened to meet.
It needs only a spreadsheet with three columns: `Student_ID`, `Examiner` and `Score`.

## How it works

For each assessor, every mark is expressed relative to that assessor's own marks (a z-score) and
then converted back to a mark on the cohort's scale:

```
z        = (mark - assessor's mean) / assessor's SD
adjusted = cohort mean + z * cohort SD          (capped at the maximum mark)
```

Students keep their order among the students examined by the same assessor, while every
assessor's marks share the cohort's mean and spread. This is mean-sigma linear equating. It
assumes that each assessor examined a broadly comparable group of students, which is reasonable
when students are allocated to assessors without regard to ability (for example, by timetable).

## Recommended use

* Allocate students to assessors without regard to ability.
* Aim for **about 30 students per assessor** at each station; 15 can suffice when assessors differ
  substantially. Avoid fewer than about 10.
* Adjust **each station separately**, then combine the adjusted station marks.
* Keep the raw marks, and review students whose grade or pass/fail status changes.
* Tell students in advance that marks may be standardised across assessors.

## Repository contents

| Folder | Contents |
|---|---|
| `app/` | The BiasFix Shiny web application (`app.R`, version 1.0 beta) and its `www/` assets (templates, sample data, instructions, logo) |
| `R/biasfix.R` | Stand-alone R functions: `biasfix()` (the adjustment) and `assessor_summary()` (assessor means, SDs and the share of variation due to assessors) |
| `examples/` | `example.R` and `sample_dataset.csv` |
| `tests/` | `test_biasfix.R`: checks of the adjustment's properties |
| `analysis/` | Analysis code for the article (see `analysis/README.md`) |

## Run the app on your own computer

1. Install R (https://cran.r-project.org) and, optionally, RStudio.
2. Install the packages once:
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "psych", "lme4", "readxl"))
   ```
3. Run:
   ```r
   shiny::runApp("app")
   ```
   from the repository folder (or open `app/app.R` in RStudio and click **Run App**).

## Use the R function

```r
source("R/biasfix.R")
d   <- read.csv("examples/sample_dataset.csv")
assessor_summary(d)                    # how much do assessors differ?
adj <- biasfix(d, max_score = 100)     # Retransformed_Score = adjusted mark
```

## Citation

Please cite the article above and the archived software (see `CITATION.cff`):

> Mathangasinghe Y, et al. BiasFix: adjusting assessment marks for assessor leniency (version 1.0.0)
> [software]. Zenodo. https://doi.org/10.5281/zenodo.24003229

## Licence

MIT (see `LICENSE`). Contact: yasith.mathangasinghe1@monash.edu
