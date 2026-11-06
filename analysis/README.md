# Analysis code for the article

These scripts reproduce the analyses, tables and figures in the article and its Supporting
Information. They start from de-identified data files (`data/viva_encounters_deid.csv` and
`data/viva_items_deid.csv`), which are available from the corresponding authors on reasonable
request, subject to ethics approval. The script that created the de-identified files from the
original marking spreadsheets is not shared, because it contains identifying information.

Run from a folder that contains `data/` (outputs are written to `out/` and `fig/`):

| Order | Script | Purpose |
|---|---|---|
| 1 | `03_mfrm.py` | Many-facet Rasch measurement (marginal maximum likelihood); writes `out/mfrm_scores.csv` |
| 2 | `02_analysis.R` | Assessor variance, persistence, allocation, adjustments, independent evidence, consequences |
| 3 | `02b_blups.R` | Assessor-combination effects in multi-station vivas |
| 4 | `04_simulation.py` | Monte Carlo simulation (students per assessor) |
| 5 | `05_crossval.R` | Cross-validation of the adjustments in held-out students |
| 6 | `06_gtheory.R` | Generalisability study and decision study |
| 7 | `13_figures.R` | All figures (ggplot2) |

Example: `python3 03_mfrm.py . && Rscript 02_analysis.R . && Rscript 02b_blups.R . && python3 04_simulation.py . && Rscript 05_crossval.R . && Rscript 06_gtheory.R . && Rscript 13_figures.R .`

Software: R 4.3 (lme4, lmerTest, dplyr, tidyr, psych, ggplot2, patchwork, ragg, ggalluvial,
scales, png); Python 3.11 (NumPy, SciPy, pandas).
