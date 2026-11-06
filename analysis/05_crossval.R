## =============================================================================
## 05_crossval.R - Cross-validation of assessor adjustments (BIOMED 2024, DEV 2024)
## Repeatedly split students into a calibration group and a held-out validation group.
## Each assessor's adjustment (BiasFix: assessor mean and SD and cohort mean and SD;
## mean shift; mixed-model BLUP from a crossed model) is estimated from the calibration
## group only and applied to the held-out students' marks. Agreement between stations and
## with the academic record (WAM; BIOMED 2024) is then compared for raw and adjusted marks
## in the held-out group. Output: out/T15_crossvalidation.csv, out/T15_crossval_splits.csv
## =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lme4) })
args <- commandArgs(trailingOnly = TRUE); base <- if (length(args)) args[1] else "."
A <- read.csv(file.path(base, "out", "adjusted_scores_deid.csv"), stringsAsFactors = FALSE)
set.seed(20241106)

metrics <- function(v, yvar) {                         # v: long held-out data with column yvar
  w <- v %>% select(unit_id, station, y = all_of(yvar)) %>% pivot_wider(names_from = station, values_from = y)
  X <- as.matrix(w[, -1]); k <- ncol(X)
  cr <- cor(X, use = "pairwise.complete.obs"); rr <- mean(cr[upper.tri(cr)])
  alpha <- k / (k - 1) * (1 - sum(apply(X, 2, var)) / var(rowSums(X)))
  fin <- w %>% mutate(final = rowMeans(X)) %>% select(unit_id, final) %>% left_join(v %>% distinct(unit_id, ext_measure), by = "unit_id")
  rw <- if (all(is.na(fin$ext_measure))) NA else cor(fin$final, fin$ext_measure, use = "complete.obs")
  c(r_station = rr, alpha = alpha, r_wam = rw)
}

one_split <- function(d, frac) {
  ids <- unique(d$unit_id); cal <- sample(ids, round(frac * length(ids)))
  dc <- d %>% filter(unit_id %in% cal); dv <- d %>% filter(!unit_id %in% cal)
  ## BiasFix and mean shift parameters from the calibration group, per station
  st <- dc %>% group_by(station) %>% summarise(M = mean(score_pct), S = sd(score_pct), .groups = "drop")
  ex <- dc %>% group_by(station, examiner_global) %>% summarise(m = mean(score_pct), s = sd(score_pct), n = n(), .groups = "drop")
  ## mixed model (crossed students x assessors, station fixed) on the calibration group
  fit <- lmer(score_pct ~ factor(station) + (1 | unit_id) + (1 | examiner_global), data = dc, REML = TRUE)
  bl <- ranef(fit)$examiner_global; bl <- data.frame(examiner_global = rownames(bl), blup = bl[, 1])
  dv <- dv %>% left_join(st, by = "station") %>% left_join(ex, by = c("station", "examiner_global")) %>% left_join(bl, by = "examiner_global") %>%
    mutate(ok = !is.na(m) & !is.na(s) & s > 0,
           bf = ifelse(ok, pmin(M + S * (score_pct - m) / s, 100), score_pct),
           ms = ifelse(!is.na(m), pmin(score_pct - m + M, 100), score_pct),
           lmm = ifelse(!is.na(blup), pmin(score_pct - blup, 100), score_pct))
  ## students whose station assessors were all represented in calibration
  keep <- dv %>% group_by(unit_id) %>% summarise(all_ok = all(ok)) %>% filter(all_ok) %>% pull(unit_id)
  dv <- dv %>% filter(unit_id %in% keep)
  out <- sapply(c("score_pct", "bf", "ms", "lmm"), function(y) metrics(dv, y))
  data.frame(metric = rownames(out), out, n_val = length(keep), min_cal_per_assessor = min(ex$n), med_cal_per_assessor = median(ex$n), row.names = NULL)
}

res <- list()
for (e in c("BIO24", "DEV24")) {
  d <- A %>% filter(exam_id == e) %>% select(unit_id, station, examiner_global, score_pct, ext_measure)
  for (frac in c(0.5, 0.8)) {
    R <- if (frac == 0.5) 1000 else 500
    sp <- bind_rows(lapply(seq_len(R), function(r) suppressMessages(one_split(d, frac)) %>% mutate(rep = r)))
    res[[paste(e, frac)]] <- sp %>% mutate(exam_id = e, cal_fraction = frac)
    cat(e, frac, "done\n")
  }
}
sp <- bind_rows(res)
write.csv(sp, file.path(base, "out", "T15_crossval_splits.csv"), row.names = FALSE)
summ <- sp %>% filter(!(metric == "r_wam" & exam_id == "DEV24")) %>%
  pivot_longer(c(bf, ms, lmm), names_to = "method", values_to = "adj") %>%
  mutate(gain = adj - score_pct) %>%
  group_by(exam_id, cal_fraction, metric, method) %>%
  summarise(raw = mean(score_pct), adjusted = mean(adj), gain_lo = quantile(gain, 0.025), gain_hi = quantile(gain, 0.975),
            pct_splits_gain = 100 * mean(gain > 0), gain = mean(gain), n_val = median(n_val), med_cal = median(med_cal_per_assessor), min_cal = min(min_cal_per_assessor), .groups = "drop")
write.csv(summ, file.path(base, "out", "T15_crossvalidation.csv"), row.names = FALSE)
print(as.data.frame(summ %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
