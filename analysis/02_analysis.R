## =============================================================================
## 02_analysis.R  -  Assessor leniency in anatomy viva voce examinations
## Input : viva_encounters_deid.csv (built by 01_build_deidentified_dataset.py)
##         mfrm_scores.csv (optional; built by 03_mfrm.py)
## Output: out/*.csv  (tables used in the manuscript and supplement)
## R 4.3; packages: lme4, lmerTest, dplyr, tidyr, psych
## =============================================================================
suppressPackageStartupMessages({
  library(lme4); library(dplyr); library(tidyr); library(psych)
})
set.seed(2024)
args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args)) args[1] else "."
dat  <- read.csv(file.path(base, "data", "viva_encounters_deid.csv"), stringsAsFactors = FALSE)
outd <- file.path(base, "out"); dir.create(outd, showWarnings = FALSE)
w <- function(x, f) write.csv(x, file.path(outd, f), row.names = FALSE)

exam_order <- c("MED23-1","MED23-2","MED23-3","MED24-1","MED24-2","MED24-3","BIO23","BIO24","DEV24")
dat$exam_id <- factor(dat$exam_id, levels = exam_order)
## DEV24: keep students with all three stations for multi-station analyses
dev_complete <- dat %>% filter(exam_id == "DEV24") %>% count(unit_id) %>% filter(n == 3) %>% pull(unit_id)
dat <- dat %>% filter(!(exam_id == "DEV24" & !unit_id %in% dev_complete))
## "rating stream" = the set of scores that is standardised together
## (single-station exams: the exam; multi-station exams: each station separately)
dat$stream <- ifelse(dat$exam_id %in% c("BIO24","DEV24"), paste0(dat$exam_id, "-S", dat$station), as.character(dat$exam_id))

## ---------------------------------------------------------------- helpers ----
biasfix <- function(x, g, cap = 100) {          # z within assessor, re-expressed on cohort scale
  m <- mean(x); s <- sd(x)
  z <- ave(x, g, FUN = function(v) if (length(v) > 1 && sd(v) > 0) (v - mean(v)) / sd(v) else NA_real_)
  pmin(z * s + m, cap)
}
meanshift <- function(x, g) x - ave(x, g, FUN = mean) + mean(x)
eta2 <- function(y, g) { a <- anova(lm(y ~ factor(g))); a[1, 2] / sum(a[, 2]) }
omega2 <- function(y, g) { a <- anova(lm(y ~ factor(g))); (a[1,2] - a[1,1]*a[2,3]) / (sum(a[,2]) + a[2,3]) }
grade <- function(s) cut(s, c(-Inf, 50, 60, 70, 80, Inf), right = FALSE, labels = c("N","P","C","D","HD"))

## -------------------------------------------------- 1. descriptive table ----
desc <- dat %>% group_by(exam_id) %>% summarise(
  programme = first(programme), year = first(year), format = first(format),
  encounters = n(), candidates_units = n_distinct(unit_id),
  students = ifelse(first(format) == "team", sum(n_students), n_distinct(unit_id)),
  stations = n_distinct(station), assessors = n_distinct(examiner_global),
  enc_per_assessor_median = median(table(examiner_global)),
  enc_per_assessor_min = min(table(examiner_global)), enc_per_assessor_max = max(table(examiner_global)),
  mean = mean(score_pct), sd = sd(score_pct), median = median(score_pct),
  q1 = quantile(score_pct, .25), q3 = quantile(score_pct, .75), .groups = "drop")
## spread of assessor means
am <- dat %>% group_by(exam_id, examiner_global) %>% summarise(m = mean(score_pct), n = n(), .groups = "drop")
desc <- desc %>% left_join(am %>% group_by(exam_id) %>% summarise(
  assessor_mean_min = min(m), assessor_mean_max = max(m), assessor_mean_range = max(m) - min(m),
  assessor_mean_range_n5 = { mm <- m[n >= 5]; if (length(mm) > 1) max(mm) - min(mm) else NA }), by = "exam_id")
w(desc, "T1_descriptives.csv")

## final (student-level) scores for multi-station exams
fin <- dat %>% filter(exam_id %in% c("BIO24","DEV24")) %>% group_by(exam_id, unit_id) %>%
  summarise(final = mean(score_pct), .groups = "drop")
w(fin %>% group_by(exam_id) %>% summarise(n = n(), mean = mean(final), sd = sd(final)), "T1b_final_scores.csv")

## ------------------------------------- 2. assessor effect within each exam ----
eff <- lapply(split(dat, dat$stream), function(d) {
  d <- droplevels(d)
  kw <- kruskal.test(score_pct ~ factor(examiner_global), data = d)
  a  <- anova(lm(score_pct ~ factor(examiner_global), data = d))
  m  <- lmer(score_pct ~ 1 + (1 | examiner_global), data = d, REML = TRUE)
  vc <- as.data.frame(VarCorr(m))
  icc <- vc$vcov[1] / sum(vc$vcov)
  ## parametric bootstrap CI for the ICC
  bt <- tryCatch(bootMer(m, function(x) { v <- as.data.frame(VarCorr(x))$vcov; v[1] / sum(v) },
                         nsim = 500, seed = 1)$t, error = function(e) NA)
  re <- ranef(m)$examiner_global[, 1]
  data.frame(stream = d$stream[1], exam_id = d$exam_id[1], n = nrow(d), assessors = n_distinct(d$examiner_global),
             F = a[1, 4], df1 = a[1, 1], df2 = a[2, 1], p_anova = a[1, 5],
             eta2 = eta2(d$score_pct, d$examiner_global), omega2 = omega2(d$score_pct, d$examiner_global),
             KW_chi2 = unname(kw$statistic), p_KW = kw$p.value,
             epsilon2_KW = unname(kw$statistic) / ((nrow(d)^2 - 1) / (nrow(d) + 1)),
             var_assessor = vc$vcov[1], var_resid = vc$vcov[2], ICC_assessor = icc,
             ICC_lo = quantile(bt, .025, na.rm = TRUE), ICC_hi = quantile(bt, .975, na.rm = TRUE),
             sd_assessor_effect = sqrt(vc$vcov[1]),
             lottery_min = fixef(m)[1] + min(re), lottery_max = fixef(m)[1] + max(re),
             lottery_p10 = fixef(m)[1] + quantile(re, .1), lottery_p90 = fixef(m)[1] + quantile(re, .9))
})
eff <- bind_rows(eff); rownames(eff) <- NULL
w(eff, "T2_assessor_effects_by_exam.csv")

## crossed designs: student x assessor variance components
vc_crossed <- lapply(c("BIO24","DEV24"), function(e) {
  d <- dat %>% filter(exam_id == e)
  m <- lmer(score_pct ~ factor(station) + (1 | unit_id) + (1 | examiner_global), data = d, REML = TRUE)
  v <- as.data.frame(VarCorr(m))
  tot <- sum(v$vcov)
  bt <- bootMer(m, function(x) { vv <- as.data.frame(VarCorr(x)); vv$vcov / sum(vv$vcov) }, nsim = 500, seed = 2)$t
  data.frame(exam_id = e, component = c(v$grp), variance = v$vcov, proportion = v$vcov / tot,
             lo = apply(bt, 2, quantile, .025), hi = apply(bt, 2, quantile, .975))
})
vc_crossed <- bind_rows(vc_crossed)
w(vc_crossed, "T3_variance_components_crossed.csv")

## ------------------- 3. pooled 2023 analysis (common assessors across 4 exams) ----
p23 <- dat %>% filter(common_examiner_2023 == 1, exam_id %in% c("MED23-1","MED23-2","MED23-3","BIO23"))
m_full <- lmer(score_pct ~ (1 | examiner_global) + (1 | exam_id) + (1 | examiner_global:exam_id), data = p23, REML = TRUE)
v23 <- as.data.frame(VarCorr(m_full))
bt23 <- bootMer(m_full, function(x) { vv <- as.data.frame(VarCorr(x)); vv$vcov / sum(vv$vcov) }, nsim = 500, seed = 3)$t
pool23 <- data.frame(component = v23$grp, variance = v23$vcov, proportion = v23$vcov / sum(v23$vcov),
                     lo = apply(bt23, 2, quantile, .025), hi = apply(bt23, 2, quantile, .975), n = nrow(p23),
                     assessors = n_distinct(p23$examiner_global))
## likelihood-ratio tests (ML)
m0  <- lmer(score_pct ~ (1 | exam_id), data = p23, REML = FALSE)
m1  <- lmer(score_pct ~ (1 | exam_id) + (1 | examiner_global), data = p23, REML = FALSE)
m2  <- lmer(score_pct ~ (1 | examiner_global), data = p23, REML = FALSE)
m00 <- lm(score_pct ~ 1, data = p23)
lrt_assessor <- anova(m1, m0); lrt_exam <- anova(m1, m2)
## fixed-effect version (as in earlier drafts): assessor as fixed, exam random
mfix <- lmer(score_pct ~ factor(examiner_global) + (1 | exam_id), data = p23, REML = TRUE)
pool23$LRT_assessor_chi2 <- lrt_assessor$Chisq[2]; pool23$LRT_assessor_p <- lrt_assessor$`Pr(>Chisq)`[2]
pool23$LRT_exam_chi2 <- lrt_exam$Chisq[2]; pool23$LRT_exam_p <- lrt_exam$`Pr(>Chisq)`[2]
w(pool23, "T4_pooled2023_variance.csv")
fx <- summary(mfix)$coefficients
w(data.frame(term = rownames(fx), fx, row.names = NULL), "T4b_pooled2023_fixed_assessor.csv")

## -------------------------------- 4. stability of leniency across exams ----
dev_tab <- dat %>% group_by(stream) %>% mutate(es = mean(score_pct), esd = sd(score_pct)) %>%
  group_by(stream, exam_id, examiner_global) %>%
  summarise(n = n(), dev = mean(score_pct) - first(es), dev_z = (mean(score_pct) - first(es)) / first(esd), .groups = "drop")
w(dev_tab, "T5a_assessor_deviation_by_stream.csv")
## assessors with >= 3 encounters in >= 2 different exams (collapse stations within exam)
dev_exam <- dat %>% group_by(exam_id) %>% mutate(es = mean(score_pct), esd = sd(score_pct)) %>%
  group_by(exam_id, examiner_global) %>%
  summarise(n = n(), dev_z = (mean(score_pct) - first(es)) / first(esd), dev = mean(score_pct) - first(es), .groups = "drop") %>%
  filter(n >= 3)
multi <- dev_exam %>% group_by(examiner_global) %>% filter(n_distinct(exam_id) >= 2) %>% ungroup()
ms <- lmer(dev_z ~ 1 + (1 | examiner_global), data = multi)
vs <- as.data.frame(VarCorr(ms))
bts <- bootMer(ms, function(x) { vv <- as.data.frame(VarCorr(x))$vcov; vv[1] / sum(vv) }, nsim = 1000, seed = 4)$t
## within MED23 (same 9 assessors, 3 exams): pairwise correlations of assessor deviations
med23 <- dev_exam %>% filter(exam_id %in% c("MED23-1","MED23-2","MED23-3")) %>% select(exam_id, examiner_global, dev) %>%
  pivot_wider(names_from = exam_id, values_from = dev)
cm <- cor(med23[, -1], use = "pairwise.complete.obs")
## split-half reliability of assessor means within large exams (odd/even candidates)
split_half <- dat %>% filter(exam_id %in% c("BIO23","BIO24","DEV24")) %>% group_by(stream, examiner_global) %>%
  mutate(half = rep(1:2, length.out = n())) %>% group_by(stream, examiner_global, half) %>%
  summarise(m = mean(score_pct), .groups = "drop") %>% group_by(stream) %>% mutate(m = m - mean(m)) %>%
  pivot_wider(names_from = half, values_from = m, names_prefix = "h")
sh_r <- cor(split_half$h1, split_half$h2)
stab <- data.frame(n_assessor_exam_pairs = nrow(multi), n_assessors = n_distinct(multi$examiner_global),
                   ICC_stability = vs$vcov[1] / sum(vs$vcov), ICC_lo = quantile(bts, .025), ICC_hi = quantile(bts, .975),
                   r_MED23_1v2 = cm[1, 2], r_MED23_1v3 = cm[1, 3], r_MED23_2v3 = cm[2, 3],
                   split_half_r = sh_r, split_half_SB = 2 * sh_r / (1 + sh_r), split_half_n = nrow(split_half))
w(stab, "T5_leniency_stability.csv"); w(multi, "T5b_multi_exam_assessors.csv")

## ------------------------------------------------ 5. adjusted scores ----
dat <- dat %>% group_by(stream) %>% mutate(
  bf = biasfix(score_pct, examiner_global),
  ms = meanshift(score_pct, examiner_global)) %>% ungroup()
## model-based (shrunken) adjustment: score minus assessor BLUP
dat$lmm <- NA_real_
for (s in unique(dat$stream)) {
  i <- dat$stream == s; d <- dat[i, ]
  m <- if (d$exam_id[1] %in% c("BIO24","DEV24")) {
    ## crossed model fitted on the whole exam, BLUPs for assessor (station-specific intercepts not needed)
    lmer(score_pct ~ factor(station) + (1 | unit_id) + (1 | examiner_global), data = dat[dat$exam_id == d$exam_id[1], ])
  } else lmer(score_pct ~ 1 + (1 | examiner_global), data = d)
  re <- ranef(m)$examiner_global
  dat$lmm[i] <- d$score_pct - re[as.character(d$examiner_global), 1]
}
## optional MFRM fair scores
mf <- file.path(base, "out", "mfrm_scores.csv")
if (file.exists(mf)) {
  mfr <- read.csv(mf, stringsAsFactors = FALSE)
  dat <- dat %>% left_join(mfr %>% select(exam_id, unit_id, station, mfrm), by = c("exam_id", "unit_id", "station"))
  dat$mfrm[dat$exam_id == "BIO23"] <- NA_real_   # single-assessor (disconnected) design: MFRM not identified without the random-groups assumption; reported in supplement only
} else dat$mfrm <- NA_real_
w(dat %>% select(exam_id, stream, unit_id, station, examiner_local, examiner_global, score_pct, bf, ms, lmm, mfrm, adhoc_score_pct, ext_measure),
  "adjusted_scores_deid.csv")

## residual assessor effect after each adjustment (descriptive; BiasFix and mean-shift remove it by construction)
resid_eff <- dat %>% group_by(stream) %>% summarise(
  eta2_raw = eta2(score_pct, examiner_global), eta2_bf = eta2(bf, examiner_global),
  eta2_ms = eta2(ms, examiner_global), eta2_lmm = eta2(lmm, examiner_global),
  eta2_mfrm = if (all(is.na(mfrm))) NA else eta2(mfrm, examiner_global),
  eta2_adhoc = if (all(is.na(adhoc_score_pct))) NA else eta2(adhoc_score_pct, examiner_global),
  sd_assessor_means_raw = sd(tapply(score_pct, examiner_global, mean)),
  sd_assessor_means_adhoc = if (all(is.na(adhoc_score_pct))) NA else sd(tapply(adhoc_score_pct, examiner_global, mean)),
  sd_assessor_sds_raw = sd(tapply(score_pct, examiner_global, sd), na.rm = TRUE), .groups = "drop")
w(resid_eff, "T6_residual_assessor_effect.csv")

## ---------------------------------- 6. non-circular validity evidence ----
boot_diff <- function(df, f, B = 2000) {      # paired bootstrap over candidates
  n <- nrow(df); est <- f(df)
  bs <- replicate(B, f(df[sample.int(n, n, TRUE), , drop = FALSE]))
  c(est = est, lo = unname(quantile(bs, .025)), hi = unname(quantile(bs, .975)))
}
methods <- c(raw = "score_pct", meanshift = "ms", biasfix = "bf", lmm = "lmm", mfrm = "mfrm")
## 6a. inter-station agreement (BIO24: r between the two stations; DEV24: alpha + mean inter-station r)
agree <- list()
for (e in c("BIO24","DEV24")) {
  for (mname in names(methods)) {
    v <- methods[[mname]]
    wd <- dat %>% filter(exam_id == e) %>% select(unit_id, station, val = all_of(v)) %>%
      pivot_wider(names_from = station, values_from = val, names_prefix = "s") %>% na.omit()
    if (nrow(wd) < 10) next
    S <- as.matrix(wd[, -1])
    rbar <- function(M) { cm <- cor(M); mean(cm[upper.tri(cm)]) }
    alph <- function(M) { k <- ncol(M); k / (k - 1) * (1 - sum(apply(M, 2, var)) / var(rowSums(M))) }
    agree[[length(agree) + 1]] <- data.frame(exam_id = e, method = mname, n = nrow(S),
                                             mean_interstation_r = rbar(S), alpha = alph(S))
  }
}
agree <- bind_rows(agree)
## bootstrap CIs for the gain (biasfix - raw) etc.
gain <- list()
for (e in c("BIO24","DEV24")) {
  wd <- dat %>% filter(exam_id == e) %>% select(unit_id, station, score_pct, bf, ms, lmm, mfrm) %>%
    pivot_wider(names_from = station, values_from = c(score_pct, bf, ms, lmm, mfrm))
  for (mname in c("meanshift","biasfix","lmm","mfrm")) {
    v <- methods[[mname]]
    cols_m <- grep(paste0("^", v, "_"), names(wd), value = TRUE)
    cols_r <- grep("^score_pct_", names(wd), value = TRUE)
    dd <- wd[, c(cols_r, cols_m)] %>% na.omit()
    if (nrow(dd) < 10) next
    f <- function(x) { a <- cor(as.matrix(x[, cols_m])); b <- cor(as.matrix(x[, cols_r]))
                       mean(a[upper.tri(a)]) - mean(b[upper.tri(b)]) }
    bd <- boot_diff(as.data.frame(dd), f, B = 2000)
    gain[[length(gain) + 1]] <- data.frame(exam_id = e, method = mname, delta_r = bd[1], lo = bd[2], hi = bd[3])
  }
}
gain <- bind_rows(gain)
w(agree, "T7a_interstation_agreement.csv"); w(gain, "T7b_interstation_gain_bootstrap.csv")

## 6b. criterion relationships with an independent measure
crit <- list()
## BIO24: final viva (mean of stations) vs WAM
b24 <- dat %>% filter(exam_id == "BIO24") %>% group_by(unit_id) %>%
  summarise(across(c(score_pct, ms, bf, lmm, mfrm), mean), wam = first(ext_measure), .groups = "drop") %>% filter(!is.na(wam))
for (mname in names(methods)) {
  v <- methods[[mname]]; if (all(is.na(b24[[v]]))) next
  f <- function(x) cor(x[[v]], x$wam, use = "complete.obs") - cor(x$score_pct, x$wam, use = "complete.obs")
  bd <- boot_diff(as.data.frame(b24), f)
  crit[[length(crit) + 1]] <- data.frame(exam_id = "BIO24", measure = "WAM", method = mname, n = sum(!is.na(b24[[v]])),
                                         r = cor(b24[[v]], b24$wam, use = "complete.obs"),
                                         delta_vs_raw = bd[1], lo = bd[2], hi = bd[3])
}
## MED23: team viva vs team-mean written anatomy exam (pooled across the three vivas, within-exam centred)
m23 <- dat %>% filter(exam_id %in% c("MED23-1","MED23-2","MED23-3"), !is.na(ext_measure)) %>%
  group_by(exam_id) %>% mutate(across(c(score_pct, ms, bf, lmm, ext_measure), ~ . - mean(., na.rm = TRUE))) %>% ungroup()
for (mname in c("raw","meanshift","biasfix","lmm")) {
  v <- methods[[mname]]
  f <- function(x) cor(x[[v]], x$ext_measure, use = "complete.obs") - cor(x$score_pct, x$ext_measure, use = "complete.obs")
  bd <- boot_diff(as.data.frame(m23), f)
  crit[[length(crit) + 1]] <- data.frame(exam_id = "MED23 (pooled)", measure = "Written anatomy exam (team mean)",
                                         method = mname, n = nrow(m23), r = cor(m23[[v]], m23$ext_measure, use = "complete.obs"),
                                         delta_vs_raw = bd[1], lo = bd[2], hi = bd[3])
}
crit <- bind_rows(crit); w(crit, "T8_criterion_relationships.csv")

## 6c. were candidates allocated to assessors comparably? (independent measure by assessor)
bal <- list()
for (s in c("BIO24-S1","BIO24-S2")) {
  d <- dat %>% filter(stream == s, !is.na(ext_measure))
  a <- anova(lm(ext_measure ~ factor(examiner_global), data = d))
  bal[[length(bal) + 1]] <- data.frame(stream = s, measure = "WAM", n = nrow(d),
    eta2_measure = eta2(d$ext_measure, d$examiner_global), p_measure = a[1, 5],
    eta2_viva = eta2(d$score_pct, d$examiner_global), p_viva = anova(lm(score_pct ~ factor(examiner_global), data = d))[1, 5])
}
for (s in c("MED23-1","MED23-2","MED23-3")) {
  d <- dat %>% filter(stream == s, !is.na(ext_measure))
  bal[[length(bal) + 1]] <- data.frame(stream = s, measure = "Written anatomy exam (team mean)", n = nrow(d),
    eta2_measure = eta2(d$ext_measure, d$examiner_global), p_measure = anova(lm(ext_measure ~ factor(examiner_global), data = d))[1, 5],
    eta2_viva = eta2(d$score_pct, d$examiner_global), p_viva = anova(lm(score_pct ~ factor(examiner_global), data = d))[1, 5])
}
bal <- bind_rows(bal); w(bal, "T9_allocation_balance.csv")

## ------------------------------------------ 7. consequences for students ----
cons_src <- bind_rows(
  dat %>% filter(exam_id == "BIO23") %>% transmute(exam_id, unit_id, raw = score_pct, bf, ms, lmm, mfrm, adhoc = adhoc_score_pct),
  dat %>% filter(exam_id %in% c("BIO24","DEV24")) %>% group_by(exam_id, unit_id) %>%
    summarise(raw = mean(score_pct), bf = mean(bf), ms = mean(ms), lmm = mean(lmm), mfrm = mean(mfrm), adhoc = NA_real_, .groups = "drop"))
cons <- list(); flows <- list()
for (e in c("BIO23","BIO24","DEV24")) for (mname in c("bf","ms","lmm","mfrm","adhoc")) {
  d <- cons_src %>% filter(exam_id == e); if (all(is.na(d[[mname]]))) next
  d <- d[!is.na(d[[mname]]), ]
  g0 <- grade(d$raw); g1 <- grade(d[[mname]]); dl <- d[[mname]] - d$raw
  cons[[length(cons) + 1]] <- data.frame(exam_id = e, method = mname, n = nrow(d),
    pct_band_change = mean(g0 != g1) * 100, pct_up = mean(as.integer(g1) > as.integer(g0)) * 100,
    pct_down = mean(as.integer(g1) < as.integer(g0)) * 100,
    pass_to_fail = sum(d$raw >= 50 & d[[mname]] < 50), fail_to_pass = sum(d$raw < 50 & d[[mname]] >= 50),
    fails_raw = sum(d$raw < 50), fails_adj = sum(d[[mname]] < 50),
    median_abs_change = median(abs(dl)), p90_abs_change = quantile(abs(dl), .9), max_abs_change = max(abs(dl)),
    pct_change_ge5 = mean(abs(dl) >= 5) * 100, pct_change_ge10 = mean(abs(dl) >= 10) * 100,
    spearman_raw_adj = cor(d$raw, d[[mname]], method = "spearman"))
  if (mname == "bf") flows[[e]] <- data.frame(exam_id = e, as.data.frame(table(raw = g0, adjusted = g1)))
}
cons <- bind_rows(cons); w(cons, "T10_consequences.csv"); w(bind_rows(flows), "T10b_grade_flows_biasfix.csv")
## pooled summative
pool <- cons_src %>% filter(!is.na(bf))
w(data.frame(n = nrow(pool), pct_band_change = mean(grade(pool$raw) != grade(pool$bf)) * 100,
             pct_ge5 = mean(abs(pool$bf - pool$raw) >= 5) * 100, median_abs = median(abs(pool$bf - pool$raw)),
             pass_fail_changes = sum((pool$raw >= 50) != (pool$bf >= 50))), "T10c_pooled_summative.csv")
## agreement among adjustment methods (student level)
agm <- cons_src %>% group_by(exam_id) %>% summarise(
  r_bf_ms = cor(bf, ms), r_bf_lmm = cor(bf, lmm), r_bf_mfrm = if (all(is.na(mfrm))) NA else cor(bf, mfrm, use = "complete.obs"),
  r_lmm_mfrm = if (all(is.na(mfrm))) NA else cor(lmm, mfrm, use = "complete.obs"),
  rho_bf_mfrm = if (all(is.na(mfrm))) NA else cor(bf, mfrm, method = "spearman", use = "complete.obs"),
  r_bf_adhoc = if (all(is.na(adhoc))) NA else cor(bf, adhoc), .groups = "drop")
w(agm, "T11_method_agreement.csv")

## ---------------------------- 8. averaging over stations: residual lottery ----
## contribution of the particular assessor combination to a candidate's final mark
avg <- lapply(c("BIO24","DEV24"), function(e) {
  d <- dat %>% filter(exam_id == e)
  m <- lmer(score_pct ~ factor(station) + (1 | unit_id) + (1 | examiner_global), data = d)
  re <- ranef(m)$examiner_global
  d$aeff <- re[as.character(d$examiner_global), 1]
  comb <- d %>% group_by(unit_id) %>% summarise(comb_eff = mean(aeff), .groups = "drop")
  k <- n_distinct(d$station); s2 <- as.data.frame(VarCorr(m))
  data.frame(exam_id = e, stations = k, sd_single_assessor_effect = sqrt(s2$vcov[s2$grp == "examiner_global"]),
             sd_combination_effect_observed = sd(comb$comb_eff),
             sd_combination_expected = sqrt(s2$vcov[s2$grp == "examiner_global"] / k),
             range_combination = diff(range(comb$comb_eff)),
             p10 = quantile(comb$comb_eff, .1), p90 = quantile(comb$comb_eff, .9),
             pct_candidates_combo_ge3 = mean(abs(comb$comb_eff) >= 3) * 100)
})
w(bind_rows(avg), "T12_combination_effects.csv")

## ----------------------------------------- 9. assessor preparation ----
trn <- dat %>% filter(exam_id %in% c("BIO24","DEV24"), !is.na(training)) %>% group_by(exam_id) %>%
  mutate(es = mean(score_pct), esd = sd(score_pct)) %>% group_by(exam_id, examiner_global, training) %>%
  summarise(n = n(), abs_dev_z = abs(mean(score_pct) - first(es)) / first(esd), sd_ratio = sd(score_pct) / first(esd), .groups = "drop")
trn_test <- trn %>% group_by(exam_id) %>% summarise(
  n_assessors = n(), levels = paste(sort(unique(training)), collapse = "/"),
  absdev_lo_train = median(abs_dev_z[training == min(training)]), absdev_hi_train = median(abs_dev_z[training == max(training)]),
  p_absdev = wilcox.test(abs_dev_z ~ training, exact = FALSE)$p.value,
  sdratio_lo_train = median(sd_ratio[training == min(training)]), sdratio_hi_train = median(sd_ratio[training == max(training)]),
  p_sdratio = wilcox.test(sd_ratio ~ training, exact = FALSE)$p.value, .groups = "drop")
w(trn, "T13a_training_assessor_level.csv"); w(trn_test, "T13_training.csv")

## ------------------------------------------ 10. candidate characteristics ----
b24d <- dat %>% filter(exam_id == "BIO24", gender %in% c("M","F")) %>% mutate(wamz = as.numeric(scale(ext_measure)))
fair <- lapply(c("score_pct","bf"), function(v) {
  m <- lmerTest::lmer(as.formula(paste(v, "~ gender + international + wamz + factor(station) + (1|unit_id) + (1|examiner_global)")), data = b24d)
  cf <- summary(m)$coefficients
  data.frame(outcome = v, term = rownames(cf), cf, row.names = NULL)
})
w(bind_rows(fair), "T14_candidate_characteristics_BIO24.csv")

## ---------------------------------------------------------- 11. log ----
sink(file.path(outd, "analysis_log.txt"))
cat("Encounters analysed:", nrow(dat), "\n"); print(desc); print(eff); print(vc_crossed); print(pool23)
print(stab); print(cm); print(resid_eff); print(agree); print(gain); print(crit); print(bal); print(cons); print(agm)
print(bind_rows(avg)); print(trn_test); print(bind_rows(fair)); print(sessionInfo())
sink()
cat("done\n")
