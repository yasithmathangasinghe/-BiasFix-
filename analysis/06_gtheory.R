## =============================================================================
## 06_gtheory.R - Generalisability (G) study and decision (D) study for the multi-station vivas
## Design: students (p) crossed with stations (s); at each station each student is marked by one
## assessor drawn from that station's pool, so assessors are nested within stations (a:s).
## With one mark per student-station, the student-by-station interaction is confounded with the
## student-by-assessor interaction and error (ps,e).
##   y_psa = mu + p + s + a:s + ps,e
## Fitted with lme4 (REML) to raw and BiasFix-adjusted station marks; 95% CIs from parametric bootstrap.
## D-study: n_s stations, each student marked by n_a independent assessors per station:
##   relative error  sigma2_delta = sigma2_as/(n_s n_a) + sigma2_pse/n_s
##   absolute error  sigma2_Delta = sigma2_delta + sigma2_s/n_s
##   E rho^2 = sigma2_p/(sigma2_p + sigma2_delta); Phi = sigma2_p/(sigma2_p + sigma2_Delta)
## (n_a > 1 assumes ps,e is not assessor-specific, so it gives a lower bound on the benefit of extra assessors.)
## Outputs: out/T16_gstudy.csv, out/T16_dstudy.csv
## =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lme4) })
args <- commandArgs(trailingOnly = TRUE); base <- if (length(args)) args[1] else "."
A <- read.csv(file.path(base, "out", "adjusted_scores_deid.csv"), stringsAsFactors = FALSE)
set.seed(20241107)
vc <- function(fit) { v <- as.data.frame(VarCorr(fit)); setNames(v$vcov, v$grp) }
comp <- function(v) c(p = unname(v["unit_id"]), s = unname(v["station"]), as = unname(v["station:examiner_global"]), pse = unname(v["Residual"]))
G <- function(cm, ns, na = 1, rel = TRUE) { d <- cm["as"] / (ns * na) + cm["pse"] / ns; if (!rel) d <- d + cm["s"] / ns; unname(cm["p"] / (cm["p"] + d)) }
need <- function(cm, target, na = 1) { for (n in 1:30) if (G(cm, n, na) >= target) return(n); NA }

gs <- list(); ds <- list()
for (e in c("BIO24", "DEV24")) for (mk in c("raw", "BiasFix")) {
  d <- A %>% filter(exam_id == e) %>% mutate(y = if (mk == "raw") score_pct else bf)
  fit <- lmer(y ~ 1 + (1 | unit_id) + (1 | station) + (1 | station:examiner_global), data = d, REML = TRUE)
  cm <- comp(vc(fit))
  ns_obs <- length(unique(d$station))
  bt <- bootMer(fit, function(f) { c2 <- comp(vc(f)); c(c2, rel_obs = G(c2, ns_obs), abs_obs = G(c2, ns_obs, rel = FALSE),
                                                            n70 = need(c2, 0.7), n80 = need(c2, 0.8)) }, nsim = 500, seed = 1)
  ci <- apply(bt$t, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
  tot <- sum(cm)
  gs[[paste(e, mk)]] <- data.frame(exam_id = e, marks = mk, component = c("Student (p)", "Station (s)", "Assessor within station (a:s)", "Student x station, residual (ps,e)"),
                                   variance = cm, share = cm / tot, lo = ci[1, 1:4], hi = ci[2, 1:4], row.names = NULL)
  for (ns in 1:6) for (na in 1:2) ds[[paste(e, mk, ns, na)]] <- data.frame(exam_id = e, marks = mk, stations = ns, assessors_per_station = na,
       G_relative = G(cm, ns, na), Phi_absolute = G(cm, ns, na, rel = FALSE))
  ds[[paste(e, mk, "summary")]] <- data.frame(exam_id = e, marks = mk, stations = ns_obs, assessors_per_station = NA,
       G_relative = G(cm, ns_obs), Phi_absolute = G(cm, ns_obs, rel = FALSE),
       G_lo = ci[1, "rel_obs"], G_hi = ci[2, "rel_obs"], stations_for_0.70 = need(cm, 0.7), stations_for_0.80 = need(cm, 0.8),
       s70_lo = ci[1, "n70"], s70_hi = ci[2, "n70"], s80_lo = ci[1, "n80"], s80_hi = ci[2, "n80"],
       stations_for_0.80_two_assessors = need(cm, 0.8, 2))
  cat(e, mk, "done\n")
}
g <- bind_rows(gs); dd <- bind_rows(ds)
write.csv(g, file.path(base, "out", "T16_gstudy.csv"), row.names = FALSE)
write.csv(dd, file.path(base, "out", "T16_dstudy.csv"), row.names = FALSE)
print(g %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
print(dd %>% filter(is.na(assessors_per_station)) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
print(dd %>% filter(assessors_per_station == 1) %>% select(exam_id, marks, stations, G_relative) %>% pivot_wider(names_from = stations, values_from = G_relative) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
