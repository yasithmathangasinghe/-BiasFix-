suppressPackageStartupMessages({library(lme4); library(dplyr)})
dat <- read.csv("data/viva_encounters_deid.csv")
keep <- dat %>% filter(exam_id=="DEV24") %>% count(unit_id) %>% filter(n==3) %>% pull(unit_id)
dat <- dat %>% filter(!(exam_id=="DEV24" & !unit_id %in% keep))
res <- list()
for (e in c("BIO24","DEV24")) {
  d <- dat %>% filter(exam_id==e)
  m <- lmer(score_pct ~ factor(station) + (1|unit_id) + (1|examiner_global), data=d)
  re <- ranef(m)$examiner_global
  d$aeff <- re[as.character(d$examiner_global),1]
  res[[e]] <- d %>% group_by(exam_id, unit_id) %>% summarise(comb_eff=mean(aeff), .groups="drop")
}
write.csv(bind_rows(res), "out/T12b_combination_effects_students.csv", row.names=FALSE)
