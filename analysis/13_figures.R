## =============================================================================
## 13_figures.R - all manuscript and supplementary figures (ggplot2)
## Inputs: data/viva_encounters_deid.csv, out/*.csv ; Output: fig/*.png|tiff|pdf
## =============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(ragg); library(scales); library(ggalluvial)
})
args <- commandArgs(trailingOnly = TRUE); base <- if (length(args)) args[1] else "."
O <- file.path(base, "out"); FG <- file.path(base, "fig"); dir.create(FG, showWarnings = FALSE)
FAM <- "Liberation Sans"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; AQUA <- "#1baf7a"; YELLOW <- "#eda100"; GREY <- "#898781"; INK <- "#0b0b0b"; INK2 <- "#52514e"
PROG <- c(MED = BLUE, BIOMED = ORANGE, DEV = AQUA)
th <- theme_classic(base_size = 8, base_family = FAM) +
  theme(axis.line = element_line(colour = "#c3c2b7", linewidth = 0.35), axis.ticks = element_line(colour = "#c3c2b7", linewidth = 0.35),
        axis.text = element_text(colour = INK2, size = 7), axis.title = element_text(colour = INK, size = 7.5),
        plot.tag = element_text(face = "bold", size = 11), plot.title = element_text(size = 8, face = "bold", colour = INK),
        legend.title = element_blank(), legend.text = element_text(size = 7), legend.key.size = unit(3, "mm"),
        legend.background = element_blank(), strip.background = element_blank(), strip.text = element_text(size = 7.5, face = "bold"),
        plot.margin = margin(4, 6, 4, 4))
save <- function(p, name, w, h) {
  agg_png(file.path(FG, paste0(name, ".png")), width = w, height = h, units = "in", res = 300, background = "white"); print(p); invisible(dev.off())
  agg_tiff(file.path(FG, paste0(name, ".tiff")), width = w, height = h, units = "in", res = 600, background = "white", compression = "lzw"); print(p); invisible(dev.off())
}
E   <- read.csv(file.path(base, "data", "viva_encounters_deid.csv"), stringsAsFactors = FALSE)
A   <- read.csv(file.path(O, "adjusted_scores_deid.csv"), stringsAsFactors = FALSE)
A   <- A %>% left_join(E %>% select(exam_id, unit_id, station, common_examiner_2023, n_students), by = c("exam_id", "unit_id", "station"))
lab <- c("MED23-1" = "MED viva 1", "MED23-2" = "MED viva 2", "MED23-3" = "MED viva 3", "BIO23" = "BIOMED",
         "MED24-1" = "MED viva 1", "MED24-2" = "MED viva 2", "MED24-3" = "MED viva 3", "BIO24" = "BIOMED")
VIVACOL <- c("MED viva 1" = BLUE, "MED viva 2" = ORANGE, "MED viva 3" = AQUA, "BIOMED" = YELLOW)
code_by_mean <- function(d) { o <- d %>% group_by(examiner_global) %>% summarise(m = mean(score_pct)) %>% arrange(desc(m))
  setNames(paste0("A", seq_len(nrow(o))), o$examiner_global) }

## ---------------------------------------------------------------- Figure 1 (design)
fig1 <- function() {
  cols <- data.frame(prog = c("MED", "BIOMED", "DEV"), x0 = c(23, 49, 75), x1 = c(47, 73, 99),
                     name = c("Medicine (MED)", "Biomedical Science (BIOMED)", "Science (DEV)"))
  rows <- data.frame(lab = c("Students", "Assessor\ntraining", "Structure", "Format", ""), y = c(51, 40, 30, 19, 6))
  person <- function(x, y, s = 1, col = INK2) list(
    annotate("point", x = x, y = y + 1.55 * s, size = 2.2 * s, colour = col),
    annotate("rect", xmin = x - 0.75 * s, xmax = x + 0.75 * s, ymin = y - 0.9 * s, ymax = y + 0.75 * s, fill = col, colour = NA))
  arrow_ <- function(x0, x1, y) annotate("segment", x = x0, xend = x1, y = y, yend = y, colour = GREY, linewidth = 0.35,
                                         arrow = arrow(length = unit(1.2, "mm"), type = "closed"))
  txt <- function(x, y, s, size = 2.45, hjust = 0.5, col = INK, fontface = "plain") annotate("text", x = x, y = y, label = s, size = size, hjust = hjust,
                                                                                            colour = col, family = FAM, fontface = fontface, lineheight = 0.95)
  bars <- data.frame(prog = rep(c("MED", "BIOMED", "DEV"), each = 3), k = rep(1:3, 3), lev = c(2.5, 1, 1, 3, 2, 2, 1, 3, 3))
  bars <- bars %>% left_join(cols, by = "prog") %>% mutate(y = 9.4 - (k - 1) * 3.0, xs = x0 + 7, xe = xs + lev * 5.2)
  p <- ggplot() + coord_cartesian(xlim = c(0, 100), ylim = c(0, 62), expand = FALSE) + theme_void(base_family = FAM) +
    geom_rect(data = cols, aes(xmin = x0, xmax = x1, ymin = 1, ymax = 61), fill = "#fcfcfb", colour = NA) +
    geom_rect(data = cols, aes(xmin = x0, xmax = x1, ymin = 57, ymax = 61, fill = prog), colour = NA) +
    geom_text(data = cols, aes(x = (x0 + x1) / 2, y = 59, label = name), colour = "white", size = 2.6, fontface = "bold", family = FAM) +
    scale_fill_manual(values = PROG, guide = "none") +
    geom_text(data = rows, aes(x = 1.5, y = y, label = lab), hjust = 0, size = 2.6, fontface = "bold", colour = INK, family = FAM, lineheight = 0.9) +
    geom_hline(yintercept = c(45.5, 34.5, 25, 13), colour = "#e1e0d9", linewidth = 0.3)
  ## students
  p <- p + txt(35, 51, "2023: 765 in 3 team vivas\n2024: 687 in 3 team vivas", 2.35) +
    txt(61, 51, "2023: 565\n2024: 527", 2.35) + txt(87, 51, "2024: 315", 2.35)
  ## training
  p <- p + txt(35, 40, "Workshops, videos,\nweekly meetings, mock vivas", 2.35) + txt(61, 40, "Workshops\nmock vivas (2024)", 2.35) + txt(87, 40, "Workshop", 2.35)
  ## structure
  p <- p + txt(35, 30, "Rubric, proforma", 2.35) + txt(61, 30, "Rubric, question pool", 2.35) + txt(87, 30, "Rubric, prescribed questions", 2.35)
  ## format icons
  for (i in 0:3) p <- p + person(28.5 + i * 2.6, 18.3, 0.75, BLUE)
  p <- p + arrow_(39.8, 37.6, 18.8) + person(42, 18.3, 1)
  p <- p + person(52.5, 20.5, 0.75, ORANGE) + person(56, 20.5, 1) + txt(54.3, 15.6, "2023", 2.2, col = INK2)
  p <- p + person(61.5, 20.5, 0.75, ORANGE) + person(65, 20.5, 1) + arrow_(66.6, 68.6, 21) + person(70.3, 20.5, 1) + txt(66, 15.6, "2024", 2.2, col = INK2)
  p <- p + person(79, 18.3, 0.75, AQUA) + person(83, 18.3, 1) + arrow_(84.6, 86.6, 18.8) + person(88.3, 18.3, 1) + arrow_(89.9, 91.9, 18.8) + person(93.6, 18.3, 1)
  ## intensity bars (as in the conference presentation)
  p <- p + geom_text(data = data.frame(k = 1:3, y = 9.4 - (0:2) * 3, l = c("Training", "Structure", "Format")), aes(x = 1.5, y = y, label = l),
                     hjust = 0, size = 2.3, colour = INK2, family = FAM) +
    geom_rect(data = bars, aes(xmin = xs, xmax = xe, ymin = y - 0.9, ymax = y + 0.9, fill = prog), alpha = 0.85) +
    geom_rect(data = bars %>% distinct(prog, k, y, xs), aes(xmin = xs, xmax = xs + 15.6, ymin = y - 0.9, ymax = y + 0.9), fill = NA, colour = "#e1e0d9", linewidth = 0.25)
  p
}
save(fig1(), "Figure1_study_design", 7.0, 3.5)

## ---------------------------------------------------------------- Figure 2 (the assessor matters)
## A: the six assessors who examined all four 2023 vivas (as in the conference presentation)
d23 <- A %>% filter(exam_id %in% c("MED23-1", "MED23-2", "MED23-3", "BIO23"), common_examiner_2023 == 1) %>%
  mutate(viva = factor(lab[exam_id], levels = names(VIVACOL)))
cm <- code_by_mean(d23); d23$Assessor <- factor(cm[d23$examiner_global], levels = cm)
off <- setNames(c(-0.3, -0.1, 0.1, 0.3), names(VIVACOL))
set.seed(1)
d23 <- d23 %>% mutate(x = as.integer(Assessor) + off[as.character(viva)], xj = x + runif(n(), -0.045, 0.045))
pA <- ggplot(d23, aes(x, score_pct)) +
  geom_boxplot(aes(fill = viva, group = interaction(Assessor, viva)), width = 0.17, outlier.shape = NA, linewidth = 0.3, colour = INK2) +
  geom_point(aes(x = xj), size = 0.35, alpha = 0.35, colour = INK2) +
  scale_fill_manual(values = VIVACOL) +
  scale_x_continuous(breaks = seq_along(cm), labels = cm, expand = c(0.03, 0.03)) +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) +
  labs(x = "Assessor", y = "Mark (%)") + th + theme(legend.position = "top", legend.margin = margin(0, 0, -4, 0))
## B: the same assessors one year later (all assessors who examined in both years)
mx <- read.csv(file.path(O, "T5b_multi_exam_assessors.csv")) %>% mutate(yr = ifelse(grepl("23", exam_id), "2023", "2024")) %>%
  group_by(examiner_global, yr) %>% summarise(z = weighted.mean(dev_z, n), .groups = "drop") %>%
  group_by(examiner_global) %>% filter(n() == 2) %>% ungroup() %>%
  group_by(examiner_global) %>% mutate(dir = ifelse(z[yr == "2023"] > 0, "Lenient in 2023", "Stringent in 2023")) %>% ungroup()
pB <- ggplot(mx, aes(yr, z, group = examiner_global, colour = dir)) + geom_hline(yintercept = 0, colour = "#c3c2b7", linewidth = 0.3) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.3) +
  scale_colour_manual(values = c("Lenient in 2023" = BLUE, "Stringent in 2023" = ORANGE), guide = "none") +
  scale_x_discrete(expand = c(0.15, 0.15)) + scale_y_continuous(limits = c(-0.7, 0.7), breaks = c(-0.5, 0, 0.5)) +
  labs(x = NULL, y = "Leniency (SD above cohort mean)") + th
## C: two assessors per student (BIOMED 2024): final mark by assessor pair
pr <- A %>% filter(exam_id == "BIO24") %>% group_by(unit_id) %>%
  summarise(final = mean(score_pct), pair = paste(sort(examiner_global), collapse = "+"), .groups = "drop") %>%
  group_by(pair) %>% filter(n() >= 5) %>% mutate(m = mean(final)) %>% ungroup() %>% mutate(pair = reorder(pair, m))
pm <- pr %>% distinct(pair, m)
pC <- ggplot(pr, aes(pair, final)) + geom_boxplot(outlier.shape = NA, linewidth = 0.2, colour = "#a9a8a0", fill = "#fbe1d6", width = 0.7) +
  geom_line(data = pm, aes(pair, m, group = 1), colour = ORANGE, linewidth = 0.6) +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) +
  labs(x = "Assessor pair (BIOMED 2024)", y = "Final mark (%)") + th + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
fig2 <- pA / (pB + pC + plot_layout(widths = c(1, 2.6))) + plot_layout(heights = c(1.15, 1)) + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 11, family = FAM))
save(fig2, "Figure2_assessor_effect", 7.0, 5.4)

## ---------------------------------------------------------------- Figure 3 (BiasFix)
b23 <- A %>% filter(exam_id == "BIO23")
mm <- b23 %>% group_by(examiner_global) %>% summarise(m = mean(score_pct), n = n()) %>% filter(n >= 30) %>% arrange(m)
two <- b23 %>% filter(examiner_global %in% c(mm$examiner_global[nrow(mm)], mm$examiner_global[1])) %>%
  mutate(Assessor = ifelse(examiner_global == mm$examiner_global[nrow(mm)], "Lenient", "Stringent"))
tl <- bind_rows(two %>% transmute(Assessor, step = "Raw marks", y = score_pct), two %>% transmute(Assessor, step = "After BiasFix", y = bf)) %>%
  mutate(step = factor(step, levels = c("Raw marks", "After BiasFix")))
pA3 <- ggplot(tl, aes(Assessor, y, fill = Assessor)) + facet_wrap(~step) +
  geom_hline(yintercept = mean(b23$score_pct), colour = BLUE, linewidth = 0.4) +
  geom_boxplot(width = 0.6, outlier.shape = NA, linewidth = 0.3, colour = INK2, alpha = 0.9) +
  geom_point(position = position_jitter(width = 0.12, seed = 3), size = 0.4, alpha = 0.4, colour = INK2) +
  scale_fill_manual(values = c(Lenient = "#6da7ec", Stringent = "#ec8a87"), labels = c("Lenient", "Stringent")) +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) +
  labs(x = NULL, y = "Mark (%)") + th + theme(legend.position = "top", legend.margin = margin(0, 0, -4, 0),
                                               axis.text.x = element_blank(), axis.ticks.x = element_blank())
ex3 <- c("BIO23" = "BIOMED 2023", "BIO24-S1" = "BIOMED 2024 station 1", "DEV24-S1" = "DEV 2024 station 1")
d3 <- A %>% filter(stream %in% names(ex3)) %>% group_by(stream) %>%
  mutate(o = rank(-ave(score_pct, examiner_global, FUN = mean), ties.method = "min")) %>% ungroup()
d3 <- d3 %>% group_by(stream) %>% mutate(Assessor = factor(dense_rank(o))) %>% ungroup()
d3l <- bind_rows(d3 %>% transmute(stream, Assessor, step = "Raw", y = score_pct), d3 %>% transmute(stream, Assessor, step = "BiasFix", y = bf)) %>%
  mutate(step = factor(step, levels = c("Raw", "BiasFix")), viva = factor(ex3[stream], levels = ex3))
pB3 <- ggplot(d3l, aes(Assessor, y)) + facet_grid(step ~ viva, scales = "free_x", space = "free_x", switch = "y") +
  geom_hline(data = d3l %>% group_by(viva, step) %>% summarise(m = mean(y), .groups = "drop"), aes(yintercept = m), colour = BLUE, linewidth = 0.35) +
  geom_boxplot(aes(fill = step), outlier.shape = NA, linewidth = 0.25, colour = INK2, width = 0.65) +
  scale_fill_manual(values = c(Raw = "#d9d8d2", BiasFix = "#b7d3f6"), guide = "none") + scale_y_continuous(limits = c(20, 100), breaks = c(25, 50, 75, 100)) +
  labs(x = "Assessor", y = "Mark (%)") + th +
  theme(strip.placement = "outside", axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.spacing = unit(2, "mm"))
fig3 <- (pA3 | pB3) + plot_layout(widths = c(1, 2.2)) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 11, family = FAM))
save(fig3, "Figure3_biasfix", 7.0, 3.3)

## ---------------------------------------------------------------- Figure 4 (does it work; how many students)
s1 <- A %>% filter(stream == "BIO24-S1")
cm4 <- code_by_mean(s1 %>% select(examiner_global, score_pct))
s1l <- bind_rows(s1 %>% transmute(examiner_global, m = "Raw", y = score_pct), s1 %>% transmute(examiner_global, m = "MFRM", y = mfrm),
                 s1 %>% transmute(examiner_global, m = "BiasFix", y = bf)) %>%
  mutate(m = factor(m, levels = c("Raw", "MFRM", "BiasFix")), Assessor = factor(cm4[examiner_global], levels = cm4))
pA4 <- ggplot(s1l, aes(Assessor, y, fill = m)) + facet_wrap(~m, nrow = 1) +
  geom_hline(yintercept = mean(s1$score_pct), colour = BLUE, linewidth = 0.35) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.25, colour = INK2, width = 0.65) +
  scale_fill_manual(values = c(Raw = "#d9d8d2", MFRM = "#f6c4a8", BiasFix = "#b7d3f6"), guide = "none") +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) + labs(x = "Assessor", y = "Mark (%)") + th +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ag <- read.csv(file.path(O, "T7a_interstation_agreement.csv")); cr <- read.csv(file.path(O, "T8_criterion_relationships.csv"))
vd <- bind_rows(ag %>% filter(exam_id == "BIO24", method %in% c("raw", "mfrm", "biasfix")) %>% transmute(metric = "Station 1 vs\nstation 2", method, r = mean_interstation_r),
                cr %>% filter(exam_id == "BIO24", method %in% c("raw", "mfrm", "biasfix")) %>% transmute(metric = "Viva vs\nacademic record", method, r)) %>%
  mutate(method = factor(c(raw = "Raw", mfrm = "MFRM", biasfix = "BiasFix")[method], levels = c("Raw", "MFRM", "BiasFix")))
pB4 <- ggplot(vd, aes(metric, r, fill = method)) + geom_col(position = position_dodge(0.75), width = 0.7) +
  scale_fill_manual(values = c(Raw = "#bdbcb5", MFRM = ORANGE, BiasFix = BLUE)) + scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.6, 0.2), expand = c(0, 0)) +
  labs(x = NULL, y = "Correlation (r)") + th + theme(legend.position = "top", legend.margin = margin(0, 0, -4, 0))
sim <- read.csv(file.path(O, "S_simulation_results.csv")) %>% filter(method == "BiasFix", allocation == "random", sd_b == 0.2, sd_L %in% c(2, 4, 6, 8))
pC4 <- ggplot(sim, aes(m, pct_closer, colour = factor(sd_L))) + geom_hline(yintercept = 50, colour = "#c3c2b7", linewidth = 0.35, linetype = 2) +
  geom_line(linewidth = 0.6) + geom_point(size = 0.9) + scale_x_continuous(trans = "log10", breaks = c(5, 10, 15, 30, 60)) +
  scale_colour_manual(values = c("2" = "#cde2fb", "4" = "#86b6ef", "6" = "#3987e5", "8" = "#184f95"), labels = c("2", "4", "6", "8"),
                      name = "Assessor SD") +
  scale_y_continuous(limits = c(20, 75), breaks = seq(20, 70, 10)) + labs(x = "Students per assessor", y = "Students moved closer\nto their fair mark (%)") + th +
  theme(legend.position = "top", legend.title = element_text(size = 7), legend.margin = margin(0, 0, -4, 0))
dst <- read.csv(file.path(O, "T16_dstudy.csv")) %>% filter(assessors_per_station == 1) %>%
  mutate(viva = ifelse(exam_id == "BIO24", "BIOMED 2024", "DEV 2024"), marks = factor(marks, levels = c("raw", "BiasFix"), labels = c("Raw", "BiasFix")))
pD4 <- ggplot(dst, aes(stations, G_relative, colour = viva, linetype = marks, shape = marks)) +
  geom_hline(yintercept = 0.8, colour = "#c3c2b7", linewidth = 0.35, linetype = 2) +
  geom_line(linewidth = 0.55) + geom_point(size = 1, fill = "white") +
  scale_colour_manual(values = c("BIOMED 2024" = ORANGE, "DEV 2024" = AQUA)) + scale_linetype_manual(values = c(Raw = "22", BiasFix = "solid")) +
  scale_shape_manual(values = c(Raw = 21, BiasFix = 16)) + scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(limits = c(0.4, 0.92), breaks = seq(0.4, 0.9, 0.1)) +
  annotate("text", x = 2.75, y = 0.875, label = "BIOMED", colour = ORANGE, size = 2.5, family = FAM, fontface = "bold") +
  annotate("text", x = 3.6, y = 0.635, label = "DEV", colour = AQUA, size = 2.5, family = FAM, fontface = "bold") +
  labs(x = "Stations per student", y = "Reliability of final mark") + th +
  theme(legend.position = "top", legend.margin = margin(0, 0, -4, 0), legend.key.width = unit(6, "mm")) +
  guides(colour = "none")
fig4 <- pA4 / (pB4 + pD4 + pC4 + plot_layout(widths = c(1, 1.05, 1.15))) + plot_layout(heights = c(1, 1.15)) + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 11, family = FAM))
save(fig4, "Figure4_biasfix_evaluation", 7.0, 5.2)

## ================================================================ Supplementary figures
tagt <- theme(plot.tag = element_text(face = "bold", size = 11, family = FAM))
## S1: every rating stream, raw marks by assessor
ord <- c("MED23-1", "MED23-2", "MED23-3", "MED24-1", "MED24-2", "MED24-3", "BIO23", "BIO24-S1", "BIO24-S2", "DEV24-S1", "DEV24-S2", "DEV24-S3")
slab <- c("MED23-1" = "MED 2023 viva 1", "MED23-2" = "MED 2023 viva 2", "MED23-3" = "MED 2023 viva 3", "MED24-1" = "MED 2024 viva 1", "MED24-2" = "MED 2024 viva 2",
          "MED24-3" = "MED 2024 viva 3", "BIO23" = "BIOMED 2023", "BIO24-S1" = "BIOMED 2024 station 1", "BIO24-S2" = "BIOMED 2024 station 2",
          "DEV24-S1" = "DEV 2024 station 1", "DEV24-S2" = "DEV 2024 station 2", "DEV24-S3" = "DEV 2024 station 3")
sA <- A %>% group_by(stream) %>% mutate(Assessor = factor(dense_rank(-ave(score_pct, examiner_global, FUN = mean)))) %>% ungroup() %>%
  mutate(stream = factor(slab[stream], levels = slab[ord]), prog = substr(exam_id, 1, 3))
pS1 <- ggplot(sA, aes(Assessor, score_pct, fill = prog)) + facet_wrap(~stream, ncol = 3, scales = "free_x") +
  geom_hline(data = sA %>% group_by(stream) %>% summarise(m = mean(score_pct)), aes(yintercept = m), colour = BLUE, linewidth = 0.3) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.25, colour = INK2, width = 0.65, alpha = 0.6) +
  scale_fill_manual(values = c(MED = BLUE, BIO = ORANGE, DEV = AQUA), guide = "none") + scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Assessor (most lenient to most stringent)", y = "Mark (%)") + th + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save(pS1, "FigureS1_all_vivas", 7.0, 7.0)

## S2: comparable students, different marks (BIOMED 2024, station 1)
s1w <- A %>% filter(stream == "BIO24-S1", !is.na(ext_measure)) %>%
  mutate(viva = (score_pct - mean(score_pct)) / sd(score_pct), wam = (ext_measure - mean(ext_measure)) / sd(ext_measure))
g <- s1w %>% group_by(examiner_global) %>% summarise(n = n(), v = mean(viva), vse = sd(viva) / sqrt(n), w = mean(wam), wse = sd(wam) / sqrt(n)) %>%
  arrange(desc(v)) %>% mutate(Assessor = factor(paste0("A", row_number()), levels = paste0("A", row_number())))
gl <- bind_rows(g %>% transmute(Assessor, what = "Viva mark", m = v, se = vse), g %>% transmute(Assessor, what = "Academic record (WAM)", m = w, se = wse)) %>%
  mutate(what = factor(what, levels = c("Viva mark", "Academic record (WAM)")))
pS2 <- ggplot(gl, aes(Assessor, m, colour = what, shape = what)) + geom_hline(yintercept = 0, colour = "#c3c2b7", linewidth = 0.3) +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se), position = position_dodge(0.5), size = 0.25, linewidth = 0.4, fill = "white") +
  scale_colour_manual(values = c(BLUE, GREY)) + scale_shape_manual(values = c(16, 21)) +
  labs(x = "Assessor", y = "Students' mean (SD from cohort mean)") + th + theme(legend.position = "top")
save(pS2, "FigureS2_same_students", 5.0, 3.2)

## S3: persistence of leniency between vivas
dv <- read.csv(file.path(O, "T5b_multi_exam_assessors.csv"))
ord_e <- c("MED23-1", "MED23-2", "MED23-3", "BIO23", "MED24-1", "BIO24", "MED24-2", "MED24-3", "DEV24")
pairs <- dv %>% inner_join(dv, by = "examiner_global", suffix = c("1", "2"), relationship = "many-to-many") %>%
  filter(match(exam_id1, ord_e) < match(exam_id2, ord_e))
pS3 <- ggplot(pairs, aes(dev_z1, dev_z2)) + geom_hline(yintercept = 0, colour = "#e1e0d9") + geom_vline(xintercept = 0, colour = "#e1e0d9") +
  geom_abline(colour = "#c3c2b7", linewidth = 0.3) + geom_point(colour = BLUE, alpha = 0.6, size = 1.2) +
  coord_equal(xlim = c(-1.8, 1.5), ylim = c(-1.8, 1.5)) + labs(x = "Leniency in earlier viva (SD)", y = "Leniency in later viva (SD)") + th
save(pS3, "FigureS3_persistence", 3.4, 3.2)

## S4: grade flows (BiasFix) in the summative BIOMED vivas
gr <- function(s) cut(s, c(-Inf, 50, 60, 70, 80, Inf), right = FALSE, labels = c("N", "P", "C", "D", "HD"))
fl <- bind_rows(A %>% filter(exam_id == "BIO23") %>% transmute(exam = "BIOMED 2023", raw = score_pct, bf),
                A %>% filter(exam_id == "BIO24") %>% group_by(unit_id) %>% summarise(raw = mean(score_pct), bf = mean(bf)) %>% transmute(exam = "BIOMED 2024", raw, bf)) %>%
  mutate(Raw = gr(raw), BiasFix = gr(bf)) %>% count(exam, Raw, BiasFix)
pS4 <- ggplot(fl, aes(axis1 = Raw, axis2 = BiasFix, y = n)) + facet_wrap(~exam, scales = "free_y") +
  geom_alluvium(aes(fill = Raw), width = 0.25, alpha = 0.75) + geom_stratum(width = 0.25, fill = "#f0efec", colour = INK2, linewidth = 0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.3, family = FAM) +
  scale_x_discrete(limits = c("Raw", "BiasFix"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = c(HD = "#184f95", D = "#3987e5", C = "#86b6ef", P = "#cde2fb", N = "#e34948"), guide = "none") +
  labs(x = NULL, y = "Students") + th
save(pS4, "FigureS4_grade_flows", 6.0, 3.2)

## S5: coordinators' ad hoc adjustment (BIOMED 2023)
ah <- b23 %>% group_by(examiner_global) %>% summarise(raw = mean(score_pct), adhoc = mean(adhoc_score_pct), n = n()) %>% arrange(desc(raw)) %>%
  mutate(code = paste0("A", row_number()))
ah$Assessor <- factor(ah$code, levels = rev(ah$code))
pS5 <- ggplot(ah, aes(y = Assessor)) + geom_vline(xintercept = mean(b23$score_pct), colour = BLUE, linewidth = 0.4) +
  geom_segment(data = ah %>% filter(abs(adhoc - raw) > 0.01), aes(x = raw, xend = adhoc, yend = Assessor), colour = ORANGE,
               arrow = arrow(length = unit(1.3, "mm"), type = "closed"), linewidth = 0.5) +
  geom_point(aes(x = raw), shape = 21, fill = "white", colour = INK2, size = 1.6) +
  scale_y_discrete(limits = rev(ah$code)) + labs(x = "Assessor mean mark (%)", y = NULL) + th
save(pS5, "FigureS5_adhoc", 4.0, 3.4)

## S6: MFRM (connected designs): A severity vs raw leniency; B adjusted final marks
par <- read.csv(file.path(O, "mfrm_parameters.csv"))
ms <- bind_rows(lapply(c("BIO24", "DEV24"), function(e) {
  d <- A %>% filter(exam_id == e); dvv <- d %>% group_by(examiner_global) %>% summarise(dev = mean(score_pct) - mean(d$score_pct), n = n())
  par %>% filter(exam_id == e, facet == "assessor") %>% transmute(examiner_global = element, sev = measure, se = se) %>% inner_join(dvv, by = "examiner_global") %>%
    mutate(exam = ifelse(e == "BIO24", "BIOMED 2024", "DEV 2024"))
}))
pS6a <- ggplot(ms, aes(sev, dev, colour = exam)) + facet_wrap(~exam) + geom_hline(yintercept = 0, colour = "#e1e0d9") + geom_vline(xintercept = 0, colour = "#e1e0d9") +
  geom_errorbarh(aes(xmin = sev - 1.96 * se, xmax = sev + 1.96 * se), height = 0, colour = "#c3c2b7", linewidth = 0.3) +
  geom_point(aes(size = n), alpha = 0.8) + scale_size_area(max_size = 3, guide = "none") + scale_colour_manual(values = c(ORANGE, AQUA), guide = "none") +
  labs(x = "MFRM severity (logits)", y = "Assessor mean minus\ncohort mean (points)") + th
f6 <- A %>% filter(exam_id %in% c("BIO24", "DEV24")) %>% group_by(exam_id, unit_id) %>% summarise(bf = mean(bf), mfrm = mean(mfrm), .groups = "drop") %>%
  mutate(exam = ifelse(exam_id == "BIO24", "BIOMED 2024", "DEV 2024"))
pS6b <- ggplot(f6, aes(mfrm, bf, colour = exam)) + facet_wrap(~exam) + geom_abline(colour = "#c3c2b7", linewidth = 0.35) +
  geom_point(size = 0.5, alpha = 0.5) + scale_colour_manual(values = c(ORANGE, AQUA), guide = "none") +
  coord_equal(xlim = c(10, 100), ylim = c(10, 100)) + labs(x = "MFRM-adjusted final mark (%)", y = "BiasFix-adjusted\nfinal mark (%)") + th
save((pS6a / pS6b) + plot_annotation(tag_levels = "A") & tagt, "FigureS6_MFRM", 5.6, 5.6)

## S7: simulation, all methods
simall <- read.csv(file.path(O, "S_simulation_results.csv")) %>% filter(sd_b == 0.2) %>%
  mutate(method = factor(method, levels = c("raw", "mean-shift", "BiasFix", "shrinkage"), labels = c("Raw", "Mean shift", "BiasFix", "Mixed model")),
         allocation = factor(allocation, levels = c("random", "ability-related"), labels = c("Random allocation", "Ability-related allocation")),
         L = factor(paste0("SD ", sd_L), levels = paste0("SD ", c(0, 2, 4, 6, 8, 10))))
pS7 <- ggplot(simall, aes(m, assessor_error, colour = method)) + facet_grid(allocation ~ L) + geom_line(linewidth = 0.5) + geom_point(size = 0.6) +
  scale_x_continuous(trans = "log10", breaks = c(5, 15, 40)) + scale_colour_manual(values = c(Raw = GREY, `Mean shift` = YELLOW, BiasFix = BLUE, `Mixed model` = AQUA)) +
  labs(x = "Students per assessor", y = "Error due to assessors (points)") + th + theme(legend.position = "top", strip.text = element_text(size = 6.5))
save(pS7, "FigureS7_simulation", 7.0, 3.8)
cat("figures done\n")

## S8: the BiasFix application (frames from Supplementary Video 1)
fr <- file.path(base, "data", "app_frames", paste0("frame_", c(25, 45, 59, 71), ".png"))
if (all(file.exists(fr))) {
  pp <- lapply(fr, function(f) { r <- png::readPNG(f)
    ggplot() + annotation_custom(grid::rasterGrob(r, interpolate = TRUE), -Inf, Inf, -Inf, Inf) + coord_fixed(ratio = dim(r)[1] / dim(r)[2], xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      theme_void() + theme(plot.margin = margin(3, 3, 3, 3), panel.border = element_rect(fill = NA, colour = "#c3c2b7", linewidth = 0.3)) })
  save(wrap_plots(pp, ncol = 2) + plot_annotation(tag_levels = "A") & tagt, "FigureS8_app", 7.0, 3.75)
}

## S9: cross-validation - held-out gains across random splits (50% calibration)
cv <- read.csv(file.path(O, "T15_crossval_splits.csv")) %>% filter(cal_fraction == 0.5, metric %in% c("r_station", "r_wam")) %>%
  filter(!(exam_id == "DEV24" & metric == "r_wam")) %>%
  pivot_longer(c(bf, ms, lmm), names_to = "method", values_to = "adj") %>% mutate(gain = adj - score_pct,
    method = factor(method, levels = c("bf", "ms", "lmm"), labels = c("BiasFix", "Mean shift", "Mixed model")),
    panel = factor(paste(ifelse(exam_id == "BIO24", "BIOMED 2024", "DEV 2024"), ifelse(metric == "r_station", "station agreement", "academic record"), sep = ": "),
                   levels = c("BIOMED 2024: station agreement", "BIOMED 2024: academic record", "DEV 2024: station agreement")))
pS9 <- ggplot(cv, aes(method, gain, fill = method)) + facet_wrap(~panel, nrow = 1) + geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_violin(colour = NA, alpha = 0.5, scale = "width", width = 0.8) + geom_boxplot(width = 0.18, outlier.shape = NA, linewidth = 0.3, colour = INK2, fill = "white") +
  scale_fill_manual(values = c(BiasFix = BLUE, `Mean shift` = YELLOW, `Mixed model` = AQUA), guide = "none") +
  coord_cartesian(ylim = c(-0.15, 0.2)) + labs(x = NULL, y = "Change in held-out correlation (r)") + th + theme(strip.text = element_text(size = 7))
save(pS9, "FigureS9_crossvalidation", 7.0, 2.8)
