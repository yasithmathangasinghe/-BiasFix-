## =============================================================================
## biasfix.R - stand-alone implementation of the BiasFix adjustment
## Adjusts marks for assessor (examiner) leniency by placing each assessor's marks
## on the scale of the whole cohort (mean-sigma linear equating):
##   z  = (score - assessor mean) / assessor SD
##   adjusted = cohort mean + z * cohort SD   (capped at the maximum mark)
## Use one rating stream at a time (a single-station viva, or one station of a
## multi-station viva); combine stations after adjustment.
## Licence: MIT. See README.md for citation.
## =============================================================================

#' Adjust marks for assessor leniency (BiasFix)
#'
#' @param data      data frame with one row per student mark
#' @param max_score maximum possible mark (adjusted marks are capped at this value)
#' @param student, examiner, score  column names
#' @param min_n     warn when an assessor marked fewer than this many students
#' @return data with columns Standardized_Score (z within examiner) and
#'         Retransformed_Score (adjusted mark; use this as the final mark)
biasfix <- function(data, max_score = 100, student = "Student_ID", examiner = "Examiner",
                    score = "Score", min_n = 15) {
  stopifnot(all(c(student, examiner, score) %in% names(data)))
  d <- data
  d[[score]] <- suppressWarnings(as.numeric(as.character(d[[score]])))
  d <- d[!is.na(d[[score]]), , drop = FALSE]
  g <- as.character(d[[examiner]])
  M <- mean(d[[score]]); S <- sd(d[[score]])
  n_g  <- tapply(d[[score]], g, length)
  m_g  <- tapply(d[[score]], g, mean)
  s_g  <- tapply(d[[score]], g, sd)
  small <- names(n_g)[n_g < min_n]
  if (length(small)) warning(sprintf("Assessor(s) with fewer than %d students: %s. Adjustment for these assessors is uncertain; consider allocating more students per assessor.",
                                     min_n, paste(small, collapse = ", ")), call. = FALSE)
  bad <- names(s_g)[is.na(s_g) | s_g == 0]
  if (length(bad)) warning(sprintf("Assessor(s) with a single student or identical marks (no SD): %s. Their marks are left unadjusted.",
                                   paste(bad, collapse = ", ")), call. = FALSE)
  z <- (d[[score]] - m_g[g]) / s_g[g]
  adj <- pmin(M + S * z, max_score)
  ok <- !(g %in% bad)
  d$Standardized_Score  <- ifelse(ok, z, NA_real_)
  d$Retransformed_Score <- ifelse(ok, adj, d[[score]])
  d
}

#' Summarise assessor leniency before adjustment
#'
#' @return one row per assessor: n, mean, SD, difference from the cohort mean (points and
#'         cohort SD units) and the ratio of the assessor's SD to the cohort SD.
#'         If lme4 is installed, the attribute "assessor_share" gives the share of the
#'         variation in marks attributable to assessors (intraclass correlation).
assessor_summary <- function(data, examiner = "Examiner", score = "Score") {
  x <- suppressWarnings(as.numeric(as.character(data[[score]]))); g <- as.character(data[[examiner]])
  keep <- !is.na(x); x <- x[keep]; g <- g[keep]
  M <- mean(x); S <- sd(x)
  out <- do.call(rbind, lapply(split(x, g), function(v) data.frame(n = length(v), mean = mean(v), sd = sd(v))))
  out$examiner <- rownames(out); rownames(out) <- NULL
  out$difference_points <- out$mean - M
  out$difference_sd     <- out$difference_points / S
  out$sd_ratio          <- out$sd / S
  out <- out[order(-out$mean), c("examiner", "n", "mean", "sd", "difference_points", "difference_sd", "sd_ratio")]
  if (requireNamespace("lme4", quietly = TRUE)) {
    fit <- lme4::lmer(x ~ 1 + (1 | g))
    vc <- as.data.frame(lme4::VarCorr(fit))
    attr(out, "assessor_share") <- vc$vcov[vc$grp == "g"] / sum(vc$vcov)
  }
  out
}
