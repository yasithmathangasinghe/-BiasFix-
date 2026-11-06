## Basic checks of the BiasFix properties. Run: Rscript tests/test_biasfix.R
source("R/biasfix.R")
set.seed(1)
d <- data.frame(Student_ID = 1:300, Examiner = rep(LETTERS[1:6], each = 50))
d$Score <- round(pmin(100, pmax(0, 70 + rep(c(-8, -4, 0, 2, 5, 9), each = 50) + rnorm(300, 0, 12))), 1)
a <- biasfix(d, max_score = 100)
M <- mean(d$Score); S <- sd(d$Score)
m <- tapply(a$Retransformed_Score, a$Examiner, mean); s <- tapply(a$Retransformed_Score, a$Examiner, sd)
stopifnot(all(abs(m - M) < 0.5), all(abs(s - S) < 0.5))                       # common scale (allowing for capping)
stopifnot(all(tapply(seq_len(nrow(a)), a$Examiner, function(i) cor(a$Score[i], a$Retransformed_Score[i], method = "spearman")) > 0.999))  # rank preserved
stopifnot(max(a$Retransformed_Score) <= 100)
cat("All checks passed\n")
