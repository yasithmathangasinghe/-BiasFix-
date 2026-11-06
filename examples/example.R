## Example: adjust the sample dataset for assessor leniency
## Run from the repository root:  Rscript examples/example.R
source("R/biasfix.R")
d <- read.csv("examples/sample_dataset.csv")
s <- assessor_summary(d)
print(s, digits = 3)
cat("Share of variation in marks due to assessors:", round(100 * attr(s, "assessor_share"), 1), "%\n")
adj <- biasfix(d, max_score = 100)           # maximum possible mark
write.csv(adj, "examples/sample_dataset_adjusted.csv", row.names = FALSE)
## raw vs adjusted marks by assessor
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
boxplot(Score ~ Examiner, data = adj, main = "Raw", ylab = "Mark", xlab = "Assessor"); abline(h = mean(adj$Score), col = "blue")
boxplot(Retransformed_Score ~ Examiner, data = adj, main = "BiasFix", ylab = "Mark", xlab = "Assessor"); abline(h = mean(adj$Score), col = "blue")
par(op)
