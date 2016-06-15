setwd("KUL/SEM2/stat/lecture_data/")

library(pastecs)

kalama = read.table("kalama.txt", header = T)

descrip.kalama <- stat.desc(kalama[, c("age", "height")], basic = T, desc = T)
descrip.kalama
plot(kalama)

cor.test(kalama$age, kalama$height, alternative = "two.sided", method = "pearson")
