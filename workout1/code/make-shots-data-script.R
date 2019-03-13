Title: Data Preparation
Description: The current goal is to create a combined csv file that contains the required variables to be used for later visualizations.
Input(s): The current inputs are from 5 different csv files: stephen-curry.csv, andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, and klay-thompson.csv.
Output(s): The goal is so create a single outputted csv file called shots-data.csv.

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)

stephen_curry <- rep("Stephen Curry", nrow(curry))
andre_iguodala <- rep("Andre Iguodala", nrow(iguodala))
draymond_green <- rep("Draymond Green", nrow(green))
kevin_durant <- rep("Kevin Durant", nrow(durant))
klay_thompson <- rep("Klay Thompson", nrow(thompson))

curry$new.col <- stephen_curry
names(curry)[14] <- "name"
curry

iguodala$new.col <- andre_iguodala
names(iguodala)[14] <- "name"
iguodala

green$new.col <- draymond_green
names(green)[14] <- "name"
green

durant$new.col <- kevin_durant
names(durant)[14] <- "name"
durant

thompson$new.col <- klay_thompson
names(thompson)[14] <- "name"
thompson

curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
curry

iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
iguodala

green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
green

durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
durant

thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
thompson

curry$new.col <-(curry$period * 12) - curry$minutes_remaining
names(curry)[15] <- "minute"
curry

iguodala$new.col <-(iguodala$period * 12) - iguodala$minutes_remaining
names(iguodala)[15] <- "minute"
iguodala

green$new.col <-(green$period * 12) - green$minutes_remaining
names(green)[15] <- "minute"
green

durant$new.col <-(durant$period * 12) - durant$minutes_remaining
names(durant)[15] <- "minute"
durant

thompson$new.col <-(thompson$period * 12) - thompson$minutes_remaining
names(thompson)[15] <- "minute"
thompson

summary(curry)
summary(iguodala)
summary(green)
summary(durant)
summary(thompson)

sink("../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink("../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink("../output/draymond-green-summary.txt")
summary(green)
sink()

sink("../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink("../output/klay-thompson-summary.txt")
summary(thompson)
sink()

stacked_tables <- rbind(curry, durant, green, iguodala, thompson)
write.csv(stacked_tables, "../data/shots-data.csv")

sink("../output/shots-data-summary.txt")
summary(stacked_tables)
sink()

