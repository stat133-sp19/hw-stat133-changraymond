Title: Shot Charts
Description: The current goal is to create shot charts and visualizations of where basketball shots are made.
Input(s): The current inputs are from 5 different csv files: stephen-curry.csv, andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, and klay-thompson.csv. There are also 
Output(s): The goal is so create a single outputted csv file called shots-data.csv.

library(jpeg)
library(grid)
library(ggplot2)

klay_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

curry_scatterplot <- ggplot(data = curry) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

curry_scatterplot
curry_shot_chart

iguodala_scatterplot <- ggplot(data = iguodala) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

iguodala_scatterplot
iguodala_shot_chart

green_scatterplot <- ggplot(data = green) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

green_scatterplot
green_shot_chart

durant_scatterplot <- ggplot(data = durant) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

durant_scatterplot
durant_shot_chart

thompson_scatterplot <- ggplot(data = thompson) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

thompson_scatterplot
thompson_shot_chart 


pdf("../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iguodala_shot_chart
dev.off()

pdf("../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_shot_chart
dev.off()

pdf("../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
durant_shot_chart
dev.off()

pdf("../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
thompson_shot_chart
dev.off()

pdf("../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

gsw_shot_chart <- ggplot(data = stacked_tables)+
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() + facet_wrap(~ name)

gsw_shot_chart

pdf("../images/gsw-shot-charts.pdf", width = 8, height = 7)
gsw_shot_chart
dev.off()

png("../images/gsw-shot-charts.png", width = 8, height = 7, units = "in", res = 300)
gsw_shot_chart
dev.off()
