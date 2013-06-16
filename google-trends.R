
#
#

require(ggplot2)
require(lubridate)
require(plyr)
require(reshape)
require(scales)
require(grid)

# Load MASS package (provides "rlm" function).
library(MASS)
# Load splines package (provides "ns" function).
library(splines)

theme_set(theme_bw(16))
theme_update(
    panel.border = element_rect(fill = NA, color = "white"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(face = "bold", vjust = 1),
    axis.title.y = element_text(angle = 90, vjust = -.25),
    axis.title.x = element_text(vjust = -1),
    legend.position = 'bottom'
  )

data = c("hadopi", "dadvsi", "loppsi") # not showing "ipred" and "acta"
path = "data/google-trends.france-2013-05-21-"

news = rbind.fill(lapply(data, function(x) 
	read.csv(paste0(path, x, ".csv"), skip = 4, nrows = 490)))
news = melt(news, id = "Week")

news$Week = substr(news$Week, 0, 10)
news$Week = as.Date(news$Week)
news$variable = toupper(news$variable)
summary(news$value)
news = na.omit(subset(news, value > 2 & year(news$Week) > 2004))

g <- qplot(data = news, x = Week, y = value, geom = "line") + 
	scale_colour_brewer("", palette = "Set1") +
	labs(y = NULL, x = NULL) +
	geom_point(data = subset(news, value > 50)) +
	geom_text(data = subset(news, value > 50), aes(label = Week), hjust = 1.1, size = 4) +
	facet_grid(variable ~ .) + 
	scale_x_date(
		breaks = date_breaks("year"), 
		labels = date_format("%Y"),
		limits = c(as.Date("2005-01-01"), max(news$Week))
		) +
	theme(panel.margin = unit(1, "cm"), strip.background = element_rect(fill = NA))
g

ggsave(g, file = "google-trends.png", width = 10, height = 7.5, unit = "in")
ggsave(g, file = "google-trends.pdf", width = 10, height = 7.5, unit = "in")

# 2013-05-21
