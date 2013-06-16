
# Classification of Numerama and ZDNet news items, 2005-2013

require(ggplot2)
require(lubridate)
require(RCurl)
require(reshape)
require(scales)
require(stringr)

rm(list = ls())

# build corpus
# see data/ scripts to recreate corpus classes data
nu = read.csv("data/numerama.corpus.classes.txt")
zd = read.csv("data/zdnet.corpus.classes.txt")

z = rbind(nu, zd)[, 2:4]
z = ddply(z, .(c, t), summarise, N = sum(N))
z$t2 <- as.Date(paste(z$t, "01"), "%Y-%m %d")

# ----
# plot
# ----

nl <- labs(y = NULL, x = NULL)
th <- theme_bw(12) + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank())

# point graph
q <- qplot(data = z, y = c, x = t2, size = N, group = c, geom = "point") +
  scale_x_date(labels = date_format("%Y")) +
  scale_size_area("Items", max_size = 5) + nl + th
q
ggsave("mediagraph.pdf", width = 10, height = 7.5, unit = "in")

write.csv(z, "corpus.classes.txt")

# facet recodes

z$c2 <- NA
z$c2 <- ifelse(grepl("Opposants|Industrie", z$c), "Groupes de pression", z$c2)
z$c2 <- ifelse(grepl("politique|Parlement|Exécutif", z$c), "Champ politique", z$c2)
z$c2 <- ifelse(is.na(z$c2), "Dossiers", z$c2)
table(z$c2)
z$c2 <- factor(z$c2)

ggplot(data = z, aes(y = c, x = t2, size = N, group = c)) + geom_point() +
  scale_x_date(labels = date_format("%Y")) +
  geom_vline(xintercept = z$t2[12]) +
  scale_size_area("Items", max_size = 5) + nl + th + 
  facet_grid(c2 ~ ., scales = "free_y")

# 2013-05-20
