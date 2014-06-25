# --------
# packages
# --------

# scraper

library(dplyr)
library(lubridate)
library(RCurl)
library(stringr)
library(XML)

# networks

library(intergraph)
library(network)
library(sna)
library(tnet)

# plots

library(GGally)
library(grid)
library(ggplot2)

# tables

library(knitr)

# --------
# articles
# --------

source("functions.r")

# page counters last updated on June 22, 2014

get_articles("echos",    "dadvsi", pages = 1 )  # 3    articles
get_articles("ecrans",   "dadvsi", pages = 13)  # 123  articles
get_articles("lefigaro",    "dadvsi", pages = 2)  # 39  articles
get_articles("numerama", "dadvsi", pages = 36)  # 717  articles
get_articles("zdnet",    "dadvsi", pages = 23)  # 227  articles

get_articles("echos",    "hadopi", pages = 22 ) # 192  articles
get_articles("ecrans",   "hadopi", pages = 100) # 997  articles
get_articles("lefigaro",    "hadopi", pages = 23) # 441 articles
get_articles("zdnet",    "hadopi", pages = 48 ) # 464  articles
get_articles("numerama", "hadopi", pages = 137) # 2729 articles

get_corpus(sample = FALSE, update = FALSE)

# --------
# networks
# --------

# Sequence 1, sep. 2005 - sep. 2006 (DADVSI promulgated Aug 1, 2006)

head(get_ranking(end = "2006-09-01", file = "seq1"), 25)

n = get_network(threshold = 2/3, end = "2006-09-01")
g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) + guides(size = FALSE)

ggsave("plots/seq1.png", g, width = 9, height = 9)

# Sequence 2, juil. 2007 - dec. 2009 (mission Olivennes - décrets Hadopi 2)

head(get_ranking(start = "2007-07-01", end = "2009-12-31", file = "seq2"), 25)

n = get_network(threshold = 2/3, start = "2007-07-01", end = "2009-12-31") 
g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) + guides(size = FALSE)

ggsave("plots/seq2.png", g, width = 9, height = 9)

# Sequence 3, juin 2012 - juin 2014 (mission Lescure - today)

head(get_ranking(start = "2012-05-22", file = "seq3"), 25)

n = get_network(threshold = 2/3, start = "2012-05-22")
g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) + guides(size = FALSE)

ggsave("plots/seq3.png", g, width = 9, height = 9)

# Full sequence (2005-2014, 9 years)

head(get_ranking(file = "full"), 25)

n = get_network(threshold = 2/3)
g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) +
  guides(size = FALSE)

ggsave("plots/full.png", g, width = 9, height = 9)

# have a nice day
