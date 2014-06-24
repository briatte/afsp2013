# --------
# packages
# --------

library(dplyr)
library(lubridate)
library(RCurl)
library(stringr)
library(XML)
library(network)
library(tnet)
library(GGally)

# --------
# articles
# --------

source("scraper.r")
source("network.r")

# page counters last updated on June 22, 2014

get_articles("echos",    "dadvsi", pages = 1 )  # 3  articles
get_articles("ecrans",   "dadvsi", pages = 13)  # 123  articles
get_articles("numerama", "dadvsi", pages = 36)  # 717  articles
get_articles("zdnet",    "dadvsi", pages = 23)  # 227  articles

get_articles("echos",    "hadopi", pages = 22 ) # 192  articles
get_articles("ecrans",   "hadopi", pages = 100) # 997  articles
get_articles("zdnet",    "hadopi", pages = 48 ) # 464  articles
get_articles("numerama", "hadopi", pages = 137) # 2729 articles

get_corpus(threshold = .9, sample = FALSE, update = FALSE)

# --------
# networks
# --------

# Sequence 1, sep. 2005 - sep. 2006 (DADVSI promulgated 2006-08-01, 1 year)
s = get_ranking(end = "2006-09-01")
head(s, 15)

n = get_network(threshold = 2/3, end = "2006-09-01")

g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) +
  guides(size = FALSE)

ggsave("plots/seq1.pdf", g, width = 9, height = 9)
ggsave("plots/seq1.png", g, width = 9, height = 9)

# Sequence 2, juil. 2007 - dec. 2009 (mission Olivennes - décrets Hadopi 2, 2.5 years)
s = get_ranking(start = "2007-07-01", end = "2009-12-31")
head(s, 15)

n = get_network(threshold = 2/3, start = "2007-07-01", end = "2009-12-31") 

g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) +
  guides(size = FALSE)

ggsave("plots/seq2.pdf", g, width = 9, height = 9)
ggsave("plots/seq2.png", g, width = 9, height = 9)

# Sequence 3, juin 2012 - juin 2014 (mission Lescure - today, 2 years)
s = get_ranking(start = "2012-05-22")
head(s, 15)

n = get_network(threshold = 2/3, start = "2012-05-22")

g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) +
  guides(size = FALSE)

ggsave("plots/seq3.pdf", g, width = 9, height = 9)
ggsave("plots/seq3.png", g, width = 9, height = 9)

# full sequence
a = get_ranking()
head(a, 15)

n = get_network(threshold = 2/3)

g = ggnet(n, size = 0, segment.alpha = n %e% "alpha") +
  geom_text(aes(label = network.vertex.names(n), size = n %v% "degree")) + 
  scale_size_continuous(range = c(2, 6)) +
  guides(size = FALSE)

ggsave("plots/full.pdf", g, width = 9, height = 9)
ggsave("plots/full.png", g, width = 9, height = 9)

# have a nice day
