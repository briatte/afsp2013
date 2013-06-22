#
# plot DADVSI and HADOPI networks
#
downloader::source_url("https://raw.github.com/briatte/ggnet/master/ggnet.R",
                       prompt = FALSE)

#
# plot function
#
ggnet.kk <- function(x) {
  net = network::read.paj(x)
  ggnet(net,
        weight = "indegree", top8 = TRUE, # identify top 8 nodes by indegree
        mode = "kamadakawai",             # use Kamada-Kawai placement algorithm
        arrow = TRUE, arrow.size = .2,    # draw small arrows
        subset = 3,                       # subset to nodes with indegree ≥ 3
        names = c("", "Liens entrants"))
}

#
# DADVSI
#
ggnet.kk("data/voson.dadvsi.2008.net")
## ggsave("dadvsi.network.pdf", width = 10, height = 7.5, unit = "in")

#
# HADOPI
#
ggnet.kk("data/voson.hadopi.2011.net")
## ggsave("hadopi.network.pdf", width = 10, height = 7.5, unit = "in")

# rev. 2013-06-16
