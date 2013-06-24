![](figure.png)

# This repository contains the replication files for a [conference paper][paper].

The figures that it replicates are located in Appendix A--B of the paper:

* `google-trends.R` reproduces Appendix A1, using data collected with [Google Trends][gtrends] in May 2013
* `scrapers.R` reproduces Appendix A2, using data [scraped][scrapers] from Numerama.com and ZDNet.fr in May 2013
* `voson.R` reproduces Appendix B1 (shown above) and B2, using data collected with [VOSON][voson] in 2008 and 2011

See also the [`flegscrap`][flegscrap] and [`ggnet`][ggnet] repositories for related code.

[paper]: https://github.com/briatte/afsp2013/raw/master/paper.pdf
[scrapers]: blob/master/data/scrape.all.R
[gtrends]: https://www.google.com/trends/
[voson]: http://voson.anu.edu.au/
[flegscrap]: https://github.com/briatte/flegscrap/
[ggnet]: https://github.com/briatte/ggnet/

## Work in progress.

Thanks to [Bram][bram] and [Stef][stef] for letting me know about XPath syntax, and apologies for using R where Python would certainly do a better scraping job.

[bram]: https://github.com/Psycojoker
[stef]: https://github.com/stef

> Last revised 2013-06-24
