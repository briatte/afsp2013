This repository contains the replication material for a [conference paper][paper] presented at the [AFSP](http://www.afsp.msh-paris.fr/) Congress 2013 in Paris, France.

The figures that it replicates are located in Appendix A and B of the paper:

* `google-trends.R` reproduces App. A1, using data collected with [Google Trends][gtrends] in May 2013
* `scrapers.R` reproduces App. A2, using data [scraped][scrapers] from Numerama.com and ZDNet.fr in May 2013
* `voson.R` reproduces App. B1 (shown below) and B2, using data collected with [VOSON][voson] in 2008 and 2011

![](figure.png)

The network plots use the [`ggnet`][ggnet] function.

[paper]: https://github.com/briatte/afsp2013/raw/master/paper.pdf
[scrapers]: blob/master/data/scrape.all.R
[gtrends]: https://www.google.com/trends/
[voson]: http://voson.anu.edu.au/
[flegscrap]: https://github.com/briatte/flegscrap/
[ggnet]: https://github.com/briatte/ggnet/

# Credits

Thanks to [Bram][bram] and [Stef][stef] for letting me know about XPath syntax, and apologies for using R where Python would certainly do a better scraping job.

[bram]: https://github.com/Psycojoker
[stef]: https://github.com/stef

# Contact

Please feel free to send comments at [f.briatte@ed.ac.uk](f.briatte@ed.ac.uk).

> First release: May 2013.  
> Last revision: June 2013.
