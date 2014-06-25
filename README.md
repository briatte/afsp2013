Code related to a [working paper][paper] that was [first presented][draft] at the [AFSP](http://www.afsp.msh-paris.fr/) Annual Meeting in Paris, 2013. See the [HOWTO](#howto) below for a technical summary of the data presented in Section 1 of the [paper][paper] and in the [appendix][appendix].

> * June 2014 – Updated [working paper][paper]
	* Added [new appendix][appendix]
	* Added two media scrapers
	* Updated [Google Trends][gtrends] data
* June 2013 – First release
	* [First draft][draft]
	* [Conference slides][slides]

[gtrends]: https://www.google.com/trends/
[draft]: https://github.com/briatte/afsp2013/raw/master/afsp2013/draft.pdf
[slides]: https://github.com/briatte/afsp2013/raw/master/afsp2013/slides.pdf
[paper]: http://goo.gl/C8kW1s
[appendix]: http://goo.gl/gaKlFD

# DATA

The scraper currently collects slightly over 5,000 articles from 

* ecrans.fr, including articles from liberation.fr
* lesechos.fr
* numerama.com, including articles from ratiatium.com (defunct)
* zdnet.fr

![](plots/counts.png)

# HOWTO

The entry point is `make.r`:

* `get_articles` will scrape the news sources (adjust page counters to current website search results to update the data)
* `get_corpus` will extract all entities and list the top 10% most common ones (adjust `threshold` to anything between `0` and `1`)
* `get_ranking` will export the top 15 central nodes of the co-occurrence network to the `tables` folder, in Markdown format
* `get_network` returns the co-occurrence network, optionally trimmed to its top weighted edges (adjust `threshold` to anything between `0` and `1`)

## Summary tables

* `corpus.terms.csv` – a list of all entities, ordered by their raw counts
* `corpus.freqs.csv` – a list of weighted entities in each article
* `corpus.edges.csv` – a list of weighted network edges

## Weighting scheme

* For each entity in each article of the corpus, the propensity of occurrence is its frequency divided by its overall frequency in the corpus
* For each dyad of entities in each article, the propensity of co-occurrence is the product of their propensity of occurrence
* For each dyad, the overall weighted propensity of co-occurrence is the sum of its weights over all articles in the corpus

The tie weight used to compute the degree of each node in the co-occurrence network is therefore

![WPC_{ij} = \sum_1^c \frac{ p_i }{ \sum_1^c p_i } \cdot \frac{ p_j }{ \sum_1^c p_j }](http://i.imgur.com/yJwXITO.png)

where _c_ is a corpus of 1, 2, ..., _c_ articles, and _p_ the frequency of each entity.

The weighted degree formula is by [Tore Opsahl](http://toreopsahl.com/tnet/weighted-networks/node-centrality/) and uses an alpha parameter of 1.
