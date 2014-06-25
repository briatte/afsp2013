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

* for each entity in each article of the corpus, the propensity of occurrence is its frequency divided by its overall frequency in the corpus
* for each dyad of entities in each article, the propensity of co-occurrence is the product of their propensity of occurrence
* for each dyad, the overall weighted propensity of co-occurrence is the sum of its weights over all articles in the corpus

![WPC_{ij} = \sum_1^c \frac{ p_i }{ \sum_1^c p_i } \cdot \frac{ p_j }{ \sum_1^c p_j }](eq.png)

where _c_ is a corpus of 1, 2, ..., _c_ articles, and _p_ the frequency of each entity.