# --------
# packages
# --------

require(ggplot2)
require(RCurl)
require(reshape)
require(stringr)
require(XML)

# ------
# folder
# ------

setwd("data")

# ---------
# stopwords
# ---------

sw <- getURL("http://members.unine.ch/jacques.savoy/clef/frenchST.txt")
sw <- readLines(textConnection(sw))

# --------
# keywords
# --------

keywords <- function(d) {
  #
  # extract keywords
  #
  kw = sapply(d, function(x) {
    t <- readLines(x)[-2]
    t <- gsub("\\s{2,}", " ", t, perl = TRUE) # trim double spaces
    r <- "((rapport|commission|mission)*\\s*[A-ZÉ]+[a-zâäàêëéèîïôöûüç-]*\\s*(-|à |de |de la |d'|de l'|du |des )*)+"
    k <- unique(str_trim(unlist(str_extract_all(t, r))))
    s <- which(tolower(k) %in% c(sw))
    k[-s]
  })
  #
  # clean prefixes and suffixes
  #
  r = "^Culture |^UMP |^Mme |^Madame |^Me |^La |^Le |^Les |^Dans |^En |^Ce |^Lors de |^Lors de l'|^Lors de la|^Lors du |^Lors des |^Selon |^Selon le |^Selon la |^Selon les |^Un |^Comment |^Peut-être |^Mais |^Malgré |^Même |^Merci |^Rendez-nous |^Sur |^du |^Or | à$| de$| de la$| d'$| de l'$| du$| des$| à des$| -$"
  kw <- lapply(kw, function(x) gsub(r, "", x))
  #
  # return keywords
  #
  allk = as.vector(unlist(kw))
  freq = as.data.frame(table(allk), stringAsFactors = FALSE)
  sort_df(freq, vars = "Freq")
  #
  #
  #
  return(kw)
}

source("build.numerama.corpus.R")
# 2. Numerama: HADOPI
# 3. ZDNet: DADVSI
# 4. ZDNet: HADOPI
