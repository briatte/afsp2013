
#
#

require(ggplot2)
require(RCurl)
require(reshape)
require(stringr)
require(XML)

#
#

setwd("~/Desktop")

# -------------
# extract links
# -------------

extract <- function(x) {
	message(x)
	html = htmlParse(paste0("http://www.numerama.com/magazine/recherche/", x, "/hadopi/date"))
	html = xpathApply(html, "//table")[[3]]
	html = xpathApply(html, ".//a[contains(@href,'/magazine/') and not(contains(@href, '/recherche/'))]/@href")
	html = gsub("#ac_newscomment", "", html)
	html = unique(html)
}

d = lapply(1:125, extract)
d = unlist(d)

#
#

write.table(d, "numerama.hadopi.news.txt", row.names = FALSE)

# ------
# scrape
# ------

d = read.table("numerama.hadopi.news.txt", stringsAsFactors = FALSE, header = TRUE)

folder = "numerama.hadopi.corpus"
dir.create(folder)
setwd(folder)

for(i in 1:nrow(d)) {
	f = paste0(gsub("/magazine/|\\.html", "", d$x[i]), ".txt")
	if(!file.exists(f)) {
		message(i, "...")
		e = htmlParse(paste0("http://www.numerama.com", d$x[i]))
		title = xpathApply(e, "//h1", xmlValue)[[1]]
		date = xpathApply(e, "//h1/following::div[1]", xmlValue)
		txt = xpathApply(e, "//div[@id='newstext']/*/p", xmlValue)
		full = c(title, date, txt)
		full = gsub("[\r|\n|\t]", "", full)
		full = gsub("\"", "'", full)
		full[full != ""]
		write(full, f)
	}
	else {
		message("skipping: ", f)
	}
}

# ---------
# stopwords
# ---------

sw <- getURL("http://members.unine.ch/jacques.savoy/clef/frenchST.txt")
sw <- readLines(textConnection(sw))

# --------
# keywords
# --------

setwd("~/Desktop/numerama.hadopi.corpus/")

d = dir()

kw = sapply(d, function(x) {
  r <- "((rapport|commission|mission)*\\s*[A-ZÉ]+[a-zâäëéèîïöôç-]*\\s*(-|de la|à|d'|de l'|du|des)*)+"
  k <- unique(str_trim(unlist(str_extract_all(readLines(x)[-1:-2], r))))
  s <- which(tolower(k) %in% sw)
  k[-s]
})

kw <- gsub(" à|La |Les| En | des| d'", "", kw)

# que c'est laborieux...
allk = as.vector(unlist(kw))
freq = as.data.frame(table(allk), stringAsFactors = FALSE)
sort_df(freq, vars = "Freq")

matches = rbind(
	c("LQDN", grepl("Quadrature|Quadrature du [N|n]et|Jérémie Zimmermann|Philippe Aigrain", kw)),
	c("HADOPI", grepl("Hadopi|HADOPI|Haute Autorité|Haute|CPD|Eric Walter|Mireille Imbert-Quaretta|Imbert-Quaretta|Marie-Françoise Marais", kw)),
	c("Rapports", grepl("Zelnik|Denis Olivennes|Olivennes|Lescure|mission Lescure", kw)),
	c("Majors/SPRD", grepl("Sony|Warner|SACEM|Sacem|Sacem|SACD|Société des Auteurs|SCPP|Société Civile des Producteurs Phonographiques|SPRD|Marc Guez|SNEP|Syndicat National de l'Edition Phonographique|David El Sayegh|Vivendi|Universal| EMI|Pascal Nègre|Pascal Rogard|Bernard Miyet", kw)),
	c("Informatique/Web", grepl("Apple|Microsoft|Spotify|Dailymotion", kw)),
	c("ACTA/SOPA/PIPA", grepl("ACTA|SOPA|PIPA", kw)),
	c("DRM", grepl("DRM|ARMT|Régulation des Mesures Techniques", kw)),
	c("Télécom", grepl("Numericable|Free|Xavier Niel|Orange|ARCEP|France Télécom|Bouygues", kw)),
	c("UE", grepl("CJUE|Parlement européen|Bruxelles|Paquet Télécom|IPRED|Commission Européenne|Guy Bono|Catherine Trautmann|Daniel Cohn-Bendit|Viviane Reding", kw)),
	c("Président", grepl("Président|Élysée|Sarkozy|Hollande|François Fillon|Jean-Marc Ayrault", kw)),
	c("Ministère", grepl("CSPLA|Donnedieu|Albanel|Mitterrand|Filipetti|Valois|Eric Walter|Fleur Pellerin|Christophe Tardieu", kw)),
	c("Gouvernement", grepl("Brice Hortefeux|Michèle Alliot-Marie|Eric Besson|Nathalie Kosciusko-Morizet", kw)),
	c("Parlement", grepl("Parlement|Assemblée|Assemblée [N|n]ationale|Sénat|CMP|Commission Mixte Paritaire", kw)),
	c("Droite", grepl("UMP|Frédéric Lefebvre|Fran[c|ck] Riester|Muriel Marland-Militello|Jean-François Copé|Lionel Tardy|Jean Dionis du Séjour|Jacques Toubon|Laure de la Raudière|François Bayrou|NKM|Muriel Marland-Militello", kw)),
	c("Gauche", grepl("PS|Parti|Parti [S|s]ocialiste|Patrick Bloche|Christian Paul|Martine Aubry|Christian Vanneste|Didier Mathus|Jack Lang|Jean-Pierre Brard|François Bayrou|Michel Thiollière|Martine Billard|Eva Joly|", kw))
	)

# TV (Canal, TF <-- Jérôme Bourreau-Guggenheim, France Télévisions)
# CNIL ("Commission Nationale de l'Informatique"), CSA, Conseil Supérieur de l'Audiovisue, TMG, Michel Riguidel, UFC-Que Choisir, Conseil Constitutionnel et CE, ARJEL
# DVD: Nicolas Dupont-Aignan
# Benjamin Bayart, French Data Network
# Parti pirate, Maxime Rouquet
# MoDem
# Conseil National du Numérique, CPI (?)
# LOPPSI
# ... et pas fait < 10

# -----
# dates
# -----

dates = sapply(d, function(x) {
  gsub("(.)+ publié le \\w+\\s| à (.*)", "", readLines(x)[2])
})
dates = gsub(" Juillet ", "-07-", dates)

m = as.data.frame(matches)
names(m)[1] = "id"
names(m)[-1] = dates
m = melt(m, id = "id", variable = "date")
m$date = tolower(as.character(m$date))
m$date = parse_date_time(m$date, "%d %m %Y", locale = "fr_FR")
qplot(data = m, y = id, x = date, fill = value, alpha = I(.75), geom = "tile") +
	scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "white")) + 
	theme_bw() + theme(panel.grid = element_blank(), legend.position = "none") + 
	labs(y = NULL, x = NULL)
# or use y = reorder(id, as.numeric(value), sum)
