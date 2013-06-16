
# Classification of Numerama news items, 2005-2013
# N = 2920 items with keywords "DADVSI" or "HADOPI"

require(ggplot2)
require(lubridate)
require(RCurl)
require(reshape)
require(scales)
require(stringr)

rm(list = ls())
setwd("~/Desktop/numerama-scraper/numerama.corpus")
d = dir()

# ---------
# stopwords
# ---------

sw <- getURL("http://members.unine.ch/jacques.savoy/clef/frenchST.txt")
sw <- readLines(textConnection(sw))

# --------
# keywords
# --------

kw = sapply(d, function(x) {
  t <- readLines(x)[-2]
  t <- gsub("\\s{2,}", " ", t, perl = TRUE) # trim double spaces
  r <- "((rapport|commission|mission)*\\s*[A-ZÉ]+[a-zâäàêëéèîïôöûüç-]*\\s*(-|à |de |de la |d'|de l'|du |des )*)+"
  k <- unique(str_trim(unlist(str_extract_all(t, r))))
  s <- which(tolower(k) %in% c(sw))
  k[-s]
})

# prefixes and suffixes
r = "^Culture |^UMP |^Mme |^Madame |^Me |^La |^Le |^Les |^Dans |^En |^Ce |^Lors de |^Lors de l'|^Lors de la|^Lors du |^Lors des |^Selon |^Selon le |^Selon la |^Selon les |^Un |^Comment |^Peut-être |^Mais |^Malgré |^Même |^Merci |^Rendez-nous |^Sur |^du |^Or | à$| de$| de la$| d'$| de l'$| du$| des$| à des$| -$"
kw <- lapply(kw, function(x) gsub(r, "", x))

# list
kwds = as.vector(unlist(kw))

# adverbs and such
kwds <- kwds[-which(grepl("mment$|vant$|hier$|nné[e]*$|^d'$", kwds))]
kwds <- kwds[-which(grepl("-être$|-on$|-nous$|-il[s]*$|-elle[s]*$", kwds))]

# final manual fixes
s <- which(tolower(kwds) %in% tolower(c(sw, "Puis", "Pourtant", "Contrairement", "Visiblement", "Malheureusement", "Aujourd'", "Est-ce", "Alors", "Interrogé", "Interrogée", "Toutefois", "Ca", "de la","France", "Reste", "Or", "Mise", "Enfin", "Déjà", "Face", "Lorsqu", "Ensuite", "Finalement", "Contacté", "Contactée", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Mieux", "Parce", "Autant", "Grâce", "Preuve", "Fin", "Hier", "Souvent", "Toujours", "Jamais", "Seul", "Seuls", "Seule", "Suite", "Actuellement", "Début", "MAJ", "MàJ", "Officiellement", "Difficile", "Au-delà", "Histoire", "Puisqu", "Généralement", "Heureusement", "Notons", "Rappelons", "Problème", "Signe", "ElementsByTagName", "ElementById", "Element", "PDF", "Probablement", "Mise", "Vient", "Ici", "Vraiment", "Bref", "Impossible")))
kwds <- kwds[-s]

# frequency table
freq <- as.data.frame(table(kwds), stringAsFactors = FALSE)
freq <- freq[order(-freq$Freq), ]
names(freq) <- c("keyword", "frequency")
rownames(freq) <- NULL

# save
sink("../numerama.corpus.keywords.txt")
freq
sink()

# -------
# matches
# -------

matches = rbind(
	c("Opposants : EUCD/LQDN", 
	grepl("EUCD|Quadrature|Quadrature du [N|n]et|Christophe Espern|Jérémie Zimmermann|Philippe Aigrain", kw)),
	c("HADOPI/DADVSI/LOPPSI", 
	grepl("DADVSI|Dadvsi|DADVSi|DAVDSI|LOPPSI|Lopsi|Hadopi|HADOPI|Hadop|Hadoi|Haute Autorité|Haute|CPD|Eric Walter|Mireille Imbert-Quaretta|Imbert-Quaretta|Marie-Françoise Marais", kw)),
	c("Conseils/Rapports", 
	grepl("CSPLA|Conseil Supérieur de la Propriété Littéraire|Pierre Sirinelli|CNN|CNNum|Conseil National du Numérique|Benoît Tabaka|FDI|Forum des [D|d]roits sur Internet|Falque-Pierrotin|Zelnik|Denis Olivennes|rapport Olivennes|Olivennes|Lescure|mission Lescure", kw)),
	c("Industrie : Culture", 
	grepl("ALPA|Piraterie Audiovisuelle|Alpa|SDRM|Sony|Warner|UPFI|Union [P|p]roducteurs [P|p]honographiques [F|f]rançais [I|i]ndépendants|Union [P|p]roducteurs [P|p]honographiques [I|i]ndépendants|SACEM|Sacem|Sacem|SACD|Société [A|a]uteurs|[C|c]ompositeurs [D|d]ramatiques|Société des [A|a]uteurs|SPPF|Phonogrammes|SCPP|Société [C|c]ivile [P|p]roducteurs|Société [C|c]ivile des [P|p]roducteurs [P|p]honographiques|SPRD|Marc Guez|SNEP|Syndicat [N|n]ational de l'[É|E|é]dition [P|p]honographique|David El Sayegh|Vivendi|Universal|EMI|Pascal Nègre|Pascal Rogard|Bernard Miyet|RIAA|MPAA|Motion Picture Association|Fédération [I|i]nternationale de l'[I|i]ndustrie [P|p]honographique|IFPI|British Phonographic Industry|BPI|Lagardère|EMI|Fnac|FNAC|International Intellectual Property Alliance|IIPA|Midem|ForumAvignon|Miyet", kw)),
	c("Industrie : Info/Web", 
	grepl("Apple|Microsoft|Spotify|Dailymotion|Google|Facebook|Amazon|Software Alliance|BSA", kw)),
	c("ACTA/SOPA/PIPA", 
	grepl("Acta|ACTA|Sopa|SOPA|Pipa|PIPA", kw)),
	c("DRM", 
	grepl("DRM|MTP|ARMT|Régulation des [M|m]esures [T|t]echniques|Musitelli", kw)),
	c("CNIL/CSA", 
	grepl("CSA|CNIL|Cnil|Türk|Conseil [S|s]upérieur de l'[A|a]udiovisuel|Commission [N|n]ationale de l'[I|i]nformatique", kw)),
	c("P2P/DDL", 
	grepl("[P]eer [T|t]o [P|p]eer|[P]eer-[T|t]o-[P|p]eer|p2p|P2P|[T|t]orrent|The Pirate Bay|TPB|DDL|Mega[u|U]pload|Mule|Emule|Donkey|Lime[W|w]ire|Ka[Z|z]a[A|a]", kw)),
	c("Opposants : Autres", 
	grepl("April|APRIL|Alix Cazenave|Couchet|Bayart|FDN|French Data Network|FSF|FSFE|Free Software Foundation|Stallman|UFC|Union Consommateurs|Dourgnon|CLCV|Parti [P|p]irate|Maxime Rouquet|ODEBI|Obedi|[S|s]topDRM|[S|s]top [drm|DRM]|EFF|Electronic Frontier Fo[u]*ndation|ISOC|Geist|Spedidam|Audionautes|Public-Artistes|Isoc|ISOC|Framasoft", kw)),
	c("Industrie : Télécom", 
	grepl("Numericable|Free|Niel|Orange|Stéphane Richard|ARCEP|Arcep|France [Télécom|Telecom]|Bouygues|SFR", kw)),
	c("UE/IPRED", grepl("CJCE|[J|j]ustice [E|e]uropéenne|Justice de l'Union|CEDH|[E|e]uropéenne [D|d]roits|CJUE|Parlement [E|e]uropéen|Bruxelles|Paquet Télécom|IPRED|Commission [E|e]uropéenne|Reding|Gallo|Bono|Trautmann|Cohn-Bendit|Lipietz|Boumedie[n|nn]e-Thiery", kw)),
	c("Exécutif : PR/PM/GOUV", 
	grepl("Président|[É|E|é]lysée|Chirac|Sarkozy|Hollande|Matignon|Villepin|Fillon|Ayrault|Hortefeux|Alliot-Marie|Eric Besson|NKM|Nathalie Kosciusko-Morizet|Pellerin|Chatel", kw)),
	c("Exécutif : Ministère", 
	grepl("Donnedieu|RDDV|Albanel|Mitterrand|Filipetti|Valois|Eric Walter|Tardieu", kw)),
	c("Parlement", 
	grepl("Parlement|Assemblée|Assemblée [N|n]ationale|Sénat|CMP|Commission Mixte Paritaire|Commission [des A|des a|A|a]ffaires [C|c]ulturelles", kw)),
	c("Classe politique : Droite", 
	grepl("UMP", kw) | grepl("Vanneste|Lefebvre|Lefèbvre|Lefebre|Riester|Marland-Militello|Copé|Tardy|Dionis|Toubon|Lancar|Poisson|Morano|Gosselin|Raudière|Bayrou|Marland-Militello|Dupont-Aignan|Panafieu|Retailleau|Boutin|Suguenot|Mariton|Lepage|Carayon|Gaino|Guaino|Karoutchi", kw, ignore.case = TRUE)),
	c("Classe politique : Gauche", 
	grepl("PS|Parti [S|s]ocialiste|Socialistes|Groupe [C|c]ommuniste|Verts|cologie|PCF|Parti de [G|g]auche|Front de [G|g]auche", kw) | grepl("Bloche|Christian Paul|Ségolène Royal|Aubry|Mathus|Lang|Brard|Thiollière|Billard|Blandin|Pinel|Lamberts|Poursinoff|Joly|Rebsamen|Tasca|Montebourg|Lagauche|Badinter|Hidalgo", kw, ignore.case = TRUE))
	)

# -----
# dates
# -----

dates = sapply(d, function(x) {
  gsub("(.)+ publié le \\w+\\s| à (.*)", "", readLines(x)[2])
})
dates = gsub(" Juillet ", "-07-", dates)

# ----
# plot
# ----

# collate
m = as.data.frame(matches)
names(m)[1] = "id"
names(m)[-1] = dates

# parse
m = melt(m, id = "id", variable = "date")
m$date = tolower(as.character(m$date))
m$date = parse_date_time(m$date, "%d %m %Y", locale = "fr_FR")
table(substr(m$date, 0, 4))

# graph with days
g <- qplot(data = m, y = id, x = date, fill = value, alpha = I(.75), geom = "tile") +
	scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "white")) + 
	theme_bw(16) + theme(panel.grid = element_blank(), legend.position = "none") + 
	labs(y = NULL, x = NULL)
g

# graph by year
h <- qplot(data = m, y = id, x = year(date), fill = value, alpha = I(.75), geom = "tile") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "white")) + 
  theme_bw(16) + theme(panel.grid = element_blank(), legend.position = "none") + 
  labs(y = NULL, x = NULL)
h

ym <- with(m, ym <- data.frame(
  t = substr(m$date, 0, 7),
  c = id,
  n = as.numeric(value) - 1))

z = ddply(ym, .(t, c), summarize, N = sum(n))
summary(z$N)

nl <- labs(y = NULL, x = NULL)
th <- theme_bw(12) + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank())

# graph by year-month
p <- qplot(data = z, y = c, x = t, fill = N, geom = "tile") +
  scale_fill_gradient(low = "grey20", high = "yellow") +
  nl + th + theme(axis.text.x = element_blank())
p + theme(axis.ticks.x = element_blank())

z$t2 <- as.Date(paste(z$t, "01"), "%Y-%m %d")

# point graph
q <- qplot(data = z, y = c, x = t2, size = N, group = c, geom = "point") +
  scale_x_date(labels = date_format("%Y")) +
  scale_size_area("Items", max_size = 5) + nl + th
q
ggsave("../numerama.mediagraph.pdf", width = 10, height = 7.5, unit = "in")

write.csv(z, "../numerama.corpus.classes.txt")
# 2013-05-20
