
# ----------------
# Numerama: DADVSI
# ----------------

file = "numerama.dadvsi.news.txt"

#
# links
#

extract <- function(x) {
	message(x)
	html = htmlParse(paste0("http://www.numerama.com/magazine/recherche/", x, "/dadvsi/date"))
	html = xpathApply(html, "//table")[[3]]
	html = xpathApply(html, ".//a[contains(@href,'/magazine/') and not(contains(@href, '/recherche/'))]/@href")
	html = gsub("#ac_newscomment", "", html)
	html = unique(html)
}

if(!file.exists(file)) {
	d = lapply(1:36, extract)
	d = unlist(d)
	write.table(d, , row.names = FALSE)
}

#
# scrape
#

d = read.table("numerama.dadvsi.news.txt", stringsAsFactors = FALSE, header = TRUE)

folder = "numerama.dadvsi.corpus"
dir.create(folder)
setwd(folder)

for(i in 1:nrow(d)) {
	f = paste0(gsub("/magazine/|\\.html", "", d$x[i]), ".txt")
	if(!file.exists(f)) {
		message(i, "...")
		e = htmlParse(paste0("http://www.numerama.com", d$x[i]))
		title = xpathApply(e, "//h1", xmlValue)[[1]]
		date = xpathApply(e, "//h1/following::div[1]", xmlValue)
		# for numerama format
		txt = xpathApply(e, "//div[@id='newstext']/*/p", xmlValue)
		# for ratiatum format (articles are sorted ante-chronologically)
		if(i > 431) txt = xpathApply(e, "//div[@id='newstext']/*", xmlValue)
		full = c(title, date, txt)
		full = gsub("[\r|\n|\t]", "", full)
		full = gsub("\"", "'", full)
		full[full != ""]
		write(full, f)
	}
	else {
		message(i, " -- skipping: ", f)
	}
}

d = dir()
kw = keywords(d)

# que c'est laborieux...
allk = as.vector(unlist(kw))
freq = as.data.frame(table(allk), stringAsFactors = FALSE)
sort_df(freq, vars = "Freq")

# I know about ignore.case, thanks
matches = rbind(
	c("EUCD/LQDN", grepl("EUCD|Quadrature|Quadrature du [N|n]et|Christophe Espern|Jérémie Zimmermann|Philippe Aigrain", kw)),
	c("HADOPI/DADVSI/LOPPSI", grepl("DADVSI|DAVDSI|LOPPSI|Lopsi|Hadopi|HADOPI|Haute Autorité|Haute|CPD|Eric Walter|Mireille Imbert-Quaretta|Imbert-Quaretta|Marie-Françoise Marais", kw)),
	c("Rapports", grepl("Zelnik|Denis Olivennes|rapport Olivennes|Olivennes|Lescure|mission Lescure", kw)),
	c("Majors/SPRD", grepl("Sony|Warner|SACEM|Sacem|Sacem|SACD|Société des Auteurs|SCPP|Société [C|c]ivile des [P|p]roducteurs Phonographiques|SPRD|Marc Guez|SNEP|Syndicat National de l'Edition Phonographique|David El Sayegh|Vivendi|Universal|EMI|Pascal Nègre|Pascal Rogard|Bernard Miyet|RIAA|MPAA|Motion Picture Association|Fédération Internationale de l'Industrie Phonographique|IFPI|Lagardère", kw)),
	c("Informatique/Web", grepl("Apple|Microsoft|Spotify|Dailymotion|Google", kw)),
	c("ACTA/SOPA/PIPA", grepl("Acta|ACTA|Sopa|SOPA|Pipa|PIPA", kw)),
	c("DRM", grepl("DRM|MTP|ARMT|Régulation des [M|m]esures [T|t]echniques|Musitelli", kw)),
	c("CSA/CNIL", grepl("CSA|CNIL|Cnil|Conseil [S|s]upérieur de l'[A|a]udiovisuel|Commission [N|n]ationale de l'[I|i]nformatique", kw)),
	c("CC/CE", grepl("Conseil d'[E|É|é|e]tat|Conseil [C|c]onstitutionnel|Jean-Louis Debré", kw)),
	c("Autres opposants", grepl("April|APRIL|Alix Cazenave|Couchet|Bayart|FDN|French Data Network|FSF|FSFE|Free Software Foundation|Stallman|UFC|Dourgnon|Parti [P|p]irate|Maxime Rouquet|ODEBI|Obedi|[S|s]topDRM|[S|s]top [drm|DRM]|EFF|Electronic Frontier Foundation|ISOC|Geist|Spedidam", kw)),
	c("Télécom", grepl("Numericable|Free|Niel|Orange|Stéphane Richard|ARCEP|Arcep|France [Télécom|Telecom]|Bouygues|SFR", kw)),
	c("UE", grepl("CJUE|Parlement européen|Bruxelles|Paquet Télécom|IPRED|Commission [E|e]uropéenne|Reding|Bono|Trautmann|Cohn-Bendit|Lipietz|Boumedie[n|nn]e-Thiery", kw)),
	c("Président", grepl("Président|Élysée|Chirac|Sarkozy|Hollande|Villepin|Fillon|Ayrault", kw)),
	c("Ministère", grepl("CSPLA|Donnedieu|RDDV|Albanel|Mitterrand|Filipetti|Valois|Eric Walter|Tardieu", kw)),
	c("Gouvernement", grepl("Hortefeux|Alliot-Marie|Eric Besson|NKM|Nathalie Kosciusko-Morizet|Pellerin|Chatel", kw)),
	c("Parlement", grepl("Parlement|Assemblée|Assemblée [N|n]ationale|Sénat|CMP|Commission Mixte Paritaire|Commission [des A|des a|A|a]ffaires [C|c]ulturelles", kw)),
	c("Droite", grepl("UMP|Vanneste|Lefebvre|Riester|Marland-Militello|Copé|Tardy|Dionis|Toubon|Raudière|Bayrou|Marland-Militello|Dupont-Aignan|Panafieu|Retailleau|Boutin|Suguenot|Mariton|Lepage|Carayon", kw)),
	c("Gauche", grepl("PS|Parti|Parti [S|s]ocialiste|Bloche|Christian [PAUL|Paul]|Aubry|Vanneste|Mathus|Lang|Brard|Thiollière|Billard|Blandin|Joly|Rebsamen|Tasca|Montebourg|Lagauche|Badinter|Hidalgo", kw))
	)

# TV (Canal, TF <-- Jérôme Bourreau-Guggenheim, France Télévisions)
# TMG, Michel Riguidel, UFC-Que Choisir, Conseil Constitutionnel et CE, ARJEL
# OMPI
# CNC, Midem, Miyet
# MoDem, Mouvement Démocrate
# Conseil National du Numérique, CPI (Création Public Internet/Code de la Propriété Intellectuelle), PUR
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
