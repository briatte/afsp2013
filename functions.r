get_articles <- function(source, keyword, pages, step = 10, sw = sw) {
  
  # -----
  # files
  # -----

  dir.create("data", showWarnings = FALSE)
  file = paste0("data/", source, ".", keyword, ".index.txt")
  folder = paste0(source, ".corpus")
  
  dir.create(paste0("data/", folder), showWarnings = FALSE)
  
  # -----
  # links
  # -----
  
  extract = function(x) {
    
    cat(".")
    
    if(source == "numerama") {
      
      html = htmlParse(paste0("http://www.numerama.com/magazine/recherche/", x, "/", keyword, "/date"))
      html = xpathSApply(html, "//ul[@id='list_news']//a[contains(@href,'/magazine/')]/@href")
      html = html[ str_detect(html, "magazine/\\d+") ]
      html = paste0("http://www.numerama.com", html)
      
    } else if(source == "zdnet") {
      
      html = htmlParse(paste0("http://www.zdnet.fr/rechercher/", keyword, ".htm?p=", x))
      html = xpathSApply(html, "//section[contains(@class, 'river')]//a[contains(@href,'/actualites/')]/@href")
      html = str_replace(html, ".htm(.*)", ".htm")
      html = html[ str_detect(html, ".htm$") ]
      
    } else if(source == "ecrans") {
      
      html = htmlParse(paste0("http://ecrans.liberation.fr/recherche/?page=", x, "&sort=-publication_date_time&period_start_day=0&period_start_month=0&period_end_year=0&period_start_year=0&period_end_day=0&editorial_source=&period=forever&q=", keyword, "&period_end_month=0&paper_channel="))
      html = xpathSApply(html, "//section[@class='timeline']//a/@href")
      html = str_replace(html, "^/", "http://ecrans.liberation.fr/")
      
    } else if(source == "echos") {
      
      html = htmlParse(paste0("http://recherche.lesechos.fr/?exec=1&texte=", 
                              keyword, "&dans=touttexte&date1=&date2=&page=", x))
      html = xpathSApply(html, "//div[@class='bloc-resultat-recherche']//a/@href")
      html = str_replace(html, ".php(.*)", ".php")
      html = str_replace(html, "(.*)(.php|.htm)(.*)", "\\1\\2")
      html = html[ str_length(html) > 1 ]
      
    } else if(source == "lefigaro") {
    
      html = htmlParse(paste0("http://recherche.lefigaro.fr/recherche/recherche.php?ecrivez=", 
                              keyword, "&page=articles&next=", 1 + 20 * (x - 1)))
      html = xpathSApply(html, "//h3[@class='entry-title']/a/@href")
      html = str_replace(html, "^/", "http://recherche.lefigaro.fr/")
      
    } else if(source == "lemonde") { # dadvsi = 9, hadopi = 70
      
      html = htmlParse(paste0("http://www.lemonde.fr/recherche/?keywords=", keyword,
                              "&qt=recherche_globale&page_num=", x))

      # timestamp not always present on archive page, save it in front of link
      time = xpathSApply(html, "//article[contains(@class, 'mgt8')]//span[contains(@class, 'signature')]", xmlValue)
      time = str_extract(time, "[0-9]{1,2} [a-zéû]+ [0-9]{4}")
      time = as.character(parse_date_time(time, "%d %m %Y", locale = "fr_FR"))
      
      html = xpathSApply(html, "//article[contains(@class, 'mgt8')]//h3/a/@href")
      html = str_replace(html, "^/", "http://www.lemonde.fr/")
      html = paste0(time, ":", html)
      html = html[ str_detect(html, "lemonde.fr") ]
      
    }
    
    html = unique(html)
    
  }
    
  if(!file.exists(file)) {
    
    cat("Scraping index", pages, "pages")
    d = lapply(pages:1, extract)
    
    cat("done.\n")
    write(unlist(d), file)
    
  }
      
  # --------
  # articles
  # --------
  
  d = read.table(file, stringsAsFactors = FALSE, header = FALSE)[, 1]
  
  if(source == "zdnet")
    d = d[ !grepl("q.htm$", d) ]
  
  if(source == "ecrans")
    d = d[ !grepl("libelyon|assemblee.blogs", d) ]
  
  if(source == "echos")
    d = d[ !grepl("(business|blogs|bourse|videos).lesechos.fr|connaissancedesarts.com|investir.fr|/dossier/", d) ]
  
  n = length(d)
  cat(folder, keyword, ":", pages, "pages", n, "articles\n")
  
  for(i in 1:n) {
    
    if(source == "numerama") {
      
      f = gsub("http://www.numerama.com/magazine/|\\.html", "", d[i])
      
    } else if(source == "zdnet") {
      
      f = gsub("http://www.zdnet.fr/actualites/|\\.htm", "", d[i])
      
    } else if(source == "ecrans") {
      
      f = gsub("(.*)(\\d{4})/(\\d{2})/(\\d{2})/(.*)", "\\2-\\3-\\4-\\5", d[i])
      
      if(grepl("^http", f))
        f = gsub("(.*)(\\d{4})/(\\d{2})/(.*).html", "\\2-\\3-\\4", d[i])
      
    } else if(source == "echos") {
      
      f = gsub("(.*)(\\d{2})/(\\d{2})/(\\d{4})/(.*)(.htm|.php)", "\\2-\\3-\\4-\\5", d[i])
      
    } else if(source == "lefigaro") {
      
      f = d[i]
    
    } else if(source == "lemonde") {

      if(grepl("acheter.cgi", d[i]))
        f = gsub("(.*)objet_id=(\\d+)(.*)", "archives_\\2", d[i])
      else
        f = substring(gsub("\\?xtmc=(.*)", "", d[i]), 12)

      # append date
      f = paste0(substr(d[i], 1, 10), "-", f)

    }
    
    f = gsub("/", "-", gsub("http://(www.)?|.htm(l)?$|.php$", "", f))
    f = paste0("data/", folder, "/", f, ".txt")

    if(!file.exists(f)) {
      
      if(!i %% step)
        cat("Scraping article", i, "out of", n, "...\n")
      
      if(source == "numerama") {
        
        e = htmlParse(d[i])
        title = xpathApply(e, "//h1", xmlValue)
        date = xpathApply(e, "//span[@class='datepublish']", xmlValue)
        txt = xpathApply(e, "//span[@id='intelliTXT']", xmlValue)
        txt = c(xpathApply(e, "//h2[@class='intro']", xmlValue), txt)
        
      } else if(source == "zdnet") {
        
        e = htmlParse(d[i])
        title = xpathApply(e, "//h1", xmlValue)[[1]]
        date = xpathApply(e, "//time", xmlValue)[[1]]
        txt = xpathApply(e, "//div[@class='storyBody']", xmlValue)
        
      } else if(source == "ecrans") {
        
        date = str_extract(d[i], "\\d{4}/\\d{2}/\\d{2}")
        date = gsub("/", "-", date)
        e = try(htmlParse(d[i]))
        
        if("try-error" %in% class(e)) {
          
          warning("Fix:\n", d[i])
          title = gsub("-|_|\\d", " ", gsub("(.*)(\\d{4})/(\\d{2})/(\\d{2})/(.*)", "\\5", d[i]))
          txt = ""
          
        } else {
          
          title = xpathApply(e, "//title", xmlValue)[[1]]
          title = gsub(" - Libération", "", title)
          txt = xpathApply(e, "//div[@id='article-body']//p", xmlValue)
          txt = c(xpathApply(e, "//meta[@name='news_keywords']/@content"), txt)
          txt = txt[ !grepl("Lire les réactions", txt) ]
          
        }
        
      } else if(source == "echos") {
        
        e = try(htmlParse(d[i]))
        
        if("try-error" %in% class(e)) {
          
          warning("Fix:\n", d[i])
          title = gsub("http://|www|\\.|/|lesechos.fr|-|_|\\d|.htm", " ", d[i])
          date = str_extract(d[i], "\\d{4}/\\d{2}/\\d{2}")
          txt = ""
          
        } else {
          
          title = xpathApply(e, "//title", xmlValue)
          title = gsub("Les Echos - | - Archives", "", title) # archives only
          date = xpathApply(e, "//meta[@itemprop='dateCreated']/@content")
          txt = xpathApply(e, "//div[@class='contenu_article']//p[@itemprop='articleBody']", xmlValue)
          txt = c(xpathApply(e, "//div[@class='contenu_article']//h2[@itemprop='articleBody']", xmlValue), txt)
          txt = c(xpathApply(e, "//meta[@name='news_keywords']/@value"), txt)
          
        }
         
      } else if(source == "lefigaro") {

        e = htmlParse(d[i], encoding = "UTF-8")
      
        if(grepl("recherche.lefigaro.fr", d[i])) {
          
          title = xpathApply(e, "//div[@id='article']/h1", xmlValue)
          date = xpathApply(e, "//div[@id='article']/span[@class='sign']", xmlValue)
          date = gsub("(.*)([0-9]{2})/([0-9]{2})/([0-9]{4})(.*)", "\\4-\\3-\\2", date)
          txt = xpathApply(e, "//div[@id='article']/div[@class='texte']", xmlValue)
          txt = c(xpathApply(e, "//div[@id='article']/h2", xmlValue), txt)
          txt = txt[ !grepl("pointer: 0x", txt) ]
          
        } else if(grepl("www.lefigaro.fr", d[i])) {
         
          title = xpathApply(e, "//title", xmlValue)
          date = xpathApply(e, "//time[@itemprop='datePublished']/@datetime")
          date = substr(date, 1, 10)
          txt = xpathApply(e, "//div[@itemprop='articleBody']", xmlValue)
          txt = c(xpathApply(e, "//meta[@name='description']/@content"), txt)
          txt = unlist(txt[ str_length(unlist(txt)) > 1 ])

        }
        
      } else if(source == "lemonde") {
        
        date = substr(d[i], 1, 10)
        e = try(htmlParse(substring(d[i], 12), encoding = "UTF-8"))
        
        if("try-error" %in% class(e)) {
          
          warning("Fix:\n", d[i])
          title = ""
          txt = ""
          
        } else {
          
          if(grepl("acheter.cgi", d[i])) {
            
            # archive pages
            title = xpathApply(e, "//div[@class='pad']/h2", xmlValue)
            txt = xpathApply(e, "//div[@class='pad']/p[not(@class)]", xmlValue)[[1]]
            
          } else {
            
            title = xpathApply(e, "//title", xmlValue)
            txt = xpathApply(e, "//div[@id='articleBody']/p", xmlValue)
            txt = c(xpathApply(e, "//div[@id='articleBody']/h2", xmlValue), txt)
            
          }
          
        }
        
      }

      if(!length(date))
        date = ""
      if(grepl("Lundi|Mardi|Mercredi|Jeudi|Vendredi|Samedi|Dimanche", date))
        date = as.character(parse_date_time(date, "%d %m %Y", locale = "fr_FR"))
            
      e = c(title, date, txt[ sapply(as.character(txt), nchar) > 1 ])
      e = gsub("[\r|\n|\t]", " ", e)
      e = gsub("(;|:|\\.|!|\\?|…)(\\w)+", "\\1 \\2", e)
      e = gsub("\\s+", " ", e)
      e = gsub("\"", "'", e)
      e = str_trim(e)
      
      write(e, f)
      
    }
    
  }
    
}

get_corpus <- function(threshold = 10, sample = FALSE, update = FALSE) {

  counts = sapply(dir("data", ".corpus$", full.names = TRUE), function(x) length(dir(x)))
  
  cat("Corpus contains", sum(counts), "articles:\n")
  print(counts)
  
  get_terms(threshold, update) # corpus.terms.csv
  get_freqs(sample, update)    # corpus.match.csv
  get_edges(sample, update)    # corpus.edges.csv
  
  # plot raw counts
  dir.create("plots", showWarnings = FALSE)

  files = unique(read.csv("data/corpus.match.csv")[, 1:3 ])
  files = summarise(group_by(files, source, t), n = n())
  files = summarise(group_by(files, source, ym = substr(t, 1, 7)), n = sum(n))
  
  qplot(data = files, fill = source, y = n, x = ym, alpha = I(2/3),
        stat = "identity", geom = "bar") +
    geom_text(data = subset(summarise(group_by(files, ym), n = sum(n) * 1.05),
                            ym %in% c("2006-03", "2009-06", "2010-10", "2011-05", "2012-01")),
              aes(fill = NULL, x = ym, y = n, label = ym), color = "grey25") +
    scale_fill_brewer("", palette = "Set1") +
    scale_x_discrete(breaks = paste0(2005:2014, "-01"), labels = 2005:2014) +
    labs(y = NULL, x = NULL) +
    theme_linedraw(18)
  
  ggsave("plots/counts.png", width = 12, height = 6)
    
}

get_terms <- function(threshold = 10, update = FALSE) {
  
  if(!file.exists("data/corpus.terms.csv") | update) {
    
    # ---------
    # stopwords
    # ---------
    
    sw = getURL("http://members.unine.ch/jacques.savoy/clef/frenchST.txt")
    sw = readLines(textConnection(sw))
    
    # --------
    # keywords
    # --------
    
    for(i in dir("data", ".corpus$", full.names = TRUE)) {
      
      files = dir(i, ".txt", full.names = TRUE)
      
      kw = sapply(files, function(x) {
        
#         message(x)
        t = readLines(x, encoding = "UTF-8")[ -2 ]
        t = gsub("\\s+", " ", t) # trim double spaces
        r = "(([R|r]apport|[C|c]ommission|[M|m]ission)*\\s*[A-ZÉ]+[a-zâäàêëéèîïôöûüç-]*\\s*(-|à |de |de la |d'|de l'|du |des )*)+"
        unique(str_trim(unlist(str_extract_all(t, r))))
        
      })
      
      # prefixes and suffixes
      r = "^Culture |^UMP |^Mme |^Madame |^Me |^La |^Le |^Les |^Pour |^Dans |^En |^Ce |^Lors de |^Lors de l'|^Lors de la|^Lors du |^Lors des |^Selon |^Selon le |^Selon la |^Selon les |^Un |^Comment |^Peut-être |^Mais |^Malgré |^Même |^Merci |^Rendez-nous |^Sur |^du |^Or | à$| de$| de la$| d'$| de l'$| du$| des$| à des$| -$"
      kw = lapply(kw, function(x) gsub(r, "", x))
      
      # list
      kwds = as.vector(unlist(kw))
      
      # adverbs and such
      kwds = kwds[ -which(grepl("mment$|vant$|hier$|nné[e]*$|^d'$", kwds)) ]
      kwds = kwds[ -which(grepl("-être$|-on$|-nous$|-vous$|-il[s]?$|-elle[s]?$", kwds)) ]
      
      # final manual fixes
      s = which(tolower(kwds) %in% tolower(c(sw, "Puis", "Pourtant", "Contrairement", "Visiblement", "Malheureusement", "Aujourd'", "Est-ce", "Alors", "Interrogé", "Interrogée", "Toutefois", "Ca", "de la","France", "Reste", "Or", "Mise", "Enfin", "Déjà", "Face", "Lorsqu", "Ensuite", "Finalement", "Contacté", "Contactée", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Mieux", "Parce", "Autant", "Grâce", "Preuve", "Fin", "Hier", "Souvent", "Toujours", "Jamais", "Seul", "Seuls", "Seule", "Suite", "Actuellement", "Début", "MAJ", "MàJ", "Officiellement", "Difficile", "Au-delà", "Histoire", "Puisqu", "Généralement", "Heureusement", "Notons", "Rappelons", "Problème", "Signe", "ElementsByTagName", "ElementById", "Element", "PDF", "Probablement", "Mise", "Vient", "Ici", "Vraiment", "Bref", "Impossible", "Oui", "Maintenant", "Retrouvez", "Faire", "Voir", "Loin", "Résultat", "Bon", "Pire", "Abonnements", "Attention", "Faute", "Lire", "Sujet", "Suivez", "Parallèlement", "Retour", "Idem", "Petit", "Nouvelle", "Espérons", "Monsieur")))
      kwds = kwds[ -s ]
      kwds = kwds[ nchar(kwds) > 2 ] # short terms: PC, TV, CD
      
      # frequency table
      freq = as.data.frame(table(kwds), stringAsFactors = FALSE)
      freq = freq[ order(-freq$Freq), ]
      names(freq) = c("term", "freq")
      rownames(freq) = NULL
      
      # save
      write.csv(freq, paste0(i, ".terms.csv"), row.names = FALSE)
      
    }
    
    corpus = lapply(dir("data", "\\w.corpus.terms.csv$", full.names = TRUE), read.csv)
    corpus = rbind.fill(corpus)
    corpus = aggregate(freq ~ term, sum, data = corpus)

    corpus = corpus[ corpus$freq >= threshold, ]
    corpus = corpus[ order(corpus$freq, decreasing = TRUE), ]

    cat("Found", nrow(corpus), "terms occurring at least", threshold, "times\n")
    write.csv(corpus, file = "data/corpus.terms.csv", row.names = FALSE)

  }

}

get_freqs <- function(sample = FALSE, update = FALSE) {
  
  if(!file.exists("data/corpus.match.csv") | update) {

    # ~ 340 unambiguous unique terms, extracted from first 1,000 keywords   
    entities = c("(Alain )?Suguenot", "(Aurélie )?Filip(p)?etti", "(Barack )?Obama", 
      "(Christian )?Vanneste", "(Christine )?Albanel", "(Didier )?Mathus", 
      "(Franck |Frank )?Riester", "(François )?Bayrou", "(François )?Fillon", 
      "(François )?Hollande", "(Frédéric )?Lefebvre", "(Frédéric )?Mitterrand", 
      "(Guy )?Bono", "(Jacques )?Chirac", "(Jean )?Dionis du Séjour", 
      "(Jean-François )?Copé", "(Jean-Marc )?Ayrault", "(Ligue )?Odebi", 
      "(Lionel )?Jospin", "Martin Bouygues", "Bouygues( Telecom| Télécom)?", 
      "(Martine )?Aubry", "(Martine )?Billard", "(Michel )?Riguidel", 
      "(Mireille )?Imbert-Quaretta|(rapport )?MIQ", "(Muriel )?Marland-Militello", 
      "(Nicolas )?Sarkozy", "(Patrick )?Bloche", "(Pierre |mission |Mission |rapport | Rapport)?Lescure", 
      "(rapport |Patrick |mission |commission )?Zelnik", "(Renaud )?Donnedieu de Vabre(s)?|RDDV", 
      "(Renaud )?Veeckman", "(Riposte )?graduée", "(Ségolène )?Royal", 
      "(The )?Pirate Bay", "(Xavier )?Niel", "Acta", "ACTA", "Adami", 
      "ADAMI", "Adobe", "AFA", "Alain Lipietz", "Alex Türk", 
      "Alice", "Alicebox TV", "Alliance Public-Artistes", "Allostreaming", 
      "Alpa", "ALPA", "Amazon", "Anne Hidalgo", "Anonymous", "Apple", 
      "APRIL", "Arcep", "Arjel", "ARJEL", "ARMT", "Arnaud Montebourg", 
      "ASIC", "Association de Lutte|Piraterie Audiovisuelle", "Aurélie Filipetti", 
      "Autorité de Régulation des Mesures Techniques", "Axel Dauchez", 
      "Benjamin Bayart", "Benjamin Lancar", "Benoît Hamon", "Benoît Tabaka", 
      "Bernard Accoyer", "Bernard Carayon", "Bernard Kouchner", "Bernard Miyet", 
      "Bibliothèque Nationale de France", "BPI", "Brice Hortefeux", 
      "British Phonographic Industry", "British Telecom", "Bruno Retailleau", 
      "BSA", "Calimaq", "Calogero", "Carla Bruni", "Carte Musique Jeune", 
      "Catherine Tasca", "Catherine Trautmann", "Deep Packet Inspection",
      "Centre National de la Cinématographie", "Centre National de la Musique", 
      "CGTI", "Christian Paul", "Christine Boutin", "Christine Lagarde", 
      "Christophe Lameignère", "Christophe Tardieu", "CJUE", "CLCV", 
      "CMP", "CNC", "Cnil", "CNIL", "CNM", "Collège de l'Hadopi", 
      "Commerce", "Commission de Protection des Droits", "Commission des Affaires Culturelles", 
      "Commission Européenne", "Commission Mixte Paritaire", "Commission Nationale de l'Informatique", 
      "Conseil Constitutionnel", "Conseil d'Etat", "Conseil de l'Union( Européenne)?", 
      "Conseil National du Numérique", "Conseil Supérieur de l'Audiovisuel", 
      "Conseil Supérieur de la Propriété Littéraire", "Corinne Erhel", 
      "COSIP", "Cour de Justice de l'Union Européenne", "CPD", "Création Public Internet", 
      "Csa", "CSA", "CSPLA", "Dailymotion", "DailyMotion", "Daniel Cohn-Bendit", 
      "David Assouline", "David El Sayegh", "David Guetta", "Deezer", 
      "Défense", "Denis Ladegaillerie", "(Denis |rapport |mission |Mission)?Olivennes", "Digital Economy Act", 
      "Digital Economy Bill", "Digital Millennium Copyright Act", "Digital Rights Management", 
      "Disney", "DMCA", "Dominique de Villepin",
      "DRM", "Ecologie", "EELV", "EFF", "Electronic Frontier Foundation", 
      "EMI", "Eric Besson", "Eric Walter", "Éric Walter", "Eric Woerth", 
      "EUCD", "Eva Joly", "Facebook", "FDN", "Fédération Française des Télécoms", 
      "Fédération Internationale de l'Industrie Phonographique", 
      "Finances", "Fleur Pellerin", "Fnac", "FNAC", "Forum d'Avignon", 
      "Framasoft", "France Télécom", "France Télévisions", "Francis Lalanne", 
      "Françoise Castex", "Françoise de Panafieu", "Frédéric Couchet", 
      "Free|Iliad", "French Data Network", "Front National", "Gaumont", 
      "Gilles Babinet", "Giuseppe de Martino", "Google France", "Guillaume Cerutti", 
      "Hadopi Eric Walter", "Hervé Rony", "IBM", "IFPI", "INA", "Intérieur", 
      "IPRED", "Isabelle Falque-Pierrotin", "ISF", "Jack Lang", "Jacques Attali", 
      "Jacques Bille", "Jacques Legendre", "Jacques Toubon", "Jamendo", 
      "Jean Musitelli", "Jean-Bernard Lévy", "Jean-Frédéric Poisson", 
      "Jean-Louis Debré", "Jean-Luc Godard", "Jean-Michel Planche", 
      "Jean-Noël Tronc", "Jean-Pierre Brard", "Jérémie Zimmermann", 
      "Jérôme Bourreau-Guggenheim", "Jérôme Roger", "Justice", 
      "Kim Dotcom", "Labs Hadopi", "Lady Gaga", "Lagardère", "Laure de la Raudière", 
      "Laure de La Raudière", "Laurent Chemla", "Laurent Fabius", 
      "Laurent Petitgirard", "Laurent Wauquiez", "LCEN", "Lionel Tardy", 
      "Lionel Thoumyre", "Loppsi", "LOPPSI", "Luc Besson", "Luc Chatel", 
      "Maison Blanche", "Maître Eolas", "Manuel Valls", "Marc Guez", 
      "Marc Le Fur", "Marie-Christine Blandin", "Marie-Françoise Marais", 
      "Marine Le Pen", "Mark Zuckerberg", "Matignon", "Maxime Rouquet", 
      "Megaupload", "MegaUpload", "Michel Barnier", "Michel Boyon", 
      "Michel Sardou", "Michel Thiollière", "Michèle Alliot-Marie", 
      "Microsoft", "Midem", "Modem", "MoDem", "Motion Picture Association", "Mouvement Démocrate", 
      "MPAA", "Mulholland Drive", "Nadine Morano", "Naïve", "Napster", 
      "Nathalie Kosciusko-Morizet|NKM", "Neelie Kroes", "Netflix", 
      "Nicolas Dupont-Aignan", "Nicolas Seydoux", "Nouveau Centre", 
      "NPA", "Numericable", "OCDE", "Ofcom", "Olivier Henrard", 
      "Olivier Schrameck", "OMPI", "ONU", "Orange", "Organisation Mondiale de la Propriété Intellectuelle", 
      "OVH", "Paquet Télécom", "Parlement Européen", "Parti Pirate", 
      "Parti Socialiste", "Pascal Nègre", "Pascal Rogard", "Patrice Martin-Lalande", 
      "PCF", "Philippe Aigrain", "Philippe Gosselin", "Pierre Arditi", 
      "Pierre Sirinelli", "Pipa", "PIPA", "Prince", "PUR|Promotion Usages Responsables", 
      "Quadrature( du Net)?", "Rachida Dati", "Radiohead", "rapport Gallo", "Marielle Gallo",
      "Recording Industry Association", "Reporters", "RIAA", "Richard Cazenave",
      "Richard Stallman", "Roger Karoutchi", "RSF", "Sabam", "SACD", "Sacem", "SACEM", 
      "SAMUP", "SCAM", "SCPP", "SELL", "Serge Lagauche", "SFR", "Simavelec", 
      "Snep", "SNEP", "Société Civile des Producteurs de Phonogrammes", 
      "Société Civile des Producteurs Phonographiques", "Société des Auteurs|Compositeurs Dramatiques", 
      "Sony( Music| BMG)", "Sopa", "SOPA", "SOS Hadopi", "SOS-Hadopi", 
      "Spedidam", "Spotify", "SPPF", "SPRD", "Stéphane Richard", "Steve Jobs", 
      "StopDRM", "Syndicat National de l'Edition Phonographique", "Thierry Lhermitte", 
      "TMG", "Travail", "Trident Media Guard", "TVA", "Twitter", "UDF", 
      "UFC(-Que Choisir| Que Choisir|-Que-Choisir|-Que)?", "UMP", "Union Européenne", 
      "UPFI", "Verizon", "Verts", "Vincent Peillon", "Virgin(Mega| Media)?", 
      "Vivendi|Universal( Music)?( France)?", "Viviane Reding", "Warner", 
      "Wikileaks", "WikiLeaks", "Yahoo")

    # check against empirical corpus
    kw = as.character(read.csv("data/corpus.terms.csv")$term)
    kw = sum(str_detect(kw, paste0(entities, collapse = "|")))
    
    cat("Matched", kw, "entities in empirical corpus\n")

    files = dir("data", ".txt", recursive = TRUE, full.names = TRUE)
    files = files[ !grepl(".index.txt$", files) ]
    
    if(sample)
      files = sample(files, sample)
    
    cat("Finding", length(entities), "entities in", length(files), "articles...\n")

    files = lapply(files, function(x) {
      
      j = readLines(x, encoding = "UTF-8")
      t = str_detect(paste0(j[ -2 ], collapse = " "), entities)
      
      date = gsub("(.)+ publié le \\w+\\s| à (.*)", "", j[ 2 ])
      
      if(is.null(date) | nchar(date) < 10)
        date = NA
      else
        date = parse_date_time(date, "%Y %m %d", locale = "fr_FR")
      
      if(sum(t)) {
        
        k = unique(entities[ t ])
        
        # rapport/mission/person
        k[ grepl("Lescure", k) ] = "Mission Lescure"
        k[ grepl("Olivennes", k) ] = "Mission Olivennes"
        k[ grepl("Zelnik", k) ] = "Patrick Zelnik"
        k[ grepl("MIQ$|Quaretta", k) ] = "Mireille Imbert-Quaretta" # incl. a few mentions as "rapport MIQ"
        # name fixes
        k[ grepl("Filip(.*)etti", k) ] = "Aurélie Filippetti" # spelling
        k[ grepl("NKM$|Kosciusko", k) ] = "Nathalie Kosciusko-Morizet"
        k[ grepl("RDDV$|Donnedieu", k) ] = "Renaud Donnedieu de Vabres"
        k[ grepl("Eric Walter", k) ] = "Éric Walter" # accent        
        k[ grepl("Raudière", k) ] = "Laure de La Raudière" # de la / de La
        k[ grepl("Riester", k) ] = "Franck Riester" # Frank/Franck
        # companies/organizations
        k[ grepl("^Bouygues", k) ] = "Bouygues" # Bouygues (Telecom)
        k[ grepl("^Alice", k) ] = "Alice" # Alice, Alice Box TV
        k[ k == "Free|Iliad" ] = "Free"
        k[ grepl("^Sony", k) ] = "Sony" # Sony, Sony BMG, Sony Music
        k[ grepl("^UFC", k) ] = "UFC-QC"
        k[ grepl("^Virgin", k) ] = "Virgin" # Virgin, VirginMega, Virgin Media
        k[ grepl("^Vivendi", k) ] = "V/U"
        k[ k %in% c("ARMT", "Autorité de Régulation des Mesures Techniques") ] = "ARMT"
        k[ k %in% c("ACTA", "Acta") ] = "ALPA"
        k[ k %in% c("ADAMI", "Adami") ] = "ADAMI"
        k[ k %in% c("ALPA", "Alpa", "Association de Lutte|Piraterie Audiovisuelle") ] = "ALPA"
        k[ k %in% c("ARJEL", "Arjel") ] = "ARJEL"
        k[ k %in% c("Barack Obama", "Maison Blanche") ] = "Maison Blanche"
        # "Besson" might be Eric or Luc
        k[ k %in% c("BPI", "British Phonographic Industry") ] = "BPI"
        k[ k %in% c("CMP", "Commission Mixte Paritaire") ] = "CMP"
        k[ k %in% c("CNC", "Centre National de la Cinématographie") ] = "CNC"
        k[ k %in% c("CNM", "Centre National de la Musique") ] = "CNM"
        k[ k %in% c("CNIL", "Cnil", "Commission Nationale de l'Informatique") ] = "CNIL"
        k[ k %in% c("CPD", "Commission de Protection des Droits") ] = "Hadopi-CPD"
        k[ k %in% c("Collège de l'Hadopi") ] = "Hadopi-Collège"
        k[ k %in% c("CSA", "Conseil Supérieur de l'Audiovisuel", "Csa") ] = "CSA"
        k[ k %in% c("Conseil Supérieur de la Propriété Littéraire", "CSPLA") ] = "CSPLA"
        k[ k %in% c("Dailymotion", "DailyMotion") ] = "Dailymotion"
        k[ k %in% c("Digital Economy Act", "Digital Economy Bill") ] = "DEA"
        k[ k %in% c("DMCA", "Digital Millennium Copyright Act") ] = "DMCA"
        k[ k %in% c("DRM", "Digital Rights Management") ] = "DRM"
        k[ k %in% c("EFF", "Electronic Frontier Foundation") ] = "EFF"
        k[ k %in% c("FDN", "French Data Network") ] = "FDN"
        k[ k %in% c("FNAC", "Fnac") ] = "FNAC"
        k[ k %in% c("Free", "Iliad") ] = "Free"
        k[ k %in% c("IFPI", "Fédération Internationale de l'Industrie Phonographique") ] = "IFPI"
        k[ k %in% c("LOPPSI", "Loppsi") ] = "LOPPSI"
        k[ k %in% c("Megaupload", "MegaUpload", "Kim Dotcom") ] = "Megaupload" # excluding "Mega"
        k[ k %in% c("MoDem", "Modem", "Mouvement Démocrate") ] = "MoDem"
        k[ k %in% c("MPAA", "Motion Picture Association") ] = "MPAA"
        k[ k %in% c("OMPI", "Organisation Mondiale de la Propriété Intellectuelle") ] = "OMPI"
        k[ k %in% c("PIPA", "Pipa") ] = "PIPA"
        # k[ k %in% c("Rapidshare", "RapidShare") ] = "Rapidshare"
        k[ k %in% c("RIAA", "Recording Industry Association") ] = "RIAA"
        k[ k %in% c("Reporters", "RSF") ] = "RSF"
        k[ k %in% c("SACD", "Société des Auteurs|Compositeurs Dramatiques") ] = "SACD"
        k[ k %in% c("SACEM", "Sacem") ] = "SACEM"
        k[ k %in% c("SCPP", "Société Civile des Producteurs Phonographiques", "Société Civile des Producteurs de Phonogrammes") ] = "SCPP"
        k[ k %in% c("SNEP", "Snep", "Syndicat National de l'Edition Phonographique") ] = "SNEP"
        k[ k %in% c("SOS Hadopi", "SOS-Hadopi") ] = "SOS-Hadopi" # "Jérôme Bourreau-Guggenheim"
        k[ k %in% c("SOPA", "Sopa") ] = "SOPA"
        k[ k %in% c("TMG", "Trident Media Guard") ] = "TMG"
        k[ k %in% c("WikiLeaks", "Wikileaks") ] = "Wikileaks"
        # k[ k %in% c("Wikipedia", "Wikipédia") ] = "Wikipedia"
        k = str_trim(gsub("\\s+", " ", gsub("\\(|\\||\\?|\\)", " ", k)))
        
        # ministries (excl. Budget and Affaires étrangères/européennes)
        m = k %in% c("Culture", "Justice", "Industrie", "Intérieur", "Défense", "Finances", "Commerce", "Travail", "Agriculture")
        k[ m ] = paste("Min.", k[ m ])

        # drop frequencies (sensitive to acronyms)
        return(data.frame(gsub("data/(.*).corpus/(.*)" , "\\1", x),
                          as.character(date),
                          gsub("data/(.*).corpus/(.*).txt" , "\\2", x),
                          paste0(unique(k), collapse = ";")))
        
      } else {
        
        return(data.frame())
        
      }
      
    })
    
    files = rbind.fill(files)
    names(files) = c("source", "t", "uid", "k")

    if(sum(is.na(files$date)))
      cat("Missing", sum(is.na(files$date)), "dates\n")

    files = arrange(subset(files, !is.na(date)), t, source, uid, k)
    write.csv(files, file = "data/corpus.match.csv", row.names = FALSE)
    
  }
    
}

get_edges <- function(sample = FALSE, update = FALSE) {
  
  if(!file.exists("data/corpus.edges.csv") | update) {
    
    files = read.csv("data/corpus.match.csv", stringsAsFactors = FALSE)
    
    # qplot(year(as.Date(files$t)), geom = "histogram") +
    #   scale_x_continuous(breaks = 2005:2014)
    
    if(sample)
      sample = sample(unique(files$uid), sample)
    else
      sample = unique(files$uid)
    
    cat("Processing edges in", length(sample), "articles...\n")
    
    files = lapply(sort(sample), function(x) {
            
      y = unlist(strsplit(files$k[ files$uid == x ], ";"))
      y = expand.grid(y, y)
      y = subset(y, Var1 != Var2)

      if(!nrow(y)) {
    
        return(data.frame())
    
      } else {
    
        # unique (undirected) ties
        y = y[ apply(y, 1, function(x) x[1] == sort(x)[1]), ]
        names(y) = c("i", "j")

        # inverse frequency weight
        y$w = 1 / nrow(y)
    
        return(data.frame(t = files$t[ files$uid == x ], y))
    
      }
        
    })
    
    write.csv(rbind.fill(files), file = "data/corpus.edges.csv", row.names = FALSE)
    
  }
  
}

get_ranking <- function(start = NULL, end = NULL,
                        file = NULL, rows = 15, verbose = FALSE) {
  
  dir.create("tables", showWarnings = FALSE)

  ini = read.csv("data/corpus.match.csv")
  ini$t = as.Date(as.character(ini$t))
  net = ini

  if(!is.null(start))
    net = subset(net, t >= as.Date(start))
  
  if(!is.null(end))
    net = subset(net, t <= as.Date(end))

  if(verbose) {

    cat(n_distinct(net$uid), "articles:\n")
    print(aggregate(uid ~ source, n_distinct, data = net))

    span = as.numeric(diff(range(net$t)))
    cat(span, "days", round(span / 365, 2), "years:\n")
    print(summary(net$t))

  }
  
  n = get_network(0, start, end, verbose)
  n = data.frame(k = network.vertex.names(n), degree = n %v% "degree")
  n = n[ order(n$degree, decreasing = TRUE), ]
  rownames(n) = NULL
  
  n$n = sapply(n$k, function(x) sum(grepl(x, net$k)))
  
  if(!is.null(file)) {
    
    file = paste0("tables/", file, ".md")
    write(kable(head(n, rows), output = FALSE), file)

    cat("First", rows, "most central nodes exported to", file, "\n")

  }

  return(n)
  
}

get_network <- function(threshold = 0, start = NULL, end = NULL, verbose = TRUE) {
  
  ini = read.csv("data/corpus.edges.csv")
  ini$t = as.Date(as.character(ini$t))
  net = ini
  
  if(!is.null(start))
    net = subset(net, t >= as.Date(start))
  
  if(!is.null(end))
    net = subset(net, t <= as.Date(end))

  if(verbose)
    cat("Processing", nrow(net), "out of", nrow(ini), "edges...\n")

  # sum tie weights over time period
  net$uid = apply(net[, 2:3 ], 1, function(x) paste0(sort(x), collapse = "_"))
  net = aggregate(w ~ uid, sum, data = net)
  net = data.frame(i = gsub("(.*)_(.*)", "\\1", net$uid),
                   j = gsub("(.*)_(.*)", "\\2", net$uid),
                   w = net$w, stringsAsFactors = FALSE)
                   
#   # normalize by frequency of sender
#   # similar to WPC in Gross, Kirkland and Shalizi 2012
#   t = table(i)
#   net$w = net$w / t[ net$i ]
  
  n = network(net[, 1:2 ], directed = FALSE)
  set.edge.attribute(n, "w", net[, 3])
  
  tnet = symmetrise_w(as.sociomatrix(n, attrname = "w"), method = "AMEAN")
  tnet = as.tnet(tnet, "weighted one-mode tnet")

  n %v% "degree" = degree_w(tnet, measure = "degree")[, 2]
  n %v% "distance" = colSums(distance_w(tnet), na.rm = TRUE)
  n %v% "closeness" = closeness_w(tnet)[, 3] # normalized
  n %v% "betweenness" = betweenness_w(tnet)[, 2]

  network::delete.vertices(n, which(n %v% "degree" < quantile(n %v% "degree", threshold)))

  if(verbose)
    cat("Entity network:", network.size(n), "nodes",
        network.edgecount(n), "edges.\n")
  
  return(n)
  
}
