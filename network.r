get_corpus <- function(threshold = .9, sample = FALSE, update = FALSE) {

  counts = sapply(c("echos", "ecrans", "numerama", "zdnet"), function(x)
    length(dir(paste0("data/", x, ".corpus"))))
  
  cat("Corpus contains", sum(counts), "articles:\n")
  print(counts)
  
  get_terms(threshold, update) # corpus.terms.csv
  get_freqs(sample, update)  # corpus.freqs.csv
  get_edges(sample, update)  # corpus.edges.csv
  
  dir.create("plots", showWarnings = FALSE)

}

get_terms <- function(threshold = .9, update = FALSE) {
  
  if(!file.exists("data/corpus.terms.csv") | update) {
    
    # ---------
    # stopwords
    # ---------
    
    sw = getURL("http://members.unine.ch/jacques.savoy/clef/frenchST.txt")
    sw = readLines(textConnection(sw))
    
    # --------
    # keywords
    # --------
    
    for(i in dir("data", ".corpus$")) {
      
      files = paste0("data/", i, "/", dir(paste0("data/", i), ".txt"))
      
      kw = sapply(files, function(x) {
        
        t = readLines(x)[ -2 ]
        t = gsub("\\s{2,}", " ", t, perl = TRUE) # trim double spaces
        r = "((rapport|commission|mission)*\\s*[A-ZÉ]+[a-zâäàêëéèîïôöûüç-]*\\s*(-|à |de |de la |d'|de l'|du |des )*)+"
        unique(str_trim(unlist(str_extract_all(t, r))))
        
      })
      
      # prefixes and suffixes
      r = "^Culture |^UMP |^Mme |^Madame |^Me |^La |^Le |^Les |^Dans |^En |^Ce |^Lors de |^Lors de l'|^Lors de la|^Lors du |^Lors des |^Selon |^Selon le |^Selon la |^Selon les |^Un |^Comment |^Peut-être |^Mais |^Malgré |^Même |^Merci |^Rendez-nous |^Sur |^du |^Or | à$| de$| de la$| d'$| de l'$| du$| des$| à des$| -$"
      kw = lapply(kw, function(x) gsub(r, "", x))
      
      # list
      kwds = as.vector(unlist(kw))
      
      # adverbs and such
      kwds = kwds[ -which(grepl("mment$|vant$|hier$|nné[e]*$|^d'$", kwds)) ]
      kwds = kwds[ -which(grepl("-être$|-on$|-nous$|-il[s]*$|-elle[s]*$", kwds)) ]
      
      # final manual fixes
      s = which(tolower(kwds) %in% tolower(c(sw, "Puis", "Pourtant", "Contrairement", "Visiblement", "Malheureusement", "Aujourd'", "Est-ce", "Alors", "Interrogé", "Interrogée", "Toutefois", "Ca", "de la","France", "Reste", "Or", "Mise", "Enfin", "Déjà", "Face", "Lorsqu", "Ensuite", "Finalement", "Contacté", "Contactée", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Mieux", "Parce", "Autant", "Grâce", "Preuve", "Fin", "Hier", "Souvent", "Toujours", "Jamais", "Seul", "Seuls", "Seule", "Suite", "Actuellement", "Début", "MAJ", "MàJ", "Officiellement", "Difficile", "Au-delà", "Histoire", "Puisqu", "Généralement", "Heureusement", "Notons", "Rappelons", "Problème", "Signe", "ElementsByTagName", "ElementById", "Element", "PDF", "Probablement", "Mise", "Vient", "Ici", "Vraiment", "Bref", "Impossible")))
      kwds = kwds[ -s ]
      kwds = kwds[ nchar(kwds) > 2 ] # short terms: PC, TV, CD
      
      # frequency table
      freq = as.data.frame(table(kwds), stringAsFactors = FALSE)
      freq = freq[order(-freq$Freq), ]
      names(freq) = c("term", "freq")
      rownames(freq) = NULL
      
      # save
      write.csv(freq, paste0("data/", i, ".terms.csv"), row.names = FALSE)
      
      #
      #
      #
      
      #     matches = rbind(
      #       c("Opposants : EUCD/LQDN", 
      #         grepl("EUCD|Quadrature|Quadrature du [N|n]et|Christophe Espern|Jérémie Zimmermann|Philippe Aigrain", kw)),
      #       c("HADOPI/DADVSI/LOPPSI", 
      #         grepl("DADVSI|Dadvsi|DADVSi|DAVDSI|LOPPSI|Lopsi|Hadopi|HADOPI|Hadop|Hadoi|Haute Autorité|Haute|CPD|Eric Walter|Mireille Imbert-Quaretta|Imbert-Quaretta|Marie-Françoise Marais", kw)),
      #       c("Conseils/Rapports", 
      #         grepl("CSPLA|Conseil Supérieur de la Propriété Littéraire|Pierre Sirinelli|CNN|CNNum|Conseil National du Numérique|Benoît Tabaka|FDI|Forum des [D|d]roits sur Internet|Falque-Pierrotin|Zelnik|Denis Olivennes|rapport Olivennes|Olivennes|Lescure|mission Lescure", kw)),
      #       c("Industrie : Culture", 
      #         grepl("ALPA|Piraterie Audiovisuelle|Alpa|SDRM|Sony|Warner|UPFI|Union [P|p]roducteurs [P|p]honographiques [F|f]rançais [I|i]ndépendants|Union [P|p]roducteurs [P|p]honographiques [I|i]ndépendants|SACEM|Sacem|Sacem|SACD|Société [A|a]uteurs|[C|c]ompositeurs [D|d]ramatiques|Société des [A|a]uteurs|SPPF|Phonogrammes|SCPP|Société [C|c]ivile [P|p]roducteurs|Société [C|c]ivile des [P|p]roducteurs [P|p]honographiques|SPRD|Marc Guez|SNEP|Syndicat [N|n]ational de l'[É|E|é]dition [P|p]honographique|David El Sayegh|Vivendi|Universal|EMI|Pascal Nègre|Pascal Rogard|Bernard Miyet|RIAA|MPAA|Motion Picture Association|Fédération [I|i]nternationale de l'[I|i]ndustrie [P|p]honographique|IFPI|British Phonographic Industry|BPI|Lagardère|EMI|Fnac|FNAC|International Intellectual Property Alliance|IIPA|Midem|ForumAvignon|Miyet", kw)),
      #       c("Industrie : Info/Web", 
      #         grepl("Apple|Microsoft|Spotify|Dailymotion|Google|Facebook|Amazon|Software Alliance|BSA", kw)),
      #       c("ACTA/SOPA/PIPA", 
      #         grepl("Acta|ACTA|Sopa|SOPA|Pipa|PIPA", kw)),
      #       c("DRM", 
      #         grepl("DRM|MTP|ARMT|Régulation des [M|m]esures [T|t]echniques|Musitelli", kw)),
      #       c("CNIL/CSA", 
      #         grepl("CSA|CNIL|Cnil|Türk|Conseil [S|s]upérieur de l'[A|a]udiovisuel|Commission [N|n]ationale de l'[I|i]nformatique", kw)),
      #       c("P2P/DDL", 
      #         grepl("[P]eer [T|t]o [P|p]eer|[P]eer-[T|t]o-[P|p]eer|p2p|P2P|[T|t]orrent|The Pirate Bay|TPB|DDL|Mega[u|U]pload|Mule|Emule|Donkey|Lime[W|w]ire|Ka[Z|z]a[A|a]", kw)),
      #       c("Opposants : Autres", 
      #         grepl("April|APRIL|Alix Cazenave|Couchet|Bayart|FDN|French Data Network|FSF|FSFE|Free Software Foundation|Stallman|UFC|Union Consommateurs|Dourgnon|CLCV|Parti [P|p]irate|Maxime Rouquet|ODEBI|Obedi|[S|s]topDRM|[S|s]top [drm|DRM]|EFF|Electronic Frontier Fo[u]*ndation|ISOC|Geist|Spedidam|Audionautes|Public-Artistes|Isoc|ISOC|Framasoft", kw)),
      #       c("Industrie : Télécom", 
      #         grepl("Numericable|Free|Niel|Orange|Stéphane Richard|ARCEP|Arcep|France [Télécom|Telecom]|Bouygues|SFR", kw)),
      #       c("UE/IPRED", grepl("CJCE|[J|j]ustice [E|e]uropéenne|Justice de l'Union|CEDH|[E|e]uropéenne [D|d]roits|CJUE|Parlement [E|e]uropéen|Bruxelles|Paquet Télécom|IPRED|Commission [E|e]uropéenne|Reding|Gallo|Bono|Trautmann|Cohn-Bendit|Lipietz|Boumedie[n|nn]e-Thiery", kw)),
      #       c("Exécutif : PR/PM/GOUV", 
      #         grepl("Président|[É|E|é]lysée|Chirac|Sarkozy|Hollande|Matignon|Villepin|Fillon|Ayrault|Hortefeux|Alliot-Marie|Eric Besson|NKM|Nathalie Kosciusko-Morizet|Pellerin|Chatel", kw)),
      #       c("Exécutif : Ministère", 
      #         grepl("Donnedieu|RDDV|Albanel|Mitterrand|Filipetti|Valois|Eric Walter|Tardieu", kw)),
      #       c("Parlement", 
      #         grepl("Parlement|Assemblée|Assemblée [N|n]ationale|Sénat|CMP|Commission Mixte Paritaire|Commission [des A|des a|A|a]ffaires [C|c]ulturelles", kw)),
      #       c("Classe politique : Droite", 
      #         grepl("UMP", kw) | grepl("Vanneste|Lefebvre|Lefèbvre|Lefebre|Riester|Marland-Militello|Copé|Tardy|Dionis|Toubon|Lancar|Poisson|Morano|Gosselin|Raudière|Bayrou|Marland-Militello|Dupont-Aignan|Panafieu|Retailleau|Boutin|Suguenot|Mariton|Lepage|Carayon|Gaino|Guaino|Karoutchi", kw, ignore.case = TRUE)),
      #       c("Classe politique : Gauche", 
      #         grepl("PS|Parti [S|s]ocialiste|Socialistes|Groupe [C|c]ommuniste|Verts|cologie|PCF|Parti de [G|g]auche|Front de [G|g]auche", kw) | grepl("Bloche|Christian Paul|Ségolène Royal|Aubry|Mathus|Lang|Brard|Thiollière|Billard|Blandin|Pinel|Lamberts|Poursinoff|Joly|Rebsamen|Tasca|Montebourg|Lagauche|Badinter|Hidalgo", kw, ignore.case = TRUE))
      #     )
      #     
      #     # -----
      #     # dates
      #     # -----
      #     
      #     dates = sapply(files, function(x) {
      # 
      #       j = gsub("(.)+ publié le \\w+\\s| à (.*)", "", readLines(x)[2])
      #       
      #       if(grepl("/|-", j))
      #         date = parse_date_time(j, "%Y %m %d", locale = "fr_FR")
      #       else
      #         date = parse_date_time(j, "%d %m %Y", locale = "fr_FR")
      #       
      #       return(as.character(date))
      #       
      #     })
      #     
      #     # ---------
      #     # aggregate
      #     # ---------
      #     
      #     # collate
      #     m = as.data.frame(matches)
      #     names(m)[1] = "id"
      #     names(m)[-1] = dates
      #     
      #     # parse
      #     m = melt(m, id = "id", variable = "date")
      #     m$date = tolower(as.character(m$date))
      #     m$value = as.numeric(as.logical(m$value))
      #     print(str(m))
      # #     m$date = parse_date_time(m$date, "%Y %m %d", locale = "fr_FR")
      #     print(table(substr(m$date, 0, 4)))
      # 
      #     write.csv(m[ !is.na(m$date), ], file = paste0(i, ".entities.csv"), row.names = FALSE)
      
    }
    
    corpus = lapply(dir("data", ".terms.csv$", full.names = TRUE), read.csv)
    corpus = rbind.fill(corpus)
    corpus = aggregate(freq ~ term, sum, data = corpus)
    
    corpus = corpus[ corpus$freq > quantile(corpus$freq, threshold), ]
    corpus = corpus[ order(corpus$freq, decreasing = TRUE), ]
    
    cat("Saved", nrow(corpus), "terms\n")
    write.csv(corpus, file = "data/corpus.terms.csv", row.names = FALSE)
    
  }

}

get_freqs <- function(sample = FALSE, update = FALSE) {
  
  if(!file.exists("data/corpus.freqs.csv") | update) {
    
    kw = as.character(read.csv("data/corpus.terms.csv")$term)
    kw = kw[ kw %in% c(# list of ~ 400 unambiguous unique terms, from first 1,036 keywords
                       # excluded: "Hadopi", "HADOPI", "DADVSI", "Dadvsi", "Loi Dadvsi", "Projet Dadvsi",
                       # "Loi Hadopi", "DAVDSI", "Loi Création", "Pour Hadopi"
                       # also excluded: "Parlement", "Elysée", "Sénat", "Assemblée Nationale", "Assemblée", 
                       #  "Président de la République", "Élysée", "Culture", "Ministre de la Culture", 
                       # - media, journalists (e.g. "Korben", "Bluetouff"), other generic terms ("FAI", "SPRD");
                       # - small stories: "Renaud Veeckman", 
                       # included: politicians, industries and
                       # nominal ISPs from France, Europe, N. America (e.g. "Verizon"), civil servants,
                       # representatives, trade unions, parties, activists, academics, prominent legislation
                       # and legal cases (e.g. "Jamendo", "Megaupload", "The Pirate Bay", "Mulholland Drive"),
                       # plus a few more organizations and people (e.g. "Dailymotion")
                       "Nicolas Sarkozy", "Christine Albanel", "UMP", "CSA", "Aurélie Filippetti",
                       "Frédéric Mitterrand", "Quadrature du Net", "CNIL", "Cnil", "Sacem", "Free", "Orange",
                       "François Hollande", "Apple", "Twitter", "Pierre Lescure", "Facebook", "Patrick Bloche",
                       "Renaud Donnedieu de Vabres", "Conseil d'Etat", "mission Lescure", "Christian Paul",
                       "SNEP", "Conseil Constitutionnel", "Vivendi", "Microsoft", "SFR", "CPD",
                       "Dailymotion", "TMG", "Parti Socialiste", "Franck Riester", "rapport Lescure",
                       "Gouvernement", "Justice", "Eric Walter", "MegaUpload",
                       "Mireille Imbert-Quaretta", "SACD", "SCPP", "Albanel", "Lionel Tardy",
                       "Spotify", "Frédéric Lefebvre", "Hollande", "Marie-Françoise Marais",
                       "The Pirate Bay", "Verts", "Denis Olivennes", "Arcep",
                       "ACTA", "Universal", "Amazon", "Parti Pirate", "RIAA", "Fleur Pellerin",
                       "Pascal Nègre", "Frank Riester", "Fnac", "Universal Music", "France Télévisions",
                       "Michèle Alliot-Marie", "Paquet Télécom", "CNC", "François Bayrou", "Jack Lang",
                       "Martine Aubry", "Jean-Marc Ayrault", "Olivennes", "SACEM", "Sony", "SPPF", "Mitterrand",
                       "Bono", "Bouygues", "UDF", "FDN", "Jean-Pierre Brard", "Michel Thiollière", "EMI", "LCEN",
                       "EUCD", "Jean-François Copé", "MPAA", "Alain Suguenot", "Megaupload", "Ségolène Royal",
                       "Trident Media Guard", "UFC-Que Choisir", "Yahoo", "Eric Besson", "Didier Mathus",
                       "Jérémie Zimmermann", "Anonymous", "Christian Vanneste", "Commission Européenne",
                       "Union Européenne", "CMP", "Napster", "Xavier Niel", "Christine Boutin",
                       "Martine Billard", "Lescure", "ARMT", "Warner", "IFPI", "France Télécom", "DMCA",
                       "Jacques Toubon", "Matignon", "Snep", "François Fillon", "Quadrature", "rapport Zelnik",
                       "ALPA", "Guy Bono", "Jean Dionis du Séjour", "Filippetti", "Jérôme Bourreau-Guggenheim",
                       "Nathalie Kosciusko-Morizet", "Olivier Schrameck", "Patrick Zelnik", "RDDV", "Benjamin Bayart",
                       "Adami", "Arnaud Montebourg", "Conseil Supérieur de l'Audiovisuel", "Numericable",
                       "Philippe Aigrain", "Virgin" , "Commission Mixte Paritaire", "Mission Lescure",
                       "mission Zelnik", "Netflix", "Spedidam", "Commission de Protection des Droits",
                       "CSPLA", "Nicolas Dupont-Aignan", "Françoise de Panafieu", "Muriel Marland-Militello",
                       "Pascal Rogard", "Autorité de Régulation des Mesures Techniques", "Dominique de Villepin",
                       "Intérieur", "UPFI", "French Data Network", "Midem", "OMPI", "RapidShare", "Wikileaks",
                       "FNAC", "mission Olivennes", "Nouveau Centre", "David El Sayegh", "NKM", "Rapidshare",
                       "Alpa", "Acta", "Bernard Miyet", "Iliad", "Jacques Chirac", "SOPA",
                       "Bouygues Telecom", "Hadopi Eric Walter", "Jacques Attali", "Luc Besson", "Manuel Valls",
                       "Société des Auteurs", "Brice Hortefeux", "Ministère de la Culture",
                       "Syndicat National de l'Edition Phonographique", "Bernard Accoyer", "Donnedieu de Vabres",
                       "Michel Riguidel", "Alex Türk", "Allostreaming", "SPRD", "Association de Lutte",
                       "Piraterie Audiovisuelle", "Daniel Cohn-Bendit", "Fillon", "INA", "Jamendo", "LOPPSI",
                       "Loppsi", "Olivier Henrard", "Riester", "Ayrault", "David Assouline",
                       "OVH", "BPI", "Catherine Trautmann", "Compositeurs Dramatiques", "Lagardère", "Naïve",
                       "Richard Stallman", "Universal Music France", "Alliance Public-Artistes", "ARJEL",
                       "Catherine Tasca", "Catherine Tasca", "Conseil National du Numérique", 
                       "Digital Millennium Copyright Act", "Eva Joly", "Laure de la Raudière", "Odebi",
                       "Maître Eolas", "Marine Le Pen", "Maxime Rouquet", "Mega", "Nadine Morano",
                       "Neelie Kroes", "RSF", "Suguenot", "VirginMega", "WikiLeaks", "Anne Hidalgo", 
                       "Christophe Tardieu", "IPRED", "Barack Obama", "Marc Guez", "MoDem", "Riguidel",
                       "Société Civile des Producteurs Phonographiques", "Sony Music", "commission Zelnik",
                       "Electronic Frontier Foundation", "Fédération Française des Télécoms", "Framasoft",
                       "Front National", "Gaumont", "Philippe Gosselin", "rapport Olivennes", "Laurent Petitgirard",
                       "Marland-Militello", "Bouygues Télécom",
                       "British Phonographic Industry", "Carla Bruni", "Jean-Michel Planche", "Lionel Jospin",
                       "Martin Bouygues", "Mireille Imbert Quaretta", "Thierry Lhermitte", "Arjel", "Aubry",
                       "Copé", "Eric Woerth", "Jérôme Roger", "Nicolas Seydoux", "Stéphane Richard", "Steve Jobs",
                       "Bayrou", "Commission Nationale de l'Informatique", "Défense", "Finances", "Jean Musitelli",
                       "Jean-Louis Debré", "Kim Dotcom", "Motion Picture Association", "ONU", "PIPA", "Rachida Dati",
                       "Sony BMG", "StopDRM", "Verts Martine Billard", "Viviane Reding", "Forum d'Avignon", "Jospin",
                       "Lionel Thoumyre", "rapport Gallo", "Reporters", "Zelnik", "APRIL", "Google France", 
                       "Ligue Odebi", "Maison Blanche", "Marc Le Fur", "Modem", "Roger Karoutchi", "Sopa",
                       "Collège de l'Hadopi", "Jean-Frédéric Poisson", "Laurent Fabius", "Michel Barnier",
                       "Ofcom", "UFC", "UFC Que Choisir", "Vivendi Universal", "Chirac", "Christine Lagarde",
                       "Commerce", "Digital Economy Act", "Disney", "Hervé Rony", "PCF", "Royal", "Travail",
                       "ADAMI", "Benjamin Lancar", "Conseil Supérieur de la Propriété Littéraire",
                       "Luc Chatel", "Michel Sardou", "ASIC", "Aurélie Filipetti", "Axel Dauchez", "Bernard Carayon",
                       "Bloche", "Christophe Lameignère", "Frédéric Couchet", "Jean-Noël Tronc", "Labs Hadopi",
                       "Niel", "Pipa", "Richard Cazenave", "UFC-Que", "Vanneste", "AFA", "Corinne Erhel",
                       "COSIP", "David Guetta", "EELV", "Éric Walter", "ISF", "Laurent Wauquiez", "Lefebvre",
                       "Ligue", "Marie-Christine Blandin", "Mulholland Drive", "Bernard Kouchner", "Calimaq",
                       "CLCV", "Denis Ladegaillerie", "Dionis du Séjour", "Françoise Castex",
                       "Isabelle Falque-Pierrotin", "MIQ", "OCDE", 
                       "Organisation Mondiale de la Propriété Intellectuelle", "Patrice Martin-Lalande",
                       "Rapport Lescure", "rapport MIQ", "Recording Industry Association", "SOS Hadopi",
                       "Verizon", "DailyMotion", "EFF", "Mark Zuckerberg", "Mathus", "Michel Boyon", 
                       "PS Christian Paul", "SCAM", "Bibliothèque Nationale de France", "BSA", "CGTI",
                       "Commission des Affaires Culturelles", "Création Public Internet", "Csa",
                       "IBM", "Jacques Legendre", "Jean-Luc Godard", "Lady Gaga", "Laure de La Raudière",
                       "Laurent Chemla", "Renaud Donnedieu de Vabre", "SAMUP", "SELL", "Serge Lagauche",
                       "Simavelec", "SOS-Hadopi", "Vincent Peillon", "Virgin Media", "Benoît Hamon", "British Telecom",
                       "Centre National de la Musique", "Imbert-Quaretta", "Mouvement Démocrate", "NPA",
                       "Pirate Bay", "Prince", "Radiohead", "Société Civile des Producteurs de Phonogrammes",
                       "UFC-Que-Choisir", "Alain Lipietz", "Bruno Retailleau", "Calogero",
                       "Centre National de la Cinématographie", "Digital Economy Bill", "Ecologie",
                       "Fédération Internationale de l'Industrie Phonographique", "Francis Lalanne",
                       "Giuseppe de Martino", "Guillaume Cerutti") ]


    files = dir("data", ".txt", recursive = TRUE, full.names = TRUE)
    files = files[ !grepl(".index.txt$", files) ]
    
    if(sample)
      files = sample(files, sample)
    
    cat("Processing terms in", length(files), "articles...\n")

    files = lapply(files, function(x) {
      
      j = readLines(x)
      term = str_detect(paste0(j[ -2 ], collapse = " "), kw)
      
      date = gsub("(.)+ publié le \\w+\\s| à (.*)", "", j[ 2 ])
      
      if(grepl("/|-", date))
        date = parse_date_time(date, "%Y %m %d", locale = "fr_FR")
      else
        date = parse_date_time(date, "%d %m %Y", locale = "fr_FR")
      
      if(sum(term)) {
        
        k = kw[ term ]
        
        # named entities
        k[ grepl("hadopi", k, ignore.case = TRUE) ] = "HADOPI"
        k[ grepl("dadvsi", k, ignore.case = TRUE) ] = "DADVSI"
        # k[ grepl("^Assemblée", k) ] = "Assemblée Nationale"
        k[ grepl("Albanel", k) ] = "Christine Albanel"
        k[ grepl("Ayrault", k) ] = "Jean-Marc Ayrault"
        k[ grepl("Aubry", k) ] = "Martine Aubry"
        k[ grepl("Bayrou", k) ] = "François Bayrou"
        k[ grepl("Bloche", k) ] = "Patrick Bloche"
        k[ grepl("Billard", k) ] = "Martine Billard"
        k[ grepl("Christian Paul", k) ] = "Christian Paul"
        k[ grepl("Chirac", k) ] = "Jacques Chirac"
        k[ grepl("Copé", k) ] = "Jean-François Copé"
        k[ grepl("Dionis du Séjour", k) ] = "Jean Dionis du Séjour"
        k[ grepl("^RDDV$|Donnedieu de V", k) ] = "Renaud Donnedieu de Vabres"
        k[ grepl("Eric Walter", k) ] = "Éric Walter"
        k[ grepl("Filip(p)?etti", k) ] = "Aurélie Filippetti"
        k[ grepl("Fillon", k) ] = "François Fillon"
        k[ grepl("Hollande", k) ] = "François Hollande"
        k[ grepl("Jospin", k) ] = "Lionel Jospin"
        k[ grepl("Lefebvre", k) ] = "Frédéric Lefebvre"
        k[ grepl("Lescure", k) ] = "Mission Lescure"
        k[ grepl("Mathus", k) ] = "Didier Mathus"
        k[ grepl("Mitterrand", k) ] = "Frédéric Mitterrand"
        k[ grepl("MIQ$|Quaretta", k) ] = "Mireille Imbert-Quaretta" # also rapport
        k[ grepl("Niel", k) ] = "Xavier Niel"
        k[ grepl("Olivennes", k) ] = "Mission Olivennes"
        k[ grepl("Raudière", k) ] = "Laure de La Raudière"
        k[ grepl("Riester", k) ] = "Franck Riester"
        k[ grepl("Riguidel", k) ] = "Michel Riguidel" # ENST
        k[ grepl("Royal", k) ] = "Ségolène Royal"
        k[ grepl("Sarkozy", k) ] = "Nicolas Sarkozy"
        k[ grepl("Suguenot", k) ] = "Alain Suguenot"
        k[ grepl("Vanneste", k) ] = "Christian Vanneste"
        k[ grepl("Zelnik", k) ] = "Mission Zelnik"
        k[ k %in% c("ARMT", "Autorité de Régulation des Mesures Techniques") ] = "ARMT"
        k[ k %in% c("ACTA", "Acta") ] = "ALPA"
        k[ k %in% c("ADAMI", "Adami") ] = "ADAMI"
        k[ k %in% c("ALPA", "Alpa", "Association de Lutte", "Piraterie Audiovisuelle") ] = "ALPA"
        k[ k %in% c("ARJEL", "Arjel") ] = "ARJEL"
        # k[ k %in% c("Barack Obama", "Maison Blanche") ] = "Barack Obama"
        # "Besson" might be Eric or Luc
        k[ k %in% c("Bono", "Guy Bono") ] = "Guy Bono"
        k[ k %in% c("Bouygues", "Bouygues Telecom", "Bouygues Télécom", "Martin Bouygues") ] = "Bouygues"
        k[ k %in% c("BPI", "British Phonographic Industry") ] = "BPI"
        k[ k %in% c("CMP", "Commission Mixte Paritaire") ] = "CMP"
        k[ k %in% c("CNC", "Centre National de la Cinématographie") ] = "CNC"
        k[ k %in% c("CNIL", "Cnil", "Commission Nationale de l'Informatique") ] = "CNIL"
        k[ k %in% c("CPD", "Commission de Protection des Droits") ] = "Hadopi-CPD"
        k[ k %in% c("Collège de l'Hadopi") ] = "Hadopi-Collège"
        k[ k %in% c("CSA", "Conseil Supérieur de l'Audiovisuel", "Csa") ] = "CSA"
        k[ k %in% c("Conseil Supérieur de la Propriété Littéraire", "CSPLA") ] = "CSPLA"
        # k[ k %in% c("Culture", "Ministre de la Culture") ] = "Ministre Culture"
        k[ k %in% c("Dailymotion", "DailyMotion") ] = "Dailymotion"
        k[ k %in% c("Digital Economy Act", "Digital Economy Bill") ] = "DEA"
        k[ k %in% c("DMCA", "Digital Millennium Copyright Act") ] = "DMCA"
        # k[ k %in% c("Élysée", "Elysée", "Président de la République") ] = "Présidence"
        k[ k %in% c("EFF", "Electronic Frontier Foundation") ] = "EFF"
        k[ k %in% c("FDN", "French Data Network") ] = "FDN"
        k[ k %in% c("FNAC", "Fnac") ] = "FNAC"
        k[ k %in% c("Free", "Iliad") ] = "Free"
        k[ k %in% c("LOPPSI", "Loppsi") ] = "LOPPSI"
        k[ k %in% c("Megaupload", "MegaUpload", "Mega", "Kim Dotcom") ] = "Megaupload"
        k[ k %in% c("Marland-Militello", "Muriel Marland-Militello") ] = "Muriel Marland-Militello"
        k[ k %in% c("MoDem", "Modem", "Mouvement Démocrate") ] = "MoDem"
        k[ k %in% c("MPAA", "Motion Picture Association") ] = "MPAA"
        k[ k %in% c("NKM", "Nathalie Kosciusko-Morizet") ] = "Nathalie Kosciusko-Morizet"
        k[ k %in% c("Odebi", "Ligue Odebi", "Ligue") ] = "Ligue Odebi"
        k[ k %in% c("OMPI", "Organisation Mondiale de la Propriété Intellectuelle") ] = "OMPI"
        k[ k %in% c("PIPA", "Pipa") ] = "PIPA"
        k[ k %in% c("Quadrature", "Quadrature du Net", "Jérémie Zimmermann") ] = "LQDN"
        k[ k %in% c("Rapidshare", "RapidShare") ] = "Rapidshare"
        k[ k %in% c("RIAA", "Recording Industry Association") ] = "RIAA"
        k[ k %in% c("Reporters", "RSF") ] = "RSF"
        k[ k %in% c("SACD", "Société des Auteurs", "Compositeurs Dramatiques") ] = "SACD"
        k[ k %in% c("SACEM", "Sacem") ] = "SACEM"
        k[ k %in% c("SCPP", "Société Civile des Producteurs Phonographiques", "Société Civile des Producteurs de Phonogrammes") ] = "SCPP"
        k[ k %in% c("SNEP", "Snep", "Syndicat National de l'Edition Phonographique") ] = "SNEP"
        k[ k %in% c("SOS Hadopi", "SOS-Hadopi") ] = "SOS-Hadopi" # "Jérôme Bourreau-Guggenheim"
        k[ k %in% c("Sony", "Sony BMG", "Sony Music") ] = "Sony"
        k[ k %in% c("SOPA", "Sopa") ] = "SOPA"
        k[ k %in% c("TMG", "Trident Media Guard") ] = "TMG"
        k[ k %in% c("Pirate Bay", "The Pirate Bay") ] = "TPB"
        k[ k %in% c("UFC", "UFC-Que Choisir", "UFC Que Choisir", "UFC-Que", "UFC-Que-Choisir") ] = "UFC-QC"
        k[ k %in% c("Virgin", "VirginMega", "Virgin Media") ] = "Virgin"
        k[ k %in% c("Vivendi", "Universal", "Vivendi Universal", "Universal Music", "Universal Music France") ] = "V/U"
        k[ k %in% c("WikiLeaks", "Wikileaks") ] = "Wikileaks"
        # k[ k %in% c("Wikipedia", "Wikipédia") ] = "Wikipedia"
        
        return(data.frame(gsub("data/(.*).corpus/(.*)" , "\\1", x),
                          gsub("data/(.*).corpus/(.*).txt" , "\\2", x),
                          as.character(date), k))
        
      } else {
        
        return(data.frame())
        
      }
      
    })
    
    files = rbind.fill(files)
    names(files) = c("source", "uid", "t", "k")
    files = summarise(group_by(files, source, t, uid, k), n = n()) # total occurrences per article
    files = mutate(group_by(files, k), w = n / sum(n)) # weight
    files = arrange(files, t, source, uid, k)
    
    cat("Term occurrences:\n")
    print(table(files$n))
    
    cat("Normalized weights:\n")
    print(summary(files$w))
    
    write.csv(files, file = "data/corpus.freqs.csv", row.names = FALSE)
    
  }
    
}

get_edges <- function(sample = FALSE, update = FALSE) {
  
  if(!file.exists("data/corpus.edges.csv") | update) {
    
    files = read.csv("data/corpus.freqs.csv")
    
    # qplot(year(as.Date(files$t)), geom = "histogram") +
    #   scale_x_continuous(breaks = 2005:2014)
    
    if(sample)
      sample = sample(unique(files$uid), sample)
    else
      sample = unique(files$uid)
    
    cat("Processing edges in", length(sample), "articles...\n")
    
    files = lapply(sort(sample), function(x) {
            
      y = files$k[ files$uid == x ]
      w = files$w[ files$uid == x ]
      names(w) = y
      
      y = expand.grid(y, y)
      y = subset(y, Var1 != Var2)
      if(!nrow(y)) {
        
        return(data.frame())
        
      } else {
        
        y$w = w[ as.character(y$Var1) ] * w[ as.character(y$Var2) ] # joint prob
        y$uid = apply(y[, 1:2 ], 1, function(x) paste0(sort(x), collapse = "_"))
        y = aggregate(w ~ uid, sum, data = y)
        
        return(data.frame(t = unique(files$t[ files$uid == x ]),
                          i = gsub("(.*)_(.*)", "\\1", y$uid),
                          j = gsub("(.*)_(.*)", "\\2", y$uid),
                          w = y$w))
        
      }
      
    })
    
    write.csv(rbind.fill(files), file = "data/corpus.edges.csv", row.names = FALSE)
    
  }
  
}

get_ranking <- function(start = NULL, end = NULL) {
  
  ini = read.csv("data/corpus.freqs.csv")
  ini$t = as.Date(as.character(ini$t))
  net = ini

  if(!is.null(start))
    net = subset(net, t >= as.Date(start))
  
  if(!is.null(end))
    net = subset(net, t <= as.Date(end))
    
  cat(n_distinct(net$uid), "articles:\n")
  print(aggregate(uid ~ source, n_distinct, data = net))

  span = as.numeric(diff(range(net$t)))
  cat(span, "days", round(span / 365, 2), "years:\n")
  print(summary(net$t))
  
  n = get_network(0, start, end)
  n = data.frame(k = network.vertex.names(n), degree = n %v% "degree")
  n = n[ order(n$degree, decreasing = T), ]
  rownames(n) = NULL
  
  counts = summarise(group_by(net, k), n = sum(n))
  
  return(join(n, counts, by = "k"))
  
}

get_network <- function(threshold = 0, start = NULL, end = NULL) {
  
  ini = read.csv("data/corpus.edges.csv")
  ini$t = as.Date(as.character(ini$t))
  net = ini
  
  if(!is.null(start))
    net = subset(net, t >= as.Date(start))
  
  if(!is.null(end))
    net = subset(net, t <= as.Date(end))

  cat("Processing", nrow(net), "out of", nrow(ini), "edges...\n")
  
  net$uid = apply(net[, 2:3 ], 1, function(x) paste0(sort(x), collapse = "_"))
  
  net = aggregate(w ~ uid, sum, data = net)
  net = data.frame(i = gsub("(.*)_(.*)", "\\1", net$uid),
                   j = gsub("(.*)_(.*)", "\\2", net$uid),
                   w = net$w, stringsAsFactors = FALSE)
  
  n = network(net[, 1:2 ], directed = FALSE)
  n %e% "w" = net[, 3]
  
  tnet = symmetrise_w(as.sociomatrix(n, attrname = "w"), method = "AMEAN")
  tnet = as.tnet(tnet, "weighted one-mode tnet")

  n %v% "degree" = degree_w(tnet, measure = "degree")[, 2]
  n %v% "distance" = colSums(distance_w(tnet), na.rm = TRUE)
  n %v% "closeness" = closeness_w(tnet)[, 3] # normalized
  n %v% "betweenness" = betweenness_w(tnet)[, 2]

  network::delete.vertices(n, which(n %v% "degree" < quantile(n %v% "degree", threshold)))
  
  cat("Entity network:", network.size(n), "nodes", network.edgecount(n), "edges.\n")
  
  return(n)
  
}
