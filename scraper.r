get_articles <- function(source, keyword, pages, step = 10, sw = sw) {
  
  # -----
  # files
  # -----
  
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
      
      html = htmlParse(paste0("http://recherche.lesechos.fr/?exec=1&texte=", keyword, "&dans=touttexte&date1=&date2=&page=", x))
      html = xpathSApply(html, "//div[@class='bloc-resultat-recherche']//a/@href")
      html = str_replace(html, ".php(.*)", ".php")
      html = str_replace(html, "(.*)(.php|.htm)(.*)", "\\1\\2")
      html = html[ str_length(html) > 1 ]
      
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
      
      f = gsub("/", "-", gsub("(.*)(\\d{2})/(\\d{2})/(\\d{4})/(.*)(.htm|.php)", "\\2-\\3-\\4-\\5", d[i]))
      
    }
    
    f = paste0("data/", folder, "/", f, ".txt")
    if(!file.exists(f)) {
      
      if(!i %% step)
        cat("Scraping article", i, "out of", n, "...\n")
      
      if(source == "numerama") {
        
        e = htmlParse(d[i])
        title = xpathApply(e, "//h1", xmlValue)[[1]]
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
        e = try(htmlParse(d[i]))
        
        if("try-error" %in% class(e)) {
          
          warning("Fix:\n", d[i])
          title = str_trim(gsub("-|_|\\d", " ", gsub("(.*)(\\d{4})/(\\d{2})/(\\d{2})/(.*)", "\\5", d[i])))
          txt = NA
          
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
          title = str_trim(gsub("http://|www|\\.|/|lesechos.fr|-|_|\\d|.htm", " ", d[i]))
          date = str_extract(d[i], "\\d{4}/\\d{2}/\\d{2}")
          txt = NA          
          
        } else {
          
          title = xpathApply(e, "//title", xmlValue)
          date = xpathApply(e, "//meta[@itemprop='dateCreated']/@content")
          txt = xpathApply(e, "//div[@class='contenu_article']//p[@itemprop='articleBody']", xmlValue)
          txt = c(xpathApply(e, "//meta[@name='news_keywords']/@value"), txt)
          
        }
        
      }
      
      e = c(title, date, txt)
      e = gsub("[\r|\n|\t]", "", e)
      e = gsub("\"", "'", e)
      
      write(e, f)
      
    }
    
  }
    
}
