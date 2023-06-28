classifyPIDS <- function(data) {
  data %<>%
    mutate(type = case_when(grepl("orcid",identifier) ~ "orcid",
                            grepl("0000-",identifier,fixed=T) ~ "orcid",
                            grepl("wikidata",identifier) ~ "wikidata",
                            grepl("viaf.",identifier,fixed=T) ~ "viaf",
                            grepl("ipni.",identifier,fixed=T) ~ "ipni",
                            grepl("isni.",identifier,fixed=T) ~ "isni",
                            grepl("harvard.",identifier,fixed=T) ~ "harvard",
                            !grepl("[^0-9]",identifier) ~ "int",
                            grepl("zoobank",identifier,fixed=T) ~ "zoobank",
                            grepl("biodiversitylibrary",identifier,fixed=T) ~ "bhl",
                            grepl("linkedin",identifier,fixed=T) ~ "linkedin",
                            grepl("researchgate",identifier,fixed=T) ~ "researchgate",
                            grepl("scholar.google",identifier,fixed=T) ~ "googlescholar",
                            T  ~ "other",))
  return(data)
}

cleanPIDS <- function(data,which) {
  if (which == "orcid") {
    data %<>%
      filter(type=="orcid") %>%
      mutate(orcid = str_extract(identifier,
                                 "[0-9|\\-|X]+"),
             orcid = ifelse(substr(orcid,4,4)=="-",
                            paste0("0",orcid),
                            orcid)) %>%
      filter(nchar(orcid)==19)
    return(data)
  } else if (which == "wikidata") {
    data %<>%
      filter(type=="wikidata") %>%
      mutate(wikidata = gsub(".*Q","",identifier),
             wikidata = paste0("Q",wikidata),
             wikidata = gsub("/.*","",wikidata))
    return(data)
  } else if (which == "viaf") {
    data %<>%
      filter(type == "viaf") %>%
      mutate(viaf = gsub(".*/","",identifier))
    return(data)
  } else if (which == "ipni") {
    data %<>%
      filter(type == "ipni") %>%
      mutate(ipni = gsub(".*\\?id=","",identifier))
    return(data)
  }
}

querki <- function(query,h="text/csv",agent="MatDillen") {
  require(httr)
  response <- httr::GET(url = "https://query.wikidata.org/sparql", 
                        query = list(query = query),
                        httr::add_headers(Accept = h),
                        httr::user_agent(agent))
  return(httr::content(response,
                       type=h,
                       col_types = cols(.default = "c")))
}

puerki <- function(ids,
                   which = "ids") {
  require(httr)
  
  steps = seq(1,
              dim(ids)[1],
              by=50)
  
  resu.r = list()
  j=1
  for (i in steps) {
    if (i == steps[length(steps)]) {
      jump = dim(ids)[1] - i
    } else {
      jump = 49
    }
    tst = paste(ids[[which]][i:(i+jump)],
                collapse="|")
    resu = httr::GET(url = paste0("https://www.wikidata.org/w/api.php",
                                  "?action=wbgetentities&ids=",
                                  tst,
                                  "&format=json"),
                     httr::user_agent("Matdillen"))
    resu.r[[j]] = httr::content(resu,
                                type="application/json")
    print(j)
    j=j+1
  }
  return(resu.r)
}

puerki_stack <- function(resu,
                         which = "claims") {
  if (which == "claims") {
    j = 1
    l = 50*(length(resu)-1)+length(resu[[length(resu)]]$entities)
    
    properties = names(resu[[1]]$entities[[1]]$claims)
    for (i in 2:l) {
      properties = c(properties,
                     names(resu[[j]]$entities[[i-50*(j-1)]]$claims))
      if(i%%50==0) {
        j=j+1
      }
    }
    return(properties)
  } else if (which == "labels") {
    j = 1
    l = 50*(length(resu)-1)+length(resu[[length(resu)]]$entities)
    
    properties = resu[[1]]$entities[[1]]$labels$en$value
    for (i in 2:l) {
      properties = c(properties,
                     resu[[j]]$entities[[i-50*(j-1)]]$labels$en$value)
      if(i%%50==0) {
        j=j+1
      }
    }
    return(properties)
  }
}

stack_count <- function(resu,prop) {
  new = tibble(id = NA,
               snak = NA)
  for (i in 1:length(resu)) {
    for (j in 1:length(resu[[i]]$entities)) {
      if (!is.null(resu[[i]]$entities[[j]]$claims[[prop]])) {
        for (k in 1:length(resu[[i]]$entities[[j]]$claims[[prop]])) {
          temp = tibble(id = names(resu[[i]]$entities)[[j]],
                        snak = resu[[i]]$entities[[j]]$claims[[prop]][[k]]$mainsnak$datavalue$value$id)
          new = rbind(new,temp)
        }
      }
    }
  }
  new = new[-1,]
  new$prop = prop
  return(new)
}

get_items_with_prop <- function(data,prop,pid) {
  require(rlang)
  sub = data %>%
    cleanPIDS(which = prop) %>%
    filter(!duplicated(eval(parse_expr(prop))))
  
  pubst = tibble(item = 0,itemLabel = 0,id=0)
  for (i in 1:dim(sub)[1]) {
    query <- paste0('SELECT ?item ?itemLabel WHERE 
        {
        ?item wdt:',pid,' \"',sub[[prop]][i],'\".
        SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" }
        }')
    pubs = querki(query)
    if (dim(pubs)[1]>0) {
      pubs$id = sub[[prop]][i]
      pubst = rbind(pubst,pubs)
    }
    print(i)
  }
  pubst = pubst[-1,]
  return(pubst)
}