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

classifySpecimenPIDS <- function(data) {
  data %<>%
    mutate(lsid_type = case_when(grepl("botanicalcollections",cspp) ~ "MeiseBG",
                                 grepl("jacq",cspp) ~ "JACQ",
                                 grepl("rbge.org.uk",cspp,fixed=T) ~ "RBGE",
                                 grepl("bgbm.org",cspp,fixed=T) ~ "BGBM",
                                 T ~ "other"))
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
             wikidata = gsub("/.*","",wikidata),
             wikidata = gsub("[^0-9|Q]","",wikidata))
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

filter_items_with_prop <- function(data,prop,not=F) {
  require(rlang)
  
  new = tibble(id = NA,
               label = NA)
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]]$entities)) {
      if ((!is.null(data[[i]]$entities[[j]]$claims[[prop]])&not==F)|
          (is.null(data[[i]]$entities[[j]]$claims[[prop]])&not==T)) {
          temp = tibble(id = names(data[[i]]$entities)[[j]],
                        label = data[[i]]$entities[[j]]$labels$en$value)
          new = rbind(new,temp)
      }
    }
  }
  new = new[-1,]
  return(new)
}

make_quickstatements <- function(data) {
  inst = read_csv("institutions_in_wikidata.csv")
  
  new = data %>%
    filter(grepl("wikidata",recordedBy_IRI))
  
  new %<>% 
    left_join(inst,
              by=c("institutionID"="institutionID")) %>%
    ungroup() %>%
    mutate(recordedBy_IRI = gsub(".*/","",recordedBy_IRI),
           jacq_id = gsub("https://","",cspp,fixed=T),
           jacq_id = gsub("http://","",cspp,fixed=T),
           jacq_id = gsub("\\..*","",jacq_id),
           jacq_id = ifelse(lsid_type=="JACQ",
                            paste0("\"",
                                   toupper(jacq_id),
                                   "\""),
                            NA),
           cspp = paste0("\"",
                         cspp,
                         "\""),
           ih = ifelse(is.na(jacq_id),
                       NA,
                       "S5858"),
           ref_url = "S854",
           coll_items = "P11146") %>%
    filter(!is.na(qid)) %>%
    select(recordedBy_IRI,
           coll_items,
           qid,
           ref_url,
           cspp,
           ih,
           jacq_id)
  write_tsv(new,
            "quickstatements.txt",
            na="",
            escape="none")
}

get_failed_qs <- function(batchid,
                          size = 300) {
  fails = fromJSON(paste0("https://quickstatements.toolforge.org/api.php?action=",
                          "get_commands_from_batch&start=",
                          0,
                          "&limit=",
                          size,
                          "&filter=",
                          "RUN",
                          "&batch=",
                          batchid))
  
  fails2 = tibble(recordedBy_IRI  = fails$data$json$item,
                  qid = NA)
  
  for (i in 1:dim(fails2)[1]) {
    fails2$qid[i] = fails$data$json$datavalue$value$id[[i]]
  }
  
  fails2 %<>%
    mutate(subid = paste0(recordedBy_IRI,qid))
  
  qs = read_tsv("quickstatements.txt") %>%
    mutate(subid = paste0(recordedBy_IRI,qid)) %>%
    filter(subid%in%fails2$subid) %>%
    select(-subid) %>%
    mutate(cspp = paste0("\"",
                         cspp,
                         "\""),
           jacq_id = ifelse(is.na(jacq_id),
                       NA,
                       paste0("\"",jacq_id,"\"")),)
    
  write_tsv(qs,
            "quickstatements_missing.txt",
            na="",
            escape="none")
}

remove_references <- function(data,
                              property = "P11146",
                              refproperty = "P5858") {
  require(httr)
  
  query = paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
                 paste(data$recordedBy_IRI,
                       collapse="|"),
                 "&format=json")
  
  data_info = content(GET(query),
                      type = "application/json")
  data %<>% 
    mutate(claimid = NA,
           hash = NA)
  for (i in 1:length(data_info$entities)) {
    for (j in 1:length(data_info$entities[[i]]$claims[[property]])) {
      if (data_info$entities[[i]]$claims[[property]][[j]]$mainsnak$datavalue$value$id == data$qid[i]) {
        for (k in 1:length(data_info$entities[[i]]$claims[[property]][[j]]$references)) {
          if (grepl("HTTP://",
                    data_info$entities[[i]]$claims[[property]][[j]]$references[[k]]$snaks[[refproperty]][[1]]$datavalue$value,
                    fixed=T)) {
            data$claimid[i] = data_info$entities[[i]]$claims[[property]][[j]]$id
            data$hash[i] = data_info$entities[[i]]$claims[[property]][[j]]$references[[k]]$hash
            break
          }
        }
        break
      }
    }
  }
  
  auth_status = GET("https://www.wikidata.org/w/api.php?action=query&meta=authmanagerinfo&amirequestsfor=login")
  
  
  for (i in 1:dim(data)[1]) {
    base_url = "https://www.wikidata.org/w/api.php"
    payload = list(action = "wbremovereferences",
                   statement = data$claimid[i],
                   references = data$hash[i],
                   token = token)
    
    headers = c("Authorization" = paste("Bearer", token))
    
    r = POST(base_url,
             body = payload,
             encode = "form")#,
             #add_headers(.headers = headers))
    r2 = content(r)
    write_xml(r2,"r2.html")
  }
}