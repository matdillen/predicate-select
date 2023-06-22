library(tidyverse)
library(magrittr)
setwd("D:/apm/bicikl/7.3/predicate-select")
#oc = read_tsv("occurrence.txt",quote="",col_types = cols(.default = "c"))
pr = read_tsv("predicate-select.txt",quote="",col_types = cols(.default = "c"))

source("clean_functions.R")

oc_ids = pr %>%
  count(recordedByID) %>%
  rename(identifier = recordedByID) 

oc_ids2 = pr %>%
  count(identifiedByID) %>%
  rename(identifier = identifiedByID) %>%
  rbind(oc_ids) %>%
  filter(!duplicated(identifier)) %>%
  classifyPIDS()

#filter(oc_ids2,type == "other") %>% count(identifier) %>% View()

oc_ids3 = oc_ids2 %>%
  filter(type!="int",type!="other") %>%
  separate_rows(identifier,sep=";") %>%
  #mutate(test = nchar(identifier)) %>%
  classifyPIDS()

#count(oc_ids3,type) %>% arrange(desc(n))
#filter(oc_ids3,type=="viaf") %>% View()
orcids = oc_ids3 %>%
  cleanPIDS(which="orcid")

#initialize
pubst = tibble(item = 0,itemLabel = 0,id=0)

#modify iterator range to apply multiple batches
#allb.nd is the result of the joined queries, filter for items without any date
#take querki function from authormatching.R
for (i in 1:dim(orcids)[1]) {
  query <- paste0('SELECT ?item ?itemLabel WHERE 
  {
  ?item wdt:P496 \"',orcids$orcid[i],'\".
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" }
  }')
  pubs = querki(query)
  if (dim(pubs)[1]>0) {
    pubs$id = orcids$orcid[i]
    pubst = rbind(pubst,pubs)
  }
  print(i)
}
pubst = pubst[-1,]

viaf = oc_ids3 %>%
  cleanPIDS(which = "viaf")
#initialize
pubst2 = tibble(item = 0,itemLabel = 0,id=0)

#modify iterator range to apply multiple batches
#allb.nd is the result of the joined queries, filter for items without any date
#take querki function from authormatching.R
for (i in 1:dim(viaf)[1]) {
  query <- paste0('SELECT ?item ?itemLabel WHERE 
  {
  ?item wdt:P214 \"',viaf$viaf[i],'\".
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" }
  }')
  pubs = querki(query)
  if (dim(pubs)[1]>0) {
    pubs$id = viaf$viaf[i]
    pubst2 = rbind(pubst2,pubs)
  }
  print(i)
}
pubst2 = pubst2[-1,]

ipni = oc_ids3 %>%
  cleanPIDS(which = "ipni")
#initialize
pubst3 = tibble(item = 0,itemLabel = 0,id=0)

#modify iterator range to apply multiple batches
#allb.nd is the result of the joined queries, filter for items without any date
#take querki function from authormatching.R
for (i in 1:dim(ipni)[1]) {
  query <- paste0('SELECT ?item ?itemLabel WHERE 
  {
  ?item wdt:P586 \"',ipni$ipni[i],'\".
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" }
  }')
  pubs = querki(query)
  if (dim(pubs)[1]>0) {
    pubs$id = ipni$ipni[i]
    pubst3 = rbind(pubst3,pubs)
  }
  print(i)
}
pubst3 = pubst3[-1,]

checkids = pubst %>%
  pull(item) %>%
  c(pull(pubst2,item)) %>%
  c(pull(pubst3,item)) %>%
  tibble(ids = .) %>%
  distinct() %>%
  mutate(ids = gsub(".*/","",ids))

resu.r = list()
j=1

#assess is the list of wikidata items which were matched to the MeiseBG collector list
#any filter of the SPARQL query results could do
#only item ids (not URLs) and labels are needed
steps = seq(1,dim(checkids)[1],by=49)
steps = c(steps,dim(checkids)[1])
for (i in steps[-length(steps)]) {
  tst = paste(checkids$ids[i:steps[j+1]],
              collapse="|")
  resu = httr::GET(url = paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
                                tst,
                                "&format=json"),
                   httr::user_agent("Matdillen"))
  resu.r[[j]] = httr::content(resu,type="application/json")
  print(j)
  j=j+1
}

#stack all listed property ids
j=1
props = names(resu.r[[1]]$entities[[checkids$ids[1]]]$claims)
for (i in 2:dim(checkids)[1]) {
  props = c(props,names(resu.r[[j]]$entities[[checkids$ids[i]]]$claims))
  if(i%%49==0) {
    j=j+1
  }
}

#make frequency table
props=tibble(props)
props2 = count(props,props)
