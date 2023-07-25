library(tidyverse)
library(magrittr)
library(jsonlite)
#oc = read_tsv("occurrence.txt",quote="",col_types = cols(.default = "c"))
pr = read_tsv("predicate-select3.txt",quote="",col_types = cols(.default = "c"))

source("clean_functions.R")

oc_ori = pr %>%
  count(datasetKey) %>%
  arrange(desc(n)) %>%
  mutate(cum = cumsum(n),
         cump = cum/sum(n))

oc_ori2 = pr %>%
  filter(basisOfRecord!="OCCURRENCE") %>%
  count(datasetKey) %>%
  arrange(desc(n)) %>%
  mutate(cum = cumsum(n),
         cump = cum/sum(n))

library(jsonlite)
oc_ori2$name = NA
for (i in 1:dim(oc_ori2)[1]) {
  url = paste0("https://api.gbif.org/v1/dataset/",oc_ori2$datasetKey[i])
  temp = fromJSON(url)
  oc_ori2$name[i] = temp$title
}

oc_ids = pr %>%
  count(recordedByID) %>%
  rename(identifier = recordedByID)

oc_ids2 = pr %>%
  count(identifiedByID) %>%
  rename(identifier = identifiedByID) %>%
  rbind(oc_ids) %>%
  filter(!duplicated(identifier),
         !is.na(identifier)) %>%
  classifyPIDS()

count(oc_ids2,type) %>% arrange(desc(n))

pr_lsids = pr %>%
  group_by(institutionID,recordedByID) %>%
  summarize(occurrenceID = first(occurrenceID)) %>%
  classifySpecimenPIDS()
#filter(oc_ids2,type == "other") %>% count(identifier) %>% View()

oc_ids3 = oc_ids2 %>%
  #filter(type!="int",type!="other") %>%
  separate_rows(identifier,sep=";") %>%
  #mutate(test = nchar(identifier)) %>%
  classifyPIDS()

count(oc_ids3,type) %>% arrange(desc(n))

oc_ids3 %>%
  group_by(type) %>%
  summarise(sum = sum(n)) %>%
  arrange(desc(sum))

oc_ids2 %>%
  group_by(type) %>%
  summarise(sum = sum(n)) %>%
  arrange(desc(sum))

others_temp = filter(oc_ids3,type=="int")
others = pr %>%
  filter((!grepl("[^0-9]",recordedByID)&!is.na(recordedByID))|
           (!grepl("[^0-9]",identifiedByID)&!is.na(identifiedByID)))

orcids = oc_ids3 %>%
  get_items_with_prop("orcid","P496")

viaf = oc_ids3 %>%
  get_items_with_prop("viaf","P214")

ipni = oc_ids3 %>%
  get_items_with_prop("ipni","P586")

checkids = oc_ids3 %>%
  cleanPIDS(which="wikidata") %>%
  pull(wikidata) %>%
  c(pull(orcids,item)) %>%
  c(pull(viaf,item)) %>%
  c(pull(ipni,item)) %>%
  tibble(ids = .) %>%
  mutate(ids = gsub(".*/","",ids)) %>%
  distinct()

resu.r = puerki(checkids)

props = puerki_stack(resu.r) %>%
  tibble(props = .)

#make frequency table
props2 = count(props,props)

resu.p = puerki(props2,which="props")

props_ids = puerki_stack(resu.p,
                         which="labels")
props2$id = props_ids

props2 %<>%
  arrange(desc(n)) %>%
  mutate(perc = n/max(props2$n))

occupation = stack_count(resu.r,"P106")

occups = occupation %>%
  count(snak) %>%
  arrange(desc(n)) %>%
  mutate(perc = n/dim(checkids)[1])

fieldofwork = stack_count(resu.r,"P101")

fow = fieldofwork %>%
  count(snak) %>%
  arrange(desc(n)) %>%
  mutate(perc = n/dim(checkids)[1])

#resu.orcid = puerki(orcids,which = "id")
oc_ids3 %>% cleanPIDS(which="wikidata") %>% filter(!duplicated(wikidata)) %>% dim()
oc_ids3 %>% cleanPIDS(which="orcid") %>% filter(!duplicated(orcid)) %>% dim()
oc_ids3 %>% cleanPIDS(which="viaf") %>% filter(!duplicated(viaf)) %>% dim()
oc_ids3 %>% cleanPIDS(which="ipni") %>% filter(!duplicated(ipni)) %>% dim()

resu.r = fromJSON("gbif-pred-wd-results.json",simplifyVector = F)