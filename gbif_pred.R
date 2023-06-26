library(tidyverse)
library(magrittr)
setwd("D:/apm/bicikl/7.3/predicate-select/predicate-select")
#oc = read_tsv("occurrence.txt",quote="",col_types = cols(.default = "c"))
pr = read_tsv("predicate-select2.txt",quote="",col_types = cols(.default = "c"))

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

orcids = oc_ids3 %>%
  get_items_with_prop("orcid","P496")

viaf = oc_ids3 %>%
  get_items_with_prop("viaf","P214")

ipni = oc_ids3 %>%
  get_items_with_prop("ipni","P586")

checkids = pubst %>%
  pull(item) %>%
  c(pull(pubst2,item)) %>%
  c(pull(pubst3,item)) %>%
  tibble(ids = .) %>%
  distinct() %>%
  mutate(ids = gsub(".*/","",ids))

resu.r = puerki(checkids)

props = puerki_stack(resu.r) %>%
  tibble(props = .)

#make frequency table
props2 = count(props,props)

resu.p = puerki(props2,which="props")

props_ids = puerki_stack(resu.p,
                         which="labels")
props2$id = props_ids

occupation = stack_count(resu.r,"P106")

occups = occupation %>%
  count(snak) %>%
  arrange(desc(n)) %>%
  mutate(cum = cumsum(n),
         cump = cum/sum(n))