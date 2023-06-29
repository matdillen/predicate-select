library(tidyverse)
library(magrittr)
library(jsonlite)

bn = read_csv("bionomia/bionomia-public-claims.csv")

bn_c = count(bn,Subject,Object)

bn_ca = count(bn,Object,Predicate)

bn_u = bn_ca %>%
  rename(identifier = Object) %>%
  classifyPIDS() %>%
  filter(!duplicated(identifier))

source("clean_functions.R")

orcids = bn_u %>%
  get_items_with_prop("orcid","P496")

checkids_bn = bn_u%>%
  cleanPIDS(which="wikidata") %>%
  pull(wikidata) %>%
  c(pull(orcids,item)) %>%
  tibble(ids = .) %>%
  mutate(ids = gsub(".*/","",ids),
         ids = gsub("[^0-9|Q]","",ids)) %>%
  distinct()

resu.bn = puerki(checkids_bn)

props_bn = puerki_stack(resu.bn) %>%
  tibble(props = .)

#make frequency table
props_bn2 = count(props_bn,props)

resu_bn.p = puerki(props_bn2,which="props")

props_ids = puerki_stack(resu_bn.p,
                         which="labels")
props_bn2$id = props_ids

props_bn2 %<>%
  arrange(desc(n)) %>%
  mutate(perc = n/max(props_bn2$n))

occupation_bn = stack_count(resu.bn,"P106")

occups_bn = occupation_bn %>%
  count(snak) %>%
  arrange(desc(n)) %>%
  mutate(cum = cumsum(n),
         cump = cum/sum(n))

#resu.bn = fromJSON("bionomia.json",simplifyVector = F)
