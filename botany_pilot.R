library(tidyverse)
library(magrittr)
library(jsonlite)

bp = list.files("botany_pilot",
                full.names = T,
                pattern="*.csv")

for (i in 1:length(bp)) {
  if (i==1) {
    oc = read_csv(bp[i],
                  col_types = cols(.default = "c"))
  } else {
    oc_temp = read_csv(bp[i],
                       col_types = cols(.default = "c"))
    oc = rbind(oc,oc_temp)
  }
}

source("clean_functions.R")

oc_u = filter(oc,!duplicated(recordedBy_IRI)) %>%
  rename(identifier = recordedBy_IRI) %>%
  classifyPIDS()

#count(oc_u,type) %>% arrange(desc(n)) %>% mutate(perc = n/sum(n))

oc_lsids = oc %>%
  group_by(institutionID,recordedBy_IRI) %>%
  summarize(cspp = first(cspp)) %>%
  classifySpecimenPIDS()

oc_lsids %>% make_quickstatements()

checkids = oc_u %>%
  filter(type == "wikidata") %>%
  select(identifier) %>%
  rename(ids = identifier) %>%
  mutate(ids = gsub(".*/","",ids)) %>%
  filter(!duplicated(ids))

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

resu.r = fromJSON("bp.json",simplifyVector = F)
