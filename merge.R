wd = list.files(pattern = "wikidata_ids_.*")
allwd = tibble(item = NA,src = NA)
for (i in wd) {
  a = read_tsv(i)
  colnames(a) = "item"
  a$src = gsub("wikidata_ids_","",i) %>%
    gsub(".txt","",.)
  allwd = rbind(allwd,a)
}
allwd = allwd[-1,]

dups = count(allwd,item)
