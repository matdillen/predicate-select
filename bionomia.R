library(tidyverse)
library(magrittr)

bn = read_csv("bionomia/bionomia-public-claims.csv")

bn_c = count(bn,Subject,Object)

bn_ca = count(bn,Object,Predicate)
