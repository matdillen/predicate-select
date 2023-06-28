library(tidyverse)
library(magrittr)

bn = read_csv("bionomia/bionomia-public-claims.csv")

count(bn,Subject,Object)
