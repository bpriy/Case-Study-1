library(rnoaa)
library(tidyverse)


cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

write.csv(cherry, "cherryrawdata.csv", row.names=FALSE)



