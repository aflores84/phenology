library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

setwd("c:/UTEP/ThesisRelated/Data/Phenology/Data/FLCE")
getwd() 
flce <- read_excel("FLCE_2010-20181102v2.xlsx")

######RENAME COLUMNS############################################################
setnames(flce, old = c('DS01','DS02', 'DS03', 'DS04', 'DS05', 'DS06', 'DS07', 'DS08', 'DS09', 'DS10', 'DS11', 'DS12'),
         new = c('Breaking Leaf Bud','Young Unfolded Leaves', '>25% Full Leaf Size', '>= 75% full leaf size', '>50% leaves fallen',
                 'All leaves fallen', 'flower buds', 'Open flowers', 'Full Flowering', 'Fruits', 'Ripe fruits', 'Recent Fruit drops' ))




flce_tidy <- flce %>% 
  select(date_id, plant_id, `Breaking Leaf Bud`:`Recent Fruit drops`) %>%   #pick variables by their name
  gather(phenophase, occurrence, -date_id, -plant_id)%>%
  mutate(year = year(date_id))

# remove duplicates from data
flce_tidy_2 <- unique(flce_tidy)

##############transect/plot/species############################################################


#separate plant ID into additional column (don't need to load any additional libraries)

flce_tidy$transect <- sapply(strsplit(as.character(flce_tidy$plant_id),"_"),"[",1)

flce_tidy$plot <- paste(sapply(strsplit(as.character(flce_tidy$plant_id),"_"),"[",1), 
                        sapply(strsplit(as.character(flce_tidy$plant_id),"_"),"[",2), sep = "_")  #what is sep = "_"


flce_tidy$species <- sapply(strsplit(as.character(flce_tidy$plant_id),"_"),"[",3)

####################CALCuLATE % Phenphase for the whole site ###########################################################################
# calculate total number of individuals observed per year and transect
flce_count_obs <- flce_tidy_2 %>%
  group_by(date_id, year, phenophase) %>%
  summarise(pheno_obs = sum(occurrence))


# calculate number of individuals observed for each date and phenophase
flce_count_total <- flce_tidy_2 %>%
  group_by(date_id, year, phenophase) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

flce_count <- merge(flce_count_total,flce_count_obs,by=c("date_id","year","phenophase"),all.x=TRUE)

# give labels according to the phenophase type
flce_count_labels <- flce_count %>%
  # divide phenophase into life stages
  mutate(pheno_type = ifelse(phenophase %in% c("Breaking Leaf Bud","Young Unfolded Leaves", ">25% Full Leaf Size",
                                               ">= 75% full leaf size", ">50% leaves fallen",
                                               "All leaves fallen"), "primary production", "reproduction")) %>%
  # group into life stages
  #mutate(pheno_detail = factor(ifelse(phenophase %in% c("A-Breaking Leaf Bud","B-Young Unfolded Leaves"),"leaf out",
  #                            ifelse(phenophase %in% c("C->25% Full Leaf Size", "D->= 75% full leaf size"), "greening", "senescing")),
  #                             levels=c("leaf out","greening","senescing"))) %>% 
  # create day of year column
  mutate(doy = yday(date_id))

flce_count_labels$phenophase = factor(flce_count_labels$phenophase, 
                                      levels = c("Breaking Leaf Bud","Young Unfolded Leaves",
                                                 ">25% Full Leaf Size", ">= 75% full leaf size",
                                                 ">50% leaves fallen", "All leaves fallen"))



