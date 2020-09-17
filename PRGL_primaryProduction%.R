library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

setwd("c:/UTEP/ThesisRelated/Data/Phenology/Data/PRGL")
getwd() 
prgl <- read_excel("PRGL_2010_20191220.xlsx")

#deleting #3s if 4s, if #3s and #4s have a 1, put DS03's value as zero into new column DS03_1
prgl$DS03_1<-ifelse(prgl$DS04==1, 0, prgl$DS03)


setnames(prgl, old = c('DS01','DS02', 'DS03_1', 'DS04', 'DS05', 'DS06', 'DS07', 'DS08', 'DS09', 'DS10', 'DS11'), #keeps this order
         new = c('Breaking Leaf Bud','Young Unfolded Leaves', '>25% Full Leaf Size', '>= 75% Full Leaf Size', '>50% Leaves Fallen',
                 'All Leaves Fallen', 'Flower Buds', 'Open Flowers', 'Full Flowering', 'Fruits', 'Ripe Fruits'))

prgl_tidy <- prgl %>%
  select(date_id, plant_id,`Breaking Leaf Bud`,`Young Unfolded Leaves`,`>25% Full Leaf Size`,`>= 75% Full Leaf Size`,
         `>50% Leaves Fallen`,`All Leaves Fallen`) %>%   #pick variables by their name
  # select(-date_string) %>% # drop specifc columns
  gather(phenophase, occurrence, -date_id, -plant_id)%>%
  mutate(year = year(date_id))

prgl_tidy_2 <- unique(prgl_tidy)

# TOTAL NUMBER OF PHENOPHASE EXPRESSIONS
prgl_count_total <- prgl_tidy_2 %>%
  group_by(date_id, year, phenophase) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

# TOTAL OBSERVATIONS MEASURED
prgl_count_obs <- prgl_tidy_2 %>%
  group_by(date_id, year, phenophase) %>%
  summarise(pheno_obs = sum(occurrence))
############################prgl_count_obs/prgl_count_total############################################

# combine the total counts and the 1 counts and keep all the rows that exist in total count data frame
prgl_count <- merge(prgl_count_total,prgl_count_obs,by=c("date_id","year","phenophase"),all.x=TRUE)

# calculate the percent of individuals in a phenophase on each date
prgl_count <- mutate(prgl_count,pheno_perc = (pheno_obs/total_obs)*100) %>%
  mutate(doy = yday(date_id))#added this one 20191010, but it already included it before this date on previous prgl_count function

# give labels according to the phenophase type
prgl_count_labels <- prgl_count %>%
  # divide phenophase into life stages
  mutate(pheno_type = ifelse(phenophase %in% c("Breaking Leaf Bud","Young Unfolded Leaves", ">25% Full Leaf Size",
                                               ">= 75% Full Leaf Size", ">50% Leaves Fallen",
                                               "All Leaves Fallen"), "primary production", "reproduction")) %>%
  # group into life stages
  #mutate(pheno_detail = factor(ifelse(phenophase %in% c("A-Breaking Leaf Bud","B-Young Unfolded Leaves"),"leaf out",
  #                            ifelse(phenophase %in% c("C->25% Full Leaf Size", "D->= 75% full leaf size"), "greening", "senescing")),
  #                             levels=c("leaf out","greening","senescing"))) %>% 
  # create day of year column
  mutate(doy = yday(date_id))

prgl_count_labels$phenophase = factor(prgl_count_labels$phenophase, 
                                      levels = c("Breaking Leaf Bud","Young Unfolded Leaves",
                                                 ">25% Full Leaf Size", ">= 75% Full Leaf Size",
                                                  ">50% Leaves Fallen", "All Leaves Fallen"))

#plots primary phenophases as all years in X variable and % phenophase as Y variable
ggplot(subset(prgl_count_labels,pheno_type=="primary production"),
       aes(date_id,pheno_perc,colour=phenophase))+geom_line(size=1.2)+ggtitle("Honey mesquite") + facet_grid(phenophase~.) +
  theme(axis.title.x = element_text(face = "bold", size = 25), axis.text=element_text(size=30), 
        axis.title.y = element_text(face = "bold", size = 25), axis.text.y = element_text(size = 25),
        strip.text.y = element_text(size=25, face="bold"),
        plot.title = element_text(size = 30, face = "bold", color = "red"))
