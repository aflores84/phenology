library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

setwd("c:/UTEP/ThesisRelated/Data/Phenology/Data/LATR")
getwd() 
latr <- read_excel("LATR_20100309-20191220.xlsx")

latr$date_id <- as.Date(latr$date_id)
setnames(latr, old = c('BE01','BE02', 'BE03', 'BE04', 'BE05', 'BE06', 'BE07', 'BE08'),
         new = c('Breaking Leaf Bud','Young Unfolded Leaves', 'Flower buds', 
                 'Open Flowers', 'Full flowering', 'Fruits', 'Ripe fruits','Fruits from past growing season'))

latr_tidy <- latr %>% 
  select(date_id, plant_id, `Breaking Leaf Bud`:`Ripe fruits`) %>%   #pick variables by their name
  gather(phenophase, occurrence, -date_id, -plant_id)%>%
  mutate(year = year(date_id))


#separate plant ID into additional column (don't need to load any additional libraries)
latr_tidy$transect <- sapply(strsplit(as.character(latr_tidy$plant_id),"_"),"[",1)

latr_tidy$plot <- paste(sapply(strsplit(as.character(latr_tidy$plant_id),"_"),"[",1), 
                        sapply(strsplit(as.character(latr_tidy$plant_id),"_"),"[",2), sep = "_")

latr_tidy$species <- sapply(strsplit(as.character(latr_tidy$plant_id),"_"),"[",3)


####################CALCULATE % Phenphase for the whole site ###########################################################################

# remove duplicates from data
latr_tidy_2 <- unique(latr_tidy)

# calculate number of individuals observed for each date and phenophase
latr_count_total <- latr_tidy_2 %>%
  group_by(date_id, year, phenophase, species) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

# calculate the total number of phenophase observations for each week(date)
latr_count_obs <- latr_tidy_2 %>%
  group_by(date_id, year, phenophase,species) %>%
  summarise(pheno_obs = sum(occurrence))


#######################latr_count_obs/latr_count_total#####################
# combine the total counts and the 1 counts and keep all the rows that exist in total count data frame
latr_count <- merge(latr_count_total,latr_count_obs,by=c("date_id","year","phenophase","species"),all.x=TRUE)

# calculate the percent of individuals in a phenophase on each date
latr_count <- mutate(latr_count,
                     pheno_perc = (pheno_obs/total_obs)*100)%>% 
  mutate(doy = yday(date_id))  #added this one 20191010, but it already included it before this date on previous latr_count function


latr_all_dates <- latr_tidy_2 %>% 
  filter(occurrence==1) %>%
  group_by(year,plant_id,phenophase) %>%
  mutate(pheno_date=(date_id))%>%
  mutate(doy = yday(pheno_date))


latr_first_date <- latr_tidy_2 %>% 
  filter(occurrence==1 & !phenophase %in% c('>50% leaves fallen','All leaves fallen')) %>%
  group_by(year,plant_id,phenophase) %>%
  summarise(pheno_start=min(date_id))%>%
  mutate(doy = yday(pheno_start))

latr_count <- latr_count %>%
  # divide phenophase into life strategy
  mutate(pheno_type = ifelse(phenophase %in% c("Breaking Leaf Bud","Young Unfolded Leaves"), "primary production", "reproduction")) %>%
  mutate(doy = yday(date_id))


#just changed the "" information in the  "" to change phenophases
ggplot(subset(latr_count,pheno_type=="reproduction"),
       aes(date_id,pheno_perc,colour=phenophase))+geom_line()+ggtitle("Creosote")+facet_grid(phenophase~.)+
  theme(axis.title.x = element_text(face = "bold", size = 25), axis.text=element_text(size=30), 
        axis.title.y = element_text(face = "bold", size = 25), axis.text.y = element_text(size = 20),
        strip.text.y = element_text(size=20, face="bold"),
        plot.title = element_text(size = 20, face = "bold", color = "red"))
