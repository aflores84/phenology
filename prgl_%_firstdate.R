library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

#####Test change 

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

prgl_tidy <- unique(prgl_tidy)

##############transect/plot/species############################################################

#separate plant ID into additional column (don't need to load any additional libraries)
# this splits 
prgl_tidy$transect <- sapply(strsplit(as.character(prgl_tidy$plant_id),"_"),"[",1)

# from S_S1_PRGL_01, it seperates S_S1
prgl_tidy$plot <- paste(sapply(strsplit(as.character(prgl_tidy$plant_id),"_"),"[",1), 
                        sapply(strsplit(as.character(prgl_tidy$plant_id),"_"),"[",2), sep = "_")  #what is sep = "_"
#FROM S_S1_PRGL_01, it seperates PRGL
prgl_tidy$plot <- paste(sapply(strsplit(as.character(prgl_tidy$plant_id),"_"),"[",2), sep = "_")

prgl_tidy$species <- sapply(strsplit(as.character(prgl_tidy$plant_id),"_"),"[",3)

ggplot(prgl_tidy, aes(occurrence))+ geom_density() + facet_grid(year~phenophase)

############################prgl_count_obs/prgl_count_total############################################
# TOTAL NUMBER OF PHENOPHASE EXPRESSIONS
prgl_count_total <- prgl_tidy %>%
  group_by(date_id, year, phenophase) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

# TOTAL OBSERVATIONS MEASURED
prgl_count_obs <- prgl_tidy %>%
  group_by(date_id, year, phenophase) %>%
  summarise(pheno_obs = sum(occurrence))
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
  mutate(doy = yday(date_id))

prgl_count_labels$phenophase = factor(prgl_count_labels$phenophase, ##this rearranges the phenophases in order instead of starting with >=75% full leaf size
                                      levels = c("Breaking Leaf Bud","Young Unfolded Leaves",
                                                 ">25% Full Leaf Size", ">= 75% Full Leaf Size",
                                                 ">50% Leaves Fallen", "All Leaves Fallen"))

#plots primary phenophases as all years in X variable and % phenophase as Y variable
ggplot(subset(prgl_count_labels,pheno_type=="primary production"),
       aes(date_id,pheno_perc,colour=phenophase))+geom_line(size=1.2)+ggtitle("Honey mesquite") + facet_grid(phenophase~.) +
  theme(axis.title.x = element_text(face = "bold", size = 25), axis.text=element_text(size=25), 
        axis.title.y = element_text(face = "bold", size = 25), axis.text.y = element_text(size = 15),
        strip.text.y = element_text(size=25, face="bold"),
        plot.title = element_text(size = 30, face = "bold", color = "red"))

############################################################ALL DATES SHOWN ##########################################################

prgl_all_dates <- prgl_tidy %>% 
  filter(occurrence==1) %>%
  group_by(year,plant_id,phenophase) %>%
  mutate(pheno_date=(date_id))%>%
  mutate(doy = yday(pheno_date))

prgl_all_dates$phenophase = factor(prgl_all_dates$phenophase, ##this rearranges the phenophases in order instead of starting with >=75% full leaf size
                                      levels = c("Breaking Leaf Bud","Young Unfolded Leaves",
                                                 ">25% Full Leaf Size", ">= 75% Full Leaf Size",
                                                 ">50% Leaves Fallen", "All Leaves Fallen"))

#PLOTS ALL YEARS accross all primary production phenophases
ggplot(subset(prgl_all_dates), aes(doy,fill=phenophase))+geom_density()+facet_grid(year~phenophase)+
  theme(axis.title.x = element_text(face = "bold", size = 15), axis.text=element_text(size=20))

ggplot(prgl_all_dates, aes(doy, phenophase, linetype = phenophase))

#breaking leaf
ggplot(subset(prgl_all_dates, phenophase=="Breaking Leaf Bud"), aes(doy)) + xlim(c(0,365)) + 
  theme(axis.title.x = element_text(face = "bold", size = 25), axis.text=element_text(size=30), 
        axis.title.y = element_text(face = "bold", size = 25), axis.text.y = element_text(size = 20),
        strip.text.y = element_text(size=20, face="bold"),
        plot.title = element_text(size = 30, face = "bold", color = "red")) +
  geom_bar() + ggtitle("PRGL:Breaking Leaf Bud-All Dates") + facet_grid(year~.)

ggplot(prgl_all_dates, aes(phenophase,doy,color=factor(year)))+geom_point()+facet_grid(year~.)#wierd graph

#####################################################FIRST DATE OF OBSERVATIONS##################################################
prgl_first_date <- prgl_tidy %>% 
  filter(occurrence==1 & !phenophase %in% c('>50% leaves fallen','All leaves fallen')) %>%
  group_by(year,plant_id,phenophase) %>%
  summarise(pheno_start=min(date_id))%>%
  mutate(doy = yday(pheno_start))

ggplot(subset(prgl_first_date, phenophase=="Breaking Leaf Bud"), aes(doy)) + geom_bar() + ggtitle("Breaking Leaf Bud: First Dates:All Years") +
  theme(axis.title.x = element_text(face = "bold", size = 25), axis.text=element_text(size=25), 
        axis.title.y = element_text(face = "bold", size = 25), axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold", color = "red"))
