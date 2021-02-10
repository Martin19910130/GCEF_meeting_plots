####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####                                                                                  ####
####                            Overview Demographic data                             ####
####                                                                                  ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
rm(list = ls())
gc()

## Load needed Packages
library(dplyr)
library(ggplot2)

## Load data
dat_trans <- read.csv("https://raw.githubusercontent.com/Martin19910130/GCEF_meeting_plots/main/GCEF_demodata_complete_2020-11-20.csv")
dat_excl <- read.csv("https://raw.githubusercontent.com/Martin19910130/GCEF_meeting_plots/main/Tab_excl_BL_2020-11-20.csv")

## get a year column by using the date data
dat_trans$date <- as.Date(dat_trans$date, "%d.%m.%Y")
dat_trans$year <- format(dat_trans$date, "%Y")

dat_excl$date <- as.Date(dat_excl$date, "%d.%m.%Y")
dat_excl$year <- format(dat_excl$date, "%Y")

##create a column treatment where we combine landuse and climate
colnames(dat_trans)[which(c(colnames(dat_trans)) == "treatment")] <- "landuse"
dat_trans$treatment <- paste(dat_trans$climate, dat_trans$landuse, sep = "_")

colnames(dat_excl)[which(c(colnames(dat_excl)) == "treatment")] <- "landuse"
dat_excl$treatment <- paste(dat_excl$climate, dat_excl$landuse, sep = "_")
dat_excl <- subset(dat_excl, treatment != "_")

##change xleaf 2019 to numeric to create NA's
dat_trans$xleaf_spring_2019 <- as.numeric(dat_trans$xleaf_spring_2019)

## create survival column first for size plants and than for leaf
## area 18 - 19
dat_trans$surv18_19 <- ifelse(!is.na(dat_trans$area_spring_2018) & !is.na(dat_trans$area_spring_2019), 1, 0)
#dat_trans$surv18_19 <- ifelse(is.na(dat_trans$area_spring_2018), NA, dat_trans$surv18_19)

## area 19 - 20
dat_trans$surv19_20 <- ifelse(!is.na(dat_trans$area_spring_2019) & !is.na(dat_trans$area_spring_2020), 1, 0)
#dat_trans$surv19_20 <- ifelse(is.na(dat_trans$area_spring_2019), NA, dat_trans$surv19_20)

## leaf 18 - 19
dat_trans$surv18_19 <- ifelse(!is.na(dat_trans$xleaf_spring_2018) & !is.na(dat_trans$xleaf_spring_2019), 1, 
                              dat_trans$surv18_19)
#dat_trans$surv18_19 <- ifelse(is.na(dat_trans$xleaf_spring_2018), NA, dat_trans$surv18_19)

## leaf 19 - 20
dat_trans$surv19_20 <- ifelse(!is.na(dat_trans$xleaf_spring_2019) & !is.na(dat_trans$number_leafs_spring_2020), 1, 
                              dat_trans$surv19_20)
#dat_trans$surv19_20 <- ifelse(is.na(dat_trans$xleaf_spring_2019), NA, dat_trans$surv19_20)

## area 18 - 19
dat_excl$surv18_19 <- ifelse(!is.na(dat_excl$area_spring_2018) & !is.na(dat_excl$area_spring_2019), 1, 0)
#dat_excl$surv18_19 <- ifelse(is.na(dat_excl$area_spring_2018), NA, dat_excl$surv18_19)

## area 19 - 20
dat_excl$surv19_20 <- ifelse(!is.na(dat_excl$area_spring_2019) & !is.na(dat_excl$area_spring_2020), 1, 0)
#dat_excl$surv19_20 <- ifelse(is.na(dat_excl$area_spring_2019), NA, dat_excl$surv19_20)

## leaf 18 - 19
dat_excl$surv18_19 <- ifelse(!is.na(dat_excl$xleaf_spring_2018) & !is.na(dat_excl$xleaf_spring_2019), 1, 
                              dat_excl$surv18_19)
#dat_excl$surv18_19 <- ifelse(is.na(dat_excl$xleaf_spring_2018), NA, dat_excl$surv18_19)

## leaf 19 - 20
dat_excl$surv19_20 <- ifelse(!is.na(dat_excl$xleaf_spring_2019) & !is.na(dat_excl$number_leafs_spring_2020), 1, 
                              dat_excl$surv19_20)
## How many reps per year. br 
dat_trans$pl18 <- ifelse(!is.na(dat_trans$area_spring_2018) | !is.na(dat_trans$xleaf_spring_2018), 1, 0)
dat_trans$pl19 <- ifelse(!is.na(dat_trans$area_spring_2019) | !is.na(dat_trans$xleaf_spring_2019), 1, 0)
dat_trans$pl20 <- ifelse(!is.na(dat_trans$area_spring_2020) | !is.na(dat_trans$number_leafs_spring_2020), 1, 0)

pl_18_trans <- aggregate(dat_trans$pl18, 
                         by = list(dat_trans$species, dat_trans$treatment, 
                                   dat_trans$climate, dat_trans$landuse), FUN = sum)  
pl_19_trans <- aggregate(dat_trans$pl19, 
                         by = list(dat_trans$species, dat_trans$treatment, 
                                   dat_trans$climate, dat_trans$landuse), FUN = sum)
pl_20_trans <- aggregate(dat_trans$pl20, 
                         by = list(dat_trans$species, dat_trans$treatment, 
                                   dat_trans$climate, dat_trans$landuse), FUN = sum)

pl_18_trans$year <- 2018
pl_19_trans$year <- 2019
pl_20_trans$year <- 2020

plants_dev_trans <- rbind(pl_18_trans, pl_19_trans, pl_20_trans)
colnames(plants_dev_trans) <- c("species", "treatment", "climate", "landuse", "number", "year")

nr_dev_pl_trans <- aggregate(plants_dev_trans$number, 
                             by = list(plants_dev_trans$species, plants_dev_trans$treatment, 
                                       plants_dev_trans$climate, plants_dev_trans$year, 
                                       plants_dev_trans$landuse), 
                             FUN = sum)

colnames(nr_dev_pl_trans) <- c("species", "treatment", "climate", "year", "landuse","number")

rbPalette <- c("#0072B2", "#D55E00")


## exclosure
dat_excl$pl18 <- ifelse(!is.na(dat_excl$area_spring_2018) | !is.na(dat_excl$xleaf_spring_2018), 1, 0)
dat_excl$pl19 <- ifelse(!is.na(dat_excl$area_spring_2019) | !is.na(dat_excl$xleaf_spring_2019), 1, 0)
dat_excl$pl20 <- ifelse(!is.na(dat_excl$area_spring_2020) | !is.na(dat_excl$number_leafs_spring_2020), 1, 0)

pl_18_excl <- aggregate(dat_excl$pl18, by = list(dat_excl$species, dat_excl$treatment, 
                                                 dat_excl$climate, dat_excl$landuse), FUN = sum)  
pl_19_excl <- aggregate(dat_excl$pl19, by = list(dat_excl$species, dat_excl$treatment, 
                                                 dat_excl$climate, dat_excl$landuse), FUN = sum)
pl_20_excl <- aggregate(dat_excl$pl20, by = list(dat_excl$species, dat_excl$treatment, 
                                                 dat_excl$climate, dat_excl$landuse), FUN = sum)

pl_18_excl$year <- 2018
pl_19_excl$year <- 2019
pl_20_excl$year <- 2020

plants_dev_excl <- rbind(pl_18_excl, pl_19_excl, pl_20_excl)
colnames(plants_dev_excl) <- c("species", "treatment", "climate", "excl", "number", "year")

nr_dev_pl_excl <- aggregate(plants_dev_excl$number, 
                            by = list(plants_dev_excl$species, plants_dev_excl$treatment, 
                                      plants_dev_excl$climate, plants_dev_excl$year,
                                      plants_dev_excl$excl), FUN = sum)
colnames(nr_dev_pl_excl) <- c("species", "treatment", "climate", "year", "landuse", "number")

###
ez_gg <- function(data = nr_dev_pl_trans, ylab = "Number of individuals", xlab = "", 
                  shape_size = 1, 
                  main_title = "Individuals", sub_title = "", col_legend_title = "Climate", 
                  shape_legend_title = "Landuse")
{
  rbPalette <- c("#0072B2", "#D55E00")
  
  ind_trans <- ggplot(data, aes(x = year, y = number, color = climate, shape = landuse, 
                                           linetype = landuse)) + 
    geom_point(size = shape_size) + geom_line() + facet_wrap( ~ species) + ylab(ylab) + 
    theme_bw() + xlab(xlab) + ggtitle(main_title, sub_title) +
    scale_x_continuous(breaks = c(2018,2019,2020)) + scale_color_manual(values = rbPalette) + 
    guides(color = guide_legend(title = col_legend_title), shape = guide_legend(title = shape_legend_title), 
           linetype = guide_legend(shape_legend_title))
    
  
  return(ind_trans)
}

ez_gg(data = nr_dev_pl_excl)

