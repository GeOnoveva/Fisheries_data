library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)

WGMIXFISH_SKA <- c("RJC","SKA","RAJ","RJA","RJB","RJC","RJE","RJF","RJH","RJI","RJM","RJO","RJR","SKX","SRX")
WGMIXFISH_splist <- c("ANF","ANK","BLL","CAA","COD","COE","DAB","FLE","GUG","GUR","HAD","HAL","HER","HKE","HOM","LBD","LEM","LEZ","LIN","MAC","MEG","MON","NEP","NOP","PLE","POK","POL","RJU","SKA","SDV","SOL","SPR","TUR","WHB","WHG","WIT")
effort <- effort %>% mutate(Species = ifelse(HOVEDART_FAO %in% WGMIXFISH_SKA, "SKA", HOVEDART_FAO),
                 Species = ifelse(Species %in% WGMIXFISH_splist, Species, "OTH"))
effort <- effort %>% mutate(Date = as.Date(STARTTIDSPUNKT), 
                            Year = lubridate::year(Date), 
                            Month = lubridate::month(Date), 
                            Quarter = lubridate::quarter(Date),
                            VesselLenthCategory = cut(STØRSTE_LENGDE, c(10, 24, 40)),
                            IntercatchMetierTag = metier_level_6,
                            Area = area)

effort <- effort %>% mutate(duration = as.numeric((strptime(STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S") - strptime(STARTTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S"))/86400))
effort <- effort %>% mutate(KWdays = duration * MOTORKRAFT * 0.735499)  # this is converting horse power to kilowatt


## Now generating the catch file
Catch <- effort %>% filter(Country == "NO") %>% 
  group_by(Species, Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area) %>%
  dplyr::summarise(
    Landings = sum(RUNDVEKT1),
    Value = sum(value),
    value_euro = sum(value_euro)
  )

Catch$FDFVessel<- NA
Catch$Discards<- NA

write.csv(Catch, file="output/Catch.csv")


## Now generating the effort file
Effort <- effort %>% filter(Country == "NO") %>% 
  group_by(Species, Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area) %>%
  dplyr::summarise(
    KWdays = sum(KWdays),
    DaysatSea = sum(duration),
    NoVessels = unique(REGM)
  )
Effort$FDFVessel<- NA
write.csv(Effort, file="output/Effort.csv")

