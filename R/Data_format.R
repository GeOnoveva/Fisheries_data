#### Set the library path
.libPaths(c("C:/Program Files/R/R-3.6.2/library", "C:/Program Files/R/R-4.0.5/library"))


#### Loading libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)

#### Reading in the data
Data <- readRDS("dat.RDS")

#### Specifying the data.call requirements
WGMIXFISH_area <- c("27.3.a.20", "27.3.a.21", "27.3.a", "27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.26", "27.3.d.27", "27.3.d.28", "27.3.d.28.1", "27.3.d.28.2", "27.3.d.29", "27.3.d.30", "27.3.d.31", "27.3.d.32", "27.4.a", "27.4.b", "27.4.c", "27.6.a", "27.6.b", "27.7.a", "27.7.b", "27.7.c", "27.7.d", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.j", "27.7.k", "27.8.a", "27.8.b", "27.8.c", "27.8.d", "27.9.a")
WGMIXFISH_SKA <- c("RJC","SKA","RAJ","RJA","RJB","RJC","RJE","RJF","RJH","RJI","RJM","RJO","RJR","SKX","SRX")
WGMIXFISH_splist <- c("ANF","ANK","BLL","CAA","COD","COE","DAB","FLE","GUG","GUR","HAD","HAL","HER","HKE","HOM","LBD","LEM","LEZ","LIN","MAC","MEG","MON","NEP","NOP","PLE","POK","POL","RJU","SKA","SDV","SOL","SPR","TUR","WHB","WHG","WIT")

#### Now filtering/modifying the data accordingly
  ### converting the species code to the one required
    Data <- Data %>% mutate(Species = ifelse(HOVEDART_FAO %in% WGMIXFISH_SKA, "SKA", HOVEDART_FAO),
                   Species = ifelse(Species %in% WGMIXFISH_splist, Species, "OTH"))
  ### Selecting only the data from the required area
    Data <- Data %>% filter(area %in% WGMIXFISH_area)
  ### Changing names to match the final data call table
    Data <- Data %>% mutate(Date = as.Date(STARTTIDSPUNKT), 
                            Year = lubridate::year(Date), 
                            Month = lubridate::month(Date), 
                            Quarter = lubridate::quarter(Date),
                            VesselLenthCategory = cut(STØRSTE_LENGDE, c(10, 24, 40, 10000), right = FALSE),
                            IntercatchMetierTag = metier_level_6,
                            Area = area)
    ### Create vessel length bins with the required labels 
      Data$VesselLenthCategory = ifelse(Data$VesselLenthCategory=="[10,24)", "10<24m", 
                                    ifelse(Data$VesselLenthCategory=="[24,40)", "24<40m", 
                                      ifelse(Data$VesselLenthCategory=="[40,1e+04)", "≥40m", Data$VesselLenthCategory)))
    ### Creating a few more variables
      Data <- Data %>% mutate(duration = as.numeric((strptime(STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S") - strptime(STARTTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S"))/86400))
      Data <- Data %>% mutate(KWdays = duration * MOTORKRAFT * 0.735499)  # this is converting horse power to kilowatt


#### verification 1: making sure that each vessel has consistent length info across time
  test <- with(Data, table(RC, VesselLenthCategory, useNA = "always"))
  to_look <- which(apply(cbind(apply(test[,1:3], 1, sum), test[,4]),1,function(x) sum(x>0)) == 2)
  
  test[to_look,]
  
  Data %>% filter(RC %in% names(to_look)) %>% group_by(RC, FANGSTÅR, STØRSTE_LENGDE) %>% summarize(n=n()) %>% View()
  
  # --> conclusion: we can safely replace all the NA with the corresponding length

  Data1 <- Data %>% filter(RC %in% names(to_look)) %>% group_by(RC) %>% mutate(STØRSTE_LENGDE = unique(STØRSTE_LENGDE)[!is.na(unique(STØRSTE_LENGDE))])
  Data2 <- Data %>% filter(! RC %in% names(to_look))
  
  Data <- rbind(Data1, Data2)
  
## Now generating the catch file
Catch <- Data %>% 
  group_by(Species, Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area) %>%
  dplyr::summarise(
    Landings = sum(RUNDVEKT1),
    Value = sum(value),
    value_euro = sum(value_euro)
  )

Catch$FDFVessel<- NA
Catch$Discards<- NA
Catch$Country<- "NO"
Catch$ID<- 1:nrow(Catch)

write.csv(Catch, file="output/Catch.csv")


## Now generating the Data file
Effort <- Data %>%  
  group_by(Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area) %>%
  dplyr::summarise(
    KWdays = sum(KWdays),
    DaysatSea = sum(duration),
    NoVessels = length(unique(REGM))
  )
Effort$FDFVessel<- NA
Effort$Country<- "NO"
Effort$ID<- 1:nrow(effort)
write.csv(Effort, file="output/Effort.csv")

