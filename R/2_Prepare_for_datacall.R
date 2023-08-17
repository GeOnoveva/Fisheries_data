#### Set the library path
#.libPaths(c("C:/Program Files/R/R-3.6.2/library", "C:/Program Files/R/R-4.0.5/library"))


#### Loading libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)

#### Reading in the data
Data <- readRDS("dat.RDS")
Data <- readRDS("dat_new.RDS") # with the data i genrated myself

if(TRUE %in% grepl("area.x", names(Data))) Data$area = Data$area.x

#### Reading the shapefiles (needed to define nephrops FUs)
nephrops <- read_sf("data/Nephrops_FU_20160621.shp")
projection <- st_crs(nephrops)
world <- st_read("data/ne_10m_land.shp")
Atlantic <- st_crop(world, c(xmin = -40, ymin = 46, xmax = 32, ymax = 80))


#### Specifying the data.call requirements and specific species grouping
WGMIXFISH_area <- c("27.3.a.20", "27.3.a.21", "27.3.a", "27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.26", "27.3.d.27", "27.3.d.28", "27.3.d.28.1", "27.3.d.28.2", "27.3.d.29", "27.3.d.30", "27.3.d.31", "27.3.d.32", "27.4.a", "27.4.b", "27.4.c", "27.6.a", "27.6.b", "27.7.a", "27.7.b", "27.7.c", "27.7.d", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.j", "27.7.k", "27.8.a", "27.8.b", "27.8.c", "27.8.d", "27.9.a")
WGMIXFISH_SKA <- c("RJC","SKA","RAJ","RJA","RJB","RJC","RJE","RJF","RJH","RJI","RJM","RJO","RJR","SKX","SRX")
WGMIXFISH_SDV <- c("DGS", "DGH", "DGX", "DGZ", "SDV")
WGMIXFISH_splist <- c("ANF","ANK","BLL","CAA","COD","COE","DAB","FLE","GUG","GUR","HAD","HAL","HER","HKE","HOM","LBD","LEM","LEZ","LIN","MAC","MEG","MON","NEP","NOP","PLE","POK","POL","RJU","SKA","SDV","SOL","SPR","TUR","WHB","WHG","WIT")

#### Now filtering/modifying the data accordingly
#### EDIT October 2022: filtering on FANGSTART_FAO instead of HOVEDART_FAO

  ### Some data correction
    Data$ID <- 1:nrow(Data)    
    (subset(Data, FANGSTART_FAO == "NEP")[which(subset(Data, FANGSTART_FAO == "NEP")$START_LT > 80),])[,c("START_LT", "STOPP_LT")]
    to_correct <- (subset(Data, FANGSTART_FAO == "NEP")[which(subset(Data, FANGSTART_FAO == "NEP")$START_LT > 80),"ID"])
    Data[to_correct, "START_LT"] <- Data[to_correct, "START_LT"] - 30
    
  ### Now adding the nephrops FUs to the data (only basing it off on the starting lat/lon for simplicity)
    Data_sf <- Data %>% st_as_sf(crs = 4326, coords = c("START_LG", "START_LT")) %>% st_cast("POINT")
    nephrops_FU <- as.matrix(st_intersects(Data_sf, nephrops))
    which_area <- apply(nephrops_FU, 1, function(x) ifelse(TRUE %in% x, which(x == TRUE), NA))
    Data$nephrops_FU <- paste0("NEP.FU.", nephrops$FU[which_area])
    Data$nephrops_FU <- ifelse(Data$nephrops_FU == "NEP.FU.NA", paste0("NEP.OUT", Data$area), Data$nephrops_FU)
    
# Plot
    ggplot(nephrops) + geom_sf() + geom_sf(data = Atlantic) +  geom_sf_label(aes(label= FU)) + 
      geom_point(data =Data %>% filter(FANGSTART_FAO == "NEP"), aes(x=START_LG, y=START_LT)) + 
      theme_bw()
        
    
  ### converting the species code to the one required
    Data <- Data %>% mutate(Species = ifelse(FANGSTART_FAO %in% WGMIXFISH_SKA, "SKA", FANGSTART_FAO),
                            Species = ifelse(FANGSTART_FAO %in% WGMIXFISH_SDV, "SDV", FANGSTART_FAO),
                   Species = ifelse(Species %in% WGMIXFISH_splist, Species, "OTH"))
    Data$Species <- ifelse(Data$Species == "NEP", Data$nephrops_FU, Data$Species)
  ### Selecting only the data from the required area
    Data <- Data %>% filter(area %in% WGMIXFISH_area)
  ### Changing names to match the final data call table
    Data <- Data %>% mutate(Date_start = as.Date(STARTTIDSPUNKT), 
                            Date_end = as.Date(STOPPTIDSPUNKT),
                            Year = FANGSTÅR, 
                            Month_start = lubridate::month(Date_start), 
                            Month_end = lubridate::month(Date_end), 
                            Quarter_start = lubridate::quarter(Date_start),
                            Quarter_end = lubridate::quarter(Date_end),
                            VesselLenthCategory = cut(STØRSTE_LENGDE, c(10, 24, 40, 10000), right = FALSE),
                            IntercatchMetierTag = metier_level_6,
                            Area = area)
    ## Sometimes, info on STARTTIDSPUNKT is missing and we only have STOPPTIDSPUNKT - 
    ## so in the above we created two version using each and now merging. There are many non-unique but thenm just using the start date as reference (if possible)
    Data <- Data %>% mutate(Quarter = unique(c(Quarter_start, Quarter_end))[1])
    Data <- Data %>% mutate(Quarter = Quarter_start)
    
    ### Create vessel length bins with the required labels AND completing the data when info is missing
      Data$VesselLenthCategory = ifelse(Data$VesselLenthCategory=="[10,24)", "10<24m", 
                                    ifelse(Data$VesselLenthCategory=="[24,40)", "24<40m", 
                                      ifelse(Data$VesselLenthCategory=="[40,1e+04)", "≥40m", Data$VesselLenthCategory)))
      
      Data$VesselLenthCategory = ifelse((is.na(Data$VesselLenthCategory)  & !is.na(Data$STØRSTE_LENGDE)),
                                        ifelse(Data$STØRSTE_LENGDE< 24, "10<24m", 
                                               ifelse((Data$STØRSTE_LENGDE>= 24 & Data$STØRSTE_LENGDE < 40 ), "24<40m", "≥40m")), Data$VesselLenthCategory)
                                                     
                                        
      ### Creating a few more variables
      Data <- Data %>% mutate(duration = as.numeric((strptime(STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S") - strptime(STARTTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S"))/86400))
      Data <- Data %>% mutate(KWdays = duration * MOTORKRAFT * 0.735499)  # this is converting horse power to kilowatt


#### verification 1: making sure that each vessel has consistent length info across time 
  test <- with(Data, table(RC, VesselLenthCategory, useNA = "always"))
  to_look <- which(apply(cbind(apply(test[,1:3], 1, sum), test[,4]),1,function(x) sum(x>0)) == 2)
  no_info <- which(apply(cbind(apply(test[,1:3], 1, sum), test[,4]),1,function(x) (x[1]==0 & x[2]>0)) == TRUE)
  
  ##### It seems like there are some vessels (for some occasion) miss the vessel length info
    if (length(to_look) > 0) {
      test[to_look,]
      vessel_tocorrect <- test[to_look,-4] %>% as.data.frame() %>% filter(Freq != 0)
      sel_tocorrect = which(is.na(Data$VesselLenthCategory)==TRUE & (Data$RC %in% vessel_tocorrect$RC))
      Data[sel_tocorrect,"VesselLenthCategory"] <-  as.character(vessel_tocorrect$VesselLenthCategory)[match(Data[sel_tocorrect,"RC"], vessel_tocorrect$RC)]
    }  
  
  ##### What are the vessels without any length info
  test[no_info,]
  
  Data %>% filter(RC %in% names(to_look)) %>% group_by(RC, FANGSTÅR, STØRSTE_LENGDE) %>% summarize(n=n()) %>% View()

  # --> conclusion: we can safely replace all the NA with the corresponding length

  # just FYI: checking why some vessels do not have any length, motor power, info 
  Data %>% filter(RC %in% names(no_info)) %>% group_by(RC,  FANGSTART_FAO) %>% summarize(n=n()) %>% View()
  Data %>% filter(FANGSTART_FAO %in% "LAH") %>% group_by(RC) %>% summarize(n=n(), catch = sum(RUNDVEKT, na.rm=T)) 
  
  Data1 <- Data %>% filter(RC %in% names(to_look)) %>% group_by(RC) %>% mutate(STØRSTE_LENGDE = unique(STØRSTE_LENGDE)[!is.na(unique(STØRSTE_LENGDE))])
  Data2 <- Data %>% filter(! RC %in% names(to_look))
  
  Data <- rbind(Data1, Data2)

## Testing if there is any NAs in the above cleaned data. If yes, need further cleaning
  apply(Data %>% dplyr::select(Species, Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area), 2, function(x) sum(is.na(x)))
  View(Data[is.na(Data[,c('VesselLenthCategory')]),c('STØRSTE_LENGDE', 'IntercatchMetierTag', 'VesselLenthCategory', 'BRUTTOTONNASJE', 'MOTORKRAFT')])
  table(Data[is.na(Data[,c('VesselLenthCategory')]),'RC'])         
  
  # among other exclude these kelp boats!
  Data <- Data %>% filter(!RC %in% names(no_info))
       
## Now generating the catch file
Catch <- Data %>% 
  group_by(Species, Year, Quarter, IntercatchMetierTag, VesselLenthCategory, Area) %>%
  dplyr::summarise(
    Landings = sum(RUNDVEKT1, na.rm=T)/1000,
    Value = sum(value),
    value_euro = sum(value_euro)
  )

Catch$FDFVessel<- NA
Catch$Discards<- NA
Catch$Country<- "NO"
Catch$ID<- 1:nrow(Catch)
Catch$Quarter <- paste0("Q", Catch$Quarter)



#write.csv(Catch, file="output/Catch_new.csv")
write.csv(Catch, file="output/Catch_corrected.csv")


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
Effort$ID<- 1:nrow(Effort)
# Effort$Quarter1 <- paste("Q", Effort$Quarter, sep="")
Effort$Quarter <- paste("Q", Effort$Quarter, sep="")
Effort <- Effort[-which(Effort$Quarter == "QNA"),]

#write.csv(Effort, file="output/Effort_new.csv")
write.csv(Effort, file="output/Effort_corrected.csv")


ggplot(data = Effort %>% group_by(IntercatchMetierTag, Year) %>% summarize(KWH = sum(KWdays, na.rm=T)*24/1000), aes(x=Year, y=KWH)) + 
  geom_line() + theme_bw() + facet_wrap(~IntercatchMetierTag)

ggplot(data = Effort %>% group_by(Year) %>% summarize(KWH = sum(KWdays, na.rm=T)*24/1000), aes(x=Year, y=KWH)) + 
  geom_line() + theme_bw() 




#### Compare new files with old outputs using wrong field (HOVEDART_FAO) ####

oldcatch <- read.csv("./output/2022/Catch_corrected.csv")
oldeffort <- read.csv("./output/2022/Effort_corrected.csv")

ggplot(data = Effort %>% group_by(Year) %>% summarize(KWH = sum(KWdays, na.rm=T)*24/1000), aes(x=Year, y=KWH)) + 
  geom_line(col="black") + theme_bw() + ylab("1000 KWh") +
  geom_line(data = oldeffort %>% group_by(Year) %>% summarize(KWH = sum(KWdays, na.rm=T)*24/1000), col="red")

ggplot(data = Catch %>% group_by(Year) %>% summarize(sum_Landings = sum(Landings, na.rm=T)/1000), aes(x=Year, y=sum_Landings)) + 
  geom_line(col="black") + theme_bw() + ylab("Landings (t))") +
  geom_line(data = oldcatch %>% group_by(Year) %>% summarize(sum_Landings = sum(Landings, na.rm=T)/1000), col="red")


View(group_by(Catch, Species, Year) %>%
  summarise(TOT = as.character(sum(Landings))))

View(group_by(oldcatch, Species, Year) %>%
       summarise(TOT = as.character(sum(Landings))))

# Output from new files seems much more in line with the official landings from InterCatch!
# Still slightly underestimated but this is due to vessels <15m and uneven logbooks
