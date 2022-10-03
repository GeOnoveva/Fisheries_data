#### loading the logbook data 

setwd("D:/Dropbox/IMR_projects/BarentsRISK_CoastRISKS/data/logbook")

file_list <- list.files(path=getwd())
file_list <- file_list[grep("FDIR", file_list)]
file_list <- file_list[grep("psv", file_list)]

new_proj <- "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"


library(sf)
library(raster)
library(sp)
library(tidyverse)

# Loading different shape files
MENUUII <- st_read("../MENUIIareasPolNewId_grass_tol0p01.shp")
MENUUII_proj <- st_transform(MENUUII, crs=new_proj)

OGRGeoJSON <- st_read("../OGRGeoJSON.shp")
OGRGeoJSON_proj <- st_transform(OGRGeoJSON, crs=new_proj)

svo_grense <- st_read("../svo_grense.shp")
svo_grense_proj <- st_transform(svo_grense, crs=new_proj)

alternative_svo <- st_read("../Kandidatomr_der sj_fugl.shp")
alternative_svo_proj <- st_transform(alternative_svo, crs=new_proj)

ggplot(data = MENUUII) + geom_sf(aes(geometry = geometry)) + geom_sf_label(aes(label = box_id)) 

# Some plotting to see whether it makes sense or not  
# ggplot(data = svo_grense_proj) + geom_sf(aes(geometry = geometry)) + geom_sf_label(aes(label = cat)) + 
#   geom_line(data=test[(349*2):(349*2+1),], aes(x=X, y=Y), col="red", cex=2)  


#### Code for allocating the catch and effort 

  OUTPUT <- vector("list", length(file_list))

  # SP_list <- c()
  # for (yr in seq_along(file_list))
  # {
  #   test <- read.table(file_list[yr], sep="|", header=TRUE)
  #   bla <- test %>% filter(REDSKAP_NS %in% c(50)) %>% group_by(FANGSTART_FAO) %>% summarize(n=n())
  #   SP_list <- c(SP_list, as.character(unique(test$FANGSTART_FAO)))
  # }
  # SP_list <- unique(SP_list)
  # sort(SP_list)

  for (yr in seq_along(file_list))
  {

    if(file.exists(paste0(getwd(), "/", (2011:2019)[yr], "intersect_equal_fishingtrip.Rdata")) == FALSE){
			test <- data.table::fread(file_list[yr])
      # test %>% group_by(REDSKAP_NS) %>% summarise(n=sum(RUNDVEKT, na.rm=T))  %>% mutate(prop = n/sum(n)) %>% print(n=30)
      
      # test %>% filter(is.na(REDSKAP_NS)) %>% group_by(REDSKAP_FAO) %>% summarise(n=sum(RUNDVEKT, na.rm=T))  %>% mutate(prop = n/sum(n)) %>% print(n=35)
      # 
      # 
      # 
      # test %>% filter(REDSKAP_NS %in% c(58,59)) %>% group_by(REDSKAP_NS, FANGSTART_FAO) %>% summarise(n=sum(RUNDVEKT, na.rm=T))      
      # test %>% filter(REDSKAP_NS %in% c(50)) %>% group_by(REDSKAP_NS, FANGSTART_FAO) %>% summarise(n=sum(RUNDVEKT, na.rm=T))      
      # 
      # test %>% filter(REDSKAP_FAO %in% "SDN") %>% group_by(REDSKAP_NS, REDSKAP) %>% summarize(n=sum(RUNDVEKT, na.rm=T)) %>% print(n=35)
      # 
      
      # Some database correction before pursuing with the analysis
        # correction for the FAO_gearcode "SND" to "SDN" = danish seine and correcting for REDSKAP_NS and REDSKAP
        test$REDSKAP <- as.character(test$REDSKAP)
        if (sum(test$REDSKAP_FAO %in% "SND")>0){
          test$REDSKAP_FAO[which(test$REDSKAP_FAO == "SND")] <- "SDN"
          test$REDSKAP_NS[which(test$REDSKAP_FAO == "SND")] <- 61
          test$REDSKAP[which(test$REDSKAP_FAO == "SND")] <- "Snurrevad, dansk"
        }
        if (sum(test$REDSKAP_FAO %in% "SDN")>0){
          test$REDSKAP_NS[which(test$REDSKAP_FAO == "SDN")] <- 61
          test$REDSKAP[which(test$REDSKAP_FAO == "SDN")] <- "Snurrevad, dansk"
        }
      
      # Now we need to deal with gear code 50 because it is both used for pelagic and bottom fish catch
        pelagic_fish <- c("ANE", "BLU", "CAP", "CLU", "HER", "HOM", "MAC", "MAS", "MAX", "MHA", "PIL", "SIX", "SME", "SPR", "SSM")
        test <-  test %>% mutate(Fish_group = ifelse(FANGSTART_FAO %in% pelagic_fish, "pelagic", "other"))
        
        bla <- test %>% group_by(RC, STARTTIDSPUNKT, Fish_group) %>% summarize(n=sum(RUNDVEKT)) 
        bla1 <- bla  %>% group_by(RC, STARTTIDSPUNKT) %>% mutate(prop = n/sum(n))
        
        bla1$ID <- apply(bla1[,c('RC','STARTTIDSPUNKT')], 1, function(x) paste(x, collapse="_"))
        qwe_pelagic <- bla1 %>% filter(Fish_group == "pelagic") 
        qwe_bottom <- bla1 %>% filter(Fish_group == "other")
        # all(qwe == FALSE)  
        # 
        ####Need to determine which haul can be considered majority 
        Pelagic_50 <- qwe_pelagic %>% filter(prop > 0.75) %>% as.data.frame() %>% select(ID) %>% unique() %>% unlist()
        Bottom_50 <- unique(bla1$ID)[which(unique(bla1$ID) %in% Pelagic_50 == FALSE)] 
        
        test$ID <- apply(test[,c('RC','STARTTIDSPUNKT')], 1, function(x) paste(x, collapse="_"))

      # Gear code conversion     
      trawl_gear <- c(50:54,56:59)
      trawl_bottom <- c(51:52,56:57)
      trawl_pelagic <- c(53:54)
      trawl_bottom_maybe <- c(58:59)
      shrimp_trawl <- 55
      cage <- 40:45
      longline <- c(30,31,32,35)
      hook <- c(33,34)
      net <- c(20:22)
      seine <- c(10:15)
      spinning <- 61
      canon <- c(70:72)
      other_gear <- c(80,81,82,90,99)
      
      # convert the gear group based on the above categories
      test <- test %>% mutate(Gear = ifelse(REDSKAP_NS %in% trawl_bottom, "trawl_bottom",
                                            ifelse(REDSKAP_NS %in% trawl_pelagic, "trawl_pelagic",
                                            ifelse(REDSKAP_NS %in% trawl_bottom_maybe, "trawl_bottom_maybe",
                                            ifelse(REDSKAP_NS == 50 & ID %in% Pelagic_50, "trawl_pelagic",
                                            ifelse(REDSKAP_NS == 50 & ID %in% Bottom_50, "trawl_bottom",
                                            ifelse(REDSKAP_NS %in% shrimp_trawl, "shrimp_trawl",
                                            ifelse(REDSKAP_NS %in% cage, "cage",
                                            ifelse(REDSKAP_NS %in% longline, "longline",
                                            ifelse(REDSKAP_NS %in% hook, "hook",
                                            ifelse(REDSKAP_NS %in% net, "net",
                                            ifelse(REDSKAP_NS %in% seine, "seine",
                                            ifelse(REDSKAP_NS %in% spinning, "spinning",
                                            ifelse(REDSKAP_NS %in% canon, "canon",
                                            ifelse(REDSKAP_NS %in% other_gear, "other_gear", "NA"))))))))))))))) 
      
      test <- test %>% mutate(Gear2 = ifelse(REDSKAP_NS %in% trawl_gear, "trawl_gear", Gear))
      
      # print(sum(test$Gear == "NA"))
      
      test1 <- test %>% mutate(lon_start = as.numeric(sub(",",".", START_LG)),
                               la_start = as.numeric(sub(",",".", START_LT)),
                               lon_end = as.numeric(sub(",",".", STOPP_LG)),
                               la_end = as.numeric(sub(",",".", STOPP_LT)),
                               duration = as.numeric((strptime(test$STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S") - strptime(test$STARTTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S"))/3600),
                               month = lubridate::month(strptime(test$STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S")),
                               ID = 1:nrow(test)) %>% 
                mutate(Quarter = ifelse(month %in% c(1:3), 1, ifelse(month %in% c(4:6), 2, ifelse(month %in% c(7:9), 3, 4)))) %>% 
        select(lon_start, lon_end, la_start, la_end, Gear, Gear2, FANGSTART_FAO, RUNDVEKT, duration, Quarter)
      
      ## convert projection
      test1_1 <- test1[, c('lon_start', 'la_start')]
      coordinates(test1_1) <- c("lon_start", "la_start")
      crs(test1_1) <- "+proj=longlat +datum=WGS84"
      test1_1 <- spTransform(test1_1, new_proj)
      test1_1 <- as.data.frame(test1_1)
      test1_2 <- test1[, c('lon_end', 'la_end')]
      coordinates(test1_2) <- c("lon_end", "la_end")
      crs(test1_2) <- "+proj=longlat +datum=WGS84"
      test1_2 <- spTransform(test1_2, new_proj)
      test1_2 <- as.data.frame(test1_2)
      
      ## Put back together
      test1_proj <- cbind(test1_1, test1_2, test1[, c('Gear', 'Gear2', 'FANGSTART_FAO', 'RUNDVEKT', 'duration', 'Quarter')])
      saveRDS(test1_proj, file=paste0(getwd(), "/", (2011:2019)[yr], "data.rds"))
  
      Catch_allocation <- function(x) {
        # convert to list to make a "LINESTRING" object 
        toline <- st_sfc(st_linestring(matrix(as.numeric(x[c('lon_start', 'la_start', 'lon_end', 'la_end')]), ncol=2, byrow=T)), crs=new_proj)
        # calculate which polygon the above fishing transectc intersects
        Atlantis <- st_intersects(toline, MENUUII_proj)
        OGR <- st_intersects(toline, OGRGeoJSON_proj)
        SVO <- st_intersects(toline, svo_grense_proj)
        SVO_alt <- st_intersects(toline, alternative_svo_proj)
        
        # ggplot(MENUUII_proj) + geom_sf() + geom_sf_text(aes(label=cat)) + geom_sf(data=toline, lwd = 3, col="red")
        
        # Now divide catch by gear, species and polygons, as well as effort
        Atlantis_add <- c()
        if (length(unique(unlist(Atlantis)))>0) {
          for (ijk in 1:length(unique(unlist(Atlantis)))){
            asd <- data.frame(Gear = x['Gear'], Gear2 = x['Gear2'], Quarter = x[,'Quarter'], Atlantis = MENUUII_proj$box_id[unique(unlist(Atlantis))[ijk]], Species = x['FANGSTART_FAO'], Catch = x['RUNDVEKT']/length(unique(unlist(Atlantis))), effort_min = x['duration']/length(unique(unlist(Atlantis))), effort_unit = 1/length(unique(unlist(Atlantis))))
            Atlantis_add <- rbind(Atlantis_add, asd)
          }
        } else {
          Atlantis_add <- data.frame(Gear = NA, Gear2 = NA, Quarter = NA, Atlantis = NA, FANGSTART_FAO = NA, RUNDVEKT = NA, duration = NA, effort_unit = NA)
        }
	    	OGR_add <- c()
        if (length(unique(unlist(OGR)))>0) {
          for (ijk in 1:length(unique(unlist(OGR)))){
            asd <- data.frame(Gear = x['Gear'], Gear2 = x['Gear2'], Quarter = x[,'Quarter'], OGR = unique(unlist(OGR))[ijk], Species = x['FANGSTART_FAO'], Catch = x['RUNDVEKT']/length(unique(unlist(OGR))), effort_min = x['duration']/length(unique(unlist(OGR))), effort_unit = 1/length(unique(unlist(OGR))))
            OGR_add <- rbind(OGR_add, asd)
          }
        } else {
          OGR_add <- data.frame(Gear = NA, Gear2 = NA, Quarter = NA, OGR = NA, FANGSTART_FAO = NA, RUNDVEKT = NA, duration = NA, effort_unit = NA)
        }
        SVO_add <- c()
        if (length(unique(unlist(SVO)))>0) {
          for (ijk in 1:length(unique(unlist(SVO)))){
            asd <- data.frame(Gear = x['Gear'], Gear = x['Gear2'], Quarter = x[,'Quarter'], SVO = unique(unlist(SVO))[ijk], Species = x['FANGSTART_FAO'], Catch = x['RUNDVEKT']/length(unique(unlist(SVO))), effort_min = x['duration']/length(unique(unlist(SVO))), effort_unit = 1/length(unique(unlist(SVO))))
            SVO_add <- rbind(SVO_add, asd)
          }
        } else {
          SVO_add <- data.frame(Gear = NA, Gear2 = NA, Quarter = NA, SVO = NA, FANGSTART_FAO = NA, RUNDVEKT = NA, duration = NA, effort_unit = NA)
        }
        SVO_alt_add <- c()
        if (length(unique(unlist(SVO_alt)))>0) {
          for (ijk in 1:length(unique(unlist(SVO_alt)))){
            asd <- data.frame(Gear = x['Gear'], Gear2 = x['Gear2'], Quarter = x[,'Quarter'], SVO_alt = unique(unlist(SVO_alt))[ijk], Species = x['FANGSTART_FAO'], Catch = x['RUNDVEKT']/length(unique(unlist(SVO_alt))), effort_min = x['duration']/length(unique(unlist(SVO_alt))), effort_unit = 1/length(unique(unlist(SVO_alt))))
            SVO_alt_add <- rbind(SVO_alt_add, asd)
          }
        } else {
          SVO_alt_add <- data.frame(Gear = NA, Gear2 = NA, Quarter = NA, SVO_alt = NA, FANGSTART_FAO = NA, RUNDVEKT = NA, duration = NA, effort_unit = NA)
        }
        
        # Return results
        res <- list()
        res$Altantis = Atlantis_add
        res$OGR = OGR_add
        res$SVO = SVO_add
        res$SVO_alt = SVO_alt_add
        
        return(res)
      }
      
      # system.time(
        # RES <- lapply(1:nrow(test1_proj), function(x) Catch_allocation(test1_proj[x,]))
      # )
      
    	RES <- vector("list", nrow(test1_proj))
    	system.time(
        for (ijk in 1:nrow(test1_proj)) {
    		RES[[ijk]] <- Catch_allocation(test1_proj[ijk,])
    		if(ijk %% 1000 == 0) print(ijk)
    	}
    	)
  
      save(RES, file=paste0(getwd(), "/", (2011:2019)[yr], "intersect_equal_fishingtrip.Rdata"))
      OUTPUT[[yr]] <- RES
    } else {
      load(file=paste0(getwd(), "/", (2011:2019)[yr], "intersect_equal_fishingtrip.Rdata"))
      OUTPUT[[yr]] <- RES
    }
  }
  
  

  ## Calculate the effort duration and effort unit by gear and year 
    Atlantis_effort_duration_all <- c()  
    Atlantis_effort_nb_all <- c()  
    OGR_effort_duration_all <- c()
    OGR_effort_nb_all <- c()
    SVO_effort_duration_all <- c()  
    SVO_effort_nb_all <- c()  
    SVO_alt_effort_duration_all <- c()  
    SVO_alt_effort_nb_all <- c()  
    for (yr in 1:9){
      
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "Altantis")))
      Atlantis_info <- data.table::rbindlist(asd, fill=T)
      Atlantis_effort_duration <- Atlantis_info %>% group_by(Gear2, Quarter, Atlantis) %>% summarize(tot_duration = sum(duration))
      Atlantis_effort_nb <- Atlantis_info %>% group_by(Gear2, Quarter, Atlantis) %>% summarize(tot_effort = sum(effort_unit))
      Atlantis_effort_duration$Year = (2011:2019)[yr]
      Atlantis_effort_nb$Year = (2011:2019)[yr]
      Atlantis_effort_duration_all <- rbind(Atlantis_effort_duration_all, Atlantis_effort_duration)
      Atlantis_effort_nb_all <- rbind(Atlantis_effort_nb_all, Atlantis_effort_nb)
      
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "OGR")))
      OGR_info <- data.table::rbindlist(asd, fill=T)
      OGR_effort_duration <- OGR_info %>% group_by(Gear2, Quarter, OGR) %>% summarize(tot_duration = sum(duration))
      OGR_effort_nb <- OGR_info %>% group_by(Gear2, Quarter, OGR) %>% summarize(tot_effort = sum(effort_unit))
      OGR_effort_duration$Year = (2011:2019)[yr]
      OGR_effort_nb$Year = (2011:2019)[yr]
      OGR_effort_duration_all <- rbind(OGR_effort_duration_all, OGR_effort_duration)
      OGR_effort_nb_all <- rbind(OGR_effort_nb_all, OGR_effort_nb)
      
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "SVO")))
      SVO_info <- data.table::rbindlist(asd, fill=T)
      SVO_effort_duration <- SVO_info %>% group_by(Gear2, Quarter, SVO) %>% summarize(tot_duration = sum(duration))
      SVO_effort_nb <- SVO_info %>% group_by(Gear2, Quarter, SVO) %>% summarize(tot_effort = sum(effort_unit))
      SVO_effort_duration$Year = (2011:2019)[yr]
      SVO_effort_nb$Year = (2011:2019)[yr]
      SVO_effort_duration_all <- rbind(SVO_effort_duration_all, SVO_effort_duration)
      SVO_effort_nb_all <- rbind(SVO_effort_nb_all, SVO_effort_nb)
      
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "SVO_alt")))
      SVO_alt_info <- data.table::rbindlist(asd, fill=T)
      SVO_alt_effort_duration <- SVO_alt_info %>% group_by(Gear2, Quarter, SVO_alt) %>% summarize(tot_duration = sum(duration))
      SVO_alt_effort_nb <- SVO_alt_info %>% group_by(Gear2, Quarter, SVO_alt) %>% summarize(tot_effort = sum(effort_unit))
      SVO_alt_effort_duration$Year = (2011:2019)[yr]
      SVO_alt_effort_nb$Year = (2011:2019)[yr]
      SVO_alt_effort_duration_all <- rbind(SVO_alt_effort_duration_all, SVO_alt_effort_duration)
      SVO_alt_effort_nb_all <- rbind(SVO_alt_effort_nb_all, SVO_alt_effort_nb)
      
    }
    
    # Atlantis_effort_duration_all$Atlantis <- MENUUII_proj$box_id[Atlantis_effort_duration_all$Atlantis]
    # Atlantis_effort_nb_all$Atlantis <- MENUUII_proj$box_id[Atlantis_effort_nb_all$Atlantis]
    # 
    write.table(Atlantis_effort_duration_all, file="../output/Effort_duration_polygon_gear_Atlantis.txt")
    write.table(OGR_effort_duration_all, file="../output/Effort_duration_polygon_gear_Production.txt")
    write.table(SVO_effort_duration_all, file="../output/Effort_duration_polygon_gear_SVO.txt")
    write.table(SVO_alt_effort_duration_all, file="../output/Effort_duration_polygon_gear_SVO_alt.txt")
    write.table(Atlantis_effort_nb_all, file="../output/Effort_unit_polygon_gear_Atlantis.txt")
    write.table(OGR_effort_nb_all, file="../output/Effort_unit_polygon_gear_Production.txt")
    write.table(SVO_catch_all, file="../output/Effort_unit_polygon_gear_SVO.txt")
    write.table(SVO_alt_catch_all, file="../output/Effort_unit_polygon_gear_SVO_alt.txt")
    
    
  ## Calculate catch, effort duration and effort unit by gear, species year 
    Atlantis_catch_all <- c()  
    Atlantis_effort_duration_all <- c()  
    Atlantis_effort_nb_all <- c()  
    OGR_catch_all <- c()
    OGR_effort_duration_all <- c()
    OGR_effort_nb_all <- c()
    SVO_catch_all <- c()  
    SVO_effort_duration_all <- c()  
    SVO_effort_nb_all <- c()  
    SVO_alt_catch_all<- c()  
    SVO_alt_effort_duration_all <- c()  
    SVO_alt_effort_nb_all <- c()  
    for (yr in 1:9){
  
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "Altantis")))
      Atlantis_info <- data.table::rbindlist(asd, fill=T)
      # Atlantis_info <- matrix(unlist(map(OUTPUT[[1]], function(x) matrix(unlist(pluck(x, "Altantis")), ncol=6, byrow=F)), ncol=6, byrow=T)
      # colnames(Atlantis_info) <- c("Gear", "Polygon", "Species", "Weight", "Duration", "Effort", "Weird", "Weird", "Weird")
      Atlantis_catch <- Atlantis_info %>% group_by(Gear, Quarter, Atlantis, FANGSTART_FAO) %>% summarize(tot_catch = sum(RUNDVEKT))
      Atlantis_effort_duration <- Atlantis_info %>% group_by(Gear, Quarter,Atlantis, FANGSTART_FAO) %>% summarize(tot_duration = sum(duration))
      Atlantis_effort_nb <- Atlantis_info %>% group_by(Gear, Quarter, Atlantis, FANGSTART_FAO) %>% summarize(tot_effort = sum(effort_unit))
      Atlantis_catch$Year = (2011:2019)[yr]
      Atlantis_effort_duration$Year = (2011:2019)[yr]
      Atlantis_effort_nb$Year = (2011:2019)[yr]
      Atlantis_catch_all <- rbind(Atlantis_catch_all, Atlantis_catch)
      Atlantis_effort_duration_all <- rbind(Atlantis_effort_duration_all, Atlantis_effort_duration)
      Atlantis_effort_nb_all <- rbind(Atlantis_effort_nb_all, Atlantis_effort_nb)
  
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "OGR")))
      OGR_info <- data.table::rbindlist(asd, fill=T)
      #OGR_info <- matrix(unlist(map(OUTPUT[[1]], function(x) matrix(unlist(pluck(x, "Altantis")), ncol=6, byrow=F)), ncol=6, byrow=T)
      #colnames(OGR_info) <- c("Gear", "Polygon", "Species", "Weight", "Duration", "Effort", "Weird")
      OGR_catch <- OGR_info %>% group_by(Gear, Quarter,OGR, FANGSTART_FAO) %>% summarize(tot_catch = sum(RUNDVEKT))
      OGR_effort_duration <- OGR_info %>% group_by(Gear, Quarter,OGR, FANGSTART_FAO) %>% summarize(tot_duration = sum(duration))
      OGR_effort_nb <- OGR_info %>% group_by(Gear, Quarter,OGR, FANGSTART_FAO) %>% summarize(tot_effort = sum(effort_unit))
      OGR_catch$Year = (2011:2019)[yr]
      OGR_effort_duration$Year = (2011:2019)[yr]
      OGR_effort_nb$Year = (2011:2019)[yr]
      OGR_catch_all <- rbind(OGR_catch_all, OGR_catch)
      OGR_effort_duration_all <- rbind(OGR_effort_duration_all, OGR_effort_duration)
      OGR_effort_nb_all <- rbind(OGR_effort_nb_all, OGR_effort_nb)
      
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "SVO")))
      SVO_info <- data.table::rbindlist(asd, fill=T)
      #SVO_info <- matrix(unlist(map(OUTPUT[[1]], function(x) matrix(unlist(pluck(x, "Altantis")), ncol=6, byrow=F)), ncol=6, byrow=T)
      #colnames(SVO_info) <- c("Gear", "Polygon", "Weird", "Weird", "Weird", "Species", "Weight", "Duration", "Effort")
      SVO_catch <- SVO_info %>% group_by(Gear, Quarter,SVO, FANGSTART_FAO) %>% summarize(tot_catch = sum(RUNDVEKT))
      SVO_effort_duration <- SVO_info %>% group_by(Gear, Quarter,SVO, FANGSTART_FAO) %>% summarize(tot_duration = sum(duration))
      SVO_effort_nb <- SVO_info %>% group_by(Gear, Quarter,SVO, FANGSTART_FAO) %>% summarize(tot_effort = sum(effort_unit))
      SVO_catch$Year = (2011:2019)[yr]
      SVO_effort_duration$Year = (2011:2019)[yr]
      SVO_effort_nb$Year = (2011:2019)[yr]
      SVO_catch_all <- rbind(SVO_catch_all, SVO_catch)
      SVO_effort_duration_all <- rbind(SVO_effort_duration_all, SVO_effort_duration)
      SVO_effort_nb_all <- rbind(SVO_effort_nb_all, SVO_effort_nb)
  
      asd <-map(OUTPUT[[yr]], function(x) as.data.frame(pluck(x, "SVO_alt")))
      SVO_alt_info <- data.table::rbindlist(asd, fill=T)
      #SVO_alt_info <- matrix(unlist(map(OUTPUT[[1]], function(x) matrix(unlist(pluck(x, "Altantis")), ncol=6, byrow=F)), ncol=6, byrow=T)
      #colnames(SVO_alt_info) <- c("Gear", "Polygon", "Species", "Weight", "Duration", "Effort", "Weird")
      SVO_alt_catch <- SVO_alt_info %>% group_by(Gear, Quarter, SVO_alt, FANGSTART_FAO) %>% summarize(tot_catch = sum(RUNDVEKT))
      SVO_alt_effort_duration <- SVO_alt_info %>% group_by(Gear, Quarter, SVO_alt, FANGSTART_FAO) %>% summarize(tot_duration = sum(duration))
      SVO_alt_effort_nb <- SVO_alt_info %>% group_by(Gear, Quarter, SVO_alt, FANGSTART_FAO) %>% summarize(tot_effort = sum(effort_unit))
      SVO_alt_catch$Year = (2011:2019)[yr]
      SVO_alt_effort_duration$Year = (2011:2019)[yr]
      SVO_alt_effort_nb$Year = (2011:2019)[yr]
      SVO_alt_catch_all <- rbind(SVO_alt_catch_all, SVO_alt_catch)
      SVO_alt_effort_duration_all <- rbind(SVO_alt_effort_duration_all, SVO_alt_effort_duration)
      SVO_alt_effort_nb_all <- rbind(SVO_alt_effort_nb_all, SVO_alt_effort_nb)
  
    }
    
  
    sp_code <- read.table("species_code.txt")
    colnames(sp_code) <- c("Name", "FANGSTART_FAO")
    Atlantis_catch_all <- Atlantis_catch_all %>% left_join(sp_code) %>% filter(!is.na(Name))
    Atlantis_effort_duration_all <-  Atlantis_effort_duration_all %>% left_join(sp_code) %>% filter(!is.na(Name))
    Atlantis_effort_nb_all <- Atlantis_effort_nb_all %>% left_join(sp_code)  %>% filter(!is.na(Name))
    SVO_catch_all <- SVO_catch_all %>% left_join(sp_code)   %>% filter(!is.na(Name))
    SVO_effort_duration_all <- SVO_effort_duration_all %>% left_join(sp_code)   %>% filter(!is.na(Name))
    SVO_effort_nb_all <- SVO_effort_nb_all %>% left_join(sp_code)  %>% filter(!is.na(Name))
    SVO_alt_catch_all<- SVO_alt_catch_all %>% left_join(sp_code)  %>% filter(!is.na(Name))
    SVO_alt_effort_duration_all <- SVO_alt_effort_duration_all %>% left_join(sp_code)  %>% filter(!is.na(Name))
    SVO_alt_effort_nb_all <- SVO_alt_effort_nb_all %>% left_join(sp_code) %>% filter(!is.na(Name))
    OGR_catch_all <- OGR_catch_all %>% left_join(sp_code)   %>% filter(!is.na(Name))
    OGR_effort_duration_all <- OGR_effort_duration_all %>% left_join(sp_code)   %>% filter(!is.na(Name))
    OGR_effort_nb_all <- OGR_effort_nb_all %>% left_join(sp_code)  %>% filter(!is.na(Name))
    
    # 
    # Atlantis_catch_all$Atlantis <- MENUUII_proj$box_id[Atlantis_catch_all$Atlantis]
    # Atlantis_effort_duration_all$Atlantis <- MENUUII_proj$box_id[Atlantis_effort_duration_all$Atlantis]
    # Atlantis_effort_nb_all$Atlantis <- MENUUII_proj$box_id[Atlantis_effort_nb_all$Atlantis]
    
    write.table(Atlantis_catch_all, file="../output/Catch_summary_Atlantis.txt")
    write.table(OGR_catch_all, file="../output/Catch_summary_Production.txt")
    write.table(SVO_catch_all, file="../output/Catch_summary_SVO.txt")
    write.table(SVO_alt_catch_all, file="../output/Catch_summary_SVO_alt.txt")
    write.table(Atlantis_effort_duration_all, file="../output/Effort_duration_summary_Atlantis.txt")
    write.table(OGR_effort_duration_all, file="../output/Effort_duration_summary_Production.txt")
    write.table(SVO_effort_duration_all, file="../output/Effort_duration_summary_SVO.txt")
    write.table(SVO_alt_effort_duration_all, file="../output/Effort_duration_summary_SVO_alt.txt")
    write.table(Atlantis_effort_nb_all, file="../output/Effort_unit_summary_Atlantis.txt")
    write.table(OGR_effort_nb_all, file="../output/Effort_unit_summary_Production.txt")
    write.table(SVO_catch_all, file="../output/Effort_unit_summary_SVO.txt")
    write.table(SVO_alt_catch_all, file="../output/Effort_unit_summary_SVO_alt.txt")
    
    
    
    Tot_summary <- c()
    for (yr in seq_along(file_list)) {
      test <- read.table(file_list[yr], sep="|", header=TRUE)
      
      # Gear code conversion     
      trawl_gear <- c(50:54,56:59)
      shrimp_trawl <- 55
      cage <- 40:45
      longline <- c(30,31,32,35)
      hook <- c(33,34)
      net <- c(20:22)
      seine <- c(10:15)
      spinning <- 61
      canon <- c(70:72)
      other_gear <- c(80,81,82,90,99)
      
      # convert the gear group based on the above categories
      test <- test %>% mutate(Gear = ifelse(REDSKAP_NS %in% trawl_gear, "trawl_gear", 
                                            ifelse(REDSKAP_NS %in% shrimp_trawl, "shrimp_trawl",
                                                   ifelse(REDSKAP_NS %in% cage, "cage",
                                                          ifelse(REDSKAP_NS %in% longline, "longline",
                                                                 ifelse(REDSKAP_NS %in% hook, "hook",
                                                                        ifelse(REDSKAP_NS %in% net, "net",
                                                                               ifelse(REDSKAP_NS %in% seine, "seine",
                                                                                      ifelse(REDSKAP_NS %in% spinning, "spinning",
                                                                                             ifelse(REDSKAP_NS %in% canon, "canon", "other"))))))))))
      
      
      test1 <- test %>% mutate(lon_start = as.numeric(sub(",",".", START_LG)),
                               la_start = as.numeric(sub(",",".", START_LT)),
                               lon_end = as.numeric(sub(",",".", STOPP_LG)),
                               la_end = as.numeric(sub(",",".", STOPP_LT)),
                               duration = as.numeric((strptime(test$STOPPTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S") - strptime(test$STARTTIDSPUNKT, format= "%Y-%m-%d %H:%M:%S"))/60),
                               ID = 1:nrow(test)) %>% 
        select(lon_start, lon_end, la_start, la_end, Gear, FANGSTART_FAO, RUNDVEKT, duration)
      
      out <- test1 %>% group_by(Gear, FANGSTART_FAO) %>% summarize(Tot_catch= sum(RUNDVEKT),
                                                                   Tot_duration = sum(duration),
                                                                   Tot_effort = n())
      out$Year = (2011:2019)[yr]  
  
      Tot_summary <- rbind(Tot_summary, out)    
    }  
    sp_code <- read.table("species_code.txt")
    colnames(sp_code) <- c("Name", "FANGSTART_FAO")
    Tot_summary <- Tot_summary %>% left_join(sp_code) %>% filter(!is.na(Name))
    
    write.table(Tot_summary, file="../output/Tot_summary.txt")
  
  
###### OLD CODE
# # convert to list to make a "LINESTRING" object 
#   test1_list <- lapply(1:nrow(test), function(x) as.matrix(test3[(x-1)*2+1:2,c('X','Y')]))
#   system.time(
#   test1_list1 <- lapply(test1_list, function(x) st_sfc(st_linestring(x), crs = new_proj))
#   )
# 
# # Now calculate which polygon each fishing transect intersect (no attribute by length of transect within polygon)
#   system.time(
#     res <- lapply(test1_list1, function(x) st_intersects(x, MENUUII_proj))
#   )
#   save(res, file=paste0(getwd(), "/", (2011:2019)[i], "intersect_equal.Rdata"))
#   
# Now calculate which polygon each fishing transect intersect by length of transect within polygon)
# system.time(
#   test1_list1 <- lapply(test1_list1, function(x) { x$length = st_length(x); y = st_intersection(x, MENUUII_proj); return(y)})
# )  
# system.time(
#   res1 <- lapply(test1_list1, function(x) st_intersection(x, MENUUII_proj))
# )
# 
#   save(res1, file=paste0(getwd(), "/", (2011:2019)[i], "intersect_length.Rdata"))
# rep <- c()
# for (ijk in 1:nrow(test))
# {
#   ll <- length(unique(unlist(res[[ijk]]))) 
#   if (ll>0){
#     for (j in 1:ll) { 
#       asd <- data.frame(Gear = test[ijk, 'Gear'], MENUUII = unique(unlist(res[[ijk]]))[j], Species = test[ijk, 'FANGSTART_FAO'], Catch = test[ijk, 'RUNDVEKT']/ll)
#       rep <- rbind(rep, asd)
#     }
#   }
# }
# 
# summary_rep <- rep %>% group_by(Species, MENUUII, Gear) %>% summarize(Tot_catch_kg = sum(Catch)) %>% as.data.frame()
# write.table(summary_rep, file=paste0(getwd(), "/Catch_sp_gear_MENUUII_", (2011:2019)[i], ".txt"))

