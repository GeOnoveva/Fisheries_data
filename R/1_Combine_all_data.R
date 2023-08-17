dataPath <- "D:\\Dropbox\\IMR_projects\\Data_call\\Fisheries_data\\data\\processed"
files<-list.files(dataPath, pattern = ".RDS")
files <- files[grepl("_all", (files)) == FALSE]

effort <- NULL
# effort <-  read.csv(file.path(dataPath,files[1]))
for (f in file.path(dataPath,files)) effort <- rbind(effort, readRDS(f))   

saveRDS(effort, file="dat_new.RDS")
