dataPath <- "Q:\\ressurs\\mare\\fiskstat\\dagbok\\elFangstdagbok_detaljert\\ICES_datacalls_products"
files<-list.files(dataPath, pattern = ".csv")

effort <-  read.csv(file.path(dataPath,files[1]))
for (f in file.path(dataPath,files[-1])) effort <- rbind(effort, read.csv(f))   