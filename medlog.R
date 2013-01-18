setwd("C:/PaulM/Medlog")
library(Hmisc)
library(stringr)
library(cwhmisc)
library(plyr)
library(gdata)

####################
#FUNCTIONS
####################

pick <- function(x,n) {
  return(x[n])
}

patient.level.time.invariate <- function(patient) {
  patient <- gsub("\n","",patient)
  patient <- gsub(" ","",patient)
  encounters <- unlist(str_split(patient,"\\|ENCOUNTR"))
  encounters <- sub("TEXTVARIABLES","",encounters[1])
  encounters <- unlist(str_split(encounters,"\\|"))
  encounters <- str_split(encounters,"\\=")
  varnames <- do.call(cbind,lapply(encounters,pick,1))
  values <- do.call(cbind,lapply(encounters,pick,2))
  df <- data.frame(rbind(varnames,values))
  names(df) <- varnames
  df <- df[-1,]
  return(df)
}
#patient.level.time.invariate(data3)

makeDataFrame <- function(encounter,mrno) {
  varnames <- do.call(cbind,lapply(encounter,pick,1))
  values <- do.call(cbind,lapply(encounter,pick,2))
  df <- data.frame(rbind(varnames,values),stringsAsFactors=FALSE)
  names(df) <- varnames
  df <- df[-1,]
  df[which(names(df)=="")] <- NULL
  df <- data.frame(MRNO=mrno,df)
  return(df)
}

patient.level.time.variate <- function(patient.id,dataset) {
  patient <- dataset[[patient.id]]
  patient <- gsub("\n","",patient)
  encounters <- unlist(str_split(patient,"\\|ENCOUNTR"))
  encounters <- encounters[-1]
  encounters <- paste("ENCOUNTR",encounters,sep="")
  encounters <- str_split(encounters,"\\|")
  encounters <- lapply(encounters,str_split,"\\=")
  data.frames <- lapply(encounters,makeDataFrame,patient.id)
  return(data.frames)
}

rbindEncounters <- function(encounters) {
  do.call(rbind.fill,encounters)
}


##############
#READ-IN THE DATA
##############

data <- scan("SSAmdf.txt",character(0),skip=162,blank.lines.skip=TRUE)
#data <- scan("medlog sample.txt",character(0),skip=161,blank.lines.skip=TRUE)
data <- paste(data[1:length(data)],collapse="")
data <- unlist(strsplit(data,"Patient=============================="))
data <- data[-1]

time.invariate.L <- lapply(data, patient.level.time.invariate)
time.invariate <- do.call(rbind.fill, lapply(data,patient.level.time.invariate))
names(data) <- time.invariate$MRNO

IDs.L <- as.list(names(data))
names(IDs.L) <- names(data)


#time.variate <- lapply(IDs.L, patient.level.time.variate, data)
#longitudinal.dataset <- do.call(rbind.fill, lapply(time.variate, rbindEncounters))

createLongitudinal <- function(m,n) {
  time.variate <- lapply(IDs.L[m:n], patient.level.time.variate, data[m:n])
  longitudinal.dataset <- do.call(rbind.fill, lapply(time.variate, rbindEncounters))
  return(longitudinal.dataset)
}

longitudinal1 <- createLongitudinal(1,100)
longitudinal2 <- createLongitudinal(101,200)
longitudinal3 <- createLongitudinal(201,294)

longitudinal <- do.call(rbind.fill,list(longitudinal1,longitudinal2,longitudinal3))
write.csv(longitudinal,"longitudinal.csv",quote=FALSE,na="")

save.image("mdf.RData")




