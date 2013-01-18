load("mdf.RData")
library(reshape)

elapsedTime <- function(x) {
  diff <- x-x[1]
  return(diff)
}

format <- function(subj) {
  subj <- transform(subj,new.DATE=ifelse(is.na(DATE),ENTRYDAY,DATE))
  subj <- transform(subj,new.DATE=as.Date(new.DATE,format='%Y%m%d'))
  subj <- transform(subj,days.from.bsl=elapsedTime(new.DATE))
  subj <- transform(subj,period=cut(as.numeric(days.from.bsl),c(-90,groups$V3)))
  subj <- transform(subj,group=factor(period,levels=levels(period),labels=groups$V1))
  return(subj)
}

groups <- read.csv("I:\\Paulus-CPR\\groups.csv",header=F)

longitudinal.spl <- with(longitudinal,split(longitudinal,MRNO))
longitudinal.spl <- lapply(longitudinal.spl,format)

#subset(format(longitudinal.spl[[1]]),select=c(MRNO,ENTRYDAY,new.DATE,days.from.bsl,period,TENDER.,SWELL.))

pt1 <- longitudinal.spl[[1]]
pt2 <- longitudinal.spl[[2]]

testnames <- names(pt1)[-pmatch(c("MRNO","DATE","TIME","VISIT","new.DATE","days.from.bsl","period","group"),names(pt1))]
testnamesL <- as.list(testnames)
names(testnamesL) <- testnames

pickFirst <- function(pair) {
  return(pair[1,])
}

pickOnePerGroup <- function(varname,pt) {
  var <- pt[[pmatch(varname,names(pt))]]
  df <- with(pt,data.frame(group,var))
  names(df)[2] <- varname
  df <- with(df,df[!is.na(var),])
  df.spl <- with(df,split(df,group))
  nobs <- do.call(rbind,lapply(df.spl,nrow))
  zero.rows <- which(nobs==0)
  first.occurrences <- do.call(rbind,lapply(df.spl[-zero.rows],pickFirst))
  return(first.occurrences)
}

allPicks <- function(pt) {
  return(lapply(testnamesL,pickOnePerGroup,pt))
}

mergeVarsFirstPicks <- function(first.picks) {
  return(merge_recurse(first.picks,by='group',all.x=T,all.y=T))
}

picksL <- lapply(longitudinal.spl,allPicks)

mergeVarsFirstPicks <- function(first.picks) {
  merged <- first.picks[[1]]
  for (i in 2:length(first.picks)) {
    if (is.null(first.picks[[i]])) 
      next
    else
      merged <- merge(merged,first.picks[[i]],by="group",all.x=T,all.y=T)
  }
  return(merged)
}
#test.picks1 <- allPicks(pt1)
#test.picks2 <- allPicks(pt2)

test.picks1.M <- mergeVarsFirstPicks(picksL[[1]])
test.picks2.M <- mergeVarsFirstPicks(picksL[[2]])

picks.as.data.frames <- lapply(picksL,mergeVarsFirstPicks) 
