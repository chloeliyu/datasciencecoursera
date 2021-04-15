pollutantmean<-function(directory,pollutant,id=1:332) {
      # pa<-paste(getwd(),directory,"/",sep= "")
      #name<-list.files(directory,full.names=TRUE)
      name<-list.files(paste(getwd(),"/",directory,"/",sep=""),full.names=TRUE)
       dat<-data.frame()
        for (i in id) {
            dat<-rbind(dat,read.csv(name[i],header=TRUE))
            
        }
      means<-mean(dat[,pollutant],na.rm=TRUE)
      means
}
