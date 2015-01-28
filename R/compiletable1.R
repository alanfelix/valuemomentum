compiletable1<-function(a,b)
{
  x<-a
  panel1<-x[,c(1,2,3,4,56,5,6,7,57,8,9,10,58,11,12,13,59,14,15,16,60,17,18,19,61,20,21,22,62,23,24,25,63)]
  VAL1WOR<-with(panel1,rowMeans(panel1[,c(2,10,18,26)],na.rm=TRUE))
  VAL2WOR<-with(panel1,rowMeans(panel1[,c(3,11,19,27)],na.rm=TRUE))
  VAL3WOR<-with(panel1,rowMeans(panel1[,c(4,12,20,28)],na.rm=TRUE))
  VAL.WOR<-with(panel1,rowMeans(panel1[,c(5,13,21,29)],na.rm=TRUE))
  MOM1WOR<-with(panel1,rowMeans(panel1[,c(6,14,22,30)],na.rm=TRUE))
  MOM2WOR<-with(panel1,rowMeans(panel1[,c(7,15,23,31)],na.rm=TRUE))
  MOM3WOR<-with(panel1,rowMeans(panel1[,c(8,16,24,32)],na.rm=TRUE))
  MOM.WOR<-with(panel1,rowMeans(panel1[,c(9,17,25,33)],na.rm=TRUE))
  panel1<-cbind(panel1,VAL1WOR,VAL2WOR,VAL3WOR,VAL.WOR,MOM1WOR,MOM2WOR,MOM3WOR,MOM.WOR)
  valp3p1US<-unname(panel1[4]-panel1[2])
  momp3p1US<-unname(panel1[8]-panel1[6])
  valp3p1UK<-unname(panel1[12]-panel1[10])
  momp3p1UK<-unname(panel1[16]-panel1[14])
  valp3p1EU<-unname(panel1[20]-panel1[18])
  momp3p1EU<-unname(panel1[24]-panel1[22])
  valp3p1JP<-unname(panel1[28]-panel1[26])
  momp3p1JP<-unname(panel1[32]-panel1[30])
  valp3p1WOR<-unname(panel1[36]-panel1[34])
  momp3p1WOR<-unname(panel1[40]-panel1[38])
  panel1<-cbind(panel1,valp3p1US,momp3p1US,valp3p1UK,momp3p1UK,valp3p1EU,momp3p1EU, valp3p1JP,momp3p1JP,valp3p1WOR,momp3p1WOR)
  
  fiftypUS<-unname((panel1[42]+panel1[43])/2)
  fiftyfactorUS<-unname((panel1[5]+panel1[9])/2)
  fiftypUK<-unname((panel1[44]+panel1[45])/2)
  fiftyfactorUK<-unname((panel1[13]+panel1[17])/2)
  fiftypEU<-unname((panel1[46]+panel1[47])/2)
  fiftyfactorEU<-unname((panel1[21]+panel1[25])/2)
  fiftypJP<-unname((panel1[48]+panel1[49])/2)
  fiftyfactorJP<-unname((panel1[29]+panel1[33])/2)
  fiftypWOR<-unname((panel1[50]+panel1[51])/2)
  fiftyfactorWOR<-unname((panel1[37]+panel1[41])/2)
  
  panel1<-cbind(panel1,fiftypUS,fiftyfactorUS,fiftypUK,fiftyfactorUK,fiftypEU,fiftyfactorEU,fiftypJP,fiftyfactorJP,fiftypWOR,fiftyfactorWOR)
  
  panel1<-panel1[,c(1,2,3,4,42,5,6,7,8,43,9,52,53,10,11,12,44,13,14,15,16,45,17,54,55,18,19,20,46,21,22,23,24,47,25,56,57,26,27,28,48,29,30,31,32,49,33,58,59,34,35,36,50,37,38,39,40,51,41,60,61)]
  columnnames<-colnames(panel1)[2:61]
  means<- sapply(2:61,overallmean,yearrange=b,datatable2=panel1)
  deviations<-sapply(2:61,overallsd,x=b,datatable3=panel1)
  tstats<-sapply(2:61,overalltstat,x=b,datatable4=panel1)
  sharpes<-sapply(2:61,sharpe,x=b,datatable=panel1)
  panel1table<-rbind(means,deviations,tstats,sharpes)
  colnames(panel1table)<-columnnames
  return(panel1table)
}