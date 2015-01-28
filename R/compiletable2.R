compiletable2<-function(a,b)
{
  x<-a
  panel2<-
    x[,c(1,26,27,28,64,29,30,31,65,32,33,34,66,35,36,37,67,38,39,40,68,41,42,43,69,44,45,46,70,47,48,49,71)]
  
  
  VAL1WOR2<-with(panel2,rowMeans(panel2[,c(2,10,18,26)],na.rm=TRUE))
  VAL2WOR2<-with(panel2,rowMeans(panel2[,c(3,11,19,27)],na.rm=TRUE))
  VAL3WOR2<-with(panel2,rowMeans(panel2[,c(4,12,20,28)],na.rm=TRUE))
  VAL.WOR2<-with(panel2,rowMeans(panel2[,c(5,13,21,29)],na.rm=TRUE))
  MOM1WOR2<-with(panel2,rowMeans(panel2[,c(6,14,22,30)],na.rm=TRUE))
  MOM2WOR2<-with(panel2,rowMeans(panel2[,c(7,15,23,31)],na.rm=TRUE))
  MOM3WOR2<-with(panel2,rowMeans(panel2[,c(8,16,24,32)],na.rm=TRUE))
  MOM.WOR2<-with(panel2,rowMeans(panel2[,c(9,17,25,33)],na.rm=TRUE))
  
  panel2<-cbind(panel2,VAL1WOR2,VAL2WOR2,VAL3WOR2,VAL.WOR2,MOM1WOR2,MOM2WOR2,MOM3WOR2,MOM.WOR2)
  
  VAL1WOR3<-with(x,rowMeans(x[,c(2,8,14,20,26,32,38,44)],na.rm=TRUE))
  VAL2WOR3<-with(x,rowMeans(x[,c(3,9,15,21,27,33,39,45)],na.rm=TRUE))
  VAL3WOR3<-with(x,rowMeans(x[,c(4,10,16,22,28,34,40,46)],na.rm=TRUE))
  MOM1WOR3<-with(x,rowMeans(x[,c(5,11,17,23,29,35,41,47)],na.rm=TRUE))
  MOM2WOR3<-with(x,rowMeans(x[,c(6,12,18,24,30,36,42,48)],na.rm=TRUE))
  MOM3WOR3<-with(x,rowMeans(x[,c(7,13,19,25,31,37,43,49)],na.rm=TRUE))
  VAL.WOR3<-x[,50]
  MOM.WOR3<-x[,51]
  
  panel2<-cbind(panel2,VAL1WOR3,VAL2WOR3,VAL3WOR3,VAL.WOR3,MOM1WOR3,MOM2WOR3,MOM3WOR3,MOM.WOR3)
  
  
  
  valp3p1EQ<-unname(panel2[4]-panel2[2])
  momp3p1EQ<-unname(panel2[8]-panel2[6])
  valp3p1FX<-unname(panel2[12]-panel2[10])
  momp3p1FX<-unname(panel2[16]-panel2[14])
  valp3p1FI<-unname(panel2[20]-panel2[18])
  momp3p1FI<-unname(panel2[24]-panel2[22])
  valp3p1CM<-unname(panel2[28]-panel2[26])
  momp3p1CM<-unname(panel2[32]-panel2[30])
  valp3p1WOR2<-unname(panel2[36]-panel2[34])
  momp3p1WOR2<-unname(panel2[40]-panel2[38])
  valp3p1WOR3<-unname(panel2[44]-panel2[42])
  momp3p1WOR3<-unname(panel2[48]-panel2[46])
  
  panel2<-cbind(panel2,valp3p1EQ,momp3p1EQ,valp3p1FX,momp3p1FX,valp3p1FI,momp3p1FI, valp3p1CM,momp3p1CM,valp3p1WOR2,momp3p1WOR2,valp3p1WOR3,momp3p1WOR3)
  
  
  fiftypEQ<-unname((panel2[50]+panel2[51])/2)
  fiftyfactorEQ<-unname((panel2[5]+panel2[9])/2)
  fiftypFX<-unname((panel2[52]+panel2[53])/2)
  fiftyfactorFX<-unname((panel2[13]+panel2[17])/2)
  fiftypFI<-unname((panel2[54]+panel2[55])/2)
  fiftyfactorFI<-unname((panel2[21]+panel2[25])/2)
  fiftypCM<-unname((panel2[56]+panel2[57])/2)
  fiftyfactorCM<-unname((panel2[29]+panel2[33])/2)
  fiftypWOR2<-unname((panel2[58]+panel2[59])/2)
  fiftyfactorWOR2<-unname((panel2[37]+panel2[41])/2)
  fiftypWOR3<-unname((panel2[60]+panel2[61])/2)
  fiftyfactorWOR3<-unname((panel2[45]+panel2[49])/2)
  
  panel2<-cbind(panel2,fiftypEQ,fiftyfactorEQ,fiftypFX,fiftyfactorFX,fiftypFI,fiftyfactorFI,fiftypCM,fiftyfactorCM,fiftypWOR2,fiftyfactorWOR2,fiftypWOR3,fiftyfactorWOR3)
  
  panel2<-panel2[,c(1,2,3,4,50,5,6,7,8,51,9,62,63,10,11,12,52,13,14,15,16,53,17,64,65,18,19,20,54,21,22,23,24,55,25,66,67,26,27,28,56,29,30,31,32,57,33,68,69,34,35,36,58,37,38,39,40,59,41,70,71,42,43,44,60,45,46,47,48,61,49,72,73)]
  
  columnnames2<-colnames(panel2)[2:73]
  means<- sapply(2:73,overallmean,yearrange=b,datatable2=panel2)
  deviations<-sapply(2:73,overallsd,x=b,datatable3=panel2)
  tstats<-sapply(2:73,overalltstat,x=b,datatable4=panel2)
  sharpes<-sapply(2:73,sharpe,x=b,datatable=panel2)
  panel2table<-rbind(means,deviations,tstats,sharpes)
  colnames(panel2table)<-columnnames2
  return(panel2table)
}
