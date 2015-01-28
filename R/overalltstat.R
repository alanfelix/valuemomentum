overalltstat<-function(x,y,datatable4)
{
  if(length(grep("p3p1",colnames(datatable4)[y]))>0){
    colnumber3=(y)-1
    colnumber4=(y) -3
    a<-sapply(x,yearlyreturn,colnumber=colnumber3,datatable=datatable4)
    b<-sapply(x,yearlyreturn,colnumber=colnumber4,datatable=datatable4)
    c<-a-b
    ttest2<-t.test(c)
    return(unname(ttest2$statistic))
  } else {
    if(length(grep("fifty",colnames(datatable4)[y]))>0){
      colnumber3=(y)-2
      colnumber4=(y)-7
      a<-sapply(x,yearlyreturn,colnumber=colnumber3,datatable=datatable4)
      b<-sapply(x,yearlyreturn,colnumber=colnumber4,datatable=datatable4)
      c<-(a+b)/2
      ttest2<-t.test(c)
      return(unname(ttest2$statistic))
    } else {
      
      ttest<-t.test(sapply(x,yearlyreturn,colnumber=y,datatable=datatable4))
      return(unname(ttest$statistic))
    }
  }
}