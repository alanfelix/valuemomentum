overallsd<-function(x,y,datatable3)
{
  if(length(grep("p3p1",colnames(datatable3)[y]))>0){
    colnumber3=(y)-1
    colnumber4=(y) -3
    a<-sapply(x,yearlyreturn,colnumber=colnumber3,datatable=datatable3)
    b<-sapply(x,yearlyreturn,colnumber=colnumber4,datatable=datatable3)
    c<-a-b
    return(sd(c,na.rm=TRUE))
  }
  else{
    if(length(grep("fifty",colnames(datatable3)[y]))>0){
      colnumber3=(y)-2
      colnumber4=(y) -7
      a<-sapply(x,yearlyreturn,colnumber=colnumber3,datatable=datatable3)
      b<-sapply(x,yearlyreturn,colnumber=colnumber4,datatable=datatable3)
      c<-(a+b)/2
      return(sd(c,na.rm=TRUE))
    } else {
      return(sd(sapply(x,yearlyreturn,colnumber=y,datatable=datatable3)))
    }
  }
}