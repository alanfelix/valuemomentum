overallmean<-function(yearrange,colnumber2,datatable2)
{
  if(length(grep("p3p1",colnames(datatable2)[colnumber2]))>0)
  {
    
    colnumber3=(colnumber2)-1
    colnumber4=(colnumber2) -3
    x<-sapply(yearrange,yearlyreturn,colnumber=colnumber3,datatable=datatable2)
    y<-sapply(yearrange,yearlyreturn,colnumber=colnumber4,datatable=datatable2)
    z<-x-y
    return(mean(z,na.rm=TRUE))
  } else {
    if(length(grep("fifty",colnames(datatable2)[colnumber2]))>0)
    {
      
      colnumber3=(colnumber2)-2
      colnumber4=(colnumber2) -7
      x<-sapply(yearrange,yearlyreturn,colnumber=colnumber3,datatable=datatable2)
      y<-sapply(yearrange,yearlyreturn,colnumber=colnumber4,datatable=datatable2)
      z<-(x+y)/2
      return(mean(z,na.rm=TRUE))
    } else
    {
      return(mean(sapply(yearrange,yearlyreturn,colnumber=colnumber2,datatable=datatable2),na.rm=TRUE))
    }
    
  }
}