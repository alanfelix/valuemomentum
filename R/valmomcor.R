valmomcor<-function(yearrange,colnumber3,datatable5)
{
  colnumber4=(colnumber3)+5
  a<-sapply(yearrange,yearlyreturn,colnumber=colnumber3,datatable=datatable5)
  b<-sapply(yearrange,yearlyreturn,colnumber=colnumber4,datatable=datatable5)
  return(cor(a,b))
}