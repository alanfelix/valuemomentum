yearlyreturn<-function(yearvalue,colnumber,datatable)
{
  yearreturn<-prod(datatable[year(datatable$DATE)==yearvalue,colnumber]+1,na.rm=TRUE)-1
  return(yearreturn)
}