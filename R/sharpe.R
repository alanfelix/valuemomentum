sharpe<-function(x,y,datatable)
{
  return(overallmean(x,y,datatable)/overallsd(x,y,datatable))
}