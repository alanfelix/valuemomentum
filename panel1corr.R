panel1corr<-function(a)
{
  x<-sapply(c(5,6,17,18,29,30,41,42,53,54),valmomcor,yearrange=1983:2014,datatable5=a)
  return(x)
}