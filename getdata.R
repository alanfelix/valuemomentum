getdata<-function()
{
  
  download.file("https://www.aqr.com/~/media/files/data-sets/value-and-momentum-everywhere-portfolios-monthly.xlsx",
                "portfolios.xlsx",
                method = "curl")
  portfolios<-read.xlsx("portfolios.xlsx",1,rowIndex=21:537)
  portfolios$DATE <- as.Date(portfolios$DATE, "%y-%m-%d")
  portfolios$NA.<-NULL
  
  download.file("https://www.aqr.com/~/media/files/data-sets/value-and-momentum-everywhere-factors-monthly.xlsx",
                "factors.xlsx",
                method = "curl")
  factors<-read.xlsx("factors.xlsx",1,rowIndex=22:538)
  factors$DATE <- as.Date(factors$DATE, "%y-%m-%d")
  factors$NA.<-NULL
  x<-cbind(portfolios,factors)
  x[50]<-NULL
  return(x)
}