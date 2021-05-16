Citeringer <- read_excel("C:/Users/Gustav Larsen/OneDrive - CBS - Copenhagen Business School/CBS/Speciale/Data/Citeringer.xlsx",col_types = c("text", "numeric"))
as.data.frame(Citeringer)
tmp<-data.frame(matrix(0,40,2))
tmp<-Citeringer$citering
names(tmp)<-Citeringer$År
par(mar=c(2,4,1,1))
barplot(tmp, ylab = "Citeringer",las=1)
