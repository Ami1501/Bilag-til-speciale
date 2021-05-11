#install.packages("xtable")
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())


library(xtable)
par(mar=c(5, 4, 2, 2)) #Plot størrelse

#GENSKABELSE AF BAB MED LIGEVÆGTEDE INDUSTRI OG VÆRDIVÆGTET MARKED

############ ANDERS ############
Raw_daily <- read.csv("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/49_Industry_Portfolios_Daily_EW.csv"); names(Raw_daily)[1] <- "Date"
Raw_Monthly <- read.csv("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/49_Industry_Portfolios_Monthly_EW.csv"); names(Raw_Monthly)[1] <- "Date"

FF3d <- read.csv2("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/F-F_Research_Data_Factors_daily.CSV")
FF3m <- read.csv2("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/F-F_Research_Data_Factors_Monthly.CSV")



############ Transformering ########### 
n_Daily <- nrow(Raw_daily); n_Monthly <- nrow(Raw_Monthly)

#NA for -99.99
Raw_daily = replace(Raw_daily[,], Raw_daily[,]==-99.99, NA) 
Raw_Monthly = replace(Raw_Monthly[,], Raw_Monthly[,]==-99.99, NA)

#Tilføjer returns
Raw_daily$MarketReturn <- FF3d[,2]  #Ændre daily til 7 hvis vi bruger ligevægtet marketsafkast
Raw_Monthly$MarketReturn <- FF3m[,2]  #Ændre daily til 7 hvis vi bruger ligevægtet marketsafkast

#Rearanger data
Raw_daily <- Raw_daily[,c(1,51,2:50)]
Raw_Monthly <- Raw_Monthly[,c(1,51,2:50)]

#Markedet er allerede er uden RF
Raw_daily_mer <- Raw_daily[,3:51] - FF3d[,5]; Raw_daily_mer <- cbind(Raw_daily[,1:2],Raw_daily_mer)
Raw_Monthly_mer <- Raw_Monthly[,3:51] - FF3m[,5]; Raw_Monthly_mer <- cbind(Raw_Monthly[,1:2],Raw_Monthly_mer)


#Til tabel hurtig hurtig:
tmp <- data.frame(matrix(data=0, nrow=50, ncol=4))
for (i in 1:50){
  tmp[i,1] <- colnames(Raw_daily)[i+1]
  tmp[i,2] <- round(mean(Raw_daily[,i+1],na.rm = TRUE) *250,2)
  tmp[i,3] <- round(sd(Raw_daily[,i+1],na.rm = TRUE)* sqrt(250),2)
  tmp[i,4] <- round(tmp[i,2]/tmp[i,3],2)
}




#Konstanter
c=50            #Antal kolonner der skal regnes på
Forskydning = 1 #Forskydning grundet "date"-variablen
WinStd = 250    #Antal dage på 1 år til std
WinCor = 1250   #Antal dage på 5 år til cor
MinWinCor = 750 #Antal dage på 3 år til mindst at tage med i correlationen

# Længde af data
Dag1Index <- 1  
Dag1 <- substr(Raw_daily_mer[Dag1Index,1], 1,6)

DagNIndex <- nrow(Raw_daily_mer)
DagN <- substr(Raw_daily_mer[DagNIndex,1], 1,6)






############ Simpel beregning ############ 

#For dagligt data
data <- Raw_daily_mer[1:n_Daily,]
data_log <- Raw_daily_mer[1:n_Daily,];data_log[,2:51] <- log(1+data[,2:51]/100)

n <- nrow(data)

a_sim <- matrix(nrow=c)

std_sim <- matrix(nrow=c)
std_log_sim <- matrix(nrow=c)
cor_sim <- matrix(nrow=c)
cor_log_sim <- matrix(nrow=c)
beta_sim <- matrix(nrow=c)
beta_log_sim <- matrix(nrow=c)

for (j in (1+Forskydning):(c+Forskydning) ){
  
  if(min(which(is.na(data[,j])==0))>1){
    tmp_min= max(which(is.na(data[,j])==1)+1)
  } else{
    tmp_min = min(which(is.na(data[,j])==0))
  }
  
  tmp_max = max(which(is.na(data[,j])==0))
  
  
  a_sim[j] <- mean(data[tmp_min:tmp_max,j],na.rm=TRUE)
  
  
  std_sim[j] <- sd(data[tmp_min:tmp_max,j])
  std_log_sim[j] <- sd(data_log[tmp_min:tmp_max,j])
  
  
  cor_sim[j] <- cor(data[tmp_min:tmp_max,2],data[tmp_min:tmp_max,j])
  cor_log_sim[j] <- cor(data_log[tmp_min:tmp_max,2],data_log[tmp_min:tmp_max,j])
  
  
  beta_sim[j] <- cor_sim[j]*std_sim[j]/std_sim[2]
  beta_log_sim[j] <- cor_log_sim[j]*std_log_sim[j]/std_log_sim[2]
  
  
  
}


#Plots uden markedsafkast
par(mfrow=c(1,1))
plot(beta_sim[2:50],a_sim[2:50]*250/100,ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf", main="Forskel på normal og log (sort er log)",type="n"); text(beta_sim[2:50], a_sim[2:50]*250/100, cex=0.5,col="green")
points(beta_log_sim[2:50],a_sim[2:50]*250/100,ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf", main="Ligevægtede afkast D.data",type="n"); text(beta_log_sim[2:50], a_sim[2:50]*250/100, cex=0.5)


#Simpel metode
par(mfrow=c(1,1))
plot(beta_sim[3:51],a_sim[3:51]*250/100,ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1) #Ændre titlen
abline(0,a_sim[2]*250/100)
l<-lm(a_sim[3:51]*250/100~beta_log_sim[3:51]);   abline(l$coefficients[1],l$coefficients[2],col="Blue")






############ Kompliceret beregning ############ 

n <- n_Daily

#Definitioner af matricer
betas <- data.frame(matrix(nrow=n_Monthly,ncol=c))  #antal måneder i datasæt
betasNon <- data.frame(matrix(nrow=n_Monthly,ncol=c))
MlogReturn <- matrix(nrow=DagNIndex)
MlogReturn3d <- matrix(nrow=DagNIndex)


Stdtest <- data.frame(matrix(nrow=n_Monthly,ncol=c))
Cortest <- data.frame(matrix(nrow=n_Monthly,ncol=c))





#Loopet
for (j in (1+Forskydning):(c+Forskydning)) {
  
  # log returns for industrier
  logReturns <- matrix(nrow=n)
  for (i in 1:n){
    #For ligevægtede
    if ( is.na(Raw_daily_mer[i,j]) ) {
      logReturns[i] <- NA
    } else {
      logReturns[i] <- log(1+Raw_daily_mer[i,j]/100)
    }
    
    if (j==(1+Forskydning)){
      MlogReturn[i] <- logReturns[i]
    }
    
  }
  
  # 3 dags log returns for industrier
  logReturns3d <- matrix(nrow=n)
  
  for (i in 3:n) {
    #For ligevægtede
    if (anyNA(c(Raw_daily_mer[i,j], Raw_daily_mer[i-1,j], Raw_daily_mer[i-2,j]))) {
      logReturns3d[i] <- NA
    } else {
      logReturns3d[i] <- log(1+Raw_daily_mer[i,j]/100) + log(1+Raw_daily_mer[i-1,j]/100) + log(1+Raw_daily_mer[i-2,j]/100)
    }
    
    if (j==(1+Forskydning)){
      MlogReturn3d[i] <- logReturns3d[i]
    }
    
  }
  
  
  #Månedlige estimater for std og cor for måned k
  # Artiklen kræver min 750 dage for cor estimatet
  
  Index <- MinWinCor+3 #3 er for 3-dags log return
  k <- 1
  
  # Første måned
  Måned1 <- substr(Raw_daily_mer[Index,1], 1,6)
  while (substr(Raw_daily_mer[Index,1], 1,6) == Måned1) {
    Index <- Index + 1
  }
  iMåned <- substr(Raw_daily_mer[Index,1], 1,6)
  
  
  repeat {
    # Finder måneder og regner på estimater
    while (substr(Raw_daily_mer[Index,1], 1,6) == iMåned) {
      Index <- Index + 1
    }
    iMåned <- substr(Raw_daily_mer[Index,1], 1,6)
    #Måneden noteres og bruges til at regne beta
    row.names(betas)[k] <- iMåned; row.names(betasNon)[k] <- iMåned
    
    
    if ( anyNA(logReturns[(Index-WinStd):(Index-1)]) ) {
      Std <- NA # Hvis vi mangler data
      
    } else {
      # Denne måneds std estimatet findes over seneste år (250 dage i WinStd)
      Std <- sd(logReturns[(Index-WinStd):(Index-1)])
      
      
      mStd <- sd(MlogReturn[(Index-WinStd):(Index-1)])
      
      Stdtest[k,j] <- Std
      
    }
    
    
    # Denne måneds cor estimatet findes over seneste 5 år (1250 dage i WinCor)
    # Siden 3 år (750) dage er nok til at estimere cor, definere vi range
    if (Index < WinCor +3 ){corRange <- Index - 3} else {corRange <- WinCor}
    
    if ( anyNA(logReturns3d[(Index-corRange):(Index-1)]) ) {
      SenesteNA <- max(is.na(logReturns3d[(Index-corRange):(Index-1)])*
                         ((Index-corRange):(Index-1)))
      corRange <- (Index-1) - SenesteNA
    }
    
    # Tjekker om vi har nok data på de 3 år til cor
    if (corRange < MinWinCor) {
      Cor <- NA
      
    } else  {
      Cor <- cor(logReturns3d[(Index-corRange):(Index-1)],
                 MlogReturn3d[(Index-corRange):(Index-1)])
      
      
      Cortest[k,j] <- Cor
      
    }
    
    # Udregner Beta
    if ( anyNA(c(Std, Cor))) {
      betas[k,j-Forskydning] <- NA
      
    } else {
      beta <- Cor*Std/mStd   #DETTE SKAL RETTES TILBAGE
      #beta <- beta_sim[j]
      
      
      shrinkBeta <- 0.6*beta + (1-0.6)*1 #Justeret beta mod 1    0.74 er fra wi, 1.08 er fra meanbeta
      
      
      betas[k,j-Forskydning] <- shrinkBeta
      
      
      betasNon[k,j-Forskydning] <- beta
      
    }
    
    
    # Springer sidste måned over
    if (row.names(betas)[k] == substr(Raw_daily_mer[n,1], 1,6)) break 
    k <- k + 1
  }
  
  
}


AntalMåneder <- k


a <- colMeans(Raw_daily_mer[,(1+Forskydning):(c+Forskydning)],na.rm=TRUE)/100*250
b <- colMeans(betasNon[,1:c],na.rm = TRUE)
plot(b[2:50],a[2:50],ylim=c(0,0.3),xlim=c(0,1.6),main="Rullende ligevægt",labels=rownames(a),type="n");text(b[2:50], a[2:50], cex=0.75,col="red")




#Simpel metode
par(mfrow=c(1,1))
plot(beta_log_sim[3:51],a_sim[3:51]*250/100,ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1,col="Darkgreen") #Ændre titlen
abline(0,a_sim[2]*250/100,col="black")
l<-lm(a_sim[3:51]*250/100~beta_log_sim[3:51]);   abline(l$coefficients[1],l$coefficients[2],col="Darkgreen")

par(new = TRUE)
#Kompliceret metode
par(mfrow=c(1,1))
plot(b[2:50],a[2:50],ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1,col="Darkblue")
abline(0,a[1],col="black")
l<-lm(a[2:50]~b[2:50]);   abline(l$coefficients[1],l$coefficients[2],col="Darkblue")
legend(0,0.3,c("Simpel","Kompliceret"),col=c("DarkGreen","Darkblue"),lwd=c(2,2))




############ P1-P10############
måned <- 1
while (Raw_Monthly_mer[måned,1] != Måned1) {
  måned <- måned + 1
}


p <- 10

Ppf <- data.frame(matrix(0, nrow=AntalMåneder,ncol=2*p))
betasMdr <- betas[,2:c]


IndustriesMdr <- Raw_Monthly_mer[,3:(c+1)]


for (m in 1:(AntalMåneder-1)){ 
  # Ekskluder NA beta og NA return da vi ikke vil bruge disse
  inkluder_ab<- (!is.na(betasMdr[m,])) * (!is.na(IndustriesMdr[måned+m+1,])) == 1
  inkluder_ret<- (!is.na(betasMdr[m,])) * (!is.na(IndustriesMdr[måned+m+2,])) == 1
  
  relBetas_ab <- betasMdr[m,inkluder_ab]
  
  
  relBetas_ret <- betasMdr[m,inkluder_ret]
  
  
  nonNApfs_ab <- IndustriesMdr[måned+m+1,inkluder_ab]
  
  
  nonNApfs_ret <- IndustriesMdr[måned+m+2,inkluder_ret]
  
  
  
  nonNAs_ab <- dim(relBetas_ab)[2]
  nonNAs_ret <- dim(relBetas_ret)[2]
  
  
  # assignment bruges til at sige hvor de beta-sorterert returns skal samles
  assignment_ab <- ceiling((1:nonNAs_ab)*p/nonNAs_ab)
  assignment_ret <- ceiling((1:nonNAs_ret)*p/nonNAs_ret)
  
  
  row.names(Ppf)[m] <- substr(Raw_Monthly_mer[måned+m+1,1], 1,6)
  
  
  for (j in 1:nonNAs_ab) {
    
    # Portefølje beta.
    Ppf[m,assignment_ab[j]] <- Ppf[m,assignment_ab[j]] + (relBetas_ab[order(relBetas_ab)[j]]) / sum(assignment_ab == assignment_ab[j])    
  }
  
  for (j in 1:nonNAs_ret){
    
    # Portefølje afkast
    Ppf[m+1,p+assignment_ret[j]] <- Ppf[m+1,p+assignment_ret[j]] + nonNApfs_ret[order(relBetas_ret)[j]] / sum(assignment_ret == assignment_ret[j])    
    
  }
  
}

#Jensen Alpha for porteføljerne
JAlpha <- data.frame(matrix(0, nrow=p,ncol=1))
JAlpha3 <- data.frame(matrix(0, nrow=p,ncol=1))
RealBeta <- data.frame(matrix(0, nrow=p,ncol=1))
estBeta <- data.frame(matrix(0, nrow=p,ncol=1))
exRet <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_alpha <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_exRet <- data.frame(matrix(0, nrow=p,ncol=1))
JAlpha3 <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_alpha3 <- data.frame(matrix(0, nrow=p,ncol=1))



for (i in 1:p){
  tmp <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2):n_Monthly),2])
  
  tmp2 <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2):n_Monthly),2]+FF3m[(måned+2):n_Monthly,3]+FF3m[(måned+2):n_Monthly,4])
  tstat_P_alpha3[i,1] <- summary(tmp2)$coefficients[1,3]
  JAlpha3[i,1] <- tmp2$coefficients[1]
  
  tstat_P_alpha[i,1] <- summary(tmp)$coefficients[1,3]
  tstat_P_exRet[i,1] <- t.test(Ppf[,10+i], mu = 0)$statistic
  JAlpha[i,1] <- tmp$coefficients[1]
  RealBeta[i,1] <- tmp$coefficients[2]
  estBeta[i,1] <- mean(Ppf[,i])
  exRet[i,1] <- mean(Ppf[,10+i])  
  
}
mean(Ppf[62:AntalMåneder,10+10]) 

row.names(Ppf)[62]


xx <- barplot(JAlpha[,1], #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="CAPM alfa",xlab="Porteføljer",col=c("darkblue"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.2,0.5))
text(x = xx, y = JAlpha[,1], label = round(JAlpha[,1],2), pos = c(3,3,3,3,3,3,3,3,3,1), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red"))
box(lty = 'solid', col = 'black')

str(JAlpha)

#Plots over tid
# Lav beta, midt beta, høj beta og market
Sammenligning <- data.frame(matrix(data=0, nrow=AntalMåneder,ncol=5))
Sammenligning[1,]=1

# Lav beta (P1):
RetLav <- Ppf[,1+p]
BetaLav <- coef(lm(RetLav ~ Raw_Monthly_mer[((måned+2):n_Monthly),2]))[2]
RetLav <- RetLav / BetaLav # Gearing
# Mid beta (P5):
RetMid <- Ppf[,5+p]
BetaMid <- coef(lm(RetMid ~ Raw_Monthly_mer[((måned+2):n_Monthly),2]))[2]
RetMid <- RetMid / BetaMid # Gearing
# Høj beta (P10)
RetHøj <- Ppf[,10+p]
BetaHøj <- coef(lm(RetHøj ~ Raw_Monthly_mer[((måned+2):n_Monthly),2]))[2]
RetHøj <- RetHøj / BetaHøj # Gearing
# Market
RetMarket <- Raw_Monthly_mer[((måned+2):n_Monthly),2]
rf <- FF3m[((måned+2):n_Monthly),5] #SKAL DEN HER MED?


for (i in 2:AntalMåneder) {
  Sammenligning[i,1] <- row.names(betas)[i]
  Sammenligning[i,2] <- Sammenligning[i-1,2]*(1+(RetLav[i]+rf[i])/100)
  Sammenligning[i,3] <- Sammenligning[i-1,3]*(1+(RetMid[i]+rf[i])/100)
  Sammenligning[i,4] <- Sammenligning[i-1,4]*(1+(RetHøj[i]+rf[i])/100)
  Sammenligning[i,5] <- Sammenligning[i-1,5]*(1+(RetMarket[i]+rf[i])/100)
}

options(scipen=999)
plot(ts(Sammenligning[,2], start=c(1929,4), end=2021, frequency = 12), type='l',lwd=2,col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue");axis(2, c(1,10,100,1000,10000,100000),las=1)
lines(ts(Sammenligning[,3], start=c(1929,4), end=2021, frequency = 12),col="red",lwd=2)
lines(ts(Sammenligning[,4], start=c(1929,4), end=2021, frequency = 12),col="blue",lwd=2)
lines(ts(Sammenligning[,5], start=c(1929,4), end=2021, frequency = 12),lwd=2)
legend("topleft",c("Lav-Beta", "Middel-Beta","Høj-Beta","Markedet"), lwd=c(2,2), col=c("green","red","blue","black"),box.col="transparent",box.lwd = 0,bg="transparent",ncol = 2)



############ P1-P10 tid64############ 

#Fra 64:
#row.names(Ppf_ew)[431]
#Raw_Monthly_VW_mer[463,1]

AntalMåneder64 <- AntalMåneder - 431

Sammenligning64 <- data.frame(matrix(data=0, nrow=AntalMåneder64,ncol=4))
Sammenligning64[1,]=1


# Lav beta (P1):
RetLav64 <- Ppf[431:AntalMåneder,1+p]
BetaLav64 <- coef(lm(RetLav64 ~ Raw_Monthly_mer[463:n_Monthly,2]))[2]
RetLav64 <- RetLav64 / BetaLav64 # Gearing
# Mid beta (P5):
RetMid64 <- Ppf[431:AntalMåneder,5+p]
BetaMid64 <- coef(lm(RetMid64 ~ Raw_Monthly_mer[463:n_Monthly,2]))[2]
RetMid64 <- RetMid64 / BetaMid64 # Gearing
# Høj beta (P10)
RetHøj64 <- Ppf[431:AntalMåneder,10+p]
BetaHøj64 <- coef(lm(RetHøj64 ~ Raw_Monthly_mer[463:n_Monthly,2]))[2]
RetHøj64 <- RetHøj64 / BetaHøj64 # Gearing
# Market
RetMarket64 <- Raw_Monthly_mer[463:n_Monthly,2]
rf64 <- FF3m[463:n_Monthly,5] #SKAL DEN HER MED?


for (i in 2:AntalMåneder64) {
  Sammenligning64[i,1] <- Sammenligning64[i-1,1]*(1+(RetLav64[i]+rf64[i])/100)
  Sammenligning64[i,2] <- Sammenligning64[i-1,2]*(1+(RetMid64[i]+rf64[i])/100)
  Sammenligning64[i,3] <- Sammenligning64[i-1,3]*(1+(RetHøj64[i]+rf64[i])/100)
  Sammenligning64[i,4] <- Sammenligning64[i-1,4]*(1+(RetMarket64[i]+rf64[i])/100)
}

plot(Sammenligning64[,1], type='l',col="green",log="y",yaxt = "n");axis(2, c(1,10,100,1000,10000))
lines(Sammenligning64[,2],col="red")
lines(Sammenligning64[,3],col="blue")
lines(Sammenligning64[,4])


#ELLER MERE PRÆCIST (hvis vi ikke estimere beta, men bruger det rent faktiske):
for (i in 2:AntalMåneder64) {
  Sammenligning64[i,1] <- Sammenligning64[i-1,1]*(1+Ppf[431+i,1+p]/Ppf[431+i-1,1]/100)
  Sammenligning64[i,2] <- Sammenligning64[i-1,2]*(1+Ppf[431+i,5+p]/Ppf[431+i-1,5]/100)
  Sammenligning64[i,3] <- Sammenligning64[i-1,3]*(1+Ppf[431+i,10+p]/Ppf[431+i-1,10]/100)
  Sammenligning64[i,4] <- Sammenligning64[i-1,4]*(1+(RetMarket[431+i])/100)
}








############ BAB PF############
BABpf <- data.frame(matrix(data=0, nrow=AntalMåneder, ncol=8))
betaTilplot <- data.frame(matrix(data=0, nrow=AntalMåneder, ncol=2))

row.names(BABpf)[1102] <- "202012"
colnames(BABpf)[1] <- 'Kort return'
colnames(BABpf)[2] <- 'Lang return'
colnames(BABpf)[3] <- 'BAB return'
colnames(BABpf)[4] <- 'Cumulative BAB return'
colnames(BABpf)[5] <- '$long'
colnames(BABpf)[6] <- '$short'
colnames(BABpf)[7] <- 'Beta spread'
colnames(BABpf)[8] <- 'Kumuleret BAB skaleret med betaspread'

for (m in 1:(AntalMåneder-1)) {
  row.names(BABpf)[m] <- substr(FF3m[måned+m+1,1], 1,6)
  # Samme rutine! (KAN MAN SMIDE I TIDLIGERE LOOP?!)
  inkluder_bab <- (!is.na(betasMdr[m,])) * (!is.na(IndustriesMdr[måned+m+2,])) == 1
  relBetas_bab <- betasMdr[m,inkluder_bab]
  nonNApfs_bab <- IndustriesMdr[måned+m+2,inkluder_bab]
  
  zbar <- mean(rank(relBetas_bab))
  diffzbar <- rank(relBetas_bab) - zbar
  k <- 2/sum(abs(diffzbar))
  
  #testen: Nedenstående summer til 1
  #sum(k*abs(diffzbar)*(diffzbar>0))
  #sum(k*abs(diffzbar)*(diffzbar<0))
  
  
  # Kort i højbeta
  BABpf[m+1,1] <- sum(k*(diffzbar>0)*diffzbar * nonNApfs_bab)
  betaH <- sum(k*(diffzbar>0)*diffzbar * relBetas_bab)
  betaTilplot[m,1] <- betaH
  # Lang i lavbeta
  BABpf[m+1,2] <- sum(k*(diffzbar<0)*abs(diffzbar) * nonNApfs_bab)
  betaL <- sum(k*(diffzbar<0)*abs(diffzbar) * relBetas_bab)
  betaTilplot[m,2] <- betaL
  #BAB return
  BABpf[m+1,3] <- 1/betaL * BABpf[m+1,2] - 1/betaH * BABpf[m+1,1]
  # $Lang
  BABpf[m,5] <- 1/betaL
  # $kort
  BABpf[m,6] <- 1/betaH
  
  # Betaspread hvis det skal bruges
  BABpf[m,7] <- (betaH - betaL)/(betaH*betaL)
  BABpf[m+1,8] <- BABpf[m,8] + 1/BABpf[m,7] * BABpf[m+1,3]
}

1/mean(BABpf[,5]) #gns lav
1/mean(BABpf[,6]) #gns høj


# BAB return over årene
BABReturn <- mean(BABpf[,3])
tstat_bab_exRet <- t.test(BABpf[,3], mu = 0)$statistic

# BAB alpha
model_bab <- lm(BABpf[,3] ~ Raw_Monthly_mer[((måned+2):n_Monthly),2])
tstat_bab_alpha <- summary(model_bab)$coefficients[1,3]

JAlpha_bab <- model_bab$coefficients[1]
Beta_bab <- model_bab$coefficients[2]

# BAB alpha (trefaktors)
model_bab3 <- lm(BABpf[,3] ~ Raw_Monthly_mer[((måned+2):n_Monthly),2]+FF3m[(måned+2):n_Monthly,3]+FF3m[(måned+2):n_Monthly,4])
tstat_bab3_alpha <- summary(model_bab3)$coefficients[1,3]
JAlpha_bab3 <- model_bab3$coefficients[1]


sum3fak <- data.frame(nrow=4,ncol=4)
sum3fak <- summary(model_bab3)$coefficients[,]
row.names(sum3fak) <- c("Alfa","Beta_M","SML","HML")
print(xtable(sum3fak, type = "latex"), file = "GENSKABELSE 3fak.tex")




# BAB Sharpe
BABvol <- sd(BABpf[,3])*sqrt(12)
BABSharpe <- 12*BABReturn / BABvol

# cum BAB
for (i in 2:AntalMåneder) {
  BABpf[i,4] <- BABpf[i-1,4] + BABpf[i,3]
}


plot(ts(Sammenligning[,2], start=c(1929,4), end=2020, frequency = 12), type='l',col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue",cex.lab=0.3);axis(2, c(1,10,100,1000,10000,100000))
lines(ts(Sammenligning[,3], start=c(1929,4), end=2020, frequency = 12),col="red")
lines(ts(Sammenligning[,4], start=c(1929,4), end=2020, frequency = 12),col="blue")
lines(ts(Sammenligning[,5], start=c(1929,4), end=2020, frequency = 12))
lines(ts(BABpf[,4],start=c(1929,4), end=2020, frequency = 12),col="gold") #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
legend(1927,200000,c("Lav-beta", "Middel-Beta","Høj-Beta","Markedet","BAB"), lwd=c(1,1), col=c("green","red","blue","black","gold"))

plot(ts(BABpf[,8], start=c(1929,4), end=2020, frequency = 12), type='l',col="green",xlab="Tid",ylab="BAB afkast",cex.lab=0.3)
lines(ts(BABpf[,8]-BABpf[,4], start=c(1929,4), end=2020, frequency = 12), type='l',col="red",xlab="Tid",ylab="BAB afkast",cex.lab=0.3)
lines(ts(BABpf[,4],start=c(1929,4), end=2020, frequency = 12),col="black") #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)



############ Tabel Oversigt ############ 
tabel<- data.frame(matrix(data=0, nrow=10, ncol=11))
colnames(tabel) <- paste("P", 1:10, sep = "")
colnames(tabel)[11] <- "BAB"
for (j in 1:10){
  row.names(tabel)[1] <- "Merafkast"; tabel[1,j] <- exRet[j,1]
  row.names(tabel)[2] <- "T-værdi"; tabel[2,j] <- tstat_P_exRet[j,1]
  row.names(tabel)[3] <- "CAPM alfa"; tabel[3,j] <- JAlpha[j,1]
  row.names(tabel)[4] <- "T-værdi "; tabel[4,j] <- tstat_P_alpha[j,1]
  
  row.names(tabel)[5] <- "3 faktor alfa"; tabel[5,j] <- JAlpha3[j,1]
  row.names(tabel)[6] <- "T-værdi  "; tabel[6,j] <- tstat_P_alpha3[j,1]
  
  row.names(tabel)[7] <- "Beta (est.)"; tabel[7,j] <- estBeta[j,1]
  row.names(tabel)[8] <- "Beta (real.)"; tabel[8,j] <- RealBeta[j,1]
  row.names(tabel)[9] <- "Volatilitet"; tabel[9,j] <- sd(Ppf[,10+j])*sqrt(12)
  row.names(tabel)[10] <- "Sharpe ratio"; tabel[10,j] <- 12*exRet[j,1]/(sd(Ppf[,10+j])*sqrt(12))
}


tabel[1,11] <- BABReturn
tabel[2,11] <- tstat_bab_exRet
tabel[3,11] <- JAlpha_bab
tabel[4,11] <- tstat_bab_alpha

tabel[5,11] <- JAlpha_bab3
tabel[6,11] <- tstat_bab3_alpha

tabel[7,11] <- 0
tabel[8,11] <- Beta_bab
tabel[9,11] <- BABvol 
tabel[10,11] <- BABSharpe 


print(xtable(tabel, type = "latex"), file = "GENSKABELSE tabel.tex") #Gemmes som LigeVærdi



############ Wi shrinkBeta ############
varbeta <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=49))
wit <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=49))
sdcross <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=1))

for (i in 60:AntalMåneder){
  
  for(j in 1:49){
    varbeta[i-59,j] = sd(betasNon[(i-59):i,j+1],na.rm = TRUE)^2 #Var for industrier over alle dage 
  }
  sdcross[i-59,1] <- sd(as.numeric(betasNon[i,2:50]),na.rm = TRUE) #cross var af betaer (varians over beta for alle industrier)
  
  for (j in 1:49){
    wit[i-59,j] <- 1 - varbeta[i-59,j]/(varbeta[i-59,j]+sdcross[i-59,1]) #Vægten 
  }
  
}
w <- sum(wit[,],na.rm=TRUE)/sum(!is.na(wit[,]));w
betaXS <- sum(betasNon[60:AntalMåneder,2:50],na.rm=TRUE)/sum(!is.na(betasNon[60:AntalMåneder,2:50]));betaXS


par(mar=c(4, 4, 2, 5) + 0.1)
plot(ts(betaTilplot[,1], start=c(1934,4), end=2021, frequency = 12), ylim=c(0.3,2.7), xlab="Tid", ylab="Beta", type="l",col="red")
par(new = TRUE)
plot(ts(betaTilplot[,2], start=c(1934,4), end=2021, frequency = 12), axes=FALSE, ylim=c(0.3,2.7), xlab="", ylab="", type="l",col="green")
#axis(2, ylim=c(0.3,2.5),col="black",las=1)  ## las=1 makes horizontal labels
par(new = TRUE)
plot(ts(rowMeans(wit[,],na.rm = TRUE), start=c(1934,4), end=2021, frequency = 12), axes=FALSE, ylim=c(0.70,1.15), type='l',col="black",xlab="",ylab="")
mtext("wt",side=4,col="black",line=3)
axis(4,c(0.7,0.8,0.9,1),col="black",las=1,line=0)  ## las=1 makes horizontal labels
legend(1990,1.155,c("Høj-beta", "Lav-Beta","wt"), lwd=c(1,1,1), col=c("red","green","black"))



plot(ts(rowMeans(wit[,],na.rm = TRUE), start=c(1934,4), end=2021, frequency = 12), type='l',col="black",xlab="Tid",ylab="wt",cex.lab=0.3)
abline(h=w,lty=2)
par(mar=c(5, 4, 2, 2)) #Plot størrelse


par(mar=c(2, 4, 1, 1)) #uden xlab
#barplot
tmp <- union(JAlpha[,1],JAlpha_bab[1]) 
PogBABAlpha <- cbind(tmp,c(0.52,0.48,0.42,0.39,0.34,0.34,0.22,0.21,0.10,-0.10,0.73))

xx <- barplot(t(PogBABAlpha),beside=T, #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="CAPM alfa",xlab="Porteføljer",col=c("grey80","grey40"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10","BAB"),ylim=c(-0.1,0.8))
#text(x = xx, y = PogBABAlpha, label = round(PogBABAlpha,2), pos = c(3,3,3,3,3,3,3,3,3,1,3), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red","darkgreen"))
legend(1,0.8, c("Genskabelse","BAB-arktiklen"),col=c("grey80","grey40"),pch=c(15,15),pt.cex=2)
box(lty = 'solid', col = 'black')








###############################
### TED spread TIIIIIIIIIID ###
###############################

row.names(BABpf)[684]
row.names(BABpf)[1102]
nTedStart <- 684
nTedEnd <- 1102

TEDTemp <- readxl::read_excel("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/TEDRATE.xls")
TEDTemp <- as.data.frame(TEDTemp)
TED <- data.frame(matrix(nrow=nrow(TEDTemp),ncol=1))
TED[,1] <- TEDTemp[,2]
row.names(TED) <- TEDTemp[,1]

nMåneder <- 600
TEDstat <- data.frame(matrix(nrow=nMåneder,ncol=3))
lastindex <- dim(TED)[1]
lastday <- rownames(TED)[lastindex]

# Calculate monthly data for month k
# Min 750 days for corr data
currentIndex <- 1
k <- 1

# First month
startIndex <- currentIndex
startTED <- 0.9 # Hard coded for first month since first observation 19860102 left out
currentMonth <- substring(rownames(TED)[startIndex],1,7)
colnames(TEDstat)[1] <- 'Volatility during month'
colnames(TEDstat)[2] <- 'TED spread by end of last month'
colnames(TEDstat)[3] <- 'Change in TED spread during month'
repeat {
  # Find end of month
  while (substring(rownames(TED)[currentIndex],1,7) == currentMonth) {
    currentIndex <- currentIndex + 1
  }
  endIndex <- currentIndex - 1
  endTED <- TED[endIndex,1] # Last value in month t-1
  # Calculate volatility for month currentMonth
  row.names(TEDstat)[k] <- currentMonth
  TEDstat[k,1] <- sd(TED[startIndex:endIndex,1])
  TEDstat[k,2] <- startTED # NOTE: Different for first month, consider skipping
  TEDstat[k,3] <- endTED - startTED # NOTE: Different for first month, consinder skipping 
  # Preparing for calculation of next month's volatility
  startIndex <- currentIndex # First day in month t
  startTED <- TED[startIndex - 1,1] # Last value in month t-1
  currentMonth <- substring(rownames(TED)[startIndex],1,7) # Month t
  
  # Do not calculate data for last month
  if (substring(row.names(TED)[startIndex],1,7) == substring(lastday,1,7)) break
  k <- k + 1
}
nMåneder <- k
nrow(TEDstat)


BABforReg <- BABpf[(nTedStart-1):nTedEnd,3] #return
laggedTED <- TEDstat[1:420,2] * 100 # To bps
changeInTED <- TEDstat[1:420,3] * 100 # To bps

betaSpread <- BABpf[(nTedStart-1):nTedEnd,7] * 100 # Not bp, just scaled up
marketForReg <- FF3m[(nTedStart+måned):n_Monthly,2]
laggedBAB <- BABpf[(nTedStart-2):(nTedEnd-1),3]

# Plot af TED



TEDvolBPs <- TEDstat[1:nMåneder,1] * 100
plot(ts(TEDvolBPs,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="TED vol")
plot(ts(laggedTED,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="Lagged TED i bps")

plot(ts(TEDstat[,2]*100,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="TED spread i bps")


# Test of Prop 3
model1 <- lm(BABforReg ~ laggedTED + changeInTED)
summary(model1)
model2 <- lm(BABforReg ~ laggedTED + changeInTED +
               betaSpread+laggedBAB+marketForReg)
summary(model2)

tabelTED <- data.frame(matrix(nrow=10,ncol=2))
colnames(tabelTED) <- c("Uden korrigering","Med korrigering")
row.names(tabelTED)[1] <- "Lagged TED-spread"
row.names(tabelTED)[2] <- " "
row.names(tabelTED)[3] <- "Ændring i TED-spread"
row.names(tabelTED)[4] <- "  "
row.names(tabelTED)[5] <- "Beta-spread"
row.names(tabelTED)[6] <- "   "
row.names(tabelTED)[7] <- "Lagged BAB"
row.names(tabelTED)[8] <- "    " 

row.names(tabelTED)[9] <- "Markedafkast"
row.names(tabelTED)[10] <-"     "


tabelTED[1,1] <- summary(model1)[["coefficients"]][2, "Estimate"]
tabelTED[2,1] <- summary(model1)[["coefficients"]][2, "t value"]
tabelTED[3,1] <- summary(model1)[["coefficients"]][3, "Estimate"]
tabelTED[4,1] <- summary(model1)[["coefficients"]][3, "t value"]


tabelTED[1,2] <- summary(model2)[["coefficients"]][2, "Estimate"]
tabelTED[2,2] <- summary(model2)[["coefficients"]][2, "t value"]
tabelTED[3,2] <- summary(model2)[["coefficients"]][3, "Estimate"]
tabelTED[4,2] <- summary(model2)[["coefficients"]][3, "t value"]
tabelTED[5,2] <- summary(model2)[["coefficients"]][4, "Estimate"]
tabelTED[6,2] <- summary(model2)[["coefficients"]][4, "t value"]
tabelTED[7,2] <- summary(model2)[["coefficients"]][5, "Estimate"]
tabelTED[8,2] <- summary(model2)[["coefficients"]][5, "t value"]
tabelTED[9,2] <- summary(model2)[["coefficients"]][6, "Estimate"]
tabelTED[10,2] <- summary(model2)[["coefficients"]][6, "t value"]

print(xtable(tabelTED, type = "latex"), file = "Genskabelse TED-tabel.tex") #TED-tabel


