#install.packages("xtable")
library(xtable)
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())#Plot størrelse #Plot størrelse


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
plot(beta_log_sim[3:51],a_sim[3:51]*250/100,ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1) #Ændre titlen
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
      beta <- Cor*Std/mStd
      
      
      shrinkBeta <- 0.907*beta + (1-0.907)*1 #Justeret beta mod 1    0.74 er fra wi, 1.08 er fra meanbeta
      
      
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


#Kompliceret metode
par(mfrow=c(1,1))
plot(b[2:50],a[2:50],ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1)
abline(0,a[1])
l<-lm(a[2:50]~b[2:50]);   abline(l$coefficients[1],l$coefficients[2],col="Blue")






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
RealBeta <- data.frame(matrix(0, nrow=p,ncol=1))
estBeta <- data.frame(matrix(0, nrow=p,ncol=1))
exRet <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_alpha <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_exRet <- data.frame(matrix(0, nrow=p,ncol=1))
JAlpha3 <- data.frame(matrix(0, nrow=p,ncol=1))
tstat_P_alpha3 <- data.frame(matrix(0, nrow=p,ncol=1))



for (i in 1:p){
  tmp <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2):n_Monthly),2])
  tstat_P_alpha[i,1] <- summary(tmp)$coefficients[1,3]
  tstat_P_exRet[i,1] <- t.test(Ppf[,10+i], mu = 0)$statistic
  JAlpha[i,1] <- tmp$coefficients[1]
  RealBeta[i,1] <- tmp$coefficients[2]
  estBeta[i,1] <- mean(Ppf[,i])
  exRet[i,1] <- mean(Ppf[,10+i])  
  
  tmp2 <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2):n_Monthly),2]+FF3m[(måned+2):n_Monthly,3]+FF3m[(måned+2):n_Monthly,4])
  tstat_P_alpha3[i,1] <- summary(tmp2)$coefficients[1,3]
  JAlpha3[i,1] <- tmp2$coefficients[1]
  
  
}


xx <- barplot(JAlpha[,1], #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="CAPM alfa",xlab="Porteføljer",col=c("darkblue"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.2,0.5))
text(x = xx, y = JAlpha[,1], label = round(JAlpha[,1],2), pos = c(3,3,3,3,3,3,3,3,3,1), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red"))
box(lty = 'solid', col = 'black')


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

plot(ts(Sammenligning[,2], start=c(1929,4), end=2021, frequency = 12), type='l',col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue",cex.lab=0.3);axis(2, c(1,10,100,1000,10000,100000))
lines(ts(Sammenligning[,3], start=c(1929,4), end=2021, frequency = 12),col="red")
lines(ts(Sammenligning[,4], start=c(1929,4), end=2021, frequency = 12),col="blue")
lines(ts(Sammenligning[,5], start=c(1929,4), end=2021, frequency = 12))
legend("topleft",c("Lav-beta", "Middel-Beta","Høj-Beta","Markedet"), lwd=c(1,1), col=c("green","red","blue","black"))





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
print(xtable(sum3fak, type = "latex"), file = "LigeVærdi w09 b1 3fak.tex") #Gemmes som LigeVærdi





# BAB Sharpe
BABvol <- sd(BABpf[,3])*sqrt(12)
BABSharpe <- 12*BABReturn / BABvol

# cum BAB
for (i in 2:AntalMåneder) {
  BABpf[i,4] <- BABpf[i-1,4] + BABpf[i,3]
}

plot(ts(BABpf[,8], start=c(1929,4), end=2020, frequency = 12), type='l',col="gold",xlab="Tid",ylab="BAB afkast",cex.lab=0.3)


plot(ts(Sammenligning[,2], start=c(1929,4), end=2020, frequency = 12), type='l',col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue",cex.lab=0.3);axis(2, c(1,10,100,1000,10000,100000))
lines(ts(Sammenligning[,3], start=c(1929,4), end=2020, frequency = 12),col="red")
lines(ts(Sammenligning[,4], start=c(1929,4), end=2020, frequency = 12),col="blue")
lines(ts(Sammenligning[,5], start=c(1929,4), end=2020, frequency = 12))
lines(ts(BABpf[,4],start=c(1929,4), end=2020, frequency = 12),col="gold") #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
legend(1927,200000,c("Lav-beta", "Middel-Beta","Høj-Beta","Markedet","BAB"), lwd=c(1,1), col=c("green","red","blue","black","gold"))



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





print(xtable(tabel, type = "latex"), file = "LigeVærdi tabel.tex") #Gemmes som LigeVærdi



############ Wi shrinkBeta ############
varbeta <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=49))
wit <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=49))
betaXSt <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=1))
sdcross <- data.frame(matrix(data=0, nrow=AntalMåneder-59, ncol=1))



for (i in 60:AntalMåneder){
  
  for(j in 1:49){
    varbeta[i-59,j] = sd(betasNon[(i-59):i,j+1],na.rm = TRUE)^2 #Var for industrier over alle dage 
  }
  sdcross[i-59,1] <- sd(as.numeric(betasNon[i,2:50]),na.rm = TRUE) #cross var af betaer (varians over beta for alle industrier)
  
  for (j in 1:49){
    wit[i-59,j] <- 1 - varbeta[i-59,j]/(varbeta[i-59,j]+sdcross[i-59,1]) #Vægten 
  }
  row.names(wit)[i-59] <- row.names(betasNon)[i]
  row.names(betaXSt)[i-59] <- row.names(betasNon)[i]
  
  #betaXSt[i-59,1] <- sum(betasNon[(i-59):i,2:50],na.rm=TRUE)/sum(!is.na(betasNon[(i-59):i,2:50]))
  betaXSt[i-59,1] <- sum(betasNon[(i-59):i,2:50],na.rm=TRUE)/sum(!is.na(betasNon[(i-59):i,2:50]))
  
}
w <- sum(wit[,],na.rm=TRUE)/sum(!is.na(wit[,]));w

summary(betaXSt)


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


par(mar=c(5, 4, 2, 2),xpd=FALSE) #Plot størrelse
plot(ts(rowMeans(wit[,],na.rm = TRUE), start=c(1934,4), end=2020, frequency = 12), type='l',col="black",xlab="Tid",ylab="wt",cex.lab=0.3)
abline(h=w)







#MED WT


############ WT #########################################################
BetasWit <- data.frame(matrix(data=1, nrow=AntalMåneder, ncol=50))

for (i in 60:AntalMåneder){
  row.names(BetasWit)[i] <- row.names(betasNon)[i]
  for (j in 1:49){
    BetasWit[i,1] <- 1  
    BetasWit[i,j+1] <- betasNon[i,j+1]*rowMeans(wit[i-59,1:49],na.rm = TRUE) +(1-rowMeans(wit[i-59,1:49],na.rm = TRUE))*1 #59 er antal måender for de 5 ekstra år der bruges til at estimere wit
    
  }
}


############ P1-P10############
måned <- 1
while (Raw_Monthly_mer[måned,1] != Måned1) {
  måned <- måned + 1
}

p <- 10

Ppf <- data.frame(matrix(0, nrow=AntalMåneder,ncol=2*p))
#betasMdr <- betas[,2:c]
betasMdr <- BetasWit[,2:c]


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


#test og definering
row.names(Ppf)[60]
Raw_Monthly_mer[((måned+2+59)),1]

Ppf<-Ppf[60:AntalMåneder,]



for (i in 1:p){
  tmp <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2+59):n_Monthly),2])
  
  tmp2 <- lm(Ppf[,10+i]~Raw_Monthly_mer[((måned+2+59):n_Monthly),2]+FF3m[(måned+2+59):n_Monthly,3]+FF3m[(måned+2+59):n_Monthly,4])
  tstat_P_alpha3[i,1] <- summary(tmp2)$coefficients[1,3]
  JAlpha3[i,1] <- tmp2$coefficients[1]
  
  tstat_P_alpha[i,1] <- summary(tmp)$coefficients[1,3]
  tstat_P_exRet[i,1] <- t.test(Ppf[,10+i], mu = 0)$statistic
  JAlpha[i,1] <- tmp$coefficients[1]
  RealBeta[i,1] <- tmp$coefficients[2]
  estBeta[i,1] <- mean(Ppf[,i])
  exRet[i,1] <- mean(Ppf[,10+i])  
  
}



xx <- barplot(JAlpha[,1], #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="CAPM alfa",xlab="Porteføljer",col=c("darkblue"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.2,0.5))
text(x = xx, y = JAlpha[,1], label = round(JAlpha[,1],2), pos = c(3,3,3,3,3,3,3,3,3,1), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red"))
box(lty = 'solid', col = 'black')


#Plots over tid
# Lav beta, midt beta, høj beta og market
Sammenligning <- data.frame(matrix(data=0, nrow=AntalMåneder,ncol=5))
Sammenligning[1,]=1

# Lav beta (P1):
RetLav <- Ppf[,1+p]
BetaLav <- coef(lm(RetLav ~ Raw_Monthly_mer[((måned+2+59):n_Monthly),2]))[2]
RetLav <- RetLav / BetaLav # Gearing
# Mid beta (P5):
RetMid <- Ppf[,5+p]
BetaMid <- coef(lm(RetMid ~ Raw_Monthly_mer[((måned+2+59):n_Monthly),2]))[2]
RetMid <- RetMid / BetaMid # Gearing
# Høj beta (P10)
RetHøj <- Ppf[,10+p]
BetaHøj <- coef(lm(RetHøj ~ Raw_Monthly_mer[((måned+2+59):n_Monthly),2]))[2]
RetHøj <- RetHøj / BetaHøj # Gearing
# Market
RetMarket <- Raw_Monthly_mer[((måned+2+59):n_Monthly),2]
rf <- FF3m[((måned+2+59):n_Monthly),5] #SKAL DEN HER MED?


for (i in 2:AntalMåneder) {
  #Sammenligning[i,1] <- row.names(betas)[i]
  Sammenligning[i,2] <- Sammenligning[i-1,2]*(1+(RetLav[i]+rf[i])/100)
  Sammenligning[i,3] <- Sammenligning[i-1,3]*(1+(RetMid[i]+rf[i])/100)
  Sammenligning[i,4] <- Sammenligning[i-1,4]*(1+(RetHøj[i]+rf[i])/100)
  Sammenligning[i,5] <- Sammenligning[i-1,5]*(1+(RetMarket[i]+rf[i])/100)
}

plot(ts(Sammenligning[,2], start=c(1929,4), end=2020, frequency = 12), type='l',col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue",cex.lab=0.3,cex.axis=0.3);axis(2, c(1,10,100,1000,10000,100000))
lines(ts(Sammenligning[,3], start=c(1929,4), end=2020, frequency = 12),col="red")
lines(ts(Sammenligning[,4], start=c(1929,4), end=2020, frequency = 12),col="blue")
lines(ts(Sammenligning[,5], start=c(1929,4), end=2020, frequency = 12))
legend(1927,200000,c("Lav-beta", "Middel-Beta","Høj-Beta","Markedet"), lwd=c(1,1), col=c("green","red","blue","black"))



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

row.names(BABpf)[60]
BABpf <- BABpf[60:AntalMåneder,]


1/mean(BABpf[,5]) #gns lav
1/mean(BABpf[,6]) #gns høj


# BAB return over årene
BABReturn <- mean(BABpf[,3],na.rm = TRUE)
tstat_bab_exRet <- t.test(BABpf[,3], mu = 0)$statistic

# BAB alpha
model_bab <- lm(BABpf[,3] ~ Raw_Monthly_mer[((måned+2+59):n_Monthly),2])
tstat_bab_alpha <- summary(model_bab)$coefficients[1,3]

JAlpha_bab <- model_bab$coefficients[1]
Beta_bab <- model_bab$coefficients[2]

# BAB alpha (trefaktors)
model_bab3 <- lm(BABpf[,3] ~ Raw_Monthly_mer[((måned+2+59):n_Monthly),2]+FF3m[(måned+2+59):n_Monthly,3]+FF3m[(måned+2+59):n_Monthly,4])
tstat_bab3_alpha <- summary(model_bab3)$coefficients[1,3]
JAlpha_bab3 <- model_bab3$coefficients[1]

sum3fak <- data.frame(nrow=4,ncol=4)
sum3fak <- summary(model_bab3)$coefficients[,]
row.names(sum3fak) <- c("Alfa","Beta_M","SML","HML")
print(xtable(sum3fak, type = "latex"), file = "LigeVærdi wt b1 3fak.tex") #Gemmes som LigeVærdi



# BAB Sharpe
BABvol <- sd(BABpf[,3],na.rm = TRUE)*sqrt(12)
BABSharpe <- 12*BABReturn / BABvol

# cum BAB
for (i in 2:AntalMåneder) {
  BABpf[i,4] <- BABpf[i-1,4] + BABpf[i,3]
}


plot(ts(Sammenligning[,2], start=c(1934,2), end=2020, frequency = 12), type='l',col="green",log="y",yaxt = "n",xlab="Tid",ylab="Formue",cex.lab=0.3);axis(2, c(1,10,100,1000,10000,100000))
lines(ts(Sammenligning[,3], start=c(1934,2), end=2020, frequency = 12),col="red")
lines(ts(Sammenligning[,4], start=c(1934,2), end=2020, frequency = 12),col="blue")
lines(ts(Sammenligning[,5], start=c(1934,2), end=2020, frequency = 12))
lines(ts(BABpf[,4],start=c(1934,2), end=2020, frequency = 12),col="gold") #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
legend(1927,200000,c("Lav-beta", "Middel-Beta","Høj-Beta","Markedet","BAB"), lwd=c(1,1), col=c("green","red","blue","black","gold"))



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

row.names(betaXSt)[371]
summary(betaXSt[1:371,1])
summary(betaXSt[372:AntalMåneder,1])

sd(betaXSt[1:371,1],na.rm=TRUE)
sd(betaXSt[372:AntalMåneder,1],na.rm=TRUE)


print(xtable(tabel, type = "latex"), file = "LigeVærdi wt b1.tex") #Gemmes som LigeVærdi


############ TEDSPREAD###############

row.names(BABpf)[625]
row.names(BABpf)[1043]
nTedStart <- 625
nTedEnd <- 1043

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


LaggedForskydning <- 2

BABforReg <- BABpf[(nTedStart-1+LaggedForskydning):nTedEnd,3] #return
laggedTED <- TEDstat[(1+LaggedForskydning):420,2] * 100 # To bps
changeInTED <- TEDstat[(1+LaggedForskydning):420,3] * 100 # To bps
betaSpread <- BABpf[(nTedStart-1+LaggedForskydning):nTedEnd,7] * 100 # Not bp, just scaled up
marketForReg <- FF3m[(nTedStart-1+LaggedForskydning+måned+60):n_Monthly,2]
laggedBAB <- BABpf[(nTedStart-2+LaggedForskydning):(nTedEnd-1),3]

laggedchangeInTED <- TEDstat[1:(420-LaggedForskydning),3] * 100

# Plot af TED
plot(ts(TEDstat[,2]*100,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="TED spread i bps")

plot(ts(laggedTED,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="Lagged TED i bps")

TEDvolBPs <- TEDstat[1:nMåneder,1] * 100
plot(ts(TEDvolBPs,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="TED vol")


# Test of Prop 3
model1 <- lm(BABforReg ~ laggedTED + changeInTED)
summary(model1)
model2 <- lm(BABforReg ~ laggedTED + changeInTED +
               betaSpread+laggedBAB+marketForReg)
summary(model2)


model3 <- lm(BABforReg ~ changeInTED+laggedchangeInTED)
summary(model3)



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


print(xtable(tabelTED, type = "latex"), file = "TED-tabel.tex") #TED-tabel




par(mar=c(4, 4, 2, 5) + 0.1)
plot(ts(TEDvolBPs,start= 1986, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="TED spread i bps")
par(new = TRUE)
plot(ts(rowMeans(wit[624:nTedEnd,1:49]), start=c(1986,1), end=2020, frequency = 12), axes=FALSE, ylim=c(0.7,1), xlab="", ylab="", type="l",col="green")


















############ Rullede alfa ############


CAPMa <- data.frame(matrix(data=0, nrow=nrow(BABpre)-59, ncol=1))
CAPM3 <- data.frame(matrix(data=0, nrow=nrow(BABpre)-59, ncol=1))
CAPMCar <- data.frame(matrix(data=0, nrow=nrow(BABpre)-59, ncol=1))
CAPMCarRev <- data.frame(matrix(data=0, nrow=nrow(BABpre)-59, ncol=1))

row.names(BABpf)[1]    #193402
row.names(BABpf)[371]  #196412
row.names(BABpf)[372]  #196501
row.names(BABpf)[1043] #202012


#post
for (i in 60:(nrow(BABpf)-59)) {
  tmp <- lm(BABpf[(i-59):i,3]~FF3m[(i+91-59):(i+91),2])
  tmp2 <- lm(BABpf[(i-59):i,3]~FF3m[(i+91-59):(i+91),2]+FF3m[(i+91-59):(i+91),3]+FF3m[(i+91-59):(i+91),4])
  tmp3 <- lm(BABpf[(i-59):i,3]~FF3m[(i+91-59):(i+91),2]+FF3m[(i+91-59):(i+91),3]+FF3m[(i+91-59):(i+91),4]+FF3m[(i+91-59):(i+91),8])
  tmp4 <- lm(BABpf[(i-59):i,3]~FF3m[(i+91-59):(i+91),2]+FF3m[(i+91-59):(i+91),3]+FF3m[(i+91-59):(i+91),4]+FF3m[(i+91-59):(i+91),8]+FF3m[(i+91-59):(i+91),12]+FF3m[(i+91-59):(i+91),13])
  
  CAPMa[i-59,1] <- tmp$coefficients[1]; row.names(CAPMa)[i-59] <- row.names(BABpf)[i]
  CAPM3[i-59,1] <- tmp2$coefficients[1]; row.names(CAPM3)[i-59] <- row.names(BABpf)[i]
  CAPMCar[i-59,1] <- tmp3$coefficients[1]; row.names(CAPMCar)[i-59] <- row.names(BABpf)[i]
  CAPMCarRev[i-59,1] <- tmp4$coefficients[1]; row.names(CAPMCarRev)[i-59] <- row.names(BABpf)[i]
  
}


lm(BABpf[1:371,3]~Raw_Monthly_mer[((60+måned+1):(måned+nCAPMslut+60)),2]) #pre
lm(BABpf[372:1043,3]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2]) #post

#plot(ts(CAPMa[,1],start= 1932, end = 2021, frequency = 12),type = 'l', xlab= "Tid", ylab="CAPM alfa")








############ Pre og Post CAPM P1-P10 #######
row.names(BABpf)[1] #193402
row.names(BABpf)[371] # 196412
row.names(BABpf)[1043] #202012

Raw_Monthly_mer[måned+1+60,1] #193402
Raw_Monthly_mer[måned+nCAPMslut+60,1] #196412
Raw_Monthly_mer[n_Monthly,1] #202012

nTOTslut <- 1043
nCAPMslut <- 371

Ppre <- Ppf[1:nCAPMslut,]
Ppost <- Ppf[(nCAPMslut+1):nTOTslut,]

tabelPpre<- data.frame(matrix(data=0, nrow=12, ncol=10))
tabelPpost<- data.frame(matrix(data=0, nrow=12, ncol=10))


tstat_P_alphaCar<- data.frame(matrix(data=0, nrow=10, ncol=1))
JAlphaCar<- data.frame(matrix(data=0, nrow=10, ncol=1))

tstat_P_alphaCarREV<- data.frame(matrix(data=0, nrow=10, ncol=1))
JAlphaCarREV<- data.frame(matrix(data=0, nrow=10, ncol=1))


#Pre
for (i in 1:p){
  estBeta[i,1] <- mean(Ppre[,i])
  exRet[i,1] <- mean(Ppre[,10+i])
  tstat_P_exRet[i,1] <- t.test(Ppre[,10+i], mu = 0)$statistic
  
  tmp <- lm(Ppre[,10+i]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2])
  RealBeta[i,1] <- tmp$coefficients[2]
  tstat_P_alpha[i,1] <- summary(tmp)$coefficients[1,3]
  JAlpha[i,1] <- tmp$coefficients[1]
  
  tmp2 <- lm(Ppre[,10+i]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2]+FF3m[(måned+1+60):(måned+nCAPMslut+60),3]+FF3m[(måned+1+60):(måned+nCAPMslut+60),4])
  tstat_P_alpha3[i,1] <- summary(tmp2)$coefficients[1,3]
  JAlpha3[i,1] <- tmp2$coefficients[1]
  
  tmp3 <- lm(Ppre[,10+i]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2]+FF3m[(måned+1+60):(måned+nCAPMslut+60),3]+FF3m[(måned+1+60):(måned+nCAPMslut+60),4]+FF3m[(måned+1+60):(måned+nCAPMslut+60),8])
  tstat_P_alphaCar[i,1] <- summary(tmp3)$coefficients[1,3]
  JAlphaCar[i,1] <- tmp3$coefficients[1]
  
  
  tmp4 <- lm(Ppre[,10+i]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2]+FF3m[(måned+1+60):(måned+nCAPMslut+60),3]+FF3m[(måned+1+60):(måned+nCAPMslut+60),4]+FF3m[(måned+1+60):(måned+nCAPMslut+60),8]+FF3m[(måned+1+60):(måned+nCAPMslut+60),12]+FF3m[(måned+1+60):(måned+nCAPMslut+60),13])
  tstat_P_alphaCarREV[i,1] <- summary(tmp4)$coefficients[1,3]
  JAlphaCarREV[i,1] <- tmp4$coefficients[1]
  
  
}


for (j in 1:10){
  row.names(tabelPpre)[1] <- "Merafkast"; tabelPpre[1,j] <- exRet[j,1]
  row.names(tabelPpre)[2] <- "T-værdi"; tabelPpre[2,j] <- tstat_P_exRet[j,1]
  row.names(tabelPpre)[3] <- "CAPM alfa"; tabelPpre[3,j] <- JAlpha[j,1]
  row.names(tabelPpre)[4] <- "T-værdi "; tabelPpre[4,j] <- tstat_P_alpha[j,1]
  
  row.names(tabelPpre)[5] <- "3 faktor alfa"; tabelPpre[5,j] <- JAlpha3[j,1]
  row.names(tabelPpre)[6] <- "T-værdi  "; tabelPpre[6,j] <- tstat_P_alpha3[j,1]
  
  row.names(tabelPpre)[7] <- "Carhart alfa"; tabelPpre[7,j] <- JAlphaCar[j,1]
  row.names(tabelPpre)[8] <- "T-værdi   "; tabelPpre[8,j] <- tstat_P_alphaCar[j,1]

  
  row.names(tabelPpre)[9] <- "Carhart alfa REV"; tabelPpre[9,j] <- JAlphaCarREV[j,1]
  row.names(tabelPpre)[10] <- "T-værdi    "; tabelPpre[10,j] <- tstat_P_alphaCarREV[j,1]
  
  
  row.names(tabelPpre)[11] <- "Beta (real.)"; tabelPpre[11,j] <- RealBeta[j,1]
  row.names(tabelPpre)[12] <- "Sharpe ratio"; tabelPpre[12,j] <- 12*exRet[j,1]/(sd(Ppre[,10+j])*sqrt(12))
}
print(xtable(tabelPpre, type = "latex"), file = "tabel P1-P10 pre.tex") #TED-tabel



#Post
for (i in 1:p){
  tmp <- lm(Ppost[,10+i]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2])
  
  tmp2 <- lm(Ppost[,10+i]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4])
  tstat_P_alpha3[i,1] <- summary(tmp2)$coefficients[1,3]
  JAlpha3[i,1] <- tmp2$coefficients[1]
  
  tstat_P_alpha[i,1] <- summary(tmp)$coefficients[1,3]
  tstat_P_exRet[i,1] <- t.test(Ppost[,10+i], mu = 0)$statistic
  JAlpha[i,1] <- tmp$coefficients[1]
  RealBeta[i,1] <- tmp$coefficients[2]
  estBeta[i,1] <- mean(Ppost[,i])
  exRet[i,1] <- mean(Ppost[,10+i])  
  
  tmp3 <- lm(Ppost[,10+i]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),8])
  tstat_P_alphaCar[i,1] <- summary(tmp3)$coefficients[1,3]
  JAlphaCar[i,1] <- tmp3$coefficients[1]
  
  
  tmp4 <- lm(Ppost[,10+i]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),8]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),12]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),13])
  tstat_P_alphaCarREV[i,1] <- summary(tmp4)$coefficients[1,3]
  JAlphaCarREV[i,1] <- tmp4$coefficients[1]
  
  
  
  
}
for (j in 1:10){
  row.names(tabelPpost)[1] <- "Merafkast"; tabelPpost[1,j] <- exRet[j,1]
  row.names(tabelPpost)[2] <- "T-værdi"; tabelPpost[2,j] <- tstat_P_exRet[j,1]
  row.names(tabelPpost)[3] <- "CAPM alfa"; tabelPpost[3,j] <- JAlpha[j,1]
  row.names(tabelPpost)[4] <- "T-værdi "; tabelPpost[4,j] <- tstat_P_alpha[j,1]
  
  row.names(tabelPpost)[5] <- "3 faktor alfa"; tabelPpost[5,j] <- JAlpha3[j,1]
  row.names(tabelPpost)[6] <- "T-værdi  "; tabelPpost[6,j] <- tstat_P_alpha3[j,1]
  
  row.names(tabelPpost)[7] <- "Carhart alfa"; tabelPpost[7,j] <- JAlphaCar[j,1]
  row.names(tabelPpost)[8] <- "T-værdi   "; tabelPpost[8,j] <- tstat_P_alphaCar[j,1]
  
  
  row.names(tabelPpost)[9] <- "Carhart alfa REV"; tabelPpost[9,j] <- JAlphaCarREV[j,1]
  row.names(tabelPpost)[10] <- "T-værdi    "; tabelPpost[10,j] <- tstat_P_alphaCarREV[j,1]
  
  
  row.names(tabelPpost)[11] <- "Beta (real.)"; tabelPpost[11,j] <- RealBeta[j,1]
  row.names(tabelPpost)[12] <- "Sharpe ratio"; tabelPpost[12,j] <- 12*exRet[j,1]/(sd(Ppost[,10+j])*sqrt(12))
}
print(xtable(tabelPpost, type = "latex"), file = "tabel P1-P10 post.tex") #TED-tabel





#BarPlot

par(mar=c(4, 4, 2, 5) + 0.1)

PorteføljerPre  <- cbind(t(tabelPpre[3,1:10]),t(tabelPpre[5,1:10]),t(tabelPpre[7,1:10]),t(tabelPpre[9,1:10]))
PorteføljerPost <- cbind(t(tabelPpost[3,1:10]),t(tabelPpost[5,1:10]),t(tabelPpost[7,1:10]),t(tabelPpost[9,1:10]))



par(mar=c(2, 4, 1, 1))
xx <- barplot(t(PorteføljerPre), beside=T, #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="Alfa",xlab="Porteføljer",col=c("darkgreen","forestgreen","green","lawngreen"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.05,0.5))
#text(x = xx, y = PogBABAlpha, label = round(PogBABAlpha,2), pos = c(3,3,3,3,3,3,3,3,3,1,3), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red","darkgreen"))
#legend(37,0.45, c("Alfa","3-faktor alfa"),pch=c(15,15),col=c("darkgreen","limegreen"),pt.cex=2,box.col="white")
legend("topright", c("CAPM alfa","3-faktor alfa","Carhart alfa","Carhart + REV alfa"),pch=c(15,15,15,15),col=c("darkgreen","forestgreen","green","lawngreen"),pt.cex=2,box.col="transparent",box.lwd = 0,bg="transparent",ncol = 2)
box(lty = 'solid', col = 'black')


xx <- barplot(t(PorteføljerPost), beside=T, #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="Alfa",xlab="Porteføljer",col=c("darkblue","blue","deepskyblue","cyan"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.45,0.6))
#text(x = xx, y = PogBABAlpha, label = round(PogBABAlpha,2), pos = c(3,3,3,3,3,3,3,3,3,1,3), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red","darkgreen"))
#legend(37,0.45, c("Alfa","3-faktor alfa"),pch=c(15,15),col=c("darkgreen","limegreen"),pt.cex=2,box.col="white")
legend("topright", c("CAPM alfa","3-faktor alfa","Carhart alfa","Carhart + REV alfa"),pch=c(15,15,15,15),col=c("darkblue","blue","deepskyblue","cyan"),pt.cex=2,box.col="transparent",box.lwd = 0,bg="transparent",ncol = 2)
box(lty = 'solid', col = 'black')




PorteføljerMerPrePost  <- cbind(t(tabelPpre[1,1:10]),t(tabelPpost[1,1:10]))
#t-test
merafkastTest <- data.frame(matrix(0,nrow=10,ncol=1))
for (i in 1:p){
  merafkastTest[i,1] <- t.test(Ppost[,10+i],Ppre[,10+i])[1]
}

par(mar=c(2, 4, 1, 1))
xx <- barplot(t(PorteføljerMerPrePost), beside=T, #main="Jensens alfa fordelt ud på P1 - P10",
              ylab="Merafkast",xlab="Porteføljer",col=c("green3","blue"),names.arg=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),ylim=c(-0.20,1.6))
#text(x = xx, y = PogBABAlpha, label = round(PogBABAlpha,2), pos = c(3,3,3,3,3,3,3,3,3,1,3), cex = 0.8, col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","red","darkgreen"))
#legend(37,0.45, c("Alfa","3-faktor alfa"),pch=c(15,15),col=c("darkgreen","limegreen"),pt.cex=2,box.col="white")
legend("topleft", c("Præ CAPM","Post CAPM"),pch=c(15,15),col=c("green3","blue"),pt.cex=2,box.col="transparent",box.lwd = 0,bg="transparent",ncol = 1)
box(lty = 'solid', col = 'black')
text(2,-0.1, expression(paste(italic("(-0.64)")) ))
text(5,-0.1, expression(paste(italic("(-0.78)")) ))
text(8,-0.1, expression(paste(italic("(-0.20)")) ))
text(11,-0.1, expression(paste(italic("(-0.74)")) ))
text(14,-0.1, expression(paste(italic("(-1.10)")) ))
text(17,-0.1, expression(paste(italic("(-1.04)")) ))
text(20,-0.1, expression(paste(italic("(-1.40)")) ))
text(23,-0.1, expression(paste(italic("(-0.94)")) ))
text(26,-0.1, expression(paste(italic("(-1.32)")) ))
text(29,-0.1, expression(paste(italic("(-1.74)")) ))





############ BAB PRE OG POST CAPM ####################### 

BABpre <- BABpf[1:nCAPMslut,]
BABpost <- BABpf[(nCAPMslut+1):nTOTslut,]


tabelpre<- data.frame(matrix(data=0, nrow=13, ncol=3))
tabelpost<- data.frame(matrix(data=0, nrow=13, ncol=3))

tabelBetaerpre<- data.frame(matrix(data=0, nrow=28, ncol=3))
tabelBetaerpost<- data.frame(matrix(data=0, nrow=28, ncol=3))

colnames(tabelpre) <- c("Høj-beta","Lav beta","BAB")
colnames(tabelpost) <-  c("Høj-beta","Lav beta","BAB")

colnames(tabelBetaerpre) <-  c("Høj-beta","Lav beta","BAB")
colnames(tabelBetaerpost) <-  c("Høj-beta","Lav beta","BAB")

#PreCAPM:
summary(lm(BABpre[,3]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),3]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),4]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),8]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),12]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),13]))

summary(lm(BABpre[,2]~Raw_Monthly_mer[(måned+1+60):(måned+nCAPMslut+60),2])) #Beta T-værdi

for (i in 1:3){
  row.names(tabelpre)[1] <- "Merafkast"; tabelpre[1,i] <- mean(BABpre[,i],na.rm=TRUE)
  row.names(tabelpre)[2] <- "T-værdi"; tabelpre[2,i] <- t.test(BABpre[,i], mu = 0)$statistic
  
  
  tmp <- lm(BABpre[,i]~Raw_Monthly_mer[(måned+1+60):(måned+nCAPMslut+60),2])
  row.names(tabelpre)[3] <- "CAPM alfa";tabelpre[3,i] <- tmp$coefficients[1]
  row.names(tabelpre)[4] <- "T-værdi ";   tabelpre[4,i] <- summary(tmp)$coefficients[1,3]
  
  tmp2 <- lm(BABpre[,i]~Raw_Monthly_mer[(måned+1+60):(måned+nCAPMslut+60),2]+FF3m[(måned+1+60):(måned+nCAPMslut+60),3]+FF3m[(måned+1+60):(måned+nCAPMslut+60),4])
  row.names(tabelpre)[5] <- "3 faktor alfa"; tabelpre[5,i] <- tmp2$coefficients[1]
  row.names(tabelpre)[6] <- "T-værdi  "; tabelpre[6,i] <- summary(tmp2)$coefficients[1,3]
  
  tmp3 <- lm(BABpre[,i]~Raw_Monthly_mer[(måned+1+60):(måned+nCAPMslut+60),2]+FF3m[(måned+1+60):(måned+nCAPMslut+60),3]+FF3m[(måned+1+60):(måned+nCAPMslut+60),4]+FF3m[(måned+1+60):(måned+nCAPMslut+60),8])
  row.names(tabelpre)[7] <- "Carhart alfa"; tabelpre[7,i] <- tmp3$coefficients[1]
  row.names(tabelpre)[8] <- "T-værdi   "; tabelpre[8,i] <- summary(tmp3)$coefficients[1,3]
  
  tmp4 <- lm(BABpre[,i]~Raw_Monthly_mer[((måned+1+60):(måned+nCAPMslut+60)),2]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),3]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),4]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),8]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),12]+FF3m[((måned+1+60):(måned+nCAPMslut+60)),13])
  row.names(tabelpre)[9] <- "C alfa"; tabelpre[9,i] <- NA
  row.names(tabelpre)[10] <- "T-værdi    "; tabelpre[10,i] <- NA
  
  
  row.names(tabelpre)[11] <- "Beta (real.)";  tabelpre[11,i] <- tmp$coefficients[2]
  row.names(tabelpre)[12] <- "Volatilitet"; tabelpre[12,i] <- sd(BABpre[,i],na.rm=TRUE)*sqrt(12)
  row.names(tabelpre)[13] <- "Sharpe ratio"; tabelpre[13,i] <- mean(BABpre[,i],na.rm=TRUE)/sd(BABpre[,i],na.rm=TRUE)*sqrt(12)
  

  
  
  tabelBetaerpre[1,i] <- summary(tmp)$coefficients[2,1];row.names(tabelBetaerpre)[1] <- "Markeds beta"
  tabelBetaerpre[2,i] <- summary(tmp)$coefficients[2,3]
  
 
  tabelBetaerpre[3,i] <- summary(tmp2)$coefficients[2,1];row.names(tabelBetaerpre)[3] <- "Markeds beta "
  tabelBetaerpre[4,i] <- summary(tmp2)$coefficients[2,3]
  
  tabelBetaerpre[5,i] <- summary(tmp2)$coefficients[3,1];row.names(tabelBetaerpre)[5] <- "SMB beta"
  tabelBetaerpre[6,i] <- summary(tmp2)$coefficients[3,3]
  
  tabelBetaerpre[7,i] <- summary(tmp2)$coefficients[4,1];row.names(tabelBetaerpre)[7] <- "HML beta"
  tabelBetaerpre[8,i] <- summary(tmp2)$coefficients[4,3]
  
  
  tabelBetaerpre[9,i] <- summary(tmp3)$coefficients[2,1];row.names(tabelBetaerpre)[9] <- "Markeds beta  "
  tabelBetaerpre[10,i] <- summary(tmp3)$coefficients[2,3]
  
  tabelBetaerpre[11,i] <- summary(tmp3)$coefficients[3,1];row.names(tabelBetaerpre)[11] <- "SMB beta "
  tabelBetaerpre[12,i] <- summary(tmp3)$coefficients[3,3]
  
  tabelBetaerpre[13,i] <- summary(tmp3)$coefficients[4,1];row.names(tabelBetaerpre)[13] <- "HML beta "
  tabelBetaerpre[14,i] <- summary(tmp3)$coefficients[4,3]
  
  tabelBetaerpre[15,i] <- summary(tmp3)$coefficients[5,1];row.names(tabelBetaerpre)[15] <- "MOM beta"
  tabelBetaerpre[16,i] <- summary(tmp3)$coefficients[5,3]
  
  

  
  
  
}


#Post CAPM

summary(lm(BABpost[,3]~Raw_Monthly_mer[(måned+nCAPMslut+60+1):(n_Monthly),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),8]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),12]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),13]))
summary(lm(BABpost[,3]~Raw_Monthly_mer[(måned+nCAPMslut+60+1):(n_Monthly),2]))


for (i in 1:3){
  row.names(tabelpost)[1] <- "Merafkast"; tabelpost[1,i] <- mean(BABpost[,i],na.rm=TRUE)
  row.names(tabelpost)[2] <- "T-værdi"; tabelpost[2,i] <- t.test(BABpost[,i], mu = 0)$statistic
  
  tmp <- lm(BABpost[,i]~Raw_Monthly_mer[((måned+nCAPMslut+60+1):(n_Monthly)),2])
  row.names(tabelpost)[3] <- "CAPM alfa";tabelpost[3,i] <- tmp$coefficients[1]
  row.names(tabelpost)[4] <- "T-værdi ";   tabelpost[4,i] <- summary(tmp)$coefficients[1,3]
  
  tmp2 <- lm(BABpost[,i]~Raw_Monthly_mer[(måned+nCAPMslut+60+1):(n_Monthly),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4])
  row.names(tabelpost)[5] <- "3 faktor alfa"; tabelpost[5,i] <- tmp2$coefficients[1]
  row.names(tabelpost)[6] <- "T-værdi  "; tabelpost[6,i] <- summary(tmp2)$coefficients[1,3]
  
  
  tmp3 <- lm(BABpost[,i]~Raw_Monthly_mer[(måned+nCAPMslut+60+1):(n_Monthly),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),3]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),8])
  row.names(tabelpost)[7] <- "Carhart alfa"; tabelpost[7,i] <- tmp3$coefficients[1]
  row.names(tabelpost)[8] <- "T-værdi   "; tabelpost[8,i] <- summary(tmp3)$coefficients[1,3]
  
  tmp4 <- lm(BABpost[,i]~Raw_Monthly_mer[(måned+nCAPMslut+60+1):(n_Monthly),2]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),11]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),4]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),8]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),9]+FF3m[(måned+nCAPMslut+60+1):(n_Monthly),10])
  row.names(tabelpost)[9] <- "FF6 alfa"; tabelpost[9,i] <- tmp4$coefficients[1]
  row.names(tabelpost)[10] <-"T-værdi    "; tabelpost[10,i] <- summary(tmp4)$coefficients[1,3]
  
  
  
  row.names(tabelpost)[11] <- "Beta (real.)";  tabelpost[11,i] <- tmp$coefficients[2]
  row.names(tabelpost)[12] <- "Volatilitet"; tabelpost[12,i] <- sd(BABpost[,i],na.rm=TRUE)*sqrt(12)
  row.names(tabelpost)[13] <- "Sharpe ratio"; tabelpost[13,i] <- mean(BABpost[,i],na.rm=TRUE)/sd(BABpost[,i],na.rm=TRUE)*sqrt(12)
  

  
  
  
  
  tabelBetaerpost[1,i] <- summary(tmp)$coefficients[2,1];row.names(tabelBetaerpost)[1] <- "Markeds beta"
  tabelBetaerpost[2,i] <- summary(tmp)$coefficients[2,3]
  
  
  tabelBetaerpost[3,i] <- summary(tmp2)$coefficients[2,1];row.names(tabelBetaerpost)[3] <- "Markeds beta "
  tabelBetaerpost[4,i] <- summary(tmp2)$coefficients[2,3]
  
  tabelBetaerpost[5,i] <- summary(tmp2)$coefficients[3,1];row.names(tabelBetaerpost)[5] <- "SMB beta"
  tabelBetaerpost[6,i] <- summary(tmp2)$coefficients[3,3]
  
  tabelBetaerpost[7,i] <- summary(tmp2)$coefficients[4,1];row.names(tabelBetaerpost)[7] <- "HML beta"
  tabelBetaerpost[8,i] <- summary(tmp2)$coefficients[4,3]
  
  
  tabelBetaerpost[9,i] <- summary(tmp3)$coefficients[2,1];row.names(tabelBetaerpost)[9] <- "Markeds beta  "
  tabelBetaerpost[10,i] <- summary(tmp3)$coefficients[2,3]
  
  tabelBetaerpost[11,i] <- summary(tmp3)$coefficients[3,1];row.names(tabelBetaerpost)[11] <- "SMB beta "
  tabelBetaerpost[12,i] <- summary(tmp3)$coefficients[3,3]
  
  tabelBetaerpost[13,i] <- summary(tmp3)$coefficients[4,1];row.names(tabelBetaerpost)[13] <- "HML beta "
  tabelBetaerpost[14,i] <- summary(tmp3)$coefficients[4,3]
  
  tabelBetaerpost[15,i] <- summary(tmp3)$coefficients[5,1];row.names(tabelBetaerpost)[15] <- "MOM beta"
  tabelBetaerpost[16,i] <- summary(tmp3)$coefficients[5,3]
  
  
  summary(tmp4)
  summary(tmp4)$coefficients[2,1];summary(tmp4)$coefficients[2,3] #Mkt beta
  tabelBetaerpost[17,i] <- summary(tmp4)$coefficients[2,1];row.names(tabelBetaerpost)[17] <- "mkt beta    "
  tabelBetaerpost[18,i] <- summary(tmp4)$coefficients[2,3]
  
  summary(tmp4)$coefficients[3,1];summary(tmp4)$coefficients[3,3] #SMB5 beta
  tabelBetaerpost[19,i] <- summary(tmp4)$coefficients[3,1];row.names(tabelBetaerpost)[19] <- "SMB5 beta"
  tabelBetaerpost[20,i] <- summary(tmp4)$coefficients[3,3]
  
  summary(tmp4)$coefficients[4,1];summary(tmp4)$coefficients[4,3] #HML beta
  tabelBetaerpost[21,i] <- summary(tmp4)$coefficients[4,1];row.names(tabelBetaerpost)[21] <- "HML beta    "
  tabelBetaerpost[22,i] <- summary(tmp4)$coefficients[4,3]
  
  summary(tmp4)$coefficients[5,1];summary(tmp4)$coefficients[5,3] #MOM beta
  tabelBetaerpost[23,i] <- summary(tmp4)$coefficients[5,1];row.names(tabelBetaerpost)[23] <- "MOM beta     "
  tabelBetaerpost[24,i] <- summary(tmp4)$coefficients[5,3]
  
  summary(tmp4)$coefficients[6,1];summary(tmp4)$coefficients[6,3] #RMW beta
  tabelBetaerpost[25,i] <- summary(tmp4)$coefficients[6,1];row.names(tabelBetaerpost)[25] <- "RMW beta"
  tabelBetaerpost[26,i] <- summary(tmp4)$coefficients[6,3]
  
  summary(tmp4)$coefficients[7,1];summary(tmp4)$coefficients[7,3] #CMA beta
  tabelBetaerpost[27,i] <- summary(tmp4)$coefficients[7,1];row.names(tabelBetaerpost)[27] <- "CMA beta"
  tabelBetaerpost[28,i] <- summary(tmp4)$coefficients[7,3]
  
  }

tabelBetaerprepost <- cbind(tabelBetaerpre,tabelBetaerpost)
print(xtable(tabelBetaerprepost, type = "latex"), file = "tabel betaer prepost.tex") #TED-tabel





tabelprepost<- data.frame(matrix(data=0, nrow=13, ncol=6))
for (i in 1:13){
  row.names(tabelprepost)[i] <- row.names(tabelpre)[i]
  for (j in 1:3){
    colnames(tabelprepost)[j] <- colnames(tabelpre)[j]
    tabelprepost[i,j] <- tabelpre[i,j]
    
    colnames(tabelprepost)[j+3] <- colnames(tabelpre)[j]
    tabelprepost[i,j+3] <- tabelpost[i,j]
    
  }
};tabelprepost<-round(tabelprepost,2)
print(xtable(tabelprepost, type = "latex"), file = "LigeVærdi prepost tabel.tex") #Gemmes som LigeVærdi




############ Test for forskelle Pre og Post alt ####################
t.test(BABpre[,3],BABpost[,3], alternative = "two.sided",paired = FALSE)
#øv


nBABpfstart <- 1
nBABpfCAPMslut <- 371
nBABpfFFslut <- 719
nBABpfBABslut <- 959
nBABpfslut <- 1043

nFFstart <- 92
nFFCAPMslut <- 462
nFFFFslut <- 810
nFFBABslut <- 1050
nFFslut <- n_Monthly



#Pre og Post CAPM
t.test(BABpf[(nBABpfCAPMslut+1):nBABpfslut,3],BABpf[nBABpfstart:nBABpfCAPMslut,3], alternative = "two.sided",paired = FALSE)


MKT<-FF3m[nFFstart:n_Monthly,2]
SMB<-FF3m[nFFstart:n_Monthly,3]
HML<-FF3m[nFFstart:n_Monthly,4]
MOM<-FF3m[nFFstart:n_Monthly,8]
LTREV<-FF3m[nFFstart:n_Monthly,12]
STREV<-FF3m[nFFstart:n_Monthly,13]

PostCAPM<-c(rep(0,nBABpfCAPMslut),rep(1,nBABpfslut-nBABpfCAPMslut))
y<-BABpf[nBABpfstart:nBABpfslut,3]
fit<-lm(y~MKT*factor(PostCAPM));summary(fit) #CAPM
fit<-lm(y~MKT*factor(PostCAPM)+SMB*factor(PostCAPM)+HML*factor(PostCAPM));summary(fit) #3-faktor
#fit<-lm(y~MKT*factor(PostCAPM)+SMB*factor(PostCAPM)+HML*factor(PostCAPM)+MOM*factor(PostCAPM));summary(fit) #Carhart
fit<-lm(y~MKT*factor(PostCAPM)+SMB*factor(PostCAPM)+HML*factor(PostCAPM)+MOM*factor(PostCAPM)+LTREV*factor(PostCAPM)+STREV*factor(PostCAPM));summary(fit)#Carhart +REV


par(mar=c(2, 4, 1, 1)) #uden xlab
plot(ts(BABpf[,4],start=c(1934,2), end=2021, frequency = 12),ylab="Kumuleret BAB-afkast",xlab="Tid",las=1) #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
lines(x=c(1934,1965),y=c(1,BABpf[nBABpfCAPMslut,4]),col=rgb(0.3,1,0.3),lwd=2)
lines(x=c(1965,2021),y=c(BABpf[(nBABpfCAPMslut+1),4],BABpf[nBABpfslut,4]),col=rgb(0.529411765, 0.807843137, 0.980392157),lwd=2)
abline(v=1965)
text(1948, 650, "Præ CAPM"); text(1995, 650, "Post CAPM")



#Pre og Post FF
t.test(BABpf[nBABpfstart:nBABpfFFslut,3],BABpf[(nBABpfFFslut+1):nBABpfslut,3], alternative = "two.sided",paired = FALSE)

MKT<-FF3m[nFFstart:n_Monthly,2]
SMB<-FF3m[nFFstart:n_Monthly,3]
HML<-FF3m[nFFstart:n_Monthly,4]
MOM<-FF3m[nFFstart:n_Monthly,8]
LTREV<-FF3m[nFFstart:n_Monthly,12]
STREV<-FF3m[nFFstart:n_Monthly,13]

group<-c(rep(0,nBABpfFFslut),rep(1,nBABpfslut-nBABpfFFslut))
y<-BABpf[nBABpfstart:nBABpfslut,3]
fit<-lm(y~MKT*factor(group));summary(fit) #CAPM
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group));summary(fit) #3-faktor
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group));summary(fit) #Carhart
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group));summary(fit)#Carhart +REV
summary(fit)


par(mar=c(2, 4, 1, 1)) #uden xlab
plot(ts(BABpf[,4],start=c(1934,2), end=2021, frequency = 12),ylab="Kumuleret BAB-afkast",xlab="Tid",las=1) #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
lines(x=c(1934,1994),y=c(1,BABpf[nBABpfFFslut,4]),col=rgb(0.3,1,0.3),lwd=2)
lines(x=c(1994,2021),y=c(BABpf[(nBABpfFFslut)+1,4],BABpf[nBABpfslut,4]),col=rgb(0.529411765, 0.807843137, 0.980392157),lwd=2)
abline(v=1994)
text(1965, 700, "Præ Fama French"); text(2010, 700, "Post Fama French")




#Pre og Post BAB
t.test(BABpf[nBABpfstart:nBABpfBABslut,3],BABpf[(nBABpfBABslut+1):nBABpfslut,3], alternative = "two.sided",paired = FALSE)

MKT<-FF3m[nFFstart:n_Monthly,2]
SMB<-FF3m[nFFstart:n_Monthly,3]
HML<-FF3m[nFFstart:n_Monthly,4]
MOM<-FF3m[nFFstart:n_Monthly,8]
LTREV<-FF3m[nFFstart:n_Monthly,12]
STREV<-FF3m[nFFstart:n_Monthly,13]

group<-c(rep(0,nBABpfBABslut),rep(1,nBABpfslut-nBABpfBABslut))
y<-BABpf[nBABpfstart:nBABpfslut,3]
fit<-lm(y~MKT*factor(group));summary(fit) #CAPM
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group));summary(fit) #3-faktor
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group));summary(fit) #Carhart
fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group));#Carhart +REV
summary(fit)

par(mar=c(2, 4, 1, 1)) #uden xlab
plot(ts(BABpf[,4],start=c(1934,2), end=2021, frequency = 12),ylab="Kumuleret BAB-afkast",xlab="Tid",las=1) #Tror den hopper i starten grundet negativt cummulative afkast, og log til negativt er lort :)
lines(x=c(1934,2014),y=c(1,BABpf[nBABpfBABslut,4]),col=rgb(0.3,1,0.3),lwd=2)
lines(x=c(2014,2021),y=c(BABpf[(nBABpfBABslut)+1,4],BABpf[nBABpfslut,4]),col=rgb(0.529411765, 0.807843137, 0.980392157),lwd=2)
abline(v=2014)
text(1965, 700, "Præ BAB"); text(2010, 700, "Post BAB")






############ SML pre og post #######################

#Test for om marrkedet har ændret sig
t.test(FF3m[nFFstart:nFFCAPMslut,2],FF3m[(nFFCAPMslut+1):nFFslut,2], alternative = "two.sided",paired = FALSE)



apre1 <- colMeans(Raw_daily_mer[2272:10799,(1+Forskydning):(c+Forskydning)],na.rm=TRUE)/100*250
bpre1 <- colMeans(betasNon[60:430,1:c],na.rm = TRUE)

apost1 <- colMeans(Raw_daily_mer[2273:n_Daily,(1+Forskydning):(c+Forskydning)],na.rm=TRUE)/100*250
bpost1 <- colMeans(betasNon[431:AntalMåneder,1:c],na.rm = TRUE)

apre <- apre1[-c(1,4,12,21,27,28,37)]
bpre <- bpre1[-c(1,4,12,21,27,28,37)]
apost <- apost1[-1]
bpost <- bpost1[-1]

MerAfkast <- union(apre,apost)
Betaer <- union(bpre,bpost)

PostCAPM<-c(rep(0,43),rep(1,49))
fit<-lm(MerAfkast~Betaer*factor(PostCAPM));summary(fit) #Signifikant forskelig fra hinanden

#prepost SML
par(mar=c(4, 4, 1, 1)) #med  xlab
par(mfrow=c(1,1))
plot(bpre[1:43],apre[1:43],ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1,col="darkgreen",lwd=1,pch=16,las=1,yaxs="i",xaxs="i")
abline(0,apre1[1],col="darkgreen",lty=2)
l<-lm(apre[1:43]~bpre[1:43]);   abline(l$coefficients[1],l$coefficients[2],col="darkgreen",lwd=1)
par(new = TRUE)
plot(bpost[1:49],apost[1:49],ylim = c(0,0.3),xlim=c(0,1.8),xlab="Beta",ylab="E(r) - rf",cex.main = 1.1,col="darkblue",pch=16,lwd=1,las=1,yaxs="i",xaxs="i")
abline(0,apost1[1],col="darkblue",lty=2)
l<-lm(apost[1:49]~bpost[1:49]);   abline(l$coefficients[1],l$coefficients[2],col="darkblue",lwd=1)
legend(0,0.3,c("Præ CAPM empirisk SML","Post CAPM empirisk SML","Præ CAPM teoretisk SML","Post CAPM teoretisk SML"),col=c("darkgreen","darkblue","darkgreen","darkblue"),lwd=c(2,2,2,2),lty=c(1,1,2,2),box.col = "transparent",bg="transparent",ncol=1,cex = 0.8)











############# BAB PRÆ OG POST smart beta ##################
#Tabel Pre VS Post CAPM for BAB:

TabelprepostFF <- data.frame(matrix(nrow=11,ncol=3))
row.names(TabelprepostFF)[1] <- "Merafkast"
row.names(TabelprepostFF)[2] <- "T-værdi"
row.names(TabelprepostFF)[3] <- "CAPM alfa"
row.names(TabelprepostFF)[4] <- "T-værdi "
row.names(TabelprepostFF)[5] <- "3-faktor alfa"
row.names(TabelprepostFF)[6] <- "T-værdi  "
row.names(TabelprepostFF)[7] <- "Carhart + REV alfa"
row.names(TabelprepostFF)[8] <- "T-værdi   "
row.names(TabelprepostFF)[9] <- "Beta (real.)"
row.names(TabelprepostFF)[10] <- "Volatilitet"
row.names(TabelprepostFF)[11] <- "Sharpe Ratio"
colnames(TabelprepostFF) <- c("Præ FF","Post FF","Præ VS Post - Anders TEST")


#pre
TabelprepostFF[1,1] <- mean(BABpf[nBABpfstart:nBABpfFFslut,3],na.rm = TRUE)
TabelprepostFF[2,1] <- t.test(BABpf[nBABpfstart:nBABpfFFslut,3])[1]
tmp <- lm(BABpf[nBABpfstart:nBABpfFFslut,3]~FF3m[nFFstart:nFFFFslut,2])
TabelprepostFF[3,1] <-summary(tmp)$coefficients[1,1]
TabelprepostFF[4,1] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(BABpf[nBABpfstart:nBABpfFFslut,3]~FF3m[nFFstart:nFFFFslut,2]+FF3m[nFFstart:nFFFFslut,3]+FF3m[nFFstart:nFFFFslut,4])
TabelprepostFF[5,1] <-summary(tmp2)$coefficients[1,1]
TabelprepostFF[6,1] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(BABpf[nBABpfstart:nBABpfFFslut,3]~FF3m[nFFstart:nFFFFslut,2]+FF3m[nFFstart:nFFFFslut,3]+FF3m[nFFstart:nFFFFslut,4]+FF3m[nFFstart:nFFFFslut,8]+FF3m[nFFstart:nFFFFslut,12]+FF3m[nFFstart:nFFFFslut,13]) 
TabelprepostFF[7,1] <-summary(tmp3)$coefficients[1,1]
TabelprepostFF[8,1] <-summary(tmp3)$coefficients[1,3]

TabelprepostFF[9,1] <-summary(tmp)$coefficients[2,1]

TabelprepostFF[10,1] <-sd(BABpf[nBABpfstart:nBABpfFFslut,3],na.rm = TRUE)*sqrt(12)

TabelprepostFF[11,1] <- mean(BABpf[nBABpfstart:nBABpfFFslut,3],na.rm = TRUE) / sd(BABpf[nBABpfstart:nBABpfFFslut,3],na.rm = TRUE)*sqrt(12)


#post
TabelprepostFF[1,2] <-mean(BABpf[(nBABpfFFslut+1):nBABpfslut,3],na.rm = TRUE)
TabelprepostFF[2,2] <- t.test(BABpf[(nBABpfFFslut+1):nBABpfslut,3])[1]
tmp <- lm(BABpf[(nBABpfFFslut+1):nBABpfslut,3]~FF3m[(nFFFFslut+1):nFFslut,2])
TabelprepostFF[3,2] <-summary(tmp)$coefficients[1,1]
TabelprepostFF[4,2] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(BABpf[(nBABpfFFslut+1):nBABpfslut,3]~FF3m[(nFFFFslut+1):nFFslut,2]+FF3m[(nFFFFslut+1):nFFslut,3]+FF3m[(nFFFFslut+1):nFFslut,4])
TabelprepostFF[5,2] <-summary(tmp2)$coefficients[1,1]
TabelprepostFF[6,2] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(BABpf[(nBABpfFFslut+1):nBABpfslut,3]~FF3m[(nFFFFslut+1):nFFslut,2]+FF3m[(nFFFFslut+1):nFFslut,3]+FF3m[(nFFFFslut+1):nFFslut,4]+FF3m[(nFFFFslut+1):nFFslut,8]+FF3m[(nFFFFslut+1):nFFslut,12]+FF3m[(nFFFFslut+1):nFFslut,13]) 
TabelprepostFF[7,2] <-summary(tmp3)$coefficients[1,1]
TabelprepostFF[8,2] <-summary(tmp3)$coefficients[1,3]

TabelprepostFF[9,2] <-summary(tmp)$coefficients[2,1]

TabelprepostFF[10,2] <-sd(BABpf[(nBABpfFFslut+1):nBABpfslut,3],na.rm = TRUE)*sqrt(12)

TabelprepostFF[11,2] <-mean(BABpf[(nBABpfFFslut+1):nBABpfslut,3],na.rm = TRUE) / sd(BABpf[(nBABpfFFslut+1):nBABpfslut,3],na.rm = TRUE)*sqrt(12)



#Pre og Post FF

TabelprepostFF[2,3] <- t.test(BABpf[(nBABpfFFslut+1):nBABpfslut,3],BABpf[nBABpfstart:nBABpfFFslut,3])[1]

group<-c(rep(0,nBABpfFFslut),rep(1,nBABpfslut-nBABpfFFslut))
fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group))
TabelprepostFF[4,3] <- summary(fit)$coefficients[3,3] #CAPM

fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group)+SMB*factor(group)+HML*factor(group))
TabelprepostFF[6,3] <- summary(fit)$coefficients[3,3] #3-faktor

fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group))
TabelprepostFF[8,3] <- summary(fit)$coefficients[3,3]#Carhart +REV

print(xtable(TabelprepostFF, type = "latex"), file = "TabelprepostFF.tex") #TED-tabel











############# BAB PRÆ OG POST BAB ##################
#Tabel Pre VS Post BABfor BAB:

TabelprepostBAB <- data.frame(matrix(nrow=11,ncol=3))
row.names(TabelprepostBAB)[1] <- "Merafkast"
row.names(TabelprepostBAB)[2] <- "T-værdi"
row.names(TabelprepostBAB)[3] <- "CAPM alfa"
row.names(TabelprepostBAB)[4] <- "T-værdi "
row.names(TabelprepostBAB)[5] <- "3-faktor alfa"
row.names(TabelprepostBAB)[6] <- "T-værdi  "
row.names(TabelprepostBAB)[7] <- "Carhart + REV alfa"
row.names(TabelprepostBAB)[8] <- "T-værdi   "
row.names(TabelprepostBAB)[9] <- "Beta (real.)"
row.names(TabelprepostBAB)[10] <- "Volatilitet"
row.names(TabelprepostBAB)[11] <- "Sharpe Ratio"
colnames(TabelprepostBAB) <- c("Præ BAB","Post BAB","Præ VS Post - Anders TEST")


#pre
TabelprepostBAB[1,1] <- mean(BABpf[nBABpfstart:nBABpfBABslut,3],na.rm = TRUE)
TabelprepostBAB[2,1] <- t.test(BABpf[nBABpfstart:nBABpfBABslut,3])[1]
tmp <- lm(BABpf[nBABpfstart:nBABpfBABslut,3]~FF3m[nFFstart:nFFBABslut,2])
TabelprepostBAB[3,1] <-summary(tmp)$coefficients[1,1]
TabelprepostBAB[4,1] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(BABpf[nBABpfstart:nBABpfBABslut,3]~FF3m[nFFstart:nFFBABslut,2]+FF3m[nFFstart:nFFBABslut,3]+FF3m[nFFstart:nFFBABslut,4])
TabelprepostBAB[5,1] <-summary(tmp2)$coefficients[1,1]
TabelprepostBAB[6,1] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(BABpf[nBABpfstart:nBABpfBABslut,3]~FF3m[nFFstart:nFFBABslut,2]+FF3m[nFFstart:nFFBABslut,3]+FF3m[nFFstart:nFFBABslut,4]+FF3m[nFFstart:nFFBABslut,8]+FF3m[nFFstart:nFFBABslut,12]+FF3m[nFFstart:nFFBABslut,13]) 
TabelprepostBAB[7,1] <-summary(tmp3)$coefficients[1,1]
TabelprepostBAB[8,1] <-summary(tmp3)$coefficients[1,3]

TabelprepostBAB[9,1] <-summary(tmp)$coefficients[2,1]

TabelprepostBAB[10,1] <-sd(BABpf[nBABpfstart:nBABpfBABslut,3],na.rm = TRUE)*sqrt(12)

TabelprepostBAB[11,1] <- mean(BABpf[nBABpfstart:nBABpfBABslut,3],na.rm = TRUE) / sd(BABpf[nBABpfstart:nBABpfBABslut,3],na.rm = TRUE)*sqrt(12)


#post
TabelprepostBAB[1,2] <-mean(BABpf[(nBABpfBABslut+1):nBABpfslut,3],na.rm = TRUE)
TabelprepostBAB[2,2] <- t.test(BABpf[(nBABpfBABslut+1):nBABpfslut,3])[1]
tmp <- lm(BABpf[(nBABpfBABslut+1):nBABpfslut,3]~FF3m[(nFFBABslut+1):nFFslut,2])
TabelprepostBAB[3,2] <-summary(tmp)$coefficients[1,1]
TabelprepostBAB[4,2] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(BABpf[(nBABpfBABslut+1):nBABpfslut,3]~FF3m[(nFFBABslut+1):nFFslut,2]+FF3m[(nFFBABslut+1):nFFslut,3]+FF3m[(nFFBABslut+1):nFFslut,4])
TabelprepostBAB[5,2] <-summary(tmp2)$coefficients[1,1]
TabelprepostBAB[6,2] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(BABpf[(nBABpfBABslut+1):nBABpfslut,3]~FF3m[(nFFBABslut+1):nFFslut,2]+FF3m[(nFFBABslut+1):nFFslut,3]+FF3m[(nFFBABslut+1):nFFslut,4]+FF3m[(nFFBABslut+1):nFFslut,8]+FF3m[(nFFBABslut+1):nFFslut,12]+FF3m[(nFFBABslut+1):nFFslut,13]) 
TabelprepostBAB[7,2] <-summary(tmp3)$coefficients[1,1]
TabelprepostBAB[8,2] <-summary(tmp3)$coefficients[1,3]

TabelprepostBAB[9,2] <-summary(tmp)$coefficients[2,1]

TabelprepostBAB[10,2] <-sd(BABpf[(nBABpfBABslut+1):nBABpfslut,3],na.rm = TRUE)*sqrt(12)

TabelprepostBAB[11,2] <-mean(BABpf[(nBABpfBABslut+1):nBABpfslut,3],na.rm = TRUE) / sd(BABpf[(nBABpfBABslut+1):nBABpfslut,3],na.rm = TRUE)*sqrt(12)



#Pre og Post FF

TabelprepostBAB[2,3] <- t.test(BABpf[(nBABpfBABslut+1):nBABpfslut,3],BABpf[nBABpfstart:nBABpfBABslut,3])[1]

group<-c(rep(0,nBABpfBABslut),rep(1,nBABpfslut-nBABpfBABslut))
fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group))
TabelprepostBAB[4,3] <- summary(fit)$coefficients[3,3] #CAPM

fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group)+SMB*factor(group)+HML*factor(group))
TabelprepostBAB[6,3] <- summary(fit)$coefficients[3,3] #3-faktor

fit<-lm(BABpf[nBABpfstart:nBABpfslut,3]~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group))
TabelprepostBAB[8,3] <- summary(fit)$coefficients[3,3]#Carhart +REV

print(xtable(TabelprepostBAB, type = "latex"), file = "TabelprepostBAB.tex") #TED-tabel









############# Sammenligning med AQR datasæt PRÆ OG POST CAPM###########################
library(readxl)

AQR_sorteret <- as.data.frame(read_excel("C:/Users/ander/OneDrive - CBS - Copenhagen Business School/Speciale/Data/AQR sorteret.xlsx", 
                                         col_types = c("date", "numeric")));AQR_sorteret[,2] <- AQR_sorteret[,2]*100


row.names(BABpf)[nBABpfstart]
nAQRstart <- 39
nAQRCAPMslut <- 409
nAQRFFslut <- 757
nAQRBABslut <- 997
nAQRslut <- 1081


MKT<-FF3m[nFFstart:n_Monthly,2]
SMB<-FF3m[nFFstart:n_Monthly,3]
HML<-FF3m[nFFstart:n_Monthly,4]
MOM<-FF3m[nFFstart:n_Monthly,8]
LTREV<-FF3m[nFFstart:n_Monthly,12]
STREV<-FF3m[nFFstart:n_Monthly,13]


#Tabel Pre VS Post CAPM:

TabelAQRprepostCAPM <- data.frame(matrix(nrow=11,ncol=3))
row.names(TabelAQRprepostCAPM)[1] <- "Merafkast"
row.names(TabelAQRprepostCAPM)[2] <- "T-værdi"
row.names(TabelAQRprepostCAPM)[3] <- "CAPM alfa"
row.names(TabelAQRprepostCAPM)[4] <- "T-værdi "
row.names(TabelAQRprepostCAPM)[5] <- "3-faktor alfa"
row.names(TabelAQRprepostCAPM)[6] <- "T-værdi  "
row.names(TabelAQRprepostCAPM)[7] <- "Carhart + REV alfa"
row.names(TabelAQRprepostCAPM)[8] <- "T-værdi   "
row.names(TabelAQRprepostCAPM)[9] <- "Beta (real.)"
row.names(TabelAQRprepostCAPM)[10] <- "Volatilitet"
row.names(TabelAQRprepostCAPM)[11] <- "Sharpe Ratio"
colnames(TabelAQRprepostCAPM) <- c("Præ CAPM","Post CAPM","Præ VS Post - Anders TEST")
y<-AQR_sorteret[nAQRstart:nAQRslut,2]
#pre
TabelAQRprepostCAPM[1,1] <- mean(AQR_sorteret[nAQRstart:nAQRCAPMslut,2])
TabelAQRprepostCAPM[2,1] <- t.test(AQR_sorteret[nAQRstart:nAQRCAPMslut,2])[1]
tmp <- lm(AQR_sorteret[nAQRstart:nAQRCAPMslut,2]~FF3m[nFFstart:nFFCAPMslut,2])
TabelAQRprepostCAPM[3,1] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostCAPM[4,1] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[nAQRstart:nAQRCAPMslut,2]~FF3m[nFFstart:nFFCAPMslut,2]+FF3m[nFFstart:nFFCAPMslut,3]+FF3m[nFFstart:nFFCAPMslut,4])
TabelAQRprepostCAPM[5,1] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostCAPM[6,1] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[nAQRstart:nAQRCAPMslut,2]~FF3m[nFFstart:nFFCAPMslut,2]+FF3m[nFFstart:nFFCAPMslut,3]+FF3m[nFFstart:nFFCAPMslut,4]+FF3m[nFFstart:nFFCAPMslut,8]+FF3m[nFFstart:nFFCAPMslut,12]+FF3m[nFFstart:nFFCAPMslut,13]) 
TabelAQRprepostCAPM[7,1] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostCAPM[8,1] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostCAPM[9,1] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostCAPM[10,1] <-sd(AQR_sorteret[nAQRstart:nAQRCAPMslut,2])*sqrt(12)

TabelAQRprepostCAPM[11,1] <- mean(AQR_sorteret[nAQRstart:nAQRCAPMslut,2]) / sd(AQR_sorteret[nAQRstart:nAQRCAPMslut,2])*sqrt(12)

AQR_sorteret[nAQRstart:nAQRCAPMslut,1]

#post
TabelAQRprepostCAPM[1,2] <-mean(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2])
TabelAQRprepostCAPM[2,2] <- t.test(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2])[1]
tmp <- lm(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2]~FF3m[(nFFCAPMslut+1):nFFslut,2])
TabelAQRprepostCAPM[3,2] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostCAPM[4,2] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2]~FF3m[(nFFCAPMslut+1):nFFslut,2]+FF3m[(nFFCAPMslut+1):nFFslut,3]+FF3m[(nFFCAPMslut+1):nFFslut,4])
TabelAQRprepostCAPM[5,2] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostCAPM[6,2] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2]~FF3m[(nFFCAPMslut+1):nFFslut,2]+FF3m[(nFFCAPMslut+1):nFFslut,3]+FF3m[(nFFCAPMslut+1):nFFslut,4]+FF3m[(nFFCAPMslut+1):nFFslut,8]+FF3m[(nFFCAPMslut+1):nFFslut,12]+FF3m[(nFFCAPMslut+1):nFFslut,13]) 
TabelAQRprepostCAPM[7,2] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostCAPM[8,2] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostCAPM[9,2] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostCAPM[10,2] <-sd(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2])*sqrt(12)

TabelAQRprepostCAPM[11,2] <-mean(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2]) / sd(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2])*sqrt(12)



#Pre og Post CAPM
TabelAQRprepostCAPM[2,3] <- t.test(AQR_sorteret[(nAQRCAPMslut+1):nAQRslut,2],AQR_sorteret[nAQRstart:nAQRCAPMslut,2])[1]

group<-c(rep(0,(nAQRCAPMslut-nAQRstart)),rep(1,nAQRslut-nAQRCAPMslut+1))
fit<-lm(y~MKT*factor(group))
TabelAQRprepostCAPM[4,3] <- summary(fit)$coefficients[3,3] #CAPM

fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group))
TabelAQRprepostCAPM[6,3] <- summary(fit)$coefficients[3,3] #3-faktor

fit<-lm(y~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group))
TabelAQRprepostCAPM[8,3] <- summary(fit)$coefficients[3,3]#Carhart +REV



print(xtable(TabelAQRprepostCAPM, type = "latex"), file = "TabelAQRprepostCAPM.tex") #TED-tabel




############# Sammenligning med AQR datasæt PRÆ OG POST FF##########################
#Tabel Pre VS Post CAPM:

TabelAQRprepostFF <- data.frame(matrix(nrow=11,ncol=3))
row.names(TabelAQRprepostFF)[1] <- "Merafkast"
row.names(TabelAQRprepostFF)[2] <- "T-værdi"
row.names(TabelAQRprepostFF)[3] <- "CAPM alfa"
row.names(TabelAQRprepostFF)[4] <- "T-værdi "
row.names(TabelAQRprepostFF)[5] <- "3-faktor alfa"
row.names(TabelAQRprepostFF)[6] <- "T-værdi  "
row.names(TabelAQRprepostFF)[7] <- "Carhart + REV alfa"
row.names(TabelAQRprepostFF)[8] <- "T-værdi   "
row.names(TabelAQRprepostFF)[9] <- "Beta (real.)"
row.names(TabelAQRprepostFF)[10] <- "Volatilitet"
row.names(TabelAQRprepostFF)[11] <- "Sharpe Ratio"
colnames(TabelAQRprepostFF) <- c("AQR Præ FF","AQR Post FF","AQR Præ VS Post - Anders TEST")

#pre
TabelAQRprepostFF[1,1] <- mean(AQR_sorteret[nAQRstart:nAQRFFslut,2])
TabelAQRprepostFF[2,1] <- t.test(AQR_sorteret[nAQRstart:nAQRFFslut,2])[1]
tmp <- lm(AQR_sorteret[nAQRstart:nAQRFFslut,2]~FF3m[nFFstart:nFFFFslut,2])
TabelAQRprepostFF[3,1] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostFF[4,1] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[nAQRstart:nAQRFFslut,2]~FF3m[nFFstart:nFFFFslut,2]+FF3m[nFFstart:nFFFFslut,3]+FF3m[nFFstart:nFFFFslut,4])
TabelAQRprepostFF[5,1] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostFF[6,1] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[nAQRstart:nAQRFFslut,2]~FF3m[nFFstart:nFFFFslut,2]+FF3m[nFFstart:nFFFFslut,3]+FF3m[nFFstart:nFFFFslut,4]+FF3m[nFFstart:nFFFFslut,8]+FF3m[nFFstart:nFFFFslut,12]+FF3m[nFFstart:nFFFFslut,13]) 
TabelAQRprepostFF[7,1] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostFF[8,1] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostFF[9,1] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostFF[10,1] <-sd(AQR_sorteret[nAQRstart:nAQRFFslut,2])*sqrt(12)

TabelAQRprepostFF[11,1] <- mean(AQR_sorteret[nAQRstart:nAQRFFslut,2]) / sd(AQR_sorteret[nAQRstart:nAQRFFslut,2])*sqrt(12)


#post
TabelAQRprepostFF[1,2] <-mean(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2])
TabelAQRprepostFF[2,2] <- t.test(AQR_sorteret[(nAQRFFslut+1):nAQRFFslut,2])[1]
tmp <- lm(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2]~FF3m[(nFFFFslut+1):nFFslut,2])
TabelAQRprepostFF[3,2] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostFF[4,2] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2]~FF3m[(nFFFFslut+1):nFFslut,2]+FF3m[(nFFFFslut+1):nFFslut,3]+FF3m[(nFFFFslut+1):nFFslut,4])
TabelAQRprepostFF[5,2] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostFF[6,2] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2]~FF3m[(nFFFFslut+1):nFFslut,2]+FF3m[(nFFFFslut+1):nFFslut,3]+FF3m[(nFFFFslut+1):nFFslut,4]+FF3m[(nFFFFslut+1):nFFslut,8]+FF3m[(nFFFFslut+1):nFFslut,12]+FF3m[(nFFFFslut+1):nFFslut,13]) 
TabelAQRprepostFF[7,2] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostFF[8,2] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostFF[9,2] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostFF[10,2] <-sd(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2])*sqrt(12)

TabelAQRprepostFF[11,2] <-mean(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2]) / sd(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2])*sqrt(12)



#Pre og Post CAPM
TabelAQRprepostFF[2,3] <- t.test(AQR_sorteret[(nAQRFFslut+1):nAQRslut,2],AQR_sorteret[nAQRstart:nAQRFFslut,2])[1]

group<-c(rep(0,(nAQRFFslut-nAQRstart)),rep(1,nAQRslut-nAQRFFslut+1))
fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group))
TabelAQRprepostFF[4,3] <- summary(fit)$coefficients[3,3] #CAPM

fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group)+SMB*factor(group)+HML*factor(group))
TabelAQRprepostFF[6,3] <- summary(fit)$coefficients[3,3] #3-faktor


fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group))
TabelAQRprepostFF[8,3] <- summary(fit)$coefficients[3,3]#Carhart +REV



print(xtable(TabelAQRprepostFF, type = "latex"), file = "TabelAQRprepostFF.tex") #TED-tabel







############# Sammenligning med AQR datasæt PRÆ OG POST BAB #################
#Tabel Pre VS Post CAPM:

TabelAQRprepostBAB <- data.frame(matrix(nrow=11,ncol=3))
row.names(TabelAQRprepostBAB)[1] <- "Merafkast"
row.names(TabelAQRprepostBAB)[2] <- "T-værdi"
row.names(TabelAQRprepostBAB)[3] <- "CAPM alfa"
row.names(TabelAQRprepostBAB)[4] <- "T-værdi "
row.names(TabelAQRprepostBAB)[5] <- "3-faktor alfa"
row.names(TabelAQRprepostBAB)[6] <- "T-værdi  "
row.names(TabelAQRprepostBAB)[7] <- "Carhart + REV alfa"
row.names(TabelAQRprepostBAB)[8] <- "T-værdi   "
row.names(TabelAQRprepostBAB)[9] <- "Beta (real.)"
row.names(TabelAQRprepostBAB)[10] <- "Volatilitet"
row.names(TabelAQRprepostBAB)[11] <- "Sharpe Ratio"
colnames(TabelAQRprepostBAB) <- c("AQR Præ BAB","AQR Post BAB","AQR Præ VS Post - Anders TEST")


#pre
TabelAQRprepostBAB[1,1] <- mean(AQR_sorteret[nAQRstart:nAQRBABslut,2])
TabelAQRprepostBAB[2,1] <- t.test(AQR_sorteret[nAQRstart:nAQRBABslut,2])[1]
tmp <- lm(AQR_sorteret[nAQRstart:nAQRBABslut,2]~FF3m[nFFstart:nFFBABslut,2])
TabelAQRprepostBAB[3,1] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostBAB[4,1] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[nAQRstart:nAQRBABslut,2]~FF3m[nFFstart:nFFBABslut,2]+FF3m[nFFstart:nFFBABslut,3]+FF3m[nFFstart:nFFBABslut,4])
TabelAQRprepostBAB[5,1] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostBAB[6,1] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[nAQRstart:nAQRBABslut,2]~FF3m[nFFstart:nFFBABslut,2]+FF3m[nFFstart:nFFBABslut,3]+FF3m[nFFstart:nFFBABslut,4]+FF3m[nFFstart:nFFBABslut,8]+FF3m[nFFstart:nFFBABslut,12]+FF3m[nFFstart:nFFBABslut,13]) 
TabelAQRprepostBAB[7,1] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostBAB[8,1] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostBAB[9,1] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostBAB[10,1] <-sd(AQR_sorteret[nAQRstart:nAQRBABslut,2])*sqrt(12)

TabelAQRprepostBAB[11,1] <- mean(AQR_sorteret[nAQRstart:nAQRBABslut,2]) / sd(AQR_sorteret[nAQRstart:nAQRBABslut,2])*sqrt(12)


#post
TabelAQRprepostBAB[1,2] <-mean(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2])
TabelAQRprepostBAB[2,2] <- t.test(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2])[1]
tmp <- lm(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2]~FF3m[(nFFBABslut+1):nFFslut,2])
TabelAQRprepostBAB[3,2] <-summary(tmp)$coefficients[1,1]
TabelAQRprepostBAB[4,2] <-summary(tmp)$coefficients[1,3]

tmp2 <- lm(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2]~FF3m[(nFFBABslut+1):nFFslut,2]+FF3m[(nFFBABslut+1):nFFslut,3]+FF3m[(nFFBABslut+1):nFFslut,4])
TabelAQRprepostBAB[5,2] <-summary(tmp2)$coefficients[1,1]
TabelAQRprepostBAB[6,2] <-summary(tmp2)$coefficients[1,3]


tmp3 <- lm(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2]~FF3m[(nFFBABslut+1):nFFslut,2]+FF3m[(nFFBABslut+1):nFFslut,3]+FF3m[(nFFBABslut+1):nFFslut,4]+FF3m[(nFFBABslut+1):nFFslut,8]+FF3m[(nFFBABslut+1):nFFslut,12]+FF3m[(nFFBABslut+1):nFFslut,13]) 
TabelAQRprepostBAB[7,2] <-summary(tmp3)$coefficients[1,1]
TabelAQRprepostBAB[8,2] <-summary(tmp3)$coefficients[1,3]

TabelAQRprepostBAB[9,2] <-summary(tmp)$coefficients[2,1]

TabelAQRprepostBAB[10,2] <-sd(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2])*sqrt(12)

TabelAQRprepostBAB[11,2] <-mean(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2]) / sd(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2])*sqrt(12)



#Pre og Post CAPM
TabelAQRprepostBAB[2,3] <- t.test(AQR_sorteret[(nAQRBABslut+1):nAQRslut,2],AQR_sorteret[nAQRstart:nAQRBABslut,2])[1]

group<-c(rep(0,(nAQRBABslut-nAQRstart)),rep(1,nAQRslut-nAQRBABslut+1))
fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group))
TabelAQRprepostBAB[4,3] <- summary(fit)$coefficients[3,3] #CAPM

fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group)+SMB*factor(group)+HML*factor(group))
TabelAQRprepostBAB[6,3] <- summary(fit)$coefficients[3,3] #3-faktor

fit<-lm(AQR_sorteret[nAQRstart:nAQRslut,2]~MKT*factor(group)+SMB*factor(group)+HML*factor(group)+MOM*factor(group)+LTREV*factor(group)+STREV*factor(group))
TabelAQRprepostBAB[8,3] <- summary(fit)$coefficients[3,3]#Carhart +REV



print(xtable(TabelAQRprepostBAB, type = "latex"), file = "TabelAQRprepostBAB.tex") #TED-tabel























############# TEST ##################
#Tabel Pre VS Post CAPM for BAB:


############# TESTS TEST ##################


