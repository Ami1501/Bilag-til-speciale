#install.packages("xtable")
library(xtable)


rm(list = ls())
############ TOKE ############
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


############ P1-P10############
måned <- 1
while (Raw_Monthly_mer[måned,1] != Måned1) {
  måned <- måned + 1
}

market2 <- FF3m[måned:(n_Monthly-1),2:4]
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

AntalMåneder64 <- AntalMåneder - 431

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

# BAB Sharpe
BABvol <- sd(BABpf[,3])*sqrt(12)
BABSharpe <- 12*BABReturn / BABvol

# cum BAB
for (i in 2:AntalMåneder) {
  BABpf[i,4] <- BABpf[i-1,4] + BABpf[i,3]
}












###############################
### TED spread TIIIIIIIIIID ###
###############################


row.names(BABpf)[684]
nTED <- AntalMåneder - 683

BABdataframe <- data.frame(matrix(nrow=nrow(TEDTemp),ncol=1))





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

### Create n portfolios from low to high TED volatility. ###
# Creating one single portfolio, sorted, would have been faster,
# but this approach makes it easier to treat each case separately as well
#n <- 3 # Number of TED-sorted-pfs
#TEDpf <- data.frame(matrix(data=0, nrow=nMåneder,ncol=4*n))

# First index to grab from BABpf (we use lagged TED volatility
# so first month to fetch BAB return for is '198602')
#firstBAB <- 684
#TEDlength <- nMåneder # How much TED data to use (while there's still corresponding BAB returns)
#TEDorder <- order(TEDstat[1:TEDlength,1])
#TEDorder <- order(TEDstat[1:TEDlength,1]/TEDstat[1:TEDlength,2]) # Alternative sorting
#BABbyTED <- BABpf[firstBAB-1+TEDorder,3] # Sort relevant BAB returns
#MKTbyTED <- market2[firstBAB-1+TEDorder,1:3] # Sort rel. exc. MKT returns



#firstBAB <- 684
#TEDlength <- nMåneder 
# Split above data in n separate portfolios
#for (i in 1:n) {
  # Pf i shall consist of  indices "leftIndex" to "rightIndex" of the sorted BAB returns
 # leftIndex <- floor(TEDlength/n*(i-1)) + 1
 # rightIndex <- floor(TEDlength/n*i)
 # colnames(TEDpf)[4*i-3] <- paste('PF',i,sep='')
#  colnames(TEDpf)[4*i-2] <- 'MKT-RF'
 # colnames(TEDpf)[4*i-1] <- 'SMB'
 # colnames(TEDpf)[4*i] <- 'HML'
 # TEDpf[1:(1+rightIndex-leftIndex),4*i-3] <-
 #   BABbyTED[leftIndex:rightIndex] # Add quantile of sorted BAB returns
 # TEDpf[1:(1+rightIndex-leftIndex),(4*i-2):(4*i)] <-
 #   MKTbyTED[leftIndex:rightIndex,1:3] # Add MKT+SMB+HML corresponding to BAB returns
#}

# Relevant data (some scaled to compare with Frazzini+Pedersen)

BABforReg <- BABpf[683:1102,3] #return
laggedTED <- TEDstat[1:420,2] * 100 # To bps
changeInTED <- TEDstat[1:420,3] * 100 # To bps
betaSpread <- BABpf[683:1102,7] * 100 # Not bp, just scaled up
marketForReg <- FF3m[(683+måned+1):n_Monthly,2]



row.names(BABpf)[1102]
row.names(TEDstat)[1]

row.names(TEDstat)[420]
# Plot af TED
plot(ts(TEDstat[1:420,2]*100,start= 1986, end = 2020, frequency = 12),type = 'l', xlab= "Tid", ylab="TED spread i bps")

plot(laggedTED,type='l')
TEDvolBPs <- TEDstat[1:TEDlength,1] * 100
plot(TEDvolBPs,type='l')

# Test of Prop 3
model <- lm(BABforReg ~ laggedTED + changeInTED)
summary(model)
model <- lm(BABforReg ~ laggedTED + changeInTED +
              marketForReg + betaSpread)
summary(model)

# Common model with different beta for each TED volatility domain
MKT1 <- c(TEDpf[1:115,2],rep(0,2*116))
MKT2 <- c(rep(0,115),TEDpf[1:116,6],rep(0,116))
MKT3 <- c(rep(0,115+116),TEDpf[1:116,10])
MKT31 <- c(TEDpf[1:115,2],rep(0,116),TEDpf[1:116,10])
model <- lm(BABbyTED ~ MKT1 + MKT2 + MKT3 + MKTbyTED[,2] + MKTbyTED[,3])
summary(model)


#############################
### Simulating investment ###
#############################
# Low beta, mid beta, high beta, market
# Starting in September 1964 (month 428)
comparison <- data.frame(matrix(data=0, nrow=numberOfMonths-428,ncol=3))


### Simulating low, mid, high beta (and market) investing to create figure ###
# Low beta stocks init.
comparison[1,1] <- 1
exLow <- Ppf[428:1031,1]
lowBeta <- coef(lm(exLow ~ market2[428:1031,1]))[2]
exLow <- exLow / lowBeta # Gearing
# Mid beta stocks init.
comparison[1,2] <- 1
exMid <- Ppf[428:1031,5]
midBeta <- coef(lm(exMid ~ market2[428:1031,1]))[2]
exMid <- exMid / midBeta # Gearing
# High beta stocks init.
comparison[1,3] <- 1
exHigh <- Ppf[428:1031,10]
highBeta <- coef(lm(exHigh ~ market2[428:1031,1]))[2]
exHigh <- exHigh / highBeta # Gearing
# Market init.
comparison[1,4] <- 1
exMarket <- market2[428:1031,1]

riskFree <- market2[428:1031,4]

for (i in 2:604) {
  comparison[i,1] <- comparison[i-1,1]*(1+(exLow[i]+riskFree[i])/100)
  comparison[i,2] <- comparison[i-1,2]*(1+(exMid[i]+riskFree[i])/100)
  comparison[i,3] <- comparison[i-1,3]*(1+(exHigh[i]+riskFree[i])/100)
  comparison[i,4] <- comparison[i-1,4]*(1+(exMarket[i]+riskFree[i])/100)
}
plot(comparison[,1], type='l')
lines(comparison[,2])
lines(comparison[,3])
lines(comparison[,4])
