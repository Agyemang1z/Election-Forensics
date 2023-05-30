library(readxl)
library(diptest)
library(moments)

par(mfrow=c(1,2))

## NPP ASHANTI 2020
ASHANTI2020<-read_excel("C:UsersEDMONDO EL FREDONDODropboxMy PC (EDMONDO-el-FREDONDO)DesktopEDMONDO EL FREDONDOACADEMIAPROJECTMPHIL_PHD THESISMO SALAH MPHIL FINAL PROJECTProject Data20202020ASHANTI.xlsx")
View(ASHANTI2020)
ASHANTI2020dataframe<-data.frame(ASHANTI2020)
ASHANTI2020data<-ASHANTI2020dataframe[1:47,c(4:6)]
ASHANTI2020NPP<-dip.test(ASHANTI2020data$NPP,simulate.p.value = TRUE,B = 2000)
plot(density(ASHANTI2020data$NPP),col="blue");rug(ASHANTI2020data$NPP,col="blue")
mean(ASHANTI2020data$NPP)
median(ASHANTI2020data$NPP)
skewness(ASHANTI2020data$NPP)
kurtosis(ASHANTI2020data$NPP)
(dSNPP<-dip(ASHANTI2020data$NPP, full = TRUE, debug = TRUE))
plot(dSNPP)
rug(ASHANTI2020data$NPP, col="midnight blue"); abline(h=0, col="gray")
(dS2NPP<-dip(ASHANTI2020data$NPP, full = "all", debug = 3))
plot(dS2NPP)

## NDC ASHANTI 2020
ASHANTI2020dataframe<-data.frame(ASHANTI2020)
ASHANTI2020data<-ASHANTI2020dataframe[1:47,c(4,5,6)]
ASHANTI2020NDC<-dip.test(ASHANTI2020data$NDC,simulate.p.value = TRUE,B = 2000)
plot(density(ASHANTI2020data$NDC),col="green");rug(ASHANTI2020data$NDC,col="green")
mean(ASHANTI2020data$NDC)
median(ASHANTI2020data$NDC)
skewness(ASHANTI2020data$NDC)
kurtosis(ASHANTI2020data$NDC)
(dSNDC<-dip(ASHANTI2020data$NDC, full = TRUE, debug = TRUE))
plot(dSNDC)
rug(ASHANTI2020data$NDC, col="green"); abline(h=0, col="gray")
(dS2NDC<-dip(ASHANTI2020data$NDC, full = "all", debug = 3))
plot(dS2NDC)

## OTHER ASHANTI 2020
ASHANTI2020dataframe<-data.frame(ASHANTI2020)
ASHANTI2020data<-ASHANTI2020dataframe[1:47,c(4,5,6)]
ASHANTI2020Other<-dip.test(ASHANTI2020data$Other,simulate.p.value = TRUE,B = 2000)
plot(density(ASHANTI2020data$Other),col="red");rug(ASHANTI2020data$Other,col="red")
mean(ASHANTI2020data$Other)
median(ASHANTI2020data$Other)
skewness(ASHANTI2020data$Other)
kurtosis(ASHANTI2020data$Other)
(dSOther<-dip(ASHANTI2020data$Other, full = TRUE, debug = TRUE))
plot(dSOther)
rug(ASHANTI2020data$Other, col="red"); abline(h=0, col="gray")
(dS2Other<-dip(ASHANTI2020data$Other, full = "all", debug = 3))
plot(dS2Other)


## NPP VOLTA 2020
VOLTA2020<-read_excel("C:UsersEDMONDO EL FREDONDODropboxMy PC (EDMONDO-el-FREDONDO)DesktopEDMONDO EL FREDONDOACADEMIAPROJECTMPHIL_PHD THESISMO SALAH MPHIL FINAL PROJECTProject Data20202020VOLTA.xlsx")
view(VOLTA2020)
VOLTA2020dataframe<-data.frame(VOLTA2020)
VOLTA2020data<-VOLTA2020dataframe[1:18,c(4,5,6)]
VOLTA2020NPP<-dip.test(VOLTA2020data$NPP,simulate.p.value = TRUE,B = 2000)

plot(density(VOLTA2020data$NPP),col="blue");rug(VOLTA2020data$NPP,col="blue")
mean(VOLTA2020data$NPP)
median(VOLTA2020data$NPP)
skewness(VOLTA2020data$NPP)
kurtosis(VOLTA2020data$NPP)
(dSNPP<-dip(VOLTA2020data$NPP, full = TRUE, debug = TRUE))
plot(dSNPP)
rug(VOLTA2020data$NPP, col="midnight blue"); abline(h=0, col="gray")
(dS2NPP<-dip(VOLTA2020data$NPP, full = "all", debug = 3))
plot(dS2NPP)

## NDC VOLTA 2020
VOLTA2020data<-VOLTA2020dataframe[1:18,c(4,5,6)]
VOLTA2020NDC<-dip.test(VOLTA2020data$NDC,simulate.p.value = TRUE,B = 2000)

plot(density(VOLTA2020data$NDC),col="green");rug(VOLTA2016data$NDC,col="green")
mean(VOLTA2020data$NDC)
mean(VOLTA2020data$NDC)
median(VOLTA2020data$NDC)
skewness(VOLTA2020data$NDC)
kurtosis(VOLTA2020data$NDC)
(dSNDC<-dip(VOLTA2020data$NDC, full = TRUE, debug = TRUE))
plot(dSNDC)
rug(VOLTA2020data$NDC, col="midnight green"); abline(h=0, col="gray")
(dS2NDC<-dip(VOLTA2020data$NDC, full = "all", debug = 3))
plot(dS2NDC)


## OTHER VOLTA 2020
VOLTA2020data<-VOLTA2020dataframe[1:26,c(4,5,6)]
VOLTA2020Other<-dip.test(VOLTA2020data$Other,simulate.p.value = TRUE,B = 2000)

plot(density(VOLTA2020data$Other),col="red");rug(VOLTA2020data$Other,col="red")
mean(VOLTA2020data$Other)
median(VOLTA2020data$Other)
skewness(VOLTA2020data$Other)
kurtosis(VOLTA2020data$Other)
(dSOther<-dip(VOLTA2020data$Other, full = TRUE, debug = TRUE))
plot(dSOther)
rug(VOLTA2020data$Other, col="midnight red"); abline(h=0, col="gray")
(dS2Other<-dip(VOLTA2020data$Other, full = "all", debug = 3))
plot(dS2Other)

### SWING REGIONS
## NPP GREATER ACCRA 2020
GAR2020<-read_excel("C:UsersEDMONDO EL FREDONDODropboxMy PC (EDMONDO-el-FREDONDO)DesktopEDMONDO EL FREDONDOACADEMIAPROJECTMPHIL_PHD THESISMO SALAH MPHIL FINAL PROJECTProject Data20202020GAR.xlsx")
view(GAR2020)
GAR2020dataframe<-data.frame(GAR2020)
GAR2020data<-GAR2020dataframe[1:34,c(4:6)]
GAR2016NPP<-dip.test(GAR2020data$NPP,simulate.p.value = TRUE,B = 2000)

plot(density(GAR2020data$NPP),col="blue");rug(GAR2020data$NPP,col="blue")
mean(GAR2020data$NPP)
median(GAR2020data$NPP)
skewness(GAR2020data$NPP)
kurtosis(GAR2020data$NPP)
(dSNPP<-dip(GAR2020data$NPP, full = TRUE, debug = TRUE))
plot(dSNPP)
rug(GAR2020data$NPP, col="midnight blue"); abline(h=0, col="gray")
(dS3NPP<-dip(GAR2020data$NPP, full = "all", debug = 3))
plot(dS3NPP)

## NDC GREATER ACCRA 2020
GAR2020NDC<-dip.test(GAR2020data$NDC,simulate.p.value = TRUE,B = 2000)
plot(density(GAR20120data$NDC),col="green");rug(GAR2020data$NDC,col="green")
mean(GAR2020data$NDC)
median(GAR2020data$NDC)
skewness(GAR2020data$NDC)
kurtosis(GAR2020data$NDC)
(dSNDC<-dip(GAR2020data$NDC, full = TRUE, debug = TRUE))
plot(dSNDC)
rug(GAR2020data$NDC, col="midnight green"); abline(h=0, col="gray")
(dS3NDC<-dip(GAR2020data$NDC, full = "all", debug = 3))
plot(dS3NDC)


## OTHER GREATER ACCRA 2020
GAR2020Other<-dip.test(GAR2020data$Other,simulate.p.value = TRUE,B = 2000)
plot(density(GAR2020data$Other),col="red");rug(GAR2016data$Other,col="red")
mean(GAR2020data$Other)
median(GAR2020data$Other)
skewness(GAR2020data$Other)
kurtosis(GAR2020data$Other)
(dSOther<-dip(GAR2020data$Other, full = TRUE, debug = TRUE))
plot(dSOther)
rug(GAR2020data$Other, col="midnight red"); abline(h=0, col="gray")
(dS3Other<-dip(GAR2020data$Other, full = "all", debug = 3))
plot(dS3Other)

## NPP CENTRAL 2020
CENTRAL2020<-read_excel("C:UsersEDMONDO EL FREDONDODropboxMy PC (EDMONDO-el-FREDONDO)DesktopEDMONDO EL FREDONDOACADEMIAPROJECTMPHIL_PHD THESISMO SALAH MPHIL FINAL PROJECTProject Data20202020CENTRAL.xlsx")
view(CENTRAL2020)
CENTRAL2020dataframe<-data.frame(CENTRAL2020)
CENTRAL2020data<-CENTRAL2020dataframe[1:23,c(4:6)]
CENTRAL2020NPP<-dip.test(CENTRAL2020data$NPP,simulate.p.value = TRUE,B = 2000)

plot(density(CENTRAL2020data$NPP),col="blue");rug(CENTRAL2020data$NPP,col="blue")
mean(CENTRAL2020data$NPP)
median(CENTRAL2020data$NPP)
skewness(CENTRAL2020data$NPP)
kurtosis(CENTRAL2020data$NPP)
(dSNPP<-dip(CENTRAL2020data$NPP, full = TRUE, debug = TRUE))
plot(dSNPP)
rug(CENTRAL2020data$NPP, col="midnight blue"); abline(h=0, col="gray")
(dS4NPP<-dip(CENTRAL2020data$NPP, full = "all", debug = 3))
plot(dS4NPP)

## NDC CENTRAL 2020
CENTRAL2020NDC<-dip.test(CENTRAL2020data$NDC,simulate.p.value = TRUE,B = 2000)

plot(density(CENTRAL2020data$NDC),col="green");rug(CENTRAL2020data$NDC,col="green")
mean(CENTRAL2020data$NDC)
median(CENTRAL2020data$NDC)
skewness(CENTRAL2020data$NDC)
kurtosis(CENTRAL2020data$NDC)
(dSNDC<-dip(CENTRAL2020data$NDC, full = TRUE, debug = TRUE))
plot(dSNDC)
rug(CENTRAL2020data$NDC, col="midnight green"); abline(h=0, col="gray")
(dS4NDC<-dip(CENTRAL2020data$NDC, full = "all", debug = 3))
plot(dS4NDC)


## OTHER CENTRAL 2020
CENTRAL2020Other<-dip.test(CENTRAL2020data$Other,simulate.p.value = TRUE,B = 2000)

plot(density(CENTRAL2020data$Other),col="red");rug(CENTRAL2020data$Other,col="red")
mean(CENTRAL2020data$Other)
median(CENTRAL2020data$Other)
skewness(CENTRAL2020data$Other)
kurtosis(CENTRAL2020data$Other)
(dSOther<-dip(CENTRAL2020data$Other, full = TRUE, debug = TRUE))
plot(dSOther)
rug(CENTRAL2020data$Other, col="midnight red"); abline(h=0, col="gray")
(dS4Other<-dip(CENTRAL2020data$Other, full = "all", debug = 3))
plot(dS4Other)


par(mfrow=c(1,3))

## NPP WESTERN 2020
WESTERN2020<-read_excel("C:UsersEDMONDO EL FREDONDODropboxMy PC (EDMONDO-el-FREDONDO)DesktopEDMONDO EL FREDONDOACADEMIAPROJECTMPHIL_PHD THESISMO SALAH MPHIL FINAL PROJECTProject Data20202020WESTERN.xlsx")
View(WESTERN2020)
WESTERN2020dataframe<-data.frame(WESTERN2020)
WESTERN2020data<-WESTERN2020dataframe[1:17,c(4:6)]
WESTERN2020NPP<-dip.test(WESTERN2020data$NPP,simulate.p.value = TRUE,B = 2000)

plot(density(WESTERN2020data$NPP),col="blue");rug(WESTERN2020data$NPP,col="blue")
mean(WESTERN2020data$NPP)
median(WESTERN2020data$NPP)
skewness(WESTERN2020data$NPP)
kurtosis(WESTERN2020data$NPP)
(dSNPP<-dip(WESTERN2020data$NPP, full = TRUE, debug = TRUE))
plot(dSNPP)
rug(WESTERN2020data$NPP, col="midnight blue"); abline(h=0, col="gray")
(dS5NPP<-dip(WESTERN2020data$NPP, full = "all", debug = 3))
plot(dS5NPP)

## NDC WESTERN 2020
WESTERN2020NDC<-dip.test(WESTERN2020data$NDC,simulate.p.value = TRUE,B = 2000)

plot(density(WESTERN2020data$NDC),col="green");rug(WESTERN2020data$NDC,col="green")
mean(WESTERN2020data$NDC)
median(WESTERN2020data$NDC)
skewness(WESTERN2020data$NDC)
kurtosis(WESTERN2020data$NDC)
(dSNDC<-dip(WESTERN2020data$NDC, full = TRUE, debug = TRUE))
plot(dSNDC)
rug(WESTERN2020data$NDC, col="midnight green"); abline(h=0, col="gray")
(dS5NDC<-dip(WESTERN2020data$NDC, full = "all", debug = 3))
plot(dS5NDC)


## OTHER WESTERN 2020
WESTERN2020Other<-dip.test(WESTERN2020data$Other,simulate.p.value = TRUE,B = 2000)

plot(density(WESTERN2020data$Other),col="red");rug(WESTERN2020data$Other,col="red")

mean(WESTERN2020data$Other)
median(WESTERN2020data$Other)
skewness(WESTERN2020data$Other)
kurtosis(WESTERN2020data$Other)
(dSOther<-dip(WESTERN2020data$Other, full = TRUE, debug = TRUE))
plot(dSOther)
rug(WESTERN2020data$Other, col="midnight red"); abline(h=0, col="gray")
(dS5Other<-dip(WESTERN2020data$Other, full = "all", debug = 3))
plot(dS5Other)



### Sample code for Permutation Test 

A1 <- c(ASHANTI2020dataNPP)
A2<-c(ASHANTI2020dataNDC)
A3<-c(ASHANTI2020dataOther)
obsstat <- dip.test(A1)statistic

A1A2A3<- c(A1, A2,A3)
nperm <- 10000
simstats <- numeric(nperm)
for (i in 1:nperm) {simstats[i] <- dip.test(sample(A1A2A3, length(A1)))statistic
}
pval<- mean(abs(simstats) >= abs(obsstat)) 
cat("Observed dip Statistic:", round(obsstat, 3), "n")
cat("P-value:", round(pval, 3), "n")

group1 <- c(ASHANTI2020dataNPP)
group2<-c(ASHANTI2020dataNDC)
group3<-c(ASHANTI2020dataOther)

obsdiff <-c(mean(group1) - mean(group2), mean(group2) - mean(group3), mean(group1) - mean(group3))

permtest <-onewaytest(response ~ group, data = data.frame(group = c(rep("group1", length(group1)), rep("group2", length(group2)), rep("group3", length(group3))), response = c(group1, group2, group3)), ytrafo = function(data) {data - mean(c(group1, group2, group3))}, alternative = "two.sided", distribution = "approximate")


n1<- length(group1)
n2<- length(group2)
n3 <- length(group3)
pooledsd1 <- sqrt(((n1 - 1) * var(group1) + (n2 - 1) * var(group2)) / (n1 + n2 - 2))
pooledsd2 <- sqrt(((n2 - 1) * var(group2) + (n3 - 1) * var(group3)) / (n2 + n3 - 2))
pooledsd3 <- sqrt(((n1 - 1) * var(group1) + (n3 - 1) * var(group3)) / (n1 + n3 - 2))
cohend1 <- obsdiff[1] / pooledsd1
cohend2<- obsdiff[2] / pooledsd2
cohend3 <- obsdiff[3] / pooledsd3

print(paste("Permutation test p-value:", signif(permtestpvalue, digits = 4)))
print(paste("Cohen's d effect size for group1 vs. group2:", signif(cohend1, digits = 4))) 
