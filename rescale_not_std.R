library(dplyr)
library(psych)
file1 <- read.csv('~/RSAKehl2020/051220_undergraduatesample.csv')
file1 <- read.csv('~/RSAKehl2020/IPCx3 items Kehl.csv')

reCSIE <- function(x){

	x <- x/(2.2)
#	x <- scale(x)
	x

}

reCSIV <- function(x){

	
#	x <- scale(x)
	x
}

CSIE <- file1 %>% select(CSIE_PA, CSIE_BC, CSIE_DE, CSIE_FG, CSIE_HI, CSIE_JK, CSIE_LM, CSIE_NO)
CSIV <- file1 %>% select(CSIV_PA, CSIV_BC, CSIV_DE, CSIV_FG, CSIV_HI, CSIV_JK, CSIV_LM, CSIV_NO)
IIPSC <- file1 %>% select(IIPSC_PA, IIPSC_BC, IIPSC_DE, IIPSC_FG, IIPSC_HI,IIPSC_JK, IIPSC_LM, IIPSC_NO)
IIPSC <- file1 %>% select(iipscpa, iipscbc, iipscde, iipscfg, iipschi,iipscjk,iipsclm, iipscno)
colnames(IIPSC) <- c('IIPSC_PA', 'IIPSC_BC', 'IIPSC_DE', 'IIPSC_FG', 'IIPSC_HI','IIPSC_JK', 'IIPSC_LM', 'IIPSC_NO')

#CSIE <- CSIE %>% mutate_all(reCSIE)
CSIE <- CSIE %>% mutate_all(scale)
CSIV <- CSIV %>% mutate_all(scale)
#for reliabilities
IIPSC <- IIPSC%>% mutate_all(scale)

#checks out on hopwood
CSIV['CSIV_PA'] <- (CSIV$CSIV_PA-2.53)/.63
CSIV['CSIV_BC'] <- (CSIV$CSIV_BC-1.38)/.71
CSIV['CSIV_DE']<-(CSIV$CSIV_DE-1.10)/.70
CSIV['CSIV_FG'] <-(CSIV$CSIV_FG-1.66)/.78
CSIV['CSIV_HI'] <-(CSIV$CSIV_HI-1.77)/.75
CSIV['CSIV_JK'] <-(CSIV$CSIV_JK-2.676)/.71
CSIV['CSIV_LM'] <- (CSIV$CSIV_LM-2.83)/.69
CSIV['CSIV_NO'] <-(CSIV$CSIV_NO-2.93)/.57




dom_csie <- 0.25*((CSIE$CSIE_PA - CSIE$CSIE_HI) + (0.707*(CSIE$CSIE_BC +  CSIE$CSIE_NO -  CSIE$CSIE_FG -  CSIE$CSIE_JK)))
warm_csie <- 0.25*((CSIE$CSIE_LM - CSIE$CSIE_DE) + (0.707*(CSIE$CSIE_JK + CSIE$CSIE_NO - CSIE$CSIE_FG - CSIE$CSIE_BC)))


dom_csiv <- 0.25*((CSIV$CSIV_PA - CSIV$CSIV_HI) + (0.707*(CSIV$CSIV_BC + CSIV$CSIV_NO - CSIV$CSIV_FG - CSIV$CSIV_JK)))
warm_csiv <- 0.25*((CSIV$CSIV_LM - CSIV$CSIV_DE) + (0.707*(CSIV$CSIV_JK + CSIV$CSIV_NO - CSIV$CSIV_FG - CSIV$CSIV_BC)))

elev_iip <- rowSums(IIPSC)*0.125
dom_iip <-0.25*((IIPSC$IIPSC_PA - IIPSC$IIPSC_HI) + (0.707*(IIPSC$IIPSC_BC + IIPSC$IIPSC_NO - IIPSC$IIPSC_FG - IIPSC$IIPSC_JK)))
warm_iip <-0.25*((IIPSC$IIPSC_LM - IIPSC$IIPSC_DE) + (0.707*(IIPSC$IIPSC_JK + IIPSC$IIPSC_NO - IIPSC$IIPSC_FG - IIPSC$IIPSC_BC)))

mean(warm2, na.rm = T)

