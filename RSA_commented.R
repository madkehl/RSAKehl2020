#-This code takes in our undergraduate sample and/or our community sample
#and allows for running our set of analyses(primary, exploratory I
#exploratory II) on one sample at a time.
#-It gives the option to scale and winsorize the data, as included in our 
#supplementary table
#Note: even data that is not scaled + winsorized is centered in the RSA syntax  
models = c("absdiff","absunc","diff","mean","additive","IA","SQD","RR","SRR","SRRR","SSQD")


#MODULE IMPORTS
library(RSA)
library(dplyr)

#undergraduate sample, subsetting relevant variables
undergrad <- read.csv('/Users/madke/documents/051220_undergraduatesample.csv')
undergrad <- undergrad %>% select(IIPSC_ELEV,IIPSC_DOM,IIPSC_LOV, CSIV_LOV, CSIE_LOV, CSIV_DOM, CSIE_DOM, CSIE_ELEV, CSIV_ELEV)

#community sample, subsetting relevant variables and matching var names
community <- read.csv('/Users/madke/downloads/051220_communitysample.csv')
community <- community  %>% select(IIPSC_ELEV = IIPELEV,IIPSC_DOM = IIPDOM,IIPSC_LOV= IIPLOV, CSIV_LOV =CSIVLOV, CSIE_LOV, CSIV_DOM = CSIVDOM,CSIE_DOM, CSIE_ELEV, CSIV_ELEV = CSIVELEV)

#Pick which dataset to use for the models
file_df1 <- community
file_df1 <- undergrad

file_df1 <- cbind(file_df1, dom_csie, warm_csie, dom_csiv, warm_csiv, dom_iip, warm_iip, elev_iip)

#FOR SUPPLEMENTAL TABLE:
#this function takes SCALED data and brings outliers in to being within three 
#standard deviations of the mean.  

winsorize <- function(x){
    x[ x < -3 ] <- -3
    x[ x > 3 ] <- 3
    x
}

file_df1 <- file_df1 %>% mutate_all(scale)
file_df1 <- file_df1 %>% mutate_all(winsorize)

#RSA MODELS
#Note: by default, the RSA package interprets: 
#'z ~ x + y' as:
#'z ~ x + x*x + y + y*y + x*y'

#PRIMARY ANALYSES
primary_w <- RSA(IIPSC_ELEV ~ CSIV_LOV + CSIE_LOV, file_df1, center = TRUE)
primary_d <- RSA(IIPSC_ELEV ~ CSIV_DOM + CSIE_DOM, file_df1, center = TRUE)

#EXPLORATORY ANALYSES I
explor_1_w <- RSA(IIPSC_LOV ~ CSIV_LOV + CSIE_LOV, file_df1, center = TRUE)
explor_1_d <- RSA(IIPSC_DOM ~ CSIV_DOM + CSIE_DOM, file_df1, center = TRUE)

#EXPLORATORY ANALYSES II
explor_2_w <- RSA(IIPSC_LOV ~  CSIV_ELEV + CSIE_ELEV, file_df1, center = TRUE)
explor_2_d <- RSA(IIPSC_DOM ~ CSIV_ELEV + CSIE_ELEV, file_df1, center = TRUE)
explor_2_e <- RSA(IIPSC_ELEV~ CSIV_ELEV+ CSIE_ELEV, file_df1, center = TRUE)

explor_2_w <- RSA(IIPSC_LOV ~  CSIV_ELEV + CSIE_ELEV, file_df1, center = F)
explor_2_d <- RSA(IIPSC_DOM ~ CSIV_ELEV + CSIE_ELEV, file_df1, center = F)
explor_2_e <- RSA(IIPSC_ELEV~ CSIV_ELEV+ CSIE_ELEV, file_df1, center = F)


#PRIMARY ANALYSES
primary_w <- RSA(elev_iip ~ warm_csiv + warm_csie, file_df1, models = models, center = TRUE)
primary_d <- RSA(elev_iip ~ dom_csiv + dom_csie, file_df1, center = TRUE)

#EXPLORATORY ANALYSES I
explor_1_w <- RSA(warm_iip ~ warm_csiv + warm_csie,  file_df1, models = models, center = TRUE)
explor_1_d <- RSA(dom_iip ~  dom_csiv + dom_csie, center = TRUE)


#TO PRINT RESULTS FOR ANY MODEL
summary(primary_d)
plot(primary_d)


