library(psych)
library(dplyr) 

file <- read.csv('/Users/madke/downloads/IPCx3 items Kehl.csv')

file_df <- data.frame(file)

#csiv
PA <- file_df %>% dplyr::select('csiv01','csiv09','csiv17','csiv25','csiv33','csiv41','csiv49','csiv57')
BC <- file_df %>% dplyr::select('csiv04','csiv12','csiv20','csiv28','csiv36','csiv44','csiv52','csiv60')
DE <- file_df %>% dplyr::select('csiv07','csiv15','csiv23','csiv31','csiv39','csiv47','csiv55','csiv63')
FG <- file_df %>% dplyr::select('csiv02','csiv10','csiv18','csiv26','csiv34','csiv42','csiv50','csiv58')
HI <- file_df %>% dplyr::select('csiv05','csiv13','csiv21','csiv29','csiv37','csiv45','csiv53','csiv61')
JK <- file_df %>% dplyr::select('csiv08','csiv16','csiv24','csiv32','csiv40','csiv48','csiv56','csiv64')
LM <- file_df %>% dplyr::select('csiv03','csiv11','csiv19','csiv27','csiv35','csiv43','csiv51','csiv59')
NO <- file_df %>% dplyr::select('csiv06','csiv14','csiv22','csiv30','csiv38','csiv46','csiv54','csiv62')


#csie
PA <- file_df %>% dplyr::select('csie04','csie12','csie20','csie28')
BC <- file_df %>% dplyr::select('csie07','csie15','csie23','csie31')
DE <- file_df %>% dplyr::select('csie02','csie10','csie18','csie26')
FG <- file_df %>% dplyr::select('csie05','csie13','csie21','csie29')
HI <- file_df %>% dplyr::select('csie08','csie16','csie24','csie32')
JK <- file_df %>% dplyr::select('csie03','csie11','csie19','csie27')
LM <- file_df %>% dplyr::select('csie06','csie14','csie22','csie30')
NO <- file_df %>% dplyr::select('csie01','csie09','csie17','csie25')

#iipscSC
PA <- file_df %>% dplyr::select('iipsc01','iipsc09','iipsc17','iipsc25')
BC <- file_df %>% dplyr::select('iipsc02','iipsc10','iipsc18','iipsc26')
DE <- file_df %>% dplyr::select('iipsc03','iipsc11','iipsc19','iipsc27')
FG <- file_df %>% dplyr::select('iipsc04','iipsc12','iipsc20','iipsc28')
HI <- file_df %>% dplyr::select('iipsc05','iipsc13','iipsc21','iipsc29')
JK <- file_df %>% dplyr::select('iipsc06','iipsc14','iipsc22','iipsc30')
LM <- file_df %>% dplyr::select('iipsc07','iipsc15','iipsc23','iipsc31')
NO <- file_df %>% dplyr::select('iipsc08','iipsc16','iipsc24','iipsc32')


#get omegas
om_PA = omega(PA, nfactors = 1)
om_BC = omega(BC, nfactors = 1)
om_DE = omega(DE, nfactors = 1)
om_FG = omega(FG, nfactors = 1)
om_HI = omega(HI, nfactors = 1)
om_JK = omega(JK, nfactors = 1)
om_LM = omega(LM, nfactors = 1)
om_NO = omega(NO, nfactors = 1)

#iip (scored with sums)
circum <- cbind(rowSums(PA), rowSums(BC), rowSums(DE), rowSums(FG), rowSums(HI), rowSums(JK), rowSums(LM), rowSums(NO))
#csiv/e (scored with means)
circum <- cbind(rowMeans(PA), rowMeans(BC), rowMeans(DE), rowMeans(FG), rowMeans(HI), rowMeans(JK), rowMeans(LM), rowMeans(NO))

#zscore all columns
circum <- data.frame(circum) %>% mutate_all(scale)
#rename
colnames(circum) <- c('PA','BC', 'DE', 'FG', 'HI', 'JK', 'LM', 'NO')

#calculate dom, warm axes
dom <-((circum$PA - circum$HI) + (0.707*(circum$BC + circum$NO - circum$FG - circum$JK)))
warm <-((circum$LM - circum$DE) + (0.707*(circum$JK + circum$NO - circum$FG - circum$BC)))

#calculate reliabilities on subscale level
#rel_axis = 1 - ((sigma(w**2) - sigma(w**2*octants))/var(axis))
d_sigwrelsc = (om_PA$omega.tot*1*1) + (om_HI$omega.tot*-1*-1) + (om_BC$omega.tot*0.707*0.707) + (om_NO$omega.tot*0.707*0.707) + (om_FG$omega.tot*-0.707*-0.707)+ (om_JK$omega.tot*-0.707*-0.707)
d_var_axis = (var(dom, na.rm = T))
d_rel_axis = 1 - ((4 - d_sigwrelsc) /d_var_axis)

#elev

elev = (om_PA$omega.tot + om_BC$omega.tot + om_DE$omega.tot + om_FG$omega.tot + om_HI$omega.tot + om_JK$omega.tot + om_LM$omega.tot + om_NO$omega.tot)/8


#warmth
w_sigwrelsc = (om_LM$omega.tot*1*1) + (om_DE$omega.tot*-1*-1) + (om_JK$omega.tot*0.707*0.707) + (om_NO$omega.tot*0.707*0.707) + (om_FG$omega.tot*-0.707*-0.707)+ (om_BC$omega.tot*-0.707*-0.707)
w_var_axis = (var(warm, na.rm = T))
w_rel_axis = 1 - ((4 - w_sigwrelsc) /w_var_axis)




describe(file$IIPELEV)
describe(file$IIPLOV)
describe(file$IIPDOM)
