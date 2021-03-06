library(psych)
library(dplyr)

file1 <- read.csv('/Users/madke/downloads/MadelinesThesis_ForReliabilities.csv')

#reading these in with just octant labels to make bottom code more reusable
file_df <- data.frame(file1)
#CSIV
PA <- file_df %>% dplyr::select('CSIV01','CSIV09','CSIV17','CSIV25','CSIV33','CSIV41','CSIV49','CSIV57')
BC <- file_df %>% dplyr::select('CSIV04','CSIV12','CSIV20','CSIV28','CSIV36','CSIV44','CSIV52','CSIV60')
DE <- file_df %>% dplyr::select('CSIV07','CSIV15','CSIV23','CSIV31','CSIV39','CSIV47','CSIV55','CSIV63')
FG <- file_df %>% dplyr::select('CSIV02','CSIV10','CSIV18','CSIV26','CSIV34','CSIV42','CSIV50','CSIV58')
HI <- file_df %>% dplyr::select('CSIV05','CSIV13','CSIV21','CSIV29','CSIV37','CSIV45','CSIV53','CSIV61')
JK <- file_df %>% dplyr::select('CSIV08','CSIV16','CSIV24','CSIV32','CSIV40','CSIV48','CSIV56','CSIV64')
LM <- file_df %>% dplyr::select('CSIV03','CSIV11','CSIV19','CSIV27','CSIV35','CSIV43','CSIV51','CSIV59')
NO <- file_df %>% dplyr::select('CSIV06','CSIV14','CSIV22','CSIV30','CSIV38','CSIV46','CSIV54','CSIV62')


#CSIE
PA <- file_df %>% dplyr::select('CSIE04','CSIE12','CSIE20','CSIE28')
BC <- file_df %>% dplyr::select('CSIE07','CSIE15','CSIE23','CSIE31')
DE <- file_df %>% dplyr::select('CSIE02','CSIE10','CSIE18','CSIE26')
FG <- file_df %>% dplyr::select('CSIE05','CSIE13','CSIE21','CSIE29')
HI <- file_df %>% dplyr::select('CSIE08','CSIE16','CSIE24','CSIE32')
JK <- file_df %>% dplyr::select('CSIE03','CSIE11','CSIE19','CSIE27')
LM <- file_df %>% dplyr::select('CSIE06','CSIE14','CSIE22','CSIE30')
NO <- file_df %>% dplyr::select('CSIE01','CSIE09','CSIE17','CSIE25')

#IIPSC
PA <- file_df %>% dplyr::select('IIP01','IIP09','IIP17','IIP25')
BC <- file_df %>% dplyr::select('IIP02','IIP10','IIP18','IIP26')
DE <- file_df %>% dplyr::select('IIP03','IIP11','IIP19','IIP27')
FG <- file_df %>% dplyr::select('IIP04','IIP12','IIP20','IIP28')
HI <- file_df %>% dplyr::select('IIP05','IIP13','IIP21','IIP29')
JK <- file_df %>% dplyr::select('IIP06','IIP14','IIP22','IIP30')
LM <- file_df %>% dplyr::select('IIP07','IIP15','IIP23','IIP31')
NO <- file_df %>% dplyr::select('IIP08','IIP16','IIP24','IIP32')


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

#calculate dom, warm axes for unweighted var
dom <- ((circum$PA - circum$HI) + (0.707*(circum$BC + circum$NO - circum$FG - circum$JK)))
warm <-((circum$LM - circum$DE) + (0.707*(circum$JK + circum$NO - circum$FG - circum$BC)))

#calculate reliabilities on subscale level
#rel_axis = 1 - ((sigma(w**2) - sigma(w**2*octants))/var(axis))

#dom
d_sigwrelsc = om_PA$omega.tot*((1)**2) + om_HI$omega.tot*((1)**2) + om_BC$omega.tot*((0.707)**2) + 
om_NO$omega.tot*((0.707)**2) + om_FG$omega.tot*((-0.707)**2)+ om_JK$omega.tot*((-0.707)**2)
d_var_axis = (var(dom, na.rm = T))
d_rel_axis = 1 - ((4 - d_sigwrelsc) /d_var_axis)

#warm
w_sigwrelsc = (om_LM$omega.tot*1*1) + (om_DE$omega.tot*-1*-1) + (om_JK$omega.tot*0.707*0.707) + (om_NO$omega.tot*0.707*0.707) + (om_FG$omega.tot*-0.707*-0.707)+ (om_BC$omega.tot*-0.707*-0.707)
w_var_axis = (var(warm, na.rm = T))
w_rel_axis = 1 - ((4 - w_sigwrelsc) /w_var_axis)

#elev

elev = (om_PA$omega.tot + om_BC$omega.tot + om_DE$omega.tot + om_FG$omega.tot + om_HI$omega.tot + om_JK$omega.tot + om_LM$omega.tot + om_NO$omega.tot)/8

