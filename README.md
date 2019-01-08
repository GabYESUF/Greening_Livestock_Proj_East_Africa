# Greening_Livestock_Proj_East_Africa
This project seeks to reduce greenhouse gas emissions in the dairy sector whilst improving productivity of smallholder farmers through the implementation of climate-smart agricultural practices in Kenya and Tanzania
#reads survey data
all.dat<-read.csv("dairy_producer_survey_7.18.csv",stringsAsFactors=FALSE)# changed the format of this line

# select subset of data for Kenyan analysis
ke.dat<-all.dat[all.dat$country == "ke",]

# dividing data file into smaller factors
dairy_sec1<-ke.dat[,c(1:42)]          # general                     # to AP  
dairy_sec2<-ke.dat[,c(43:626)]        # plots                       # to AIU
dairy_sec3<-ke.dat[,c(627:1481)]      # livelihoods and income      # to AIV to 
dairy_sec5<-ke.dat[,c(1482:1494)]     # choice profile              # BDY to BDK
dairy_sec9<-ke.dat[,c(1494:1517)]     # reasons for keeping cows    # BDY to BDK
dairy_sec6<-ke.dat[,c(1204:1281)]     # practices                   # BFJ
dairy_sec7<-ke.dat[,c(1282:1484)]     # cattle                      # BFJ
dairy_sec8.1<-ke.dat[,c(1485:1586)]   # feeding
dairy_sec8.2<-ke.dat[,c(1587:1619)]   # grazing                     # BVE to CKU       # + 407
dairy_sec8.3<-ke.dat[,c(1620:1812)]   # livestock exits
# loans
# food crops
# cash crops
# fodder crops
dairy_sec10<-ke.dat                   # selling milk
dairy_sec4c<-ke.dat[,c(2483:2554)]    # 4c crop costs             # to 
dairy_sec16<-ke.dat[,c(4322:4596)]    # 16 -- labour              # FJF

# HFIAS
dairy_sec17<-ke.dat[,c()]             # 17 -- shocks
# HFIAS
View(dairy_sec4c)

###Select subset of data for Tanzania analysis
tz.dat<-all.dat[all.dat$country == "tz",]

# dividing data file into smaller factors
dairy_sec1<-tz.dat[,c(1:42)]          # general                     # to AP  
dairy_sec2<-tz.dat[,c(43:626)]        # plots                       # to AIU
dairy_sec3<-tz.dat[,c(627:1481)]      # livelihoods and income      # to AIV to 
dairy_sec5<-tz.dat[,c(1482:1494)]     # choice profile              # BDY to BDK
dairy_sec9<-tz.dat[,c(1494:1517)]     # reasons for keeping cows    # BDY to BDK
dairy_sec6<-tz.dat[,c(1204:1281)]     # practices                   # BFJ
dairy_sec7<-tz.dat[,c(1282:1484)]     # cattle                      # BFJ
dairy_sec8.1<-tz.dat[,c(1485:1586)]   # feeding
dairy_sec8.2<-tz.dat[,c(1587:1619)]   # grazing                     # BVE to CKU       # + 407
dairy_sec8.3<-tz.dat[,c(1620:1812)]   # livestock exits
# loans
# food crops
# cash crops
# fodder crops
dairy_sec10<-tz.dat                   # selling milk
dairy_sec4c<-tz.dat[,c(2483:2554)]    # 4c crop costs             # to 
dairy_sec16<-tz.dat[,c(4322:4596)]    # 16 -- labour              # FJF

# HFIAS
dairy_sec17<-tz.dat[,c()]             # 17 -- shocks
# HFIAS
View(dairy_sec4c)


####SECTION 6##############################################################
###########################################################################

colnames(dairy_sec6)[1]<- "feed_source"
colnames(dairy_sec6)[2]<- "residues"
colnames(dairy_sec6)[3]<- "grow_fodder"
colnames(dairy_sec6)[4]<- "stall_feed"
colnames(dairy_sec6)[5]<- "hours_graz"
colnames(dairy_sec6)[6]<- "feed_conc"
colnames(dairy_sec6)[7]<- "concentrates_quan"
colnames(dairy_sec6)[8]<- "mineral_licks"
colnames(dairy_sec6)[9]<- "conserve_feed"
colnames(dairy_sec6)[10]<- "store_feed"
colnames(dairy_sec6)[11]<- "access_water"
colnames(dairy_sec6)[12]<- "ecf_vacc"
colnames(dairy_sec6)[13]<- "fmd_vacc"
colnames(dairy_sec6)[14]<- "bq_vacc"
colnames(dairy_sec6)[15]<- "a_vacc"
colnames(dairy_sec6)[16]<- "lu_vacc"
colnames(dairy_sec6)[17]<- "deworm"
colnames(dairy_sec6)[18]<- "spray"
colnames(dairy_sec6)[19]<- "separate"
colnames(dairy_sec6)[27]<- "confined"
colnames(dairy_sec6)[28]<- "clean"
colnames(dairy_sec6)[29]<- "clean_other"
colnames(dairy_sec6)[30]<- "clean_teat"
colnames(dairy_sec6)[31]<- "collect_manu"
colnames(dairy_sec6)[32]<- "store_manu"
colnames(dairy_sec6)[33]<- "collect_urin"
colnames(dairy_sec6)[34]<- "manu_use"
colnames(dairy_sec6)[35]<- "manu_app"
colnames(dairy_sec6)[36]<- "apply_manu"
colnames(dairy_sec6)[37]<- "biodigest_w"
colnames(dairy_sec6)[38]<- "use_biogas"
colnames(dairy_sec6)[39]<- "use_bio_slu"

###
sub_sec6 <- dairy_sec6[c(1:1051), c(1:19,27:39)]###for Kenyan selection
sub_sec6 <- dairy_sec6[c(1:1199), c(1:19,27:39)]###for tanzania selection
##SECTION 7#####################################################################
################################################################################
colnames(dairy_sec7)[1]<-"quant_1"
colnames(dairy_sec7)[4]<-"country"
colnames(dairy_sec7)[5]<-"county_ke"
colnames(dairy_sec7)[6]<-"district_tz"
colnames(dairy_sec7)[7]<-"hhid"
colnames(dairy_sec7)[8]<-"breed_1"
colnames(dairy_sec7)[9]<-"cohort_1"
colnames(dairy_sec7)[10]<-"number_1"
colnames(dairy_sec7)[11]<-"milk_yield_1"
colnames(dairy_sec7)[17]<-"purchase_price_1"
colnames(dairy_sec7)[22]<-"breed_2"
colnames(dairy_sec7)[23]<-"cohort_2"
colnames(dairy_sec7)[24]<-"number_2"
colnames(dairy_sec7)[25]<-"milk_yield_2"
colnames(dairy_sec7)[31]<-"purchase_price_2"
colnames(dairy_sec7)[36]<-"breed_3"
colnames(dairy_sec7)[37]<-"cohort_3"
colnames(dairy_sec7)[38]<-"number_3"
colnames(dairy_sec7)[39]<-"milk_yield_3"
colnames(dairy_sec7)[43]<-"purchase_price_3"
colnames(dairy_sec7)[50]<-"breed_4"
colnames(dairy_sec7)[51]<-"cohort_4"
colnames(dairy_sec7)[52]<-"number_4"
colnames(dairy_sec7)[53]<-"milk_yield_4"
colnames(dairy_sec7)[58]<-"purchase_price_4"
colnames(dairy_sec7)[64]<-"breed_5"
colnames(dairy_sec7)[65]<-"cohort_5"
colnames(dairy_sec7)[66]<-"number_5"
colnames(dairy_sec7)[67]<-"milk_yield_5"
colnames(dairy_sec7)[72]<-"purchase_price_5"
colnames(dairy_sec7)[78]<-"breed_6"
colnames(dairy_sec7)[79]<-"cohort_6"
colnames(dairy_sec7)[80]<-"number_6"
colnames(dairy_sec7)[81]<-"milk_yield_6"
colnames(dairy_sec7)[86]<-"purchase_price_6"
colnames(dairy_sec7)[92]<-"breed_7"
colnames(dairy_sec7)[93]<-"cohort_7"
colnames(dairy_sec7)[94]<-"number_7"
colnames(dairy_sec7)[95]<-"milk_yield_7"
colnames(dairy_sec7)[100]<-"purchase_price_7"
colnames(dairy_sec7)[106]<-"breed_8"
colnames(dairy_sec7)[107]<-"cohort_8"
colnames(dairy_sec7)[108]<-"number_8"
colnames(dairy_sec7)[109]<-"milk_yield_8"
colnames(dairy_sec7)[114]<-"purchase_price_8"
colnames(dairy_sec7)[120]<-"breed_9"
colnames(dairy_sec7)[121]<-"cohort_9"
colnames(dairy_sec7)[122]<-"number_9"
colnames(dairy_sec7)[123]<-"milk_yield_9"
colnames(dairy_sec7)[128]<-"purchase_price_9"
colnames(dairy_sec7)[134]<-"breed_10"
colnames(dairy_sec7)[135]<-"cohort_10"
colnames(dairy_sec7)[136]<-"number_10"
colnames(dairy_sec7)[137]<-"milk_yield_10"
colnames(dairy_sec7)[142]<-"purchase_price_10"

sub_sec7 <- dairy_sec7[c(1:1051),c(1,4:11,17,22:25,31,36:39,43,50:53,58,64:67,
            72,78:81,86,92:95,100,106:109,114,120:123,128,134:137,142)]########Kenya analysis

sub_sec7 <- dairy_sec7[c(1:1199),c(1,4:11,17,22:25,31,36:39,43,50:53,58,64:67,
            72,78:81,86,92:95,100,106:109,114,120:123,128,134:137,142)]########tanzania analysis
####SECTION 8.1##########################################################
#########################################################################
colnames(dairy_sec8.1)[2] <- "feed_type"
colnames(dairy_sec8.1)[4] <- "gf_feed"
colnames(dairy_sec8.1)[5] <- "other_gf"
colnames(dairy_sec8.1)[8] <- "fcr_feed"
colnames(dairy_sec8.1)[9] <- "other_fcr"
colnames(dairy_sec8.1)[10] <- "dcr_feed"
colnames(dairy_sec8.1)[11] <- "other_dcr"
colnames(dairy_sec8.1)[12] <- "conc_feed"
colnames(dairy_sec8.1)[13] <- "other_conc"
colnames(dairy_sec8.1)[16] <- "gf_amnt_dy"
colnames(dairy_sec8.1)[17] <- "gf_avg_ut"
colnames(dairy_sec8.1)[18] <- "gf_amt_fm"
colnames(dairy_sec8.1)[20] <- "gf_amt_otr"
colnames(dairy_sec8.1)[22] <- "gf_amt_purc"
colnames(dairy_sec8.1)[26] <- "gf_amt_pd"
colnames(dairy_sec8.1)[27] <- "gf_other"
colnames(dairy_sec8.1)[28] <- "gf_source"
colnames(dairy_sec8.1)[29] <- "gf_source_otr"
colnames(dairy_sec8.1)[33] <- "df_amnt_dy"
colnames(dairy_sec8.1)[34] <- "df_avg_ut"
colnames(dairy_sec8.1)[35] <- "df_amt_fm"
colnames(dairy_sec8.1)[37] <- "df_amt_otr"
colnames(dairy_sec8.1)[39] <- "df_amt_purc"
colnames(dairy_sec8.1)[43] <- "df_amt_pd"
colnames(dairy_sec8.1)[44] <- "df_other"
colnames(dairy_sec8.1)[45] <- "df_source"
colnames(dairy_sec8.1)[46] <- "df_source_otr"
colnames(dairy_sec8.1)[50] <- "fc_amnt_dy"
colnames(dairy_sec8.1)[51] <- "fc_avg_ut"
colnames(dairy_sec8.1)[52] <- "fc_amt_fm"
colnames(dairy_sec8.1)[54] <- "fc_amt_otr"
colnames(dairy_sec8.1)[56] <- "fc_amt_purc"
colnames(dairy_sec8.1)[60] <- "fc_amt_pd"
colnames(dairy_sec8.1)[61] <- "fc_other"
colnames(dairy_sec8.1)[62] <- "fc_source"
colnames(dairy_sec8.1)[63] <- "fc_source_otr"
colnames(dairy_sec8.1)[67] <- "dc_amnt_dy"
colnames(dairy_sec8.1)[68] <- "dc_avg_ut"
colnames(dairy_sec8.1)[69] <- "dc_amt_fm"
colnames(dairy_sec8.1)[71] <- "dc_amt_otr"
colnames(dairy_sec8.1)[73] <- "dc_amt_purc"
colnames(dairy_sec8.1)[77] <- "dc_amt_pd"
colnames(dairy_sec8.1)[78] <- "dc_other"
colnames(dairy_sec8.1)[79] <- "dc_source"
colnames(dairy_sec8.1)[80] <- "dc_source_otr"
colnames(dairy_sec8.1)[84] <- "conc_amnt_dy"
colnames(dairy_sec8.1)[85] <- "dc_avg_ut"
colnames(dairy_sec8.1)[86] <- "dc_amt_fm"
colnames(dairy_sec8.1)[88] <- "dc_amt_otr"
colnames(dairy_sec8.1)[90] <- "dc_amt_purc"
colnames(dairy_sec8.1)[94] <- "dc_amt_pd"
colnames(dairy_sec8.1)[95] <- "dc_other"
colnames(dairy_sec8.1)[96] <- "dc_source"
colnames(dairy_sec8.1)[97] <- "dc_source_otr"

sub_sec8 <- dairy_sec8.1[c(1:1051),c(2,4:5,8:13,16:18,20,22,26:29,33:35,37,39,43:46,50:52,
            54,56,60:63,67:69,71,73,77:80,84:86,88,90,94:97)]############kenya analysis
sub_sec8 <- dairy_sec8.1[c(1:1199),c(2,4:5,8:13,16:18,20,22,26:29,33:35,37,39,43:46,50:52,
            54,56,60:63,67:69,71,73,77:80,84:86,88,90,94:97)]############tanzania analysis
colnames(tz.dat)[2756] <- "latitude"
colnames(tz.dat)[2757] <- "longitude"
dairy_coordinates <- tz.dat[,c(2756:2757)]
newDF <- cbind(dairy_coordinates,sub_sec6,sub_sec7)
write.csv(newDF, file="kenya_survey.csv", row.names=FALSE)

############converting dataframe to shapefile#############################################################################################
tz_coord <- newDF
coordinates(tz_coord) = ~longitude+latitude
proj4string(tz_coord) <- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(tz_coord, "tanzania_survey.shp")
#####COMBINING MILK YIELD COLUMNS, FIRST CONVERT TO VECTOR#################################################
milk_yield <- unlist(dairy_sec7[c("milk_yield_1", "milk_yield_2", "milk_yield_3", "milk_yield_4",
                    "milk_yield_5", "milk_yield_6","milk_yield_7","milk_yield_8","milk_yield_9","milk_yield_10",use.names=FALSE)])

milk_yield <- as.data.frame(milk_yield)

milk_yield[milk_yield == "n/a"]  <- NA
na.omit(milk_yield)###OR
milk_yield[ which(complete.cases(milk_yield)) , ] 
str(milk_yield)
write.csv(milk_yield,file="milk_yield_tz.csv", row.names = FALSE)
### READ IN LITERATURE REVIEW DATA
#yield <- read.csv("lit_yields2.csv")
yield <- read.csv("lit_yields.csv")
na.omit(yield)
#New_yield <- cbind(milk_yield,other_yield)
#yield1 <- ggplot(yield, aes(x=cat, y=yield)) + geom_bar(stat="identity")
yield1 <- ggplot(yield, aes(x=HH_tz)) + geom_histogram(binwidth = 4, colour="black",fill="white") +
        theme_bw()+
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
yield2 <- yield1 + xlab(" Milk yield (l/cow/day)") + ylab ("Frequency")
plot(yield2)

#####calculating average milk yields for different surveys##################################################
mean(yield$HH_ke)####didnt work due to unequal length of variables in dataframe
summary(yield)
#####add mean lines to histograms

yield3 <- yield2 + geom_vline(xintercept=21.71, colour="red",linetype="dashed") +
        xlim(1,45)###Global survey
yield4 <- yield2 + geom_vline(xintercept=6.337, colour="red", linetype="dashed")###Household survey kenya
yield5 <- yield2 + geom_vline(xintercept=8.713, colour="red", linetype ="dashed")+
          xlim(1,30)###sub_sahara African survey
yield6 <- yield2 + geom_vline(xintercept =5.637, colour="red", linetype="dashed")###Household survey tanzania

plot(yield3)

#########################################EMISSIONS############################################################
##############################################################################################################
cf <- read.csv("emissions.csv")

################test for significant difference between both groups#############################################
SSA <- cf[cf$category=='CF_SSA',]
GS  <- cf[cf$category=='CF_GS',]

t.test(SSA$emissions,GS$emissions, paired = FALSE)

########################################################################################################

cf1 <- ggplot(cf, aes(x=category, y=emissions)) + geom_boxplot(width=.5) +
  stat_summary(fun.y = "mean", geom="point", size=2, shape=21, fill="red",colour="red")

cf2 <- cf1 + 
    theme_bw () + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank())

cf3 <- cf2 + theme(axis.text.x = element_text(colour="black",size=12,face="italic"),
                   axis.text.y = element_text(colour="black",size=12))

cf4 <- cf3 + scale_x_discrete(breaks=c("CF_GS", "CF_SSA"),
                              labels=c("Global", "Sub-Saharan Africa"))

cf5 <- cf4 + xlab("Survey") + ylab (bquote('Emission Intensity ('*kg ~ CO[2]~eq~kg^-1~milk*')')) 

plot(cf5)

#######plotting emissions from global studies#######################################################
####################################################################################################
gs_em <- read.csv("emissions.csv")
gs_em1 <- ggplot(gs_em, aes(x=cat_GS, y=GS_EM)) + geom_boxplot(width=.5)
gs_em2 <- gs_em1 + theme_bw() +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor.y=element_blank(),
              panel.grid.major.y=element_blank())
gs_em3 <- gs_em2 + theme(axis.text.x = element_text(colour="black", size=12),
                         axis.text.y=element_text(colour="black", size=12))
gs_em4 <- gs_em3 + scale_x_discrete(breaks=c("CH4_GS","CO_GS","NO_GS"),
                                    labels=c("methane","Carbondioxide","Nitrous oxide"))

gs_em5 <- gs_em4 +xlab("") +ylab(bquote('Emission Intensity ('*kg ~ CO[2]~eq~kg^-1~milk*')'))


plot(gs_em5)
###########################test for significant difference within groups#########################################
t.test(gs_em$cat_GS=='NO_GS', gs_em$cat_GS=='CH4_GS', paired = TRUE)
t.test(gs_em$cat_GS=='CO_GS', gs_em$cat_GS=='CH4_GS', paired=TRUE)
t.test(gs_em$cat_GS=='NO_GS', gs_em$cat_GS=='CO_GS', paired=TRUE)
################################Subsaharan African studies########################################
##################################################################################################
sa_em <- read.csv("emissions_SSA.csv")

sa_em1 <- ggplot(sa_em, aes(x=cat_SSA,y=SSA_EM))+geom_boxplot(width=.5)
sa_em2 <- sa_em1 +theme_bw()+
          theme(panel.grid.major.x=element_blank(),
                panel.grid.minor.y=element_blank(),
                panel.grid.major.y=element_blank())
sa_em3 <- sa_em2 + theme(axis.text.x=element_text(colour="black",size=12),
                         axis.text.y=element_text(colour="black",size=12))

sa_em4 <- sa_em3 + scale_x_discrete(breaks=c("CH4_SSA", "NO_SSA"),
                                    labels=c("Methane","Nitrous oxide"))

sa_em5 <- sa_em4 + xlab("") + ylab(bquote('Emission Intensity ('*kg ~ CO[2]~eq~kg^-1~milk*')'))

plot(sa_em5)

###########################test for differences within group#########################################
t.test(sa_em$cat_SSA=='CH4_SSA',sa_em$cat_SSA=='NO_SSA', paired=TRUE)


####################Household breed type####################################################################
############################################################################################################
###############Tanzania#############################################################################
breed_tz <- unlist(dairy_sec7[c("breed_1","breed_2","breed_3","breed_4","breed_5","breed_6","breed_7","breed_8",
                  "breed_9","breed_10")])

breed_tz <- as.data.frame(breed_tz)

breed_nos_tz <- unlist(dairy_sec7[c("number_1","number_2","number_3","number_4","number_5","number_6","number_7",
                       "number_8","number_9","number_10")])
breed_nos_tz <- as.data.frame(breed_nos_tz)

tz_breeds <- cbind(breed_tz,breed_nos_tz,dairy_sec7$hhid)
#tz_breeds <- tz_breeds [c(1:1199),]
#write.csv(tz_breeds, file="tz_breeds.csv", row.names = FALSE)

#######comparing number of breeds in households#################################################################
#####Remove NAs from dataframe###################################################
#tz_b[tz_b== "n/a"] <- NA
#row.has.na <- apply(tz_b,1, function(x){any(is.na(x))})
#sum(row.has.na)
#tz_b <- tz_b[!row.has.na,]
#tz_b[apply(tz_b,1,function(tz_b)any(!is.na(tz_b))),]
tz_b <- read.csv("tz_breeds.csv")
tz_b1 <- ggplot(tz_b, aes(x=breed_tz, y=breed_nos_tz,fill=type)) + geom_bar(stat = "identity") +
        scale_fill_manual(values=c("red","green"))

#################categorise into two glasses ONLY#################################################################
##################################################################################################################
tz_b1 <- ggplot(tz_b, aes(x=type, y=breed_nos_tz,fill=type)) + geom_bar(stat="identity", width=.2) +
      scale_fill_manual(values=c("red","green")) + guides(fill=FALSE)
tz_b2 <- tz_b1 + coord_flip()
tz_b3 <- tz_b2 + theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank())

tz_b4 <- tz_b3 + theme(axis.text.y = element_text(colour = "black",face = "italic",size=14),
                       axis.text.x = element_text(colour = "black",size=14))

tz_b5 <- tz_b4 + xlab("Breed type") + ylab("Total nos. of livestock") + 
         theme(axis.title.x=element_text(size=14,colour="black"),
               axis.title.y=element_text(size=14,colour="black"))

tz_b6 <- tz_b5 + theme(legend.position = c(.85,0.15)) +
        theme(legend.background = element_rect(colour="black"))

plot(tz_b5)

############comparing breeds#######################################################################################################
###################################################################################################################################
breed_count_tz <- ggplot(tz_b, aes(x=breed_tz)) + geom_bar()
breed_count_tz1 <- breed_count_tz + coord_flip()
breed_count_tz2 <- breed_count_tz1 +theme_bw()+
                theme(panel.grid.major.y=element_blank())
breed_count_tz3 <- breed_count_tz2 + xlab("Breed type") + ylab("Frequency")
plot(breed_count_tz3)
#tz_b2 <- tz_b + scale_x_discrete(breaks=c("Holstein-Friesian-crossbred", "Ayrshire-crossbred","Jersey-crossbred",
                                          ###     "Mpwapwa-crossbred","Boran-crossbred","Localcow-purebred","Holstein-Friesian-purebred","Ayrshire-purebred",
                                          ###      "Jersey-purebred","Mpwapwa-purebred","Boran-purebred"),labels=c("1","2","3","4","5","6","7","8","9","10","11"))

###############Kenya#############################################################################
breed_ke <- unlist(dairy_sec7[c("breed_1","breed_2","breed_3","breed_4","breed_5","breed_6","breed_7","breed_8",
                                "breed_9","breed_10")])

breed_ke <- as.data.frame(breed_ke)

breed_nos_ke <- unlist(dairy_sec7[c("number_1","number_2","number_3","number_4","number_5","number_6","number_7",
                                    "number_8","number_9","number_10")])
breed_nos_ke <- as.data.frame(breed_nos_ke)

ke_breeds <- cbind(breed_ke,breed_nos_ke,dairy_sec7$hhid)
#ke_breeds <- ke_breeds [c(1:1051),]
#write.csv(ke_breeds, file="ke_breeds.csv", row.names = FALSE)

#######comparing number of breeds in households#################################################################
ke_b <- read.csv("ke_breeds.csv")

#####Remove NAs from dataframe###################################################
#ke_b[ke_b== "n/a"] <- NA
#row.has.na <- apply(ke_b,1, function(x){any(is.na(x))})
#sum(row.has.na)
#ke_b <- ke_b[!row.has.na,]
#ke_b[apply(ke_b,1,function(ke_b)any(!is.na(ke_b))),]

ke_b1 <- ggplot(ke_b, aes(x=breed_ke, y=breed_nos_ke,fill=type)) + geom_bar(stat = "identity") +
  scale_fill_manual(values=c("red","green")) 

######### OR categorise into two groups############################################################################
###############################################################################################################

ke_b1 <- ggplot(ke_b, aes(x=type,y=breed_nos_ke,fill=type)) + geom_bar(stat="identity",width = .15) + 
  scale_fill_manual(values=c("red","green")) + guides (fill=FALSE) 

ke_b2 <- ke_b1 + coord_flip()
ke_b3 <- ke_b2 + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

ke_b4 <- ke_b3 + theme(axis.text.y = element_text(colour = "black",face = "italic",size=14),
                       axis.text.x = element_text(colour = "black",size=14))

ke_b5 <- ke_b4 + xlab("Breed type") + ylab("Total nos. of livestock") + 
  theme(axis.title.x=element_text(size=14,colour="black"),
        axis.title.y=element_text(size=14,colour="black"))

ke_b6 <- ke_b5 + theme(legend.position = c(0.9,0.8)) +
  theme(legend.background = element_rect(colour="black"))

ke_b7 <- ke_b6 + ylim(0,7500)
#ke_b8 <- ke_b7 + 
 # scale_y_continuous(breaks = seq(0, 2500, 5000, 7500))

plot(ke_b7)

############comparing breeds#######################################################################################################
###################################################################################################################################
breed_count_ke <- ggplot(ke_b, aes(x=breed_ke)) + geom_bar()
breed_count_ke1 <- breed_count_ke + coord_flip()
breed_count_ke2 <- breed_count_ke1 +theme_bw()+
  theme(panel.grid.major.y=element_blank())
breed_count_ke3 <- breed_count_ke2 + xlab("Breed type") + ylab("Frequency")
plot(breed_count_ke3)


####################feed analysis from household surveys#############################################################
###############################KENYA######################################################################################
feed_source_ke <- ggplot(sub_sec6, aes(x=feed_source)) + geom_histogram(binwidth = .5)
sum(sub_sec6$feed_source==5)####counting numbers
fd_sour_ke <- feed_source_ke + scale_x_continuous(breaks = c(0,1,2,3,4,5),
              labels=c( "Stovers","Grazing","Concentrates","Silage/Hay","Improved pasture","Others"))
fd_sour_ke2 <- fd_sour_ke+theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour="grey",linetype = "dashed"))
fd_sour_ke3 <- fd_sour_ke2 + xlab("Feed source") 
fd_sour_ke4 <- fd_sour_ke3 + ylim(0, 1000)
plot(fd_sour_ke4)

###########################Tanzania############################################################################
################################################################################################################
feed_source_tz <- ggplot(sub_sec6, aes(x=feed_source)) + geom_histogram(binwidth = .5)
sum(sub_sec6$feed_source==5)####counting numbers
fd_sour_tz <- feed_source_tz + scale_x_continuous(breaks = c(0,1,2,3,4,5),
                                                  labels=c( "Stovers","Grazing","Concentrates","Silage/Hay","Improved pasture","Others"))
fd_sour_tz2 <- fd_sour_tz+theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey",linetype = "dashed"))
fd_sour_tz3 <- fd_sour_tz2 + xlab("Feed source")
fd_sour_tz4 <- fd_sour_tz3 + ylim(0,1000)
plot(fd_sour_tz4)
#################Milk yield analysis from Household surveys############################################
#################Tanzania##############################################################################
breed_milk_tz <- cbind(breed_tz, milk_yield)
#write.csv(breed_milk_tz, file="breed_milk_tz.csv", row.names = FALSE)
#####REMOVE NAs###########################################################################################
####################################################################################################
row.has.na <- apply(breed_milk_tz,1, function(x){any(is.na(x))})
#sum(row.has.na)
breed_milk_tz <- breed_milk_tz[!row.has.na,]
##########################################################################################################

breed_milk_tz <- read.csv("breed_milk_tz.csv")
bm_tz1 <- breed_milk_tz[breed_milk_tz$type=="Improved",]  #################For improved breeds
bm_tz1 <- breed_milk_tz[breed_milk_tz$type=="Local",]  #################For local breeds
bm_tz2 <- ggplot(bm_tz1, aes(x=milk_yield,fill=type)) + geom_histogram(colour="black") + 
  scale_fill_manual(values=c("green")) + guides(fill=FALSE)
mean(bm_tz1$milk_yield)
bm_tz3 <- bm_tz2 + geom_vline(xintercept = 1.47, colour="black",linetype="dashed",size=1)

bm_tz4 <-bm_tz3 + theme_bw()+ 
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour="grey", linetype = "dashed"))

bm_tz5 <- bm_tz4 + xlab("Milk yield (l/cow/day)") + ylab("Nos. of cows")

plot(bm_tz5)

#########################Kenya###################################################################
#################################################################################################
breed_milk_ke <- cbind(breed_ke, milk_yield)
#write.csv(breed_milk_ke, file="breed_milk_ke.csv", row.names = FALSE)
#####REMOVE NAs###########################################################################################
####################################################################################################
#row.has.na <- apply(breed_milk_tz,1, function(x){any(is.na(x))})
#sum(row.has.na)
#breed_milk_tz <- breed_milk_tz[!row.has.na,]
##########################################################################################################

breed_milk_ke <- read.csv("breed_milk_ke.csv")
bm_ke1 <- breed_milk_ke[breed_milk_ke$type=="Improved",]  #################For improved breeds
bm_ke1 <- breed_milk_ke[breed_milk_ke$type=="Local",]  #################For local breeds
bm_ke2 <- ggplot(bm_ke1, aes(x=milk_yield,fill=type)) + geom_histogram(colour="black") + 
  scale_fill_manual(values=c("red")) + guides(fill=FALSE)
mean(bm_ke1$milk_yield)
bm_ke3 <- bm_ke2 + geom_vline(xintercept = 2.51, colour="black",linetype="dashed",size=1)

bm_ke4 <-bm_ke3 + theme_bw()+ 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey", linetype = "dashed"))

bm_ke5 <- bm_ke4 + xlab("Milk yield (l/cow/day)") + ylab("Nos. of cows")

plot(bm_ke5)




rm(list=ls())
