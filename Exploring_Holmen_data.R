
#Script - exploring validation data from Holmen

val<-read.csv2("C:\\Michelle_R\\R_library_folder\\SBB_riskindex\\Validation\\Holmen_expl.csv")
str(val)

#Add column for damage yes or no
val$damage<-ifelse(val$SVol==0,0,1)


val$dgv_t<-as.factor(ifelse(val$DGV>20,1,0)) #make binary var for diameter


val$soil<-factor(val$MARKF,order=TRUE,levels=c("1","2","3","4")) #make soil into ordinal categorical var


#I want to make a column from site index into only G or T
#Partial string match

library(dplyr)
library(purrr)

#make site index into character to be able to split 
val$SI_TM<-as.character(val$SI_TM)


#make a new column based on if G or T in site index column
val<-val %>%
  mutate(tree_site = map_chr(SI_TM, ~unlist(strsplit(.,""))[1])) %>%
  mutate(tree_site = case_when(tree_site == "T" ~ "Pine",
                               tree_site == "G" ~ "Spruce"))

val$site_p<-substring(val$SI_TM,2,3)

str(val)

#make site index and tree site columns into factors 
val$SI_TM<-as.factor(val$SI_TM)
val$tree_site<-as.factor(val$tree_site)
val$site_p<-factor(val$site_p,order=TRUE)


#Remove stands without spruce

val<-val[which(val$M3SK_TOT_G>0),]



val <- val %>% mutate_at(vars(MALKLASS,Status,ALDERSKLAS,BESTNR), as.factor)

#data set without zero damage
val0<-val[which(val$SVol!=0),]

#data set without stands with no spruce
valS<-val[which(val$M3SK_TOT_G!=0),]

library(ggplot2)


#Look at the relationship between damaged volume and damaged area 

ggplot(valS)+ 
  geom_point(aes(x=SVol,y=Areal))+ #Looks like a good correlation, meaning we can use either as response
  ylim(0,3)+
  xlim(0,400)+
  theme_classic()



#check site index
ggplot(data=valS)+
  geom_point(aes(x=SI_TM,y=SVol))+
  theme_classic()

#check spruce volume - all ways possible
ggplot(data=valS)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol)) #damaged volume

ggplot(data=valS)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol/M3SK_TOT_G))+ #damaged volume per total volume spruce
  ylim(0,1.5)
#why are there values of SVol/M3SK_TOT_G that are larger than 1? That should not be possible??
#the damaged volume should not be larger than the total volume????

ggplot(data=val)+
  geom_point(aes(x=M3SK_HA_Gr,y=Areal)) #damaged area

ggplot(data=val)+
  geom_point(aes(x=M3SK_HA_Gr,y=Areal/TOTALAREAL)) #damaged area per total area

#same without zeros
ggplot(data=val0)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol)) #damaged volume

ggplot(data=val0)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol/M3SK_TOT_G)) #damaged volume per total volume spruce

ggplot(data=val0)+
  geom_point(aes(x=M3SK_HA_Gr,y=Areal)) #damaged area

ggplot(data=val0)+
  geom_point(aes(x=M3SK_HA_Gr,y=Areal/TOTALAREAL)) #damaged area per total area

#Could it be valuable to look at individual factors without the cases when they are zero?
#For example - look at broadleaves when there are broadleaves 
#Because there are many 0 broadleave stands and they might confound the pattern

#data set without stands with no spruce
valL<-valS[which(valS$M3SK_TOT_L!=0),]

ggplot(data=valL)+
  geom_point(aes(x=M3SK_HA_Lo,y=SVol),size=2)+
  theme_classic()


#Correlation between volume spruce and volume e.g. broadleaves? Problem?

cor.test(valS$M3SK_HA_Gr,valS$M3SK_HA_Lo) #-0.07
cor.test(valS$M3SK_TOT_G,valS$M3SK_TOT_L) #0.24

#NMDS or similar 

#Damage/no damage, volume, area is explained by the combo of stand vars?

vars<-cbind(valS$ALD_TOT,valS$DGV,valS$M3SK_HA_Gr,valS$M3SK_HA_Lo,valS$TR_HA,valS$soil,valS$tree_site)

vars.pca<-prcomp(vars,center=TRUE,scale. = TRUE)
vars.pca
plot(vars.pca)
summary(vars.pca)

SBB_d<-as.factor(valS$damage)

library(ggfortify)
sbb.pca.plot <- autoplot(vars.pca,
                          data = vars,
                          colour = SBB_d)

biplot(vars.pca)

library(devtools)
install_github("vqv/ggbiplot")

#Ideas:
#Do all analyses with area and volume and the proportions
#Do all analyses with and without 0:s for damage
#Need to take number of stands into account since there might be different numbers of stands in diff categories
#Look at site index as measure of productivity + does it make a difference if it is "pine soils"
#Variables such as soil moisture, age class, and maybe site index should be ordered categorical vars 
#make DGV into a above and below 20 cm (i.e. categorical) variable, because we expect no difference above
#easy to do with ifelse(). 
#Interactions?


#MODELS

library(lme4)
library(car)

model_area<-lm(Areal~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_area)
summary(model_area)


model_vol<-lm(SVol~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_vol)

model_area_p<-lm(Areal/TOTALAREAL~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_area_p)

model_vol_p<-lm(SVol/M3SK_TOT_G~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_vol_p)

ggplot(val)+
  geom_point(aes(x=ALD_TOT,y=Areal))

ggplot(data=val)+
  geom_point(aes(x=DGV,y=SVol))




ggplot(data=val)+
  geom_point(aes(x=tree_site,y=SVol/M3SK_TOT_G))+
  theme_classic()+
  facet_wrap(~soil)

val_zero<-val[which(val$SVol!=0),]

str(val_zero)

model_vol<-glm(SVol~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+TR_HA+dgv_t+soil+tree_site,family=Gamma(link = "log"),data=val_zero)
Anova(model_vol)
#now I think tree_site is sign because there are fewer objects (need to scale for this somehow)
ci_vol<-confint(model_vol)
plot(model_vol)

hist(resid(model_vol))

ggplot(data=model_vol)+
  geom_point(aes(x=model_vol$linear.predictors,y=model_vol$fitted.values))



model_ar<-glm(Areal/TOTALAREAL~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+TR_HA+dgv_t+soil+tree_site,family=Gamma(link = "log"),data=val_zero)
Anova(model_ar)

min(val_zero$M3SK_HA_Gr)
min(val_zero$M3SK_HA_Lo)
min(val_zero$ALD_TOT)


ggplot(data=val_zero)+
  geom_point(aes(x=site_p,y=SVol))+
  theme_classic()+
  facet_wrap(~tree_site)

ggplot(data=val_zero)+
  geom_point(aes(x=soil,y=SVol))+
  theme_classic()



plot(val$SVol)
mean(val$SVol)
sd(val$SVol)
hist(rnorm(100,38,69))
hist(rgamma(100,(38^2/69^2),(38/69^2)))
hist(val$SVol)
hist(val_zero$SVol)

hist(val$Areal)




#calculate index for these stands and/or make some kind of model based on index
for(i in 1:length(val_zero$BESTNR)){
  
  val_zero$SBB_index[i]<-ifelse(val_zero$dgv_t[i]==0,0,1)
  
  
  
}

