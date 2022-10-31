
#Script - exploring validation data from Holmen

val<-read.csv2("C:\\Michelle_R\\R_library_folder\\SBB_riskindex\\Validation\\Holmen_expl.csv")
str(val)

#Add column for damage yes or no
val$damage<-ifelse(val$SVol==0,0,1)


#Remove stands without spruce

val<-val[which(val$M3SK_TOT_G>0),]

library(dplyr)

val <- val %>% mutate_at(vars(SI_TM,MALKLASS), as.factor)

#data set without zeros 
val0<-val[which(val$SVol!=0),]

library(ggplot2)

#check site index
ggplot(data=val)+
  geom_point(aes(x=SI_TM,y=SVol))

#check spruce volume - all ways possible
ggplot(data=val)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol)) #damaged volume

ggplot(data=val)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol/M3SK_TOT_G)) #damaged volume per total volume spruce

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





#Ideas:
#Do all analyses with area and volume and the proportions
#Do all analyses with and without 0:s for damage
#Need to take number of stands into account since there might be different numbers of stands in diff categories
#Look at site index as measure of productivity + does it make a difference if it is "pine soils"
#Variables such as soil moisture, age class, and maybe site index should be ordered categorical vars 


#MODELS

library(lme4)
library(car)

model_area<-lm(Areal~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_area)

model_vol<-lm(SVol~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_vol)

model_area_p<-lm(Areal/TOTALAREAL~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_area_p)

model_vol_p<-lm(SVol/M3SK_TOT_G~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+DGV+MARKF+TR_HA,data=val)
Anova(model_vol_p)

ggplot(val)+
  geom_point(aes(x=ALD_TOT,y=Areal))
