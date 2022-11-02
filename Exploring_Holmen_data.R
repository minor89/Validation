
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


#Look at the relationship between damaged volume and damaged area 

ggplot(val)+ 
  geom_point(aes(x=SVol,y=Areal))+ #Looks like a good correlation, meaning we can use either as response
  ylim(0,3)+
  xlim(0,400)+
  theme_classic()








#check site index
ggplot(data=val)+
  geom_point(aes(x=SI_TM,y=SVol))

#check spruce volume - all ways possible
ggplot(data=val)+
  geom_point(aes(x=M3SK_HA_Gr,y=SVol)) #damaged volume

ggplot(data=val)+
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


val$dgv_t<-as.factor(ifelse(val$DGV>20,1,0)) #make binary var for diameter
val$soil<-as.factor(val$MARKF) #make into factor

val$soil<-factor(val$MARKF,order=TRUE,levels=c("1","2","3","4")) #make soil into ordinal categorical var

model_vol<-lm(SVol~M3SK_HA_Gr+M3SK_HA_Lo+ALD_TOT+TR_HA+dgv_t+soil+tree_site,data=val)
Anova(model_vol)

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

#make site index and tree site columns into factors 
val$SI_TM<-as.factor(val$SI_TM)
val$tree_site<-as.factor(val$tree_site)

ggplot(data=val)+
  geom_point(aes(x=tree_site,y=SVol/M3SK_TOT_G))+
  theme_classic()+
  facet_wrap(~soil)

