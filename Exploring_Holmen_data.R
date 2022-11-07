
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
  geom_point(aes(x=M3SK_HA_Gr,y=SVol))+
  theme_classic()+
  ylab("Damaged volume")+
  xlab("Volume of spruce (per ha)")
  #damaged volume

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
vars<-data.frame(vars)
colnames(vars)<-c("Age","Diamter","Vol spruce","Vol broadleaves","Density","Soil moisture","Site type")
str(vars)


library(vegan)

#vars.pca<-prcomp(vars,center=TRUE,scale. = TRUE)
vars.pca<-rda(vars,scale=TRUE)
vars.pca
plot(vars.pca)
summary(vars.pca)
vars.pca$CA

cor(vars)

summary(eigenvals(vars.pca))

SBB_d<-as.factor(valS$damage)

vars.w.group<-cbind(vars,SBB_d)

biplot(vars.pca,display = c("sites", "species"),type = c("text", "points"))
ordiellipse(vars.pca,group=SBB_d,col=c("orange","lightblue"),draw="polygon",alpha=100,conf = .95)

biplot(vars.pca,display = "sites",type =  "points")
ordiellipse(vars.pca,group=SBB_d,col=c("orange","lightblue"),draw="polygon",alpha=100,conf = .95)
ordihull(vars.pca,group=SBB_d,draw="polygon",col=c("orange","lightblue"))


pc1<-vars.pca$CA$u[,1]
pc2<-vars.pca$CA$u[,2]

model.w.pca<-glm(damage~pc1+pc2,data=valS,family = binomial)
Anova(model.w.pca)
summary(model.w.pca)

valS1<-valS[which(valS$SVol>0),]
vars2<-cbind(valS1$ALD_TOT,valS1$DGV,valS1$M3SK_HA_Gr,valS1$M3SK_HA_Lo,valS1$TR_HA,valS1$soil,valS1$tree_site)
vars2<-data.frame(vars2)
colnames(vars2)<-c("Age","Diamter","Vol spruce","Vol broadleaves","Density","Soil moisture","Site type")
str(vars2)

#vars.pca<-prcomp(vars,center=TRUE,scale. = TRUE)
vars.pca2<-rda(vars2,scale=TRUE)
pc1.2<-vars.pca2$CA$u[,1]
pc2.2<-vars.pca2$CA$u[,2]
model.w.pca2<-glm(SVol~pc1.2+pc2.2,data=valS1,family = Gamma(link="log"))
Anova(model.w.pca2)
summary(model.w.pca2)





nmds_analysis<-metaMDS(vars,k=2)
plot(nmds_analysis)

ordiplot(nmds_analysis,type="n")
ordihull(nmds_analysis,groups=SBB_d,draw="polygon")
orditorp(nmds_analysis,display="species",col="red")

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


ggplot(data=valS)+
  geom_point(aes(x=site_p,y=SVol))+
  theme_classic()+
  facet_wrap(~tree_site,ncol=1)

ggplot(data=valS[which(valS$tree_site=="Spruce"),])+
  geom_point(aes(x=site_p,y=SVol))+
  theme_classic()+
  xlab("Site index")+
  ylab("Damged volume")+
  ggtitle("Spruce site index")


ggplot(data=valS[which(valS$tree_site=="Pine"),])+
  geom_point(aes(x=site_p,y=SVol))+
  theme_classic()+
  xlab("Site index")+
  ylab("Damged volume")+
  ggtitle("Pine site index")




ggplot(data=valS)+
  geom_boxplot(aes(x=soil,y=SVol),color="darkgrey")+
  theme_classic()+
  xlab("Soil moisture level")+
  ylab("Damged volume")+
  ylim(0,200)+
  stat_summary(fun="mean",aes(x=soil,y=SVol,label=signif(..y..,3)),geom="text")+ 
  scale_x_discrete(labels=c("1" = "Dry", "2" = "Mesic", "3" = "Mesic-moist"))

ggplot(data=valS,aes(x=soil,y=SVol))+
  geom_dotplot(binaxis = "y", stackdir = "center",binwidth = 2,color="grey")+
  theme_classic()+
  xlab("Soil moisture level")+
  ylab("Damged volume")+
  ylim(0,200)+
  stat_summary(fun="mean",aes(x=soil,y=SVol),size=0.8)+ 
  scale_x_discrete(labels=c("1" = "Dry", "2" = "Mesic", "3" = "Mesic-moist"))

ggplot(data=valS,aes(x=soil,y=SVol))+
  geom_jitter(color="grey",height=0,width=0.02)+
  theme_classic()+
  xlab("Soil moisture level")+
  ylab("Damged volume")+
  ylim(0,200)+
  stat_summary(fun="mean",aes(x=soil,y=SVol),size=0.8)+ 
  scale_x_discrete(labels=c("1" = "Dry", "2" = "Mesic", "3" = "Mesic-moist"))

#label=signif(..y..,3)

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

#is it so that "utförd" means that they have removed damaged tree and that
#all areas that are utförd has a damage volume of zero?

val_u<-val[which(val$Status!="Planerad"),]



#calculate index for these stands and/or make some kind of model based on index
for(i in 1:length(val_zero$BESTNR)){
  
  val_zero$SBB_index[i]<-ifelse(val_zero$dgv_t[i]==0,0,1)
  
  
  
}

