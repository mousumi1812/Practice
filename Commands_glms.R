
summary=read.csv("Summary.csv")
head(summary)
attach(summary)
cor.test(NB_cells, NB_count)

cor.test(B_cells, B_count)


#Breeding season
br14=read.csv("Breeding.csv")
head(br14)
names(br14)


plot(br14$C2.vegetation, br14$Species.richness, xlab = "% vegetation", ylab = "Observed species richness")

M1=glm(Species.richness~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(M1)

M2=update(M1, . ~ . - RGmean)
summary(M2)
anova(M1,M2, test='Chisq')

M3=update(M2, . ~ . - C1.Water)
summary(M3)
anova(M3,M2, test='Chisq')

br14$p=predict(M3, type="response")

plot(br14$C3.Built.up, br14$p, xlab = "% Built.up", ylab = "Expected species richness")

M4=update(M3, . ~ . - C2.vegetation)
summary(M4)
anova(M3,M4, test='Chisq')

#Species, Breeding
br14[is.na(br14)]<-0
head(br14)

#Common Myna
m1=glm(Common.Myna~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(m1)

require(MASS)

mm1=glm.nb(Common.Myna~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up,  data=br14)
summary(mm1)

sum(resid(mm1, type = "pearson")^2)/mm1$df.res 

m2=update(mm1, . ~ . - C2.vegetation)
summary(m2)
anova(m1,m2, test='Chisq')

br14$p1=predict(m2, type="response")

plot(br14$C3.Built.up, br14$p1, xlab = "% Built-up", ylab = "Predicted Common Myna abundance ")


#Jungle Myna

n1=glm(Jungle.Myna~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(m1)

n2=update(n1, . ~ . - DEMmean)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - RGmean)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C1.Water)
summary(n4)
anova(n3,n4, test='Chisq')

br14$p2=predict(n4, type="response")

plot(br14$C2.vegetation, br14$p2, xlab = "% vegetation", ylab = "Predicted Jungle Myna abundance ")


#Rock Pigeon
names(br14)

n1=glm(Rock.Pigeon~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - DEMmean)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - RGmean)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C2.vegetation)
summary(n4)
anova(n3,n4, test='Chisq')

br14$p3=predict(n4, type="response")

plot(br14$C3.Built.up, br14$p3, xlab = "% Built-up", ylab = "Predicted Rock Pigeon abundance ")



#Common tailorbird
names(br14)

n1=glm(Common.Tailorbird~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - C1.Water)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - RGmean)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - DEMmean)
summary(n4)
anova(n3,n4, test='Chisq')



br14$p4=predict(n4, type="response")

plot(br14$C3.Built.up, br14$p4, xlab = "% Built-up", ylab = "Predicted Tailorbird abundance ")


#Jungle Prinia
names(br14)

n1=glm(Jungle.Prinia~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=poisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - C1.Water)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - RGmean)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C3.Built.up)
summary(n4)
anova(n3,n4, test='Chisq')



br14$p5=predict(n1, type="response")

plot(br14$C2.vegetation, br14$p5, xlab = "%vegetation", ylab = "Predicted Jungle Prinia abundance ")

##Spot-billed Pelican
names(br14)

n1=glm(Spot.billed.Pelican~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=poisson, data=br14)
summary(n1)

br14$p6=predict(n1, type="response")

plot(br14$C1.Water, br14$p6, xlab = "%water", ylab = "Predicted SB Pelican abundance ")

##Large-billed Crow

names(br14)

n1=glm(Large.billed.Crow~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - C1.Water)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - RGmean)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C2.vegetation)
summary(n4)
anova(n3,n4, test='Chisq')



br14$p7=predict(n4, type="response")

plot(br14$C3.Built.up, br14$p7, xlab = "% Built-up", ylab = "Predicted LB Crow abundance ")


##Common Iora

names(br14)

n1=glm(Common.Iora~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - DEMmean)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - C2.vegetation)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C1.Water)
summary(n4)
anova(n3,n4, test='Chisq')

br14$p8=predict(n4, type="response")

plot(br14$C3.Built.up, br14$p8, xlab = "% Built-up", ylab = "Predicted Common Iora abundance ")

##Little Grebe
names(br14)

n1=glm(Little.Grebe~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=br14)
summary(n1)

n2=update(n1, . ~ . - DEMmean)
summary(n2)
anova(n1,n2, test='Chisq')

n3=update(n2, . ~ . - C2.vegetation)
summary(n3)
anova(n3,n2, test='Chisq')

n4=update(n3, . ~ . - C1.Water)
summary(n4)
anova(n3,n4, test='Chisq')

br14$p8=predict(n4, type="response")

plot(br14$C3.Built.up, br14$p8, xlab = "% Built-up", ylab = "Predicted Common Iora abundance ")



####Non-breeding season
nbr14=read.csv("nonbreeding_2014.csv")
head(nbr14)
names(nbr14)

#All species
M1=glm(Sp.rich~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(M1)

M2=update(M1, . ~ . - C1.Water)
summary(M2)
anova(M1, M2, test='Chisq')

M3=update(M2, . ~ . - RGmean)
summary(M3)
anova(M3, M2, test='Chisq')

M4=update(M3, . ~ . - DEMmean)
summary(M4)
anova(M3, M4, test='Chisq')
M5=update(M4, . ~ . - C2.vegetation)
summary(M5)

anova(M4,M5, test='Chisq')

nbr14$p=predict(M4, type="response")

plot(nbr14$C3.Built.up, nbr14$p, xlab = "% Built-up", ylab = "Predicted species richness ")

#Common Myna (pointless)
m1=glm(Common.My~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C3.Built.up)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - DEMmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - RGmean)
summary(m4)
anova(m3, m4, test='Chisq')


#Black Kite
m1=glm(Black.Kite~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C2.vegetation)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - C1.Water)
summary(m3)
anova(m3, m2, test='Chisq')


nbr14$p1=predict(m3, type="response")

plot(nbr14$C3.Built.up, nbr14$p1, xlab = "% Built-up", ylab = "Predicted Black kite abundance ")

names(nbr14)


#Rock.Pigeon
m1=glm(Rock.Pigeon~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C2.vegetation)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - DEMmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C1.Water)
summary(m4)
anova(m3, m4, test='Chisq')

nbr14$p2=predict(m3, type="response")

plot(nbr14$C3.Built.up, nbr14$p2, xlab = "% Built-up", ylab = "Predicted Rock pigeon abundance ")


#Cattle.Egret
m1=glm(Cattle.Egret~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C3.Built.up)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - RGmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C2.vegetation)
summary(m4)
anova(m3, m4, test='Chisq')

names(nbr14)

#Greater.Coucal
m1=glm(Greater.Coucal~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C1.Water)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - RGmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C2.vegetation)
summary(m4)
anova(m3, m4, test='Chisq')

#Asian Koel
m1=glm(Asian.Koel~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C3.Built.up)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - RGmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - DEMmean)
summary(m4)
anova(m3, m4, test='Chisq')

#Black Drongo
m1=glm(Black.Drongo~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - DEMmean)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - C1.Water)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - RGmean)
summary(m4)
anova(m3, m4, test='Chisq')

nbr14$p3=predict(m4, type="response")

plot(nbr14$C3.Built.up, nbr14$p3, xlab = "% Built-up", ylab = "Predicted Black drongo abundance ")

#Red vented Bulbul
m1=glm(Red.vented.Bulbul~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C1.Water)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - C2.vegetation)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C3.Built.up)
summary(m4)
anova(m3, m4, test='Chisq')

nbr14$p4=predict(m4, type="response")

plot(nbr14$C3.Built.up, nbr14$p3, xlab = "% Built-up", ylab = "Predicted Black drongo abundance ")

names(nbr14)

#House.Crow
m1=glm(House.Crow~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C1.Water)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - DEMmean)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C2.vegetation)
summary(m4)
anova(m3, m4, test='Chisq')

nbr14$p5=predict(m4, type="response")

plot(nbr14$C3.Built.up, nbr14$p5, xlab = "% Built-up", ylab = "Predicted House Crow abundance ")


#Indian.Robin
m1=glm(Indian.Robin~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C1.Water)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - C2.vegetation)
summary(m3)
anova(m3, m2, test='Chisq')

nbr14$p6=predict(m3, type="response")

plot(nbr14$C3.Built.up, nbr14$p6, xlab = "% Built-up", ylab = "Predicted Indian Robin abundance ")

names(nbr14)

#White-cheeked Barbet
m1=glm(White.cheeked.Barbet~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr14)
summary(m1)

m2=update(m1, . ~ . - C3.Built.up)
summary(m2)
anova(m1, m2, test='Chisq')

m3=update(m2, . ~ . - C2.vegetation)
summary(m3)
anova(m3, m2, test='Chisq')

m4=update(m3, . ~ . - C1.Water)
summary(m4)
anova(m3, m4, test='Chisq')


