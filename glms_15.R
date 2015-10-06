#non-breeding season
nbr15=read.csv("nb_15_glm.csv")
head(nbr15)
##git


M1=glm(Sp.richness~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=quasipoisson, data=nbr15)
summary(M1)

M2=update(M1, . ~ . - DEMmean)
summary(M2)
anova(M1,M2, test='Chisq')

M3=update(M2, . ~ . - C1.Water)
summary(M3)
anova(M3,M2, test='Chisq')

nbr15$p=predict(M3, type="response")

plot(nbr15$C3.Built.up, nbr15$p, xlab = "% Built.up", ylab = "Predicted species richness")

#Breeding season
br15=read.csv("br15_glm.csv")
head(br15)
tail(br15)

m1=glm(Sp..Rich~DEMmean+RGmean+C1.Water+C2.vegetation+C3.Built.up, family=poisson, data=br15)
summary(m1)

m2=update(m1, . ~ . - C2.vegetation)
summary(m2)
anova(m1,m2, test='Chisq')

m3=update(m2, . ~ . - C1.Water)
summary(m3)
anova(m3,m2, test='Chisq')

m4=update(m3, . ~ . - RGmean)
summary(m4)
anova(m3,m4, test='Chisq')

br15$p=predict(m4, type="response")

plot(br15$C3.Built.up, br15$p, xlab = "% Built.up", ylab = "Predicted species richness")