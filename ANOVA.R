data("warpbreaks")
help("warpbreaks")
head(warpbreaks)
summary(warpbreaks)

#one-way-anova
model1 <- aov(breaks~wool, data=warpbreaks)
summary(model1)
plot(model1)

#two-way anova
model2 <- aov(breaks~wool+tension+wool:tension, data=warpbreaks)
summary(model2)
plot(model2)
