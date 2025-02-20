library(ggplot2)

trees <- read.csv("files/trees.csv")

# Correlación dbh ~ height
ggplot(data = trees)+
  geom_point(mapping = aes(x=dbh, y=height, colour = sex))


# histograma distibución dbh separado por sexo
ggplot(data = trees)+
  geom_histogram(aes(dbh), colour = "black", fill= "darkgreen")+
  facet_wrap(~sex) +
  theme_minimal()
  
  