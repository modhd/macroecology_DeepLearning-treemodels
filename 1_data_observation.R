#Data observation
#extract and count the 5 most frequent tree species
data <- read.csv("data/EUForestspecies.csv")

species <- as.data.frame(table(data$SPECIES.NAME))
head(species[order(species$Freq, decreasing = T), ], 5)

jhggj
