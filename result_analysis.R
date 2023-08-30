# load data
res <- read.csv("results/results2500imgs_p_spec_32batch_6epoch.csv")
summary(res)

# accuracy analysis
# training vs. test
res$training <- as.logical(res$training)
res_training <- res[res$training, ]
summary(res_training)
res_test <- res[! res$training, ]
acc_test <- 
  nrow(res_test[res_test$SPECIES.NAME == res_test$prediction, ]) / nrow(res_test)
acc_test
acc_training <- nrow(res_training[res_training$SPECIES.NAME == res_training$prediction, ]) / nrow(res_training)
acc_training
# per spec
# test
res_test_betpub <- res_test[res_test$SPECIES.NAME == "b'Betula pubescens'", ]
res_test_picabi <- res_test[res_test$SPECIES.NAME == "b'Picea abies'", ]
res_test_fagsyl <- res_test[res_test$SPECIES.NAME == "b'Fagus sylvatica'", ]
res_test_querob <- res_test[res_test$SPECIES.NAME == "b'Quercus robur'", ]
res_test_pinsyl <- res_test[res_test$SPECIES.NAME == "b'Pinus sylvestris'", ]

acc_test_betpub <- 
  nrow(res_test_betpub[res_test_betpub$SPECIES.NAME == res_test_betpub$prediction, ]) / nrow(res_test_betpub)
acc_test_picabi <- 
  nrow(res_test_picabi[res_test_picabi$SPECIES.NAME == res_test_picabi$prediction, ]) / nrow(res_test_picabi)
acc_test_fagsyl <- 
  nrow(res_test_fagsyl[res_test_fagsyl$SPECIES.NAME == res_test_fagsyl$prediction, ]) / nrow(res_test_fagsyl)
acc_test_querob <- 
  nrow(res_test_querob[res_test_querob$SPECIES.NAME == res_test_querob$prediction, ]) / nrow(res_test_querob)
acc_test_pinsyl <- 
  nrow(res_test_pinsyl[res_test_pinsyl$SPECIES.NAME == res_test_pinsyl$prediction, ]) / nrow(res_test_pinsyl)

acc_test_per_spec <- c(acc_test_betpub, acc_test_picabi, acc_test_fagsyl, acc_test_querob, acc_test_pinsyl)
acc_test_per_spec

# training
res_training_betpub <- 
  res_training[res_training$SPECIES.NAME == "b'Betula pubescens'", ]
res_training_picabi <- 
  res_training[res_training$SPECIES.NAME == "b'Picea abies'", ]
res_training_fagsyl <- 
  res_training[res_training$SPECIES.NAME == "b'Fagus sylvatica'", ]
res_training_querob <- 
  res_training[res_training$SPECIES.NAME == "b'Quercus robur'", ]
res_training_pinsyl <- 
  res_training[res_training$SPECIES.NAME == "b'Pinus sylvestris'", ]

acc_training_betpub <- 
  nrow(res_training_betpub[res_training_betpub$SPECIES.NAME == res_training_betpub$prediction, ]) / nrow(res_training_betpub)
acc_training_picabi <- 
  nrow(res_training_picabi[res_training_picabi$SPECIES.NAME == res_training_picabi$prediction, ]) / nrow(res_training_picabi)
acc_training_fagsyl <- 
  nrow(res_training_fagsyl[res_training_fagsyl$SPECIES.NAME == res_training_fagsyl$prediction, ]) / nrow(res_training_fagsyl)
acc_training_querob <- 
  nrow(res_training_querob[res_training_querob$SPECIES.NAME == res_training_querob$prediction, ]) / nrow(res_training_querob)
acc_training_pinsyl <- 
  nrow(res_training_pinsyl[res_training_pinsyl$SPECIES.NAME == res_training_pinsyl$prediction, ]) / nrow(res_training_pinsyl)

acc_training_per_spec <- c(acc_training_betpub, acc_training_picabi, acc_training_fagsyl, acc_training_querob, acc_training_pinsyl)
acc_training_per_spec

# accumulate results
spec = c("betpub", "picabi", "fagsyl", "querob", "pinsyl", "all")
result_analysis <- data.frame(spec = rep(spec, 2),
                              set = c(rep("train", 6), rep("test", 6)),
                              acc = c(acc_training_per_spec, acc_training, acc_test_per_spec, acc_test))
write.csv(result_analysis, "results/prediction_analysis")
