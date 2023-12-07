##based on the model established in RF_model (bweek_train.forest)
##factor importance
importance_bweek<- data.frame(importance(bweek_train.forest), check.names = FALSE)
head(importance_bweek)

##use rfpermut() for factor importance analysis
#here we use Hos1w as ana example for the targets
library(rfPermute)
set.seed(123)
bweek_train<-bweek_train[,1:56]
bweek_rfP <- rfPermute(follw7~., data = bweek_train, importance = TRUE, ntree = 300, mtry=15, nrep = 1000, num.cores = 7)
bweek_rfP

importance_bweek.scale <- data.frame(importance(bweek_rfP, scale = TRUE), check.names = FALSE)
importance_bweek.scale
write.csv(importance_bweek.scale, "f7_importance.csv")

#visualization
library(ggplot2)

importance_bweek.scale$factor <- rownames(importance_bweek.scale)
dev.new()

ggplot(importance_bweek.scale, aes(x = factor(factor, levels = rev(factor)), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = %IncMSE.pval), hjust = -0.3)+
  theme_minimal() +coord_flip()+
  theme(axis.text.x = element_text(hjust = 1, vjust = 1), axis.text.y = element_text(face = "bold")) +
  labs(x = "Variables", y = "%IncMSE") +
  ggtitle("Bar chart of factor and %IncMSE")
