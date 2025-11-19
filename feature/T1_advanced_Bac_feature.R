#t1_advanced_Bac
#Bac
library(randomForest)
library(dplyr)
t1_advanced_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/t1_advanced.rds")
t1_advanced_info<-t1_advanced_info[,c("sample","group")]
t1_advanced_bac<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/t1_advanced_group_all_bac.rds")


bac <- t1_advanced_bac
rownames(bac) <- bac$Taxonomy
bac$Taxonomy <- NULL

bac_t <- as.data.frame(t(bac))
bac_t$sample <- rownames(bac_t)
df <- t1_advanced_info %>%
  left_join(bac_t, by = "sample")
df$group <- factor(df$group)
df_model <- df %>% select(-sample)

set.seed(123)
rf <- randomForest(group ~ ., 
                   data = df_model,
                   importance = TRUE,
                   ntree = 1000) 
imp <- importance(rf)
imp_order <- imp[order(imp[,"MeanDecreaseAccuracy"], decreasing = TRUE), ]
top <- rownames(imp_order)[1:7]
bac_sub<-df[,c("sample","group",top)]
saveRDS(bac_sub,"/home/zyt/16s_JiangXi/model_R/T1_T234/data/bacteria.rds")





