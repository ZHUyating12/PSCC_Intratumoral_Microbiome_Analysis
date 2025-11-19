#T1_advanced_Bac_Fungi
library(randomForest)
library(dplyr)
t1_advanced_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/t1_advanced.rds")
t1_advanced_info<-t1_advanced_info[,c("sample","group")]
t1_advanced_bac_fungi<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/t1_advanced_group_all_bac_fungi_merge.rds")

bac_fungi <- t1_advanced_bac_fungi
rownames(bac_fungi) <- bac_fungi$Taxonomy
bac_fungi$Taxonomy <- NULL

bac_fungi_t <- as.data.frame(t(bac_fungi))
bac_fungi_t$sample <- rownames(bac_fungi_t)
df <- t1_advanced_info %>%
  left_join(bac_fungi_t, by = "sample")
df$group <- factor(df$group)
df_model <- df %>% select(-sample)

set.seed(123)
rf <- randomForest(group ~ ., 
                   data = df_model,
                   importance = TRUE,
                   ntree = 1000) 
imp <- importance(rf)
imp_order <- imp[order(imp[,"MeanDecreaseAccuracy"], decreasing = TRUE), ]
top <- rownames(imp_order)[1:22]
bac_fungi_sub<-df[,c("sample","group",top)]
saveRDS(bac_fungi_sub,"/home/zyt/16s_JiangXi/model_R/T1_T234/data/fungi_bacteria.rds")