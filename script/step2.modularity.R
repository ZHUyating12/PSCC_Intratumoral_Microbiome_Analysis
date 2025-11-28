#!/TJPROJ5/META_ASS/meta/sunhongtao/script/miniforge3/envs/R/bin/Rscript
#setwd("E:\\personal_demand\\nature-network\\X101SC24040447-Z01-F002\\HPVneg")

rm(list=ls())
require(optparse)
option_list <- list(
  make_option(c("-i", "--cor"), type = "character",help = "cor_filter.csv"),
  make_option(c("-o", "--output"), type = "character",help = "./")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

library(igraph)
require(magrittr)
require(dplyr)

#setwd(opt$output)

#获取边文件
edge <- read.csv(opt$cor) %>% 
  select(.,all_of(c("Source","Target","coefficient","inter_type","connection_type"))) %>% 
  mutate(Source_label = Source)

#获取点文件
cor_tax <- unique(c(edge$Source,edge$Target))

#导入igraph
net <- graph_from_data_frame(d=edge, vertices=cor_tax, directed=F)

#cluster_louvain进行模块化
community <- cluster_louvain(net)
#把社区编号导入网络图中
V(net)$community <- membership(community)

# 转换为数据框
community_df <- data.frame(
  Species = names(V(net)),
  Community  = as.character(V(net)$community) 
)

write.table(community_df,opt$output,sep = "\t",row.names = F,quote = F)
