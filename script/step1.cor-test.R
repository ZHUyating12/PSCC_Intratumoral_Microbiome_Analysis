#!/TJPROJ5/META_ASS/meta/sunhongtao/script/miniforge3/envs/R/bin/Rscript
#setwd("E:\\personal_demand\\nature-network\\X101SC24040447-Z01-F002")
rm(list=ls())
require(optparse)
option_list <- list(
  make_option(c("--data_combine"), type = "character", help = "16s-ITS-combine-top100.xls"),
  make_option(c("--group"), type = "character", help = "group.xls"),
  make_option(c("--pvalue"), type="numeric",help = "0.05"),
  make_option(c("--corvalue"), type="numeric",help = "0.4"),
  make_option(c("--output"), type = "character",help = "./")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
setwd(opt$output)

require(magrittr)
require(dplyr)
require(tidyverse)
library(psych)
library(tibble)
require(reshape2)

#处理传参
# group <- read.table("pTis.txt",sep = "\t",header = F) %>% setNames(c("rawname","data1","data2"))
# data_combine <- read.table("16s-ITS-combine-top100.xls",sep = "\t",header = T)%>% 
#   separate(Tax_detail,into = c("k","p","o","c","f","g","s"),sep = ";",remove = F)
# colnames(data_combine)[1] <- "Taxonomy"
# pvalue <- 0.05
# corvalue <- 0.4

group <- read.table(opt$group,sep = "\t",header = F) %>% setNames(c("rawname","data1","data2"))
data_combine <- read.table(opt$data_combine,sep = "\t",header = T)%>% 
  separate(Tax_detail,into = c("k","p","o","c","f","g","s"),sep = ";",remove = F)
colnames(data_combine)[1] <- "Taxonomy"
pvalue <- opt$pvalue
corvalue <- opt$corvalue
setwd(opt$output)


data.subset <- data_combine %>% 
  select(.,all_of(c("Taxonomy",group$rawname,"Tax_detail","k","p")))
tax_description <- select(data_combine,all_of(c("Taxonomy","k","p")))
write.table(tax_description,"tax_description.xls",sep = "\t",row.names = F,quote = F)

#===================计算data1和data2 top5的门===========================
df1 <- data.subset %>% 
  subset(.,k=="k__Bacteria") %>% 
  select(.,all_of(c("p",group$rawname))) %>% 
  melt(.,id.vars = "p",variable.name = "sample",value.name = "abundance") %>% 
  dcast(formula = p~sample,fun.aggregate = sum,value.var = "abundance") %>% 
  mutate(max = do.call(pmax, across(where(is.numeric)))) %>% 
  arrange(desc(max))

df1.top5phylum <- data.frame(df1.top5phylum=df1$p[1:5])
write.table(df1.top5phylum,"k__Bacteria.top5phylum.xls",sep = "\t",row.names = F,quote = F,col.names = F)

df2 <- data.subset %>% 
  subset(.,k=="k__Fungi") %>% 
  select(.,all_of(c("p",group$rawname))) %>% 
  melt(.,id.vars = "p",variable.name = "sample",value.name = "abundance") %>% 
  dcast(formula = p~sample,fun.aggregate = sum,value.var = "abundance") %>% 
  mutate(max = do.call(pmax, across(where(is.numeric)))) %>% 
  arrange(desc(max))

df2.top5phylum <- data.frame(df2.top5phylum=df2$p[1:5])
write.table(df2.top5phylum,"k__Fungi.top5phylum.xls",sep = "\t",row.names = F,quote = F,col.names = F)
#===================done===========================

#===================计算相关性===========================
mat <- data.subset %>% 
  select(.,-c("Tax_detail","k","p")) %>% 
  remove_rownames() %>% 
  column_to_rownames("Taxonomy") %>% 
  t()

occor <-corr.test(mat, mat,  method="spearman", adjust="fdr") 
cor <- occor$r %>% as.data.frame()
p <- occor$p %>% as.data.frame()
p.adj <- occor$p.adj %>% as.data.frame()

cor[upper.tri(cor,diag = T)] <- NA
p[upper.tri(p,diag = T)] <- NA
p.adj[upper.tri(p.adj,diag = T)] <- NA

#转长数据
cor_res <- cor %>% 
  rownames_to_column("Source") %>% 
  melt(.,id.vars = "Source",variable.name = "Target",value.name = "coefficient",na.rm = T) 
cor_res_filter <- cor_res %>% subset(.,abs(coefficient)>corvalue)

p_result <- p %>% 
  rownames_to_column("Source") %>% 
  melt(.,id.vars = "Source",variable.name = "Target",value.name = "p.value",na.rm = T) 
p_result_filter <- p_result %>% subset(.,p.value<pvalue)

p.adj_result <- p.adj %>% 
  rownames_to_column("Source") %>% 
  melt(.,id.vars = "Source",variable.name = "Target",value.name = "p.adj.value",na.rm = T)

#连接总的
cor_all <- inner_join(cor_res, p_result, by = c("Source", "Target")) %>% 
  #连接p.adj
  left_join(.,p.adj_result,by = c("Source", "Target")) %>% 
  #计算edge类型
  mutate(inter_type=ifelse(coefficient>0,"positive","negative"))%>% 
  #匹配源所在的界和门
  left_join(.,tax_description,by=c("Source"="Taxonomy"))%>%
  #改个名
  rename(.,c("Source_k"="k","Source_p"="p")) %>% 
  #匹配目标所在的界和门
  left_join(.,tax_description,by=c("Target"="Taxonomy")) %>% 
  #改个名
  rename(.,c("Target_k"="k","Target_p"="p")) %>% 
  #判断连接类型 细菌自连、真菌自连、细菌-真菌
  mutate(connection_type = 
           case_when( Source_k == "k__Bacteria" & Target_k == "k__Bacteria" ~ "Bacterial self-connection",
                      Source_k == "k__Fungi" & Target_k == "k__Fungi" ~ "Fungal self-connection",
                      TRUE ~ "BF interconnection"))

write.csv(cor_all,"cor_all.csv",row.names = F,quote = F)

#连接筛选后的
cor_filter <- inner_join(cor_res_filter, p_result_filter, by = c("Source", "Target")) %>% 
  left_join(.,p.adj_result,by = c("Source", "Target")) %>% 
  mutate(inter_type=ifelse(coefficient>0,"positive","negative")) %>% 
  left_join(.,tax_description,by=c("Source"="Taxonomy"))%>%
  rename(.,c("Source_k"="k","Source_p"="p")) %>% 
  left_join(.,tax_description,by=c("Target"="Taxonomy")) %>% 
  rename(.,c("Target_k"="k","Target_p"="p")) %>% 
  mutate(connection_type = 
           case_when( Source_k == "k__Bacteria" & Target_k == "k__Bacteria" ~ "Bacterial self-connection",
                      Source_k == "k__Fungi" & Target_k == "k__Fungi" ~ "Fungal self-connection",
                      TRUE ~ "BF interconnection"))
write.csv(cor_filter,"cor_filter.csv",row.names = F,quote = F)

#将cor_filter.csv导入gephi中，获取module.csv，gephi无法export，直接复制粘贴到csv表中即可
