library(dplyr)
library(tibble)
library(DescTools)
# library(purrr)
library(ggplot2)
library(patchwork)
dir.create("output")
rm(list = ls())
source("../../function/function.R")

##meta
shotgun_phyloseq <- readRDS("../../../Analysis_shotgun_PRJDB4176/input_raw/Kraken2_shotgun_phyloseq.rds")
meta_all<-read.csv("../../../Analysis_shotgun_PRJDB4176/input_raw/meta_PRJDB4176.tsv") %>% data.frame()
meta_all$host_disease_stat= make.names(meta_all$host_disease_stat)
meta_all$host_disease_stat=gsub("CRC..","",meta_all$host_disease_stat)
meta_all$Group<-meta_all$host_disease_stat
# meta_all$Age<-meta_all$Age_at_collection
rownames(meta_all)<-meta_all$Run

sample_names(shotgun_phyloseq) =gsub(".bracken.mpa.report.*","",sample.names(shotgun_phyloseq))
sample_data(shotgun_phyloseq)<- meta_all %>% dplyr::filter(Group != "X")

shotgun_phyloseq_core <- microbiome::core(shotgun_phyloseq, detection = 5, prevalence = .2)
Meta=meta(shotgun_phyloseq_core)

## ML
shotgun_phyloseq_core_subset=shotgun_phyloseq_core
# 查看新的 phyloseq 对象
shotgun_phyloseq_core_subset
shotgun_phyloseq_core_subset.tss.level <- microbiome::aggregate_taxa(shotgun_phyloseq_core_subset,level = "Genus") %>%microbiome::transform(transform = "compositional") #Genus Species
OTU_table<-otu_table(shotgun_phyloseq_core_subset.tss.level) %>% t() %>% data.frame()
meta_phy=sample_data(shotgun_phyloseq_core_subset) %>% data.frame()  %>% dplyr::select(Group)
OTU_table_Group<-merge(meta_phy,OTU_table,by=0 ) %>% column_to_rownames("Row.names")

##STAMP
colnames(OTU_table_Group)[1]="Groups"
Meta_data=OTU_table_Group
CLass="PRJDB4176"
pathway_pdf(Meta_data=Meta_data,meta_col=c("Groups"),out.name= glue::glue("class_" ,CLass %>% make.names(),"_"),TopN=30,height=6,pvalue=0.05,width=6)



