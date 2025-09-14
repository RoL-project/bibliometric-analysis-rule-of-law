###############################################################################
## SCRIPT ID:
## Exploring the Rule of Law in Academic Discourse
## Bibliometric Description and Thematic Analysis with the Web of Science Data

## R version 4.5.0 (2022-10-11) -- "How About a Twenty-Six"
## Jaroslaw Kantorowicz and Bastián González-Bustamante
## Leiden University and Universidad Diego Portales
###############################################################################

###############################################################################
## 1. Clean environment
###############################################################################

rm(list = ls())

###############################################################################
## 2. Load packages
###############################################################################

## RELEVANT NOTE
## These are the packages necessary for all scripts

library(bibliometrix)
library(tidyverse)
library(stm)
library(tm)
library(quanteda)
library(ggsci)
library(sjPlot)
library(ggpubr)
library(grid)
library(pins)

###############################################################################
## 3. Import data

## RELEVANT NOTE
## These files are under embargo because of copyright reasons, however, you can
## replace them with your own data and use the code
###############################################################################

collection_wos <- c("../data-under-embargo/unravelling-rule-of-law/wos-data/rl1.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl2.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl3.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl4.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl5.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl6.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl7.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl8.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl9.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl10.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl11.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl12.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl13.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl14.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl15.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl16.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl17.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl18.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl19.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl20.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl21.txt",
                    "../data-under-embargo/unravelling-rule-of-law/wos-data/rl22.txt")

## Converting WoS collection into a bibliographic dataframe
preliminary_df <- convert2df(collection_wos, dbsource = "wos", format = "plaintext")

## Check
table(preliminary_df$DT)

###############################################################################
## 4. Preprocessing
###############################################################################

## Select types of documents
final_df <- preliminary_df[which(preliminary_df$DT=="ARTICLE" 
                                 | preliminary_df$DT=="ARTICLE; PROCEEDINGS PAPER" 
                                 | preliminary_df$DT=="REVIEW" 
                                 | preliminary_df$DT=="ARTICLE; BOOK CHAPTER" 
                                 | preliminary_df$DT=="REVIEW; BOOK CHAPTER" 
                                 | preliminary_df$DT=="ARTICLE; DATA PAPER" 
                                 | preliminary_df$DT=="ARTICLE; EARLY ACCESS" 
                                 | preliminary_df$DT=="BOOK REVIEW" 
                                 | preliminary_df$DT=="BOOK REVIEW; EARLY ACCESS" 
                                 | preliminary_df$DT=="EDITORIAL MATERIAL" 
                                 | preliminary_df$DT=="EDITORIAL MATERIAL; EARLY ACCESS" 
                                 | preliminary_df$DT=="LETTER" 
                                 | preliminary_df$DT=="REVIEW" 
                                 | preliminary_df$DT=="REVIEW; EARLY ACCESS"),]

## Cleaning function
source("code/functions/clean_authors.R")
final_df <- clean_author_names(final_df)

## Create additional variables
final_df <- metaTagExtraction(final_df, Field = "CR_AU", sep = ";")
final_df <- metaTagExtraction(final_df, Field = "CR_SO", sep = ";")
final_df <- metaTagExtraction(final_df, Field = "AU_CO", sep = ";")
final_df <- metaTagExtraction(final_df, Field = "AU1_CO", sep = ";")
final_df <- metaTagExtraction(final_df, Field = "AU_UN", sep = ";")
final_df <- metaTagExtraction(final_df, Field = "SR", sep = ";")

## Check
table(final_df$DT)

## Save data (under embargo)
pin_write(board_folder("../data-under-embargo/unravelling-rule-of-law/wos-data/"), final_df, name = "final_df", versioned = TRUE)
