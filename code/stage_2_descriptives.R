###############################################################################
## SCRIPT ID:
## Exploring the Rule of Law in Academic Discourse
## Bibliometric Description and Thematic Analysis with the Web of Science Data

## R version 4.5.0 (2022-10-11) -- "How About a Twenty-Six"
## Jaroslaw Kantorowicz and Bastián González-Bustamante
## Leiden University and Universidad Diego Portales

## Contact Email
## b.a.gonzalez.bustamante@fgga.leidenuniv.nl; bastian.gonzalez.b@mail.udp.cl
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
###############################################################################

## RELEVANT NOTE
## This file is under embargo because of copyright reasons, however, you can
## replace them with your own data and use the code

final_df <- pin_read(board_folder("../data-under-embargo/unravelling-rule-of-law/wos-data/"), "final_df")

###############################################################################
## 4. Main descriptive bibliometric analysis
###############################################################################

results_all <- biblioAnalysis(final_df, sep = ";")
options(width=100)

S1 <- summary(object = results_all, k = 30, pause = FALSE)
S1[[1]]
S1[[2]]
S1[[3]]
S1[[4]]
S1[[5]]
S1[[6]]
S1[[7]]
S1[[8]]
S1[[9]]
S1[[10]]

## Most important sources
most_prelevant_sources <- as.data.frame(S1[[9]])
write.csv(most_prelevant_sources, "results/most_prelevant_sources.csv")

## Most productive authors
most_productive_authors <- as.data.frame(S1[[5]])
write.csv(most_productive_authors, "results/most_productive_authors.csv")

## Most cited manuscripts
top_cited_manuscripts <- as.data.frame(S1[[6]])
write.csv(top_cited_manuscripts, "results/top_cited_manuscripts.csv")

## Most cited authors
top_cited_authors <- final_df %>% 
  dplyr::select(AU, PY, TC) %>%
  dplyr::mutate(yearly_citation=TC/(2021-PY)) %>% 
  separate_rows(AU, sep=";", convert = TRUE)

top_cited_authors_grouped <- top_cited_authors %>% 
  dplyr::group_by(AU) %>% 
  dplyr::summarise(sum_total_cit=sum(TC), sum_avg_cit=sum(yearly_citation))

top_cited_authors_grouped_arranged_tot <- top_cited_authors_grouped %>%
  dplyr::arrange(-sum_total_cit)

write.csv(top_cited_authors_grouped_arranged_tot, "results/top_cited_authors_total.csv")

###############################################################################
## 5. Analysis of cited references
###############################################################################

## Cited authors
CR_author <- citations(final_df, field = "author", sep = ";")
cbind(CR_author$Cited[1:1000])
AU_1000 <- data.frame(names = row.names(cbind(CR_author$Cited[1:1000])), Citations = cbind(CR_author$Cited[1:1000])[,1])
write_csv(AU_1000, "results/cited_authors.csv")

## Cited articles
CR_article <- citations(final_df, field = "article", sep = ";")
cbind(CR_article$Cited[1:30])
CR_5000 <- data.frame(NO = 1:30000,CR = row.names(cbind(CR_article$Cited[1:30000])))
write_csv(CR_5000, "results/cited_references.csv")

## Where the knowledge is produced
## results_all <- biblioAnalysis(final_df, sep = ";")
## options(width=100)
S2 <- summary(object = results_all, k = 200, pause = FALSE)

most_productive_countries <- S2$MostProdCountries
most_productive_countries$Freq <- as.numeric(most_productive_countries$Freq) 
most_productive_countries <- most_productive_countries %>% arrange(-Freq)
most_productive_countries <- most_productive_countries[c(1:20),]
most_productive_countries <- most_productive_countries %>% select(Country, SCP, MCP)
most_productive_countries$SCP <- as.numeric(most_productive_countries$SCP)
most_productive_countries$MCP <- as.numeric(most_productive_countries$MCP)
most_productive_countries_long <- most_productive_countries %>% pivot_longer(!Country, names_to = "Collaboration", values_to = "Count")
most_productive_countries_long$Count <- as.numeric(most_productive_countries_long$Count)
most_productive_countries_long$Collaboration <- as.factor(most_productive_countries_long$Collaboration)
most_productive_countries_long$Country <- as.factor(most_productive_countries_long$Country)
most_productive_countries_long <- most_productive_countries_long %>% mutate(ID = row_number())

countries <- ggplot(most_productive_countries_long) +
  geom_bar(aes(x = reorder(Country, desc(ID)), y = Count, fill = Collaboration), position = "stack", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of documents") +
  theme_bw() +
  scale_fill_manual(values=c("black", "#999999"))

ggsave(countries, file = "results/countries.jpg",
       height = 6, width = 10, scale = 0.8)
