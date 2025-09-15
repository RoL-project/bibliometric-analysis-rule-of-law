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
###############################################################################

## RELEVANT NOTE
## This file is under embargo because of copyright reasons, however, you can
## replace them with your own data and use the code

final_df <- pin_read(board_folder("../data-under-embargo/unravelling-rule-of-law/wos-data/"), "final_df")

###############################################################################
## 4. Topic modelling
###############################################################################

## Preprocessing
final_df <- final_df %>% mutate(ID = row_number())
final_df <- final_df %>% mutate(TIME = ifelse(PY>=1950 & PY<2010, "<2010",
                                              ifelse(PY >= 2010 & PY <= 2024, "2010-2024", NA)))
final_df$TIME <- factor(final_df$TIME, levels = c("<2010", "2010-2024"))

final_df$Legal <- str_detect(final_df$SC, pattern = "GOVERNMENT & LAW")
final_df$Philo <- str_detect(final_df$SC, pattern = "PHILOSOPHY")
final_df$Econ <- str_detect(final_df$SC, pattern = "BUSINESS & ECONOMICS")

final_df <- final_df %>% mutate(LAW = ifelse(Legal == TRUE | Philo == TRUE, "Law & Philosophy", "Other"))
final_df$LAW <- factor(final_df$LAW, levels = c("Law & Philosophy", "Other"))
final_df <- final_df %>% drop_na(LAW)
df_lda <- final_df %>% filter(AB != "") 

## Text preprocessing
out <- textProcessor(documents = df_lda$AB, metadata = df_lda)
temp <- prepDocuments(out$documents, out$vocab, out$meta, lower.thresh = 5)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

## Optimal number of topics
storage_final <- searchK(docs, vocab, 
                         prevalence = ~ LAW * TIME,
                         K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                         data = meta,
                         seed = 1000,
                         init.type = "Spectral")

plot(storage_final)

df_top <- tibble(topics = storage_final[["results"]][["K"]],
                 exclusivity = storage_final[["results"]][["exclus"]],
                 coherence = storage_final[["results"]][["semcoh"]])
df_top$topics <- as.numeric(df_top$topics)
df_top$topics <- as.factor(df_top$topics)
df_top$topics_name <- as.character(df_top$topics)
df_top$exclusivity <- as.numeric(df_top$exclusivity)
df_top$coherence <- as.numeric(df_top$coherence)

## Plot
ggplot(df_top,
       aes(x = coherence, y = exclusivity, color = topics, label = topics_name)) + 
  geom_point() +
  geom_text(hjust=-0.5, vjust=0.5, color = "black") +
  theme_bw() +
  scale_color_uchicago() +
  xlab("Semantic coherence") +
  ylab("Exclusivity") +
  theme(legend.position="none") +
  geom_curve(aes(x = -95, y = 9.3, xend = -86.5, yend = 9.45), 
             curvature = 0.2,
             size = 0.1,
             arrow = arrow(length = unit(0.2, "cm")),
             linetype=7,
             color = "black") +
  geom_text(aes(x = -103, y = 9.3), label = "Preferred model", color = "black", size = 4)

ggsave(file = "results/coherence_vs_exclusivity_new.jpg", scale = 0.8, width = 8, height = 7)

###############################################################################
## 5. Main topic model
###############################################################################

topic_model <- stm(docs, vocab, 15, prevalence = ~ LAW * TIME, data = meta, seed = 1000, init.type = "Spectral")

### Assign topics corpus
topics_documents <- cbind(df_lda, topic_model[["theta"]])
topics_documents$ID <- as.character(topics_documents$ID)
topics_20_prob <- data.frame(t(labelTopics(topic_model, n = 20)$prob))
topics_20_frex <- data.frame(t(labelTopics(topic_model, n = 20)$frex))
write.csv(topics_20_prob, file = "results/topics_20_prob.csv")
write.csv(topics_20_frex, file = "results/topics_20_frex.csv")

topic_df <- labelTopics(topic_model, n = 20)
topic_df[["prob"]]
topic_dataframe <- data.frame(topic_df[["frex"]])
dataset_topics <- data.frame(No = 1:15, FREX = paste(topic_dataframe$X1, 
                                                     topic_dataframe$X2,
                                                     topic_dataframe$X3,
                                                     topic_dataframe$X4,
                                                     topic_dataframe$X5,
                                                     topic_dataframe$X6,
                                                     topic_dataframe$X7,
                                                     topic_dataframe$X8,
                                                     topic_dataframe$X9,
                                                     topic_dataframe$X10,
                                                     topic_dataframe$X11, 
                                                     topic_dataframe$X12,
                                                     topic_dataframe$X13,
                                                     topic_dataframe$X14,
                                                     topic_dataframe$X15,
                                                     topic_dataframe$X16,
                                                     topic_dataframe$X17,
                                                     topic_dataframe$X18,
                                                     topic_dataframe$X19,
                                                     topic_dataframe$X20,
                                                     sep = ", ", 
                                                     collapse=NULL))
write.csv(dataset_topics, "results/topics_frex.csv")

labels = c("Development and reforms in China", ## 1
           "European context", ## 2
           "Research approaches", ## 3
           "Crime and punishment", ## 4 
           "Theory and normative approaches", ## 5
           "Democratic backsliding", ## 6
           "Social values", ## 7 
           "Constitution-making context", ## 8 
           "Economic growth aspects", ## 9 
           "Critical approaches", ## 10
           "Fundamental rights", ## 11
           "Judicial aspects", ## 12
           "Trade and investment", ## 13
           "Security and conflict", ## 14
           "Public administration") ## 15


d <- labelTopics(topic_model)

plot.STM(topic_model, 
         type="summary", 
         main="", 
         custom.labels = labels)


mean_values <- c(mean(topic_model[["theta"]][,1]),
                 mean(topic_model[["theta"]][,2]),
                 mean(topic_model[["theta"]][,3]),
                 mean(topic_model[["theta"]][,4]),
                 mean(topic_model[["theta"]][,5]),
                 mean(topic_model[["theta"]][,6]),
                 mean(topic_model[["theta"]][,7]),
                 mean(topic_model[["theta"]][,8]),
                 mean(topic_model[["theta"]][,9]),
                 mean(topic_model[["theta"]][,10]),
                 mean(topic_model[["theta"]][,11]),
                 mean(topic_model[["theta"]][,12]),
                 mean(topic_model[["theta"]][,13]),
                 mean(topic_model[["theta"]][,14]),
                 mean(topic_model[["theta"]][,15]))

topic_proportions <- data.frame(Labels = labels, Proportions = mean_values)

write.csv(topic_proportions, "results/topic_proportions.csv")

topic_proportions <- topic_proportions %>%
  arrange(desc(Proportions))

## Creating a horizontal lollipop chart
ggplot(topic_proportions, aes(x = reorder(Labels, Proportions), y = Proportions)) +
  geom_segment(aes(xend = Labels, yend = 0), color = "black") +
  geom_point(size = 3, color = "black") +
  coord_flip() +  
  labs(x = "",
       y = "Topic proportions") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

topic_proportions_figure <- ggplot(topic_proportions, aes(x = reorder(Labels, Proportions), y = Proportions)) +
  geom_segment(aes(xend = Labels, yend = 0), color = "black") +
  geom_point(size = 3, color = "black") +
  coord_flip() +  
  labs(x = "",
       y = "Topic proportions") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(topic_proportions_figure, 
       file = "results/topic_proportions_figure.jpg",
       height = 6, 
       width = 6, 
       scale = 0.8)

topicCorr(topic_model, method = c("simple", "huge"), cutoff = 0.01, verbose = TRUE)

plot.topicCorr(topicCorr(topic_model, method = c("simple", "huge"), cutoff = 0.01, 
                         verbose = TRUE), custom.labels = labels)

prep <- estimateEffect(1:15 ~ LAW * TIME, 
                       topic_model, 
                       meta = meta, 
                       uncertainty = "Global", 
                       documents=docs)

###############################################################################
## 6. Regression analysis
###############################################################################

## 1. Development and reforms
summary(prep, topics = 1)

topic_1_dr <- data.frame(topic = topic_model[["theta"]][,1], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ TIME * LAW, data = topic_1_dr))

model_1 <- lm(formula = topic ~ TIME * LAW, data = topic_1_dr)

plot_1 <- plot_model(model_1, type = "int", colors = c("black", "grey")) + 
  ggtitle("Development and reforms in China") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 2. European contex
summary(prep, topics = 2)

topic_2_dr <- data.frame(topic = topic_model[["theta"]][,2], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_2_dr))

model_2 <- lm(formula = topic ~ TIME * LAW, data = topic_2_dr)

plot_2 <- plot_model(model_2, type = "int", colors = c("black", "grey")) + 
  ggtitle("European context") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))


## 3. Research methods
summary(prep, topics = 3)

topic_3_dr <- data.frame(topic = topic_model[["theta"]][,3], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_3_dr))

model_3 <- lm(formula = topic ~ TIME * LAW, data = topic_3_dr)

plot_3 <- plot_model(model_3, type = "int", colors = c("black", "grey")) + 
  ggtitle("Research approaches") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 4. Crime and punishment
summary(prep, topics = 4)

topic_4_dr <- data.frame(topic = topic_model[["theta"]][,4], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_4_dr))

model_4 <- lm(formula = topic ~ TIME * LAW, data = topic_4_dr)

plot_4 <- plot_model(model_4, type = "int", colors = c("black", "grey")) + 
  ggtitle("Crime and punishment") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 5. Theory and normative aspects
summary(prep, topics = 5)

topic_5_dr <- data.frame(topic = topic_model[["theta"]][,5], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_5_dr))

model_5 <- lm(formula = topic ~ TIME * LAW, data = topic_5_dr)

plot_5 <- plot_model(model_5, type = "int", colors = c("black", "grey")) + 
  ggtitle("Theory and normative approaches") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 6. Democratic backsliding
summary(prep, topics = 6)

topic_6_dr <- data.frame(topic = topic_model[["theta"]][,6], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_6_dr))

model_6 <- lm(formula = topic ~ TIME * LAW, data = topic_6_dr)

plot_6 <- plot_model(model_6, type = "int", colors = c("black", "grey")) + 
  ggtitle("Democratic backsliding") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 7. Social values
summary(prep, topics = 7)

topic_7_dr <- data.frame(topic = topic_model[["theta"]][,7], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_7_dr))

model_7 <- lm(formula = topic ~ TIME * LAW, data = topic_7_dr)

plot_7 <- plot_model(model_7, type = "int", colors = c("black", "grey")) + 
  ggtitle("Social values") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 8. Constitution-making
summary(prep, topics = 8)

topic_8_dr <- data.frame(topic = topic_model[["theta"]][,8], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_8_dr))

model_8 <- lm(formula = topic ~ TIME * LAW, data = topic_8_dr)

plot_8 <- plot_model(model_8, type = "int", colors = c("black", "grey")) + 
  ggtitle("Consitution-making aspects") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 9. Institutional and growth
summary(prep, topics = 9)

topic_9_dr <- data.frame(topic = topic_model[["theta"]][,9], 
                         LAW = prep[["data"]][,1], 
                         TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_9_dr))

model_9 <- lm(formula = topic ~ TIME * LAW, data = topic_9_dr)

plot_9 <- plot_model(model_9, type = "int", colors = c("black", "grey")) + 
  ggtitle("Economic growth aspects") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 10. Critical approaches
summary(prep, topics = 10)

topic_10_dr <- data.frame(topic = topic_model[["theta"]][,10], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_10_dr))

model_10 <- lm(formula = topic ~ TIME * LAW, data = topic_10_dr)

plot_10 <- plot_model(model_10, type = "int", colors = c("black", "grey")) + 
  ggtitle("Critical approaches") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 11. Fundamental rights
summary(prep, topics = 11)

topic_11_dr <- data.frame(topic = topic_model[["theta"]][,11], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_11_dr))

model_11 <- lm(formula = topic ~ TIME * LAW, data = topic_11_dr)

plot_11 <- plot_model(model_11, type = "int", colors = c("black", "grey")) + 
  ggtitle("Fundamental rights") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 12. Judiciary
summary(prep, topics = 12)

topic_12_dr <- data.frame(topic = topic_model[["theta"]][,12], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_12_dr))

model_12 <- lm(formula = topic ~ TIME * LAW, data = topic_12_dr)

plot_12 <- plot_model(model_12, type = "int", colors = c("black", "grey")) + 
  ggtitle("Judicial aspects") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 13. Trade and investment
summary(prep, topics = 13)

topic_13_dr <- data.frame(topic = topic_model[["theta"]][,13], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_13_dr))

model_13 <- lm(formula = topic ~ TIME * LAW, data = topic_13_dr)

plot_13 <- plot_model(model_13, type = "int", colors = c("black", "grey")) + 
  ggtitle("Trade and investment") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))


## 14. Security and conflict
summary(prep, topics = 14)

topic_14_dr <- data.frame(topic = topic_model[["theta"]][,14], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_14_dr))

model_14 <- lm(formula = topic ~ TIME * LAW, data = topic_14_dr)

plot_14 <- plot_model(model_14, type = "int", colors = c("black", "grey")) + 
  ggtitle("Security and conflict") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

## 15. Public administration
summary(prep, topics = 15)

topic_15_dr <- data.frame(topic = topic_model[["theta"]][,15], 
                          LAW = prep[["data"]][,1], 
                          TIME = prep[["data"]][,2])

summary(lm(formula = topic ~ LAW * TIME, data = topic_15_dr))

model_15 <- lm(formula = topic ~ TIME * LAW, data = topic_15_dr)

plot_15 <- plot_model(model_15, type = "int", colors = c("black", "grey")) + 
  ggtitle("Public administration") +
  theme_sjplot2() +
  theme(legend.position="right") +
  labs(y = "Topic proportion", 
       x = "Time period") +
  guides(color=guide_legend(title="Research profile")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_text(vjust = -0.5)) +
  theme(axis.title.y = element_text(vjust = 2.5))

figure_regression <- ggarrange(plot_1 + rremove("ylab") + rremove("xlab"), 
                               plot_2 + rremove("ylab") + rremove("xlab"),
                               plot_3 + rremove("ylab") + rremove("xlab"), 
                               plot_4 + rremove("ylab") + rremove("xlab"),
                               plot_5 + rremove("ylab") + rremove("xlab"), 
                               plot_6 + rremove("ylab") + rremove("xlab"),
                               plot_7 + rremove("ylab") + rremove("xlab"), 
                               plot_8 + rremove("ylab") + rremove("xlab"),
                               plot_9 + rremove("ylab") + rremove("xlab"), 
                               plot_10 + rremove("ylab") + rremove("xlab"), 
                               plot_11 + rremove("ylab") + rremove("xlab"), 
                               plot_12 + rremove("ylab") + rremove("xlab"),
                               plot_13 + rremove("ylab") + rremove("xlab"), 
                               plot_14 + rremove("ylab") + rremove("xlab"), 
                               plot_15 + rremove("ylab") + rremove("xlab"), 
                               ncol=3, nrow=5, 
                               common.legend = TRUE, 
                               legend="bottom", 
                               align = "hv",
                               labels = NULL)

annotate_figure(figure_regression, left = textGrob("Topic proportions", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

ggsave(file = "results/topic_regressions.jpg", height = 11, width = 10, scale = 1)
