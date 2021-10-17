## ---- load_packages_functions_data ----
library(igraph)
library(lsr)
library(plyr)
library(reshape2)
library(ggplot2)

# Turn off scientific notation
options(scipen = 999)

# Data
tweets <- read.csv("./data/tweets.csv")

# Make tweets safe for knitting into LaTeX
tweets$texte <- gsub("\\", "\\textebackslash", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("#", "\\#", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("$", "\\$", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("%", "\\%", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("^", "\\^", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("&", "\\&", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("_", "\\_", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("{", "\\{", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("}", "\\}", tweets$texte, fixed = TRUE)
tweets$texte <- gsub("~", "\\~", tweets$texte, fixed = TRUE)

# Collapse spelling variants
tweets$lol <- gsub("(?i).*(l+(o|e|u|aw)+l+)+.*", "lol", tweets$lol)
tweets$lol <- gsub("(?i).*(k+e+k+)+.*", "kek", tweets$lol)
tweets$lol <- gsub("(?i)l+m+a+o+2?", "lmao", tweets$lol)
tweets$lol <- gsub("(?i)#?l+m+a?f+a+o+ ?", "lmfao", tweets$lol)
tweets$lol <- gsub("(?i)r+o+f+l+", "rofl", tweets$lol)
tweets$lol <- gsub("(?i)r+o+t?f+l+m+a+o+", "roflmao", tweets$lol)
tweets$lol <- gsub("(?i).*m+d+r+.*", "mdr", tweets$lol)
tweets$lol <- gsub("#MortDeRire", "mdr", tweets$lol)
tweets$lol <- gsub("(?i)p+t+d+r+", "ptdr", tweets$lol)

# Collapse languages
tweets$langue.moi <- gsub(".*francais.*", "French", tweets$langue.moi)
tweets$langue.moi <- gsub(".*anglais.*", "English", tweets$langue.moi)
tweets$langue.moi <- gsub("(allemand|arabe|indefini)", "Other", tweets$langue.moi, perl = TRUE)

# Translate province names to English
tweets$province <- gsub("Nouveau-Brunswick", "New Brunswick", tweets$province)
tweets$province <- gsub("Nouvelle-Ecosse", "Nova Scotia", tweets$province)

# Some frequency tables for (lol)
lolByCommunity <- table(tweets[grep("French", tweets$langue.moi), "communaute"],
                        tweets[grep("French", tweets$langue.moi), "lol"])

lolByProvince <- table(tweets[grepl("French", tweets$langue.moi) & tweets$province != "indefini", "province"],
                       tweets[grepl("French", tweets$langue.moi) & tweets$province != "indefini", "lol"])

lolByCity <- table(tweets[grepl("French", tweets$langue.moi) & tweets$ville != "indefini", "ville"],
                   tweets[grepl("French", tweets$langue.moi) & tweets$ville != "indefini", "lol"])

# Cross-tabulation for community by province
commByProvince <- table(tweets$communaute, tweets$province)

## Functions
# Create a piechart for the languages of tweets in a community
pieLanguage <- function(community, colors) {
  thisPie <- tweets[tweets$communaute == community,]
  ggplot(thisPie, aes(x = factor(1), fill = langue.moi)) +
    geom_bar(width = 1) +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(text = element_text(size = 24)) +
    labs(fill = "Language")  
}

## Graphs
# Community by province bar plot
barCommByProv <- ggplot(melt(table(tweets[tweets$province == "New Brunswick" | tweets$province == "Nova Scotia",]$province, tweets[tweets$province == "New Brunswick" | tweets$province == "Nova Scotia",]$communaute)),
       aes(x = as.factor(Var2), y = value, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#1E88E5", "#D81B60")) +
  theme_bw() +
  theme(text = element_text(size = 24)) +
  labs(fill = "Province") +
  xlab("Community") +
  ylab("Tweets")