library(readxl)
IL_10_MREC <- read_excel("C:/Users/PETER/Desktop/Work/IL_10 MREC.xlsx")
View(IL_10_MREC)
library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Sex_Diet == "female/Control"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Sex_Diet) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Sex_Diet) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Sex_Diet, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Sex_Diet = names(letters$Sex_Diet$Letters), Letters = letters$Sex_Diet$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Sex_Diet")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Sex_Diet, y = mean_RQ, fill = Sex_Diet)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  geom_text(aes(label = Letters, y = mean_RQ + se_RQ + 0.1), size = 6) +
  labs(x = "Sex_Diet", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Interaction of Sex and Diet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot



library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Breed_Diet == "F_Alpha/Control"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Breed_Diet) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Breed_Diet) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Breed_Diet, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Breed_Diet = names(letters$Breed_Diet$Letters), Letters = letters$Breed_Diet$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Breed_Diet")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Breed_Diet, y = mean_RQ, fill = Breed_Diet)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  geom_text(aes(label = Letters, y = mean_RQ + se_RQ + 0.1), size = 6) +
  labs(x = "Breed_Diet", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Interaction of Breed and Diet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot



library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Breed_Sex == "F_Alpha/female"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Breed_Sex) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Breed_Sex) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Breed_Sex, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Breed_Sex = names(letters$Breed_Sex$Letters), Letters = letters$Breed_Sex$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Breed_Sex")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Breed_Sex, y = mean_RQ, fill = Breed_Sex)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  labs(x = "Breed_Sex", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Interaction of Breed and Sex") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot



library(readxl)
IL_10_MREC <- read_excel("C:/Users/PETER/Desktop/Work/IL_10 MREC.xlsx")
View(IL_10_MREC)
library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Breed_Sex_Diet == "F_Alpha/female/Control"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Breed_Sex_Diet) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Breed_Sex_Diet) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Breed_Sex_Diet, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Breed_Sex_Diet = names(letters$Breed_Sex_Diet$Letters), Letters = letters$Breed_Sex_Diet$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Breed_Sex_Diet")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Breed_Sex_Diet, y = mean_RQ, fill = Breed_Sex_Diet)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  geom_text(aes(label = Letters, y = mean_RQ + se_RQ + 0.1), size = 6) +
  labs(x = "Breed_Sex_Diet", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Interaction of Breed and Sex and Diet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot



library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Breed == "F_Alpha"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Breed) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Breed) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Breed, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Breed = names(letters$Breed$Letters), Letters = letters$Breed$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Breed")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Breed, y = mean_RQ, fill = Breed)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  labs(x = "Breed", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Breed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot 

View(data)
library(rstatix)
group_stats <- data |> group_by(Breed) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Breed) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Breed, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Breed = names(letters$Breed$Letters), Letters = letters$Breed$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Breed")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Breed, y = mean_RQ, fill = Breed)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  geom_text(aes(label = Letters, y = mean_RQ + se_RQ + 0.1), size = 6) +
  labs(x = "Breed", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Breed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot


library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Sex == "female"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Sex) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Sex) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Sex, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Sex = names(letters$Sex$Letters), Letters = letters$Sex$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Sex")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Sex, y = mean_RQ, fill = Sex)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  labs(x = "Sex", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Sex") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot


library(dplyr)
data <- IL_10_MREC |>
  mutate(DeltaCt = Ct_target -Ct_reference)  
ctrl_mean3 <- mean(data$DeltaCt[data$Diet == "Control"])
data <- data |>
  mutate(DeltaDeltaCt = DeltaCt - ctrl_mean3, RQ = 2^(-DeltaDeltaCt))
print(data)

View(data)
library(rstatix)
group_stats <- data |> group_by(Diet) |>
  summarise(mean_dCt = mean(DeltaCt), sd_dCt = sd(DeltaCt), mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n())
print(group_stats)

group_summary <- data |> group_by(Diet) |>
  summarise(mean_RQ = mean(RQ), sd_RQ = sd(RQ), n = n()) |>
  mutate(se_RQ = sd_RQ/sqrt(n))
anova_result <- aov(DeltaCt ~ Diet, data = data)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
tukey

install.packages("multcompView")
library(multcompView)
letters <- multcompLetters4(anova_result, tukey)
letters_df <- data.frame(Diet = names(letters$Diet$Letters), Letters = letters$Diet$Letters)
group_summary <- group_summary |>
  left_join(letters_df, by = "Diet")
library(ggplot2)
plot <- ggplot(group_summary, aes(x = Diet, y = mean_RQ, fill = Diet)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_errorbar(aes(ymin = mean_RQ - se_RQ, ymax = mean_RQ +se_RQ), width = 0.2) +
  geom_text(aes(label = Letters, y = mean_RQ + se_RQ + 0.1), size = 6) +
  labs(x = "Diet", y = "Relative Expression(2^-DDCt)", title = "IL_10 Gene Expression by Diet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 14), legend.position = "none")
plot
