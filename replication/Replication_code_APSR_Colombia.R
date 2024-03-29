#***************************************************************#
#                                                               #
#                    Replication file for:                      #
#                                                               #
#     "Political Responsiveness to Conflict Victims:            #
#   Evidence from a Countrywide Audit Experiment in Colombia"   #
#                                                               #
#                        Barceló, Joan                          #
#                     Vela Baron, Mauricio                      #
#          American Political Science Review (APSR)             #
#                 Last updated: 1 August 2022                   #
#                                                               #
#****************************************************************

#################
#               #
# Load packages #
#               #
#################

#install.packages("stargazer")
#library(stargazer)
# library(rstudioapi)
# library(foreign)
# library(dplyr)
# library(tidyverse)
# library(htmlTable)
# library(data.table)
# library(compareDF)
# library(reshape2)
# library(stargazer)
# library(psych)
# library(irr)
# library(SPEI)
# library(hrbrthemes)
# library(readxl)
# library(stargazer)
# library(readstata13)
# library(dotwhisker)
# library(ggpubr)
# library(broom)
# library(interplot)
# library(survival)

#################
#               #
#   Load data   #
#               #
#################

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

load("data.rda")


#####################################
#Table 2
####################################

table2 <- table(audit$depto)

#####################################
#Figure 3
####################################

#A Response Rate
audit_h1h2 <- audit[, c('victim_conflict', 'response', 'idp_status')]
audit_h1h2$victim_status <- ifelse(audit_h1h2$victim_conflict == 1, "Victim", "Non-victim")
audit_h1h2$idp_status_c <- ifelse(audit_h1h2$idp_status == 1, "IDP Victim", "Non-victim")
levels(audit_h1h2$victim_status) <- c("Non-victim", "Victim")
levels(audit_h1h2$idp_status_c) <- c("Non-victim", "IDP Victim")
audit_h1h2$victim_status <- relevel(as.factor(audit_h1h2$victim_status), ref = "Non-victim")
audit_h1h2$idp_status_c <- relevel(as.factor(audit_h1h2$idp_status_c), ref = "Non-victim")

df.summary <- audit_h1h2 %>%
  group_by(victim_status) %>%
  dplyr::summarise(
    sd = sd(response, na.rm = TRUE),
    response = mean(response),
    se = sd/sqrt(dplyr::n()))

t.test(audit[audit$victim_conflict == 0,]$response, audit[audit$victim_conflict == 1,]$response, alternative = "two.sided", var.equal = FALSE)
mean(audit[audit$victim_conflict == 1,]$response) - mean(audit[audit$victim_conflict == 0,]$response)
(mean(audit[audit$victim_conflict == 1,]$response) - mean(audit[audit$victim_conflict == 0,]$response))/mean(audit[audit$victim_conflict == 0,]$response)

figure3a <- ggplot(df.summary, aes(victim_status, response, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = response-se, ymax = response+se), width = 0.2) +
  labs(x = "", y = "Response rate") + 
  ylim(0, 0.45) + 
  annotate("text", label = expression(paste("Diff. = 5.1pp ( ", Delta, "16.6%)")), x = 1.5, y = 0.42, size = 5) + 
  annotate("text", label = expression(paste("p = .09")), x = 1.5, y = 0.40, size = 5) + 
  theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#B Friendly Response

audit_h1h2_f <- audit[, c('victim_conflict', 'friendliness', 'idp_status')]
audit_h1h2_f$victim_status <- ifelse(audit_h1h2_f$victim_conflict == 1, "Victim", "Non-victim")
audit_h1h2_f$idp_status_c <- ifelse(audit_h1h2_f$idp_status == 1, "IDP Victim", 
                                    ifelse(audit_h1h2_f$victim_conflict == 1, "Victim", 'Non-victim'
                                    ))
levels(audit_h1h2_f$victim_status) <- c("Non-victim", "Victim")
levels(audit_h1h2_f$idp_status_c) <- c("Non-victim", "Victim", "IDP Victim")
audit_h1h2_f$victim_status <- relevel(as.factor(audit_h1h2_f$victim_status), ref = "Non-victim")
audit_h1h2_f$idp_status_c <- relevel(as.factor(audit_h1h2_f$idp_status_c), ref = "Non-victim")

df.summary_f <- audit_h1h2_f %>%
  group_by(victim_status) %>%
  dplyr::summarise(
    sd = sd(friendliness, na.rm = TRUE),
    friendliness = mean(friendliness),
    se = sd/sqrt(dplyr::n()),
    n = dplyr::n())

t.test(audit_h1h2_f[audit_h1h2_f$victim_status == 'Non-victim',]$friendliness, 
       audit_h1h2_f[audit_h1h2_f$victim_status == 'Victim',]$friendliness, 
       alternative = "two.sided", var.equal = FALSE)
mean(audit_h1h2_f[audit_h1h2_f$victim_status == 'Victim',]$friendliness) - 
  mean(audit_h1h2_f[audit_h1h2_f$victim_status == 'Non-victim',]$friendliness)
(mean(audit_h1h2_f[audit_h1h2_f$victim_status == 'Victim',]$friendliness) - 
    mean(audit_h1h2_f[audit_h1h2_f$victim_status == 'Non-victim',]$friendliness))/
  sd(audit_h1h2_f[audit_h1h2_f$victim_status == 'Non-victim' | audit_h1h2_f$victim_status == 'Victim',]$friendliness)

figure3b <- ggplot(df.summary_f, aes(victim_status, friendliness, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = friendliness-se, ymax = friendliness+se), width = 0.2) +
  labs(x = "", y = "Friendly Response Score") + 
  ylim(0, 1.5) + 
  annotate("text", label = expression(paste("Diff. = 0.25 SD ( ", Delta, "58.4%)")), x = 1.5, y = 1.40, size = 5) + 
  annotate("text", label = expression(paste("p < .001")), x = 1.5, y = 1.33, size = 5) + 
  theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#Substantive Response
audit_h1h2_s <- audit[, c('victim_conflict', 'substantive', 'idp_status')]
audit_h1h2_s$victim_status <- ifelse(audit_h1h2_s$victim_conflict == 1, "Victim", "Non-victim")
audit_h1h2_s$idp_status_c <- ifelse(audit_h1h2_s$idp_status == 1, "IDP Victim", 
                                    ifelse(audit_h1h2_s$victim_conflict == 1, "Victim", 'Non-victim'
                                    ))
levels(audit_h1h2_s$victim_status) <- c("Non-victim", "Victim")
levels(audit_h1h2_s$idp_status_c) <- c("Non-victim", "Victim", "IDP Victim")
audit_h1h2_s$victim_status <- relevel(as.factor(audit_h1h2_s$victim_status), ref = "Non-victim")
audit_h1h2_s$idp_status_c <- relevel(as.factor(audit_h1h2_s$idp_status_c), ref = "Non-victim")

df.summary_s <- audit_h1h2_s %>%
  group_by(victim_status) %>%
  dplyr::summarise(
    sd = sd(substantive, na.rm = TRUE),
    substantive = mean(substantive),
    se = sd/sqrt(dplyr::n()))

t.test(audit_h1h2_s[audit_h1h2_s$victim_status == 'Non-victim',]$substantive, 
       audit_h1h2_s[audit_h1h2_s$victim_status == 'Victim',]$substantive, 
       alternative = "two.sided", var.equal = FALSE)

mean(audit_h1h2_s[audit_h1h2_s$victim_status == 'Victim',]$substantive) - 
  mean(audit_h1h2_s[audit_h1h2_s$victim_status == 'Non-victim',]$substantive)
sd(audit_h1h2_s[audit_h1h2_s$victim_status == 'Victim' | audit_h1h2_s$victim_status == 'Non-victim',]$substantive)
(mean(audit_h1h2_s[audit_h1h2_s$victim_status == 'Victim',]$substantive) - 
    mean(audit_h1h2_s[audit_h1h2_s$victim_status == 'Non-victim',]$substantive))/sd(audit_h1h2_s[audit_h1h2_s$victim_status == 'Victim' | audit_h1h2_s$victim_status == 'Non-victim',]$substantive)

figure3c <- ggplot(df.summary_s, aes(victim_status, substantive, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = substantive-se, ymax = substantive+se), width = 0.2) +
  labs(x = "", y = "Substantive Response Score") + 
  ylim(0, 0.78) + 
  annotate("text", label = expression(paste("Diff. = 0.11 SD ( ", Delta, "21.3%)")), x = 1.5, y = 0.73, size = 5) + 
  annotate("text", label = expression(paste("p = .08")), x = 1.5, y = 0.69, size = 5) + 
  theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

figure3 <- ggarrange(figure3a, figure3b, figure3c, 
          labels = c("    A) Response", "  B) Friendliness", 
                     "C) Substantiveness"),
          ncol = 3, nrow = 1,
          font.label = list(size = 14, color = "black"))

#all plots
png(filename="figure3.png", 
    units="in", 
    width=12, 
    height=6, 
    pointsize=12, 
    res=1000)
figure3
dev.off()

#####################################
#Table 3 (& Appendix L)
####################################

#A Response Rate (and table L.1)

#column 1
summary(h3_nocontrols <- lm(response ~ victim_of_left*center + victim_of_left*right, 
                            data = audit[audit$victim_conflict == 1,]))
#column 2
summary(h3_allmanip_fe <- lm(response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                             data = audit[audit$victim_conflict == 1,]))


#column 3
summary(h3_allmanip_fe_controls <- lm(response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + pvotes, 
                                      data = audit[audit$victim_conflict == 1,]))



#B Friendly Response (and table L.2)

#column 4
summary(h3_nocontrols_f <- lm(friendliness ~ victim_of_left*center + victim_of_left*right, 
                              data = audit[audit$victim_conflict == 1,]))

#column 5
summary(h3_allmanip_fe_f <- lm(friendliness ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                               data = audit[audit$victim_conflict == 1,]))

#column 6
summary(h3_allmanip_fe_controls_f <- lm(friendliness ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                          tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                          lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                          desemp_fisc + inv_gruposvunera + pvotes, 
                                        data = audit[audit$victim_conflict == 1,]))

summary(h3_allmanip_fe_f <- lm(friendliness ~ victim_of_left*center + victim_of_left*right + log(words_responses+1)+gender_female + vote_registration + employment_request + as.factor(depto), 
                               data = audit[audit$victim_conflict == 1,]))

summary(h3_allmanip_fe_controls_f <- lm(friendliness ~ victim_of_left*center + victim_of_left*right +  + log(words_responses+1)+gender_female + vote_registration + employment_request + as.factor(depto) +
                                          tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                          lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                          desemp_fisc + inv_gruposvunera + pvotes, 
                                        data = audit[audit$victim_conflict == 1,]))
#C Substantive Response (and table L.3)

#column 7
summary(h3_nocontrols_s <- lm(substantive ~ victim_of_left*center + victim_of_left*right, 
                              data = audit[audit$victim_conflict == 1,]))

#column 8
summary(h3_allmanip_fe_s <- lm(substantive ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                               data = audit[audit$victim_conflict == 1,]))

#column 9
summary(h3_allmanip_fe_controls_s <- lm(substantive ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                          tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                          lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                          desemp_fisc + inv_gruposvunera + pvotes, 
                                        data = audit[audit$victim_conflict == 1,]))

summary(h3_allmanip_fe_s <- lm(substantive ~ victim_of_left*center + victim_of_left*right  + log(words_responses+1)+ gender_female + vote_registration + employment_request + as.factor(depto), 
                               data = audit[audit$victim_conflict == 1,]))

summary(h3_allmanip_fe_controls_s <- lm(substantive ~ victim_of_left*center + victim_of_left*right + + log(words_responses+1)+gender_female + vote_registration + employment_request + as.factor(depto) +
                                          tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                          lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                          desemp_fisc + inv_gruposvunera + pvotes, 
                                        data = audit[audit$victim_conflict == 1,]))




stargazer(h3_nocontrols_s ,h3_allmanip_fe_s,h3_allmanip_fe_controls_s,

          title = " The Interaction Effects of Mayor’s Ideology and the Identity of the Perpetrator",
          type="html",out = "reg1.doc")

cor.test(log(audit$words_responses+1),audit$friendliness)
cor.test(log(audit$words_responses+1),audit$friendliness_obj)
cor.test(log(audit$words_responses+1),audit$friendliness_qual)
cor.test(log(audit$words_responses+1),audit$substantive)

#Table L.4
surv_object <- Surv(time = audit[audit$victim_conflict == 1,]$time, event = audit[audit$victim_conflict == 1,]$censored)

#column 1
summary(h3_nocontrols_t <-  coxph(surv_object  ~ victim_of_left*center + victim_of_left*right, 
                                  data = audit[audit$victim_conflict == 1,]))

#column 2
summary(h3_allmanip_fe_t <-  coxph(surv_object  ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                   data = audit[audit$victim_conflict == 1,]))

#column 3
summary(h3_allmanip_fe_controls_t <-  coxph(surv_object  ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                              tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                              lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                              desemp_fisc + inv_gruposvunera + pvotes, 
                                            data = audit[audit$victim_conflict == 1,]))

#Table L.5

#column 1
summary(h3_nocontrols_length <-  lm(log(words_responses+1) ~ victim_of_left*center + victim_of_left*right, 
                                    data = audit[audit$victim_conflict == 1,]))

#column 2
summary(h3_allmanip_fe_length <- lm(log(words_responses+1) ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                    data = audit[audit$victim_conflict == 1,]))

#column 3
summary(h3_allmanip_fe_controls_length <- lm(log(words_responses+1)  ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                               tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                               lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                               desemp_fisc + inv_gruposvunera + pvotes, 
                                             data = audit[audit$victim_conflict == 1,]))



#####################################
#Figure 4
####################################

#A Response Rate

df.summary3 <- audit %>%
  group_by(victim_group, ideology_rev) %>%
  dplyr::summarise(
    sd = sd(response, na.rm = TRUE),
    response = mean(response),
    se = sd/sqrt(dplyr::n()),
    n = dplyr::n())

t.test(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == -1,]$response, 
       audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == -1,]$response, 
       alternative = "two.sided", var.equal = FALSE)

mean(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == -1,]$response) #response by leftist govt to victims of the left
mean(audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == -1,]$response) #response by leftist govt to victims of the left

t.test(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == 0,]$response, 
       audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == 0,]$response, 
       alternative = "two.sided", var.equal = FALSE)

mean(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == 0,]$response) #response by leftist govt to victims of the left
mean(audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == 0,]$response) #response by leftist govt to victims of the left

t.test(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == 1,]$response, 
       audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == 1,]$response, 
       alternative = "two.sided", var.equal = FALSE)

df.summary3$victim_group <- factor(df.summary3$victim_group, levels = c(as.character(df.summary3$victim_group[4]), 
                                                                        as.character(df.summary3$victim_group[1]),
                                                                        as.character(df.summary3$victim_group[7])))

barplot_victim_ideology_response <- ggplot(df.summary3, aes(x=as.character(victim_group), y=response, fill=as.factor(ideology_rev))) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = response-se, ymax = response+se), position='dodge') + theme_pubr() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "", y = "Response rate") +
  guides(fill=guide_legend(title="Mayor's ideology")) +
  scale_fill_manual(labels = c("Left", "Center", "Right"), values = c('gray20', 'gray50', 'gray70'),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18))


#B Friendly Response

t.test(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == 0,]$friendliness, 
       audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == 0,]$friendliness, 
       alternative = "two.sided", var.equal = FALSE)

t.test(audit[audit$victim_conflict == 1 & audit$victim_of_left == 1 & audit$ideology_rev == 1,]$substantive, 
       audit[audit$victim_conflict == 1 & audit$victim_of_left == 0 & audit$ideology_rev == 1,]$substantive, 
       alternative = "two.sided", var.equal = FALSE)


df.summary3_f <- audit %>%
  group_by(victim_group, ideology_rev) %>%
  dplyr::summarise(
    sd = sd(friendliness, na.rm = TRUE),
    friendliness = mean(friendliness),
    se = sd/sqrt(dplyr::n()),
    n = dplyr::n())

barplot_victim_ideology_friendly <- ggplot(df.summary3_f, aes(x=as.character(victim_group), y=friendliness, fill=as.factor(ideology_rev))) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = friendliness-se, ymax = friendliness+se), position='dodge') + theme_pubr() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "Identity of the perpetrator", y = "Friendly Response Score") +
  scale_x_discrete(labels=c("0" = "Victim of state violence \n or the paramilitaries\n",  "1" = "Victim of left-wing \n armed groups \n (FARC or ELN)")) +
  guides(fill=guide_legend(title="Mayor's ideology")) +
  scale_fill_manual(labels = c("Left", "Center", "Right"), values = c('gray20', 'gray50', 'gray70'),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18))

#C Substantive Response

df.summary3_s <- audit %>%
  group_by(victim_group, ideology_rev) %>%
  dplyr::summarise(
    sd = sd(substantive, na.rm = TRUE),
    substantive = mean(substantive),
    se = sd/sqrt(dplyr::n()),
    n = dplyr::n())

barplot_victim_ideology_substantive <- ggplot(df.summary3_s, aes(x=as.character(victim_group), y=substantive, fill=as.factor(ideology_rev))) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = substantive-se, ymax = substantive+se), position='dodge') + theme_pubr() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "", y = "Substantive Response Score") +
  scale_x_discrete(labels=c("0" = "Victim of state violence \n or the paramilitaries\n",  "1" = "Victim of left-wing \n armed groups \n (FARC or ELN)")) +
  guides(fill=guide_legend(title="Mayor's ideology")) +
  scale_fill_manual(labels = c("Left", "Center", "Right"), values = c('gray20', 'gray50', 'gray70'),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18))

#Figure 4 (all)
png(filename="barplot_victim_ideology_response.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
barplot_victim_ideology_response
dev.off()

png(filename="barplot_victim_ideology_friendly.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
barplot_victim_ideology_friendly
dev.off()

png(filename="barplot_victim_ideology_substantive.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
barplot_victim_ideology_substantive
dev.off()

figure4 <- ggarrange(barplot_victim_ideology_response, barplot_victim_ideology_friendly, 
                     barplot_victim_ideology_substantive, 
                     labels = c(" A) Response", " B) Friendliness", 
                                "C) Substantiveness"),
                     ncol = 3, nrow = 1, 
                     common.legend = TRUE,
                     legend = "top",
                     hjust = c(-0.8, -0.7, -0.5),
                     vjust = 1, 
                     font.label = list(size = 14, color = "black"))

png(filename="allplots_h3.png", 
    units="in", 
    width=16, 
    height=10, 
    pointsize=15, 
    res=1000)
figure4
dev.off()


#####################################
#Appendix F
###################################

#########
#Figure F.1
#########

# audit$left_major <- ifelse(audit$ideology <= 2,1,0) #ideology 1 to 5 
# audit$right_major <- ifelse(audit$ideology >= 3,1,0) 
# 
# set.seed(12345)
# 
# muestra <- seq(from=100, to=2000, by=100)
# N_muestras <- length(muestra)
# power_N_any <- rep(NA, N_muestras)
# alpha <- 0.05
# sims <- 1000
# 
# 
# for (j in 1:N_muestras){
#   
#   N <- muestra[j]
#   p_T1vsC <- rep(NA, sims)
#   p_T2vsC <- rep(NA, sims)
#   
#   c_T1vsC <- rep(NA, sims)
#   c_T2vsC <- rep(NA, sims)
#   
#   
#   
#   for (i in 1:sims){
#     data <- audit[sample(nrow(audit), size=N, replace =T), ]
#     data_sim <- data
#     p <-0.3
#     or <- p/(1-p)
#     beta_0 <- log(or)
#     beta_1 <- log(1.55)
#     beta_2 <- log(1.55) 
#     pr <- exp(beta_0+beta_1*(data$treat2==1)+beta_2*(data$treat2==2)) / 
#       (1 + exp(beta_0+beta_1*(data$treat2==1)+beta_2*(data$treat2==2)))
#     data_sim$Y_sim <- rbinom(n=nrow(data), size=1, prob=pr)
#     
#     fit_T1vsC_sim <- lm(Y_sim ~ (treat2==1), data=subset(data_sim, treat2!=2))
#     fit_T2vsC_sim <- lm(Y_sim ~ (treat2==2),data=subset(data_sim, treat2!=1))
#     
#     c_T1vsC[i] <- summary(fit_T1vsC_sim)$coefficients[2,1]
#     c_T2vsC[i] <- summary(fit_T2vsC_sim)$coefficients[2,1]
#     
#     p_T1vsC[i] <- summary(fit_T1vsC_sim)$coefficients[2,4]
#     p_T2vsC[i] <- summary(fit_T2vsC_sim)$coefficients[2,4]
#     
#   }
#   power_N_any[j]  <- mean(c_T1vsC>0 & c_T2vsC>0 & (p_T1vsC < alpha/2 | p_T2vsC < alpha/2 ))
# 
# }
# 
# data_power <- data.frame(power_N_any, muestra)
# 
# png(filename="figure_f1.png")
# ggplot(data_power, aes(muestra)) +
#   geom_line(aes(y = power_N_any),stat="identity") + 
#   geom_hline(aes(yintercept=0.8), linetype="dashed", color = "red")+
#   xlab("sample") + ylab("power")+ggtitle("Power Analysis")+
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=16,face="bold"))
# dev.off()
# 
# ########
# #Figure F.2
# ##########
# 
# 
# set.seed(123)
# sample <- seq(from=100, to=2000, by=100)  #sample sizes
# N_samples <- length(sample)
# alpha <- 0.05
# sims <- 10000 
# power_N <- rep(NA, N_samples)
# 
# 
# 
# for (j in 1:N_samples){
#   
#   N <- sample[j]
#   p_value <- rep(NA, sims)
#   c <- rep(NA, sims)
#   t <- rep(NA, sims)
#   
#   
#   for (i in 1:sims){
#     data <- audit[sample(nrow(audit), size=N, replace =T), ]
#     data$left_victim <- sample( rep(c(1, 0), each = N/2) )
#     data$mismatch <- ifelse(data$left_victim==1 & data$right==1, 1,0)
#     
#     p <-0.4 
#     or <- p/(1-p)
#     beta_0 <- log(or)
#     beta_1 <- log(0.58) #42% lower if mismatch
#     
#     pr <- exp(beta_0+beta_1*(data$mismatch==1)) / 
#       (1 + exp(beta_0+beta_1*(data$mismatch==1)))
#     data$Y_sim <- rbinom(n=nrow(data), size=1, prob=pr)
#     
#     fit <- lm(Y_sim ~ (mismatch==1),  data=data)
#     
#     c[i] <- summary(fit)$coefficients[2,1]
#     
#     p_value[i] <- summary(fit)$coefficients[2,4]
#     
#     
#   }
#   
#   power_N[j] <- mean(c<0 & (p_value < alpha/2)  )  
# }
# 
# data_power2 <- data.frame(power_N, sample)
# 
# 
# 
# 
# png(filename="figure_f2.png")
# ggplot(data_power2, aes(sample)) +
#   geom_line(aes(y = power_N),stat="identity") + 
#   geom_hline(aes(yintercept=0.8), linetype="dashed", color = "red")+
#   xlab("Sample") + ylab("Power")+ggtitle("Power Analysis") +
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=16,face="bold"))
# dev.off()



####################################
#Appendix J
####################################

################
#Figure J.1
###############
#A Response Rate

df.summary2 <- audit_h1h2 %>%
  group_by(idp_status_c) %>%
  dplyr::summarise(
    sd = sd(response, na.rm = TRUE),
    response = mean(response),
    se = sd/sqrt(dplyr::n()))

t.test(audit_h1h2[audit_h1h2$idp_status == 0 & audit_h1h2$victim_conflict == 0,]$response, audit_h1h2[audit_h1h2$idp_status == 1,]$response, alternative = "two.sided", var.equal = FALSE)
mean(audit_h1h2[audit_h1h2$idp_status == 1 & audit_h1h2$victim_conflict == 1,]$response) - mean(audit_h1h2[audit_h1h2$idp_status == 0 & audit_h1h2$victim_conflict == 0,]$response)
(mean(audit_h1h2[audit_h1h2$idp_status == 1 & audit_h1h2$victim_conflict == 1,]$response) - mean(audit_h1h2[audit_h1h2$idp_status == 0 & audit_h1h2$victim_conflict == 0,]$response))/mean(audit_h1h2[audit_h1h2$idp_status == 0 & audit_h1h2$victim_conflict == 0,]$response)
nrow(audit_h1h2)

barplot_idp_response <- ggplot(df.summary2, aes(idp_status_c, response, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = response-se, ymax = response+se), width = 0.2) +
  annotate("text", label = expression(paste("Diff. = 7.7pp (", Delta, " 25%)")), x = 1.5, y = 0.45, size = 5) + 
  annotate("text", label = expression(paste("p = .03")), x = 1.5, y = 0.42, size = 5) + 
  labs(x = "", y = "Response rate") + 
  ylim(0, 0.48) + theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#B Friendly Response

df.summary2_f <- audit_h1h2_f %>%
  group_by(idp_status_c) %>%
  dplyr::summarise(
    sd = sd(friendliness, na.rm = TRUE),
    friendliness = mean(friendliness),
    se = sd/sqrt(dplyr::n()))
df.summary2_f <- df.summary2_f[1:2,]

t.test(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'Non-victim',]$friendliness, 
       audit_h1h2_f[audit_h1h2_f$idp_status_c == 'IDP Victim',]$friendliness, 
       alternative = "two.sided", var.equal = FALSE)

mean(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'Non-victim',]$friendliness) - 
  mean(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'IDP Victim',]$friendliness)
sd(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'Non-victim' | audit_h1h2_f$idp_status_c == 'IDP Victim',]$friendliness)
(mean(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'IDP Victim',]$friendliness) - 
    mean(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'Non-victim',]$friendliness))/
  sd(audit_h1h2_f[audit_h1h2_f$idp_status_c == 'Non-victim' | audit_h1h2_f$idp_status_c == 'IDP Victim',]$friendliness)

barplot_idp_friendly <- ggplot(df.summary2_f, aes(idp_status_c, friendliness, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = friendliness-se, ymax = friendliness+se), width = 0.2) +
  annotate("text", label = expression(paste("Diff. = 0.31 SD (", Delta, " 72.7%)")), x = 1.5, y = 1.5, size = 5) + 
  annotate("text", label = expression(paste("p < .001")), x = 1.5, y = 1.42, size = 5) + 
  labs(x = "", y = "Friendly Response Score") + 
  ylim(0, 1.60) + theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#C Substantive Response

df.summary2 <- audit_h1h2_s %>%
  group_by(idp_status_c) %>%
  dplyr::summarise(
    sd = sd(substantive, na.rm = TRUE),
    substantive = mean(substantive),
    se = sd/sqrt(dplyr::n()))
df.summary2 <- df.summary2[1:2,]

t.test(audit_h1h2_s[audit_h1h2_s$idp_status_c == 'Non-victim',]$substantive, 
       audit_h1h2_s[audit_h1h2_s$idp_status_c == 'IDP Victim',]$substantive, 
       alternative = "two.sided", var.equal = FALSE)
sd(audit_h1h2_s[audit_h1h2_s$idp_status_c == 'IDP Victim' | audit_h1h2_s$idp_status_c == 'Non-victim',]$substantive)
(mean(audit_h1h2_s[audit_h1h2_s$idp_status_c == 'IDP Victim',]$substantive) - 
    mean(audit_h1h2_s[audit_h1h2_s$idp_status_c == 'Non-victim',]$substantive))/
  sd(audit_h1h2_s[audit_h1h2_s$idp_status_c == 'IDP Victim' | audit_h1h2_s$idp_status_c == 'Non-victim',]$substantive)

barplot_idp_substantive <- ggplot(df.summary2, aes(idp_status_c, substantive, width=.65)) +
  geom_col(fill = c('gray20', 'gray70'), color = "black") +
  geom_errorbar(aes(ymin = substantive-se, ymax = substantive+se), width = 0.2) +
  annotate("text", label = expression(paste("Diff. = 0.18 SD (", Delta, " 37.8%)")), x = 1.5, y = 0.75, size = 5) + 
  annotate("text", label = expression(paste("p = .013")), x = 1.5, y = 0.71, size = 5) + 
  labs(x = "", y = "Substantive Response Score") + 
  ylim(0, 0.8) + theme_pubr() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#all plots
png(filename="allplots_h2.png", 
    units="in", 
    width=12, 
    height=6, 
    pointsize=12, 
    res=1000)
ggarrange(barplot_idp_response, barplot_idp_friendly, barplot_idp_substantive, 
          labels = c(" A) Response rate", "B) Friendly response", 
                     "C) Substantive response"),
          ncol = 3, nrow = 1)
dev.off()

#######################
#Appendix K
######################

#Table K.1 Response Rates

#column 1
summary(h1_nocontrols <- lm(response ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe <- lm(response ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls <- lm(response ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, data = audit))

#column 4
summary(h2_nocontrols <- lm(response ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe <- lm(response ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls <- lm(response ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, 
                                      data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

# Table K2 Friendliness score

#column 1
summary(h1_nocontrols <- lm(friendliness ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe <- lm(friendliness ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls <- lm(friendliness ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, data = audit))

#column 4
summary(h2_nocontrols <- lm(friendliness ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe <- lm(friendliness ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls <- lm(friendliness ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, 
                                      data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))



#Table K.3 Helpfulness Score

#column 1
summary(h1_nocontrols <- lm(substantive ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe <- lm(substantive ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls <- lm(substantive ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, data = audit))

#column 4
summary(h2_nocontrols <- lm(substantive ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe <- lm(substantive ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls <- lm(substantive ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                        tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                        lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                        desemp_fisc + inv_gruposvunera + 
                                        center + right + pvotes, 
                                      data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))


#Table K.4 Response timing in days

surv_object <- Surv(time = audit$time, event = audit$censored)

#column 1
summary(h1_nocontrols <- coxph(surv_object ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe <- coxph(surv_object ~  victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls <- coxph(surv_object ~  victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                           tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                           lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                           desemp_fisc + inv_gruposvunera + 
                                           center + right + pvotes, data = audit))


surv_object <- Surv(time = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]$time, event = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]$censored)

#column 4
summary(h2_nocontrols <- coxph(surv_object ~  idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe <- coxph(surv_object ~  idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls <- coxph(surv_object ~  idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                           tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                           lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                           desemp_fisc + inv_gruposvunera + 
                                           center + right + pvotes, 
                                         data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))


#Table K.5. Length of Responses 

#column 1
summary(h1_nocontrols_length <- lm(log(words_responses+1) ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe_length <- lm(log(words_responses+1) ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls_length <- lm(log(words_responses+1) ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                               tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                               lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                               desemp_fisc + inv_gruposvunera + 
                                               center + right + pvotes, data = audit))

#column 4
summary(h2_nocontrols_length <- lm(log(words_responses+1) ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe_length <- lm(log(words_responses+1) ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls_length <- lm(log(words_responses+1) ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                               tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                               lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                               desemp_fisc + inv_gruposvunera + 
                                               center + right + pvotes, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#####################################
#Appendix M Interaction models using the ideological mismatch dummy
#########################3

#Table M.1.

#Response rate

#column 1
summary(h3_nocontrols_collapse <- lm(response ~ victim_mayor_mismatch, 
                                     data = audit[audit$victim_conflict == 1,]))
#column 2
summary(h3_allmanip_fe_collapse <- lm(response ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto), 
                                      data = audit[audit$victim_conflict == 1,]))
#column 3
summary(h3_allmanip_fe_controls_collapse <- lm(response ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto) +
                                                 tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                                 lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                                 desemp_fisc + inv_gruposvunera + pvotes, 
                                               data = audit[audit$victim_conflict == 1,]))

#Friendly response

#column 4
summary(h3_nocontrols_collapse_f <- lm(friendliness ~ victim_mayor_mismatch, 
                                       data = audit[audit$victim_conflict == 1,]))
#column 5
summary(h3_allmanip_fe_collapse_f <- lm(friendliness ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto), 
                                        data = audit[audit$victim_conflict == 1,]))
#column 6
summary(h3_allmanip_fe_controls_collapse_f <- lm(friendliness ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto) +
                                                   tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                                   lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                                   desemp_fisc + inv_gruposvunera + pvotes, 
                                                 data = audit[audit$victim_conflict == 1,]))


#Helpful response

#column 7
summary(h3_nocontrols_collapse_s <- lm(substantive ~ victim_mayor_mismatch, 
                                       data = audit[audit$victim_conflict == 1,]))
#column 8
summary(h3_allmanip_fe_collapse_s <- lm(substantive ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto), 
                                        data = audit[audit$victim_conflict == 1,]))
#column 9
summary(h3_allmanip_fe_controls_collapse_s <- lm(substantive ~ victim_mayor_mismatch + gender_female + vote_registration + employment_request + as.factor(depto) +
                                                   tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                                   lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                                   desemp_fisc + inv_gruposvunera + pvotes, 
                                                 data = audit[audit$victim_conflict == 1,]))

########################################
#Appendix N Friendliness results (objective vs. subjective)
#####################################

#########
#Table N.1
############


#H1
#column 1
summary(h1_allmanip_obj <- lm(friendliness_obj ~ victim_conflict + gender_female + vote_registration + employment_request, data = audit))

#column 2
summary(h1_allmanip_fe_obj <- lm(friendliness_obj ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls_obj <- lm(friendliness_obj ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                            tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                            lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                            desemp_fisc + inv_gruposvunera + 
                                            center + right + pvotes, data = audit))
#H2

#column 4
summary(h2_nocontrols_obj <- lm(friendliness_obj ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe_obj <- lm(friendliness_obj ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls_obj <- lm(friendliness_obj ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                            tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                            lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                            desemp_fisc + inv_gruposvunera + 
                                            center + right + pvotes, 
                                          data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))


#H3
#column 7
summary(h3_nocontrols_f_obj <- lm(friendliness_obj ~ victim_of_left*center + victim_of_left*right, 
                                  data = audit[audit$victim_conflict == 1,]))
#column 8
summary(h3_allmanip_fe_f_obj <- lm(friendliness_obj ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                   data = audit[audit$victim_conflict == 1,]))
#column 9
summary(h3_allmanip_fe_controls_f_obj <- lm(friendliness_obj ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                              tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                              lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                              desemp_fisc + inv_gruposvunera + pvotes, 
                                            data = audit[audit$victim_conflict == 1,]))


#########
#Table N.2
############

#H1

#column 1
summary(h1_nocontrols_qual <- lm(friendliness_qual ~ victim_conflict, data = audit))

#column 2
summary(h1_allmanip_fe_qual <- lm(friendliness_qual ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto), data = audit))

#column 3
summary(h1_allmanip_fe_controls_qual <- lm(friendliness_qual ~ victim_conflict + gender_female + vote_registration + employment_request + as.factor(depto) +
                                             tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                             lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                             desemp_fisc + inv_gruposvunera + 
                                             center + right + pvotes, data = audit))


#H2

#column 4
summary(h2_nocontrols_qual <- lm(friendliness_qual ~ idp_status, data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 5
summary(h2_allmanip_fe_qual <- lm(friendliness_qual ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto), data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))

#column 6
summary(h2_allmanip_fe_controls_qual <- lm(friendliness_qual ~ idp_status + gender_female + vote_registration + employment_request + as.factor(depto) +
                                             tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                             lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                             desemp_fisc + inv_gruposvunera + 
                                             center + right + pvotes, 
                                           data = audit[audit$idp_status == 1 | audit$victim_conflict == 0,]))


#H3

#column 7
summary(h3_nocontrols_f_qual <- lm(friendliness_qual ~ victim_of_left*center + victim_of_left*right, 
                                   data = audit[audit$victim_conflict == 1,]))

#column 8
summary(h3_allmanip_fe_f_qual <- lm(friendliness_qual ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                    data = audit[audit$victim_conflict == 1,]))

#column 9
summary(h3_allmanip_fe_controls_f_qual <- lm(friendliness_qual ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                               tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                               lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                               desemp_fisc + inv_gruposvunera + pvotes, 
                                             data = audit[audit$victim_conflict == 1,]))


########################################
#Appendix O Type of violence type and mayor’s ideology
#####################################

########################################
#Appendix O.1 Correlation table
#####################################

round(cor(audit$left, audit$tpobc_DESC), 2)
round(cor(audit$left, audit$tpobc_FARC), 2)
round(cor(audit$left, audit$tpobc_ELN), 2)
round(cor(audit$left, audit$tpobc_AUC), 2)

round(cor(audit$center, audit$tpobc_DESC), 2)
round(cor(audit$center, audit$tpobc_FARC), 2)
round(cor(audit$center, audit$tpobc_ELN), 2)
round(cor(audit$center, audit$tpobc_AUC), 2)

round(cor(audit$right, audit$tpobc_DESC), 2)
round(cor(audit$right, audit$tpobc_FARC), 2)
round(cor(audit$right, audit$tpobc_ELN), 2)
round(cor(audit$right, audit$tpobc_AUC), 2)

########################################
#Appendix O.2 Main finding with a restricted sample
#####################################

summary(restricted_model <- lm(response ~ victim_of_left*center + victim_of_left*right, 
                               data = audit[audit$victim_conflict == 1 & 
                                              (audit$tpobc_FARC > quantile(audit$tpobc_FARC+audit$tpobc_ELN, 0.5) ) & audit$tpobc_AUC > quantile(audit$tpobc_AUC, 0.5),]))

########################################
#Appendix P Signal-based responsiveness
#####################################

#column 1
summary(h3_nocontrols_political <- lm(political_response ~ victim_of_left*center + victim_of_left*right, 
                                      data = audit[audit$victim_conflict == 1 ,]))
#column 2
summary(h3_allmanip_fe_political <- lm(political_response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                       data = audit[audit$victim_conflict == 1,]))
#column 3
summary(h3_allmanip_fe_controls_political <- lm(political_response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                                  tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                                  lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                                  desemp_fisc + inv_gruposvunera + pvotes, 
                                                data = audit[audit$victim_conflict == 1,]))
#column 4
summary(h3_nocontrols_bureaucrat <- lm(bureaucrat_response ~ victim_of_left*center + victim_of_left*right, 
                                       data = audit[audit$victim_conflict == 1 ,]))
#column 5
summary(h3_allmanip_fe_bureaucrat <- lm(bureaucrat_response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto), 
                                        data = audit[audit$victim_conflict == 1,]))
#column 6
summary(h3_allmanip_fe_controls_bureaucrat <- lm(bureaucrat_response ~ victim_of_left*center + victim_of_left*right + gender_female + vote_registration + employment_request + as.factor(depto) +
                                                   tpobc_DESC + tpobc_ELN + tpobc_FARC + tpobc_AUC +
                                                   lotes_coca + gini + nbi + I(pobl_tot/1000000) + ing_propios + iga_total + ate_ciudadano + 
                                                   desemp_fisc + inv_gruposvunera + pvotes, 
                                                 data = audit[audit$victim_conflict == 1,]))
