library(tidyverse)
library(lme4)
library(ggplot2)
library(MASS)
library(naniar)
library(merTools)

# read in data -----------------------------------------------------------------
consults <- read.csv("consults.csv")

# change variables to factors/binary -------------------------------------------
consults$lead_consultant <- factor(consults$lead_consultant, 
                                   levels = c("C1", "C2", "C3", "C4", "C5", 
                                              "C6", "C7", "C8", "C9", "C10", 
                                              "C11", "C12"))
consults$complete_bin <- ifelse(consults$complete == "Yes", 1, 0)

# figure 2: missing data -------------------------------------------------------

###Alison's code###


# descriptive analysis: table 1 ------------------------------------------------

consults %>% summarize(n = n())

# top three ethics issues, overall
consults %>% 
  group_by(ethics_issue_primary) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:3)

consults %>% 
  group_by(lead_consultant) %>% 
  summarise(n = n())

# top three ethics issues, per consultant
consults %>% 
  group_by(lead_consultant, ethics_issue_primary) %>% 
  summarise(n = n()) %>% 
  arrange(lead_consultant, desc(n)) %>%
  slice(1:3) %>%
  print(n = 35)

# descriptive analysis: table 2 ------------------------------------------------

###Alison's code###


# descriptive analysis: top five ethics issues ---------------------------------

proportion <- consults %>%
                group_by(Ethics.Issue) %>%
                summarise(n = n()/nrow(consults)) %>%
                arrange(desc(n))
proportion[, 2] <- 100 * round(proportion[, 2], 2)
proportion[, 3] <- rep("Overall", 6)
colnames(proportion) <- c("Issue", "Proportion", "All")

proportion$Issue <- factor(proportion$Issue, 
                           levels= c("Patient Decisional Capacity",
                                      "No Surrogate",
                                      "End of Life",
                                      "Inappropriate Treatment",
                                      "Treatment Refusal",
                                      "Other"))

palette <- c("tomato2", "tan1", "#00BC59", "#00BBDB", "#AC88FF", "#FF65AE")

# graph for presentation
ggplot(proportion, aes(x = All, y = Proportion, fill = Issue)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5),
            size = 6) +
  ylab("Percentage") +
  xlab(NULL) +
  scale_fill_manual(values = palette) +
  theme(text = element_text(size = 25))

# figure 3: observed completion rates by consultant ----------------------------

consult_completion <- consults %>% 
                        group_by(lead_consultant, 
                                 consult_end_year, 
                                 .drop = FALSE) %>% 
                        summarise(complete = mean(complete_bin))

# completion per year
all_cons <- consults %>% 
              group_by(consult_end_year) %>% 
              summarise(complete = mean(complete_bin))
all_cons$lead_consultant <- rep("Overall", 6)

cons_complete <- rbind(all_cons[, c(3,1,2)], consult_completion)
cons_complete$lead_consultant <- factor(cons_complete$lead_consultant, 
                                        levels = c("Overall", "C1", "C2", "C3", 
                                                   "C4", "C5", "C6", "C7", "C8",
                                                   "C9", "C10", "C11", "C12"))
cons_complete <- cons_complete %>% arrange(consult_end_year)

overall_data <- subset(cons_complete, lead_consultant == "Overall")
individual_data <- subset(cons_complete, lead_consultant != "Overall")
individual_data$facet <- individual_data$lead_consultant

every_facet_data <- merge(overall_data,
                          data.frame(lead_consultant = "Overall",
                                     facet = unique(individual_data$facet)))

plot_data <- rbind(every_facet_data, individual_data)

palette <- c("#A9A9A9", "#F8766D", "#E7861B", "#C59900", "#95A900", "#00BC59", 
             "#00C08D", "#00C0B8", "#00BBDB", "#529EFF", "#AC88FF", "#F763E0",
             "#FF65AE")

# plot with gray lines for overall and colored lines for consultants
ggplot(plot_data, aes(x = consult_end_year,
                      y = 100 * complete,
                      col = lead_consultant)) +
  geom_line(linewidth = 1.75) +
  geom_point(size = 4) +
  facet_wrap(~ facet) + 
  scale_color_manual(values = palette) + 
  xlab("Year") +
  ylab("% Complete") + 
  ylim(0, 100) +
  labs(col = "Lead Consultant") +
  theme(legend.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13))


# pull out C7 and C9 and plot
consult_completion2 <- consults %>%
                        filter(lead_consultant == "C7" | lead_consultant == "C9") %>%
                        group_by(lead_consultant, consult_end_year) %>%
                        summarise(complete = mean(complete_bin))

consult_completion2$lead_consultant <- factor(consult_completion2$lead_consultant,
                                              levels = c("C7", "C9"))

cons_complete2 <- rbind(all_cons[, c(3,1,2)], consult_completion2)
cons_complete2$lead_consultant <- factor(cons_complete2$lead_consultant,
                                         levels = c("Overall", "C7", "C9"))
cons_complete2 <- cons_complete2 %>% arrange(consult_end_year)

individual_data2 <- subset(cons_complete2, lead_consultant != "Overall")
individual_data2$facet <- individual_data2$lead_consultant

every_facet_data2 <- merge(overall_data,
                            data.frame(lead_consultant = "Overall",
                                       facet = unique(individual_data2$facet)))

plot_data2 <- rbind(every_facet_data2, individual_data2)

palette2 <- c("#A9A9A9", "#00C0B8","#529EFF")

ggplot(plot_data2, aes(x = consult_end_year,
                       y = 100 * complete, 
                       col = lead_consultant)) +
  geom_line(linewidth = 1.75) +
  geom_point(size = 4) +
  facet_wrap(~ facet) + 
  scale_color_manual(values = palette2) + 
  xlab("Year") +
  ylab("% Complete") + 
  ylim(0, 100) +
  labs(col = "Lead Consultant") +
  theme(legend.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13))

# figure 4: observed average quality score by consultant -----------------------

consult_quality <- consults %>% 
                      group_by(lead_consultant, 
                               consult_end_year, 
                               .drop = FALSE) %>%
                      summarise(quality = mean(Quality.score))

all_cons_qual <- consults %>% 
                    group_by(consult_end_year) %>% 
                    summarise(quality = mean(Quality.score))
all_cons_qual$lead_consultant <- rep("Overall", 6)

consult_quality <- rbind(all_cons_qual[, c(3,1,2)], consult_quality)
consult_quality <- consult_quality %>% arrange(consult_end_year)
consult_quality$lead_consultant <- factor(consult_quality$lead_consultant,
                                          levels = c("Overall", "C1", "C2", 
                                                     "C3", "C4", "C5", "C6", 
                                                     "C7", "C8", "C9", "C10", 
                                                     "C11", "C12"))

overall_data3 <- subset(consult_quality, lead_consultant == "Overall")
individual_data3 <- subset(consult_quality, lead_consultant != "Overall")
individual_data3$facet <- individual_data3$lead_consultant

every_facet_data3 <- merge(overall_data3,
                          data.frame(lead_consultant = "Overall",
                                     facet = unique(individual_data3$facet)))

plot_data3 <- rbind(every_facet_data3, individual_data3)

ggplot(plot_data3, aes(x = consult_end_year, 
                       y = quality, 
                       col = lead_consultant)) +
  geom_line(linewidth = 1.75) +
  geom_point(size = 4) +
  facet_wrap(~ facet) + 
  scale_color_manual(values = palette) + 
  xlab("Year") +
  ylab("Average Quality Score") +
  labs(col = "Lead Consultant") +
  ylim(0, 5) +
  theme(legend.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13))

# primary analysis -------------------------------------------------------------

observed_completion <- consults %>% 
                    group_by(lead_consultant) %>% 
                    summarise(complete=mean(complete_bin))

# primary model
glmm.fit <- glmer(complete_bin ~ Ethics.Issue + (1|lead_consultant),
                  data = consults,
                  family = binomial(link="logit"),
                  nAGQ = 20)

# get predicted adjusted completion
consult.mat <- rep("End of Life", 12)
cons <- c("C1", "C2", "C3", "C4", "C5", "C6", 
          "C7", "C8", "C9", "C10", "C11", "C12")
cons.mat <- as.data.frame(cbind(cons, consult.mat))
colnames(cons.mat) <- c("lead_consultant", "Ethics.Issue")

pred_prob <- predict(glmm.fit, newdata = cons.mat, type = "response")

# sensitivity analyses for primary outcome -------------------------------------

# simple model
glmm.fit.2 <- glmer(complete_bin ~ (1|lead_consultant),
                    data = consults,
                    family = binomial(link="logit"),
                    nAGQ = 20)

# predicted completion using simple model
cons <- as.data.frame(cons)
colnames(cons) <- "lead_consultant"
pred_prob_simple <- predict(glmm.fit.2, newdata = cons, type = "response")

# complex model
glmm.fit.comp <- glmer(complete_bin ~ Ethics.Issue + nu_assist_cons +
                         secondary_issues + total_time_in_hours + 
                         (1 | lead_consultant), 
                       data = consults, family = binomial(link = "logit"), 
                       nAGQ = 20)

# predicted completion for complex model
cons.mi <- rep("End of Life", 12)
cons <- c("C1", "C2", "C3", "C4", "C5", "C6", 
          "C7", "C8", "C9", "C10", "C11", "C12")
cons.sc <- rep(mean(consults$nu_assist_cons), 12)
cons.sei <- rep(mean(consults$secondary_issues), 12)
cons.time <- rep(mean(consults$total_time_in_hours), 12)

cons.mat <- as.data.frame(cbind(cons.mi, cons, cons.sc, cons.sei, cons.time))
colnames(cons.mat) <- c("Ethics.Issue", "lead_consultant", 
                        "nu_assist_cons", "secondary_issues",
                        "total_time_in_hours") 
cons.mat$nu_assist_cons <- as.numeric(cons.mat$nu_assist_cons)
cons.mat$secondary_issues <- as.numeric(cons.mat$secondary_issues)
cons.mat$total_time_in_hours <- as.numeric(cons.mat$total_time_in_hours)

pred_prob_logistic <- predict(glmm.fit.comp, 
                              newdata = cons.mat, 
                              type = "response")

# figure 5: primary results-----------------------------------------------------

###Alison's code###


# table 3: primary results with prediction intervals----------------------------

###Alison's code###


# secondary analysis------------------------------------------------------------

observed_quality <- consults %>%
                        group_by(lead_consultant) %>%
                        summarise(quality = mean(Quality.score))

# secondary model
lmm.qual <- lmer(Quality.score ~ Ethics.Issue + (1 | lead_consultant),
                 data = consults)

# predicted quality scores
pred_prob_qual <- predict(lmm.qual, newdata = cons.mat, type = "response")

# prediction intervals
preds_secondary <- predictInterval(lmm.qual, 
                                   newdata = as.data.frame(cons.mat),
                                   n.sims = 2000, 
                                   level = 0.95)

# sensitivity analyses for secondary outcome -----------------------------------

# simple model
lmm.qual.simple <- lmer(Quality.score ~ 1 + (1 | lead_consultant),
                        data = consults)

# predicted quality scores with simple model
simple.quality.scores <- coef(lmm.qual.simple)$lead_consultant["(Intercept)"]

# complex model
lmm.qual.comp <- lmer(Quality.score ~ Ethics.Issue + 
                        nu_assist_cons + secondary_issues + 
                        total_time_in_hours + (1 | lead_consultant),
                      data = consults)

# predicted quality scores for complex model
pred_prob_linear <- predict(lmm.qual.comp, newdata=cons.mat, type="response")

# figure 6: secondary results --------------------------------------------------

combo <- cbind(observed_quality, simple.quality.scores, pred_prob_qual)
combo_long <- reshape(combo, direction = "long", idvar = "lead_consultant",
                      varying = list(2:4))

ggplot(combo_long, aes(x = time,
                       y = quality,
                       col = lead_consultant,
                       label = lead_consultant)) +
  ylab("Average Quality Score") +
  geom_point(shape = 1) +
  ylim(3, 5) +
  scale_x_discrete(limits = factor(c(1, 2, 3)),
                   labels=c("Observed", 
                            "Predicted", 
                            "Predicted with adjustment"),
                   name=NULL) +
  geom_line() +
  labs(col = "Lead Consultant")

# table 4: secondary results with prediction intervals--------------------------
total_consults <- consults %>% 
                    group_by(lead_consultant) %>%
                    summarise(total = n(), avg.score = mean(Quality.score))

tbl5 <- cbind(total_consults, pred_prob_qual, preds_secondary[, c(3,2)]) %>%
          arrange(desc(total_consults[, 3]))

knitr::kable(tbl5, 
             col.names = c("Lead Consultant", 
                           "Total Number of Consultations",
                           "Obeserved Average Quality",
                           "Predicted Adjusted Average Quality", 
                           "95% CI Lower",
                           "95% CI Upper"), 
             digits = 2, 
             align = "l") %>%
                kableExtra::column_spec(2:6, width = "1in")

# figure 7: primary outcome sensitivity results --------------------------------

cons <- as.data.frame(cons)
combo <- cbind(pred_prob_simple, pred_prob, pred_prob_logistic, cons)

colnames(combo) <- (c("simple", "prim.model", "sens.model", "lead_consultant"))

# in combo_long, time=1 is predicted w/o adjustment, time=2 is adjusted 
# predicted, time=3 is complex sensitivity model
combo_long <- reshape(combo, direction = "long", idvar = "lead_consultant",
                      varying = list(1:3))
combo_long$lead_consultant <- factor(combo_long$lead_consultant,
                                     levels = c("C1", "C2", "C3", "C4", "C5", 
                                                "C6", "C7", "C8", "C9", "C10",
                                                "C11", "C12"))

ggplot(combo_long, aes(x = time,
                       y = simple,
                       col = lead_consultant,
                       label = lead_consultant)) +
  ylab("Proportion Complete") +
  geom_point(shape = 1) +
  ylim(0, 1) +
  xlim(1, 3) +
  scale_x_discrete(limits = factor(c(1, 2, 3)),
                   labels = c("Sensitivity model 1",
                              "Predicted w/ adj.",
                              "Sensitivity model 2"),
                   name = NULL) +
  geom_line() +
  labs(col = "Lead Consultant", title = "Completion Rates")


# figure 8: secondary outcome sensitivity results ------------------------------

combo <- cbind(simple.quality.scores, pred_prob_qual, pred_prob_linear, cons)
colnames(combo) <- (c("simple", "prim.model", "sens.model", "lead_consultant"))
combo_long <- reshape(combo, direction = "long", idvar = "lead_consultant",
                      varying = list(1:3))

# in combo_long, time=1 is predicted w/o adjustment, time=2 is adjusted 
# predicted, time=3 is complex sensitivity model
combo_long$lead_consultant <- factor(combo_long$lead_consultant,
                                     levels = c("C1", "C2", "C3", "C4", "C5",
                                                "C6", "C7", "C8", "C9", "C10",
                                                "C11", "C12"))

# note y axis is from 3 to 5
ggplot(combo_long, aes(x = time,
                       y = simple,
                       col = lead_consultant,
                       label = lead_consultant)) +
  ylab("Average Quality Score") +
  geom_point(shape = 1) +
  ylim(3, 5) +
  xlim(1, 3) +
  scale_x_discrete(limits = factor(c(1, 2, 3)),
                   labels = c("Sensitivity model 1",
                              "Predicted w/ adj.",
                              "Sensitivity model 2"), 
                   name=NULL) +
  geom_line() +
  labs(col = "Lead Consultant", title = "Quality Scores")
