
# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("combined_pbp", "con")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

# Clean dataset for modeling
xpass_prediction_df <- combined_pbp %>%
  select(pass_attempt, xpass, pbp_predicted_xpass, part_predicted_xpass) %>%
  filter(!is.na(pass_attempt))

#######################
# MODEL 1: Full model (for seasons with participation data: 2016-2023)
#######################

xpass_full_df <- xpass_prediction_df %>%
  filter(!is.na(xpass), !is.na(pbp_predicted_xpass), !is.na(part_predicted_xpass))

nrow(xpass_full_df)

# Correlation matrix - still useful to see multicollinearity
cor_matrix <- cor(xpass_full_df %>% dplyr::select(xpass, pbp_predicted_xpass, part_predicted_xpass))
print(round(cor_matrix, 3))

# Full GLM
glm_full <- glm(pass_attempt ~ xpass + pbp_predicted_xpass + part_predicted_xpass, 
                data = xpass_full_df, 
                family = binomial(link = "logit"))
summary(glm_full)

# VIF check - still works for glm
vif(glm_full)

# Stepwise selection via AIC (MASS::stepAIC works for glm)
step_full <- stepAIC(glm_full, direction = "both", trace = TRUE)
summary(step_full)

# Backward only
step_back <- stepAIC(glm_full, direction = "backward", trace = TRUE)
summary(step_back)


#######################
# MODEL 2: No participation (for 2024-)
#######################

xpass_nopart_df <- xpass_prediction_df %>%
  filter(!is.na(xpass), !is.na(pbp_predicted_xpass))

cor(xpass_nopart_df$xpass, xpass_nopart_df$pbp_predicted_xpass, use = "complete.obs")

glm_nopart <- glm(pass_attempt ~ xpass + pbp_predicted_xpass, 
                  data = xpass_nopart_df, 
                  family = binomial(link = "logit"))
summary(glm_nopart)

vif(glm_nopart)

step_nopart <- stepAIC(glm_nopart, direction = "both", trace = TRUE)
summary(step_nopart)


#######################
# COMPARE MODELS - does participation actually help?
#######################

# Subset to same data for fair comparison
glm_full_subset <- glm(pass_attempt ~ xpass + pbp_predicted_xpass + part_predicted_xpass, 
                       data = xpass_full_df, family = binomial)
glm_nopart_subset <- glm(pass_attempt ~ xpass + pbp_predicted_xpass, 
                         data = xpass_full_df, family = binomial)

# Likelihood ratio test (nested model comparison for glm)
anova(glm_nopart_subset, glm_full_subset, test = "Chisq")

# AIC/BIC comparison
AIC(glm_nopart_subset, glm_full_subset)
BIC(glm_nopart_subset, glm_full_subset)

# Pseudo R-squared (McFadden's)
null_model <- glm(pass_attempt ~ 1, data = xpass_full_df, family = binomial)
1 - logLik(glm_full_subset) / logLik(null_model)    # full
1 - logLik(glm_nopart_subset) / logLik(null_model)  # no part
