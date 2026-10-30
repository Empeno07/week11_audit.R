install.packages(c("car", "lmtest", "sandwich"))

library(car)
library(lmtest)
library(sandwich)

loans <- read.csv("loan_scores.csv")
str(loans)

# group is for the audit only; it is NOT a model input

fit <- lm(score ~ income_k + spend_k + years_employed + zone2, data = loans); summary(fit)

# Intercept 571.09, income_k 1.212, spend_k 1.169, years_employed 3.338,
# zone2 -36.07. RSE 26.20 on 145 df. R2 = 0.5641, adjusted R2 = 0.5521.
# Only years_employed and zone2 are significant, even though income should matter -> first hint of a multicollinearity problem.

par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

# Residuals vs Fitted: WARNING. The red line is roughly flat, but the spread is narrow on the left and wider on the right (funnel shape).
# Normal Q-Q: HEALTHY. Points follow the diagonal line closely, so the residuals look normal.
# Scale-Location: WARNING. The red line slopes upward instead of being flat, so the variance grows with the fitted score.
# Residuals vs Leverage: HEALTHY. No point goes near the Cook's distance lines; no single applicant dominates the fit.

shapiro.test(resid(fit))   # normality of residuals
bptest(fit)                # constant variance
vif_tab <- data.frame(vif = round(vif(fit), 2))
vif_tab
max(cooks.distance(fit))   # influence
cor(loans$income_k, loans$spend_k)   # about 0.97

# Shapiro-Wilk: W = 0.993, p = 0.68 -> cannot reject normality.
# MET (supported by the Q-Q plot).
# Breusch-Pagan: BP = 16.09, df = 4, p = 0.0029 -> reject constant variance. NOT MET (funnel in Residuals vs Fitted and the rising line in Scale-Location).
# VIF: income_k 18.14, spend_k 18.11, years_employed 1.01, zone2 1.02.
# income_k and spend_k are far above the 5-10 cut-off. NOT MET for those two.
# Max Cook's distance is about 0.071, far below 1 -> no influential point. MET (supported by the leverage plot).

# Why drop spend_k: it carries almost the same information as income_k
# (r = 0.97). I keep income because it is the more direct measure of ability to pay.

fit2 <- lm(score ~ income_k + years_employed + zone2, data = loans)
summary(fit2)
vif(fit2)
bptest(fit2)

# Effect of dropping spend_k:
#  - income_k is now 1.928 with SE 0.199 (was 1.212 with SE 0.840).
#  - All VIFs are about 1.0 -> multicollinearity FIXED.
#  - Fit is almost unchanged (adjusted R2 0.5528 vs 0.5521).
#  - Breusch-Pagan is still significant (BP = 15.13, p = 0.0017), so unequal variance is NOT fixed by this step.

# Robust (HC3) standard errors for the heteroscedasticity

coeftest(fit2, vcov = vcovHC(fit2, type = "HC3"))

# Robust SEs: intercept 7.18, income_k 0.224, years_employed 0.564,
# zone2 4.53. Same conclusions, but the SEs are now defensible.

# What each remedy fixed / did not fix:
#  - Dropping spend_k fixed multicollinearity, not heteroscedasticity.
#  - HC3 errors fix the inference problem, but not the funnel itself.

tapply(resid(fit), loans$group, mean)        # mean residual by group
table(loans$group, loans$zone2)              # zone2 by group
cor(loans$zone2, loans$group == "B")         # proxy check
tapply(loans$score, loans$group, mean)       # mean score by group

# Mean residual: A = +5.85, B = -8.53. The model OVER-predicts for A and UNDER-predicts for B.
# Zone 2 holds 15 of 89 in group A (17%) but 49 of 61 in group B (80%). Correlation of zone2 with group B = 0.63.
# Mean score: A = 651.8, B = 617.4 (gap of about 34 points).
# Meaning: the model never saw group, but zone2 acts as a proxy for it. The zone2 coefficient is about -36 points, so a person loses 36
# points just for living in Zone 2, regardless of income or years of work. Most of group B lives there, so group B loses those points
# far more often. Where someone lives is not a fair way to judge a person's ability to repay.

# --- Data representativeness
# 150 applicants: 89 in group A and 61 in group B. Small and synthetic, so it may not match the real applicant population. 80% of group B
# lives in Zone 2, so there is little information about group B outside Zone 2 (12 people) and group A inside Zone 2 (15 people).
# Nothing records WHY incomes are low, and there is no repayment record.

# --- Bias and proxies
# group is not an input, but zone2 acts as a proxy (r = 0.63). There is a -36 point zone2 penalty and errors differ by group (+5.85 vs -8.53).
# Income and years employed are almost equal across groups, so the 34-point gap is carried by the location variable. Passing the tests
# does not mean the model is fair.

# --- Who could be harmed
# Zone 2 residents (mostly group B) could be denied loans or given worse terms because of their address, not their ability to pay. Low or
# irregular earners and new workers are also at risk. A denial can hurt credit history and cause more denials.

# --- Data privacy (RA 10173)
# Collect only needed data, tell applicants how it is used, limit access, use codes instead of applicant_id when auditing, keep data only as
# long as needed, and let applicants see and correct their data.

# --- Decision: FIX (do not deploy as it is)
# The zone2 penalty and unequal errors by group are not acceptable, but the income and years-employed parts are stable (adjusted R2 ~ 0.55).
# Conditions / what would change my mind:
#  1. Test zone2 as a proxy: refit without it and compare group errors.
#  2. Add a human review step for borderline and Zone 2 rejections.
#  3. Give applicants a way to appeal and see the main reasons.
#  4. Collect repayment data and re-audit every year.
# If bias remains after these fixes, I would retire the model.