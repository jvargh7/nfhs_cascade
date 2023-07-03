source("preprocessing/ncpre04_nfhs5 diabetes svydesign.R")
source("preprocessing/ncpre05_nfhs5 diagnosed svydesign.R")

library(lme4)

# Used for JAMA IM paper: We lump all respondents in same state as similar (after adjusting for socio-demographics)
mod_diagnosed <- glmer(dm_diagnosed ~ age_category + education + sex + residence + wealthq_ur + (1|state),data=nfhs5dm_df,family = binomial())
summary(mod_diagnosed)
1 - performance::icc(mod_diagnosed)

mod_treated <- glmer(dm_treated ~ age_category + education + sex + residence + wealthq_ur  + (1|state),data=nfhs5dmdiag_df,family = binomial())
summary(mod_treated)
1 - performance::icc(mod_treated)


mod_controlled <- glmer(dm_controlled ~ age_category + education + sex + residence + wealthq_ur  + (1|state),data=nfhs5dmdiag_df,family = binomial())
summary(mod_controlled)
1 - performance::icc(mod_controlled)

# Used for JAMA IM paper: We partition variance at different levels --------
mod_treated2 <- glmer(dm_treated ~ age_category + education + sex + residence + wealthq_ur  + (1|state/district_df/cluster),data=nfhs5dmdiag_df,family = binomial())
# mod_treated2 = mod_treated
summary(mod_treated2)
1 - performance::icc(mod_treated2,by_group = TRUE)

