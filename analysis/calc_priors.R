library(esc)

# self-integrity scale (Sherman et al. 2009, Study 2)

# Page 755:
# Participants in the self-affirmation condition scored higher 
# on the Self-Integrity Scale (M = 6.17, SE = 0.13) 
# than did those in the no-affirmation condition 
# (M = 5.69, SE = 0.14, p = .012).

# The participants ... 
# consisted of 52 men and 36 women, 
# and included 65 European Americans, 
# 5 African Americans, 6 Latinos, 
# and 15 other/missing data (3 people declined to report all demographics)
# Participants were randomly assigned to one of three conditions: 
# self-affirmation condition, no-affirmation condition, 
# and awareness + affirmation condition.

# So I am assuming the N in affirmation vs. control is (91-3)/3 = 29

esc_mean_se(grp1m = 6.17, grp1se = 0.13, grp1n = 29,
            grp2m = 5.69, grp2se = 0.14, grp2n = 29,
            es.type = "g")
# normal(0.6624, 0.2699)

# self-esteem scale (Cohen & Garcia, 2005)


# Belongingness (Cook et al. 2012 using data from Cohen et al., 2009)
# For these historically low-achieving students, 
# academic belonging was higher in the affirmation than control condition,
# gamma = .21, t(168) = 3.29, p = .002.

# normal(0.21, 0.21/3.29)

