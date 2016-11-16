data = read.csv("../summ_data.csv")
library(stargazer)

#has been listed
data$has_listed <- ifelse(data$listed_count ==0 , 0, 1)

formula = has_listed ~ n_male + n_ga + no_name_image + followers_count + accountAge_years + statuses_count + friends_count + has_url + 
  has_loc + has_descr +  descr_length +  verified + art + artist + bloggers + business + companies + design + info + journalists + life +
  marketing + media + music + news + organizations+ politics+ sports + technology+ tv+ world+ writers 

#gender
summary (m1 <- glm(formula, data = data, family="binomial"))

#female
summary (m2 <- glm(formula, data = subset(data, n_female ==1 | n_male ==1), family="binomial"))

stargazer(m1, m2, type = "text", dep.var.labels=c("Listed"), column.labels=c("Gender", "Female"),
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Number of followers", "Account age (years)", 
                             "Number of tweets", "Number of friends",  "Has URL", "Has location", 
                             "Has description", "Description Length", "Verified", 
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE , out="/Users/shirin/Desktop/gender/icwsm/icwsm-listed-gender-logit-topics-followers.txt")


stargazer(m1, m2, type = "latex", dep.var.labels=c("Listed"), column.labels=c("Gender", "Female"), 
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Number of followers", "Account age (years)", 
                             "Number of tweets", "Number of friends",  "Has URL", "Has location", 
                             "Has description", "Description Length", "Verified", 
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE , out="/Users/shirin/Desktop/gender/icwsm/icwsm-listed-gender-logit-topics-followers.tex")


exp(coef(m1))
exp(coef(m2))

# > exp(coef(m1))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 9.107071e-02     9.053977e-01     8.922202e-01     8.142481e-01     1.000850e+00     1.291868e+00     1.000028e+00     1.001063e+00     2.597972e+00     1.141619e+00 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.487768e+00     1.000989e+00     1.772402e+06     1.296087e+05     1.254131e+05     4.555621e+05     4.666935e+05     1.326804e+05     4.392795e+05     1.522357e+05 
# journalists             life        marketing            media            music             news    organizations         politics           sports       technology 
# 3.161920e+03     1.039764e+06     1.659896e+05     2.503330e+05     4.347140e+05     1.861926e+05     2.300067e+05     8.241904e+05     2.715071e+05     5.187214e+05 
# tv            world          writers 
# 4.754078e+04     3.119356e+05     2.738848e+05 
# > exp(coef(m2))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 7.421041e-02     9.091431e-01               NA               NA     1.000957e+00     1.306830e+00     1.000033e+00     1.001284e+00     2.534271e+00     1.170365e+00 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.412544e+00     1.002445e+00     4.021121e+06     5.142037e+05     3.676111e+05     1.318315e+06     8.423434e+05     1.688811e+05     8.502053e+05     1.004399e+05 
# journalists             life        marketing            media            music             news    organizations         politics           sports       technology 
# 8.672347e-01     3.060231e+06     4.813688e+05     7.367661e+05     5.935525e+05     3.114163e+05     1.409603e+06     2.383567e+06     5.836932e+05     1.381185e+06 
# tv            world          writers 
# 1.085839e+05     7.728063e+05     7.321578e+05 




