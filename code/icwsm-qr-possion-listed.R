data = read.csv("../summ_data.csv")
library(stargazer)

formula = listed_count ~ n_male  + followers_count + accountAge_years + statuses_count + friends_count + has_url + has_loc +
  has_descr + descr_length + verified+ art + artist + bloggers + business + companies + design + info + journalists + life + marketing +
  media + music + news + organizations + politics + sports + technology + tv + world + writers
#female
summary(m0 <- glm(formula , data = subset(data, n_female == 1 | n_male == 1), family="poisson"))
#summary (m1 <- glm(user_listed_count ~ male + accountAge_years + user_statuses_count + user_friends_count + has_url + has_loc + has_descr + description_length + verified+ art + artist + bloggers + business + companies + design + info + journalists + life + marketing + media + music + news + organizations+ politics+ technology+ tv+ world+ writers, subset(data, (female == 1 | male == 1) & qr_listed == 1), family="poisson"))
#summary (m2 <- glm(user_listed_count ~ male + accountAge_years + user_statuses_count + user_friends_count + has_url + has_loc + has_descr + description_length + verified+ art + artist + bloggers + business + companies + design + info + journalists + life + marketing + media + music + news + organizations+ politics+ technology+ tv+ world+ writers, subset(data, (female == 1 | male == 1) & qr_listed == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_listed == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_listed == 4), family="poisson"))

stargazer(m0, m3, m4, type = "text", dep.var.labels=c("Listed count"), column.labels=c("Poisson", "0.75 Qnt.", "1.00 Qnt."),
          covariate.labels=c("Male", "Number of followers", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", "Has location",
                             "Has description", "Description length", "Verified", 
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out = "/Users/shirin/Desktop/gender/icwsm/icwsm-listed-female-rq-poisson-topics-followers.txt")

stargazer(m0, m3, m4, type = "latex", dep.var.labels=c("Listed count"), column.labels=c("Poisson", "0.75 Qnt.", "1.00 Qnt."),
          covariate.labels=c("Male", "Number of followers", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", "Has location", 
                             "Has description", "Description length", "Verified",
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-listed-female-rq-poisson-topics-followers.tex")


#gender
formula = listed_count ~ n_male + n_ga + no_name_image + followers_count + accountAge_years + statuses_count + friends_count + has_url + has_loc +
  has_descr + descr_length + verified+ art + artist + bloggers + business + companies + design + info + journalists + life + marketing +
  media + music + news + organizations + politics + sports + technology + tv + world + writers

summary (m0 <- glm(formula, data = data, family="poisson"))
#summary (m1 <- glm(user_listed_count ~ n_male + n_ga + no_name_image + accountAge_years + user_statuses_count + user_friends_count + has_url+  has_loc + has_descr + description_length + verified + art + artist + bloggers + business + companies + design + info + journalists + life + marketing + media + music + news + organizations + politics+ technology+ tv+ world+ writers, subset(data, qr_listed == 1), family="poisson"))
#summary (m2 <- glm(user_listed_count ~ n_male + n_ga + no_name_image + accountAge_years + user_statuses_count + user_friends_count + has_url+ has_loc + has_descr + description_length + verified + art + artist + bloggers + business + companies + design + info + journalists + life + marketing + media + music + news + organizations + politics+ technology+ tv+ world+ writers, subset(data, qr_listed == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, qr_listed == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, qr_listed == 4), family="poisson"))

stargazer(m0, m3, m4, type = "text", dep.var.labels=c("Listed count"), column.labels=c("Poisson", "0.75 Qnt.", "0.95 Qnt."), 
          covariate.labels=c("Male", "Gender ambiguous", "No name and image", "Number of followers", "Account age (years)", "Number of tweets", 
                             "Number of followings",  "Has URL", "Has location", "Has description", "Description length", 
                             "Verified", "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics","Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-listed-gender-rq-possion-topics-followers.txt")

stargazer(m0, m3, m4, type = "latex", dep.var.labels=c("Listed count"), column.labels=c("Poisson", "0.75 Qnt.", "0.95 Qnt."), 
          covariate.labels=c("Male", "Gender ambiguous", "No name and image", "Number of followers", "Account age (years)", "Number of tweets", 
                             "Number of followings",  "Has URL", "Has location", "Has description", "Description length", "Verified",
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists", 
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-listed-gender-rq-possion-topics-followers.tex")


#Female vs. male
#> exp(coef(m0))
# (Intercept)           n_male  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.3301784        0.7888353        1.0000001        1.3061594        1.0000114        1.0000182        2.8173342        1.1869114 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies 
# 2.3617363        1.0018683        6.5726867        0.7671336        1.3083630        1.5131457        0.9235429        0.8980488 
# design             info      journalists             life        marketing            media            music             news 
# 2.0692716        1.4861536        0.6518767        1.8582474        1.3585311        1.1651359        1.1079586        3.3309195 
# organizations         politics           sports       technology               tv            world          writers 
# 1.2839789        2.2098432        1.4143997        1.2903328        0.9706605        1.9215572        1.3249996 
# > exp(coef(m3))
# (Intercept)           n_male  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.9223092        1.0044109        1.0000026        1.0564812        1.0000020        1.0000827        1.0635869        1.0032161 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies 
# 1.0540027        1.0002073               NA               NA               NA               NA               NA               NA 
# design             info      journalists             life        marketing            media            music             news 
# NA               NA               NA               NA               NA               NA               NA               NA 
# organizations         politics           sports       technology               tv            world          writers 
# NA               NA               NA               NA               NA               NA               NA 
# > exp(coef(m4))
# (Intercept)           n_male  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 4.3719186        0.8331639        1.0000001        1.1241728        1.0000093        1.0000192        1.5683650        1.1038439 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies 
# 1.3433846        1.0010384        5.3474060        0.7818154        1.2471785        1.6106034        0.8881905        0.9381027 
# design             info      journalists             life        marketing            media            music             news 
# 1.9036392        1.3273481        0.6927806        1.6969571        1.4008646        1.0983396        1.0468871        2.7610529 
# organizations         politics           sports       technology               tv            world          writers 
# 1.1356857        2.0104657        1.1550249        1.2827244        1.1141394        1.8350416        1.2988322 


#Female vs. Others
exp(coef(m0))
exp(coef(m3))
exp(coef(m4))

# exp(coef(m0))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.3829104        0.7884143        0.9621288        0.9385139        1.0000001        1.3199632        1.0000049        1.0000105        2.9501148        1.0768217 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 2.3886925        1.0010167        7.0248512        0.8661500        1.1183922        1.6261752        0.9286132        1.6416232        2.4947192        1.8955398 
# journalists             life        marketing            media            music             news    organizations         politics           sports       technology 
# 0.5986408        2.0436160        0.8559926        1.4354308        1.3483487        2.7882505        0.8213369        1.7801567        1.3605978        1.4886688 
# tv            world          writers 
# 0.9461478        2.2891235        1.3517591 
# > exp(coef(m3))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.9151287        1.0049309        1.0104046        0.9966372        1.0000042        1.0582375        1.0000020        1.0000800        1.0642837        1.0009260 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.0687515        1.0000557               NA               NA               NA               NA               NA               NA               NA        1.5074142 
# journalists             life        marketing            media            music             news    organizations         politics           sports       technology 
# NA               NA               NA               NA               NA               NA               NA               NA               NA               NA 
# tv            world          writers 
# NA               NA               NA 
# > exp(coef(m4))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 4.9155315        0.8191660        1.0083340        0.9644604        1.0000001        1.1357614        1.0000040        1.0000090        1.5208334        0.9573124 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.5141400        1.0003827        6.3673812        0.8290564        1.0795109        1.5695519        0.9381330        1.6226868        2.4810544        1.9047782 
# journalists             life        marketing            media            music             news    organizations         politics           sports       technology 
# 0.6196846        1.8630860        0.8516600        1.4218541        1.2293378        2.2519750        0.7886865        1.6021813        1.0710512        1.4700081 
# tv            world          writers 
# 1.0266281        2.2094618        1.3484863 



