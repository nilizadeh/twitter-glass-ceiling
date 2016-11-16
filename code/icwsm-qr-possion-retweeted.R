data = read.csv("../summ_data.csv")
install.packages("stargazer")

formula = retweeted_count ~ n_male  + accountAge_years + statuses_count + friends_count + has_url + has_loc + has_descr +
  descr_length + verified + art + artist + bloggers + business + companies + design + info + journalists + life + marketing +
  media + music + news + organizations + politics + sports + technology + tv + world + writers 

#n_female
summary(m0 <- glm(formula, data = subset(data, n_female == 1 | n_male == 1), family="poisson"))
#summary (m1 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_retweet == 1), family="poisson"))
#summary (m2 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_retweet == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_retweet == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_retweet == 4), family="poisson"))

stargazer(m0, m3, m4, type = "text", dep.var.labels=c("Retweeted count"), column.labels=c("Poisson", "0.75 Qnt.", "1.00 Qnt."), 
          covariate.labels=c("Male", "Number of followers", "Account age (years)", "Number of tweets", "Number of friends",
                             "Has URL", "Has location", "Has description", "Description length", "Verified",
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out = "/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-female-rq-poisson-topics-followers.txt")

stargazer(m0, m3, m4, type = "latex", dep.var.labels=c("Retweeted count"), column.labels=c("Poisson", "0.75 Qnt.", "0.95 Qnt."),
          covariate.labels=c("Male", "Number of followers", "Account age (years)", "Number of tweets", "Number of friends",
                             "Has URL", "Has location", "Has description", "Description length", "Verified",
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-female-rq-poisson-topics-followers.tex")

#gender
formula = retweeted_count ~ n_male  + n_ga + no_name_image + accountAge_years + statuses_count + friends_count + has_url + has_loc + has_descr +
  descr_length + verified + art + artist + bloggers + business + companies + design + info + journalists + life + marketing +
  media + music + news + organizations + politics + sports + technology + tv + world + writers 

summary (m0 <- glm(formula, data = data, family="poisson"))
#summary (m1 <- glm(formula, subset(data, qr_retweet == 1), family="poisson"))
#summary (m2 <- glm(formula, subset(data, qr_retweet == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, qr_retweet == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, qr_retweet == 4), family="poisson"))

stargazer(m0, m3, m4, type = "text", dep.var.labels=c("Retweeted count"), column.labels=c("Poisson", "0.75 Qnt.", "1.00 Qnt."),
            covariate.labels=c("Male", "Gender ambiguous", "Unknown", "Account age (years)", "Number of tweets", "Number of friends",
                               "Has URL", "Has location", "Has description", "Description length", "Verified", 
                               "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                               "Life", "Marketing", "Media", "Music", "News", "Organizations",
                               "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-gender-rq-poisson-topics-followers.txt")

stargazer(m0, m3, m4, type = "latex", dep.var.labels=c("Retweeted count"), column.labels=c("Poisson", "0.75 Qnt.", "1.00 Qnt."),
            covariate.labels=c("Male", "Gender ambiguous", "Unknown", "Account age (years)", "Number of tweets", "Number of friends",  
                               "Has URL", "Has location", "Has description", "Description length", "Verified",
                               "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                               "Life", "Marketing", "Media", "Music", "News", "Organizations",
                               "Politics", "Technology", "TV", "World", "Writers"),  no.space=TRUE, 
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-gender-rq-poisson-topics-followers.tex")

#Female vs Male
exp(coef(m0))
exp(coef(m3))
exp(coef(m4))
c(,,0.98,1.16)
# > exp(coef(m0))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 1.5910318        1.0296755        0.7786446        1.0000162        1.0000319        1.7452225        0.6670028        3.0203264 
# descr_length         verified              art           artist         bloggers         business        companies           design 
# 1.0019411        9.4478167        0.4492710        1.1315154        1.5734863        1.0294212        0.3985431        1.1062629 
# info      journalists             life        marketing            media            music             news    organizations 
# 2.1634029        1.0173053        3.7611289        0.4241003        0.8996321        1.6351316        3.2924202        1.4758769 
# politics           sports       technology               tv            world          writers 
# 2.8227702        0.7283952        0.5241097        1.4551833        0.8533155        1.0746614 
# > exp(coef(m3))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 0.03554589       0.97894192       1.82304946       1.00000440       1.00002597       0.83141478       1.03504039       1.19689851 
# descr_length         verified              art           artist         bloggers         business        companies           design 
# 1.00005191       1.26538029       0.86167560       0.78721383       0.81850894       0.93548634       0.88348328       0.83091025 
# info      journalists             life        marketing            media            music             news    organizations 
# 0.82605280       1.48812044       0.76701512       0.95097052       0.72145234       1.01998524       0.90057269       1.09749342 
# politics           sports       technology               tv            world          writers 
# 1.07726454       1.25795977       0.60112370       0.80823854       0.85650087       0.76804826 
# > exp(coef(m4))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 8.3018122        1.1639288        0.8759032        1.0000111        1.0000255        1.3196144        0.7230184        1.7196522 
# descr_length         verified              art           artist         bloggers         business        companies           design 
# 1.0017413        4.7009646        0.3954321        0.9865342        1.4538517        0.9976255        0.3945495        0.8451035 
# info      journalists             life        marketing            media            music             news    organizations 
# 1.7895898        0.9953867        2.6625425        0.3568531        0.8450418        1.6272325        2.0944771        1.0073385 
# politics           sports       technology               tv            world          writers 
# 2.0299936        0.6189958        0.5087895        1.4539083        1.0178316        0.9798311 
