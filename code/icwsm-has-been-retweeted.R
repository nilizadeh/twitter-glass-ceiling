data = read.csv("../summ_data.csv")
library(stargazer)

#has been retweet
data$retweeted <- ifelse(data$retweeted_count>0 , 1, 0)
formula = retweeted ~ n_male + n_ga + no_name_image + followers_count + accountAge_years + statuses_count + friends_count + has_url + 
  has_loc + has_descr +  descr_length +  verified + art + artist + bloggers + business + companies + design + info + journalists + life +
  marketing + media + music + news + organizations+ politics+ technology+ tv+ world+ writers + sports

#gender
summary (m1 <- glm(formula, data = data, family="binomial"))

#female
summary (m2 <- glm(formula, data = subset(data, n_female ==1 | n_male ==1), family="binomial"))

stargazer(m1, m2, type = "text", dep.var.labels=c("Retweeted"), column.labels=c("Gender", "Female"),
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Number of followers", "Account age (years)", 
                             "Number of tweets", "Number of friends",  "Has URL", "Has location", 
                             "Has description", "Description Length", "Verified", 
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Technology", "TV", "World", "Writers", "Sports"),  no.space=TRUE , out="/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-gender-logit-topics-followers.txt")


stargazer(m1, m2, type = "latex", dep.var.labels=c("Retweeted"), column.labels=c("Gender", "Female"), 
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Number of followers", "Account age (years)", 
                             "Number of tweets", "Number of friends",  "Has URL", "Has location", 
                             "Has description", "Description Length", "Verified", 
                             "Art", "Artist", "Bloggers", "Business", "Companies", "Design", "Info", "Journalists",
                             "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Technology", "TV", "World", "Writers"),  no.space=TRUE , out="/Users/shirin/Desktop/gender/icwsm/icwsm-retweeted-gender-logit-topics-followers.tex")

exp(coef(m1))
exp(coef(m2))

# exp(coef(m1))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.2350695        0.9670665        1.1408573        1.2424356        1.0002315        0.8550611        1.0000568        1.0000937        1.2412499        0.8734459 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.7752694        0.9997345        3.8810801        0.5723617        1.5353044        0.8465724        1.0138685        1.5403540        1.0800131        1.6427602 
# journalists             life        marketing            media            music             news    organizations         politics       technology               tv 
# 1.5130247        0.9120550        0.7275858        1.3306354        0.8666806        1.4278552        1.5572519        1.5803810        1.2155179        1.3215334 
# world          writers           sports 
# 0.8615649        1.6893010        1.3964656 
# > exp(coef(m2))
# (Intercept)           n_male             n_ga    no_name_image  followers_count accountAge_years   statuses_count    friends_count          has_url          has_loc 
# 0.2301386        0.9721761               NA               NA        1.0002535        0.8650701        1.0000639        1.0000418        1.2037437        0.8544473 
# has_descr     descr_length         verified              art           artist         bloggers         business        companies           design             info 
# 1.6603877        1.0005026        3.1428662        0.7170831        1.3884542        0.7833491        1.0020243        1.5032690        1.0662765        1.4219130 
# journalists             life        marketing            media            music             news    organizations         politics       technology               tv 
# 1.5674853        0.8552302        0.7420396        1.4095414        0.8159590        1.4913451        1.6768904        1.2968811        1.2326411        1.1924268 
# world          writers           sports 
# 0.8262284        1.6301661        1.4148885 
# > 
