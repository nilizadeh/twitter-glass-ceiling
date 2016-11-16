data = read.csv("../summ_data.csv")
#install.packages("stargazer")
library(stargazer)
#install.packages("effsize")
library(effsize)

max(data$followers_count[which(data$qr_followers == 1)])
68
max(data$followers_count[which(data$qr_followers == 2)])
204
max(data$followers_count[which(data$qr_followers == 3)])
478
max(data$followers_count[which(data$qr_followers == 4)])
42391843

max(data$listed_count[which(data$qr_listed == 1)])
0
max(data$listed_count[which(data$qr_listed == 2)])
0
max(data$listed_count[which(data$qr_listed == 3)])
2
max(data$listed_count[which(data$qr_listed == 4)])
76290

max(data$retweeted_count[which(data$qr_retweet == 1)])
0
max(data$retweeted_count[which(data$qr_retweet == 2)])
0
max(data$retweeted_count[which(data$qr_retweet == 3)])
1
max(data$retweeted_count[which(data$qr_retweet == 4)])
46388

min(data$followers_count[which(data$qr_followers == 1)])
1
min(data$followers_count[which(data$qr_followers == 2)])
68
min(data$followers_count[which(data$qr_followers == 3)])
204
min(data$followers_count[which(data$qr_followers == 4)])
478

min(data$listed_count[which(data$qr_listed == 1)])
0
min(data$listed_count[which(data$qr_listed == 2)])
0
min(data$listed_count[which(data$qr_listed == 3)])
0
min(data$listed_count[which(data$qr_listed == 4)])
2

min(data$retweeted_count[which(data$qr_retweet == 1)])
0
min(data$retweeted_count[which(data$qr_retweet == 2)])
0
min(data$retweeted_count[which(data$qr_retweet == 3)])
0
min(data$retweeted_count[which(data$qr_retweet == 4)])
1

formula = followers_count ~ n_male + accountAge_years + statuses_count + friends_count + 
  has_url+ has_loc + has_descr + descr_length + verified + art + artist + bloggers +
  business + companies + design + info + journalists + life + marketing + media + music + news + 
  organizations + politics + sports + technology + tv + world + writers

#Analysis, include controling for topics
#n_female
summary (m0 <- glm(formula, data = subset(data, n_female == 1 | n_male == 1), family="poisson"))
summary (m1 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_followers == 1), family="poisson"))
summary (m2 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_followers == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_followers == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, (n_female == 1 | n_male == 1) & qr_followers == 4), family="poisson"))


cohen.d(formula, data = subset(data, n_female == 1 | n_male == 1), family="poisson")
library(rms)
mod1b <- lrm(formula, data = subset(data, n_female == 1 | n_male == 1), family="poisson")

stargazer(m0, m1, m2, m3, m4, type = "text", dep.var.labels=c("Number of Followers"), column.labels=c("Poisson", "0.25 Qnt.","0.5 Qnt.", "0.75 Qnt.", "1.00 Qnt."), 
          covariate.labels=c("Male", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", 
                             "Has location", "Has description", "Description length", "Verified", "Art", "Artist", 
                             "Bloggers", "Business", "Companies", "Design", "Info", "Journalists", "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics","Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE
                              , out="/Users/shirin/Desktop/gender/icwsm/icwsm-followers-female-rq-possion-topics.txt")

stargazer(m0, m1, m2, m3, m4, type = "latex", dep.var.labels=c("Number of Followers"), column.labels=c("Poisson", "0.25 Qnt.","0.5 Qnt.", "0.75 Qnt.", "1.00 Qnt."), 
          covariate.labels=c("Male", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", 
                             "Has location", "Has description", "Description length", "Verified", "Art", "Artist", 
                             "Bloggers", "Business", "Companies", "Design", "Info", "Journalists", "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE , 
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-followers-female-rq-possion-topics.tex")


#gender
formula = followers_count ~ n_male + n_ga + no_name_image  + accountAge_years + statuses_count + friends_count + 
  has_url+ has_loc + has_descr + descr_length + verified + art + artist + bloggers +
  business + companies + design + info + journalists + life + marketing + media + music + news + 
  organizations + politics + sports + technology + tv + world + writers

summary (m0 <- glm(formula, data = data, family="poisson"))
summary (m1 <- glm(formula, subset(data, qr_followers == 1), family="poisson"))
summary (m2 <- glm(formula, subset(data, qr_followers == 2), family="poisson"))
summary (m3 <- glm(formula, subset(data, qr_followers == 3), family="poisson"))
summary (m4 <- glm(formula, subset(data, qr_followers == 4), family="poisson"))

stargazer(m0, m1, m2, m3, m4, type = "text", dep.var.labels=c("Number of Followers"), column.labels=c("Poisson", "0.25 Qnt.","0.5 Qnt.", "0.75 Qnt.", "1.00 Qnt."), 
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", 
                             "Has location", "Has description", "Description length", "Verified", "Art", "Artist", 
                             "Bloggers", "Business", "Companies", "Design", "Info", "Journalists", "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports", "Technology", "TV", "World", "Writers"),  no.space=TRUE ,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-followers-gender-rq-possion-topics.txt")

stargazer(m0, m1, m2, m3, m4, type = "latex", dep.var.labels=c("Number of Followers"), column.labels=c("Poisson", "0.25 Qnt.","0.5 Qnt.", "0.75 Qnt.", "0.95 Qnt."), 
          covariate.labels=c("Male", "Ambiguous", "Unknown", "Account age (years)", "Number of tweets", "Number of friends",  "Has URL", 
                             "Has location", "Has description", "Description length", "Verified", "Art", "Artist", 
                             "Bloggers", "Business", "Companies", "Design", "Info", "Journalists", "Life", "Marketing", "Media", "Music", "News", "Organizations",
                             "Politics", "Sports","Technology", "TV", "World", "Writers"),  no.space=TRUE ,
          out="/Users/shirin/Desktop/gender/icwsm/icwsm-followers-gender-rq-possion-topics.tex")


# Incidence Rate Ratios 

# Female vs Male
exp(coef(m0))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr     descr_length         verified 
# 91.4365738        1.9900419        1.1041515        1.0000116        1.0000184        2.0409500        0.9527666        2.7228215        0.9957656       20.3000565 
# art           artist         bloggers         business        companies           design             info      journalists             life        marketing 
# 0.1159145        2.0532586        1.4948786        0.6283216        1.0742225        3.3439367        1.1664672        0.6121498        7.1096312        0.9260560 
# media            music             news    organizations         politics           sports       technology               tv            world          writers 
# 0.2542480        1.4166244        3.7240106        0.5927547        0.9659872        0.6212094        0.7011035        4.9252337        2.6406943        0.3881622 

exp(coef(m1))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr     descr_length         verified 
# 17.0811946        0.9321348        1.0329648        1.0000238        1.0032537        1.1442069        0.8604758        1.2847769        1.0003219               NA 
# art           artist         bloggers         business        companies           design             info      journalists             life        marketing 
# NA               NA               NA               NA               NA               NA               NA               NA               NA               NA 
# media            music             news    organizations         politics           sports       technology               tv            world          writers 
# NA               NA               NA               NA               NA               NA               NA               NA               NA               NA           

exp(coef(m2))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr     descr_length         verified 
# 112.5636135        0.9886295        1.0063591        1.0000103        1.0003313        0.9954344        0.9828790        1.0365310        1.0000253               NA 
# art           artist         bloggers         business        companies           design             info      journalists             life        marketing 
# 0.9165715        0.7840107        1.2434204        1.4111890        0.8834532               NA        0.9751625               NA        1.0140033        1.5299599 
# media            music             news    organizations         politics           sports       technology               tv            world          writers 
# 1.4625832               NA        0.7864087               NA               NA               NA        1.3144233               NA        1.0359060        1.4565431 

exp(coef(m3))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr     descr_length         verified 
# 285.2081666        0.9861687        0.9988950        1.0000055        1.0001530        1.0072177        0.9874375        1.0268357        0.9999985        1.0173059 
# art           artist         bloggers         business        companies           design             info      journalists             life        marketing 
# 1.0219623        0.9503698        0.9554306        1.0836890        1.0284378        0.9973625        0.9104017        0.9092490        0.9852080        1.0192301 
# media            music             news    organizations         politics           sports       technology               tv            world          writers 
# 1.1067416        1.0374163        1.0843733        1.0425735        1.0149877        0.9765849        1.0156464        0.9396681        1.1380781        1.1234783 

exp(coef(m4))
# (Intercept)           n_male accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr     descr_length         verified 
# 868.4335092        2.2475505        0.9773595        1.0000055        1.0000190        1.4419835        1.0925953        1.6813471        0.9938244       11.5347085 
# art           artist         bloggers         business        companies           design             info      journalists             life        marketing 
# 0.1220631        2.0593086        1.6315483        0.6114290        0.8719556        2.7467020        1.0073617        0.6950662        6.6153055        0.9145076 
# media            music             news    organizations         politics           sports       technology               tv            world          writers 
# 0.2670422        1.4693141        3.1367315        0.4617217        0.9039453        0.5469295        0.6929391        5.3406693        2.3315706        0.3700805 
#exp(confint(m0))


#Female vs. Others

exp(coef(m0))
exp(coef(m1))
exp(coef(m2))
exp(coef(m3))
exp(coef(m4))

# > exp(coef(m0))
# (Intercept)           n_male             n_ga    no_name_image accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 104.0931287        2.5315460        1.8010498        1.9528173        1.0626631        1.0000057        1.0000126        1.7429600        0.9890316        2.1684921 
# descr_length         verified              art           artist         bloggers         business        companies           design             info      journalists 
# 1.0000295       15.8996261        0.0920623        2.2272425        1.7862243        0.5684529        1.1762906        4.4500904        1.5828711        0.5563624 
# life        marketing            media            music             news    organizations         politics           sports       technology               tv 
# 7.2363698        0.9022229        0.3596204        1.2730452        3.7465798        0.5692796        0.8788297        0.6425464        0.8871510        5.1789635 
# world          writers 
# 2.7888744        0.4081122 
# > exp(coef(m1))
# (Intercept)           n_male             n_ga    no_name_image accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 16.6935936        0.9275444        0.9532245        0.9664551        1.0388282        1.0000064        1.0033079        1.1471225        0.9067976        1.2544370 
# descr_length         verified              art           artist         bloggers         business        companies           design             info      journalists 
# 1.0001306               NA               NA               NA               NA               NA               NA               NA        1.5318554               NA 
# life        marketing            media            music             news    organizations         politics           sports       technology               tv 
# NA               NA               NA               NA               NA               NA               NA               NA               NA               NA 
# world          writers 
# NA               NA 
# > exp(coef(m2))
# (Intercept)           n_male             n_ga    no_name_image accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 112.5462251        0.
# 9855706        0.9951912        1.0097010        1.0095219        1.0000041        1.0003225        1.0006494        0.9774292        1.0530471 
# descr_length         verified              art           artist         bloggers         business        companies           design             info      journalists 
# 0.9999839        1.3446277        1.0651255        1.2087113        1.2751653        1.2710548        0.7859097               NA        1.5642416        2.8613473 
# life        marketing            media            music             news    organizations         politics           sports       technology               tv 
# 1.0793161        1.5046858        1.4269176        0.8430271        0.7957454        0.6804653               NA               NA        1.2987529        0.5127342 
# world          writers 
# 1.0238922        1.4194575 
# > exp(coef(m3))
# (Intercept)           n_male             n_ga    no_name_image accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 288.4338861        0.9849596        1.0013723        1.0001129        1.0031896        1.0000037        1.0001490        1.0052580        0.9836157        1.0183859 
# descr_length         verified              art           artist         bloggers         business        companies           design             info      journalists 
# 0.9999844        1.1869389        1.0169699        0.9393918        1.0011222        1.1287952        1.0577293        1.0019663        0.9895595        0.9301008 
# life        marketing            media            music             news    organizations         politics           sports       technology               tv 
# 1.0278607        1.0577315        1.0735986        1.0034425        1.0997043        0.9797380        0.9947628        0.9891607        1.0018617        0.9250765 
# world          writers 
# 1.0662287        1.0768438 
# > exp(coef(m4))
# (Intercept)           n_male             n_ga    no_name_image accountAge_years   statuses_count    friends_count          has_url          has_loc        has_descr 
# 944.70201704       3.03445304       1.80139684       1.79952452       0.92531210       1.00000411       1.00001100       1.22093623       1.08540732       1.27563623 
# descr_length         verified              art           artist         bloggers         business        companies           design             info      journalists 
# 0.99915791      10.94127848       0.09808277       2.14432125       1.68807149       0.57705172       1.09364924       4.11582056       1.70293899       0.55933811 
# life        marketing            media            music             news    organizations         politics           sports       technology               tv 
# 6.74719088       0.86080557       0.40202106       1.24660382       3.03571442       0.46282623       0.78897027       0.50592157       0.83971890       5.40162348 
# world          writers 
# 2.70682737       0.39037068 
