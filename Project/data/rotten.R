library(rjson)
library(stringr)

# population to sample ----------------------------------------------

all = read.csv("all.csv", stringsAsFactors = FALSE)
pre_2015 = all[all$Year < 2015,]
set.seed(822016)
rows_to_samp = sample(1:nrow(pre_2015), size = 600)
samp = pre_2015[rows_to_samp,]
samp$id = str_replace(samp$const,"^tt","")
write.csv(samp, file = "movies_raw.csv", row.names = FALSE)

# download json files -----------------------------------------------

setwd("json")
key = "asd25qrhyrw7a3zf7kgwhpfq"
for(i in 1:nrow(samp)){
  url1 = "http://api.rottentomatoes.com/api/public/v1.0/movie_alias.json?id="
  id = samp$id[i]
  url2 = "&type=imdb&apikey=asd25qrhyrw7a3zf7kgwhpfq"
  url = paste(url1,id,url2, sep="")
  filename = paste(i,"_",id,".json", sep="")
  download.file(url = url, destfile = filename)
  Sys.sleep(1)
}

# create dataset ----------------------------------------------------

d = subset(samp, select = c(Title,Title.type,Directors,IMDb.Rating,Runtime..mins.,Year,Genres,Num..Votes,Release.Date..month.day.year.,URL,id))

names(d) = c("title","title_type","directors","imdb_rating","runtime","year","genres","num_votes","rel_date","url","id")

# create new columns
d$title_rt = NA
d$year_rt = NA
d$genre_rt = NA
d$mpaa_rating = NA
d$runtime_rt = NA
d$critics_rating = NA
d$critics_score = NA
d$audience_rating = NA
d$audience_score = NA
d$actor1 = NA
d$actor2 = NA
d$actor3 = NA
d$actor4 = NA
d$actor5 = NA
d$director_rt = NA
d$studio = NA
d$link = NA
dim(d)

# fill in new columns
setwd("json")

for(i in 1:nrow(d)){
  print(i)
  filename = paste(i,"_",d$id[i],".json", sep="")
  file = fromJSON(file = filename)
  if(is.null(file$title[i])){
    d[i,12:26] = NA
  }
  if(!is.null(file$title[i])){
    d$title_rt[i] = file$title
    d$year_rt[i] = file$year
    d$genre_rt[i] = ifelse((length(file$genres) == 0), NA, file$genres[1])
    print(d$genre_rt[i])
    d$mpaa_rating[i] = file$mpaa_rating
    d$runtime_rt[i] = file$runtime
    d$critics_rating[i] = ifelse(is.null(file$ratings$critics_rating), NA, file$ratings$critics_rating)
    d$critics_score[i] = file$ratings$critics_score
    d$audience_rating[i] = ifelse(is.null(file$ratings$audience_rating), NA, file$ratings$audience_rating)
    d$audience_score[i] = file$ratings$audience_score 
    d$actor1[i] = ifelse(length(file$abridged_cast)>=1, file$abridged_cast[[1]]$name, NA)
    d$actor2[i] = ifelse(length(file$abridged_cast)>=2, file$abridged_cast[[2]]$name, NA)
    d$actor3[i] = ifelse(length(file$abridged_cast)>=3, file$abridged_cast[[3]]$name, NA)
    d$actor4[i] = ifelse(length(file$abridged_cast)>=4, file$abridged_cast[[4]]$name, NA)
    d$actor5[i] = ifelse(length(file$abridged_cast)>=5, file$abridged_cast[[5]]$name, NA)
    d$director_rt[i] = ifelse(length(file$abridged_directors)>=1, file$abridged_directors[[1]]$name, NA)
    d$studio[i] = ifelse(is.null(file$studio), NA, file$studio)
    d$link[i] = file$links$alternate[1]
  }  
}

d = d[ ,-which(names(d) == "genres")]

# save
setwd("..")
write.csv(d, file = "d_imdb_rt_raw.csv", row.names = FALSE)

# get rid of rows where there is no audience or critic score
d = read.csv("d_imdb_rt_raw.csv", stringsAsFactors = FALSE)
d = d[-which(is.na(d$critics_score)),]
d = d[-which(d$critics_score < 0),]


# title check - mismatch?
title_check1 = d[which(d$title != d$title_rt),c(1,12)]
# there are too many, leave for now, may not be important

# runtime check - 
# use IMDB data as truth
runtime_check1 = d[which(d$runtime != d$runtime_rt),c(1,5,16)] # many mismatch, but by few mins
runtime_check1$diff = runtime_check1$runtime - runtime_check1$runtime_rt
hist(runtime_check1$diff)

runtime_check2 = d[which(abs(as.numeric(d$runtime) - as.numeric(d$runtime_rt)) > 20),c(1,5,16)] # some with large differences, but use IMDB as truth

# year check - 
# use IMDB data as truth
year_check = d[which(abs(as.numeric(d$year) - as.numeric(d$year_rt)) > 1),c(1,6,13)]


# oscar match // best pic nom
best_pic_noms = read.csv("best_pic_noms.csv", stringsAsFactors = FALSE)

d$best_pic_nom = "no"
for(i in 1:nrow(d)){
  if(d$title[i] %in% best_pic_noms$Title){d$best_pic_nom[i] = "yes"}
}

# oscar match // best pic win
best_pic_wins = read.csv("best_pic_wins.csv", stringsAsFactors = FALSE)

d$best_pic_win = "no"
for(i in 1:nrow(d)){
  if(d$title[i] %in% best_pic_wins$Title){d$best_pic_win[i] = "yes"}
}

# oscar match // best actor win
best_actor_wins = read.csv("best_actor_wins.csv", stringsAsFactors = FALSE)

d$best_actor_win = "no"
for(i in 1:nrow(d)){
  if(d$actor1[i] %in% best_actor_wins$name |
       d$actor2[i] %in% best_actor_wins$name |
       d$actor3[i] %in% best_actor_wins$name |
       d$actor4[i] %in% best_actor_wins$name |
       d$actor5[i] %in% best_actor_wins$name){d$best_actor_win[i] = "yes"}
}

# oscar match // best actress win
best_actress_wins = read.csv("best_actress_wins.csv", stringsAsFactors = FALSE)

d$best_actress_win = "no"
for(i in 1:nrow(d)){
  if(d$actor1[i] %in% best_actress_wins$name |
       d$actor2[i] %in% best_actress_wins$name |
       d$actor3[i] %in% best_actress_wins$name |
       d$actor4[i] %in% best_actress_wins$name |
       d$actor5[i] %in% best_actress_wins$name){d$best_actress_win[i] = "yes"}
}

# oscar match // best dir win
best_dir_wins = read.csv("best_dir_wins.csv", stringsAsFactors = FALSE)

d$best_dir_win = "no"
for(i in 1:nrow(d)){
  if(d$director_rt[i] %in% best_dir_wins$name){d$best_dir_win[i] = "yes"}
}

# top 200 box office match
# inflation adjusted
# http://boxofficemojo.com/alltime/adjusted.htm
inf_ad_bo_top200 = read.csv("inf_ad_bo_top200.csv", stringsAsFactors = FALSE)

d$top200_box = "no"

for(i in 1:nrow(d)){
  if(d$title[i] %in% inf_ad_bo_top200$title){d$top200_box[i] = "yes"}
}

# final dataset
cols_to_keep = c("title", "audience_score", "title_type", "genre_rt", "runtime",
                 "year", "mpaa_rating", "studio", "num_votes", "critics_score",
                 "critics_rating", "best_pic_nom",
                 "best_pic_win", "best_actor_win" , "best_actress_win",
                 "best_dir_win", "top200_box", "audience_rating", "director_rt",
                 "actor1", "actor2", "actor3", "actor4", "actor5" ,
                 "url", "link", "id")

d_fin = subset(d, select = cols_to_keep)

names(d_fin) = c("title", "audience_score", "type", "genre", "runtime",
                 "year", "mpaa_rating", "studio", "imdb_num_votes", "critics_score", "critics_rating", 
                 "best_pic_nom", "best_pic_win", "best_actor_win" , "best_actress_win",
                 "best_dir_win", "top200_box", "audience_rating", "director",
                 "actor1", "actor2", "actor3", "actor4", "actor5" ,
                 "imdb_url", "rt_url", "imdb_id")

d_fin$title = as.character(d_fin$title)
d_fin$audience_score = as.numeric(d_fin$audience_score)
d_fin$type = as.factor(d_fin$type)
d_fin$genre = as.factor(d_fin$genre)
d_fin$runtime = as.numeric(d_fin$runtime)
d_fin$year = as.numeric(d_fin$year)
d_fin$mpaa_rating = as.factor(d_fin$mpaa_rating)
d_fin$studio = as.factor(d$studio)
d_fin$imdb_num_votes = as.numeric(d_fin$imdb_num_votes)
d_fin$critics_score = as.numeric(d_fin$critics_score)
d_fin$critics_rating = as.factor(d_fin$critics_rating)
d_fin$best_pic_nom = as.factor(d_fin$best_pic_nom)
d_fin$best_pic_win = as.factor(d_fin$best_pic_win)
d_fin$best_actor_win = as.factor(d_fin$best_actor_win)
d_fin$best_actress_win = as.factor(d_fin$best_actress_win)
d_fin$best_dir_win = as.factor(d_fin$best_dir_win)
d_fin$top200_box = as.factor(d_fin$top200_box)
d_fin$audience_rating = as.factor(d_fin$audience_rating)
d_fin$director = as.character(d_fin$director)
d_fin$actor1 = as.character(d_fin$actor1)
d_fin$actor2 = as.character(d_fin$actor2)
d_fin$actor3 = as.character(d_fin$actor3)
d_fin$actor4 = as.character(d_fin$actor4)
d_fin$actor5 = as.character(d_fin$actor5)
d_fin$rt_url = as.character(d_fin$rt_url)
d_fin$imdb_url = as.character(d_fin$imdb_url)
d_fin$rt_url = as.character(d_fin$rt_url)
d_fin$imdb_id = as.character(d_fin$imdb_id)

d_fin$genre2 = "Other"
d_fin$genre2[d_fin$genre == "Drama"] = "Drama"
d_fin$genre2[d_fin$genre == "Action & Adventure"] = "Action & Adventure"
d_fin$genre2[d_fin$genre == "Comedy"] = "Comedy"
d_fin$genre2[d_fin$genre == "Mystery & Suspense"] = "Mystery & Suspense"
d_fin$genre2[d_fin$genre == "Horror"] = "Horror"
d_fin$genre2[d_fin$genre == "Documentary"] = "Documentary"
d_fin$genre2[d_fin$genre == "Art House & International"] = "Art House & International"
d_fin$genre2[d_fin$genre == "Science Fiction & Fantasy"] = "Science Fiction & Fantasy"

d_fin2 = d_fin
d_fin2$genre = as.factor(d_fin$genre2)
dim(d_fin2)
d_fin2 = d_fin2[,-dim(d_fin2)]

# final save
movies = d_fin2
save(movies, file = "movies.Rdata")

# model
m = lm(audience_score ~ critics_score + critics_rating + type + genre + runtime + year + mpaa_rating + imdb_num_votes + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box, data = movies)
step(m)

load("movies.Rdata")
