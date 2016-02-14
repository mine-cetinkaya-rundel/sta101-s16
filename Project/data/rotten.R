# Load all necessary packages ---------------------------------------

library(rjson)
library(stringr)
library(dplyr)
library(rvest)
library(lubridate)

# create population -------------------------------------------------

mixed <- read.csv("raw-data/All U.S. Released Movies- 1972-2016.csv", stringsAsFactors = FALSE) %>%
  select(-ForPeopleNotSheeple.rated)
doc <- read.csv("raw-data/Genre- Documentary, 1200 Titles- 1960-2013.csv", stringsAsFactors = FALSE) %>%
  select(-ForPeopleNotSheeple.rated)
tv <- read.csv("raw-data/Made for Television Movies.csv", stringsAsFactors = FALSE)

all <- rbind(mixed, doc, tv)

# population to sample ----------------------------------------------

set.seed(84268953)

samp <- all %>%
  filter(Year < 2016) %>%
  sample_n(1000) %>%
  mutate(id = str_replace(const,"^tt",""))

write.csv(samp, file = "movies_sample.csv", row.names = FALSE)

# download json files -----------------------------------------------

# setwd("json")
# for(i in 1:nrow(samp)){
#   url1 = "http://api.rottentomatoes.com/api/public/v1.0/movie_alias.json?id="
#   id = samp$id[i]
#   url2 = "&type=imdb&apikey=asd25qrhyrw7a3zf7kgwhpfq"
#   url = paste(url1,id,url2, sep="")
#   filename = paste(i,"_",id,".json", sep="")
#   download.file(url = url, destfile = filename)
#   Sys.sleep(1)
# }

# create dataset ----------------------------------------------------

d = subset(samp, select = c(Title, Title.type, Directors, IMDb.Rating,
                            Runtime..mins., Year, Genres, Num..Votes,
                            Release.Date..month.day.year., URL, id))

names(d) = c("title_imdb", "title_type_imdb", "directors_imdb", "imdb_rating_imdb", 
             "runtime_imdb", "year_imdb", "genres_imdb", "num_votes_imdb", 
             "rel_date_imdb", "url_imdb", "id_imdb")

# create new columns ------------------------------------------------

d$title_rt = NA
d$year_rt = NA
d$genre_rt = NA
d$mpaa_rating_rt = NA
d$runtime_rt = NA
d$the_rel_date_rt = NA
d$dvd_rel_date_rt = NA
d$critics_score_rt = NA
d$critics_rating_rt = NA
d$audience_score_rt = NA
d$audience_rating_rt = NA
d$actor1_rt = NA
d$actor2_rt = NA
d$actor3_rt = NA
d$actor4_rt = NA
d$actor5_rt = NA
d$director_rt = NA
d$studio_rt = NA
d$url_rt = NA
dim(d)

# fill in new columns -----------------------------------------------

setwd("json")

for(i in 1:nrow(d)){
  print(i)
  filename = paste(i,"_",d$id_imdb[i],".json", sep="")
  file = fromJSON(file = filename)
  if(is.null(file$title[i])){
    d[i,12:30] = NA
  }
  if(!is.null(file$title[i])){
    d$title_rt[i] = file$title
    d$year_rt[i] = file$year
    d$genre_rt[i] = ifelse((length(file$genres) == 0), NA, file$genres[1])
    d$mpaa_rating[i] = file$mpaa_rating
    d$runtime_rt[i] = file$runtime
    d$the_rel_date_rt[i] = ifelse((length(file$release_dates$theater) == 0), NA, file$release_dates$theater[1])
    d$dvd_rel_date_rt[i] = ifelse((length(file$release_dates$dvd) == 0), NA, file$release_dates$dvd[1])
    d$critics_score_rt[i] = file$ratings$critics_score
    d$critics_rating_rt[i] = ifelse(is.null(file$ratings$critics_rating), NA, file$ratings$critics_rating)
    d$audience_score_rt[i] = file$ratings$audience_score 
    d$audience_rating_rt[i] = ifelse(is.null(file$ratings$audience_rating), NA, file$ratings$audience_rating)
    d$actor1_rt[i] = ifelse(length(file$abridged_cast)>=1, file$abridged_cast[[1]]$name, NA)
    d$actor2_rt[i] = ifelse(length(file$abridged_cast)>=2, file$abridged_cast[[2]]$name, NA)
    d$actor3_rt[i] = ifelse(length(file$abridged_cast)>=3, file$abridged_cast[[3]]$name, NA)
    d$actor4_rt[i] = ifelse(length(file$abridged_cast)>=4, file$abridged_cast[[4]]$name, NA)
    d$actor5_rt[i] = ifelse(length(file$abridged_cast)>=5, file$abridged_cast[[5]]$name, NA)
    d$director_rt[i] = ifelse(length(file$abridged_directors)>=1, file$abridged_directors[[1]]$name, NA)
    d$studio_rt[i] = ifelse(is.null(file$studio), NA, file$studio)
    d$url_rt[i] = file$links$alternate[1]
  }  
}

# remove genres column ----------------------------------------------

d = d[ ,-which(names(d) == "genres_imdb")]

# save --------------------------------------------------------------

setwd("..")
write.csv(d, file = "d_imdb_rt_match.csv", row.names = FALSE)

# load --------------------------------------------------------------

d <- read.csv("d_imdb_rt_match.csv", stringsAsFactors = FALSE)


# remove rows where there is no audience or critic score ------------

d <- d %>%
  filter(!is.na(critics_score_rt) & critics_score_rt > 0) %>%
  filter(!is.na(audience_score_rt) & audience_score_rt > 0)

# remove movie with questionable title ------------------------------

d <- d %>%
  filter(title_imdb != "YPF")

# title check - use title_rt as truth -------------------------------

title_check1 <- d %>%
  filter(title_imdb != title_rt) %>%
  select(title_imdb, title_rt)

# keep title_rt, but fix one
d <- d %>%
  mutate(title_rt = ifelse(title_rt == "American Experience", 
                "Jonestown: The Life and Death of Peoples Temple", title_rt))

title_check2 <- d %>%
  filter(title_imdb != title_rt) %>%
  select(title_imdb, title_rt) # s/b one less than title_check1

# runtime check - use runtime_imdb as truth -------------------------

runtime_check1 <- d %>%
  mutate(runtime_diff = runtime_imdb - runtime_rt) %>%
  filter(runtime_diff != 0) %>%
  select(title_rt, runtime_imdb, runtime_rt, runtime_diff) %>%
  arrange(desc(abs(runtime_diff))) # many mismatch, but by few mins

hist(runtime_check1$runtime_diff)

# year check - use year_imdb as truth -------------------------------

year_check1 <- d %>%
  mutate(year_imdb = as.numeric(year_imdb)) %>%
  mutate(year_rt = as.numeric(year_rt)) %>%
  mutate(year_diff = year_imdb - year_rt) %>%
  filter(year_diff != 0) %>%
  select(title_rt, year_imdb, year_rt, year_diff, url_imdb, url_rt) %>%
  arrange(desc(abs(year_diff))) # many mismatch, but by few years

# oscar match - best pic nom ----------------------------------------

best_pic_noms <- read.csv("raw-data/oscar/All Best Picture Oscar-Nominated Movies.csv", 
                          stringsAsFactors = FALSE)

d$best_pic_nom = "no"
for(i in 1:nrow(d)){
  if(d$title_imdb[i] %in% best_pic_noms$Title){d$best_pic_nom[i] = "yes"}
}

# oscar match - best pic win ----------------------------------------

best_pic_wins <- read.csv("raw-data/oscar/Best Picture Oscar Winners (Academy Awards).csv", 
                          stringsAsFactors = FALSE)

d$best_pic_win = "no"
for(i in 1:nrow(d)){
  if(d$title_imdb[i] %in% best_pic_wins$Title){d$best_pic_win[i] = "yes"}
}

# oscar match - best actor win --------------------------------------

best_actor_wins = read.csv("raw-data/oscar/best_actor_wins.csv", 
                           stringsAsFactors = FALSE)

d$best_actor_win = "no"
for(i in 1:nrow(d)){
  if(d$actor1_rt[i] %in% best_actor_wins$name |
     d$actor2_rt[i] %in% best_actor_wins$name |
     d$actor3_rt[i] %in% best_actor_wins$name |
     d$actor4_rt[i] %in% best_actor_wins$name |
     d$actor5_rt[i] %in% best_actor_wins$name){d$best_actor_win[i] = "yes"}
}

# oscar match - best actress win --------------------------------------

best_actress_wins <- read.csv("raw-data/oscar/best_actress_wins.csv", 
                              stringsAsFactors = FALSE)

d$best_actress_win = "no"
for(i in 1:nrow(d)){
  if(d$actor1_rt[i] %in% best_actress_wins$name |
     d$actor2_rt[i] %in% best_actress_wins$name |
     d$actor3_rt[i] %in% best_actress_wins$name |
     d$actor4_rt[i] %in% best_actress_wins$name |
     d$actor5_rt[i] %in% best_actress_wins$name){d$best_actress_win[i] = "yes"}
}

# oscar match - best dir win ----------------------------------------

best_dir_wins <- read.csv("raw-data/oscar/best_dir_wins.csv", 
                          stringsAsFactors = FALSE)

d$best_dir_win = "no"
for(i in 1:nrow(d)){
  if(d$director_rt[i] %in% best_dir_wins$name){d$best_dir_win[i] = "yes"}
}

# top 200 box office match ------------------------------------------
# inflation adjusted
# http://boxofficemojo.com/alltime/adjusted.htm

page <- read_html("http://www.boxofficemojo.com/alltime/adjusted.htm")

top_box_titles <- page %>%
  html_nodes("td td:nth-child(2)") %>%
  html_text()

d$top200_box = "no"

for(i in 1:nrow(d)){
  if(d$title_imdb[i] %in% top_box_titles){d$top200_box[i] = "yes"}
  if(d$title_rt[i] %in% top_box_titles){d$top200_box[i] = "yes"}
}

# parse theater release date ----------------------------------------



# parse dvd release date --------------------------------------------



# final dataset -----------------------------------------------------

cols_to_keep = c("title_rt", "title_type_rt", "genre_rt", 
                 "runtime_imdb", "year_imdb", "mpaa_rating_rt", "studio_rt", 
                 "num_votes_imdb", "critics_score_rt", "critics_rating_rt", 
                 "audience_score_rt", "audience_rating_rt", 
                 "best_pic_nom", "best_pic_win", "best_actor_win" , 
                 "best_actress_win", "best_dir_win", 
                 "top200_box", "director_rt", 
                 "actor1", "actor2", "actor3", "actor4", "actor5" ,
                 "url_rt", "url_imdb", "id_imdb")

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
