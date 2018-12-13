# Loading the files
tmdb<- read.csv(file="tmdb_5000_movies.csv",header=T, sep=",")
moviePlots10to17<- read.csv(file="MoviePlot2010to2017.csv",header=T, sep=",")

# Merging the data to get dataset for sentiment analysis
movie_dataset=merge(moviePlots10to17, tmdb, by.x='movie', by.y='original_title')
movie_dataset
write.csv(movie_dataset, file = "movie_dataset.csv")