##Introduction to Hierarchical Clustering
#Analyze movieLens data
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation",
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)


# Compute distances g.e%=h7i"
distances <- dist(movies[,2:20], method = "euclidean")

# Hierarchical clustering ie'eg>$
clusterMovies = hclust(distances, method = "centroid") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)
table(clusterGroups)

# d9e	
geg>$f3e$*gc g(h/e>7f3

clusterMovies = hclust(distances, method = "ward.D") 

#Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]



##Introduction to k-means Clustering
#Analyze CharlesBookClub
Book.df=CharlesBookClub[,c(3:6,8:18)]
#g(.e- ei(f)e
set.seed(123)
#scalef-#h&e
Member.seg = kmeans(scale(Book.df), centers=3, nstart=50, iter.max=100)

#$g,&hg(d>e-g	)d;6e
g(1
#f	$f

i#e g>$e+e:d>
Member.seg
Member.seg$cluster

#

k=10
WGSS=c()
for(i in 1:k){
   WGSS[i]=sum(kmeans(scale(Book.df), centers=i)$withinss)
}
#g(fg(i
ie 1i,)c ef%-fe"e$f7h&ee9>g>$
plot(1:k, WGSS, type="b")


