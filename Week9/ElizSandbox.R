clt_tracts <- st_read('CensusTracts2010.shp')
hmda<- read.csv('hmda.csv')

cltdata<- inner_join(clt_tracts, hmda, by = c("name10" = "tract"))

##just choose the variables we will use

cltdata %>% dplyr::select( c("name10","Black18","White18","Hispanic18","med_income2018","chblack","chwhite","chincome","chhisp","minor_pop_pct", "minor_pop_pct")) 
cltdata <- st_drop_geometry(cltdata)

cltdata <- cltdata %>% mutate_if(is.character,as.numeric)%>% dplyr::select( c("name10","Black18","White18","Hispanic18","med_income2018","chblack","chwhite","chincome","chhisp","minor_pop_pct", "minor_pop_pct")) 
cltdata<- na.omit(cltdata)
data_scaled<- scale(cltdata[2:10])

distance <- get_dist(data_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(data_scaled, centers = 2, nstart = 25)
k3 <- kmeans(data_scaled, centers = 3, nstart = 25)
k4 <- kmeans(data_scaled, centers = 4, nstart = 25)
k5 <- kmeans(data_scaled, centers = 5, nstart = 25)


p1 <- fviz_cluster(k2, geom = "point", data = data_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data_scaled) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

df<- data_scaled



set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

final5 <- kmeans(data_scaled, 5, nstart = 25)
print(final5)

df<- data_scaled
####optional silhouette value analysis
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(df, kmeans, method = "silhouette")





###analyze cluster characteristics and join back to the shapefile to map
cltclusters<- cltdata %>%
  mutate(cluster5 = final5$cluster) %>%
  group_by(cluster5) %>%
  summarise_all("mean")

print(cltclusters)
fviz_cluster(final5, data = data_scaled)

cltclusters$tract<-as.character(as.numeric(cltclusters$tract))

cltdata<- cltdata %>%
  mutate(cluster5 = final5$cluster)

cltclusters$name10<-as.character(as.numeric(cltclusters$name10))
joined<-left_join(clt_tracts, cltclusters)
clustermap <- tm_shape(joined)+tm_polygons(col = "cluster5", style="cat", palette = "cat")
clustermap


##read in the geocoded zillow data

zillow<- st_read('zillow.shp')

zillow<- zillow %>% select(c("USER_ID", "USER_descr"))
zillow<- st_transform(zillow, crs = st_crs(joined))

st_write(zillow, "zillow.shp", driver="ESRI Shapefile")

joined <- joined %>% select(c("cluster5"))
zillow <- st_join(zillow, joined)

zillow<- st_drop_geometry(zillow)

words<- zillow %>% unnest_tokens(word, USER_descr) %>% anti_join(stop_words) %>% filter(!word %in% remove_list)%>% filter(!cluster5 == 0)


words_by_neighborhood <- words %>%
  count(cluster5, word, sort = TRUE) %>%
  ungroup()

cluster.lab <- c('1'= "White-Higher-Income", '2'="White Homebuyers-Minority Neighborhoods", '3'= "Increasing Black-Minority Neighborhoods", '4'= "White-Increasingly High Income", '5'="Hispanic Homebuyers-Minority Neighborhoods")

words_by_neighborhood %>%
  filter(n >= 25) %>% 
  arrange(n) %>%
  group_by(cluster5) %>%
  top_n(25, n) %>%
  ungroup() %>%
  mutate(n = factor(word, unique(word))) %>%
  ggplot(aes(word, n, fill = cluster5)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ cluster5, scales = "free", ncol = 3) +
  coord_flip() +
  labs(x = NULL, 
       y = "Words by Cluster")





names <- factor(unique(words_by_neighborhood$cluster5))

plist <- list()
plist[]
#tiff("Plot4.tiff", width = 11, height = 8, units = 'in', res = 600, compression = 'lzw') ##if you want to export a higher resolution figure

for (i in 1:length(names)) {
  d <- subset(words_by_neighborhood,cluster5 == names[i])
  d <- subset(d, n>=5)
  d <- head(d,20)
  d$word <- factor(d$word, levels=d[order(d$n),]$word)
  p1 <- ggplot(d, aes(x = word, y = n, fill = cluster5)) + 
    labs(y = NULL, x = NULL, fill = NULL) +
    geom_bar(stat = "identity") +
    facet_wrap(~cluster5, scales = "free", labeller = as_labeller(cluster.lab)) +
    coord_flip() +
    guides(fill=FALSE) +
    theme_bw() + theme( strip.background  = element_blank(),
                        panel.grid.major = element_line(colour = "grey80"),
                        panel.border = element_blank(),
                        axis.ticks = element_line(size = 0),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.y = element_blank() ) +
    theme(legend.position="bottom") 
  
  
  plist[[names[i]]] = p1
}   

do.call("grid.arrange", c(plist, ncol=3))
#dev.off()


words <- words %>% filter(!word %in% remove.list)



##td-idf

cluster_tf_idf <- words_by_neighborhood %>%
  bind_tf_idf(word, cluster5, n)

cluster_tf_idf %>%
  select(-n) %>%
  arrange(desc(tf_idf))



cluster_tf_idf %>%
  group_by(cluster5) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = cluster5)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~cluster5, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

##another way to do this is with logistic regression

library(yardstick)
library(rsample)

#Make binomial - HERE EXAMPLE FOR CLASS 1
##but this step codes everything to 1/0 so if i go to do this for class 2 next, all the data on class 2 is missing?


zillow$cluster5[zillow$cluster5!=2] <- 0    
zillow$cluster5[zillow$cluster5==2] <- 1    
words$cluster5[words$cluster5!=2] <- 0    
words$cluster5[words$cluster5==2] <- 1


#Remove words that only occurs less than 5 times
words$nn <- ave(words$word,words$word, FUN=length)
words$nn <- as.numeric(words$nn)
words<- words[ -which( words$nn <5), ]

##split into testing and training dataset

data_split<- zillow%>%select(USER_ID)
data_split<- initial_split(data_split)
train_data <- training(data_split)
test_data <- testing(data_split)

#TRAINING DATA: transform data from tidy data structure to a sparse matrix
sparse_words <- words %>%
  count(USER_ID, word) %>%
  inner_join(train_data) %>%
  cast_sparse(USER_ID, word, n)

class(sparse_words)
dim(sparse_words)

word_rownames <- as.integer(rownames(sparse_words))

data_joined<- st_drop_geometry(data_joined)

data_joined <- data_frame(USER_ID = word_rownames) %>%
  left_join(zillow %>%
              dplyr::select(USER_ID, cluster5))

#Run model on training data (slow)

is_cluster <- data_joined$cluster5 == 5             
model <- cv.glmnet(sparse_words, is_cluster,
                   family = "binomial", intercept = TRUE
                   #parallel = TRUE, keep = TRUE
)

#Pull out coefficients

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.min)

