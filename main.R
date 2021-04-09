# Φόρτωση απαιτούμενων πακέτων 
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

library(recommenderlab)

library(ggplot2)
library(data.table)
library(reshape2)

# Define UI
ui <- fluidPage(
  titlePanel("Movie Recomendations System"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", 
                  h3("Επιλογή Διαγράμματος"), 
                  choices = list(
                    "Προβολές των κορυφαίων ταινιών" = 1, 
                    "Κατανομή της μέσης βαθμολογίας ανά χρήστη" = 2, 
                    "Κανονικοποιημένες βαθμολογίες των κορυφαίων χρηστών" = 3,
                    "Heatmap των κορυφαίων χρηστών και ταινιών" = 4,
                    "Κατανομή του αριθμού των στηλών" = 5,
                    "Κατανομή του αριθμού στοιχείων για το Item Based Collaborative Filter" = 6
                  ), 
                  selected = 1)
    ),
    mainPanel(
      p("Τα αποτελέσματα προκύπτουν απο τα δεδομένα που βρίσκοντε στα αρχεια movies.csv και ratings.csv"),
      hr(),
    )
  )
)


# Define server logic
server <- function(input, output) {
  
}

# Run the app
shinyApp(ui = ui, server = server)

#== ΑΝΑΚΤΗΣΗ ΔΕΔΟΜΕΝΩΝ ==
# Ανάκτηση δεδομένων απο το αρχείο movies.csv στο dataframe movie_data και
# τα δεδομένα αρχείου ratings.csv στο dataframe rating_data.
setwd("./dataset")
movie_data <- read.csv("movies.csv", stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")

# summary(movie_data) # επισκόπηση δεδομένων ταινιών 
# summary(rating_data) # επισκόπηση δεδομένω βαθμολογιών


#== ΠΡΟΕΠΕΞΕΡΓΑΣΙΑ ΔΕΔΟΜΕΝΩΝ ==
# Από τον παραπάνω πίνακα, παρατηρούμε ότι η στήλες user_id και movie_id
# αποτελούνται από ακέραιους αριθμούς. Πρέπει να μετατρέψουμε τα είδη (genres) 
# που υπάρχουν στο movie_data dataframe σε μια πιο εύχρηστη μορφή από τους χρήστες.
# Για να το καταφέρουμε, πρέπει αρχικά  να δημιουργήσουμε matrix που να δείχνει
# αντίστοιχα είδη για καθεμία από τις ταινίες.
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), 
                              stringsAsFactors=FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre

colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index, col])
    genre_mat1[index+1, gen_col] <- 1
  }
}


# καταργήστε την πρώτη σειρά, που ήταν η λίστα των ειδών 
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat2)) {
  # μετατροπή από χαρακτήρες σε ακέραιους αριθμούς 
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
} 

# Για να βγάζει νόημα το σύστημα μας μέσω τον βαθμολογιών μέσω του πακέτου 
# recommenderlabs, πρέπει να μετατρέψουμε το matrix σε sparse matrix 
# Το νέο matrix είναι κλάσης ‘realRatingMatrix’.
ratingMatrix <- dcast(rating_data, user_id~movie_id, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) # διαγραφή userIds

# Μετατροπή matrix βαθμολογιών σε sparse matrix του πακέτου recommenderlab
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

lapply(recommendation_model, "[[", "description")

#== ΟΠΤΙΚΟΠΟΙΗΣΗ ΤΑΙΝΙΩΝ ΜΕ ΤΗΣ ΠΕΡΙΣΣΟΤΕΡΕΣ ΠΡΟΒΟΛΕΣ ==
# Αρχικα Θα μετρήσουμε πρώτα τον αριθμό των προβολών σε μια ταινία και μετά θα 
# τις οργανώσουμε σε έναν πίνακα που θα τους ομαδοποιούσε σε φθίνουσα σειρά.
total_views <- function(){
  movie_views <- colCounts(ratingMatrix) # count views for each movie
  table_views <- data.frame(movie = names(movie_views),
                            views = movie_views) # δημιουργία dataframe των προβολών
  table_views <- table_views[order(table_views$views,
                                   decreasing = TRUE), ] # ταξινόμηση με βάση τον αριθμό των προβολών
  table_views$title <- NA
  for (index in 1:10325){
    table_views[index,3] <- as.character(subset(movie_data,
                                                movie_data$movie_id == table_views[index,1])$title)
  }
  
  # Τώρα θα απεικονίσουμε μια γραφική παράσταση για τον συνολικό αριθμό προβολών 
  # από τις κορυφαίες ταινίες. Θα το πραγματοποιήσουμε χρησιμοποιώντας το ggplot2.
  ggplot(table_views[1:6, ], aes(x = title, y = views)) +
    geom_bar(stat="identity", fill = 'steelblue') +
    geom_text(aes(label=views), vjust=-0.3, size=3.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Συνολικές προβολές των κορυφαίων ταινιών")
}

# ΠΡΟΕΤΟΙΜΑΣΙΑ ΔΕΔΟΜΕΝΩΝ 
# Για την εύρεση χρήσιμων δεδομένων στο μας, έχουμε ορίσει το κατώτατο όριο για 
# τον ελάχιστο αριθμό χρηστών που έχουν αξιολογήσει μια ταινία ως 50. Αυτό είναι
# επίσης ίδιο για τον ελάχιστο αριθμό προβολών ανά ταινία. Με αυτόν τον τρόπο, 
# φιλτράραμε μια λίστα με ταινίες που παρακολουθήθηκαν από ταινίες με τις λιγότερες. 
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

# Από την παραπάνω έξοδο του ‘movie_ratings’, παρατηρούμε ότι υπάρχουν 420 χρήστες 
# και 447 ταινίες σε αντίθεση με τους προηγούμενους 668 χρήστες και 10325 ταινίες. 
# Μπορούμε τώρα οριοθετήστε τo matrix των σχετικών χρηστών ως εξής.
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)

# Τώρα, θα απεικονίσουμε την κατανομή της μέσης βαθμολογίας ανά χρήστη. 
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Κατανομή της μέσης βαθμολογίας ανά χρήστη")

#== ΚΑΝΟΝΙΚΟΠΟΙΗΣΗ ΔΕΔΟΜΕΝΩΝ ==
# Στην περίπτωση ορισμένων χρηστών, μπορεί να υπάρχουν υψηλές ή χαμηλές βαθμολογίες
# σε όλες τις ταινίες που παρακολουθήσατε. Αυτό θα λειτουργήσει ως προκατάληψη 
# κατά την εφαρμογή του μοντέλου μας. Για να το καταργήσουμε, ομαλοποιούμε τα 
# δεδομένα μας. Η κανονικοποίηση είναι μια διαδικασία προετοιμασίας δεδομένων 
# για την τυποποίηση των αριθμητικών τιμών σε μια στήλη σε μια κοινή τιμή κλίμακας.
# Αυτό γίνεται με τέτοιο τρόπο ώστε να μην υπάρχει παραμόρφωση στο εύρος τιμών. 
# Η κανονικοποίηση μετατρέπει τη μέση τιμή της στήλης αξιολογήσεων σε 0. Στη 
# συνέχεια σχεδιάζουμε έναν heatmap που οριοθετεί τις κανονικοποιημένες βαθμολογίες μας. 
normalized_ratings <- normalize(movie_ratings)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
                         main = "Κανονικοποιημένες βαθμολογίες των κορυφαίων χρηστών")

#== ΜΕΤΑΤΡΟΠΗ ΔΕΔΟΜΕΝΩΝ ΣΕ ΔΥΑΔΙΚΗ ΜΟΡΦΗ ==
# Στο τελευταίο βήμα της προετοιμασίας των δεδομένων μας, θα μετατρέψουμε τα δεδομένα μας 
# σε δυαδική μορφή. Αυτό σημαίνει ότι θα έχουμε δύο διακριτές τιμές 1 και 0, 
# που θα επιτρέψει στα συστήματά μας να λειτουργούν πιο αποτελεσματικά. We will define 
# Θα ορίσουμε ένα matrix που θα αποτελείται από 1 εάν η βαθμολογία είναι πάνω 
# από 3, διαφορετικά θα είναι 0. 
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
                      main = "Heatmap των κορυφαίων χρηστών και ταινιών")

#== ΣΥΛΛΟΓΙΚΟ ΣΥΣΤΗΜΑ ΦΙΛΤΡΑΡΙΣΜΑΤΟΣ  ==
# Σε αυτήν την ενότητα θα αναπτύξουμε ένα Σύστημα Φιλτραρίσματος Βασισμένο σε Στοιχεία. 
# Αυτός ο τύπος φιλτραρίσματος βρίσκει ομοιότητα στα στοιχεία με βάση τις 
# βαθμολογίες. Ο αλγόριθμος δημιουργεί πρώτα έναν πίνακα παρόμοιων αντικειμένων 
# των πελατών που τα αγόρασαν σε συνδυασμό παρόμοιων αντικειμένων. Αυτό στη 
# συνέχεια τροφοδοτείται στο σύστημα προτάσεων. 

# Η ομοιότητα μεταξύ μεμονωμένων προϊόντων και συναφών προϊόντων μπορεί να 
# προσδιοριστεί με τον ακόλουθο αλγόριθμο: 

# - Για κάθε είδος i1 που υπάρχει στον κατάλογο προϊόντων, που αγοράστηκε από τον πελάτη C.
# - Και, για κάθε είδος, το i2 αγοράστηκε επίσης από τον πελάτη C. 
# - Δημιουργήστε εγγραφή ότι ο πελάτης αγόρασε τα αντικείμενα i1 και i2. 
# - Υπολογίστε την ομοιότητα μεταξύ i1 και i2. 

# Θα δημιουργήσουμε αυτό το σύστημα φιλτραρίσματος χωρίζοντας το σύνολο δεδομένων 
# σε 80% training set και 20% test set. 
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#== ΠΩΣ ΝΑ ΔΗΜΙΟΥΡΓΗΣΕΤΕ ΣΥΣΤΗΜΑ ΠΡΟΤΑΣΗΣ ΤΑΙΝΙΩΝ ΧΡΗΣΗ R  ==
# We will now explore the various parameters of our Item Based Collaborative Filter.
# These parameters are default in nature. In the first step, k denotes the number of 
# items for computing their similarities. Here, k is equal to 30. Therefore, the 
# algorithm will now identify the k most similar items and store their number. We 
# use the cosine method which is the default one but you can also use pearson method.
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))

# Χρησιμοποιώντας την συνάρτηση getModel(), θα ανακτήσουμε το προτεινόμενο μοντέλο.
# Στη συνέχεια, θα βρούμε την κλάση και τις διαστάσεις του πίνακα που περιέχεται  
# στις πληροφορίες του μοντέλου. Τέλος, θα δημιουργήσουμε έναν heatmap, 
# που θα περιέχει τα 20 κορυφαία στοιχεία και θα απεικονίσουμε την ομοιότητα μεταξύ τους. 
model_info <- getModel(recommen_model)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items], 
      main = "Heatmap των πρώτων γραμμών και στηλών")

# Στο επόμενο βήμα θα εκτελέσουμε το άθροισμα των γραμμών και των στηλών με την
# ομοιότητα των αντικειμένων πάνω από 0. Θα απεικονίσουμε το άθροισμα των στηλών
# μέσω μιας κατανομής ως εξής 
sum_rows <- rowSums(model_info$sim > 0)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Κατανομή του αριθμού των στηλών")

#== ΠΩΣ ΝΑ ΔΗΜΙΟΥΡΓΗΣΕΤΕ ΣΥΣΤΗΜΑ ΠΡΟΤΑΣΗΣ ΤΑΙΝΙΩΝ ΧΡΗΣΗ R  ==
# Θα δημιουργήσουμε μια μεταβλητή top_recommendations που θα αρχικοποιηθεί με 
# τιμή 10, καθορίζοντας τον αριθμό των ταινιών σε κάθε χρήστη. Στη συνέχεια, θα 
# χρησιμοποιήσουμε τη λειτουργία predict() που θα αναγνωρίσει παρόμοια στοιχεία 
# και θα τα ταξινομήσει κατάλληλα. Εδώ, κάθε βαθμολογία χρησιμοποιείται ως βάρος.
# Κάθε βάρος πολλαπλασιάζεται με σχετικές ομοιότητες. Τέλος, όλα προστίθενται. 
top_recommendations <- 10 # αριθμός των στοιχείων που πρέπει να προτείνονται σε κάθε χρήστη 
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)

user1 <- predicted_recommendations@items[[1]] # πρόταση για τον πρώτο χρήστη 
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movie_id == movies_user1[index])$title)
}

# matrix με τις συστάσεις για κάθε χρήστη 
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 

number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Κατανομή του αριθμού στοιχείων για το Item Based Collaborative Filter"

qplot(number_of_items, fill=I("steelblue"), col=I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 5)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:5) {
  table_top[i,1] <- as.character(subset(movie_data,
                                        movie_data$movie_id == table_top[i,1])$title)
}

colnames(table_top) <- c("Τίτλος Tαινίας ", "Αριθμός αντικειμένων")
head(table_top)