library(shiny)

library(recommenderlab)

library(ggplot2)
library(data.table)
library(reshape2)

# Ορισμός Εφαρμογής
ui <- fluidPage(
    # Τίτλος Εφαρμογής
    titlePanel("Movie Recomendations System"),
    navlistPanel(
        tabPanel("Προτάσεις", tableOutput('table')),
        "Διαγράμματα",
        tabPanel("Συνολικές προβολές των κορυφαίων ταινιών", plotOutput("plot1")),
        tabPanel("Κατανομή μέσης βαθμολογίας ανά χρήστη", plotOutput("plot2")),
        tabPanel("Κατανομή του αριθμού στοιχείων για το Item Based Collaborative Filter", plotOutput("plot3"))
    )
)

# Ορισμός λειτουργίας εφαρμογής
server <- function(input, output) {
    #== ΑΝΑΚΤΗΣΗ ΔΕΔΟΜΕΝΩΝ ==
    # Ανάκτηση δεδομένων απο το αρχείο movies.csv στο dataframe movie_data και
    # τα δεδομένα αρχείου ratings.csv στο dataframe rating_data.
    movie_data <- read.csv("./dataset/movies.csv", stringsAsFactors=FALSE)
    rating_data <- read.csv("./dataset/ratings.csv")
    
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
    output$plot1 <- renderPlot({
        ggplot(table_views[1:6, ], aes(x = title, y = views)) +
            geom_bar(stat="identity", fill = 'steelblue') +
            geom_text(aes(label=views), vjust=-0.3, size=3.5) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle("Συνολικές προβολές των κορυφαίων ταινιών")
    })
    
    # ΠΡΟΕΤΟΙΜΑΣΙΑ ΔΕΔΟΜΕΝΩΝ 
    # Για την εύρεση χρήσιμων δεδομένων στο μας, έχουμε ορίσει το κατώτατο όριο για 
    # τον ελάχιστο αριθμό χρηστών που έχουν αξιολογήσει μια ταινία ως 50. Αυτό είναι
    # επίσης ίδιο για τον ελάχιστο αριθμό προβολών ανά ταινία. Με αυτόν τον τρόπο, 
    # φιλτράραμε μια λίστα με ταινίες που παρακολουθήθηκαν από ταινίες με τις λιγότερες. 
    movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                                  colCounts(ratingMatrix) > 50]

    # Από την παραπάνω έξοδο του ‘movie_ratings’, παρατηρούμε ότι υπάρχουν 420 χρήστες 
    # και 447 ταινίες σε αντίθεση με τους προηγούμενους 668 χρήστες και 10325 ταινίες. 
    # Μπορούμε τώρα οριοθετήστε τo matrix των σχετικών χρηστών ως εξής.
    minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
    minimum_users <- quantile(colCounts(movie_ratings), 0.98)
    
    # Τώρα, θα απεικονίσουμε την κατανομή της μέσης βαθμολογίας ανά χρήστη. 
    average_ratings <- rowMeans(movie_ratings)
    output$plot2 <- renderPlot({
        average_ratings <- rowMeans(movie_ratings)
        qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
            ggtitle("Κατανομή μέσης βαθμολογίας ανά χρήστη")
    })
    
    #== ΣΥΛΛΟΓΙΚΟ ΣΥΣΤΗΜΑ ΦΙΛΤΡΑΡΙΣΜΑΤΟΣ  ==
    # Σε αυτήν την ενότητα θα αναπτύξουμε ένα Item Based Collaborative Filter. 
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
    sampled_data <- sample(x = c(TRUE, FALSE),
                          size = nrow(movie_ratings),
                          replace = TRUE,
                          prob = c(0.8, 0.2))
    training_data <- movie_ratings[sampled_data, ]
    testing_data <- movie_ratings[!sampled_data, ]
    
    #== ΠΩΣ ΝΑ ΔΗΜΙΟΥΡΓΗΣΕΤΕ ΣΥΣΤΗΜΑ ΠΡΟΤΑΣΗΣ ΤΑΙΝΙΩΝ ΧΡΗΣΗ R  ==
    # Σε αυτήν την ενότητα θα εξερευνήστε τις διάφορες παραμέτρους μας για το 
    # Item Based Collaborative Filter.
    # Αυτές οι παράμετροι είναι προεπιλεγμένες. Στο πρώτο βήμα, το k δηλώνει τον
    # αριθμό των στοιχείων για τον υπολογισμό των ομοιότητάς τους. Εδώ, το k 
    # είναι ίσο με 30. Επομένως, ο αλγόριθμος θα αναγνωρίσει τώρα τα 
    # παρόμοια αντικείμενα k και θα αποθηκεύσει τον αριθμό τους. Χρησιμοποιούμε 
    # τη μέθοδο cosine που είναι η προεπιλεγμένη, αλλά μπορείτε επίσης να 
    # χρησιμοποιήσετε τη μέθοδο person.
    recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
    
    recommen_model <- Recommender(data = training_data,
                                  method = "IBCF",
                                  parameter = list(k = 30))

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
    output$plot3 <- renderPlot({
        qplot(number_of_items, fill=I("steelblue"), col=I("red")) + 
            ggtitle("Κατανομή του αριθμού στοιχείων για το Item Based Collaborative Filter")
    })
    
    number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
    number_of_items_top <- head(number_of_items_sorted, n = 5)
    table_top <- data.frame(as.integer(names(number_of_items_top)),
                            number_of_items_top)
    for(i in 1:5) {
        table_top[i,1] <- as.character(subset(movie_data,
                                              movie_data$movie_id == table_top[i,1])$title)
    }
    
    colnames(table_top) <- c("Τίτλος Tαινίας ", "Προτάσεις")
    output$table <- renderTable(table_top)
}

# Εκτέλεση εφαρμογής
shinyApp(ui = ui, server = server)
