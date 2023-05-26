
#    http://shiny.rstudio.com/

library(shiny)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(tidyr)
library(caret)
library(randomForest)


df <- read.csv("C:/Users/USER/Documents/BA/Statistics/Data Mining/DM_prject/Heart_Disease_Prediction.csv",sep = ",", dec = ".")

ui <- fluidPage(
 
  
  # Navbar 
  navbarPage(
    "Heart Disease Prediction ", 

    
    tabPanel(
      "Data Exploration",
    ),
   
    
    
    # Exploratory Data Analysis tab
    tabPanel(
      "EDA",
      fluidRow(
        # Descriptive statistics
        column(width = 6,
               h3("Descriptive Statistics"),
               verbatimTextOutput("summary_output")
        ),
        
        
  
      )
    ),
    
  ),
  # Visualizations
  column(width = 6,
         h3("Visualizations"),
         
  ),
  plotOutput("histogram_plot"),
  # Box plot output
  plotOutput("boxplot"),
  # Heat map output
  plotOutput("heatmap"),
  
  # PCA plot
  plotOutput("pca_plot"),

  # Dendogram plot
  plotOutput("dendrogram_plot"),
  
  # Heights plot
  plotOutput("heights_plot"),
  
  #hirearchie plot
  titlePanel("Cluster Visualization (hirearchie)"),
  mainPanel(
    plotOutput("cluster_plot")
  ),
  #tree plot
  plotOutput("tree_plot"),
)




server <- function(input, output) {
  
  df$Heart.Disease <- as.factor(df$Heart.Disease)
  
    # Descriptive statistics
    output$summary_output <- renderPrint({
      # Calculate descriptive statistics
      summary_data <- summary(df)
      summary_data
    })
   
  
  # Histogram plot
  output$histogram_plot <- renderPlot({
    ggplot(data = df, aes(x = Age)) +
      geom_histogram(binwidth = 5,col = "#1B6C61",fill="23A8DF") +
      labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
      theme(plot.title = element_text(hjust = 0.5)) 
    })
  #boxplot
  output$boxplot <- renderPlot({ggplot(data = df, aes(x = Heart.Disease, y = Age)) +
      geom_boxplot(fill = "436AA7")+
    labs(title = "Distribution of Age by Heart Disease Diagnosis", x = "Heart Disease Diagnosis", y = "Age")
    
    })
  # Reactive expression for generating correlation matrix
  cor_matrix <- reactive({
    cor(df[, -c(1, 14)])
  })
  # Render the heat map plot
  output$heatmap <- renderPlot({
    # Create heat map of correlation matrix
    cor_data <- reshape2::melt(cor_matrix())
    
    ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", midpoint = 0, name = "Correlation") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Correlation Matrix of Variables")
  })
  # Perform PCA analysis
  pca_Heart <- PCA(df, ncp = 13, scale.unit = TRUE, graph = TRUE, quali.sup = c("Heart.Disease"), quanti.sup = c(2, 6, 7, 9, 11, 12, 13))
  
  # Plot PCA individual
  output$pca_plot <- renderPlot({
    fviz_pca_ind(pca_Heart, axes = c(1, 2), geom = c("points", "text"), habillage = 14, addEllipses = TRUE, col.ind = "cos2")
  })
  # Perform hierarchical clustering
  P <- df[, 1:13]
  distance_matrix <- dist(P)
  d_heart <- dist(scale(distance_matrix), method = "euclidean")
  hclust_heart <- hclust(d_heart, method = "ward.D")
  
  # Plot the dendrogram
  output$dendrogram_plot <- renderPlot({
    plot(hclust_heart, hang = -1)
  })
  
  # Plot the heights
  output$heights_plot <- renderPlot({
    plot(sort(hclust_heart$height, decreasing = TRUE), type = "b", ylab = "Heights", main = "Dendrogram of Hierarchical Clustering")
  })
  #plot hirearchie 
  output$cluster_plot <- renderPlot({
    # Create a kmeans object
    kmeans_model <- kmeans(P, centers = length(unique(classes_heart)))
    
    # Generate the cluster plot using fviz_cluster
    fviz_cluster(
      list(data = P, cluster = classes_heart),
      geom = "point",
      frame.type = "norm",
      kmeans_model = kmeans_model
    )
  })
  #decision tree
  output$tree_plot <- renderPlot({
    # Build decision tree
    Decision_tree <- rpart(Heart.Disease ~ ., data = df, parms = list(prior = c(0.65, 0.35)))
    
    # Plot decision tree
    rpart.plot(Decision_tree)
  })
  
  
}



# Run the app ----
shinyApp(ui = ui, server = server)
