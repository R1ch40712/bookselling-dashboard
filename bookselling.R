#load dataset
bestselling_book <- read_csv('bookselling.csv',show_col_types = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Amazon Top 50 Bestselling Books 2009 - 2019"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(bestselling_book$year), selected = unique(bestselling_book$year)[1])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Penjualan", plotOutput("salesTrend")),
        tabPanel("Penulis Populer", plotOutput("popularAuthors")),
        tabPanel("Genre Populer", plotOutput("popularGenres")),
        tabPanel("Data Table", dataTableOutput("dataTable"))
      )
    )
  )
)

#define server
server <- function(input, output){
  #tren penjualan berdasarkan tahun
  output$salesTrend <- renderPlot({
    yearly_sales <- bestselling_book %>%
      group_by(year) %>% 
      summarize(num_books = n())
    
    ggplot(yearly_sales, aes(x=year,y=num_books)) +
      geom_line(color="blue", size=1 ) +
      geom_point(color="blue", size=3) +
      labs(title = "Trend penjualan berdasarkan tahun", x="Tahun", y="Jumlah Buku")
  })
  
  #Penulis paling populer
  output$popularAuthors <- renderPlot({
    popular_authors <- bestselling_book %>% 
      group_by(author) %>% 
      summarize(num_books =n()) %>% 
      arrange(desc(num_books)) %>% 
      head(10)
    
    ggplot(popular_authors,aes(x=reorder(author,num_books), y=num_books, fill=author))+
      geom_bar(stat="identity")+
      coord_flip()+
      labs(title = "Penulis paling populer", x="Penulis",y="Jumlah Buku")+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal()
  })
  
  #Genre paling populer
  output$popularGenres <- renderPlot({
    popular_genres <- bestselling_book %>% 
      group_by(genre) %>% 
      summarize(num_books = n()) %>% 
      arrange(desc(num_books))
    
    ggplot(popular_genres,aes(x=reorder(genre,num_books), y=num_books,fill=genre))+
      geom_bar(stat = "identity")+
      coord_flip()+
      labs(title = "Genre paling populer", x="Genre", y="Jumlah Buku")+
      scale_fill_brewer(palette = "Pastel1")+
      theme_minimal()
  })
  
  #Table data
  output$dataTable <- renderDataTable({
    datatable(bestselling_book)
  })
}

shinyApp(ui=ui, server=server)
  
