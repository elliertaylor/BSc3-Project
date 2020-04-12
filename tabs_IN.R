library(shiny)
library(DT)

# Define UI for app that calculates worm burden ----
ui <- fluidPage(
   theme = shinythemes::shinytheme("sandstone"),
  
  # App title ----
  titlePanel("Estimating Helminth Burdens using Sibship Reconstruction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric input for number of fecund worms in patient ----
      numericInput("Nmax",
                   'Maximum number of conceivable..',
                   value = 350, min = 100, max = 500, step = 1),
      
      # Input: Numeric input for number of simulations run in model -----
      numericInput("nsim",
                   "Number of simulations",
                   value = 1000, min = 1000, max = 2000, step = 1),
      
      # Input: File upload for csv file ------
      fileInput('target_upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type="tabs",
                  
                  # First Output: Summary Tab
                  tabPanel("Summary", textOutput("summary"), 
                           #life cycle figure
                           img(src="lifecycle.png", height = 300, width = 400),
                           #figure caption
                           textOutput("lifecycletext"),
                           br(),
                           #app explanation
                           textOutput("appdesc"),
                  ),
                  
                  # Second output : Plot Tab
                  tabPanel("Plot", plotOutput("distPlot"),
                           #caption explaining the plot
                           textOutput("plotdesc")), 
                  
                  # Third output : Table with results Tab
                  tabPanel("Table with results", DT::dataTableOutput("output_table"))
                 
      ),
    )
  )
)



# Define server logic required to draw plot and results table ----
server <- function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df_ <- read.csv(inFile$datapath, header = TRUE)
    return(df_)
  })
  
  
  # First output - Table with input data ------------------------------------
  
  output$input_table<- DT::renderDataTable({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df_ <- df_products_upload()
    return(DT::datatable(df_, rownames = FALSE)) })
  
  # Second output - Plot with output ----------------------------------------
  
  
  # renderPlot indicates that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlot({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    
    df_ <- df_products_upload()
    
    Nmax<- input$Nmax
    nsim<- input$nsim
    
    #m<- df_[,1]
    #n<- df_[,2]
    
    data<- as.data.frame(df_[,1:2])
    
    
    # Source model ------------------------------------------------------------
    
    source("~/Uni Yr3/diss/R Stuff/project_official/Shiny_Model2.R", local = TRUE)
    #source("~/OneDrive - Royal Veterinary College/PhD/Third year/Eleanor/Shiny_Model.R", local = TRUE)
    
    # Output ------------------------------------------------------------------
    
    plot1<- ggplot(data=df, aes(y=expectN, x=n, col=as.numeric(m), group=m)) +
      geom_errorbar(aes(ymin=df$lwr, ymax=df$upr), 
                    size=0.4, alpha=1, width=0, position=position_dodge(width=0.6))+
      geom_point(alpha=1, cex=1, position=position_dodge(width=0.6)) +
      scale_color_gradientn(name="m", colours = c("#9ecae1", "#6baed6", "#3182bd", "#08519c", "#08306b"))+
      scale_y_continuous(name="N") +
      theme_minimal()
    
    return(plot1)
    
  })
  
  # Third output - output table with dataset -------------------------------------------
  
  output$output_table<- DT::renderDataTable({
    
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    
    df_ <- df_products_upload()
    
    Nmax<- input$Nmax
    nsim<- input$nsim
    
    #m<- df_[,1]
    #n<- df_[,2]
    
    data<- as.data.frame(df_[,1:2])
    
    
    # Source model ------------------------------------------------------------
    
    source("~/Uni Yr3/diss/R Stuff/project_official/Shiny_Model2.R", local = TRUE)
    #source("~/OneDrive - Royal Veterinary College/PhD/Third year/Eleanor/Shiny_Model.R", local = TRUE)
    
    dfout <- df[,c(2,1,6,11,12)]
    
    return(DT::datatable(dfout, rownames = FALSE))
  })
  
                               
  # Output text on summary tab explaining disease
  output$summary <- renderText({
  "This app provides information on the neglected tropical disease 'Schistosomiasis'. 
    This disease is a helminth infection, meaning patients are infected by worms. The lifecycle of these worms is detailed in the figure below. 
    Currently this disease is hard to diagnose, as the patient is often asymptomatic, but the infection can lead to serious
    health complications and can be fatal in some cases.
    It is hard to determine how badly infected a person is, as usually the patients 'worm-burden' is calculatd from counting 
    worm larvae in a faecal sample. However, this is not an accurate representation of how many live and fertile worms are living in 
    the patient. Because these worms reside in the blood vessels of the patient, it is impossible to count these worms directly.
    Recently sibship reconstruction has been used as a new method to determine the relatedness of the larvae from the faecal sample,
    and hence determine an estimate of the patients' worm-burden.
    Sibshib reconstruction uses genetic data from the eggs, to determine how many live females are giving birth in the patient."
 } )
  
  # Output text on summary tab giving figure caption
  output$lifecycletext <- renderText({"explain the life cycle of this worm burden"})
  
  # Output text on summary tab explaining what app does
  output$appdesc <- renderText({"describe what app does"})
  
  
  # Output text on plot tab describing plot 
  output$plotdesc <- renderText({"describing plot"})
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

