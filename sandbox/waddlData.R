source('db_config.R')
dat <- tbl(src, 'data_Animal') %>%
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>%
  select(ndowID, Species, Sex, CapDate, Status, Age,
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'RBHS' & CapHuntUnit == '101') %>% 
  collect()
source('db_config.R')
waddl <- tbl(src, 'lab_Waddl') %>% 
  filter(EncounterID %in% dat$EncounterID) %>% 
  collect() %>% 
  left_join(dat, by = c('EncounterID' = 'EncounterID')) %>% 
  select(ndowID, EncounterID, CapDate.y, specimenlocation, test, result, lvl, level_cat,
         isolate, case, Species, CapMtnRange, CapHuntUnit)
waddl$CapYear <- year(waddl$CapDate.y)
waddl$test <- stringr::str_trim(waddl$test)
waddl$result <- stringr::str_trim(waddl$result)
waddl %>% 
  group_by(test, result) %>% 
  summarize(n = n()) %>% 
  filter(test == 'M. ovipneumoniae by ELISA')
  
levels(factor(waddl$test))
stringr::str_trim(levels(factor(waddl$test)))

pi3 <- filter(waddl, test == 'Parainfluenza-3')

df <- waddl %>% 
  filter(test == 'Bovine Viral Diarrhea') %>% 
  group_by(result, CapYear = as.factor(CapYear), CapMtnRange, CapHuntUnit) %>% 
  summarise(n = n())
col <- ''
ggplot(df, aes_string(x = 'result', y = 'n', fill = col)) +
  geom_bar(stat = 'identity', color = NA) +
  ggthemes::scale_fill_gdocs() +
  theme_bw()

col <- 'CapMtnRange'
healthDat <- waddl %>% 
  group_by_('test', 'result', col) %>% 
  summarize(n = n())

###############################################################################
# HEALTH PLOTS, FOR THE HEALTH TAB

healthPlot <- function(dat, colby, fltr) {
  dat <- dat %>% 
    filter_(~test == fltr)
  g <- ggplot(dat, aes(x = result, y = n)) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  if (colby == 'None') {
    g <- g + geom_bar(stat = 'identity', fill = 'royalblue') 
  } else if (colby == 'CapYear') {
    g <- g + geom_bar(stat = 'identity', aes_string(fill = colby)) +
      viridis::scale_fill_viridis()
  } else { 
    g <- g + geom_bar(stat = 'identity', aes_string(fill = colby)) +
      ggthemes::scale_fill_gdocs()
  }
  return(list(dat, g))
}

col <- 'CapYear'
healthDat <- waddl %>% 
  group_by_('test', col, 'result') %>% 
  summarize(n = n())
t <- healthPlot(healthDat, col, 'Parainfluenza-3')
t2 <- healthPlot(healthDat, col, 'Bovine Resp. Syncytial Virus')
t[[2]]
View(t[[1]])
d <- list(t = t, t2 = t2)
d$t[[2]]

##########
ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotOutput("distPlot")
  )
))
server <- shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton
    input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}) 
shiny::shinyApp(ui, server)

filterCrit <- parse(text = "test == 'Parainfluenza-3'")
call <- as.call(list(
  as.name('subset.data.frame'),
  waddl,
  filterCrit
))
x <- eval(call)

filterCrit <- "test == 'Parainfluenza-3' & ndowID == 1831"
fc <- list(filterCrit)
x <- filter_(waddl, .dots = fc)

