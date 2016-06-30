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
  group_by(result, CapYear = as.factor(CapYear)) %>% 
  summarise(n = n())

ggplot(df, aes(x = result, y = n, fill = '')) +
  geom_bar(stat = 'identity', color = NA) +
  ggthemes::scale_fill_gdocs() +
  theme_bw()


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

