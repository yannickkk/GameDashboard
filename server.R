library(shinydashboard)
library(shinyjs)
library(magrittr)
library(leaflet)
library(DT)
library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(sp)
library(RSQLServer)
library(DBI)
library(lazyeval)
library(viridis)
source('db_config.R')
source('global.R')

bios <- read_csv('data/bioareas.csv')
ranges <- read_csv('data/ranges.csv')

## survey data and munging
srvy_details <- read_csv('data/tbl_survey_details.txt')
srvy_details <- filter(srvy_details, !(UNIT %in% c('061ID', '066ID')))
survey <- read_csv('data/tbl_survey_comp.txt')
survey <- left_join(survey, srvy_details[, 1:5], by = c('SURVEYID' = 'SURVEYID'))
survey$TIME <- strftime(mdy_hms(survey$TIME), format = '%H:%M:%S')
survey$UNIT <- as.numeric(survey$UNIT)
survey$SURVEYDATE <- as_date(mdy_hms(survey$SURVEYDATE))
survey$YEAR <- year(survey$SURVEYDATE)
survey$MONTH <- month(survey$SURVEYDATE)
survey$BIOYEAR <- ifelse(survey$MONTH > 5, yes = survey$YEAR, no = survey$YEAR - 1)

colPalette <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6", 
                "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99", 
                "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", 
                "#5574A6", "#3B3EAC")
vBios <- bios %>% select(Biologist) %>% extract2(1) %>%  unique() %>% sort()

server <- function(input, output, session) {
#####################
# PAGE 1: ENCOUNTER #
#####################
  ## updating initial inputs
  updateSelectInput(session, 'slBiologist', choices = vBios, selected = '')
  biologist <- reactive({
    dat <- bios %>% 
      filter(Biologist == input$slBiologist) %>% 
      inner_join(ranges, by = ('HuntUnit' = 'HuntUnit')) %>% unique()
    return(dat)
  })
  observeEvent(input$slBiologist, {
    vSpecies <- biologist() %>% extract2('Species') %>% unique() %>% sort()
    updateSelectizeInput(session, 'slSpecies', choices = vSpecies, selected = '')
  })
  observeEvent(c(input$slBiologist, input$slLookup), {
    vLkp <- switch(input$slLookup,
                   'Management Area' = 'MGMT',
                   'Hunt Unit' = 'HuntUnit',
                   'Mountain Range' = 'Range')
    dat <- biologist() %>% extract2(vLkp) %>% unique() %>% sort()
    updateSelectizeInput(session, 'slLookupValue', label = input$slLookup, choices = dat)
  })
  
  ## get encounter data from database
  dat <- eventReactive(input$abGetData, {
    vSpp <- input$slSpecies
    source('db_config.R')
    dat <- tbl(src, 'data_Animal') %>%
      inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>%
      select(ndowID, Species, Sex, CapDate, Status, Age,
             capE, capN, CapMtnRange, CapHuntUnit, EncounterID)

      if (length(vSpp) == 1) {
        dat <- dat %>% filter(Species == vSpp)
      } else {
        dat <- dat %>% filter(Species %in% vSpp)
      }

    dat <- collect(dat)

    clmn <- switch(input$slLookup,
                   'Hunt Unit' = 'CapHuntUnit',
                   'Mountain Range' = 'CapMtnRange')
    print(clmn)
    val <- input$slLookupValue
    dots <- interp(~x %in% val, .values = list(x = as.name(clmn)))
    dat <- dat %>% filter_(dots)
    
    ### type conversion
    dat$CapDate <- as_date(dat$CapDate)
    dat$capE <- as.numeric(dat$capE)
    dat$capN <- as.numeric(dat$capN)
    dat$CapYear <- year(dat$CapDate)

    return(dat)
  })
  
  ## get biometric data
  biometric <- eventReactive(input$abGetData, {
    source('db_config.R')
    biometric <- tbl(src, 'data_Biometric') %>% 
      collect() %>%
      filter(EncounterID %in% dat()$'EncounterID') %>% 
      left_join(dat()) %>%
      select(ndowID, Sex, Age, Biometric, Measurement, Units, CapMtnRange,
             CapHuntUnit, CapDate, CapYear, EncounterID)
    return(biometric)
  })
  
  ## get waddl data
  waddl <- eventReactive(input$abGetData, {
    source('db_config.R')
    eid <- dat()$EncounterID
    dat <- dat() %>% select(ndowID, Species, CapMtnRange, CapHuntUnit, CapDate, EncounterID)
    waddl <- tbl(src, 'lab_Waddl') %>% 
      select(EncounterID, specimenlocation, test, result, lvl, level_cat, isolate, case) %>% 
      filter(EncounterID %in% eid) %>% 
      collect() %>% 
      left_join(dat, by = c('EncounterID' = 'EncounterID'))
    waddl$test <- stringr::str_trim(waddl$test)
    waddl$result <- stringr::str_trim(waddl$result)
    waddl$CapYear <- year(waddl$CapDate)
    return(waddl)
  })
  
  # ## species vector for selected data
  # vSpecies <- eventReactive(input$abGetData, {
  #   dat() %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
  # })
  # observeEvent(input$abGetData, {
  #   updateSelectInput(session, 'slSpecies_map', choices = c('All', vSpecies()), selected = 'All')
  # })
  
  ## encounter map
  output$mpEncounter <- renderLeaflet({
    dat <- xyConv(dat(), c('capE', 'capN'), '+init=epsg:26911', '+init=epsg:4326')
    pal <- colorFactor(gdocs_pal()(length(input$slSpecies)), input$slSpecies)
    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap',
                       options = providerTileOptions(attribution = NA)) %>%
      addCircleMarkers(lng = dat$x, lat = dat$y, 
                       stroke = FALSE, 
                       radius = 4, 
                       fillOpacity = .8,
                       color = pal(dat$Species),
                       popup = paste(sep = '</br>',
                                     paste('<b>Species:</b>', dat$Species),
                                     paste('<b>NDOW ID:</b> ', dat$ndowID),
                                     paste('<b>Date:</b> ', dat$CapDate),
                                     paste('<b>Status:</b> ', dat$Status),
                                     paste('<b>Range:</b> ', dat$CapMtnRange),
                                     paste('<b>Unit:</b> ', dat$CapHuntUnit),
                                     paste('<b>UTM:</b> ', dat$capE, '<b>E</b> ', dat$capN, '<b>N</b>')
                                     ))
  })
  
  ## encounter summary table
  output$tbEncounter <- DT::renderDataTable({
    DT::datatable(dat()[, c('Species', 'ndowID', 'Sex', 'Status', 'CapDate', 'CapMtnRange', 
                        'CapHuntUnit', 'capE', 'capN', 'CapYear')],
              options = list(
                dom = 'ltip'
              ))
  })
  
  ## species distribution for selected input
  output$plSpeciesBar <- renderPlot({
    dat() %>% group_by(Species) %>%
      summarize(Total = n()) %>%
      ggplot(aes(x = Species, y = Total)) +
      geom_bar(stat = 'identity', color = 'royalblue', fill = 'royalblue') +
      theme_bw()
  })
  
  ## species time series for selected input
  output$plSpeciesTS <- renderPlot({
    dat <- dat() %>% 
      group_by(Species, Year = year(as_date(CapDate))) %>% 
      summarize(Total = n())
    ggplot(dat, aes(x = Year, y = Total, group = Species, fill = Species)) +
      geom_bar(stat = 'identity') +
      ggthemes::scale_fill_gdocs() +
      scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), 1),
                         labels = seq(min(dat$Year), max(dat$Year), 1)) +
      theme_bw()
  })
  
  ## species distribution table
  output$tbSppDist <- DT::renderDataTable({
    dat <- dat() %>% group_by(Species, Sex) %>% 
      summarize(Total = n()) %>% 
      arrange(Species, Sex)
    dat$Sex[dat$Sex == ''] <- 'Unk'
    datatable(dat, rownames = F, options = list(dom = 't'))
  })
  
##############
# HEALTH TAB #
##############
  ## plots for disease results
  healthDat <- reactive({
    if (input$slHealthColor == 'None') {
      dat <- waddl() %>% 
        group_by(test, result) %>% 
        summarize(n = n())
    } else {
      dat <- waddl() %>% 
        group_by_('test', input$slHealthColor, 'result') %>% 
        summarize(n = n())
    }
    return(dat)
  })
  
  testDat <- reactive({
    pcr <- healthPlot(healthDat(), input$slHealthColor, 'PCR-Mycoplasma ovipneumoniae')
    elisa <- healthPlot(healthDat(), input$slHealthColor, 'M. ovipneumoniae by ELISA')
    pi3 <- healthPlot(healthDat(), input$slHealthColor, 'Parainfluenza-3')
    brsv <- healthPlot(healthDat(), input$slHealthColor, 'Bovine Resp. Syncytial Virus')
    bvd <- healthPlot(healthDat(), input$slHealthColor, 'Bovine Viral Diarrhea')
    #bt <- healthPlot(healthDat(), input$slHealthColor, 'Bluetongue Virus-ELISA')
    dat <- list(pcr = pcr, elisa = elisa, pi3 = pi3, brsv = brsv, bvd = bvd)
    return(dat)
  })
  
  ### pcr figure and table
  output$plPCR <- renderPlot({
    testDat()$pcr[[2]]
  })
  output$tbPCR <- DT::renderDataTable({
    DT::datatable(testDat()$pcr[[1]], options = list(dom = 't'))
  })
  
  ### elisa figure and table
  output$plElisa <- renderPlot({
    testDat()$elisa[[2]]  
  })
  output$tbElisa <- DT::renderDataTable({
    DT::datatable(testDat()$elisa[[1]], options = list(dom = 't'))
  })
  
  ### pi3 figure and table
  output$plPI3 <- renderPlot({
    testDat()$pi3[[2]]
  })
  output$tbPI3 <- DT::renderDataTable({
    DT::datatable(testDat()$pi3[[1]], options = list(dom = 't'))
  })
  
  ### brsv figure and table
  output$plBRSV <- renderPlot({
    testDat()$brsv[[2]]
  })
  output$tbBRSV <- DT::renderDataTable({
    DT::datatable(testDat()$brsv[[1]], options = list(dom = 't'))
  })
  
  ### bvd figure and table
  output$plBVD <- renderPlot({
    testDat()$bvd[[2]]
  })
  output$tbBVD <- DT::renderDataTable({
    DT::datatable(testDat()$bvd[[1]], options = list(dom = 't'))
  })
  
##############
# SURVEY TAB #
##############
  srvyInput <- eventReactive(input$slBiologist, {
    resp <- bios %>% filter(Biologist == input$slBiologist)
    vUnit <- resp %>% select(HuntUnit) %>% extract2(1) %>% unique() %>% sort()
    vSpecies <- resp %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
    srvyDat <- survey %>% filter(UNIT %in% vUnit & SPECIES %in% vSpecies)
    vUnit <- srvyDat %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
    vSpecies <- srvyDat %>% select(SPECIES) %>% extract2(1) %>% unique() %>% sort()
    return(list(unit = vUnit, species = vSpecies, df = srvyDat))
  })
  
  observeEvent(input$slBiologist, {
    updateSelectizeInput(session, 'slSvyUnit', selected = '', choices = srvyInput()$unit)
    updateSelectInput(session, 'slSvySpecies', selected = '', choices = srvyInput()$species)
  })
  
  mapdat <- eventReactive(input$abSurveyData, {
    dat <- srvyInput()$df %>% 
      filter(UNIT %in% input$slSvyUnit, SPECIES == input$slSvySpecies)
    return(dat)
  })
  
  surveySumry <- eventReactive(input$abSurveyData, {
    dat <- mapdat() %>% 
      mutate(sub = JUVENILE + ADULT) %>% filter(sub > 0) %>% 
      group_by(BIOYEAR, TYPE)  %>% 
      summarize(
        male = sum(as.numeric(MALE)),
        female = sum(as.numeric(FEMALE)),
        juvenile = sum(JUVENILE),
        adult = sum(ADULT),
        pf = sum(JUVENILE) / (sum(ADULT) + sum(JUVENILE)),
        r = pf / (1 - pf),
        n = n(),
        ttl = sum(JUVENILE + ADULT),
        fsq = sum(JUVENILE ** 2),
        t = sum(ADULT),
        tsq = sum(ADULT ** 2),
        fxt = sum(JUVENILE * ADULT)
      ) %>% 
      mutate(
        rint = r * 100,
        rse = (n*((fsq+(r^2*tsq))-(2*r*fxt))/(t^2*(n-1)))^0.5,
        r90ci = rse * 1.645,
        lwci = (r - r90ci) * 100,
        upci = (r + r90ci) * 100
      )
    return(dat)
  })
  
  output$mpSurvey <- renderLeaflet({
    dat <- xyConv(mapdat(), xy = c('EASTING_X', 'NORTHING_Y'), 
                  '+init=epsg:26911', '+init=epsg:4326')
    n <- length(levels(as.factor(dat$YEAR))); print(n)
    vColor <- rep(gdocs_pal()(20), n)
    pal <- colorFactor(vColor, dat$YEAR)
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
      addCircleMarkers(lng = dat$x, lat = dat$y,
                       stroke = FALSE,
                       color = pal(dat$YEAR),
                       fillOpacity = .6,
                       radius = sqrt(dat$TOTAL) + 2,
                       popup = paste(sep = '</br>',
                                     paste('<b>Year:</b>', dat$YEAR),
                                     paste('<b>Male:</b> ', dat$MALE),
                                     paste('<b>Female:</b> ', dat$FEMALE),
                                     paste('<b>Juvenile:</b> ', dat$JUVENILE),
                                     paste('<b>Adult:</b> ', dat$ADULT),
                                     paste('<b>Total:</b> ', dat$TOTAL)
                                     ))
  })
  
  output$tbSurvey <- DT::renderDataTable({
    dat <- surveySumry() %>% 
      select(BIOYEAR, TYPE, male, female, juvenile, adult, ttl, round(rint, 0), round(lwci, 0), round(upci, 0)) %>% 
      ungroup %>% 
      arrange(desc(BIOYEAR), TYPE)
    DT::datatable(dat, rownames = FALSE, options = list(dom = 'ltip'))
  })
  
  output$tbSurveyGroups <- DT::renderDataTable({
    dat <- mapdat() %>% 
      select(SURVEYID, SURVEYDATE, MALE, FEMALE, JUVENILE, ADULT, TOTAL)
    DT::datatable(dat, rownames = FALSE, options = list(dom = 'ltip'))
  })
  
  output$plSurvey <- renderPlot({
    xBreaks <- seq(min(surveySumry()$BIOYEAR) - 1, max(surveySumry()$BIOYEAR) + 1, 2)
    
    ggplot(surveySumry(), aes(x = BIOYEAR, y = rint, color = TYPE, group = TYPE)) +
      geom_point(size = 2) +
      geom_line(linetype = 'dashed') +
      geom_errorbar(aes(ymin = lwci, ymax = upci), width = .25) +
      scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10), limits = c(0, 100)) +
      scale_x_continuous(labels = xBreaks, breaks = xBreaks) +
      scale_color_gdocs(name = 'Survey Type') +
      labs(x = 'Year', y = 'Fawns per 100 Adults', title = 'Annual Fawn Ratio') +
      theme_bw() +
      theme(legend.position = 'top')
  })
  
  output$plRatio <- renderPlot({
    dat <- mapdat() %>% 
      group_by(YEAR, TYPE) %>% 
      mutate(sumtotal = cumsum(TOTAL),
             ratio = cumsum(JUVENILE)/cumsum(ADULT)) %>% 
      filter(ratio < 1.25 & TYPE == input$slSuveyType)
    g <- ggplot(dat, aes(x = sumtotal, y = ratio, color = YEAR)) +
      geom_line(aes(group = YEAR), size = 1, alpha = .8) +
      #geom_point(size = 3, shape = 20, alpha = .5) +
      scale_color_viridis() +
      coord_cartesian(ylim = c(0, 1.25)) +
      theme_bw() +
      theme(legend.position = 'bottom',
            legend.title = element_blank())
    ggExtra::ggMarginal(g, col = 'grey', margins = 'y', size = 8)
  })
###############
# FIGURES TAB #
###############
  figure <- eventReactive(input$abCreatePlot, {
    type <- input$slPlotType
    x <- input$slXaxis
    y <- input$slYaxis
    color <- switch (input$slColor,
                     'None' = NULL,
                     'Sex' = 'Sex',
                     'Age' = 'Age',
                     'CapMtnRange' = 'CapMtnRange',
                     'CapHuntUnit' = 'CapHuntUnit'
    )
    fillval <- color
    facetval <- input$slFacet
    
    spreadBiom <- biometric() %>% 
      select(ndowID, EncounterID, Biometric, Measurement) %>% 
      tidyr::spread(Biometric, Measurement)
    spreadBiom <- left_join(dat(), spreadBiom, by = c('EncounterID' = 'EncounterID'))
    
    gg <- intPlot(spreadBiom, xval = x, yval = y, type = type,
                  colval = color, fillval = fillval, facetval = facetval)
    return(gg)
  })
  
  output$plFigure <- renderPlot({
    figure()
  })
############  
# DATA TAB #
############
  
  output$tbEncSummary <- DT::renderDataTable({
    DT::datatable(dat(), rownames = F, 
                  options = list(
                    dom = 'ltip',
                    scrollX = TRUE)
                  )
  })
  
  output$htmlEncSummary <- renderUI({
    HTML(
      paste(sep = '<br/>', 
            paste('<b>N Rows:</b> ', nrow(dat())),
            paste('<b>N Columns:</b>', ncol(dat())),
            paste('<b>Min Date:</b>', min(as_date(dat()$CapDate))),
            paste('<b>Max Date:</b>', max(as_date(dat()$CapDate)))
            )
      )
  })
  
  output$tbBioSummary <- DT::renderDataTable({
    DT::datatable(biometric(), rownames = F, 
                  options = list(
                    dom = 'ltip', 
                    scrollX = TRUE)
                  )
  })
  
  output$htmlBioSummary <- renderUI({
    HTML(
      paste(sep = '<br/>', 
            paste('<b>N Rows:</b>', nrow(biometric())),
            paste('<b>N Columns:</b>', ncol(biometric()))
            )
        )
  })
  
  output$tbBioNumSum <- DT::renderDataTable({
    tidySum <- biometric() %>% 
      dplyr::select(ndowID, EncounterID, Biometric, Measurement) %>% 
      spread(Biometric, Measurement)
    tidySum <- xda::numSummary(data.frame(tidySum[, 3:8]))[, c(1, 2, 4, 3, 6, 12:14, 5)]
    DT::datatable(tidySum, options = list(dom = 't'))
  })
  
  output$tbWaddlSum <- DT::renderDataTable({
    DT::datatable(waddl(), rownames = FALSE, 
                  options = list(
                    dom = 'ltip',
                    scrollX = T)
                  )
  })
}
