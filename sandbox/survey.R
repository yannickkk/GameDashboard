library(dplyr)
library(shiny)
library(magrittr)
library(ggplot2)
dets <- read_csv('data/tbl_survey_details.txt')
srvy <- read_csv('data/tbl_survey_comp.txt')
idaho <- filter(dets, UNIT %in% c('061ID', '066ID'))
dets <- filter(dets, !(UNIT %in% c('061ID', '066ID')))  ## remove idaho

dat <- left_join(srvy, dets[, 1:5], by = c('SURVEYID' = 'SURVEYID'))
dat$TIME <- strftime(lubridate::mdy_hms(dat$TIME), format = '%H:%M:%S')
dat$UNIT <- as.numeric(dat$UNIT)
dat$SURVEYDATE <- as_date(mdy_hms(dat$SURVEYDATE))
dat$YEAR <- year(dat$SURVEYDATE)

map_dat <- dat %>%  select(SURVEYID, SURVEYDATE, TIME, EASTING_X, NORTHING_Y, SPECIES, TOTAL,
                           ADULT, JUVENILE, MALE, FEMALE, UNIT, YEAR)
map_coords <- xyConv(map_dat, xy = c('EASTING_X', 'NORTHING_Y'), '+init=epsg:26911', '+init=epsg:4326')



srvyid <- dat %>% select(SURVEYID) %>% magrittr::extract2(1) %>% unique() %>% sort()
vBios <- bios %>% select(Biologist) %>% extract2(1) %>%  unique() %>% sort()

tst <- map_coords %>% 
  filter(UNIT == 77 & SPECIES == 'MULD')
yrs <- tst %>% select(YEAR) %>% extract2(1) %>% unique() %>% sort()
pal <- colorFactor(ggthemes::gdocs_pal()(length(yrs)), yrs)
previewColors(pal, tst$YEAR)

leaflet() %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lng = tst$x, lat = tst$y, 
                   stroke = F,
                   fillOpacity = .6,
                   radius = sqrt(tst$TOTAL) + 2,
                   color = pal(tst$YEAR), group = tst$YEAR) %>% 
  addLegend('bottomright', pal, tst$YEAR) %>% 
  addLayersControl(overlayGroups = tst$YEAR)

map <- leaflet() %>% addProviderTiles('Esri.WorldPhysical')
layer_var <- 'YEAR'

ids <- unique(tst[, layer_var]) %>% sort()
pal <- gdocs_pal()(3)
layers <- list()


mapPoints <- function(map, df, layer_var) {
  ids <- unique(df[, layer_var])
  pal <- gdocs_pal()(3)
  layers <- list()
  
  for(i in seq_along(ids)) {
    dat <- df %>% filter_(lazyeval::interp(~x == ids[i], .values = list(x = as.name(layer_var))))
    map <- addCircleMarkers(map, lng = dat$x, lat = dat$y,
                            group = as.character(ids[i]), color = pal[i],
                            radius = 3, stroke = FALSE,fillOpacity = .3)
    layers <- c(layers, as.character(ids[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}

mapvw <- mapPoints(map, tst, 'YEAR')
mapvw


## dynamic filter survey data by biologist area of responsibility
resp <- bios %>% filter(Biologist == 'Joe Bennett')
vUnit <- resp %>% select(HuntUnit) %>% extract2(1) %>% unique() %>% sort()
vSp <- resp %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
### filtering survey data by Joes area and species of responsibility
srvy_resp <- dat %>% 
  filter(UNIT %in% vUnit & 
         SPECIES %in% vSp)
### get lookups for hunt units and species
vnUnit <- srvy_resp %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
vnSp <- srvy_resp %>% select(SPECIES) %>% extract2(1) %>% unique() %>% sort()

lkp <- list(unit = vUnit, species = vSp)
lkp$unit

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('slBios', 'Biologist', selected = '', choices = vBios),
      selectInput('slUnit', 'Unit', selected = '', choices = ''),
      selectInput('slSpecies', 'Species', selected = '', choices = ''),
      selectInput('slYear', 'Year', selected = '', choices = ''),
      selectInput('slSurvey', 'Survey ID', selected = '', choices = srvyid),
      actionButton('acButton', 'Map')
    ),
    mainPanel(
      leaflet::leafletOutput('map'),
      DT::dataTableOutput('table'),
      plotOutput('plot')
    )
  )
)

server <- function(input, output, session) {
  lkp <- eventReactive(input$slBios, {
    resp <- bios %>% filter(Biologist == input$slBios)
    vUnit <- resp %>% select(HuntUnit) %>% extract2(1) %>% unique() %>% sort()
    vSp <- resp %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
    srvy_resp <- dat %>% 
      filter(UNIT %in% vUnit & 
               SPECIES %in% vSp)
    ### get lookups for hunt units and species
    vUnit <- srvy_resp %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
    vSp <- srvy_resp %>% select(SPECIES) %>% extract2(1) %>% unique() %>% sort()
    return(list(unit = vUnit, species = vSp))
  })
  
  observeEvent(input$slBios, {
    updateSelectInput(session, 'slUnit', choices = lkp()$unit, selected = '')
    updateSelectInput(session, 'slSpecies', choices = lkp()$species, selected = '')
  })
  
  mapdat <- eventReactive(input$acButton, {
    map_coords %>% filter(UNIT == input$slUnit & SPECIES == input$slSpecies)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>%
      addCircleMarkers(lng = mapdat()$x, lat = mapdat()$y,
                       stroke = FALSE,
                       fillOpacity = .4,
                       radius = sqrt(mapdat()$TOTAL) + 2)
  })
  output$table <- DT::renderDataTable({
    smry <- mapdat() %>% group_by(YEAR) %>% 
      summarize(male = sum(MALE, na.rm = T),
                female = sum(FEMALE, na.rm = T),
                juvenile = sum(JUVENILE, na.rm = T),
                adult = sum(ADULT, na.rm = T),
                total = sum(TOTAL, na.rm = T),
                groups = n())
    datatable(smry, rownames = F, options = list(dom = 't'))
  })
}

shiny::shinyApp(ui = ui, server = server)

# SURVEY STANDARD ERROR
svy_sum <- tst %>% 
  group_by(YEAR) %>% 
  summarise(male = sum(MALE, na.rm = T),
            female = sum(FEMALE, na.rm = T),
            juvenile = sum(JUVENILE, na.rm = T),
            adult = sum(ADULT, na.rm = T),
            ttl = sum(JUVENILE + ADULT, na.rm = T),
            fsq = sum(JUVENILE**2, na.rm = T),
            t = sum(ADULT, na.rm = T),
            tsq = sum(ADULT**2, na.rm = T),
            fxt = sum(JUVENILE * ADULT, na.rm = T),
            pf = sum(JUVENILE, na.rm = T) / (sum(ADULT, na.rm = T) + sum(JUVENILE, na.rm = T)),
            n = n()) %>%  
  mutate(femjuv = juvenile + adult,
         pf1 = juvenile / (adult + juvenile),
         r = pf / (1 - pf),
         ttl1 = juvenile + adult,
         fsq1 = juvenile**2,
         tsq1 = adult**2,
         fxt1 = juvenile * adult
         )

(n*((fsq+(r^2*tsq))-(2*r*fxt))/(t^2*(n-1)))^0.5

# SURVEY RATIO BY CUMSUM
tst <- filter(dat, SPECIES == 'MULD' & UNIT %in% 101:109)

svy_sum <- tst %>% 
  group_by(YEAR) %>% 
  summarise(male = sum(MALE, na.rm = T),
            female = sum(FEMALE, na.rm = T),
            juvenile = sum(JUVENILE, na.rm = T),
            adult = sum(ADULT, na.rm = T))

svy <- tst %>% arrange(SURVEYDATE) %>% 
  group_by(YEAR, TYPE) %>% 
  mutate(sumfawns = cumsum(JUVENILE),
         sumtotal = cumsum(TOTAL),
         ratio = sumfawns/cumsum(ADULT),
         ratio2 = sumfawns/cumsum(TOTAL))

ggplot(svy, aes(x = sumtotal, y = ratio)) + 
  geom_point(color = 'royalblue', size = .5) +
  facet_wrap(~YEAR) +
  theme_bw()

g <- ggplot(filter(svy, TYPE == 'Post-Season' & ratio < 1.25), aes(x = sumtotal, y = ratio, color = YEAR)) + 
  #geom_point(size = 1, shape = 21) +
  geom_line(aes(group = YEAR)) +
  viridis::scale_color_viridis() +
  theme_void() +
  theme(legend.position = 'none',
        legend.direction = 'horizontal',
        legend.title = element_blank())
g
ggExtra::ggMarginal(g, col = 'grey', margins = 'y')

## NAs to 0s
srvy$FEMALE[is.na(srvy$FEMALE)] <- 0
srvy$JUVENILE[is.na(srvy$JUVENILE)] <- 0
srvy$MALE[is.na(srvy$MALE)] <- 0
srvy$ADULT[is.na(srvy$ADULT)] <- 0

## remove rows where no animals are observerd?
srvy$femjuv <- srvy$JUVENILE + srvy$ADULT
sub.srvy <- subset(srvy, srvy$femjuv > 0)
data.frame(sub.srvy)

# function to calculate RSE & CI
RatioFun<-function (df) {
  yr <- unique(df$YEAR)
  ret.df <- data.frame()
  
  for (i in seq_along(yr)) {
    d <- df[df$YEAR == yr[i], ]
    
    pf<-sum(d$JUVENILE, na.rm=T)/(sum(d$ADULT, na.rm=T)+sum(d$JUVENILE, na.rm=T))
    r<-pf/(1-pf)
    n<-nrow(d)
    ttl<-sum(d$JUVENILE + d$ADULT, na.rm = T)
    fsq<-sum(d$JUVENILE^2, na.rm=T)
    t<-sum(d$ADULT, na.rm=T)
    tsq<-sum(d$ADULT^2, na.rm=T)
    fxt<-sum(d$JUVENILE*d$ADULT, na.rm=T)
    rse<-(n*((fsq+(r^2*tsq))-(2*r*fxt))/(t^2*(n-1)))^0.5
    r90ci <- rse * 1.645
    year <- yr[i]
    
    dat <- cbind(year, pf, r, n, ttl, fsq, t, tsq, fxt, rse, r90ci)
    ret.df <- rbind(ret.df, dat)
    
  }
  return(ret.df)
}

xdplyr <- srvy %>% 
  mutate(sub = JUVENILE + ADULT) %>% 
  filter(sub > 0) %>% 
  group_by(YEAR) %>% 
  summarize(
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
    rse = (n*((fsq+(r^2*tsq))-(2*r*fxt))/(t^2*(n-1)))^0.5,
    r90ci = rse * 1.645,
    lwci = r - r90ci,
    upci = r + r90ci
  )

xdplyr <- srvy %>% 
  mutate(sub = JUVENILE + ADULT) %>% filter(sub > 0) %>% 
  group_by(YEAR)  %>% 
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

x <- RatioFun(sub.srvy) %>% arrange(year)
x$up90 <- (x$r90ci + x$r) * 100
x$lw90 <- (x$r - x$r90ci) * 100
xBreaks <- seq(min(xdplyr$YEAR), max(xdplyr$YEAR), 2)
ggplot(xdplyr, aes(x = YEAR, y = rint)) +
  geom_point(size = 2, color = 'royalblue') +
  geom_line(linetype = 'dashed', color = 'royalblue') +
  geom_errorbar(aes(ymin = lwci, ymax = upci), color = 'royalblue', width = .25) +
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(labels = xBreaks, 
                     breaks = xBreaks) +
  labs(x = 'Year', y = 'Fawns per 100 Adults', title = 'Area 14 annual fawn ratio') +
  theme_bw()

## testing with biological year and survey type
dets <- read_csv('data/tbl_survey_details.txt')
srvy <- read_csv('data/tbl_survey_comp.txt')

### data munging
idaho <- filter(dets, UNIT %in% c('061ID', '066ID'))
dets <- filter(dets, !(UNIT %in% c('061ID', '066ID')))  ## remove idaho
dat <- left_join(srvy, dets[, 1:5], by = c('SURVEYID' = 'SURVEYID'))
dat$TIME <- strftime(lubridate::mdy_hms(dat$TIME), format = '%H:%M:%S')
dat$UNIT <- as.numeric(dat$UNIT)
dat$SURVEYDATE <- as_date(mdy_hms(dat$SURVEYDATE))
dat$YEAR <- year(dat$SURVEYDATE)
dat$MONTH <- month(dat$SURVEYDATE)
dat$BIOYEAR <- ifelse(dat$MONTH > 5, yes = dat$YEAR, no = dat$YEAR - 1)


mapdat <- dat %>%  
  filter(UNIT %in% 140:149) %>% 
  select(SURVEYID, SURVEYDATE, TIME, EASTING_X, NORTHING_Y, SPECIES, TOTAL,
         ADULT, JUVENILE, MALE, FEMALE, UNIT, YEAR, TYPE, BIOYEAR)

surveySum <- mapdat %>% 
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
  ) %>% 
  arrange(BIOYEAR)

xBreaks <- seq(min(surveySum$BIOYEAR) - 1, max(surveySum$BIOYEAR), 2)
ggplot(surveySum, aes(x = BIOYEAR, y = rint, group = TYPE, color = TYPE)) +
  geom_point(size = 2) +
  geom_line(linetype = 'dashed') +
  geom_errorbar(aes(ymin = lwci, ymax = upci), width = .25) +
  scale_color_gdocs(name = 'Survey Type') +
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(labels = xBreaks, 
                     breaks = xBreaks) +
  labs(x = 'Year', y = 'Fawns per 100 Adults', title = 'Area 14 annual fawn ratio') +
  theme_bw() +
  theme(legend.position = 'top')

surveySum %>% 
  select(BIOYEAR, TYPE, rint) %>% 
  ungroup() %>% 
  arrange(desc(BIOYEAR), TYPE)
