library(shinydashboard)
library(shinyjs)
library(leaflet)

dashboardPage(skin = 'green',
  dashboardHeader(title = 'Game Dashboard'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Encounter', tabName = 'encounter', icon = icon('binoculars')),
      menuItem('Health', tabName = 'health', icon = icon('stethoscope')),
      menuItem('Survey', tabName = 'survey', icon = icon('globe')),
      menuItem('Figures', tabName = 'figures', icon = icon('area-chart')),
      menuItem('Data', tabName = 'data', icon = icon('database'))
    )
  ),
  
  dashboardBody(useShinyjs(),
    tabItems(
      tabItem(tabName = 'encounter',
        fluidRow(width = 12, 
          box(title = 'Get Data', width = 3,  
              selectInput('slBiologist', 'Biologist', choices = ''),
              selectInput('slLookup', 'Search By', selected = 'Hunt Unit',
                          choices = c('Management Area', 'Hunt Unit', 'Mountain Range')),
              selectizeInput('slLookupValue', '', choices = '', multiple = TRUE),
              selectizeInput('slSpecies', 'Species', choices = '', multiple = TRUE),
              selectizeInput('slYear', 'Year', choices = '', multiple = TRUE),
              actionButton('abGetData', 'Get Data', width = '100%', icon = icon('cloud-download'))
              ),
          box(title = 'Encounter Map', width = 9, height = '600px',
              leafletOutput('mpEncounter', height = '540px')
              )
        ),
        fluidRow(
          tabBox(title = 'Species Count', width = 8, height = 500, side = 'right',
            tabPanel('Annual',
                     plotOutput('plSpeciesTS', height = '430px')),
            tabPanel('Species', 
                     plotOutput('plSpeciesBar', height = '430px'))),
          box(title = 'Distribution Tables', width = 4, height = 500,
              DT::dataTableOutput('tbSppDist'))
        ),
        fluidRow(
          box(title = 'Recent Encounters', width = 12, 
            column(width = 12,
                   DT::dataTableOutput('tbEncounter')
                   ))
                )
    ),
    ##############
    # SURVEY TAB #
    ##############
    tabItem(tabName = 'survey',
        fluidRow(width = 12,
          box(title = 'Suvey Input', width = 3,
              selectInput('slSvySpecies', 'Survey Species', selected = '', choices = ''),
              selectizeInput('slSvyUnit', 'Survey Unit', selected = '', choices = '', multiple = TRUE),
              selectInput('slSvyYear', 'Survey Year', selected = '', choices = ''),
              actionButton('abSurveyData', 'Get Survey Data', width = '100%')
              ),
          box(title = 'Survey Map', width = 9, height = '600px',
              leafletOutput('mpSurvey', height = '540px')
              )
          ),
        fluidRow(width = 12,
          tabBox(title = 'Survey Data', width = 12,
            tabPanel(title = 'Summary',
                     DT::dataTableOutput('tbSurvey')),
            tabPanel(title = 'Group',
                     DT::dataTableOutput('tbSurveyGroups'))
              )),
        fluidRow(width = 12,
          tabBox(title = 'Survey Figure', width = 12, height = '600px',
            tabPanel(title = 'Trend',
                     plotOutput('plSurvey', height = '550px')),
            tabPanel(title = 'Ratio',
                     selectInput('slSuveyType', 'Survey Type', selected = 'Spring',
                                 choices = c('Annual', 'Post-Season', 'Spring')),
                     plotOutput('plRatio', height = '475px'))
              ))
          ),
    ##############
    # HEALTH TAB #
    ##############
    tabItem(tabName = 'health',
      fluidRow(width = 12,
        box(title = 'Input Box', width = 2, height = '464px',
            selectInput('slHealthColor', 'Color By:', selected = 'None',
                        choices = c('None', 'CapHuntUnit', 'CapMtnRange', 'CapYear', 'Species'))),
        tabBox(title = 'M.ovi PCR', width = 5,
          tabPanel(title = 'Bar',
                   plotOutput('plPCR')),
          tabPanel(title = 'Table',
                   DT::dataTableOutput('tbPCR'))
            ),
          tabBox(title = 'M.ovi ELISA', width = 5,
            tabPanel(title = 'Bar',
                     plotOutput('plElisa')),
            tabPanel(title = 'Table',
                     DT::dataTableOutput('tbElisa'))
          )),
      fluidRow(width = 12,
        tabBox(title = 'PI3', width = 4,
          tabPanel(title = 'Bar',
                   plotOutput('plPI3')),
          tabPanel(title = 'Table',
                   DT::dataTableOutput('tbPI3'))
            ),
          tabBox(title = 'BRSV', width = 4,
            tabPanel(title = 'Bar',
                     plotOutput('plBRSV')),
            tabPanel(title = 'Table',
                     DT::dataTableOutput('tbBRSV'))
            ),
          tabBox(title = 'BVD', width = 4,
            tabPanel(title = 'Bar',
                     plotOutput('plBVD')),
            tabPanel(title = 'Table',
                     DT::dataTableOutput('tbBVD'))
            ))
      # fluidRow(width = 12,
      #   box(title = 'Data', width = 12,
      #       textInput('txFilterDat', 'Filter Data', width = '100%'),
      #       actionButton('abFilter', 'Filter', width = '200px'),
      #       actionButton('abReset', 'Reset', width = '200px'),
      #       br(),
      #       hr(),
      #       column(width = 12,
      #              DT::dataTableOutput('tbHealthData')
      #       )
      #     ))
      ),
    
    ###############
    # FIGURES TAB #
    ###############
    tabItem(tabName = 'figures',
      fluidRow(
        box(title = 'Input', width = 3,
            selectInput('slData', 'Data', selected = 'Biometric',
                        choices = c('Biometric')),
            selectInput('slPlotType', 'Type', selected = 'Box', 
                        choices = c('Bar', 'Box', 'Scatter', 'Density', 'Violin')),
            selectInput('slXaxis', 'X', selected = 'CapHuntUnit', 
                        choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                                    'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight')),
            selectInput('slYaxis', 'Y', selected = 'Weight',
                        choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                                    'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight')),
            selectInput('slColor', 'Color', selected = ,
                        choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit')),
            selectInput('slFacet', 'Facet', selected = '',
                        choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear')),
            actionButton('abCreatePlot', 'Create Plot', icon = icon('cogs'))
            ),
        box(title = 'Figure', width = 9, height = '600px',
            plotOutput('plFigure'))
      )),
    ############
    # DATA TAB #
    ############
    tabItem(tabName = 'data',
        fluidRow(width = 12,
        tabBox(title = 'Encounter Summary', width = 12, height = '600px',
          tabPanel('Table', 
                   column(width = 12, DT::dataTableOutput('tbEncSummary', width = 'auto'))),
          tabPanel('Summary', 
                   htmlOutput('htmlEncSummary'))
                  ),
        tabBox(title = 'Biometric Summary', width = 12, height = '600px',
          tabPanel('Table', 
                   column(width = 12, DT::dataTableOutput('tbBioSummary'))),
          tabPanel('Summary', 
                   htmlOutput('htmlBioSummary'),
                   column(width = 12, DT::dataTableOutput('tbBioNumSum')))
                  ),
        tabBox(title = 'WADDL Summary', width = 12, height = '600px',
          tabPanel('Table',
                   column(width = 12, DT::dataTableOutput('tbWaddlSum'))))
        )   
        )
  )
))