library(shiny)
library(shinydashboard)
library(magrittr)
library(shinyhelper)
library(shinyWidgets)
library(colourpicker)
library(ggiraph)
library(readxl)
library(readr)
library(shinycssloaders)
library(purrr)
library(tibble)
library(htmlwidgets)
library(dplyr)
library(patchwork)
library(ggraph)
library(igraph)
library(cowplot)
library(R.utils)
library(colorspace)
library(ggnewscale)
library(DT)
library(grDevices)
library(stats)
library(tools)
library(utils)
library(ggplot2)

# Define UI for application
shinyUI(dashboardPage(
    title = 'clevR-vis',
    ###HEADER
    dashboardHeader(
      title = span(div(
        column(2, img(src = "logo_version3_black.png", width = '50px')),
        column(8,"clevRvis"),
        column(2,img(src = "logoIMI.png", width = "50px"))
      )),
      titleWidth = '100%',

      #instructions ######
      dropdownMenu(
        type = "notifications",
        headerText = strong("INSTRUCTIONS"),
        icon = icon("question"),
        badgeStatus = NULL,
        notificationItem(
          text = 'Start at the "Inputs" section (left menu) and upload a file
                  (excel or csv) containing the clones cancer cell fractions.',
          icon = icon('download')
        ),
        notificationItem(
          text = 'Specify the time points, clone labels and parental relations
                  and validate them by clicking "Submit Inputs".',
          icon = icon('toggle-on')
        ),
        notificationItem(
          text = 'When inputs are valid, move to the "seaObject options"
                  section and select the desired estimations and click the
                  "submit" button.',
          icon = icon('filter')
        ),
        notificationItem(
          text = 'Check the Cancer Cell Fractions table created, including the
                  parental relations. If everything looks right you can now
                  plot the data.',
          icon = icon('table')
        ),
        notificationItem(
          text = 'On the "Plots" section you can decide which plots you want to
                  visualize. Each subsection shows a kind of plot and the
                  corresponding filters.',
          icon = icon('chart-area')
        ),
        notificationItem(
          text = 'To add annotations to "Dolphin or Plaice plot" go to
                  "Annotations" section (left menu). There you find a
                  subsection for each clone where you can specify the text and
                  position of the annotation.',
          icon = icon('pencil')
        )
      )
    ),

    ###SIDEBAR
    dashboardSidebar(
        width = 400,
        sidebarMenu(
          id = 'sidebar',
          menuItem('Outputs', tabName = 'outputs', icon = icon('table')),

          br(),

          h4(' INPUTS & OPTIONS', style = 'color: #343a40;'),

          ##Inputs PANEL #####
          menuItem(
            'Inputs',
            icon = icon('download'),

            br(),

            ##fractable (CCFs) file as excel or csv
            fileInput('fileCCF','Cancer Cell Fractions (CCFs) table
                      (xlsx, xls, csv files supported)',
                      accept = c('.csv','.xls','.xlsx')),

            br(),

            ##timepoints vector
            prettySwitch(
              'tpCols', 'First row contains time point labels (numeric)?',
              status = 'primary', slim = TRUE, value = TRUE) %>%
              helper(
                type = "inline",
                icon = 'circle-question',
                style = "position: absolute; left: 40px;",
                title = "Defining time points",
                content = c("<p><strong>Does the first row of the input file
                              correspond to the time points?</strong></p>",
                            "Time points must be numeric values!",
                            "To modify the time points from the app the input
                            file must not contain them.")
              ),

            conditionalPanel(
              condition = '!input.tpCols',
              prettySwitch('dfltTP', 'Use default time points? (1,2...)',
                           status = 'primary', slim = TRUE),
              conditionalPanel(
                condition = '!input.dfltTP',
                textInput('timepoints','Vector with time points') %>%
                  helper(
                    type = "inline",
                    style = "position: absolute; left: 170px;",
                    icon = 'circle-question',
                    title = "Defining manual time points",
                    content = c("Write the time points in the text box
                                 separated by commas",
                                "Time points must be numeric values",
                                "To add the time points from the app the input
                                 file must not contain them.")
                  )
              )
            ),

            br(),

            #labels
            prettySwitch('labRows', 'First column contains clone labels?',
                         status = 'primary', slim = TRUE, value = TRUE) %>%
              helper(
                type = "inline",
                icon = 'circle-question',
                style = "position: absolute; left: 40px;",
                title = "Defining Clone Labels",
                content = c("<p><strong>Does the first column of the input file
                              correspond to the clone labels?</strong></p>",
                              "To modify the clone labels from the app the
                              input file must not contain them.")
              ),

            conditionalPanel(
              condition = '!input.labRows',
              prettySwitch('cloneLabs', 'Use default clone labels',
                           status = 'primary', slim = TRUE),
              conditionalPanel(
                condition = '!input.cloneLabs',
                textInput('cloneLabsValues',
                          'Manual custom labels for clones')
              )
            ),

            br(),

            ##parental relations
            radioGroupButtons(
              'parentCol', 'How to define parental relations?',
              choiceNames = c('Column "parents" included',
                          'Define parental relations interactively'),
              choiceValues = c('column','interactive')
            )  %>%
              helper(
                type = "inline",
                icon = 'circle-question',
                style = "position: absolute; left: 100px;",
                title = "How to define parental relations",
                content = c("<p><strong>Does the input table contain a column
                            of parental relations?</strong></p>",
                            'The column of parental relations must be
                            named "parents". And it can be in any position
                            except the first column if clone labels
                            are also defined in the input table.',
                            'Parental relations must be defined by integer
                            values, indicating the position of the parent clone
                            (first row =  1). To indicate a clone emerging from
                            normal cell, the value must be 0.',
                            "To interactively define parental relations, the
                            input table must not contain them.",
                            "<p><strong>The input table doesn't contain a column
                            of parental relations?</strong></p>",
                            'A dropdown for each clone will appear with the
                            possible parents to interactively define the
                            parental relations.')
              ),

            ##TEMPORAL check parents
            #textOutput('checkColNames'),
            # textOutput('checkParents'),

            conditionalPanel(
              condition = 'input.parentCol == "interactive" & !input.expTrees',
              h5(tags$b('Interactively define parental relations:')),
              htmlOutput('askTblParents'),
              uiOutput('parents')
            ),

            br(),

            prettySwitch(
              'expTrees', 'Explore alternative trees',
              status = 'primary', slim = TRUE, value = FALSE) %>%
              helper(
                type = "inline",
                icon = 'circle-question',
                style = "position: absolute; left: 40px;",
                title = "Defining time points",
                content = c("<p><strong>Exploring alternative
                            trees</strong></p>",
                            "When switched on a dropdown list will appear
                            containing the possible parental relations for the
                            table of Cancer Cell Fractions uploaded")
              ),

            ##TEMPORAL check
            # textOutput('checkAltTrees'),

            conditionalPanel(
              condition = 'input.expTrees',
              selectInput('optParents','Alternative parental relations:',
                          choices = c('a','b'))
            ),

            br(), hr(),

            actionBttn("subInputs", "Submit inputs",
                       style = "jelly", color = "success", size = 'sm'),

            useSweetAlert(),

            br()

          ), #end of inputs panel

          ##filters PANEL #####
          menuItem(
            'seaObject options',
            icon = icon('filter'),

            #timepoints interpolation
            prettySwitch('tpEstim', 'Extra time points interpolation',
                         status = 'primary', slim = TRUE, value = TRUE),
            conditionalPanel(
              condition = 'input.tpEstim',
              ##Custom original timepoint
              prettySwitch('customOgTP', 'Custom time point of origin',
                           status = 'primary', slim = TRUE, value = FALSE),

              conditionalPanel(condition = 'input.customOgTP',
                               numericInput('ogTP', 'Time point of origin:',
                                            max = 0,value=-10))
            ),

            hr(),

            #therapy effect estimation
            prettySwitch('therapyEffect', 'Therapy Effect estimation',
                         status = 'primary', slim = TRUE),

            conditionalPanel(
              condition = 'input.therapyEffect',
              radioGroupButtons("thpEf_opt", label = "",
                                choiceNames = c("Specific time point",
                                                "Two measured time points"),
                                choiceValues = c(1,2),
                                justified = TRUE
              ),

              conditionalPanel(
                condition = 'input.thpEf_opt == 1',
                sliderInput('tpThp',
                            'Select time point to estimate therapy effect:',
                            min = 0, max = 0, value = 0)),

              conditionalPanel(
                condition = 'input.thpEf_opt == 2',
                sliderTextInput('MtpThp','Select measured time points:',
                                choices = c(1,2), selected = c(1,2),
                                grid = TRUE))
            ),

            hr(),

            #ccf or proportions switch
            radioGroupButtons('ccfVSprop', 'Which proportions to show?',
                              choiceNames = c('CCF_clone',
                                              'CCF_clone - sum(CCF_children)'),
                              choiceValues = c('ccf', 'prop')) %>%
              helper(
                type = "inline",
                icon = 'circle-question',
                style = "position: absolute; left: 200px;",
                title = "Proportions to visualize in seaObject table",
                content = c("<p><strong>CCF_clone</strong></p>",
                            "Each value represents the absolute proportion of a
                            certain clone at a certain time point. The sum of
                            all CCF values at one timepoint can be >100% as
                            each CCF value includes the proportion of the
                            corresponding clone and its children",
                            "",
                            "<p><strong>CCF_clone - sum(CCF_children)</strong>
                            </p>",
                            "Each value represents the proportion of only the
                            corresponding clone. The sum of all proportion
                            values at one timepoint can NOT be >100% as the
                            proportions are calculated like:",
                            "CCF_clone - sum(CCF_children)",
                            "",
                            "<p><strong>Example:</strong></p>",
                            "CloneA -> CloneB -> CloneC",
                            "At time point 1:",
                            "CCF_clone: CloneA = 70%,  CloneB = 50%,
                            CloneC = 10%",
                            "CCF_clone - sum(CCF_children): CloneA = 20%,
                            CloneB = 40%, CloneC = 10%")
              ),

            hr(),

            ##button to submit all changes
            actionBttn("submitSea", "Submit", style = "jelly",
                       color = "success", size = 'sm'),
            useSweetAlert()
          ), #end of seaObject filter panel

          ##PLOTS PANEL #####
          menuItem(
            'Plots',
            icon = icon('chart-area'),
            ##shark plot
            menuItem(
              'Shark Plot', tabName = 'shkDph',
              prettySwitch("shark", "Show Shark Plot",
                           status = 'primary', slim = TRUE),

              ##FILTERS ONLY VISIBLE WHEN SHARK PLOT IS SELECTED
              conditionalPanel(
                condition = 'input.shark',
                h4('Shark plot filters'),
                prettySwitch('legendShk', 'Show legend',
                             status = 'primary',slim = TRUE),
                textInput('mainShk', 'Main'),
                prettySwitch('extendedShk', 'Extended Shark Plot',
                             status = 'primary', slim = TRUE),
                conditionalPanel(
                  condition = 'input.extendedShk',
                  checkboxGroupInput('showTps', 'Shown time points',
                                     choices = c('Measured time points',
                                                 'Estimated time points',
                                                 'Custom'),
                                     selected = 'Measured time points'),
                  #only show if custom Vlines is selected
                  conditionalPanel(
                    condition="$.inArray('Custom', input.showTps) > -1",
                    textInput('customShowTps',
                              'Add manual existing time points to show')
                  )
                )
              )
            ), #end of shark menuItem

            ##dolphin plot
            menuItem(
              'Dolphin Plot',
              prettySwitch('dolphin', 'Show Dolphin Plot',
                           status = 'primary', slim = TRUE),

              ##FILTERS ONLY VISIBLE WHEN DOLPHIN PLOT IS SELECTED
              conditionalPanel(
                condition="input.dolphin",
                h4('Dolphin plot filters'),
                prettySwitch('legendDol', 'Show legend',
                             status = 'primary',slim = TRUE),
                prettySwitch('sepIndCl', 'Separate independent clones?',
                             status = 'primary',slim = TRUE),
                textInput('mainDol', 'Main'),
                radioGroupButtons('pos', 'Layout position',
                                  choices = c('center', 'bottom'),
                                  selected = 'center', justified = TRUE),
                radioGroupButtons('shape', 'Dolphin plot shape',
                                  choices = c('spline','polygon'),
                                  justified = TRUE),
                conditionalPanel(
                  condition = "input.shape == 'spline'",
                  sliderInput('pad', 'Pad Left', min = 0.001, max = 0.25,
                              step = 0.001, value = 0.005)
                ),

                prettySwitch('axisLab', 'Axis labels',
                             status = 'primary',slim = TRUE),
                conditionalPanel(condition = 'input.axisLab',
                                 textInput('xlab', 'x axis label'),
                                 textInput('ylab', 'y axis label')),
                colourInput('borderCol', 'Clones border color',
                            value = '#FFFFFF00', allowTransparent = TRUE),
                checkboxGroupInput('vlines', 'Vertical lines x position',
                                   choices = c('Measured time points',
                                               'Estimated time points',
                                               'Custom'),
                                   selected = 'Measured time points'),
                #only show if custom Vlines is selected
                conditionalPanel(
                  condition="$.inArray('Custom', input.vlines) > -1",
                  textInput('customVlines', 'Add manual vertical lines')
                ),
                ##only show if vlines is not NULL
                conditionalPanel(
                  condition ="input.vlines.length > 0",
                  colourInput('vlineCol', 'Vertical lines color',
                              value = '#6E6E66'),
                  checkboxGroupInput('vlab', 'Vertical lines labels',
                                     choices = c('Measured time points',
                                                 'Estimated time points',
                                                 'Custom'),
                                     selected = 'Measured time points')
                ),
                #only show if custom vlabs is selected
                conditionalPanel(
                  condition ="$.inArray('Custom', input.vlab) > -1",
                  textInput('customVlabs',
                            'Add manual labels for vertical lines')
                ),
                ##only show if vlabs is not NULL
                conditionalPanel(
                  condition ="input.vlab.length > 0",
                  sliderInput('vlabSize', 'Size of the vertical lines labels',
                              min = 1, max = 10, value = 5)
                ),
                checkboxGroupInput('symbols', 'Show symbol underneath',
                                   choices = c('Measured time points',
                                               'Estimated time points',
                                               "Custom")),
                #only show if custom symbols is selected
                conditionalPanel(
                  condition ="$.inArray('Custom', input.symbols) > -1",
                  textInput('customSymbols',
                            'Add manual x positions showing symbols'))
                )
              ), ##end of dolph menu item

            ##Plaice plot
            menuItem(
              'Plaice Plot', tabName = 'plaice',
              prettySwitch('plaice', 'Show Plaice Plot',
                           status = 'primary', slim = TRUE),

              ##FILTERS ONLY VISIBLE WHEN PLAICE PLOT IS SELECTED
              conditionalPanel(
                condition="input.plaice",
                h4('Plaice plot filters'),
                prettySwitch('legendPlc', 'Show legend',
                             status = 'primary',slim = TRUE),
                prettySwitch('sepIndClPlc', 'Separate independent clones?',
                             status = 'primary',slim = TRUE),
                prettySwitch('axisLabPlc', 'Axis labels',
                             status = 'primary',slim = TRUE),
                prettySwitch('fillClones', 'Mark biallelic events',
                             status = 'primary',slim = TRUE) %>%
                  helper(
                    type = "inline",
                    icon = 'circle-question',
                    style = "position: absolute; left: 177px;",
                    title = "How to color biallelic events?",
                    content = c("To show biallelic events have happened in a
                                clone you can color them.",
                                "<body><div><li> A not colored clone means
                                there is at least one copy of a healthy allele
                                </li> <li> A clone (e.g Clone 2) colored with
                                the color of another clone (e.g Clone 1) means a
                                new variant (in Clone 2) is causing a biallelic
                                event on the same gene that was affected by the
                                clone which color you are using (Clone 1).</li>
                                <ol> <i>For example</i>: consider linear clonal
                                evolution of 2 clones -- A and B.
                                Clone A is characterized by a mutation in
                                <i>TP53</i>, clone B by a deletion 17p.
                                The two variants are overlapping. If they affect
                                different alleles, no healthy allele of
                                <i>TP53</i>  remains in all cells belonging to
                                clone B. You may chose to color clone B in the
                                lower plot to indicate a decrease in healthy
                                alleles of <i>TP53</i> as the CCF of clone B
                                increases. Instead, if both variants affect the
                                same allele, one healthy copy of <i>TP53</i>
                                remains -- independent of the CCF of clones A
                                and B. Thus, no clone should be colored in the
                                lower plot.</li></ol> <li> A clone colored with
                                its own color means it implies on itself a
                                biallelic event,or it affects the only
                                available X- or Y- chromosome in male subjects.
                                </li> </div></body>")
                  ),
                ##temp check
                #textOutput('clonesfillvector'),

                conditionalPanel(
                  condition = 'input.fillClones',
                  uiOutput('fillVec')
                ),

                textInput('mainPlc', 'Main'),
                radioGroupButtons('shapePlc', 'Plaice plot shape',
                                  choices = c('spline','polygon'),
                                  justified = TRUE),
                colourInput('borderColPlc', 'Clones border color',
                            value = '#000000',  allowTransparent = TRUE),
                checkboxGroupInput('vlinesPlc', 'Vertical lines x position',
                                   choices = c('Measured time points',
                                               'Estimated time points',
                                               'Custom'),
                                   selected = 'Measured time points'),
                #only show if custom Vlines is selected
                conditionalPanel(
                  condition="$.inArray('Custom', input.vlinesPlc) > -1",
                  textInput('customVlinesPlc', 'Add manual vertical lines')
                ),
                ##only show if vlines is not NULL
                conditionalPanel(
                  condition ="input.vlinesPlc.length > 0",
                  colourInput('vlineColPlc', 'Vertical lines color',
                              value = '#6E6E66'),
                  checkboxGroupInput('vlabPlc', 'Vertical lines labels',
                                     choices = c('Measured time points',
                                                 'Estimated time points',
                                                 'Custom'),
                                     selected = 'Measured time points'),
                  #only show if custom vlabs is selected
                  conditionalPanel(
                    condition ="$.inArray('Custom', input.vlabPlc) > -1",
                    textInput('customVlabsPlc',
                              'Add manual labels for vertical lines')
                  ),
                  ##only show if vlabs is not NULL
                  conditionalPanel(
                    condition ="input.vlabPlc.length > 0",
                    sliderInput('vlabSizePlc',
                                'Size of the vertical lines labels',
                                min = 1, max = 10, value = 5)
                  )
                ),

                checkboxGroupInput('symbolsPlc', 'Show symbol underneath:',
                                   choices = c('Measured time points',
                                               'Estimated time points',
                                               "Custom")),
                #only show if custom symbols is selected
                conditionalPanel(
                  condition ="$.inArray('Custom', input.symbolsPlc) > -1",
                  textInput('customSymbols',
                            'Add manual x positions where to show symbols')
                )
              )
            ) #end of plaice plot menu item
          ), #end of plots panel

            ###### Annotations ####
            menuItem('Annotations', icon = icon('pencil'),
                     prettySwitch('addAnnot', 'Add annotations',
                                  status = 'primary', slim = TRUE),
                     menuItemOutput('annot')
                     ), ##end of annotations panel

            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

            ###Tutorial ####
            menuItem("Tutorial",
                     icon = icon("circle-question"),
                     tabName = "instructions"),

            ###About us ####
            menuItem("About Us",
                     icon = icon("info"),
                     tabName = "information")
        ) #end of sidebar menu
    ), #end of sidebar


    # Outputs #####
    dashboardBody(
        tabItems(
          tabItem(
            tabName = 'outputs',
            tabBox(id = "box_general",
                   width = NULL, height = NULL,
                   tabPanel('Input table',
                            textOutput('warnParents'),
                            tags$head(tags$style("#warnParents{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                            ),
                            h3('Input table'),
                            textOutput('inTableAsk'),
                            DT::dataTableOutput('OGccfTable'),
                            h3('Parental relations:'),
                            textOutput('colParents'),
                            tags$head(tags$style("#colParents{color: black;
                                 font-size: 17px;
                                 }"))
                   ),
                   tabPanel('seaObject table',
                            h3('seaObject Cancer Cell Fractions table'),
                            textOutput('inInputsAsk'),
                            DT::dataTableOutput('ccfTable')),
                   tabPanel('Plots',
                            tabBox(id = 'box_plots',
                                   width = NULL, height = NULL,
                                   tabPanel('Shark & Dolphin plots',
                                            textOutput('inShkDphAsk'),
                                            shinycssloaders::withSpinner(
                                              girafeOutput('intPlot'))
                                   ),
                                   tabPanel('Plaice plot',
                                            textOutput('inPlaiceAsk'),
                                            shinycssloaders::withSpinner(
                                              girafeOutput('plaicePlot')))
                            )
                   )
            )),


            ## tutorial
            tabItem(
              tabName = 'instructions',
              fluidRow(
                align="center",
                HTML('<h1><i class="fas fa-book"></i> <b> Tutorial </b></h1>')),
              br(),

              fluidRow(column(1),
                       column(10,
                              box(
                                width = NULL,
                                height = NULL,
                                includeHTML("tutorial_text.html"))),
                        column(1))
            ), #end of instructions tab

            ## About Us
            tabItem(
              tabName = "information",
              fluidRow(
                align="center",
                HTML('<h1><i class="fas fa-info"></i> <b> About Us</b></h1>')),
              br(),
              fluidRow(column(1),
                       column(10,
                              box(
                                width = NULL,
                                height = NULL,
                                includeHTML("aboutUs_text.html"))),
                       column(1))
            ) #end of about us
        ),
        tags$head(
            tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "appStyle.css")
        )
    ) #end of body
))


