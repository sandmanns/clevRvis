clevRvisShiny <- function() {
    shinyApp(
        ui=shinyUI(
            # Define UI for application
            shinyUI(dashboardPage(
                title = 'clevR-vis',
                ###HEADER
                dashboardHeader(
                    title = span(div(
                        column(2, img(src = "clevRvis/logo_version3_black.png", 
                                        width = '50px')),
                        column(8,"clevRvis"),
                        column(2,img(src = "clevRvis/logoIMI.png", width = 
                                        "50px"))
                    )),
                    titleWidth = '100%',
                    
                    #instructions ######
                    dropdownMenu(
                        type = "notifications",
                        headerText = strong("INSTRUCTIONS"),
                        icon = icon("question"),
                        badgeStatus = NULL,
                        notificationItem(
                            text = 'Start at the "Inputs" section (left menu) 
                            and upload a file (excel or csv) containing the 
                            clones cancer cell fractions.',
                            icon = icon('download')
                        ),
                        notificationItem(
                            text = 'Specify the time points, clone labels and 
                            parental relations and validate them by clicking 
                            "Submit Inputs".',
                            icon = icon('toggle-on')
                        ),
                        notificationItem(
                            text = 'When inputs are valid, move to the 
                            "seaObject options" section and select the desired 
                            estimations and click the "submit" button.',
                            icon = icon('filter')
                        ),
                        notificationItem(
                            text = 'Check the Cancer Cell Fractions table 
                            created, including the parental relations. If 
                            everything looks right you can now plot the data.',
                            icon = icon('table')
                        ),
                        notificationItem(
                            text = 'On the "Plots" section you can decide which 
                            plots you want to visualize. Each subsection shows a 
                            kind of plot and the corresponding filters.',
                            icon = icon('chart-area')
                        ),
                        notificationItem(
                            text = 'To add annotations to "Dolphin or Plaice 
                            plot" go to "Annotations" section (left menu). There 
                            you find a subsection for each clone where you can 
                            specify the text and position of the annotation.',
                            icon = icon('pencil')
                        )
                    )
                ),
                
                ###SIDEBAR
                dashboardSidebar(
                    width = 400,
                    sidebarMenu(
                        id = 'sidebar',
                        menuItem('Outputs', tabName = 'outputs', 
                                icon = icon('table')),
                        
                        br(),
                        
                        h4(' INPUTS & OPTIONS', style = 'color: #343a40;'),
                        
                        ##Inputs PANEL #####
                        menuItem(
                            'Inputs',
                            icon = icon('download'),
                            
                            br(),
                            
                            ##fractable (CCFs) file as excel or csv
                            fileInput('fileCCF','Cancer Cell Fractions (CCFs) 
                            table (xlsx, xls, csv files supported)',
                                    accept = c('.csv','.xls','.xlsx')),
                            
                            br(),
                            
                            ##timepoints vector
                            prettySwitch(
                                'tpCols', 'First row contains time point labels 
                                (numeric)?',
                                status = 'primary', slim = TRUE, value = TRUE) %>%
                                helper(
                                    type = "inline",
                                    icon = 'circle-question',
                                    style = "position: absolute; left: 40px;",
                                    title = "Defining time points",
                                    content = c("<p><strong>Does the first row 
                                    of the input file correspond to the time 
                                                points?</strong></p>",
                                                "Time points must be numeric 
                                                values!",
                                                "To modify the time points from 
                                                the app the input
                            file must not contain them.")
                                ),
                            
                            conditionalPanel(
                                condition = '!input.tpCols',
                                prettySwitch('dfltTP', 'Use default time points? 
                                            (1,2...)',
                                            status = 'primary', slim = TRUE),
                                conditionalPanel(
                                    condition = '!input.dfltTP',
                                    textInput('timepoints','Vector with time points') %>%
                                        helper(
                                            type = "inline",
                                            style = "position: absolute; left: 
                                            170px;",
                                            icon = 'circle-question',
                                            title = "Defining manual time 
                                            points",
                                            content = c("Write the time points 
                                            in the text box separated by commas",
                                                        "Time points must be 
                                                        numeric values",
                                                        "To add the time points 
                                                        from the app the input
                                file must not contain them.")
                                        )
                                )
                            ),
                            
                            br(),
                            
                            #labels
                            prettySwitch('labRows', 'First column contains clone 
                                        labels?',
                                        status = 'primary', slim = TRUE, value = TRUE) %>%
                                helper(
                                    type = "inline",
                                    icon = 'circle-question',
                                    style = "position: absolute; left: 40px;",
                                    title = "Defining Clone Labels",
                                    content = c("<p><strong>Does the first 
                                    column of the input file
                                correspond to the clone labels?</strong></p>",
                                                "To modify the clone labels from 
                                                the app the
                                input file must not contain them.")
                                ),
                            
                            conditionalPanel(
                                condition = '!input.labRows',
                                prettySwitch('cloneLabs', 'Use default clone 
                                            labels',
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
                                'parentCol', 'How to define parental 
                                relations?',
                                choiceNames = c('Column "parents" included',
                                                'Define parental relations 
                                                interactively'),
                                choiceValues = c('column','interactive')
                            )  %>%
                                helper(
                                    type = "inline",
                                    icon = 'circle-question',
                                    style = "position: absolute; left: 100px;",
                                    title = "How to define parental relations",
                                    content = c("<p><strong>Does the input table 
                                    contain a column
                            of parental relations?</strong></p>",
                                                'The column of parental 
                                                relations must be
                            named "parents". And it can be in any position
                            except the first column if clone labels
                            are also defined in the input table.',
                                                'Parental relations must be 
                                                defined by integer
                            values, indicating the position of the parent clone
                            (first row =  1). To indicate a clone emerging from
                            normal cell, the value must be 0.',
                                                "To interactively define 
                                                parental relations, the
                            input table must not contain them.",
                                                "<p><strong>The input table 
                                                doesn't contain a column
                            of parental relations?</strong></p>",
                                                'A dropdown for each clone will 
                                                appear with the
                            possible parents to interactively define the
                            parental relations.')
                                ),
                            
                            ##TEMPORAL check parents
                            #textOutput('checkColNames'),
                            # textOutput('checkParents'),
                            
                            conditionalPanel(
                                condition = 'input.parentCol == "interactive" & !input.expTrees',
                                h5(tags$b('Interactively define parental 
                                        relations:')),
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
                                    content = c("<p><strong>Exploring 
                                    alternative
                            trees</strong></p>",
                                                "When switched on a dropdown 
                                                list will appear
                            containing the possible parental relations for the
                            table of Cancer Cell Fractions uploaded")
                                ),
                            
                            ##TEMPORAL check
                            # textOutput('checkAltTrees'),
                            
                            conditionalPanel(
                                condition = 'input.expTrees',
                                selectInput('optParents','Alternative parental
                                            relations:',
                                            choices = c('a','b'))
                            ),
                            
                            br(), hr(),
                            
                            actionBttn("subInputs", "Submit inputs",
                                        style = "jelly", color = "success", 
                                        size = 'sm'),
                            
                            useSweetAlert(),
                            
                            br()
                            
                        ), #end of inputs panel
                        
                        ##filters PANEL #####
                        menuItem(
                            'seaObject options',
                            icon = icon('filter'),
                            
                            #timepoints interpolation
                            prettySwitch('tpEstim', 'Extra time points 
                                        interpolation',
                                        status = 'primary', slim = TRUE, 
                                        value = TRUE),
                            conditionalPanel(
                                condition = 'input.tpEstim',
                                ##Custom original timepoint
                                prettySwitch('customOgTP', 'Custom time point of 
                                            origin',
                                            status = 'primary', slim = TRUE, 
                                            value = FALSE),
                                
                                conditionalPanel(condition = 'input.customOgTP',
                                                numericInput('ogTP', 'Time 
                                                            point of origin:',
                                                            max = 0,value=-10))
                            ),
                            
                            hr(),
                            
                            #therapy effect estimation
                            prettySwitch('therapyEffect', 'Therapy Effect 
                                        estimation',
                                        status = 'primary', slim = TRUE),
                            
                            conditionalPanel(
                                condition = 'input.therapyEffect',
                                radioGroupButtons("thpEf_opt", label = "",
                                                choiceNames = c("Specific time 
                                                                point",
                                                                "Two measured 
                                                                time points"),
                                                choiceValues = c(1,2),
                                                justified = TRUE
                                ),
                                
                                conditionalPanel(
                                    condition = 'input.thpEf_opt == 1',
                                    sliderInput('tpThp',
                                                'Select time point to estimate 
                                                therapy effect:',
                                                min = 0, max = 0, value = 0)),
                                
                                conditionalPanel(
                                    condition = 'input.thpEf_opt == 2',
                                    sliderTextInput('MtpThp','Select measured 
                                                    time points:',
                                                    choices = c(1,2), 
                                                    selected = c(1,2),
                                                    grid = TRUE))
                            ),
                            
                            hr(),
                            
                            #ccf or proportions switch
                            radioGroupButtons('ccfVSprop', 'Which proportions 
                                            to show?',
                                            choiceNames = c('CCF_clone',
                                                            'CCF_clone - sum(CCF_children)'),
                                            choiceValues = c('ccf', 'prop')) %>%
                                helper(
                                    type = "inline",
                                    icon = 'circle-question',
                                    style = "position: absolute; left: 200px;",
                                    title = "Proportions to visualize in 
                                    seaObject table",
                                    content = c("<p><strong>CCF_clone</strong></p>",
                                                "Each value represents the 
                                                absolute proportion of a
                            certain clone at a certain time point. The sum of
                            all CCF values at one timepoint can be >100% as
                            each CCF value includes the proportion of the
                            corresponding clone and its children",
                                                "",
                                                "<p><strong>CCF_clone - sum(CCF_children)</strong>
                            </p>",
                                                "Each value represents the 
                                                proportion of only the
                            corresponding clone. The sum of all proportion
                            values at one timepoint can NOT be >100% as the
                            proportions are calculated like:",
                                                "CCF_clone - sum(CCF_children)",
                                                "",
                                                "<p><strong>Example:</strong></p>",
                                                "CloneA -> CloneB -> CloneC",
                                                "At time point 1:",
                                                "CCF_clone: CloneA = 70%,  
                                                CloneB = 50%,
                            CloneC = 10%",
                                                "CCF_clone - sum(CCF_children): 
                                                CloneA = 20%,
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
                                
                                ##FILTERS ONLY VISIBLE WHEN SHARK PLOT IS 
                                ##SELECTED
                                conditionalPanel(
                                    condition = 'input.shark',
                                    h4('Shark plot filters'),
                                    prettySwitch('legendShk', 'Show legend',
                                                status = 'primary',
                                                slim = TRUE),
                                    textInput('mainShk', 'Main'),
                                    prettySwitch('extendedShk', 'Extended Shark 
                                                Plot',
                                                status = 'primary', 
                                                slim = TRUE),
                                    conditionalPanel(
                                        condition = 'input.extendedShk',
                                        checkboxGroupInput('showTps', 
                                                            'Shown time points',
                                                            choices = c('Measured time points',
                                                                        'Estimated time points',
                                                                        'Custom'),
                                                            selected = 'Measured time points'),
                                        #only show if custom Vlines is selected
                                        conditionalPanel(
                                            condition="$.inArray('Custom', input.showTps) > -1",
                                            textInput('customShowTps',
                                                    'Add manual existing time 
                                                    points to show')
                                        )
                                    )
                                )
                            ), #end of shark menuItem
                            
                            ##dolphin plot
                            menuItem(
                                'Dolphin Plot',
                                prettySwitch('dolphin', 'Show Dolphin Plot',
                                            status = 'primary', slim = TRUE),
                                
                                ##FILTERS ONLY VISIBLE WHEN DOLPHIN PLOT IS 
                                ##SELECTED
                                conditionalPanel(
                                    condition="input.dolphin",
                                    h4('Dolphin plot filters'),
                                    prettySwitch('legendDol', 'Show legend',
                                                status = 'primary',
                                                slim = TRUE),
                                    prettySwitch('sepIndCl', 'Separate 
                                                independent clones?',
                                                status = 'primary',
                                                slim = TRUE),
                                    textInput('mainDol', 'Main'),
                                    radioGroupButtons('pos', 'Layout position',
                                                    choices = c('center', 
                                                                'bottom'),
                                                    selected = 'center', 
                                                    justified = TRUE),
                                    radioGroupButtons('shape', 'Dolphin plot 
                                                    shape',
                                                    choices = c('spline',
                                                                'polygon'),
                                                    justified = TRUE),
                                    conditionalPanel(
                                        condition = "input.shape == 'spline'",
                                        sliderInput('pad', 'Pad Left', 
                                                    min = 0.001, max = 0.25,
                                                    step = 0.001, value = 0.005)
                                    ),
                                    
                                    prettySwitch('axisLab', 'Axis labels',
                                                status = 'primary',
                                                slim = TRUE),
                                    conditionalPanel(condition = 'input.axisLab',
                                                    textInput('xlab', 
                                                                'x axis label'),
                                                    textInput('ylab', 
                                                                'y axis label')),
                                    colourInput('borderCol', 'Clones border 
                                                color',
                                                value = '#FFFFFF00', 
                                                allowTransparent = TRUE),
                                    checkboxGroupInput('vlines', 'Vertical lines 
                                                        x position',
                                                        choices = c('Measured 
                                                                time points',
                                                                'Estimated 
                                                                time points',
                                                                'Custom'),
                                                        selected = 'Measured time 
                                                        points'),
                                    #only show if custom Vlines is selected
                                    conditionalPanel(
                                        condition="$.inArray('Custom', input.vlines) > -1",
                                        textInput('customVlines', 'Add manual 
                                                    vertical lines')
                                    ),
                                    ##only show if vlines is not NULL
                                    conditionalPanel(
                                        condition ="input.vlines.length > 0",
                                        colourInput('vlineCol', 'Vertical lines 
                                                    color',
                                                    value = '#6E6E66'),
                                        checkboxGroupInput('vlab', 'Vertical 
                                                            lines labels',
                                                            choices = c('Measured time points',
                                                                    'Estimated time points',
                                                                    'Custom'),
                                                            selected = 'Measured time points')
                                    ),
                                    #only show if custom vlabs is selected
                                    conditionalPanel(
                                        condition ="$.inArray('Custom', input.vlab) > -1",
                                        textInput('customVlabs',
                                                    'Add manual labels for 
                                                    vertical lines')
                                    ),
                                    ##only show if vlabs is not NULL
                                    conditionalPanel(
                                        condition ="input.vlab.length > 0",
                                        sliderInput('vlabSize', 'Size of the 
                                                    vertical lines labels',
                                                    min = 1, max = 10, 
                                                    value = 5)
                                    ),
                                    checkboxGroupInput('symbols', 'Show symbol underneath',
                                                    choices = c('Measured time points',
                                                                'Estimated time points',
                                                                "Custom")),
                                    #only show if custom symbols is selected
                                    conditionalPanel(
                                        condition ="$.inArray('Custom', input.symbols) > -1",
                                        textInput('customSymbols',
                                                    'Add manual x positions 
                                                    showing symbols'))
                                )
                            ), ##end of dolph menu item
                            
                            ##Plaice plot
                            menuItem(
                                'Plaice Plot', tabName = 'plaice',
                                prettySwitch('plaice', 'Show Plaice Plot',
                                            status = 'primary', slim = TRUE),
                                
                                ##FILTERS ONLY VISIBLE WHEN PLAICE PLOT IS 
                                ##SELECTED
                                conditionalPanel(
                                    condition="input.plaice",
                                    h4('Plaice plot filters'),
                                    prettySwitch('legendPlc', 'Show legend',
                                                status = 'primary',
                                                slim = TRUE),
                                    prettySwitch('sepIndClPlc', 
                                                'Separate independent clones?',
                                                status = 'primary',slim = TRUE),
                                    prettySwitch('axisLabPlc', 'Axis labels',
                                                status = 'primary',slim = TRUE),
                                    prettySwitch('fillClones', 'Mark biallelic 
                                                events',
                                                status = 'primary',slim = TRUE) %>%
                                        helper(
                                            type = "inline",
                                            icon = 'circle-question',
                                            style = "position: absolute; left: 177px;",
                                            title = "How to color biallelic 
                                            events?",
                                            content = c("To show biallelic 
                                            events have happened in a
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
                                    radioGroupButtons('shapePlc', 'Plaice plot 
                                                        shape',
                                                        choices = c('spline',
                                                                    'polygon'),
                                                        justified = TRUE),
                                    colourInput('borderColPlc', 'Clones border 
                                                color',
                                                value = '#000000',  
                                                allowTransparent = TRUE),
                                    checkboxGroupInput('vlinesPlc', 'Vertical 
                                                        lines x position',
                                                        choices = c('Measured time points',
                                                                'Estimated time points',
                                                                'Custom'),
                                                        selected = 'Measured time points'),
                                    #only show if custom Vlines is selected
                                    conditionalPanel(
                                        condition="$.inArray('Custom', input.vlinesPlc) > -1",
                                        textInput('customVlinesPlc', 
                                                    'Add manual vertical lines')
                                    ),
                                    ##only show if vlines is not NULL
                                    conditionalPanel(
                                        condition ="input.vlinesPlc.length > 0",
                                        colourInput('vlineColPlc', 'Vertical 
                                                    lines color',
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
                                                        'Add manual labels for 
                                                        vertical lines')
                                        ),
                                        ##only show if vlabs is not NULL
                                        conditionalPanel(
                                            condition ="input.vlabPlc.length > 0",
                                            sliderInput('vlabSizePlc',
                                                        'Size of the vertical 
                                                        lines labels',
                                                        min = 1, max = 10, 
                                                        value = 5)
                                        )
                                    ),
                                    
                                    checkboxGroupInput('symbolsPlc', 'Show 
                                                        symbol underneath:',
                                                        choices = c('Measured time points',
                                                                'Estimated time points',
                                                                "Custom")),
                                    #only show if custom symbols is selected
                                    conditionalPanel(
                                        condition ="$.inArray('Custom', input.symbolsPlc) > -1",
                                        textInput('customSymbols',
                                                    'Add manual x positions where 
                                                    to show symbols')
                                    )
                                )
                            ) #end of plaice plot menu item
                        ), #end of plots panel
                        
                        ###### Annotations ####
                        menuItem('Annotations', icon = icon('pencil'),
                                prettySwitch('addAnnot', 'Add annotations',
                                                status = 'primary', 
                                                slim = TRUE),
                                menuItemOutput('annot')
                        ), ##end of annotations panel
                        
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                        br(),
                        
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
                    includeCSS(system.file(package="clevRvis", "www", 
                                            "appStyle.css")),
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
                                            h3('seaObject Cancer Cell Fractions 
                                            table'),
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
                                                includeHTML(system.file(package = "clevRvis", 
                                                                        "www" ,
                                                                        "tutorial_text.html")))),
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
                                                includeHTML(system.file(package = "clevRvis", 
                                                                        "www" ,
                                                                        "aboutUs_text.html")))),
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
            
            
        ),
        server=function(input, output, session) {
            vlines <- NULL
            vlabs <- NULL
            symbs <- NULL
            xlab <- NULL
            ylab <- NULL
            annotsTbl <- NULL
            
            ##helpers
            observe_helpers(session = session)
            
            # First Pop Up ###############################
            observeEvent("", {
                showModal(modalDialog(
                    includeHTML(system.file(package = "clevRvis", "www" ,
                                            "intro_text.html")),
                    easyClose = TRUE,
                    footer = tagList(
                        actionButton(inputId = "modalButton",
                                    label = "UNDERSTOOD",
                                    icon = icon("thumbs-up"),
                                    color = "success")
                    )
                ))
            })
            
            observeEvent(input$modalButton,{
                removeModal()
            })
            ######
            
            ##Ask for input table
            output$inTableAsk <- renderText({
                if (!is.null(input$fileCCF)){
                    t <- NULL
                } else {
                    t <- 'Please upload a Cancer Cell Fractions table first 
                    ("Inputs").'
                }
            })
            
            
            ##Save the input table in reactive value
            rawFile <- reactive({
                suppressWarnings({
                    req(input$fileCCF)
                    file <- input$fileCCF
                    ext <- tools::file_ext(file$datapath)
                    
                    if (ext == 'xlsx' | ext == 'xls') {
                        if (input$tpCols){
                            fracTable <-
                                as.matrix(read_excel(file$datapath,
                                                    col_names = TRUE))
                        } else {
                            fracTable <-
                                as.matrix(read_excel(file$datapath,
                                                    col_names = FALSE))
                        }
                        
                    }else if (ext == 'csv'){
                        if (input$tpCols){
                            fracTable <- as.matrix(read_csv(file$datapath))
                        } else {
                            fracTable <-
                                as.matrix(read_csv(file$datapath, 
                                                    col_names = FALSE))
                        }
                        
                    }else {
                        stop(paste('File must be an excel (.xls / .xlsx) or a 
                                    CSV (.csv)',
                                    file$datapath, 'is not a valid file.'))
                    }
                    
                }) #end of suppressWarnings
                
                return(fracTable)
                
            })
            
            #Check input options selected
            output$warnParents <- renderText({
                df <- rawFile()
                suppressWarnings(
                    if(!input$tpCols & sum(df[1,]=='parents', 
                                            na.rm = TRUE) == 1){
                        t <- "Please indicate through the corresponding switch 
                        that the first
            row of the input file contains the numeric time points"
                    } else if(is.na(sum(as.numeric(df[,1]))) & ! input$labRows){
                        t <- "Please indicate throught the corresponding switch 
                        that the first
            column of the input file contains clone labels"
                    } else if(input$parentCol=='column' & (sum(colnames(df)=='parents')==0)){
                        t <- 'Please select "Define parental relations 
                        interactively"'
                    } else if(input$parentCol == 'interactive' &
                            (sum(colnames(df)=='parents')==1)){
                        t <- 'The input file contains the column "parents", to 
                        interactively
            define parental relations, upload a file without the column "parents"'
                    } else { t <- NULL}
                )
                
                
                
                
                
            })
            
            ##Process ccf table
            fracTableR <- reactive({
                suppressWarnings({
                    req(input$fileCCF)
                    fracTable <- rawFile()
                    checkFirstCol <- sum(as.numeric(fracTable[,1]))
                    
                    if(sum(colnames(fracTable)=='parents') == 1){
                        pCol <- which(colnames(fracTable)=='parents')
                        fracTable <- fracTable[,-pCol]
                    }
                    if(sum(fracTable[1,]=='parents', na.rm = TRUE) == 1){
                        pCol <- which(fracTable[1,]=='parents')
                        fracTable <- fracTable[,-pCol]
                        coln <- fracTable[1,]
                        fracTable <- fracTable[2:nrow(fracTable),]
                    }
                    if (is.na(checkFirstCol)){ #input$labRows){
                        rown <- fracTable[,1]
                        coln <- colnames(fracTable)[2:ncol(fracTable)]
                        if(dim(fracTable)[1]==1){
                            fracTable <- dplyr::as_data_frame(matrix(
                                fracTable[,2:ncol(fracTable)], ncol = length(coln)))
                            fracTable <- apply(fracTable, 2, as.numeric)
                            fracTable <- dplyr::as_data_frame(matrix(
                                fracTable, ncol = length(fracTable)))
                            
                        } else {
                            fracTable <- dplyr::as_data_frame(fracTable[,2:ncol(fracTable)])
                            fracTable <- apply(fracTable, 2, as.numeric)
                            fracTable <- round(fracTable, 2)
                            
                        }
                        rownames(fracTable) <- rown
                        colnames(fracTable) <- coln
                    }
                }) #end of suppressWarnings
                
                return(fracTable)
                
            }) #end of fracTable processing
            
            ##Show ccf table OUTPUT
            output$OGccfTable <- DT::renderDataTable({
                suppressWarnings({
                    df <- fracTableR()
                    if (!input$tpCols){
                        tps <- getTimepoints()
                        if (length(tps) < ncol(df)){
                            colnames(df) <- c(NULL,tps)
                        } else {
                            colnames(df) <- tps
                        }
                    }
                    if (!input$labRows){
                        cloneLabs <- getCloneLabs()
                        rownames(df) <- cloneLabs
                    }
                    
                    DT::datatable(round(df,2),
                                extensions = 'Buttons', options = list(
                                    dom = 'Blfrtip',  
                                    buttons = list('copy', 'print', list(
                                        extend = 'collection',
                                        buttons = c('csv', 'excel'),
                                        text = 'Download')),
                                    scrollX=TRUE, scrollCollapse=TRUE))
                }) #end of suppressWarnings
            })
            
            ##Show parent vector as text output
            output$colParents <- renderText({
                df <- rawFile()
                if(input$expTrees | input$parentCol=='interactive' ){
                    p_vec <- getParents()
                } else if(input$parentCol=='column'){ #column of parental 
                    #relations
                    if(sum(colnames(df)=='parents')==1){
                        pCol <- which(colnames(df)=='parents')
                        p_vec <- as.numeric(df[,pCol])
                    } else {p_vec <- NULL}
                }
                p_vec
                
            })
            
            output$outExample <- renderText({
                a <- input$exampleData
                a
            })
            
            ##Create inputs for specifying parental relations
            output$parents <- renderUI({
                df <- fracTableR()
                nclones <- nrow(df)
                cloneLabs <- getCloneLabs()
                
                lapply((seq_len(nclones)), function(i) {
                    list(pickerInput(paste0('p', i), label = cloneLabs[i],
                                    choices = c('Normal cell',cloneLabs)[-(i+1)],
                                    width = 'fit')
                    )
                }) #end of lapply
            }) # end of renderUI
            
            ##Ask for input table
            output$askTblParents <- renderText({
                if (is.null(input$fileCCF) & (input$parentCol=='interactive')){
                    t <- paste('Please upload an input Cancer Cell 
                            Fractions table',
                            'to specify parental relations', sep = '<br/>')
                } else {
                    t <- NULL
                }
            })
            
            ##Get clone labels
            getCloneLabs <- reactive({
                if (input$labRows){ #first col is clone labs
                    cloneLabs <- rownames(fracTableR())
                } else if (input$cloneLabs){ # default clone labs
                    cloneLabs <- paste('Clone:',
                                    as.character(seq_len(nrow(fracTableR()))))
                } else { #not default clone labs
                    cloneLabs <- strsplit(input$cloneLabsValues, ",")[[1]]
                    
                }
                
                return(cloneLabs)
            })
            
            ##Get Timepoints
            getTimepoints <- reactive({
                df <- fracTableR()
                
                if (input$tpCols){ #first row is tps
                    if (colnames(df)[1]=='...1'){
                        tp <- colnames(df)[2:ncol(df)]
                    } else {
                        tp <- colnames(df)
                    }
                    
                } else if (input$dfltTP){ #default tps
                    tp <- seq_len(ncol(df))
                    
                } else { #manually specified tps
                    tp <- as.numeric(strsplit(input$timepoints, ",")[[1]])
                }
                return(as.numeric(tp))
            })
            
            
            
            ##Get parents vector
            getParents <- eventReactive(input$subInputs,{
                df <- rawFile()
                
                if (input$expTrees){ #using explore alternative trees
                    p_vec <- getAltTrees() %>% pluck(input$optParents)
                } else if(input$parentCol=='column'){ #column of parental 
                    #relations
                    if(sum(colnames(df)=='parents')==1){
                        pCol <- which(colnames(df)=='parents')
                        p_vec <- as.numeric(df[,pCol])
                    } else {p_vec <- NULL}
                } else { #manually defined parental relations
                    cloneLabs <- getCloneLabs()
                    p_vec <- c()
                    for (j in seq_len(length(cloneLabs))) {
                        p <- input[[paste0("p",j)]]
                        if (p == 'Normal cell') {
                            p_vec <- c(p_vec,0)
                        } else {
                            p_id <- which(cloneLabs == p)
                            p_vec <- c(p_vec, p_id)
                        }
                    }
                }
                return(p_vec)
            })
            
            ##Get list with alternative parental relations
            getAltTrees <- reactive({
                df <- fracTableR()
                # if (input$parentCol == 'column'){
                #   df <- df[,-which(colnames(df)=="parents")]
                # }
                tps <- getTimepoints()
                x <- exploreTrees(fracTable = df, timepoints = tps)
                if (is.null(x))
                    x <- "No possible trees"
                return(x)
            })
            
            ##Temp check
            # output$checkAltTrees <- renderText({
            #   x <- getAltTrees()
            #   paste('tree', names(x))
            # })
            
            ##Update dropdown (selectInput) explore trees
            observe({
                x <- getAltTrees()
                
                updateSelectInput(session, "optParents",
                                    label = 'Alternative parental relations:',
                                    choices = names(x)
                )
                
            })
            
            ####CHECKS
            # output$checkColNames <- renderText({
            #   df <- fracTableR()
            #   colnames(df)
            # })
            # output$checkParents <- renderText({
            #   parents <- getParents()
            #   paste('parents',parents)
            # })
            
            #get reactive vector with warning messages
            validateInputs <- eventReactive(input$subInputs, {
                fracTable <- fracTableR()
                # if(input$parentCol=='column'){#one of the columns is parental 
                #relations
                #   pCol <- which(colnames(fracTable)=='parents')
                #   fracTable <- fracTable[,-pCol]
                # }
                parents <- getParents()
                cloneLabels <- getCloneLabs()
                warns <- c()
                clones <- seq_len(dim(fracTable)[1])
                timepts <- seq_len(dim(fracTable)[2])
                given_tps <- getTimepoints()
                
                if(is.null(parents)){
                    warns <- c(warns, 'Please define parental relations 
                    correctly.
                    No parental relations were defined.')
                }
                
                ##Defined timepoints must be numeric values
                if(sum(is.na(as.numeric(given_tps)))>0){
                    warns <- c(warns,'At least one of the timepoints defined is 
                    not a
                    numeric value. ALL TIMEPOINTS MUST BE NUMERIC VALUES.')
                }
                
                ##no cluster can go from present to absent and then back
                ##difference in clusters cannot go from present to absent and 
                ##then back
                for(clone in clones){
                    startedClone <- FALSE
                    endedClone <- FALSE
                    startedDiff <- FALSE
                    endedDiff <- FALSE
                    
                    for(timept in timepts){
                        ##check individual clones
                        if(fracTable[clone,timept] > 0){
                            if(startedClone & endedClone){
                                warns <- c(warns,paste("Clone",clone,"goes from 
                                present
                        to absent (fraction=0) and then back to present."))
                            }
                            startedClone <- TRUE
                        } else {
                            if(startedClone){
                                endedClone <- TRUE
                            }
                        }
                        
                        ##check difference in clusters
                        diffr <- fracTable[clone,timept] -
                            sum(fracTable[which(parents==clone),timept])
                        if( diffr > 0){
                            if(startedDiff & endedDiff){
                                warns <- c(warns,paste("The difference between",
                                                        clone,
                                                        "and its subclones goes 
                                                        from present to absent
                    (difference=0) and then back to present. Subclones can't
                    have the same CCF as the parent (difference = 0) and then
                    have less CCF again."))
                            }
                            startedDiff <- TRUE
                        } else {
                            if(startedDiff){
                                endedDiff <- TRUE
                            }
                        }
                    }
                }
                
                ##clusters of entirely zero get a warning
                if(length(which(rowSums(fracTable) == 0)) > 0){
                    warns <- c(warns, "At least one cluster has fraction zero 
                    at all
            timepoints. It will not be displayed")
                }
                
                ##each timepoint doesn't sum to more than the parental value
                #at a given nest level (or 100% for level 0)
                for(timept in timepts){
                    neighbors <- which(parents == 0)
                    if(sum(fracTable[neighbors,timept]) > 100){
                        warns <- c(warns, paste("clones with same nest level 
                        cannot
                have values that sum to more than 100%: Problem is in
                clusters ", paste(neighbors,collapse=",")))
                    }
                    
                    for(i in unique(parents)){
                        if(i > 0){
                            neighbors <- which(parents==i)
                            if(sum(fracTable[neighbors,timept]) >
                                fracTable[parents[neighbors[1]], timept]){
                                warns <- c(warns, paste("clones with same 
                                parent cannot
                        have values that sum to more than the percentage of the
                        parent: Problem is in clusters ",
                                                        paste(neighbors,
                                                            collapse=","),
                                                        "at timepoint",
                                                        timept))
                                
                            }
                        }
                    }
                }
                
                ##the number of clone labels is equal to the number of clones
                if(length(cloneLabels) != nrow(fracTable)){
                    warns <- c(warns, paste("number of cloneLabels provided must
            be equal to the number of clones"))
                }
                return(unique(warns))
            })
            
            ##Success / warning messages
            observeEvent(input$subInputs,{
                warns <- validateInputs()
                if (length(warns) == 0){
                    sendSweetAlert(
                        session = session,
                        title = "Success!!",
                        text = "Parental relations have been successfully 
                        defined",
                        type = "success"
                    )
                } else {
                    sendSweetAlert(
                        session = session,
                        title = "WARNING...",
                        text = warns,
                        type = "error"
                    )
                }
            })
            
            ##Update default original timepoint (manual choice)
            observeEvent(input$subInputs,{
                timepoints <- getTimepoints()
                if (length(timepoints)==1){
                    val <- -30
                } else {
                    val <- timepoints[1] - (timepoints[2] - timepoints[1])
                }
                updateNumericInput(session = session,
                                    inputId = 'ogTP',
                                    max = min(timepoints)-1,
                                    value = val)
            })
            
            ##Update slider to select measured timepoints for therapy estimation
            observeEvent(input$subInputs,{
                timepoints <- getTimepoints()
                updateSliderTextInput(session = session,
                                    inputId = 'MtpThp',
                                    choices = timepoints,
                                    selected = c(timepoints[1],timepoints[2]))
            })
            
            ##Update slider to select specific timepoint for therapy estimation
            observeEvent(input$subInputs,{
                timepoints <- getTimepoints()
                updateSliderInput(session = session, inputId = 'tpThp',
                                    min = min(timepoints),
                                    max = max(timepoints),
                                    value = 0,
                                    step = 0.5)
            })
            
            ##Ask for submit seaObject (click button pls)
            observeEvent("", {
                output$inInputsAsk <- renderText({
                    t <- 'Please upload a Cance Cell Fractions table first 
                    ("Inputs" ->
        "Submit inputs"). Then, configure the seaObject ("seaObject options" ->
        "Submit") to visualize the seaObject Cancer Cell Fractions table.'
                })
            })
            ##Remove text asking for submit seaObject
            observeEvent(input$submitSea,{
                output$inInputsAsk <- renderText({
                    t <- NULL
                })
            })
            
            ##Validate sea Object
            validateSeaObjectFilters <- eventReactive(input$submitSea, {
                
                tps <- getTimepoints()
                warns <- c()
                if(input$therapyEffect){
                    if (input$thpEf_opt == 1){ ##thp effect is specified 
                        #timepoint
                        if (input$tpThp %in% tps){
                            warns <- c(warns, 'Specific timepoint defined for 
                            therapy
                    effect can not be a measured timepoint')
                        }
                    } else { ##thp eff defined as two measured tps
                        therapyEffectMtps <- as.numeric(input$MtpThp)
                        if (which(tps==therapyEffectMtps[2]) -
                            which(tps==therapyEffectMtps[1])>1){
                            warns <- c(warns, 'Measured Timepoints selected for 
                            therapy
                            effect must be contiguous, no other measured
                            timepoint must be in between.')
                        } else if (which(tps==therapyEffectMtps[2]) -
                                    which(tps==therapyEffectMtps[1])==0){
                            warns <- c(warns, 'Measured Timepoints selected for 
                            therapy
                            effect can not be the same one. To select a specific
                            timepoint click on "Specific Timepoint" when choosing
                            the type of Therapy Effect Estimation.')
                        }
                    }
                }
                
                if(length(tps)==1 & !input$tpEstim){
                    warns <- c(warns, 'To visualize clonal evolution from single
            timepoint extra timepoints estimation must be selected.')
                }
                if (length(tps)==1 & !input$customOgTP){
                    warns <- c(warns, "To visualize clonal evolution from a 
                    single
            timepoint a timepoint of origin must be manually specified.")
                }
                return(warns)
            })
            
            ##check inputs are there (and correct) before clicking submit sea 
            ##object
            observeEvent(input$submitSea,{
                err <- validateSeaObjectFilters()
                if (!input$subInputs){
                    sendSweetAlert(
                        session = session,
                        title = "ERROR!",
                        text = 'Submit inputs before clicking submit on 
                        seaObject filters',
                        type = "error"
                    )
                } else if (length(err)>0){
                    sendSweetAlert(
                        session = session,
                        title = "ERROR!",
                        text = err,
                        type = "error"
                    )
                } else {
                    sendSweetAlert(
                        session = session,
                        title = "SUCCESS!!",
                        text = 'All in order',
                        type = "success"
                    )
                }
            })
            
            #reactive object SeaObject will update every time we click submit
            seaObjectR <- eventReactive(input$submitSea,{
                err <- validateSeaObjectFilters()
                fracTable <- fracTableR()
                timepoints <- getTimepoints()
                cloneLabels <- getCloneLabs()
                parents <- getParents()
                if(length(err)==0){
                    if(input$therapyEffect){
                        if (input$thpEf_opt == 1){
                            therapyEffect <- input$tpThp
                        } else if (input$thpEf_opt == 2){
                            therapyEffect <- as.numeric(input$MtpThp)
                        }
                    } else{
                        therapyEffect <- NULL}
                    
                    if(input$customOgTP){
                        ogTimepoint <- input$ogTP
                    } else{
                        ogTimepoint <- NULL
                    }
                    
                    
                    seaObject <- createSeaObject(fracTable = fracTable,
                                                timepoints = timepoints,
                                                parents = parents,
                                                timepointInterpolation = input$tpEstim,
                                                therapyEffect = therapyEffect,
                                                cloneLabels = cloneLabels,
                                                originTimepoint = ogTimepoint)
                }
                
                
                return(seaObject)
                
            }) #end of create SeaObject
            
            
            ##TABLE WITH CCF AFTER SEAOBJECT IS CREATED
            observeEvent(input$submitSea,{
                output$ccfTable <- DT::renderDataTable({
                    seaObject <- seaObjectR()
                    df <- dplyr::as_data_frame(seaObject@fracTable)
                    parents <- isolate(getParents())
                    if(input$ccfVSprop == "prop"){
                        df_new <- df
                        for (i in seq_len(ncol(df))){
                            for (j in seq_len(nrow(df))){
                                df_new[j,i] <- df[j,i] - sum(df[which(parents==j),i])
                            }
                        }
                        df <- df_new
                    }
                    suppressWarnings({
                        df <- df %>% add_column(parents)
                        cloneLabs <- getCloneLabs()
                        rownames(df) <- cloneLabs
                        DT::datatable(df, 
                                    extensions = 'Buttons', options = list(
                            scrollX=TRUE, scrollCollapse=TRUE,
                            dom = 'Blfrtip',  
                            buttons = list('copy', 'print', list(
                                extend = 'collection',
                                buttons = c('csv', 'excel'),
                                text = 'Download')
                            ))
                        )
                    }) #end of suppressWarnings
                })
            })
            
            ####PLOTS####
            
            ##Ask to activate the switches to visualize the plots (shark 
            ##dolphin)
            output$inShkDphAsk <- renderText({
                if(!input$shark & !input$dolphin){
                    t <- 'Please configure the seaObject first
        ("seaObject options" -> "Submit"). Then, select the plots to visualize
        ("Plots" -> switches for Shark, Dolphin and Plaice Plots).'
                } else {t <- NULL}
            })
            
            
            ##INTERACTIVE PLOTS
            output$intPlot <- renderGirafe({
                req(input$shark | input$dolphin )
                seaObject <- seaObjectR()
                timepoints <- getTimepoints()
                if(input$dolphin){
                    ##Vertical Lines and Labels
                    vlines <- c()
                    vlabs <- c()
                    symbs <- c()
                    if('Measured time points' %in% input$vlines){
                        vlines <- c(vlines,timepoints)
                        if('Measured time points' %in% input$vlab){
                            vlabs <- c(vlabs, timepoints)
                        }
                    }
                    if('Measured time points' %in% input$symbols){
                        symbs <- c(symbs, timepoints)
                    }
                    if('Estimated time points' %in% input$vlines){
                        estimatedTimepoints <-
                            seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                        vlines <- c(vlines, estimatedTimepoints)
                        if('Estimated time points' %in% input$vlab){
                            vlabs <- c(vlabs, estimatedTimepoints)
                        }
                    }
                    if('Estimated time points' %in% input$symbols){
                        estimatedTimepoints <-
                            seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                        symbs <- c(symbs, estimatedTimepoints)
                    }
                    if('Custom' %in% input$vlines){
                        customVlines <-
                            as.numeric(strsplit(input$customVlines, ",")[[1]])
                        vlines <- c(vlines,customVlines)
                    }
                    if('Custom' %in% input$vlab){
                        customVlabs <- strsplit(input$customVlabs, ",")[[1]]
                        vlabs <- c(vlabs, customVlabs)
                    }
                    if('Custom' %in% input$symbols){
                        customSymbs <-
                            as.numeric(strsplit(input$customSymbols, ",")[[1]])
                        symbs <- c(symbs,customSymbs)
                    }
                    if(input$axisLab){
                        if(length(input$xlab)>0){
                            xlab <- input$xlab
                        }else{ xlab <- NULL}
                        if(length(input$ylab)>0){
                            ylab <- input$ylab
                        }else{ ylab <- NULL}
                    }else{
                        xlab <- NULL
                        ylab <- NULL
                    }
                }
                if(input$legendDol | input$legendShk){
                    legend <- TRUE
                }else {legend <- FALSE}
                
                if (input$addAnnot){
                    annotsTbl <- getAnnots()
                } else {annotsTbl <- NULL}
                
                if (input$extendedShk & !input$dolphin){
                    tps <- c()
                    if('Measured time points' %in% input$showTps){
                        tps <- c(tps,timepoints)
                    }
                    if('Estimated time points' %in% input$showTps){
                        estimatedTimepoints <-
                            seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                        tps <- c(tps, estimatedTimepoints)
                    }
                    if('Custom' %in% input$showTps){
                        customTps <- as.numeric(strsplit(input$customShowTps, ",")[[1]])
                        tps <- c(tps,customTps)
                    }
                    suppressWarnings(extSharkPlot(seaObject,
                                                showLegend = input$legendShk,
                                                main = input$mainShk,
                                                timepoints = tps),
                                    classes = 'warnings')
                } else {
                    suppressWarnings({
                        try(
                            combinedPlot(seaObject, dolphin = input$dolphin,
                                        shark = input$shark, vlines = vlines,
                                        mainShk = input$mainShk, 
                                        showLegend = legend,
                                        shape = input$shape, 
                                        borderCol = input$borderCol,
                                        vlineCol=input$vlineCol, vlab=vlabs,
                                        markMeasuredTimepoints = symbs, 
                                        pos = input$pos,
                                        separateIndependentClones = input$sepIndCl,
                                        mainDph = input$mainDol, 
                                        vlabSize = input$vlabSize,
                                        xlab = xlab, ylab = ylab, 
                                        annotations = annotsTbl,
                                        pad.left = input$pad)
                        )
                    })
                }
            })
            
            ##Create inputs for specifying clones to fill in plaiceplot
            output$fillVec <- renderUI({
                cloneLabs <- getCloneLabs()
                nclones <- length(cloneLabs)
                
                lapply((seq_len(nclones)), function(i) {
                    list(pickerInput(paste0('color', i), label = cloneLabs[i],
                                    choices = c('Empty',cloneLabs),
                                    width = 'fit')
                    )
                }) #end of lapply
            }) # end of renderUI
            
            ##Get clones to fill vector
            getClonesToFill <- reactive({
                req(input$fillClones)
                cloneLabs <- getCloneLabs()
                f_vec <- c()
                for (j in seq_len(length(cloneLabs))) {
                    c <- input[[paste0("color",j)]]
                    if (c == 'Empty') {
                        f_vec <- c(f_vec,0)
                    } else {
                        f_vec <- c(f_vec,which(cloneLabs == c))
                    }
                }
                return(f_vec)
            })
            
            #Ask to activate the plaiceplot switch to visualize the plot
            output$inPlaiceAsk <- renderText({
                if(!input$plaice){
                    t <- 'Please configure the seaObject first
        ("seaObject options" -> "Submit"). Then, select the plots to visualize
        ("Plots" -> switches for Shark, Dolphin and Plaice Plots).'
                } else {t <- NULL}
            })
            
            ##PLAICEPLOT
            output$plaicePlot <- renderGirafe({
                req(input$plaice)
                seaObject <- seaObjectR()
                timepoints <- getTimepoints()
                estimatedTimepoints <-
                    seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                
                ##vector clones to fill
                if (input$fillClones){
                    fillClones <- getClonesToFill()
                } else {
                    fillClones <-rep(0,length(seaObject@col))
                }
                ##annotations
                if (input$addAnnot){
                    annotsTbl <- getAnnots()
                } else {annotsTbl <- NULL}
                
                ##Vertical Lines, Labels and symbols
                vlines <- c()
                vlabs <- c()
                symbs <- c()
                if('Measured time points' %in% input$vlinesPlc){
                    vlines <- c(vlines,timepoints)
                }
                if('Measured time points' %in% input$vlabPlc){
                    vlabs <- c(vlabs, timepoints)
                }
                if('Measured time points' %in% input$symbolsPlc){
                    symbs <- c(symbs, timepoints)
                }
                if('Estimated time points' %in% input$vlinesPlc){
                    vlines <- c(vlines, estimatedTimepoints)
                }
                if('Estimated time points' %in% input$vlabPlc){
                    vlabs <- c(vlabs, estimatedTimepoints)
                }
                if('Estimated time points' %in% input$symbolsPlc){
                    symbs <- c(symbs, estimatedTimepoints)
                }
                if('Custom' %in% input$vlinesPlc){
                    customVlines <-
                        as.numeric(strsplit(input$customVlinesPlc, ",")[[1]])
                    vlines <- c(vlines,customVlines)
                }
                if('Custom' %in% input$vlabPlc){
                    customVlabs <- strsplit(input$customVlabsPlc, ",")[[1]]
                    vlabs <- c(vlabs, customVlabs)
                }
                if('Custom' %in% input$symbolsPlc){
                    customSymbs <-
                        as.numeric(strsplit(input$customSymbolsPlc, ",")[[1]])
                    symbs <- c(symbs,customSymbs)
                }
                suppressWarnings(
                    plaicePlot(seaObject, clonesToFill = fillClones,
                            shape = input$shapePlc,
                            borderCol = input$borderColPlc, vlines=vlines,
                            vlineCol = input$vlineColPlc, vlab=vlabs,
                            vlabSize = input$vlabSizePlc, 
                            annotations=annotsTbl,
                            separateIndependentClones = input$sepIndClPlc,
                            showLegend = input$legendPlc,
                            markMeasuredTimepoints = symbs,
                            main = input$mainPlc, ylab = input$axisLabPlc),
                    classes = 'warnings')
            })
            
            
            
            ##Create inputs for annotations
            output$annot <- renderMenu({
                req(input$submitSea & req(input$addAnnot))
                tps <- seaObjectR()@timepoints
                cloneLabs <- getCloneLabs()
                nclones <- length(cloneLabs)
                sidebarMenu(
                    lapply((seq_len(nclones)), function(i) {
                        list(menuItem(paste(cloneLabs[i]),
                                    ##Annotations text
                                    textInput(paste0('annot',i),
                                                paste('Annotations',
                                                    cloneLabs[i])),
                                    ##Color
                                    switchInput(paste0("col",i), "Text color",
                                                labelWidth = "80px",
                                                onLabel = 'White', 
                                                offLabel = 'Black',
                                                onStatus = 'default'
                                    ),
                                    ##X position
                                    sliderInput(paste0('x', i), 
                                                label = 'X position',
                                                max = round(max(tps),0)+((max(tps)-min(tps))/2),
                                                min = round(min(tps),0)-((max(tps)-min(tps))/2),
                                                value = 0, step=(max(tps)-min(tps))*0.05
                                    ),
                                    ##Y position
                                    sliderInput(paste0('y', i), 
                                                label = 'Y position',
                                                max = 110,
                                                min = -110,
                                                value = 50, step=1)
                                
                        )
                        
                        )
                    }) #end of lapply
                )#end of sidebarMenu
            }) # end of renderMenu
            
            ##Generate the annotations table
            getAnnots <- reactive({
                
                cloneLabs <- getCloneLabs()
                x_vec <- c()
                y_vec <- c()
                annot_vec <- c()
                col_vec <- c()
                for (j in seq_len(length(cloneLabs))) {
                    x_vec <- c(x_vec, input[[paste0("x",j)]])
                    y_vec <- c(y_vec, input[[paste0("y",j)]])
                    annot_vec <- c(annot_vec, input[[paste0("annot",j)]])
                    if (input[[paste0('col',j)]]){
                        col_vec <- c(col_vec, 'White')
                    } else {
                        col_vec <- c(col_vec, 'Black')
                        
                    }
                    #col_vec <- c(col_vec, input[[paste0('col',j)]])
                    
                }
                annot_df <- data.frame(x = x_vec, y = y_vec, lab = annot_vec,
                                        col = col_vec)
                
                return(annot_df)
            })
            
            
            session$onSessionEnded(function() {
                stopApp()
            })
        }
        
    )
}