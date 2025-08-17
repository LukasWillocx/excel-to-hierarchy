library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(htmlwidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Procedural hierarchy"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Configure", tabName = "configure", icon = icon("upload")),
      menuItem("Interactive Tree", tabName = "tree", icon = icon("sitemap"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #ffffff;
        }
        .tree-tile {
          background: linear-gradient(135deg, #EE550B 0%, #E20014 100%);
          color: white;
          border-radius: 12px;
          padding: 20px;
          margin: 8px;
          cursor: pointer;
          transition: all 0.3s ease;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          min-height: 80px;
          display: flex;
          align-items: center;
          justify-content: center;
          text-align: center;
          font-weight: 600;
        }
        .tree-tile:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.2);
          background: linear-gradient(135deg, #E20014 0%, #EE550B 100%);
        }
        .breadcrumb-tile {
          background: linear-gradient(135deg, #FBB202 0%, #EE550B 100%);
          color: white;
          border-radius: 8px;
          padding: 10px 15px;
          margin: 4px;
          cursor: pointer;
          transition: all 0.3s ease;
          display: inline-block;
          font-weight: 500;
        }
        .breadcrumb-tile:hover {
          transform: scale(1.05);
          box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        }
        .attribute-card {
          background: white;
          border-radius: 12px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          border-left: 5px solid #EE550B;
        }
        .attribute-row {
          margin-bottom: 12px;
          padding: 8px 0;
          border-bottom: 1px solid #f0f0f0;
        }
        .attribute-row:last-child {
          border-bottom: none;
        }
        .level-header {
          background: linear-gradient(135deg, #EE550B 0%, #E20014 100%);
          color: white;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 20px;
          text-align: center;
          font-size: 18px;
          font-weight: 600;
        }
        .config-box {
          background: white;
          border-radius: 12px;
          padding: 20px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .grid-container {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
          gap: 15px;
          margin-top: 20px;
        }
      "))
    ),
    
    tabItems(
      # Configuration Tab
      tabItem(tabName = "configure",
              fluidRow(
                box(
                  title = "Upload Excel File", status = "primary", solidHeader = TRUE,
                  width = 12, class = "config-box",
                  fileInput("excel_file", "", accept = c(".xlsx"),
                            buttonLabel = "Browse...", placeholder = "No file selected"),
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    br(),
                    div(class = "alert alert-success", 
                        icon("check-circle"), " File uploaded successfully!")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.file_uploaded",
                fluidRow(
                  box(
                    title = "Define Hierarchy Levels", status = "info", solidHeader = TRUE,
                    width = 8, class = "config-box",
                    div(class = "level-header", "Configure Decision Tree Hierarchy"),
                    uiOutput("hierarchy_ui")
                  ),
                  box(
                    title = "Export Options", status = "success", solidHeader = TRUE,
                    width = 4, class = "config-box",
                    textInput("output_filename", "JSON Filename", value = "decision_tree.json"),
                    br(),
                    downloadButton("download_json", "Download JSON", 
                                   class = "btn-success btn-lg", 
                                   style = "width: 100%; margin-bottom: 10px;"),
                    br(),
                    downloadButton("download_html", "Download Interactive HTML", 
                                   class = "btn-info btn-lg", 
                                   style = "width: 100%;")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Data Structure Overview", status = "warning", solidHeader = TRUE,
                    width = 12, class = "config-box",
                    verbatimTextOutput("structure_overview")
                  )
                )
              )
      ),
      
      # Interactive Tree Tab
      tabItem(tabName = "tree",
              conditionalPanel(
                condition = "!output.file_uploaded",
                div(class = "level-header", 
                    style = "background: linear-gradient(135deg, #E20014 0%, #EE550B 100%);",
                    "Please upload and configure your Excel file first")
              ),
              
              conditionalPanel(
                condition = "output.file_uploaded",
                fluidRow(
                  box(
                    title = NULL, status = "primary", solidHeader = FALSE,
                    width = 12,
                    div(class = "level-header", "Navigate Your Decision Tree"),
                    uiOutput("breadcrumb_tiles"),
                    br(),
                    uiOutput("tree_view_enhanced")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Check if file is uploaded
  output$file_uploaded <- reactive({
    return(!is.null(input$excel_file))
  })
  outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)
  
  # Load and clean data
  uploaded_data <- reactive({
    req(input$excel_file)
    df <- read_excel(input$excel_file$datapath, sheet = 1, guess_max = 10000)
    
    # Convert all columns to character and handle empty/NA values
    df <- df %>% 
      mutate(across(everything(), as.character)) %>%
      mutate(across(everything(), ~ ifelse(. == "" | is.na(.), NA_character_, .)))
    
    df
  })
  
  # Dynamic UI for hierarchy selection
  output$hierarchy_ui <- renderUI({
    req(uploaded_data())
    cols <- names(uploaded_data())
    
    # Provide defaults if possible
    default_lv1 <- if(length(cols) >= 1) cols[1] else NULL
    default_lv2 <- if(length(cols) >= 2) cols[2] else NULL
    default_lv3 <- if(length(cols) >= 3) cols[3] else NULL
    default_lv4 <- if(length(cols) >= 4) cols[4] else NULL
    default_lv5 <- if(length(cols) >= 5) cols[5] else NULL
    default_lv6 <- if(length(cols) >= 6) cols[6] else NULL
    
    tagList(
      fluidRow(
        column(6, selectInput("level1", "Level 1 (Root)", choices = cols, selected = default_lv1)),
        column(6, selectInput("level2", "Level 2", choices = cols, selected = default_lv2))
      ),
      fluidRow(
        column(6, selectInput("level3", "Level 3", choices = cols, selected = default_lv3)),
        column(6, selectInput("level4", "Level 4", choices = cols, selected = default_lv4))
      ),
      fluidRow(
        column(6, selectInput("level5", "Level 5 (Optional)", choices = c('none', cols), selected = default_lv5)),
        column(6, selectInput("level6", "Level 6 (Optional)", choices = c('none', cols), selected = default_lv6))
      )
    )
  })
  
  # Get the selected hierarchy levels as a character vector
  hierarchy_levels <- reactive({
    req(input$level1, input$level2, input$level3, input$level4)
    levels <- c(input$level1, input$level2, input$level3, input$level4)
    if (!is.null(input$level5) && input$level5 != "none") levels <- c(levels, input$level5)
    if (!is.null(input$level6) && input$level6 != "none") levels <- c(levels, input$level6)
    unique(levels)
  })
  
  # Attribute columns are those NOT in hierarchy
  attribute_cols <- reactive({
    req(uploaded_data(), hierarchy_levels())
    setdiff(names(uploaded_data()), hierarchy_levels())
  })
  
  # Improved recursive function to create nested list
  create_nested_list <- function(data, levels, attrs, current_level = 1) {
    # Base case: no more levels to process
    if (current_level > length(levels)) {
      if (nrow(data) == 0) return(NULL)
      
      create_attribute_list <- function(row_data) {
        attr_list <- as.list(row_data[attrs])
        lapply(attr_list, function(x) ifelse(is.na(x), "N/A", as.character(x)))
      }
      
      if (nrow(data) == 1) {
        return(create_attribute_list(data[1, , drop = FALSE]))
      } else {
        return(lapply(seq_len(nrow(data)), function(i) {
          create_attribute_list(data[i, , drop = FALSE])
        }))
      }
    }
    
    if (nrow(data) == 0) return(NULL)
    
    current_var <- levels[current_level]
    
    if (!(current_var %in% colnames(data))) {
      return(create_nested_list(data, levels, attrs, current_level + 1))
    }
    
    unique_vals <- unique(data[[current_var]])
    unique_vals <- unique_vals[!is.na(unique_vals)]
    
    if (length(unique_vals) == 0) {
      return(create_nested_list(data, levels, attrs, current_level + 1))
    }
    
    result <- list()
    
    for (val in unique_vals) {
      subset_data <- data[!is.na(data[[current_var]]) & data[[current_var]] == val, , drop = FALSE]
      
      if (nrow(subset_data) > 0) {
        next_level_has_data <- FALSE
        if (current_level < length(levels)) {
          for (next_level_idx in (current_level + 1):length(levels)) {
            next_var <- levels[next_level_idx]
            if (next_var %in% colnames(subset_data)) {
              next_unique_vals <- unique(subset_data[[next_var]])
              next_unique_vals <- next_unique_vals[!is.na(next_unique_vals)]
              if (length(next_unique_vals) > 0) {
                next_level_has_data <- TRUE
                break
              }
            }
          }
        }
        
        if (next_level_has_data) {
          result[[val]] <- create_nested_list(subset_data, levels, attrs, current_level + 1)
        } else {
          result[[val]] <- create_nested_list(subset_data, levels, attrs, length(levels) + 1)
        }
      }
    }
    
    na_rows <- data[is.na(data[[current_var]]), , drop = FALSE]
    if (nrow(na_rows) > 0) {
      na_has_next_level_data <- FALSE
      if (current_level < length(levels)) {
        for (next_level_idx in (current_level + 1):length(levels)) {
          next_var <- levels[next_level_idx]
          if (next_var %in% colnames(na_rows)) {
            next_unique_vals <- unique(na_rows[[next_var]])
            next_unique_vals <- next_unique_vals[!is.na(next_unique_vals)]
            if (length(next_unique_vals) > 0) {
              na_has_next_level_data <- TRUE
              break
            }
          }
        }
      }
      
      if (na_has_next_level_data) {
        na_result <- create_nested_list(na_rows, levels, attrs, current_level + 1)
        if (!is.null(na_result)) {
          if (is.list(na_result) && !is.null(names(na_result))) {
            for (na_name in names(na_result)) {
              if (!(na_name %in% names(result))) {
                result[[na_name]] <- na_result[[na_name]]
              }
            }
          }
        }
      } else {
        if (length(result) == 0) {
          result <- create_nested_list(na_rows, levels, attrs, length(levels) + 1)
        }
      }
    }
    
    return(if(length(result) == 0) NULL else result)
  }
  
  # Process uploaded data into nested list
  nested_list <- reactive({
    req(uploaded_data(), hierarchy_levels())
    df <- uploaded_data()
    levels <- hierarchy_levels()
    attrs <- attribute_cols()
    
    for (level in levels) {
      if (!(level %in% colnames(df))) {
        df[[level]] <- NA_character_
      }
    }
    
    create_nested_list(df, levels, attrs)
  })
  
  # JSON output
  processed_data <- reactive({
    jsonlite::toJSON(nested_list(), pretty = TRUE, auto_unbox = TRUE)
  })
  
  output$structure_overview <- renderText({
    req(uploaded_data(), hierarchy_levels())
    df <- uploaded_data()
    levels <- hierarchy_levels()
    attrs <- attribute_cols()
    
    txt <- paste0("Hierarchy levels: ", paste(levels, collapse = " → "), "\n\n")
    txt <- paste0(txt, "Attributes included: ", if(length(attrs) > 0) paste(attrs, collapse = ", ") else "None", "\n\n")
    
    for (i in seq_along(levels)) {
      level <- levels[i]
      if (level %in% colnames(df)) {
        unique_vals <- unique(df[[level]])
        unique_vals <- unique_vals[!is.na(unique_vals)]
        non_na_count <- sum(!is.na(df[[level]]))
        total_count <- nrow(df)
        sample_vals <- paste(utils::head(unique_vals, 5), collapse = ", ")
        if (length(unique_vals) > 5) sample_vals <- paste0(sample_vals, "...")
        txt <- paste0(txt, sprintf("Level %d (%s): %d unique values, %d/%d rows have data\n   Values: %s\n\n",
                                   i, level, length(unique_vals), non_na_count, total_count, sample_vals))
      } else {
        txt <- paste0(txt, sprintf("Level %d (%s): Column will be created as NA\n\n", i, level))
      }
    }
    txt
  })
  
  # Navigation path tracker
  nav_path <- reactiveVal(character(0))
  
  # Current level data in nested list based on nav_path
  current_level_data <- reactive({
    req(nested_list())
    data <- nested_list()
    path <- nav_path()
    for (step in path) {
      if (is.list(data) && step %in% names(data)) {
        data <- data[[step]]
      } else {
        break
      }
    }
    data
  })
  
  # Enhanced breadcrumb tiles
  output$breadcrumb_tiles <- renderUI({
    path <- nav_path()
    
    if (length(path) == 0) {
      div(
        class = "breadcrumb-tile",
        onclick = "return false;",
        icon("home"), " Root Level"
      )
    } else {
      tagList(
        div(
          class = "breadcrumb-tile",
          onclick = "Shiny.setInputValue('nav_click', '', {priority: 'event'}); return false;",
          icon("home"), " Root"
        ),
        lapply(seq_along(path), function(i) {
          name <- path[i]
          div(
            class = "breadcrumb-tile",
            onclick = sprintf("Shiny.setInputValue('breadcrumb_click', %d, {priority: 'event'}); return false;", i),
            icon("chevron-right"), " ", name
          )
        })
      )
    }
  })
  
  # Enhanced tree view with tiles
  output$tree_view_enhanced <- renderUI({
    data <- current_level_data()
    
    if (is.list(data)) {
      is_leaf <- function(x) {
        if (!is.list(x)) return(TRUE)
        all(!sapply(x, is.list))
      }
      
      if (is_leaf(data)) {
        # Display attributes in cards
        if (all(sapply(data, is.list))) {
          # Multiple leaf attribute rows
          tagList(
            div(class = "level-header", 
                style = "background: linear-gradient(135deg, #FBB202 0%, #EE550B 100%);",
                icon("info-circle"), " Decision Outcome (Multiple Records)"),
            lapply(seq_along(data), function(i) {
              attrs <- data[[i]]
              div(
                class = "attribute-card",
                tags$h5(paste("Record", i), style = "color: #EE550B; margin-bottom: 15px;"),
                lapply(names(attrs), function(nm) {
                  div(
                    class = "attribute-row",
                    tags$strong(paste0(nm, ": "), style = "color: #495057;"),
                    span(attrs[[nm]], style = "color: #6c757d;")
                  )
                })
              )
            })
          )
        } else {
          # Single row attributes
          tagList(
            div(class = "level-header", 
                style = "background: linear-gradient(135deg, #FBB202 0%, #EE550B 100%);",
                icon("check-circle"), " Decision Outcome"),
            div(
              class = "attribute-card",
              lapply(names(data), function(attr) {
                div(
                  class = "attribute-row",
                  tags$strong(paste0(attr, ": "), style = "color: #495057;"),
                  span(ifelse(is.na(data[[attr]]) || data[[attr]] == "N/A", "N/A", data[[attr]]), 
                       style = "color: #6c757d;")
                )
              })
            )
          )
        }
      } else {
        # Non-leaf: display clickable tiles for next level
        items <- names(data)
        if (is.null(items)) items <- seq_along(data)
        
        tagList(
          div(class = "level-header", 
              icon("sitemap"), " Choose Next Step (", length(items), " options)"),
          div(
            class = "grid-container",
            lapply(items, function(item) {
              div(
                class = "tree-tile",
                onclick = sprintf("Shiny.setInputValue('nav_click', '%s', {priority: 'event'});", item),
                icon("arrow-right"), " ", item
              )
            })
          )
        )
      }
    } else {
      div(class = "level-header", 
          style = "background: linear-gradient(135deg, #E20014 0%, #EE550B 100%);",
          "No data available at this level")
    }
  })
  
  # Handle nav click (going deeper)
  observeEvent(input$nav_click, {
    if (input$nav_click == "") {
      nav_path(character(0))
    } else {
      nav_path(c(nav_path(), input$nav_click))
    }
  })
  
  # Handle breadcrumb click (jump back to any level)
  observeEvent(input$breadcrumb_click, {
    idx <- as.numeric(input$breadcrumb_click)
    path <- nav_path()
    if (!is.na(idx) && idx >= 1 && idx <= length(path)) {
      nav_path(path[1:idx])
    }
  })
  
  # Generate standalone HTML
  generate_standalone_html <- function() {
    nested_data <- nested_list()
    hierarchy <- hierarchy_levels()
    
    html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Procedural decision tree CT</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            background: linear-gradient(135deg, #EE550B 0%, #E20014 100%);
            margin: 0;
            padding: 20px;
            min-height: 100vh;
            position: relative;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            border-radius: 20px;
            padding: 30px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.1);
        }
        .header {
            text-align: center;
            margin-bottom: 30px;
            padding: 20px;
            background: linear-gradient(135deg, #EE550B 0%, #E20014 100%);
            color: white;
            border-radius: 15px;
        }
        .breadcrumb {
            margin-bottom: 20px;
            display: flex;
            flex-wrap: wrap;
            align-items: center;
        }
        .breadcrumb-item {
            background: linear-gradient(135deg, #FBB202 0%, #EE550B 100%);
            color: white;
            padding: 8px 15px;
            border-radius: 20px;
            margin: 4px;
            cursor: pointer;
            transition: all 0.3s ease;
            font-weight: 500;
        }
        .breadcrumb-item:hover {
            transform: scale(1.05);
            box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        }
        .options-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
            gap: 15px;
            margin-top: 20px;
        }
        .option-tile {
            background: linear-gradient(135deg, #EE550B 0%, #E20014 100%);
            color: white;
            border-radius: 12px;
            padding: 20px;
            cursor: pointer;
            transition: all 0.3s ease;
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
            text-align: center;
            font-weight: 600;
            min-height: 80px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .option-tile:hover {
            transform: translateY(-5px);
            box-shadow: 0 8px 25px rgba(0,0,0,0.2);
            background: linear-gradient(135deg, #E20014 0%, #EE550B 100%);
        }
        .outcome-card {
            background: white;
            border-radius: 12px;
            padding: 20px;
            margin: 10px 0;
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
            border-left: 5px solid #FBB202;
        }
        .outcome-header {
            background: linear-gradient(135deg, #FBB202 0%, #EE550B 100%);
            color: white;
            padding: 15px;
            border-radius: 8px;
            text-align: center;
            font-size: 18px;
            font-weight: 600;
            margin-bottom: 20px;
        }
        .attribute-row {
            margin-bottom: 12px;
            padding: 8px 0;
            border-bottom: 1px solid #f0f0f0;
        }
        .attribute-row:last-child {
            border-bottom: none;
        }
        .attribute-label {
            font-weight: bold;
            color: #495057;
        }
        .attribute-value {
            color: #6c757d;
        }
        .watermark {
            position: fixed;
            bottom: 10px;
            right: 15px;
            font-size: 11px;
            color: rgba(255, 255, 255, 0.7);
            font-style: italic;
            background: rgba(0, 0, 0, 0.1);
            padding: 4px 8px;
            border-radius: 4px;
            backdrop-filter: blur(5px);
            z-index: 1000;
            user-select: none;
            pointer-events: none;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Procedural decision tree CT</h1>
        </div>
        <div id="breadcrumb" class="breadcrumb">
            <div class="breadcrumb-item" onclick="navigateToRoot()">🏠 Root</div>
        </div>
        <div id="content"></div>
    </div>
    
    <div class="watermark">author: L. Willocx</div>

    <script>
        const treeData = ', jsonlite::toJSON(nested_data, auto_unbox = TRUE), ';
        let currentPath = [];
        
        function isLeaf(obj) {
            if (typeof obj !== "object" || obj === null) return true;
            if (Array.isArray(obj)) return false;
            return Object.values(obj).every(val => typeof val !== "object" || val === null);
        }
        
        function renderContent() {
            let data = treeData;
            for (let step of currentPath) {
                data = data[step];
            }
            
            const contentDiv = document.getElementById("content");
            
            if (isLeaf(data)) {
                // Render outcome
                contentDiv.innerHTML = `
                    <div class="outcome-header">✅ Decision Outcome</div>
                    <div class="outcome-card">
                        ${Object.entries(data).map(([key, value]) => `
                            <div class="attribute-row">
                                <span class="attribute-label">${key}:</span>
                                <span class="attribute-value">${value || "N/A"}</span>
                            </div>
                        `).join("")}
                    </div>
                `;
            } else {
                // Render options
                const options = Object.keys(data);
                contentDiv.innerHTML = `
                    <div class="outcome-header">🔍 Choose Next Step (${options.length} options)</div>
                    <div class="options-grid">
                        ${options.map(option => `
                            <div class="option-tile" onclick="navigateToOption(\'${option}\')">
                                ${option}
                            </div>
                        `).join("")}
                    </div>
                `;
            }
        }
        
        function updateBreadcrumb() {
            const breadcrumb = document.getElementById("breadcrumb");
            let html = `<div class="breadcrumb-item" onclick="navigateToRoot()">🏠 Root</div>`;
            
            for (let i = 0; i < currentPath.length; i++) {
                html += `<div class="breadcrumb-item" onclick="navigateToLevel(${i})"> ${currentPath[i]}</div>`;
            }
            
            breadcrumb.innerHTML = html;
        }
        
        function navigateToRoot() {
            currentPath = [];
            updateBreadcrumb();
            renderContent();
        }
        
        function navigateToOption(option) {
            currentPath.push(option);
            updateBreadcrumb();
            renderContent();
        }
        
        function navigateToLevel(level) {
            currentPath = currentPath.slice(0, level + 1);
            updateBreadcrumb();
            renderContent();
        }
        
        // Initialize
        renderContent();
    </script>
</body>
</html>')
    
    return(html_content)
  }
  
  # Download JSON file
  output$download_json <- downloadHandler(
    filename = function() {
      input$output_filename
    },
    content = function(file) {
      writeLines(processed_data(), file)
    }
  )
  
  # Download HTML file
  output$download_html <- downloadHandler(
    filename = function() {
      gsub("\\.json$", ".html", input$output_filename)
    },
    content = function(file) {
      html_content <- generate_standalone_html()
      writeLines(html_content, file)
    }
  )
}

shinyApp(ui, server)