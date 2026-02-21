# ============================================================
# ZTH Core
# Product name (public): ZTH Core
# Version: 1.0.0
# Author: Maxime Tarzaali
#
# Purpose:
# - Adjust six structural tensions (R, Q, I, C, D, Ri)
# - Compute V_struct and V_human
# - Display viability zones, a radar chart, and a manual history
#
# Requires:
# - zth_core.r  (compute_V_from_vector(), compute_V_human(), etc.)
# - zth_viz.r   (plot_V_scale_vertical(), plot_tension_radar(), etc.)
# ============================================================

# ---- Packages (minimal) ----
suppressPackageStartupMessages({
  library(shiny)
  library(fmsb)    # for radar (used by plot_tension_radar in zth_viz.r)
  library(tibble)  # optional, safe to keep if your viz uses it
})

# ---- Local sources (fail fast if missing) ----
required_files <- c("zth_core.r", "zth_viz.r")
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required file(s): ", paste(missing_files, collapse = ", "),
       "\nPlace app.R in the same folder as zth_core.r and zth_viz.r.")
}

source("zth_core.r")
source("zth_viz.r")

# ---- App constants ----
APP_NAME    <- "ZTH Core"
APP_VERSION <- "v1.0.0"

# ---- Helpers ----
get_zone_label <- function(Vh) {
  if (Vh < 0.3) {
    "Zone 1 – Collapse / breakdown"
  } else if (Vh < 0.7) {
    "Zone 2 – Degradation"
  } else if (Vh <= 1.3) {
    "Zone 3 – Near intrinsic (Vi ≈ 1)"
  } else if (Vh <= 1.7) {
    "Zone 4 – Moderate amplification"
  } else {
    "Zone 5 – Strong amplification"
  }
}

# Color rule: promotive (R,Q,I) prefer high; inhibitive (C,D,Ri) prefer low.
color_for_tension <- function(name, val) {
  promotive <- name %in% c("R", "Q", "I")
  if (promotive) {
    if (val <= -1)      "red"
    else if (val == 0)  "black"
    else if (val == 1)  "#228B22"
    else if (val >= 2)  "#006400"
    else                "black"
  } else {
    if (val >= 1)       "red"
    else if (val == 0)  "black"
    else if (val == -1) "#228B22"
    else if (val <= -2) "#006400"
    else                "black"
  }
}

# ---- UI ----
ui <- fluidPage(
  title = paste(APP_NAME, APP_VERSION),
  
  titlePanel(paste(APP_NAME, "— Structural App", APP_VERSION)),
  
  tabsetPanel(
    id = "main_tabs",
    
    # ========== STRUCTURE ==========
    tabPanel(
      "Structure",
      sidebarLayout(
        sidebarPanel(
          h4("Tensions (sliders)"),
          
          sliderInput("R",  "Resource (R)", min = -3, max = 2, value = 0, step = 0.5),
          sliderInput("Q",  "Quality (Q)",  min = -3, max = 2, value = 0, step = 0.5),
          sliderInput("I",  "Impact (I)",   min = -3, max = 2, value = 0, step = 0.5),
          sliderInput("C",  "Cost (C)",     min = -3, max = 2, value = 0, step = 0.5),
          sliderInput("D",  "Delay (D)",    min = -3, max = 2, value = 0, step = 0.5),
          sliderInput("Ri", "Risk (Ri)",    min = -3, max = 2, value = 0, step = 0.5),
          
          br(),
          actionButton("save_to_history", "Save current state to history"),
          br(), br(),
          
          tags$div(
            style = "font-size:12px; color: #666;",
            HTML(paste0(
              "<b>", APP_NAME, "</b> ", APP_VERSION, "<br/>",
              "Companion software to the ZTH preprint series."
            ))
          )
        ),
        
        mainPanel(
          h3("Structural value"),
          uiOutput("V_value"),
          
          br(),
          plotOutput("V_scale_plot", height = "300px"),
          
          br(),
          h4("Current tensions (colour-coded)"),
          htmlOutput("tension_colors"),
          
          br(),
          h4("Radar – current tensions"),
          plotOutput("radar_current", height = "350px")
        )
      )
    ),
    
    # ========== HISTORY ==========
    tabPanel(
      "History",
      br(),
      fluidRow(
        column(
          width = 12,
          h4("Trajectory of V_human(t) (manual snapshots)"),
          plotOutput("history_plot", height = "350px")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          h4("History table"),
          tableOutput("history_table")
        )
      )
    )
  ),
  
  tags$hr(),
  tags$div(
    style = "text-align:center; font-size:12px; color: #777;",
    HTML(paste0("&copy; 2026 Maxime Tarzaali — ", APP_NAME, " ", APP_VERSION))
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Current tensions
  tensions_reactive <- reactiveVal(c(R = 0, Q = 0, I = 0, C = 0, D = 0, Ri = 0))
  
  observe({
    tensions_reactive(c(
      R  = input$R,
      Q  = input$Q,
      I  = input$I,
      C  = input$C,
      D  = input$D,
      Ri = input$Ri
    ))
  })
  
  # History (manual snapshots)
  history <- reactiveVal(
    data.frame(
      t        = integer(0),
      R        = numeric(0),
      Q        = numeric(0),
      I        = numeric(0),
      C        = numeric(0),
      D        = numeric(0),
      Ri       = numeric(0),
      V_struct = numeric(0),
      V_human  = numeric(0),
      Zone     = character(0),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$save_to_history, {
    tau <- tensions_reactive()
    V_struct <- compute_V_from_vector(tau)
    V_h      <- compute_V_human(V_struct)
    zone     <- get_zone_label(V_h)
    
    df <- history()
    new_t <- if (nrow(df) == 0) 1L else max(df$t) + 1L
    
    new_row <- data.frame(
      t        = new_t,
      R        = tau["R"],
      Q        = tau["Q"],
      I        = tau["I"],
      C        = tau["C"],
      D        = tau["D"],
      Ri       = tau["Ri"],
      V_struct = V_struct,
      V_human  = V_h,
      Zone     = zone,
      stringsAsFactors = FALSE
    )
    
    history(rbind(df, new_row))
    showNotification("Current state saved to history.", type = "message")
  })
  
  # ========== STRUCTURE OUTPUTS ==========
  output$V_value <- renderUI({
    tau <- tensions_reactive()
    V_struct <- compute_V_from_vector(tau)
    V_human  <- compute_V_human(V_struct)
    zone     <- get_zone_label(V_human)
    
    col <- if (V_human < 0.3) "darkred"
    else if (V_human < 0.7) "red"
    else if (V_human <= 1.3) "orange"
    else if (V_human <= 1.7) "darkgreen"
    else "darkgreen"
    
    htmltools::HTML(sprintf(
      "<b>V_struct = %.4f</b><br/>
       <b style='color:%s;'>V_human = %.3f</b><br/>
       <span style='color:%s;'>%s</span>",
      V_struct, col, V_human, col, zone
    ))
  })
  
  output$V_scale_plot <- renderPlot({
    tau <- tensions_reactive()
    V_struct <- compute_V_from_vector(tau)
    V_h      <- compute_V_human(V_struct)
    
    # assumes zth_viz.r provides plot_V_scale_vertical(V_h_current, V_h_snapshot)
    plot_V_scale_vertical(V_h_current = V_h, V_h_snapshot = NULL)
  })
  
  output$tension_colors <- renderUI({
    tau <- tensions_reactive()
    labels <- c("R", "Q", "I", "C", "D", "Ri")
    
    parts <- mapply(function(name, val) {
      col <- color_for_tension(name, val)
      sprintf("<b style='color:%s;'>%s</b> = %.1f", col, name, val)
    }, labels, tau)
    
    htmltools::HTML(paste(parts, collapse = "<br/>"))
  })
  
  output$radar_current <- renderPlot({
    tau <- tensions_reactive()
    # assumes zth_viz.r provides plot_tension_radar(tau, main=...)
    plot_tension_radar(tau, main = "Current tensions (τ)")
  })
  
  # ========== HISTORY OUTPUTS ==========
  output$history_plot <- renderPlot({
    df <- history()
    
    if (nrow(df) == 0) {
      plot(0, 0, type = "n",
           xlab = "Snapshot index",
           ylab = "V_human",
           xlim = c(0, 1),
           ylim = c(0, 2),
           main = "History of V_human")
      text(0.5, 1, "No history yet")
      return()
    }
    
    plot(df$t, df$V_human,
         type = "l",
         xlab = "Snapshot index",
         ylab = "V_human",
         ylim = c(0, 2),
         col  = "grey40",
         lwd  = 1.5,
         main = "History of V_human (manual states)")
    
    points(df$t, df$V_human, pch = 19, cex = 1.1)
    abline(h = 1, col = "grey", lty = 2)
    abline(h = c(0.3, 0.7, 1.3, 1.7), col = "lightgrey", lty = 3)
  })
  
  output$history_table <- renderTable({
    df <- history()
    if (nrow(df) == 0) return(NULL)
    df
  }, digits = 3)
}

shinyApp(ui, server)