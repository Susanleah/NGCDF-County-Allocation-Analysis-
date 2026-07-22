# ============================================================
# NGCDF MERGED SHINY APP
# Group A: Trans Nzoia | Nandi | Uasin Gishu
# Group B: Murang'a | Kiambu | Kirinyaga
# Features: Forecast · Tiers · CPI Adjustment · Gini ·
#           Per-Voter · National Ranking · Neighbours
# ============================================================
# Install:
#   install.packages(c("shiny","shinydashboard","tidyverse",
#                      "plotly","DT","scales","ineq","bslib"))
# Run:
#   shiny::runApp("NGCDF_Merged_Shiny.R")
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(ineq)
library(bslib)
library(dplyr)

options(scipen = 999)

# ============================================================
# GLOBAL CONSTANTS
# ============================================================

GROUP_A    <- c("Trans Nzoia", "Nandi", "Uasin Gishu")
GROUP_B    <- c("Murang'a", "Kiambu", "Kirinyaga")
ALL_TARGET <- c(GROUP_A, GROUP_B)
NEIGHBOURS_A <- c("Elgeyo-Marakwet","Baringo","Kakamega","West Pokot","Nyandarua")
NEIGHBOURS_B <- c("Nyandarua","Embu","Machakos","Nairobi City","Nyeri")
FC_YEARS  <- c("2027","2028","2029","2030")
COL <- c(
  "Trans Nzoia"     = "#00d4aa",
  "Nandi"           = "#f59e0b",
  "Uasin Gishu"     = "#3b82f6",
  "Murang'a"        = "#10b981",
  "Kiambu"          = "#f43f5e",
  "Kirinyaga"       = "#6366f1",
  "Elgeyo-Marakwet" = "#8b5cf6",
  "Baringo"         = "#ec4899",
  "Kakamega"        = "#f97316",
  "West Pokot"      = "#06b6d4",
  "Nyandarua"       = "#a3e635",
  "Embu"            = "#ec4899",
  "Machakos"        = "#f97316",
  "Nairobi City"    = "#06b6d4",
  "Nyeri"           = "#84cc16"
)
CPI_DF <- tibble(
  year = 2014:2026,
  cpi  = c(100,106,111,118,126,131,138,148,162,177,189,198,208))

# ============================================================
# DATA LOADING (once, shared by all groups)
# ============================================================

data_path <- "NEW_NGCDF_DATA_28-JAN_2026.csv"

load_data <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Data file not found at: ", path, "\n",
      "Make sure 'NEW_NGCDF_DATA_28-JAN_2026.csv' is in the same folder as this ",
      "app script, and launch with shiny::runApp() pointed at that folder ",
      "(not by sourcing this file from a different working directory)."
    )
  }

  df_raw <- read_csv(
    path,
    show_col_types = FALSE) %>%
    clean_names()

  names(df_raw) <- trimws(names(df_raw))
  names(df_raw) <- sub("^x(\\d{4})$", "\\1", names(df_raw))
  YEARS <<- sort(names(df_raw)[grepl("^\\d{4}$", names(df_raw))])

  df_raw %>%
    filter(!is.na(county) & county != "", !is.na(constituency) & constituency != "") %>%
    mutate(
      county = case_when(
        grepl("^Murang", county, ignore.case = TRUE) ~ "Murang'a",
        TRUE ~ county),
      voters = suppressWarnings(as.integer(voters)),
      across(all_of(YEARS), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    select(county, constituency, voters, all_of(YEARS))
}

df_all <- load_data(data_path)

# ============================================================
# HELPER: build pre-computed data for a group
# ============================================================

build_group_data <- function(TARGET) {
  df <- df_all %>% filter(county %in% TARGET)
  voter_totals <- df %>% group_by(county) %>% summarise(total_voters=sum(voters, na.rm=TRUE), .groups="drop")
  county_yr <- df %>% pivot_longer(all_of(YEARS), names_to="year", values_to="alloc") %>% mutate(year=as.integer(year)) %>%
    filter(!is.na(alloc)) %>% group_by(county, year) %>% summarise(total=sum(alloc), mean_alloc=mean(alloc),
              sd_alloc=sd(alloc), n=n(), .groups="drop") %>%
    group_by(county) %>%
    mutate(yoy_pct=(total/lag(total)-1)*100,
           cumul=cumsum(total),
           idx=total/first(total)*100) %>%
    ungroup() %>%
    left_join(CPI_DF, by="year") %>%
    left_join(voter_totals, by="county") %>%
    mutate(real_total=total/cpi*100,
           per_voter=total/total_voters)
  forecast_df <- county_yr %>%
    filter(year >= 2020) %>%
    group_by(county) %>%
    do({
      mod <- lm(total ~ year, data=.)
      tibble(year=2027:2030,
             total=predict(mod, newdata=data.frame(year=2027:2030)),
             type="Forecast")}) %>%
    ungroup()
  trend_combined <- county_yr %>%
    select(county, year, total) %>%
    mutate(type="Actual") %>%
    bind_rows(forecast_df)
  gini_df <- map_dfr(TARGET, function(co) {
    map_dfr(YEARS, function(yr) {
      vals <- df %>% filter(county==co, !is.na(.data[[yr]])) %>% pull(yr)
      if (length(vals) >= 2)
        tibble(county=co, year=as.integer(yr),
               gini=Gini(vals),
               cv=sd(vals)/mean(vals)*100,
               max_min_ratio=max(vals)/min(vals))})})
  nat_ranking <- df_all %>%
    filter(!is.na(`2025`)) %>%
    group_by(county) %>%
    summarise(total_2025=sum(`2025`), n_const=n(), .groups="drop") %>%
    arrange(desc(total_2025)) %>%
    mutate(rank=row_number(),
           avg_per_const=round(total_2025/n_const),
           is_target=county %in% TARGET)
  list(df=df, county_yr=county_yr, forecast_df=forecast_df,
       trend_combined=trend_combined, gini_df=gini_df, nat_ranking=nat_ranking)}
gA <- build_group_data(GROUP_A)
gB <- build_group_data(GROUP_B)
make_tiers <- function(df, yr) {
  df %>%
    filter(!is.na(.data[[yr]])) %>%
    group_by(county) %>%
    mutate(
      q33=quantile(.data[[yr]], 1/3),
      q67=quantile(.data[[yr]], 2/3),
      tier=case_when(.data[[yr]] >= q67 ~ "High",
                     .data[[yr]] >= q33 ~ "Mid",
                     TRUE               ~ "Low"),
      vs_mean=(.data[[yr]]/mean(.data[[yr]])-1)*100,
      per_voter=ifelse(!is.na(voters) & voters>0, .data[[yr]]/voters, NA)
    ) %>%
    ungroup() %>%
    rename(alloc=!!yr)}

# Shared helpers
dark_layout <- function(p, title="", subtitle="", xlab="", ylab="") {
  p %>% layout(
    title=list(text=paste0("<b>",title,"</b>",
                           if(nchar(subtitle)>0) paste0("<br><sup>",subtitle,"</sup>") else ""),
               font=list(color="#e2e8f0",size=15)),
    paper_bgcolor="#0F1B2D", plot_bgcolor="#0F1B2D",
    font=list(color="#94a3b8", family="sans-serif"),
    xaxis=list(title=xlab, gridcolor="#1e3050", tickfont=list(color="#64748b")),
    yaxis=list(title=ylab, gridcolor="#1e3050", tickfont=list(color="#64748b")),
    legend=list(bgcolor="rgba(0,0,0,0)", font=list(color="#94a3b8")),
    margin=list(t=70, l=60, r=20, b=60))}
fmtB <- function(x) paste0("KES ", round(x/1e9,2), "B")
fmtM <- function(x) paste0("KES ", round(x/1e6,1), "M")

# ============================================================
# SHARED CSS
# ============================================================

app_css <- "
  body { background:#060c18; color:#e2e8f0; }
  .navbar,.navbar-default { background:#0f1b2d!important; border-color:#1e3050!important; }
  .navbar-brand { color:#00d4aa!important; font-weight:800; font-size:16px; }
  .nav-tabs > li > a { color:#64748b; background:#0f1b2d; border-color:#1e3050; }
  .nav-tabs > li.active > a { color:#00d4aa!important; background:#060c18!important;
    border-color:#1e3050 #1e3050 #060c18!important; font-weight:700; }
  .nav-tabs > li > a:hover { color:#e2e8f0; background:#162236; }
  .well,.panel { background:#0f1b2d!important; border-color:#1e3050!important; color:#e2e8f0; }
  .panel-heading { background:#162236!important; border-color:#1e3050!important; font-weight:700; }
  .selectize-input,.form-control,select {
    background:#162236!important; color:#e2e8f0!important; border-color:#1e3050!important; }
  .selectize-dropdown { background:#162236!important; border-color:#1e3050!important; }
  .selectize-dropdown-content .option { color:#e2e8f0; }
  .selectize-dropdown-content .option:hover { background:#243860; }
  .kpi-box { background:#0f1b2d; border:1px solid #1e3050; border-radius:12px;
    padding:16px 18px; margin-bottom:14px; }
  .kpi-label { font-size:10px; font-weight:700; letter-spacing:1.4px;
    text-transform:uppercase; color:#64748b; margin-bottom:6px; }
  .kpi-val { font-family:'IBM Plex Mono',monospace; font-size:22px; font-weight:700; }
  .kpi-sub { font-size:11px; color:#64748b; margin-top:4px; }
  .kpi-tn { color:#00d4aa; } .kpi-na { color:#f59e0b; } .kpi-ug { color:#3b82f6; }
  .kpi-ma { color:#10b981; } .kpi-ki { color:#f43f5e; } .kpi-kr { color:#6366f1; }
  .insight-box { background:#0f1b2d; border-left:3px solid #00d4aa;
    border-radius:0 8px 8px 0; padding:13px 16px; margin:6px 0; }
  .insight-box.warn { border-left-color:#f59e0b; }
  .insight-box.info { border-left-color:#3b82f6; }
  .insight-title { font-size:12.5px; font-weight:700; margin-bottom:5px; }
  .insight-body { font-size:11.5px; color:#94a3b8; line-height:1.65; }
  .insight-verdict { font-size:12px; font-weight:600; color:#00d4aa; margin-top:6px; }
  .dataTables_wrapper { color:#e2e8f0!important; }
  table.dataTable { background:#0f1b2d!important; color:#e2e8f0!important; }
  table.dataTable thead th { background:#162236!important; color:#64748b!important;
    border-color:#1e3050!important; }
  table.dataTable tbody tr { background:#0f1b2d!important; }
  table.dataTable tbody tr:hover { background:#162236!important; }
  table.dataTable tbody td { border-color:#1e3050!important; }
  .dataTables_filter input,.dataTables_length select {
    background:#162236!important; color:#e2e8f0!important; border-color:#1e3050!important; }
  .dataTables_info,.dataTables_paginate { color:#64748b!important; }
  .paginate_button { color:#64748b!important; }
  .paginate_button.current { background:#162236!important; color:#00d4aa!important; }
  .header-strip { background:linear-gradient(135deg,#060c18,#0f1b2d);
    border-bottom:1px solid #1e3050; padding:16px 28px;
    display:flex; align-items:center; gap:16px; margin-bottom:20px; }
  .hbar { width:4px; height:52px; border-radius:2px; flex-shrink:0;
    background:linear-gradient(180deg,#00d4aa,#f59e0b,#3b82f6); }
  .fc-note { background:#162236; border:1px dashed rgba(139,92,246,.4);
    border-radius:8px; padding:9px 14px; font-size:11px; color:#8b5cf6; margin-top:8px; }
  .tier-H { background:rgba(16,185,129,.15); color:#10b981;
    padding:2px 8px; border-radius:10px; font-size:11px; font-weight:700; }
  .tier-M { background:rgba(245,158,11,.15); color:#f59e0b;
    padding:2px 8px; border-radius:10px; font-size:11px; font-weight:700; }
  .tier-L { background:rgba(239,68,68,.15); color:#ef4444;
    padding:2px 8px; border-radius:10px; font-size:11px; font-weight:700; }
  .group-badge { padding:3px 10px; border-radius:12px; font-size:10px;
    font-weight:700; font-family:'IBM Plex Mono',monospace; margin-right:4px; }
  .app-footer { background:#0f1b2d; border-top:1px solid #1e3050;
    padding:14px 28px; margin-top:24px; font-size:11px; color:#64748b;
    display:flex; flex-wrap:wrap; justify-content:space-between; gap:8px; }
  .app-footer b { color:#94a3b8; }
"

# ============================================================
# UI BUILDER reusable tab set for each group
# ============================================================

group_tab_ui <- function(grp_id, TARGET, header_title, header_sub, rank_badges) {
  ns <- function(id) paste0(grp_id, "_", id)
  tabPanel(header_title,
    br(),
    # Header strip
    div(class="header-strip",
      div(class="hbar"),
      div(
        tags$h4(header_title, style="margin:0;font-weight:800;font-size:17px;"),
        tags$p(header_sub,    style="margin:3px 0 0;font-size:11px;color:#64748b;")
      ),
      div(style="margin-left:auto;display:flex;gap:8px;", rank_badges)
    ),

    tabsetPanel(
      # Summary
      tabPanel("Summary", br(),
        fluidRow(
          column(4, uiOutput(ns("kpi1"))),
          column(4, uiOutput(ns("kpi2"))),
          column(4, uiOutput(ns("kpi3")))),
        fluidRow(
          column(6, plotlyOutput(ns("p_totals"),   height="320px")),
          column(6, plotlyOutput(ns("p_indexed"),  height="320px"))
        ), br(),
        fluidRow(
          column(6, plotlyOutput(ns("p_pervoter"), height="300px")),
          column(6, plotlyOutput(ns("p_yoy"),      height="300px")))),

      # Forecast
      tabPanel("Forecast 2027–2030", br(),
        fluidRow(
          column(4, uiOutput(ns("fc_kpi1"))),
          column(4, uiOutput(ns("fc_kpi2"))),
          column(4, uiOutput(ns("fc_kpi3")))),
        plotlyOutput(ns("p_forecast"), height="380px"), br(),
        fluidRow(
          column(6, plotlyOutput(ns("p_fc_cumul"), height="300px")),
          column(6,
            h5("Forecast Values (KES)", style="color:#e2e8f0;font-weight:700;margin-bottom:10px;"),
            DTOutput(ns("fc_table")),
            div(class="fc-note", " Forecast uses linear OLS trend from 2020–2026.
              Actual allocations depend on national budget decisions and formula revisions.")))),

      # Equity & Gini
      tabPanel("Equity & Gini", br(),
        fluidRow(
          column(4, uiOutput(ns("gini_kpi1"))),
          column(4, uiOutput(ns("gini_kpi2"))),
          column(4, uiOutput(ns("gini_kpi3")))),
        fluidRow(
          column(6, plotlyOutput(ns("p_gini"),   height="320px")),
          column(6, plotlyOutput(ns("p_spread"), height="320px"))
        ), br(),
        fluidRow(
          column(6, plotlyOutput(ns("p_cv"),          height="280px")),
          column(6, uiOutput(ns("equity_insights"))))),

      # Tiers
      tabPanel("Performance Tiers", br(),
        fluidRow(
          column(3,
            wellPanel(
              selectInput(ns("tier_year"), "Select Year:", choices=YEARS, selected="2025"),
              hr(style="border-color:#1e3050;"),
              uiOutput(ns("tier_summary")))),
          column(9,
            plotlyOutput(ns("p_tiers_bar"), height="360px"), br(),
            DTOutput(ns("tiers_table"))))),

      # CPI
      tabPanel("Real vs Nominal (CPI)", br(),
        fluidRow(
          column(3,
            wellPanel(
              selectInput(ns("infl_county"), "Select County:", choices=TARGET, selected=TARGET[1]),
              hr(style="border-color:#1e3050;"),
              uiOutput(ns("infl_kpi")))),
          column(9, plotlyOutput(ns("p_inflation"), height="320px"))
        ), br(),
        fluidRow(
          column(6, plotlyOutput(ns("p_real_comp"), height="300px")),
          column(6, plotlyOutput(ns("p_cpi_line"),  height="300px"))
        ), br(),
        uiOutput(ns("infl_insights"))),

      # National Ranking
      tabPanel("🇰🇪 National Ranking", br(),
        fluidRow(
          column(4, uiOutput(ns("rank_kpi1"))),
          column(4, uiOutput(ns("rank_kpi2"))),
          column(4, uiOutput(ns("rank_kpi3")))),
        plotlyOutput(ns("p_all_counties"), height="480px"), br(),
        DTOutput(ns("rank_table"))),

      # Neighbours
      tabPanel("Neighbours", br(),
        fluidRow(
          column(6, plotlyOutput(ns("p_nb_bar"),  height="360px")),
          column(6, plotlyOutput(ns("p_nb_line"), height="360px"))
        ), br(),
        DTOutput(ns("nb_table"))),

      # Data Table
      tabPanel("Data Table", br(),
        fluidRow(
          column(3,
            wellPanel(
              selectInput(ns("dt_county"),    "Filter County:", choices=c("All",TARGET), selected="All"),
              selectInput(ns("dt_year_from"), "From Year:", choices=YEARS, selected="2014"),
              selectInput(ns("dt_year_to"),   "To Year:",   choices=YEARS, selected="2025"))),
          column(9, DTOutput(ns("data_table")))))))}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  theme = bs_theme(
    bg="#060C18", fg="#e2e8f0", primary="#00d4aa", secondary="#1e3050",
    base_font=font_google("Sora"), code_font=font_google("IBM Plex Mono"),
    bootswatch=NULL),
  tags$head(tags$style(HTML(app_css))),
  tags$div(
    style = "padding:10px 24px;font-family:'IBM Plex Mono',monospace;
             font-size:11px;color:#94a3b8;border-bottom:1px solid #1e3050;",
    "NGCDF County Allocation Dashboard  ·  Group 9  ·  ",
    "Stephen Nzambu Ndundu & Susan Leah Wangari  ·  ",
    "Data: National Government Constituencies Development Fund Board, accessed 28 Jan 2026"
  ),
  tabsetPanel(id="top_tabs",

    # GROUP A TAB
    group_tab_ui(
      grp_id       = "A",
      TARGET       = GROUP_A,
      header_title = "Rift Valley: Trans Nzoia · Nandi · Uasin Gishu",
      header_sub   = "Kenya NGCDF 2014–2030",
      rank_badges  = tagList(
        tags$span("Trans Nzoia #31", style="background:rgba(0,212,170,.1);color:#00d4aa;
          border:1px solid rgba(0,212,170,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"),
        tags$span("Nandi #20", style="background:rgba(245,158,11,.1);color:#f59e0b;
          border:1px solid rgba(245,158,11,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"),
        tags$span("Uasin Gishu #26", style="background:rgba(59,130,246,.1);color:#3b82f6;
          border:1px solid rgba(59,130,246,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"))),

    # GROUP B TAB
    group_tab_ui(
      grp_id       = "B",
      TARGET       = GROUP_B,
      header_title = "Central: Murang'a · Kiambu · Kirinyaga",
      header_sub   = "Kenya NGCDF 2014–2030",
      rank_badges  = tagList(
        tags$span("Kiambu #2", style="background:rgba(244,63,94,.1);color:#f43f5e;
          border:1px solid rgba(244,63,94,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"),
        tags$span("Murang'a #8", style="background:rgba(16,185,129,.1);color:#10b981;
          border:1px solid rgba(16,185,129,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"),
        tags$span("Kirinyaga #35", style="background:rgba(99,102,241,.1);color:#6366f1;
          border:1px solid rgba(99,102,241,.3);padding:4px 10px;border-radius:20px;
          font-size:10px;font-weight:700;font-family:'IBM Plex Mono',monospace;"))),

    # CROSS-GROUP TAB
    tabPanel("All 6 Counties", br(),
      div(class="header-strip",
        div(class="hbar"),
        div(tags$h4("All 6 Counties — Cross-Group Comparison",
                  style="margin:0;font-weight:800;font-size:17px;"),
          tags$p("Rift Valley vs Central — Kenya NGCDF 2014–2026",
                 style="margin:3px 0 0;font-size:11px;color:#64748b;"))),
      fluidRow(
        column(6, plotlyOutput("p_cross_trends", height="360px")),
        column(6, plotlyOutput("p_cross_bar",    height="360px"))
      ), br(),
      fluidRow(
        column(6, plotlyOutput("p_cross_pervoter", height="320px")),
        column(6, plotlyOutput("p_cross_ranking",  height="320px"))))),

  div(class="app-footer",
    div(HTML("<b>NGCDF County Allocation Dashboard</b> &middot; Group 9 &middot; ",
             "Stephen Nzambu Ndundu &amp; Susan Leah Wangari &middot; SDS 6103, University of Nairobi")),
    div(HTML("Data: NGCDF Board constituency allocations 2014&ndash;2026, ",
             "IEBC registered voters 2022, KNBS CPI &middot; accessed 28 Jan 2026")))
)

# ============================================================
# SERVER BUILDER generates all outputs for one group
# ============================================================

group_server <- function(input, output, session, grp_id, TARGET, NEIGHBOURS, g) {
  ns <- function(id) paste0(grp_id, "_", id)
  SHOW_CO <- c(TARGET, NEIGHBOURS)

  # colour class per county position in TARGET
  css_cls <- c("1"=switch(grp_id, A="kpi-tn", B="kpi-ma"),
               "2"=switch(grp_id, A="kpi-na", B="kpi-ki"),
               "3"=switch(grp_id, A="kpi-ug", B="kpi-kr"))
  co_cls <- function(co) css_cls[as.character(which(TARGET==co))]

  # KPI card
  make_kpi <- function(co, yr="2025") {
    t25  <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==as.integer(yr)]
    t14  <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2014]
    gw   <- (t25/t14-1)*100
    pv   <- g$county_yr$per_voter[g$county_yr$county==co & g$county_yr$year==as.integer(yr)]
    rank <- g$nat_ranking$rank[g$nat_ranking$county==co]
    n    <- nrow(g$df[g$df$county==co,])
    nat_m_all <- df_all %>% filter(!is.na(.data[[yr]])) %>% pull(yr) %>% mean()
    vs_nat <- (mean(g$df[g$df$county==co & !is.na(g$df[[yr]]),][[yr]]) / nat_m_all - 1)*100
    div(class="kpi-box",
      div(class="kpi-label", co),
      div(class=paste("kpi-val", co_cls(co)), fmtB(t25)),
      div(class="kpi-sub", paste0(yr," total · ",n," constituencies · Rank #",rank,"/47")),
      br(),
      fluidRow(
        column(4, div(class="kpi-label","Growth 2014→25"),
          div(style=paste0("font-family:'IBM Plex Mono',monospace;font-weight:700;color:",
                           ifelse(gw>0,"#10b981","#ef4444"),";"),
              paste0(ifelse(gw>0,"↑ +","↓ "),round(gw,1),"%"))),
        column(4, div(class="kpi-label","KES / Voter"),
          div(style="font-family:'IBM Plex Mono',monospace;font-weight:600;",
              format(round(pv), big.mark=","))),
        column(4, div(class="kpi-label","vs Nat. Avg"),
          div(style=paste0("font-weight:700;color:",ifelse(vs_nat>0,"#10b981","#ef4444"),";"),
              ifelse(vs_nat>0,"↑ Above","↓ Below")))))}
  output[[ns("kpi1")]] <- renderUI(make_kpi(TARGET[1]))
  output[[ns("kpi2")]] <- renderUI(make_kpi(TARGET[2]))
  output[[ns("kpi3")]] <- renderUI(make_kpi(TARGET[3]))

  # Summary plots
  output[[ns("p_totals")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      d <- g$county_yr %>% filter(county==co)
      p <- add_trace(p, data=d, x=~year, y=~total, name=co, type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.5),
                     marker=list(color=COL[co],size=7,line=list(color="#060c18",width=2)))}
    dark_layout(p,"County Total Allocations 2014–2026","KES absolute",ylab="Total (KES)")})
  output[[ns("p_indexed")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      d <- g$county_yr %>% filter(county==co) %>% mutate(idx=total/first(total)*100)
      p <- add_trace(p, data=d, x=~year, y=~idx, name=co, type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.5),
                     marker=list(color=COL[co],size=7,line=list(color="#060c18",width=2)))}
    p %>% add_segments(x=2014,xend=2026,y=100,yend=100,
                       line=list(color="#334155",dash="dash"),showlegend=FALSE) %>%
      dark_layout("Indexed Growth (2014 = 100)","Comparable growth trajectory",ylab="Index")})

  output[[ns("p_pervoter")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      d <- g$county_yr %>% filter(county==co)
      p <- add_trace(p, data=d, x=~year, y=~per_voter, name=co, type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.5),
                     marker=list(color=COL[co],size=7,line=list(color="#060c18",width=2)))}
    dark_layout(p,"Per-Voter Allocation 2014–2026","County total ÷ registered voters",ylab="KES per Voter")})
  output[[ns("p_yoy")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      d <- g$county_yr %>% filter(county==co, !is.na(yoy_pct))
      p <- add_trace(p, data=d, x=~year, y=~yoy_pct, name=co, type="bar",
                     marker=list(color=paste0(COL[co],"99")))}
    p %>% layout(barmode="group") %>%
      dark_layout("Year-on-Year % Change","Positive = growth vs prior year",ylab="YoY Change (%)")})

  # Forecast
  make_fc_kpi <- function(co) {
    fc <- g$forecast_df %>% filter(county==co) %>% arrange(year)
    t25 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2025]
    g2030 <- (fc$total[4]/t25-1)*100
    div(class="kpi-box",
      div(class="kpi-label", paste(co,"— Projected 2030")),
      div(class=paste("kpi-val",co_cls(co)), fmtB(fc$total[4])),
      div(class="kpi-sub", paste0("From ",fmtB(t25)," (2025) · +",round(g2030,1),"% projected")),
      br(),
      fluidRow(lapply(1:4, function(i)
        column(3,
          div(class="kpi-label", as.character(2026+i)),
          div(style="font-family:'IBM Plex Mono',monospace;font-size:12px;font-weight:600;",
              fmtB(fc$total[i]))))))}
  output[[ns("fc_kpi1")]] <- renderUI(make_fc_kpi(TARGET[1]))
  output[[ns("fc_kpi2")]] <- renderUI(make_fc_kpi(TARGET[2]))
  output[[ns("fc_kpi3")]] <- renderUI(make_fc_kpi(TARGET[3]))
  output[[ns("p_forecast")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      real <- g$trend_combined %>% filter(county==co, type=="Actual")
      fc   <- g$trend_combined %>% filter(county==co, type=="Forecast")
      p <- p %>%
        add_trace(data=real, x=~year, y=~total, name=co, type="scatter", mode="lines+markers",
                  line=list(color=COL[co],width=2.5),
                  marker=list(color=COL[co],size=7,line=list(color="#060c18",width=2)),
                  legendgroup=co) %>%
        add_trace(data=fc, x=~year, y=~total, name=paste(co,"(forecast)"),
                  type="scatter", mode="lines+markers",
                  line=list(color=COL[co],width=2,dash="dash"),
                  marker=list(color=COL[co],size=8,symbol="triangle-up"),
                  legendgroup=co, showlegend=TRUE)}
    p %>%
      add_segments(x=2026.5,xend=2026.5,y=0,yend=max(g$trend_combined$total,na.rm=TRUE),
                   line=list(color="#334155",dash="dash",width=1),showlegend=FALSE) %>%
      dark_layout("Allocation Forecast 2027–2030","Dashed = linear projection",ylab="Total (KES)")})

  output[[ns("p_fc_cumul")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET) {
      d <- g$trend_combined %>% filter(county==co) %>% arrange(year) %>% mutate(cumul=cumsum(total))
      p <- add_trace(p, data=d, x=~year, y=~cumul, name=co, type="scatter", mode="lines",
                     fill="tozeroy", fillcolor=paste0(COL[co],"22"),
                     line=list(color=COL[co],width=2))}
    dark_layout(p,"Cumulative Allocation 2014–2030","Running total incl. projected years",ylab="Cumulative (KES)")})

  output[[ns("fc_table")]] <- renderDT({
    rows <- c(tail(YEARS,3), FC_YEARS)
    tbl <- map_dfr(rows, function(yr) {
      is_fc <- yr %in% FC_YEARS
      row_data <- list(Year=yr)
      for (co in TARGET) {
        v <- if (is_fc) {
          g$forecast_df$total[g$forecast_df$county==co][which(FC_YEARS==yr)]
        } else {
          g$county_yr$total[g$county_yr$county==co & g$county_yr$year==as.integer(yr)]
        }
        row_data[[co]] <- format(round(v), big.mark=",")
      }
      as_tibble(row_data)
    })
    datatable(tbl, rownames=FALSE, options=list(dom="t", pageLength=10), caption=" = Projected")})

  # Gini
  make_gini_kpi <- function(co) {
    g25  <- g$gini_df$gini[g$gini_df$county==co & g$gini_df$year==2025]
    g14  <- g$gini_df$gini[g$gini_df$county==co & g$gini_df$year==2014]
    delta <- g25 - g14
    cv25  <- g$gini_df$cv[g$gini_df$county==co & g$gini_df$year==2025]
    div(class="kpi-box",
      div(class="kpi-label", paste(co,"— Gini 2025")),
      div(class=paste("kpi-val",co_cls(co)), round(g25,4)),
      div(class="kpi-sub","0 = perfect equality · Higher = more unequal"),
      br(),
      div(style="background:#162236;border-radius:6px;height:8px;overflow:hidden;",
        div(style=paste0("width:",min(g25*1000,100),
                         "%;height:100%;background:linear-gradient(90deg,#10b981,#f59e0b,#ef4444);"))),
      br(),
      fluidRow(
        column(6, div(class="kpi-label","vs 2014"),
          div(style=paste0("font-weight:700;font-family:'IBM Plex Mono',monospace;color:",
                           ifelse(delta>0,"#ef4444","#10b981"),";"),
              paste0(ifelse(delta>0,"↑ +","↓ "),round(delta,4)))),
        column(6, div(class="kpi-label","CV %"),
          div(style="font-family:'IBM Plex Mono',monospace;font-weight:600;",
              paste0(round(cv25,1),"%")))))}
  output[[ns("gini_kpi1")]] <- renderUI(make_gini_kpi(TARGET[1]))
  output[[ns("gini_kpi2")]] <- renderUI(make_gini_kpi(TARGET[2]))
  output[[ns("gini_kpi3")]] <- renderUI(make_gini_kpi(TARGET[3]))

  output[[ns("p_gini")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET)
      p <- add_trace(p, data=filter(g$gini_df,county==co), x=~year, y=~gini,
                     name=co, type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.5),
                     marker=list(color=COL[co],size=8,line=list(color="#060c18",width=2)))
    dark_layout(p,"Gini Coefficient 2014–2026","Intra-county allocation inequality",ylab="Gini")})

  output[[ns("p_cv")]] <- renderPlotly({
    p <- plot_ly()
    for (co in TARGET)
      p <- add_trace(p, data=filter(g$gini_df,county==co), x=~year, y=~cv,
                     name=co, type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.5),
                     marker=list(color=COL[co],size=8,line=list(color="#060c18",width=2)))
    dark_layout(p,"Coefficient of Variation (CV)","Higher = more spread between constituencies",ylab="CV (%)")})

  output[[ns("p_spread")]] <- renderPlotly({
    grps  <- lapply(TARGET, function(co) g$df %>% filter(county==co,!is.na(`2025`)) %>% pull(`2025`))
    plot_ly(x=TARGET) %>%
      add_trace(y=sapply(grps,min)/1e6,  name="Min",  type="bar",
                marker=list(color=paste0(sapply(TARGET,function(co)COL[co]),"44"))) %>%
      add_trace(y=sapply(grps,mean)/1e6, name="Mean", type="bar",
                marker=list(color=paste0(sapply(TARGET,function(co)COL[co]),"99"))) %>%
      add_trace(y=sapply(grps,max)/1e6,  name="Max",  type="bar",
                marker=list(color=paste0(sapply(TARGET,function(co)COL[co]),"ee"))) %>%
      layout(barmode="group") %>%
      dark_layout("Constituency Spread — 2025","Min / Mean / Max allocation",ylab="KES Millions")})

  output[[ns("equity_insights")]] <- renderUI({
    ratios <- sapply(TARGET, function(co) {
      vals <- g$df %>% filter(county==co,!is.na(`2025`)) %>% pull(`2025`)
      round(max(vals)/min(vals),2)})
    ratio_txt <- paste(paste0("<b>",TARGET,"</b>: ",ratios,"x"), collapse=" · ")
    tagList(
      div(class="insight-box",
        div(class="insight-title","Gini Interpretation"),
        div(class="insight-body", HTML(paste0(
          "All target counties show <strong>very low Gini values</strong>, ",
          "indicating highly equal distribution across constituencies.<br>",
          "Max/Min ratios — ", ratio_txt))),
        div(class="insight-verdict","✓ Formula design ensures strong intra-county equity")
      ), br(),
      div(class="insight-box info",
        div(class="insight-title","What drives remaining inequality?"),
        div(class="insight-body", HTML(
          "• <b>Voter population differences</b> create small allocation differentials<br>
           • <b>Poverty index weighting</b> favours more rural constituencies<br>
           • 2023+ spike reflects return to voter-differentiated weighting")),
        div(class="insight-verdict","→ Spike from 2023 is structural, not inequity")))})

  # Tiers
  tiers_r <- reactive({ make_tiers(g$df, input[[ns("tier_year")]]) })

  output[[ns("tier_summary")]] <- renderUI(tagList(
    h6("Tier counts:", style="color:#64748b;"),
    DTOutput(ns("tier_count_mini"))))
  output[[ns("tier_count_mini")]] <- renderDT({
    tiers_r() %>% count(county, tier) %>%
      pivot_wider(names_from=tier, values_from=n, values_fill=0) %>%
      datatable(rownames=FALSE, options=list(dom="t"))})

  output[[ns("p_tiers_bar")]] <- renderPlotly({
    td <- tiers_r()
    tier_col <- c("High"="#10b981","Mid"="#f59e0b","Low"="#ef4444")
    p <- plot_ly()
    for (co in TARGET) {
      d <- td %>% filter(county==co) %>% arrange(desc(alloc))
      for (tier in c("High","Mid","Low")) {
        dt <- d %>% filter(tier==!!tier)
        if (nrow(dt)>0)
          p <- add_trace(p, data=dt, x=~alloc/1e6, y=~constituency,
                         name=tier, type="bar", orientation="h",
                         marker=list(color=tier_col[tier]),
                         legendgroup=tier, showlegend=(co==TARGET[1]))}}
    dark_layout(p, paste("Performance Tiers —",input[[ns("tier_year")]]),
                "High = top third · Mid = middle · Low = bottom", xlab="KES Millions")})

  output[[ns("tiers_table")]] <- renderDT({
    tiers_r() %>%
      select(County=county, Constituency=constituency, Voters=voters,
             `Allocation (KES)`=alloc, `Per Voter (KES)`=per_voter,
             Tier=tier, `vs Mean %`=vs_mean) %>%
      mutate(across(c(`Allocation (KES)`,`Per Voter (KES)`), ~round(.,0)),
             `vs Mean %`=round(`vs Mean %`,1)) %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=20, scrollX=TRUE),
                callback=JS("table.column(5).nodes().each(function(cell,i){
                  var v=cell.innerText.trim();
                  var cls=v==='High'?'tier-H':v==='Mid'?'tier-M':'tier-L';
                  cell.innerHTML='<span class=\"'+cls+'\">'+v+'</span>';
                });"))})

  # CPI
  output[[ns("infl_kpi")]] <- renderUI({
    co <- input[[ns("infl_county")]]
    t25 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2025]
    t14 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2014]
    nom_g <- (t25/t14-1)*100
    cpi_g <- (CPI_DF$cpi[CPI_DF$year==2025]/CPI_DF$cpi[CPI_DF$year==2014]-1)*100
    div(
      div(class="kpi-label","Nominal Growth"),
      div(class=paste("kpi-val",co_cls(co)), paste0("+",round(nom_g,1),"%")), br(),
      div(class="kpi-label","CPI Growth (est.)"),
      div(style="font-weight:700;color:#ef4444;font-family:'IBM Plex Mono',monospace;",
          paste0("+",round(cpi_g,1),"%")), br(),
      div(class="kpi-label","Real Growth"),
      div(style="font-weight:700;color:#10b981;font-family:'IBM Plex Mono',monospace;",
          paste0("+",round(nom_g-cpi_g,1),"% above inflation")))})

  output[[ns("p_inflation")]] <- renderPlotly({
    co <- input[[ns("infl_county")]]
    d  <- g$county_yr %>% filter(county==co)
    plot_ly(d) %>%
      add_trace(x=~year, y=~total,      name="Nominal",        type="scatter", mode="lines+markers",
                line=list(color=COL[co],width=2.5),
                marker=list(color=COL[co],size=7,line=list(color="#060c18",width=2))) %>%
      add_trace(x=~year, y=~real_total, name="Real (2014 KES)", type="scatter", mode="lines+markers",
                line=list(color="#8b5cf6",width=2,dash="dash"),
                marker=list(color="#8b5cf6",size=7)) %>%
      dark_layout(paste(co,"— Nominal vs CPI-Adjusted"),
                  "Dashed = purchasing power adjusted to 2014 KES",ylab="Total (KES)")})
  output[[ns("p_real_comp")]] <- renderPlotly({
    cpi_g <- (CPI_DF$cpi[CPI_DF$year==2025]/CPI_DF$cpi[CPI_DF$year==2014]-1)*100
    nom_g <- sapply(TARGET, function(co) {
      t25 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2025]
      t14 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2014]
      (t25/t14-1)*100
    })
    plot_ly(x=TARGET) %>%
      add_trace(y=nom_g,      name="Nominal Growth",       type="bar",
                marker=list(color=paste0(sapply(TARGET,function(co)COL[co]),"99"))) %>%
      add_trace(y=nom_g-cpi_g, name="Real Growth (CPI-adj)", type="bar",
                marker=list(color="rgba(139,92,246,.6)")) %>%
      layout(barmode="group") %>%
      dark_layout("Nominal vs Real Growth 2014–2025","All counties outpaced inflation",ylab="%")})
  output[[ns("p_cpi_line")]] <- renderPlotly({
    cpi_idx <- CPI_DF %>% mutate(idx=cpi/cpi[1]*100)
    p <- plot_ly() %>%
      add_trace(data=cpi_idx, x=~year, y=~idx, name="Kenya CPI",
                type="scatter", mode="lines",
                line=list(color="#ef4444",width=1.8,dash="dash"))
    for (co in TARGET) {
      d <- g$county_yr %>% filter(county==co) %>% mutate(idx=total/first(total)*100)
      p <- add_trace(p, data=d, x=~year, y=~idx, name=co,
                     type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2),
                     marker=list(color=COL[co],size=5))}
    dark_layout(p,"Allocation vs CPI Index (2014 = 100)","Above CPI line = real growth",ylab="Index")})
  output[[ns("infl_insights")]] <- renderUI({
    cpi_g <- (CPI_DF$cpi[CPI_DF$year==2025]/CPI_DF$cpi[CPI_DF$year==2014]-1)*100
    rows <- sapply(TARGET, function(co) {
      t25 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2025]
      t14 <- g$county_yr$total[g$county_yr$county==co & g$county_yr$year==2014]
      nom <- (t25/t14-1)*100
      paste0("<b>",co,"</b>: Nominal +",round(nom,1),"% → Real +",round(nom-cpi_g,1),"% above inflation")})
    tagList(
      div(class="insight-box warn",
        div(class="insight-title","CPI vs Allocation Growth (2014–2025)"),
        div(class="insight-body",
            HTML(paste0("Kenya CPI rose ~<b>",round(cpi_g,0),"%</b>.<br>",paste(rows,collapse="<br>")))),
        div(class="insight-verdict","✓ All counties outpaced inflation — real purchasing power grew")))})

  # National Ranking
  make_rank_kpi <- function(co) {
    rank  <- g$nat_ranking$rank[g$nat_ranking$county==co]
    t25   <- g$nat_ranking$total_2025[g$nat_ranking$county==co]
    pctile <- round((1-rank/47)*100)
    avg   <- g$nat_ranking$avg_per_const[g$nat_ranking$county==co]
    div(class="kpi-box",
      div(class="kpi-label",co),
      div(class=paste("kpi-val",co_cls(co)), paste("#",rank,"/ 47")),
      div(class="kpi-sub", paste0(fmtB(t25)," · ",(rank-1)," above · ",(47-rank)," below")),
      br(),
      fluidRow(
        column(6, div(class="kpi-label","Percentile"),
               div(style="font-family:'IBM Plex Mono',monospace;font-weight:700;",paste0(pctile,"th"))),
        column(6, div(class="kpi-label","Avg/Const"),
               div(style="font-family:'IBM Plex Mono',monospace;font-weight:600;",
                   format(avg,big.mark=",")))))}
  output[[ns("rank_kpi1")]] <- renderUI(make_rank_kpi(TARGET[1]))
  output[[ns("rank_kpi2")]] <- renderUI(make_rank_kpi(TARGET[2]))
  output[[ns("rank_kpi3")]] <- renderUI(make_rank_kpi(TARGET[3]))

  output[[ns("p_all_counties")]] <- renderPlotly({
    d <- g$nat_ranking %>%
      arrange(total_2025) %>%
      mutate(county_f=factor(county,levels=county),
             bar_col=ifelse(county %in% TARGET, COL[county], "#1e3050"))
    # handle named vector lookup safely
    d$bar_col <- sapply(d$county, function(co) if (co %in% TARGET) COL[co] else "#1e3050")
    plot_ly(d, x=~total_2025/1e9, y=~county_f, type="bar", orientation="h",
            marker=list(color=~bar_col),
            text=~paste0("<b>",county,"</b><br>Rank #",rank,"<br>",fmtB(total_2025)),
            hoverinfo="text") %>%
      dark_layout("All 47 Counties — 2025 NGCDF Ranking","Target counties highlighted",xlab="KES Billion")})
  output[[ns("rank_table")]] <- renderDT({
    g$nat_ranking %>%
      select(Rank=rank, County=county, Constituencies=n_const,
             `2025 Total`=total_2025, `Avg/Constituency`=avg_per_const, Target=is_target) %>%
      mutate(across(c(`2025 Total`,`Avg/Constituency`), ~format(round(.),big.mark=",")),
             Target=ifelse(Target,"✅","")) %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=20, scrollX=TRUE, order=list(list(0,"asc"))))
  })

  # Neighbours
  nb_yr_grp <- reactive({
    df_all %>%
      filter(county %in% SHOW_CO) %>%
      pivot_longer(all_of(YEARS), names_to="year", values_to="alloc") %>%
      mutate(year=as.integer(year)) %>%
      filter(!is.na(alloc)) %>%
      group_by(county, year) %>%
      summarise(total=sum(alloc), n=n(), .groups="drop")})

  output[[ns("p_nb_bar")]] <- renderPlotly({
    d <- nb_yr_grp() %>% filter(year==2025) %>%
      arrange(desc(total)) %>%
      mutate(county_f=factor(county,levels=county))
    d$bar_col <- sapply(d$county, function(co) if (co %in% TARGET) COL[co] else "#1e3050")
    plot_ly(d, x=~total/1e9, y=~county_f, type="bar", orientation="h",
            marker=list(color=~bar_col),
            text=~paste0("<b>",county,"</b><br>",fmtB(total)), hoverinfo="text") %>%
      dark_layout("Target vs Neighbouring Counties — 2025","Coloured = target",xlab="KES Billion")})

  output[[ns("p_nb_line")]] <- renderPlotly({
    p <- plot_ly()
    for (co in SHOW_CO) {
      d <- nb_yr_grp() %>% filter(county==co)
      if (nrow(d)==0) next
      is_tgt <- co %in% TARGET
      p <- add_trace(p, data=d, x=~year, y=~total, name=co,
                     type="scatter", mode=ifelse(is_tgt,"lines+markers","lines"),
                     line=list(color=COL[co], width=ifelse(is_tgt,2.5,1.5),
                               dash=ifelse(is_tgt,"solid","dash")),
                     marker=if(is_tgt) list(color=COL[co],size=6,
                                             line=list(color="#060c18",width=2)) else list())}
    dark_layout(p,"Allocation Trends — Target + Neighbours","Solid = target",ylab="Total (KES)")})

  output[[ns("nb_table")]] <- renderDT({
    nb_yr_grp() %>%
      pivot_wider(names_from=year, values_from=c(total,n)) %>%
      select(county, n=n_2025, t14=total_2014, t25=total_2025) %>%
      mutate(growth=round((t25/t14-1)*100,1),
             type=ifelse(county %in% TARGET,"Target","Neighbour")) %>%
      left_join(g$nat_ranking %>% select(county,rank), by="county") %>%
      arrange(desc(t25)) %>%
      select(County=county, Type=type, Constituencies=n,
             `2014 Total`=t14, `2025 Total`=t25, `Growth %`=growth, `National Rank`=rank) %>%
      mutate(across(c(`2014 Total`,`2025 Total`), ~format(round(.),big.mark=","))) %>%
      datatable(rownames=FALSE, options=list(dom="t", pageLength=20))})

  # Data Table
  output[[ns("data_table")]] <- renderDT({
    yr_range <- as.character(seq(as.integer(input[[ns("dt_year_from")]]),
                                  as.integer(input[[ns("dt_year_to")]])))
    d <- if (input[[ns("dt_county")]]=="All") g$df else g$df %>% filter(county==input[[ns("dt_county")]])
    d %>%
      select(County=county, Constituency=constituency, Voters=voters, all_of(yr_range)) %>%
      mutate(across(all_of(yr_range), ~format(round(.),big.mark=","))) %>%
      datatable(rownames=FALSE, filter="top", options=list(pageLength=20,scrollX=TRUE))})}

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # Run server logic for each group
  group_server(input, output, session, "A", GROUP_A, NEIGHBOURS_A, gA)
  group_server(input, output, session, "B", GROUP_B, NEIGHBOURS_B, gB)

  # Cross-group plots
  cy_all <- bind_rows(gA$county_yr, gB$county_yr)
  output$p_cross_trends <- renderPlotly({
    p <- plot_ly()
    for (co in ALL_TARGET) {
      d <- cy_all %>% filter(county==co)
      grp <- if (co %in% GROUP_A) "solid" else "dash"
      p <- add_trace(p, data=d, x=~year, y=~total, name=co,
                     type="scatter", mode="lines+markers",
                     line=list(color=COL[co],width=2.2,dash=grp),
                     marker=list(color=COL[co],size=6,line=list(color="#060c18",width=1.5)))}
    dark_layout(p,"All 6 Counties — Allocation Trends 2014–2026",
                "Solid = Rift Valley · Dashed = Central",ylab="Total (KES)")})
  output$p_cross_bar <- renderPlotly({
    d <- cy_all %>%
      filter(year==2025) %>%
      mutate(group=ifelse(county %in% GROUP_A,"Rift Valley","Central")) %>%
      arrange(desc(total)) %>%
      mutate(county_f=factor(county,levels=county))
    plot_ly(d, x=~total/1e9, y=~county_f, type="bar", orientation="h",
            color=~county, colors=COL[ALL_TARGET],
            text=~paste0("<b>",county,"</b><br>",fmtB(total),"<br>",group),
            hoverinfo="text") %>%
      dark_layout("2025 Allocation — All 6 Counties","Side-by-side comparison",xlab="KES Billion")})
  output$p_cross_pervoter <- renderPlotly({
    p <- plot_ly()
    for (co in ALL_TARGET) {
      d <- cy_all %>% filter(county==co)
      p <- add_trace(p, data=d, x=~year, y=~per_voter, name=co,
                     type="scatter", mode="lines",
                     line=list(color=COL[co],width=2,
                               dash=ifelse(co %in% GROUP_A,"solid","dash")))}
    dark_layout(p,"Per-Voter Allocation — All 6 Counties",
                "Solid = Rift Valley · Dashed = Central",ylab="KES per Voter")})
  output$p_cross_ranking <- renderPlotly({
    combined_ranks <- bind_rows(
      gA$nat_ranking %>% mutate(group="Rift Valley"),
      gB$nat_ranking %>% mutate(group="Central")
    ) %>%
      filter(county %in% ALL_TARGET) %>%
      distinct(county, .keep_all=TRUE) %>%
      arrange(rank)
    plot_ly(combined_ranks,
            x=~total_2025/1e9, y=~reorder(county,total_2025),
            type="bar", orientation="h",
            marker=list(color=sapply(combined_ranks$county, function(co) COL[co])),
            text=~paste0("<b>",county,"</b><br>Rank #",rank,"/47"),
            hoverinfo="text") %>%
      dark_layout("National Rank — All 6 Target Counties","2025 NGCDF allocation",xlab="KES Billion")})}

shinyApp(ui, server)
