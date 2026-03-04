library(shiny)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(sf)
library(tigris)
library(viridis)

tariff_path <- "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv"
unmatched_path <- "./output/tariff_by_industry/qa/unmatched_hs_codes.csv"
panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
panel_finance_path <- "./data/public/derived/county_tariff_panel_with_campaign_finance.csv"

if (!file.exists(tariff_path)) {
  stop("Missing mapped tariff file. Run clean_tariff_retaliation.R first.")
}

if (!file.exists(panel_path)) {
  stop("Missing county panel. Run code/overhaul_open_data_panel.R first.")
}

tariff <- read_csv(
  tariff_path,
  show_col_types = FALSE,
  col_types = cols(
    hs8_raw = col_character(),
    hs_digits = col_character(),
    hs8 = col_character(),
    hs6 = col_character(),
    naics6 = col_character(),
    mapping_level = col_character(),
    naics2 = col_character(),
    naics3 = col_character(),
    naics4 = col_character(),
    tariff = col_character(),
    Country = col_character(),
    desc = col_character()
  )
) %>%
  mutate(
    tariff_pct = readr::parse_number(tariff),
    mapping_level = if_else(is.na(mapping_level), "unmapped", mapping_level),
    naics2 = if_else(is.na(naics2), "unmapped", str_pad(naics2, width = 2, side = "left", pad = "0"))
  )

unmatched <- if (file.exists(unmatched_path)) {
  read_csv(
    unmatched_path,
    show_col_types = FALSE,
    col_types = cols(
      hs8_raw = col_character(),
      hs8 = col_character(),
      hs6 = col_character(),
      desc = col_character(),
      n_rows = col_double()
    )
  )
} else {
  tibble::tibble(hs8_raw = character(), hs8 = character(), hs6 = character(), desc = character(), n_rows = integer())
}

panel_base <- if (file.exists(panel_finance_path)) {
  read_csv(panel_finance_path, show_col_types = FALSE)
} else {
  read_csv(panel_path, show_col_types = FALSE)
}

panel <- panel_base %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    county_tariff_exposure_per_worker = as.numeric(county_tariff_exposure_per_worker),
    tariffed_emp_share = as.numeric(tariffed_emp_share),
    gop_share_20 = as.numeric(gop_share_20),
    gop_share_24 = as.numeric(gop_share_24),
    swingness_20 = as.numeric(swingness_20),
    political_salience_weighted_exposure = as.numeric(political_salience_weighted_exposure)
  )

metric_choices <- c(
  "Exposure (Level)" = "county_tariff_exposure_mean",
  "Exposure (Per Worker)" = "county_tariff_exposure_per_worker",
  "Tariffed Employment Share" = "tariffed_emp_share",
  "Political Salience Index" = "political_salience_weighted_exposure",
  "GOP Share (2020)" = "gop_share_20",
  "GOP Share (2024)" = "gop_share_24",
  "Competitiveness (2020)" = "swingness_20"
)

if ("donations_total" %in% names(panel)) {
  metric_choices <- c(
    metric_choices,
    "Campaign Donations" = "donations_total",
    "Campaign Spending" = "spending_total",
    "Net Campaign Flow" = "net_finance_flow",
    "Donations Per Worker" = "donations_per_worker"
  )
}

scatter_x_choices <- c(
  "GOP Share (2020)" = "gop_share_20",
  "GOP Share (2024)" = "gop_share_24",
  "Competitiveness (2020)" = "swingness_20",
  "Tariffed Employment Share" = "tariffed_emp_share"
)

if ("donations_total" %in% names(panel)) {
  scatter_x_choices <- c(
    scatter_x_choices,
    "Campaign Donations (log)" = "log_donations",
    "Campaign Spending (log)" = "log_spending"
  )
}

mapping_levels <- sort(unique(tariff$mapping_level))
naics2_choices <- c("all", sort(unique(tariff$naics2)))
country_choices <- c("all", sort(unique(tariff$Country)))
year_choices <- sort(unique(panel$year))

options(tigris_use_cache = TRUE)
counties_sf <- tryCatch(
  tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
    st_transform(5070) %>%
    select(GEOID, STATEFP, geometry),
  error = function(e) NULL
)
states_sf <- tryCatch(
  tigris::states(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
    st_transform(5070) %>%
    select(STATEFP, geometry),
  error = function(e) NULL
)
exclude_statefp <- c("02", "15", "60", "66", "69", "72", "78")

choice_label <- function(choices, value) {
  idx <- which(choices == value)
  if (length(idx) == 0) {
    return(value)
  }
  names(choices)[idx[1]]
}

ui <- fluidPage(
  titlePanel("Retaliatory Tariff Explorer"),
  tabsetPanel(
    tabPanel(
      "County Explorer",
      sidebarLayout(
        sidebarPanel(
          helpText("Core measure: county allocated exports x tariff severity, summed across sectors."),
          selectInput("year_pick", "Year", choices = year_choices, selected = max(year_choices)),
          selectInput("metric_pick", "Map Metric", choices = metric_choices, selected = "county_tariff_exposure_mean"),
          selectInput("scatter_x", "Scatter X", choices = scatter_x_choices, selected = "gop_share_20"),
          sliderInput("top_n", "Top counties in table", min = 10, max = 100, value = 40, step = 5),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(4, wellPanel(h4("Counties"), textOutput("county_count"))),
            column(4, wellPanel(h4("Median Exposure"), textOutput("median_exposure"))),
            column(4, wellPanel(h4("Vote Coverage"), textOutput("vote_coverage")))
          ),
          tabsetPanel(
            tabPanel("Map", plotOutput("county_map", height = 620)),
            tabPanel("Scatter", plotOutput("exposure_scatter", height = 460)),
            tabPanel("Top Counties", DTOutput("top_table")),
            tabPanel("Filtered Data", DTOutput("panel_table"))
          )
        )
      )
    ),
    tabPanel(
      "Tariff Mapping QA",
      sidebarLayout(
        sidebarPanel(
          sliderInput("tariff_range", "Tariff range (%)", min = 0, max = 300, value = c(0, 100), step = 1),
          checkboxGroupInput("mapping_level", "Mapping level", choices = mapping_levels, selected = mapping_levels),
          selectInput("naics2", "NAICS 2-digit", choices = naics2_choices, selected = "all"),
          selectInput("country", "Country", choices = country_choices, selected = "all"),
          checkboxInput("exclude_unmapped", "Exclude unmapped rows", value = TRUE),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(4, wellPanel(h4("Rows"), textOutput("n_rows"))),
            column(4, wellPanel(h4("Mapped Share"), textOutput("mapped_share"))),
            column(4, wellPanel(h4("Unmatched HS8"), textOutput("n_unmatched")))
          ),
          tabsetPanel(
            tabPanel("Charts", plotOutput("plot_mapping_levels", height = 280), plotOutput("plot_naics2", height = 380)),
            tabPanel("Filtered Rows", DTOutput("rows_table")),
            tabPanel("Unmatched QA", DTOutput("unmatched_table"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  county_filtered <- reactive({
    panel %>% filter(year == input$year_pick)
  })

  county_joined <- reactive({
    if (is.null(counties_sf)) {
      return(NULL)
    }

    counties_sf %>%
      filter(!STATEFP %in% exclude_statefp) %>%
      left_join(county_filtered(), by = c("GEOID" = "county_fips"))
  })

  output$county_count <- renderText({
    format(n_distinct(county_filtered()$county_fips), big.mark = ",")
  })

  output$median_exposure <- renderText({
    med <- median(county_filtered()$county_tariff_exposure_mean, na.rm = TRUE)
    label_number(scale_cut = cut_short_scale())(med)
  })

  output$vote_coverage <- renderText({
    df <- county_filtered()
    cov <- mean(!is.na(df$gop_share_20))
    percent(cov, accuracy = 0.1)
  })

  output$county_map <- renderPlot({
    map_df <- county_joined()
    req(!is.null(map_df))
    req(!is.null(states_sf))

    metric <- input$metric_pick
    states_map <- states_sf %>% filter(!STATEFP %in% exclude_statefp)
    metric_label <- choice_label(metric_choices, metric)
    values <- map_df[[metric]]

    if (metric == "net_finance_flow") {
      ggplot(map_df) +
        geom_sf(aes(fill = .data[[metric]]), color = NA) +
        geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
        scale_fill_gradient2(
          low = "#2166ac",
          mid = "#f7f7f7",
          high = "#b2182b",
          midpoint = 0,
          na.value = "grey92",
          labels = label_number(scale_cut = cut_short_scale())
        ) +
        labs(
          title = paste0("County map: ", metric_label, " (", input$year_pick, ")"),
          fill = NULL
        ) +
        theme_void()
    } else if (all(values >= 0, na.rm = TRUE) && max(values, na.rm = TRUE) > 1) {
      ggplot(map_df) +
        geom_sf(aes(fill = .data[[metric]] + 1), color = NA) +
        geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
        scale_fill_viridis_c(
          option = "magma",
          trans = "log10",
          na.value = "grey92",
          labels = label_number(scale_cut = cut_short_scale())
        ) +
        labs(
          title = paste0("County map: ", metric_label, " (", input$year_pick, ")"),
          fill = NULL
        ) +
        theme_void()
    } else {
      ggplot(map_df) +
        geom_sf(aes(fill = .data[[metric]]), color = NA) +
        geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
        scale_fill_viridis_c(option = "plasma", na.value = "grey92", labels = label_number()) +
        labs(
          title = paste0("County map: ", metric_label, " (", input$year_pick, ")"),
          fill = NULL
        ) +
        theme_void()
    }
  })

  output$exposure_scatter <- renderPlot({
    x_var <- input$scatter_x
    df <- county_filtered() %>%
      filter(!is.na(.data[[x_var]]), county_tariff_exposure_mean > 0)

    ggplot(df, aes(x = .data[[x_var]], y = log10(county_tariff_exposure_mean + 1))) +
      geom_point(alpha = 0.4, color = "#1b7837") +
      geom_smooth(method = "lm", se = TRUE, color = "#762a83", linewidth = 0.9) +
      labs(
        title = paste0("Exposure relationship in ", input$year_pick),
        x = choice_label(scatter_x_choices, x_var),
        y = "log10(county exposure + 1)"
      ) +
      theme_minimal(base_size = 12)
  })

  output$top_table <- renderDT({
    metric <- input$metric_pick
    county_filtered() %>%
      mutate(metric_value = .data[[metric]]) %>%
      arrange(desc(metric_value)) %>%
      select(
        county_fips, county_name, state_name, metric_value,
        county_tariff_exposure_mean, county_tariff_exposure_per_worker,
        tariffed_emp_share, gop_share_20, gop_share_24, swingness_20
      ) %>%
      slice_head(n = input$top_n)
  }, options = list(pageLength = 15, scrollX = TRUE))

  output$panel_table <- renderDT({
    county_filtered() %>%
      select(
        county_fips, county_name, state_name, year,
        county_tariff_exposure_mean, county_tariff_exposure_per_worker,
        tariffed_emp_share, gop_share_20, gop_share_24, swingness_20
      )
  }, options = list(pageLength = 20, scrollX = TRUE))

  filtered <- reactive({
    df <- tariff %>%
      filter(
        tariff_pct >= input$tariff_range[1],
        tariff_pct <= input$tariff_range[2],
        mapping_level %in% input$mapping_level
      )

    if (isTRUE(input$exclude_unmapped)) {
      df <- df %>% filter(mapping_level != "unmapped")
    }

    if (input$naics2 != "all") {
      df <- df %>% filter(naics2 == input$naics2)
    }

    if (input$country != "all") {
      df <- df %>% filter(Country == input$country)
    }

    df
  })

  output$n_rows <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })

  output$mapped_share <- renderText({
    df <- filtered()
    if (nrow(df) == 0) {
      return("0.0%")
    }
    mapped <- sum(df$mapping_level != "unmapped")
    sprintf("%.2f%%", 100 * mapped / nrow(df))
  })

  output$n_unmatched <- renderText({
    format(nrow(unmatched), big.mark = ",")
  })

  output$plot_mapping_levels <- renderPlot({
    filtered() %>%
      count(mapping_level, sort = TRUE) %>%
      ggplot(aes(x = reorder(mapping_level, n), y = n, fill = mapping_level)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Rows", title = "Rows by mapping level") +
      theme_minimal(base_size = 12)
  })

  output$plot_naics2 <- renderPlot({
    filtered() %>%
      filter(naics2 != "unmapped") %>%
      count(naics2, sort = TRUE) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = reorder(naics2, n), y = n)) +
      geom_col(fill = "#2b8cbe") +
      coord_flip() +
      labs(x = "NAICS 2-digit", y = "Rows", title = "Top NAICS 2-digit sectors") +
      theme_minimal(base_size = 12)
  })

  output$rows_table <- renderDT({
    filtered() %>%
      select(hs8_raw, hs8, hs6, Country, tariff, tariff_pct, naics6, naics2, mapping_level, desc) %>%
      arrange(desc(tariff_pct))
  }, options = list(pageLength = 20, scrollX = TRUE))

  output$unmatched_table <- renderDT({
    unmatched
  }, options = list(pageLength = 20, scrollX = TRUE))
}

shinyApp(ui, server)
