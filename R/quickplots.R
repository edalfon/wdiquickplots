#' Download WDI data and prepare the dataset for the plots
#'
#' Download data using a memoised version of WDI::WDI() and wrangle a bit
#' with the data to prepare a data.frame for the plots#'
#'
#' @param indicator character of length 1 with the indicator code
#' @param highlight_countries character vector with country names to highlight
#' @param start first year to download data
#' @param end last year to download data
#' @param country subset of countries to download data from. Default to "all".
#'                This argument is passed as is to WDI::WDI and hence, requires
#'                'ISO-2 character codes, e.g. "BR", "US", "CA"'.
#' @param regions character vector to filter the data only to specific regions
#' @param income_groups character vector to filter the data only to specific
#'                      income groups
#' @param interpolate boolean to indicate whether to interpolate missing values
#'
#' @return data.frame with columns country, year, ind_1, region, income,
#'         highlight_ind_1. highlight_ind_1 = NA, except for highlight
#'         countries in which case, it takes the value of ind_1
#'
#' @importFrom tidyr drop_na
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' download_wdi("NY.GDP.PCAP.KD", c("Colombia", "Germany"), 2019, 2019)
#' }
#'
download_wdi <- function(indicator = "NY.GDP.PCAP.KD",
                             highlight_countries = c(""),
                             start = lubridate::year(Sys.Date()) - 10,
                             end = lubridate::year(Sys.Date() - months(18)),
                             country = "all",
                             regions = default_regions(),
                             income_groups = default_income_groups(),
                             interpolate = FALSE) {

  year <- ind_1 <- region <- income <- is_highlight <- NULL # or use the .data pronoun

  regions <- match.arg(regions, several.ok = TRUE)
  income_groups <- match.arg(income_groups, several.ok = TRUE)

  wdi_raw <- memoised_wdi(
    country = country,
    # named vector will be renamed, thx WDI
    indicator = setNames(indicator, paste0("ind_", 1:length(indicator))),
    start = start,
    end = end,
    extra = TRUE # to always get region and income level
  )

  wdi_data <- wdi_raw %>%
    select(country, year, region, income, dplyr::starts_with("ind_"))

  if (isTRUE(interpolate)) {
    wdi_data <- interpolate_wdi(wdi_data)
  }

  wdi_data <- wdi_data %>%
    filter(region != "Aggregates") %>%
    filter(region %in% regions) %>%
    filter(income %in% income_groups) %>%
    tidyr::drop_na(dplyr::starts_with("ind_")) %>%
    mutate(is_highlight = country %in% highlight_countries) %>%
    mutate(highlighted_country = ifelse(is_highlight, country, NA)) %>%
    mutate(dplyr::across(
      .cols = dplyr::starts_with("ind_"),
      .fns = ~ dplyr::case_when(country %in% highlight_countries ~ .x),
      .names = "highlight_{.col}"
    )) %>%
    mutate(income = factor(income, levels = c(
      "Aggregates", "High income", "Upper middle income", "Lower middle income",
      "Low income", NA
    )))

  wdi_data
}

memoised_wdi <- memoise::memoise(
  f = WDI::WDI,
  cache = memoise::cache_filesystem("~/wdiquickplots_cache")
)


#' Get latest data per country
#'
#' @inheritParams download_wdi
#'
#' @return data.frame with columns country, year, ind_1, region, income,
#'         highlight_ind_1. highlight_ind_1 = NA, except for highlight
#'         countries in which case, it takes the value of ind_1
#'
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' \dontrun{
#' latest_wdi("NY.GDP.PCAP.KD", c("Colombia", "Germany"))
#' latest_wdi("NY.GDP.PCAP.KD", c("Colombia", "Germany"), 2019, 2019)
#' }
#'
latest_wdi <- function(indicator = "NY.GDP.PCAP.KD",
                           highlight_countries = c(""),
                           start = lubridate::year(Sys.Date()) - 10,
                           end = lubridate::year(Sys.Date() - months(18)),
                           country = "all",
                           regions = default_regions(),
                           income_groups = default_income_groups()) {

  year <- NULL # or use the .data pronoun

  wdi_data <- download_wdi(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  wdi_data %>%
    group_by(country) %>%
    dplyr::slice_max(order_by = year) %>%
    ungroup()
}

#' Plot a WDI indicator
#'
#' @param wdi_data data.frame as returned by latest_wdi
#' @param ind variable in wdi_data to plot
#' @param facets variable in wdi_data to use as facets (either region or income)
#' @param country variable in wdi_data to use as country names
#' @param highlight variable in wdi_data with the value of countries to highglight
#' @param year variable in wdi_data with the value of the year for each point
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#'
#' @noRd
plot_dist_wdi_ggpdef <- function(wdi_data, ind, facets, country, highlight,
                                     year, p = 1) {

  # wdi_data <- wdi_data %>%
  #   group_by({{facets}}) %>%
  #   mutate(custom_hjust = scales::rescale(-{{highlight}}, to = c(0, 1))) %>%
  #   ungroup()

  all_years <- vctrs::vec_count(wdi_data %>% pull({{ year }}))$key

  the_plot <- ggplot(aes(x = {{ ind }}, fill = {{ facets }}), data = wdi_data) +
    facet_wrap(vars({{ facets }}), ncol = 1, scales = "free_y") +
    geom_density(alpha = 0.7, color = NA, adjust = 0.25) +
    geom_rug() +
    geom_vline(aes(xintercept = {{ highlight }}), linetype = "dotted") +
    ggrepel::geom_text_repel(
      aes(
        x = {{ highlight }},
        y = Inf,
        # using custom_hjust, two highlights in a facet would never be in
        # the line center, so better let them just repel
        # hjust = custom_hjust,
        label = paste0(
          {{ country }}, "\n",
          # tailor the scale function using all the data in {ind} but apply it
          # only to highlight data
          get_formatter(pull(wdi_data, {{ ind }}))({{ highlight }})
        )
      ),
      direction = "x", # only let ggrepel to adjust horizontally
      point.padding = NA, # never repel if there is only 1 highlight in a facet
      ylim = c(NA, Inf), # do not repel from top edge, let vjust do the work
      vjust = 1,
      hjust = 0.5,
      lineheight = 0.75,
      fontface = "bold"
    ) +
    # coord_cartesian(clip = "off") +
    scale_y_continuous("Density", expand = expansion(mult = c(0, 0.25))) +
    #' There is an issue here, if put in separate geoms, vjust can end up
    #' being inconsistent, so let's put them together in one geom even though
    #' I would have preferred two separate geoms with y = 0 and y = Inf
    # ggrepel::geom_text_repel(
    #   aes(
    #     x = {{ highlight }},
    #     y = 0,
    #     label = get_formatter(pull(wdi_data, {{ ind }}))({{ highlight }})
    #   ),
    #   vjust = -0.1,
    #   hjust = 0,
  #   fontface = "bold"
  # ) +
    ggthemes::theme_tufte() +
    scale_x_continuous(
      name = paste0(
        attr(wdi_data %>% pull({{ ind }}), "label"), "\nYear ",
        all_years[[1]], ifelse(length(all_years) > 1, " *", "")
        # TODO: find better way to signal a few other years are there as well
      ),
      trans = scales::modulus_trans(p),
      labels = get_formatter(pull(wdi_data, {{ ind }})),
      breaks = modulus_breaks(p),
      guide = guide_axis(check.overlap = TRUE),
      expand = c(0, 0)
    ) +
    scale_fill_brewer(palette = "Dark2") +
    # scale_fill_brewer(
    #   palette = ifelse(
    #     test = "income" %in% names(select(wdi_data, {{ facets }})),
    #     yes = "Blues",
    #     no = "Dark2"),
    #   direction = -1
    # ) +
    theme(legend.position = "none") +
    #ylab("Density") +
    labs(caption = ifelse(p != 1, glue::glue("Transformed scale modulus({p})"), ""))

  if (any("income" %in% names(select(wdi_data, {{ facets }})))) {
    the_plot <- the_plot +
      ggplot2::scale_fill_discrete(
        type = rev(RColorBrewer::brewer.pal(n = 7, name = 'Blues'))
      )
  }

  the_plot
}

#' Plot the distribution (density) of a single indicator using WDI data,
#' downloading the data as necessary for the period indicated by start and end,
#' and keeping only the latest data point available for each country
#'
#' @inheritParams download_wdi
#' @param facets whether to show facets per region or income
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dist_wdi()
#' }
plot_dist_wdi <- function(indicator = "NY.GDP.PCAP.KD",
                              highlight_countries = c("Colombia", "Germany"),
                              facets = region,
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date() - months(18)),
                              country = "all",
                              regions = default_regions(),
                              income_groups = default_income_groups(),
                              p = 1) {

  ind_1 <- region <- highlight_ind_1 <- year <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi(indicator, highlight_countries, start, end,
                             country, regions, income_groups)

  plot_dist_wdi_ggpdef(wdi_data, ind_1, {{ facets }}, country, highlight_ind_1,
                           year, p)
}

#' Plot a WDI indicator as an interactive bar-plot (powered by plotly)
#'
#' @inheritParams download_wdi
#' @param base_color fill color for the bars for most countries
#' @param highlight_color fill color for the bars for highlighted countries
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bar_wdi()
#' }
plot_bar_wdi <- function(indicator = "NY.GDP.PCAP.KD",
                             highlight_countries = c("Colombia", "Germany"),
                             start = lubridate::year(Sys.Date()) - 10,
                             end = lubridate::year(Sys.Date() - months(18)),
                             country = "all",
                             regions = default_regions(),
                             income_groups = default_income_groups(),
                             base_color = "skyblue",
                             highlight_color = "red") {

  # TODO: refactor plot
  # TODO: let customize transformed scales (currently using plotly's auto mode)
  # TODO: apply transformation. Perhaps use a button or dropdown to let the
  #       user customize the transformation parameter p interactively
  #       https://plotly.com/r/dropdowns/
  # TODO: let the user decide whether horizontal or vertical bar plot
  # https://plotly.com/r/reference/

  ind_1 <- region <- highlight_ind_1 <- year <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi(indicator, highlight_countries, start, end, country,
                         regions, income_groups)

  all_years <- vctrs::vec_count(wdi_data$year)$key

  custom_formatter <- get_formatter(wdi_data$ind_1)

  wdi_data <- wdi_data %>%
    mutate(text = dplyr::case_when(
      is_highlight ~ paste0("<b>", country, " [",
                            custom_formatter(highlight_ind_1), "]</b>")
    )) %>%
    mutate(color = dplyr::case_when(is_highlight ~ highlight_color,
                                    TRUE ~ base_color))

  plotly::plot_ly(
    data = wdi_data,
    x = ~ind_1,
    y = ~reorder(country, ind_1),
    text = ~text,
    textposition = 'outside',
    textfont = list(face = "bold", color = '#000000', size = 14),
    hovertemplate = ~paste0(country, "<br>", custom_formatter(ind_1)),
    # https://github.com/d3/d3-3.x-api-reference/blob/master/Formatting.md#d3_format
    type = "bar",
    orientation = 'v',
    marker = list(color = ~color)
  ) %>%
    plotly::layout(
      # using textfont above would not let you override text size limit
      # https://stackoverflow.com/questions/62094773/is-it-possible-to-override
      # -font-size-limit-on-labels-in-r-plotly-bar-charts
      uniformtext = list(minsize  =14, mode = "show"),
      xaxis = list(title = paste0(
        attr(wdi_data$ind_1, "label"), "\nYear ",
        all_years[[1]], ifelse(length(all_years) > 1, " *", "")
        # TODO: find better way to signal a few other years are there as well
      )),
      yaxis = list(title = NA, tickfont = list(size = 7))
    )
}

#' Line plot with facets showing how the indicator has changed over time
#' in the highlight_countries, within regions or income groups (facets)
#'
#' We use the super cool gghighlight::gghighlight package to disentangle the
#' spaghetti plot and highlight selected countries in each facet
#'
#' @inheritParams download_wdi
#' @param facets variable to use for facets. Either region or income
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_time_facets_wdi()
#' }
plot_time_facets_wdi <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              facets = region,
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date() - months(18)),
                              country = "all",
                              regions = default_regions(),
                              income_groups = default_income_groups(),
                              p = 1) {

  region <- year <- ind_1 <- highlight_ind_1 <- NULL

  wdi_data <- download_wdi(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  ggplot(aes(x = year, y = ind_1, color = country), data = wdi_data) +
    geom_point() +
    geom_line(aes(group = country), size = 1.5) +
    facet_wrap(vars({{facets}})) +
    gghighlight::gghighlight(
      !is.na(highlight_ind_1),
      calculate_per_facet = TRUE,
      use_direct_label = TRUE,
      label_params = list(fill = "white", point.padding = 0.1, direction = "y")
    ) +
    scale_y_continuous(
      name = attr(wdi_data$ind_1, "label"),
      trans = scales::modulus_trans(p),
      labels = get_formatter(wdi_data$ind_1),
      breaks = modulus_breaks(p),
      guide = guide_axis(check.overlap = TRUE),
      expand = c(0, 0)
    ) +
    scale_x_continuous(labels = scales::label_number(1, big.mark = "")) +
    scale_color_brewer(palette = "Set1") +
    xlab("Year") +
    ggthemes::theme_tufte() +
    theme(panel.border = element_rect(colour = "grey", fill = NA)) +
    labs(caption = ifelse(p != 1, glue::glue("Transformed scale modulus({p})"), ""))
}

#' Line plot (plotly-powered) the indicator over time (year) for the
#' highlight_countries only
#'
#' @inheritParams download_wdi
#'
#' @return plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_time_wdi()
#' }
plot_time_wdi <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date() - months(18)),
                              country = "all",
                              regions = default_regions(),
                              income_groups = default_income_groups()) {

  # TODO: allow transformation passing p as in otherss

  year <- ind_1 <- highlight_ind_1 <- NULL

  # TODO: here you could actually download only the highlight countries
  wdi_data <- download_wdi(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  custom_formatter <- get_formatter(wdi_data$ind_1)

  wdi_data <- wdi_data %>%
    filter(!is.na(highlight_ind_1)) %>%
    group_by(country) %>%
    mutate(direct_labels = case_when(
      year == max(year, na.rm = TRUE) ~ paste0(country, "\n", custom_formatter(ind_1)),
      year == min(year, na.rm = TRUE) ~ custom_formatter(ind_1)
    )) %>%
    dplyr::arrange(country, year) # I hate that plotly makes you arrange it

  plotly::plot_ly(
    data = wdi_data,
    x = ~ year,
    y = ~ ind_1,
    # groups and assigns different colors in one step
    color = ~ country,
    # name = 'all_terms',
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 3)
  ) %>%
    plotly::add_text(
      text = ~ direct_labels,
      textfont = list(size = 12),
      textposition = "top"
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      yaxis = list(title = attr(wdi_data %>% pull(ind_1), "label")),
      yaxis = list(title = NA)
    )
}

#' Spaghetti plot that no-one really want to see (nor should want to see, ever)
#'
#' @inheritParams download_wdi
#'
#' @return a dygraphs plot
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spaghetti_wdi()
#' }
plot_spaghetti_wdi <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date() - months(18)),
                              country = "all",
                              regions = default_regions(),
                              income_groups = default_income_groups()) {

  # TODO: allow transformation passing p as in otherss

  year <- ind_1 <- highlight_ind_1 <- region <- income <- NULL

  wdi_data <- download_wdi(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  wdi_data_wide <- wdi_data %>%
    select(country, year, ind_1) %>%
    tidyr::pivot_wider(names_from = country, values_from = ind_1) %>%
    dplyr::arrange(year)

  dy_spaghetti <- dygraphs::dygraph(
    data = wdi_data_wide,
    xlab = "",
    ylab = attr(wdi_data$ind_1, "label")
  ) %>%
    dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2) %>%
    dygraphs::dyHighlight(
      highlightCircleSize = 5,
      highlightSeriesBackgroundAlpha = 0.1,
      highlightSeriesOpts = list(strokeWidth = 5),
      hideOnMouseOut = TRUE
    ) %>%
    dygraphs::dyRangeSelector()
  # to show legend only for highlighted series. Sonst it is even more messy
  dy_spaghetti$x$css <- ".dygraph-legend > span {display: none;}
                         .dygraph-legend > span.highlight {display: inline-flex;}"
  dy_spaghetti
}


#' Race bar plot
#'
#' @inheritParams download_wdi
#' @param p transformation exponent, as in scales::modulus_trans
#'
#' @return gganimate
#' @export
#'
#' @examples
#' \dontrun{
#' plot_race_wdi()
#' }
plot_race_wdi <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 15,
                              end = lubridate::year(Sys.Date() - months(18)),
                              country = "all",
                              regions = default_regions(),
                              income_groups = default_income_groups(),
                              p = 1) {

  year <- ind_1 <- highlight_ind_1 <- region <- income <- highlight_country <-
    highlight_country_label <- highlight_dummy <- ind_1_fill <- NULL

  wdi_data <- download_wdi(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  custom_formatter <- get_formatter(wdi_data$ind_1)

  wdi_race_data <- wdi_data %>%
    # most probably, there will be missing values in some countries for some years
    # so here's a controversial decision to make the animation look good
    # let's fill missing values for each country, by interpolating values in
    # the gaps and filling with the first or last value available
    tidyr::complete(country, year) %>% # not there will be NA in ind_1
    group_by(country) %>%
    arrange(country, year) %>%
    mutate(ind_1_fill = zoo::na.approx(ind_1, na.rm = FALSE)) %>%
    tidyr::fill(ind_1_fill, .direction = "downup") %>%
    mutate(highlight_ind_1 = zoo::na.approx(highlight_ind_1, na.rm = FALSE)) %>%
    tidyr::fill(highlight_ind_1, .direction = "downup") %>%
    ungroup() %>%
    # Try to be transparent about it and signal the country name with an *
    # whenever there was a missing value
    mutate(country_label = ifelse(is.na(ind_1), paste0(country, "*"), country)) %>%
    # leave unhighlighted countries as NA, then the fill color will be grey
    mutate(highlight_country = case_when(!is.na(highlight_ind_1) ~ country)) %>%
    mutate(highlight_country_label = case_when(
      !is.na(highlight_ind_1) ~ paste0(country_label, " (", custom_formatter(highlight_ind_1), ")"),
      TRUE ~ ""
    )) %>%
    mutate(highlight_dummy = case_when(!is.na(highlight_ind_1) ~ 1, TRUE ~ 0.77)) %>%
    # Now calculate country rank in every year, for the animated plot
    group_by(year) %>%
    arrange(year, -ind_1_fill) %>%
    mutate(rank = 1:n()) %>%
    mutate(rank = -rank) %>%
    ungroup()

  anim <- wdi_race_data %>%
    #filter(year %in% 2011:2011) %>%
    #filter(country %in% unique(wdi_race_data$country)[1:30]) %>%
    ggplot() +
    facet_wrap(vars(year)) +
    geom_rect(aes(
      xmin = 0,
      xmax = ind_1_fill,
      ymin = rank - .5,
      ymax = rank + .5,
      fill = highlight_country,
      alpha = highlight_dummy
    )) +
    geom_text(aes(
      label = country,
      y = rank,
      x = 0,
      hjust = 1
    ), size = 1.2) +
    geom_text(aes(
      label = highlight_country_label,
      y = rank,
      x = ind_1_fill
    ), hjust = 0) +
    scale_y_discrete("", expand = c(0, 0)) +
    scale_x_continuous(
      name = attr(wdi_data$ind_1, "label"),
      trans = scales::modulus_trans(p),
      labels = custom_formatter,
      breaks = modulus_breaks(p),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    ggthemes::theme_tufte() +
    theme(legend.position = "none") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    facet_null() +
    aes(group = country) +
    labs(title = 'Year: {frame_time}') +
    labs(caption = ifelse(p != 1, glue::glue("Transformed scale modulus({p})"), "")) +
    gganimate::transition_time(year) +
    #labs(title = 'Year: {closest_state}', y = "") +
    #gganimate::transition_states(states = year, transition_length = 1, state_length = 1) +
    gganimate::ease_aes('cubic-in-out')

  anim
  # set custom animation parameters and render the animation
  # gganimate::animate(
  #   anim,
  #   nframes = 250,
  #   fps = 25,
  #   start_pause = 25,
  #   end_pause = 25,
  #   width = 350,
  #   height = 550,
  #   renderer = gganimate::gifski_renderer()
  # )
}

#' Bubble plot, interactive (powered by plotly)
#'
#' Scatter plot that let's you map the size of the markers to an indicator
#'
#' @inheritParams download_wdi
#' @param x_indicator code of indicator for the x axis
#' @param y_indicator code of indicator for the y axis
#' @param size_indicator code of indicator for the size of the markers
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bubble_ly_wdi()
#' }
plot_bubble_ly_wdi <- function(x_indicator = "SH.XPD.GHED.GD.ZS",
                                   y_indicator = "SH.SGR.CRSK.ZS",
                                   size_indicator = "SP.POP.TOTL",
                                   highlight_countries = c("Colombia", "Germany"),
                                   start = lubridate::year(Sys.Date()) - 10,
                                   end = lubridate::year(Sys.Date() - months(18)),
                                   country = "all",
                                   regions = default_regions(),
                                   income_groups = default_income_groups()) {

  # TODO: allow modulus transformations, receiving p_x and p_y
  # TODO: allow some control over colors?, highlighted vs. rest?, regions?, inc?

  is_highlight <- NULL

  wdi_data <- latest_wdi(
    c(x_indicator, y_indicator, size_indicator), highlight_countries, start,
    end, country, regions, income_groups
  )

  all_years <- vctrs::vec_count(wdi_data$year)$key

  highlight_data <- wdi_data %>%
    filter(is_highlight)

  ind_1_lab <- attr(wdi_data$ind_1, "label")
  ind_2_lab <- attr(wdi_data$ind_2, "label")
  ind_3_lab <- attr(wdi_data$ind_3, "label")

  wdi_data %>%
    plotly::plot_ly(
      type = 'scatter',
      mode = 'markers',
      x = ~ind_1,
      y = ~ind_2,
      size = ~ind_3,
      text = ~paste0(
        "<b>", country, "</b> (", year,")<br><br>",
        ind_1_lab, ":<br>", get_formatter(wdi_data$ind_1)(ind_1), "<br><br>",
        ind_2_lab, ":<br>", get_formatter(wdi_data$ind_2)(ind_2),"<br><br>",
        ind_3_lab, ":<br>", get_formatter(wdi_data$ind_3)(ind_3)
      ),
      color = ~tidyr::replace_na(highlighted_country, "NA"),
      marker = list(opacity = 0.35, sizemode = 'diameter'),
      sizes = c(10, 50),
      hoverinfo = 'text'
    ) %>%
    plotly::add_markers( # just override the data and marker styling
      data = highlight_data, # including only highlighted countries
      marker = list(opacity = 1) # and solid markers
    ) %>%
    plotly::add_annotations(
      x = highlight_data$ind_1,
      y = highlight_data$ind_2,
      text = paste0("<b>", highlight_data$country, "</b>"),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "bottom"
    ) %>%
    plotly::layout(
      title = paste0("Year ", all_years[[1]], ifelse(length(all_years) > 1, " *", "")),
      showlegend = FALSE,
      xaxis = list(title = ind_1_lab, showgrid = FALSE),
      yaxis = list(title = ind_2_lab, showgrid = FALSE)
    )
}

#' Bubble plot
#'
#' Scatter plot that let's you map the size of the markers to an indicator
#'
#' @inheritParams download_wdi
#' @param x_indicator code of indicator for the x axis
#' @param y_indicator code of indicator for the y axis
#' @param size_indicator code of indicator for the size of the markers
#' @param p_x Transformation exponent for x-axis, as in scales::modulus_trans
#' @param p_y Transformation exponent for y-axis, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bubble_gg_wdi()
#' }
plot_bubble_gg_wdi <- function(x_indicator = "SH.XPD.GHED.GD.ZS",
                                   y_indicator = "SH.SGR.CRSK.ZS",
                                   size_indicator = "SP.POP.TOTL",
                                   highlight_countries = c("Colombia", "Germany"),
                                   start = lubridate::year(Sys.Date()) - 10,
                                   end = lubridate::year(Sys.Date() - months(18)),
                                   country = "all",
                                   regions = default_regions(),
                                   income_groups = default_income_groups(),
                                   p_x = 1,
                                   p_y = 1) {

  # TODO: allow some control over colors?, highlighted vs. rest?, regions?, inc?

  ind_1 <- ind_2 <- ind_3 <- alpha <- highlighted_country <- is_highlight <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi(
    c(x_indicator, y_indicator, size_indicator), highlight_countries, start,
    end, country, regions, income_groups
  )

  wdi_data <- wdi_data %>%
    mutate(alpha = case_when(is_highlight ~ 1, TRUE ~ 0.35))

  all_years <- vctrs::vec_count(wdi_data$year)$key

  ind_1_lab <- attr(wdi_data$ind_1, "label")
  ind_2_lab <- attr(wdi_data$ind_2, "label")
  ind_3_lab <- attr(wdi_data$ind_3, "label")

  wdi_data %>%
    ggplot() +
    aes(x = ind_1, y = ind_2) +
    geom_point(aes(size = ind_3, alpha = alpha, color = highlighted_country)) +
    ggrepel::geom_text_repel(aes(label = highlighted_country), fontface = "bold") +
    ggthemes::theme_tufte() +
    scale_alpha(range = c(0.35, 1), guide = FALSE) +
    scale_color_brewer(palette="Set2", na.value="grey70", guide = FALSE) +
    scale_size(
      name = ind_3_lab,
      range = c(2, 20),
      breaks = modulus_breaks(0, 4),
      labels = get_formatter(wdi_data$ind_3)
    ) +
    scale_x_continuous(
      name = attr(wdi_data$ind_1, "label"),
      trans = scales::modulus_trans(p_x),
      labels = get_formatter(wdi_data$ind_1),
      breaks = modulus_breaks(p_x),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    scale_y_continuous(
      name = attr(wdi_data$ind_2, "label"),
      trans = scales::modulus_trans(p_y),
      labels = get_formatter(wdi_data$ind_2),
      breaks = modulus_breaks(p_y),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    # ggplot2::geom_smooth() + TODO: conditional?
    theme(panel.border = element_rect(colour = "grey", fill = NA)) +
    guides(size = guide_legend(override.aes = list(size=c(2, 4, 6, 8)))) +
    theme(legend.position = "top") +
    labs(caption = paste(
      ifelse(p_x != 1, glue::glue("x-axis transformed scale modulus({p_x}) "), ""),
      ifelse(p_y != 1, glue::glue("y-axis transformed scale modulus({p_y}) "), "")
    )) +
    labs(subtitle = paste0(
      "Year ", all_years[[1]], ifelse(length(all_years) > 1, " *", "")
    ))
}


#' Animated bubble plot
#'
#' Scatter plot that let's you map the size of the markers to an indicator
#'
#' @inheritParams download_wdi
#' @param x_indicator code of indicator for the x axis
#' @param y_indicator code of indicator for the y axis
#' @param size_indicator code of indicator for the size of the markers
#' @param p_x Transformation exponent for x-axis, as in scales::modulus_trans
#' @param p_y Transformation exponent for y-axis, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bubble_anime_gg_wdi()
#' }
plot_bubble_anime_gg_wdi <- function(x_indicator = "SH.XPD.GHED.GD.ZS",
                                   y_indicator = "SH.SGR.CRSK.ZS",
                                   size_indicator = "SP.POP.TOTL",
                                   highlight_countries = c("Colombia", "Germany"),
                                   start = lubridate::year(Sys.Date()) - 20,
                                   end = lubridate::year(Sys.Date() - months(18)),
                                   country = "all",
                                   regions = default_regions(),
                                   income_groups = default_income_groups(),
                                   p_x = 1,
                                   p_y = 1) {

  # TODO: allow some control over colors?, highlighted vs. rest?, regions?, inc?

  ind_1 <- ind_2 <- ind_3 <- alpha <- year <- highlighted_country <- is_highlight <- NULL # or use the .data pronoun

  wdi_data <- download_wdi(
    c(x_indicator, y_indicator, size_indicator), highlight_countries, start,
    end, country, regions, income_groups, interpolate = TRUE
  )

  wdi_data <- wdi_data %>%
    mutate(alpha = case_when(is_highlight ~ 1, TRUE ~ 0.35))

  ind_1_lab <- attr(wdi_data$ind_1, "label")
  ind_2_lab <- attr(wdi_data$ind_2, "label")
  ind_3_lab <- attr(wdi_data$ind_3, "label")

  wdi_data %>%
    ggplot() +
    aes(x = ind_1, y = ind_2) +
    geom_point(aes(size = ind_3, alpha = alpha, color = highlighted_country)) +
    # not using ggrepel but geom_text because ggrepel looks unstable in the
    # animation, but then we better turn off clipping
    geom_text(
      aes(label = case_when(is_highlight ~ paste(highlighted_country, interpolate))),
      fontface = "bold", vjust = -1, hjust = 0.5
    ) +
    coord_cartesian(clip = "off") +
    ggthemes::theme_tufte() +
    scale_alpha(range = c(0.35, 1), guide = FALSE) +
    scale_color_brewer(palette="Set2", na.value="grey70", guide = FALSE) +
    scale_size(
      name = ind_3_lab,
      range = c(2, 20),
      breaks = modulus_breaks(0, 4),
      labels = get_formatter(wdi_data$ind_3)
    ) +
    scale_x_continuous(
      name = attr(wdi_data$ind_1, "label"),
      trans = scales::modulus_trans(p_x),
      labels = get_formatter(wdi_data$ind_1),
      breaks = modulus_breaks(p_x),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    scale_y_continuous(
      name = attr(wdi_data$ind_2, "label"),
      trans = scales::modulus_trans(p_y),
      labels = get_formatter(wdi_data$ind_2),
      breaks = modulus_breaks(p_y),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    # ggplot2::geom_smooth() + TODO: conditional?
    theme(panel.border = element_rect(colour = "grey", fill = NA)) +
    guides(size = guide_legend(override.aes = list(size=c(2, 4, 6, 8)))) +
    theme(legend.position = "top") +
    labs(caption = paste(
      ifelse(p_x != 1, glue::glue("x-axis transformed scale modulus({p_x}) "), ""),
      ifelse(p_y != 1, glue::glue("y-axis transformed scale modulus({p_y}) "), "")
    )) +
    labs(subtitle = 'Year: {frame_time}') +
    gganimate::transition_time(year) +
    gganimate::ease_aes('cubic-in-out')

}




#' Animated/dynamic bubble plot, interactive (powered by plotly)
#'
#' Scatter plot that let's you map the size of the markers to an indicator
#'
#' @inheritParams download_wdi
#' @param x_indicator code of indicator for the x axis
#' @param y_indicator code of indicator for the y axis
#' @param size_indicator code of indicator for the size of the markers
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bubble_anime_ly_wdi()
#' }
plot_bubble_anime_ly_wdi <- function(x_indicator = "SH.XPD.GHED.GD.ZS",
                                   y_indicator = "SH.SGR.CRSK.ZS",
                                   size_indicator = "SP.POP.TOTL",
                                   highlight_countries = c("China", "Chile"),
                                   start = lubridate::year(Sys.Date()) - 20,
                                   end = lubridate::year(Sys.Date() - months(18)),
                                   country = "all",
                                   regions = default_regions(),
                                   income_groups = default_income_groups()) {

  # TODO: allow modulus transformations, receiving p_x and p_y
  # TODO: allow some control over colors?, highlighted vs. rest?, regions?, inc?

  is_highlight <- NULL

  wdi_data <- download_wdi(
    c(x_indicator, y_indicator, size_indicator), highlight_countries, start,
    end, country, regions, income_groups, interpolate = TRUE
  )

  highlight_data <- wdi_data %>%
    filter(is_highlight)

  ind_1_lab <- attr(wdi_data$ind_1, "label")
  ind_2_lab <- attr(wdi_data$ind_2, "label")
  ind_3_lab <- attr(wdi_data$ind_3, "label")

  wdi_data %>%
    plotly::plot_ly( # set common parameters for all traces
      x = ~ind_1,
      y = ~ind_2,
      frame = ~year,
      text = ~paste0(
        "<b>", country, interpolate, "</b> (", year,")<br><br>",
        ind_1_lab, ":<br>", get_formatter(wdi_data$ind_1)(ind_1), "<br><br>",
        ind_2_lab, ":<br>", get_formatter(wdi_data$ind_2)(ind_2),"<br><br>",
        ind_3_lab, ":<br>", get_formatter(wdi_data$ind_3)(ind_3)
      )
    ) %>%
    plotly::add_markers( # add scatter plot markers
      size = ~ind_3,
      color = ~tidyr::replace_na(highlighted_country, "NA"),
      marker = list(opacity = 0.25, sizemode = 'diameter'),
      sizes = c(10, 50),
      hoverinfo = 'text'
    ) %>%
    plotly::add_markers( # override the data and marker styling opacity = 1
      data = highlight_data, # including only highlighted countries
      size = ~ind_3,
      color = ~tidyr::replace_na(highlighted_country, "NA"),
      marker = list(opacity = 1, sizemode = 'diameter'),
      sizes = c(10, 50),
      hoverinfo = 'text'
    ) %>%
    plotly::add_text(
      data = highlight_data, # including only highlighted countries
      text = ~paste0("<b>", country, interpolate, "</b>"),
      mode = 'text',
      textposition = 'middle right'
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = ind_1_lab, showgrid = FALSE),
      yaxis = list(title = ind_2_lab, showgrid = FALSE)
    ) %>%
    plotly::animation_opts(
      1000, 1000, redraw = FALSE
    )
}


#' Return special character codes
#'
#' Auxiliary function to return superscript numbers
#'
#' @param col_name name of the column indicator; ind_1, ind2, ..., ind_n
get_char_code <- function(col_name) {
  char_codes <- c("\u00b9", "\u00b2", "\u00b3", "\u2074", "\u2075")
  col_index <- as.numeric(sub("ind_", "", col_name))
  if (col_index > 5) {
    "*"
  } else {
    char_codes[col_index]
  }
}

#' Interpolate missing values in indicator variables
#'
#' most probably, there will be missing values in some countries for some years
#' so here's a controversial decision to make the animations look good. Let's
#' fill missing values for each country, by interpolating values in the gaps and
#' filling with the first or last value available
#'
#' @param wdi_data dataset to interpolate, as returned by download_wdi
#'
#' @return a data.frame with interpolated values and additional column
#'         indicating which indicators, for which year and country were
#'         interpolated
interpolate_wdi <- function(wdi_data) {

  country <- year <- NULL

  interpolated_data <- wdi_data %>%
    tidyr::complete(country, year) %>% # now there could be NA in ind_i
    mutate(across(
      .cols = starts_with("ind_"),
      .fns = ~ifelse(is.na(.x), get_char_code(dplyr::cur_column()), ""),
      .names = "interpolate_{.col}"
    )) %>%
    tidyr::unite("interpolate", starts_with("interpolate_"), sep = "") %>%
    group_by(country) %>%
    arrange(country, year) %>%
    mutate(dplyr::across( # here we are losing the labels
      .cols = dplyr::starts_with("ind_"),
      .fns = zoo::na.approx,
      na.rm = FALSE
    )) %>%
    tidyr::fill(dplyr::starts_with("ind_"), .direction = "downup") %>%
    ungroup()

  # Need to get the labels back
  for (col in names(wdi_data)) {
    attr(interpolated_data[[col]], "label") <- attr(wdi_data[[col]], "label")
  }

  interpolated_data
}

#' Default regions, just to avoid repeating so many lines in all functions
default_regions <- function() {
  c(
    "Middle East & North Africa",
    "Europe & Central Asia",
    "Sub-Saharan Africa",
    "Latin America & Caribbean",
    "East Asia & Pacific",
    "South Asia",
    "North America"
  )
}

#' Default income groups, just to avoid repeating so many lines in all functions
default_income_groups <- function() {
  c(
    "Aggregates",
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income"
  )
}

#' Modulus breaks factory
#'
#' This function tries to replicate/approximate the nice functionality of
#' log-transformation (`scales` package to be used for `ggplot2` plots, using
#' for example ggplot2::scale_x_log10()). that automatically shows breaks values
#' in the original scale, even though the axis is on a transformed scale.
#' Unfortunately, that is not the default behaviour of other transformations,
#' and the modulus transformation in particular.
#'
#' This function does not aim for a clever algorithm. Rather it just calculates
#' optimal breaks (labeling::extended) on the original and transformed scales,
#' and combine them, expressing all in the original scale
#'
#' Take a look at scales::log_breaks and ?scales::log_trans
#'
#' @param p_custom p transformation exponent, as in scales::modulus_trans
#' @param n.breaks_default number of breaks
#'
#' @return numeric vector with the breaks
modulus_breaks <- function(p_custom, n.breaks_default = 10) {

  function(limits, p = p_custom, n.breaks = n.breaks_default) {
    # you get the limits in the original scale, so let's transform them to
    # calculate the breaks on the transformed scale
    limits_trans <- scales::modulus_trans(p)$transform(limits)
    breaks_trans <- labeling::extended(
      dmin = min(limits_trans),
      dmax = max(limits_trans),
      m = n.breaks,
      Q = scales::modulus_trans(p)$transform(c(1, 5, 2, 2.5, 4, 3))
    )
    # But apply the inverse to express the breaks on the original scale
    # breaks calculated on the original  scale are already pretty, but the
    # breaks calculated on the transformed scale are not (more precisely, they
    # are only pretty on the transformed scale, but we want them in the end on
    # the original scale)
    breaks_trans <- scales::modulus_trans(p)$inverse(breaks_trans)
    breaks_trans <- purrr::map_dbl(breaks_trans, ~ pretty(.x, n = 1)[which.min(abs(pretty(.x, n = 1) - .x))])
    # And also calculate breaks on the original scale
    breaks_notrans <- labeling::extended(min(limits), max(limits), n.breaks)
    # ANd finally put them together
    breaks_combined <- unique(sort(c(breaks_trans, breaks_notrans)))
    #breaks_final <- purrr::map_dbl(breaks_combined, ~ dplyr::first(pretty(.x, n = 1)))
    breaks_combined
  }
}

#' Create custom formatter for indicators, based on the data and label
#'
#' @param plot_data indicator data, including label
#'
#' @return a formatter function from the scales package
get_formatter <- function(plot_data) {

  ind_label <- attr(plot_data, "label")
  ind_min <- min(plot_data, na.rm = TRUE)
  ind_max <- max(plot_data, na.rm = TRUE)

  if (grepl("\\(.*%.*)", ind_label)) {
    return(scales::percent_format(accuracy = 0.1, scale = 1))
  }

  if (ind_max - ind_min < 200) { # arbitrary
    return(scales::comma_format(accuracy = 0.1))
  }

  if (ind_max - ind_min < 1) { # arbitrary
    return(scales::comma_format(accuracy = 0.01))
  }

  return(scales::comma_format(accuracy = 1))
}


# https://plotly-r.com/animating-views.html
# https://xang1234.github.io/bubbleplot/

