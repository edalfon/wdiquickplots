regions <- c(
  "Middle East & North Africa",
  "Europe & Central Asia",
  "Sub-Saharan Africa",
  "Latin America & Caribbean",
  "East Asia & Pacific",
  "South Asia",
  "North America"
)

income_groups <- c(
  "Aggregates",
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

#' Wrapper around WDI::WDI() to download data for a single indicator
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
#'                      sincome groups
#'
#' @return data.frame with columns country, year, plot_ind, region, income,
#'         highlight. highlight = NA, except for highlight countries in which
#'         case takes the value of plot_ind
#'
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' \dontrun{
#' download_wdi_ind("NY.GDP.PCAP.KD", c("Colombia", "Germany"), 2019, 2019)
#' }
#'
download_wdi_ind <- function(indicator = "NY.GDP.PCAP.KD",
                             highlight_countries = c(""),
                             start = lubridate::year(Sys.Date()) - 10,
                             end = lubridate::year(Sys.Date()),
                             country = "all",
                             regions = wdiquickplots:::regions,
                             income_groups = wdiquickplots:::income_groups) {

  year <- plot_ind <- region <- income <- NULL # or use the .data pronoun

  regions <- match.arg(regions, several.ok = TRUE)
  income_groups <- match.arg(income_groups, several.ok = TRUE)

  wdi_data <- memoised_wdi(
    country = country,
    indicator = c(plot_ind = indicator), # named vector will be renamed, thx WDI
    start = start,
    end = end,
    extra = TRUE # to always get region and income level
  )

  wdi_data %>%
    select(country, year, plot_ind, region, income) %>%
    drop_na() %>%
    filter(region != "Aggregates") %>%
    mutate(highlight = ifelse(country %in% highlight_countries, plot_ind, NA)) %>%
    mutate(income = factor(income, levels = c(
      "Aggregates", "High income", "Upper middle income", "Lower middle income",
      "Low income", NA
    ))) %>%
    filter(region %in% regions) %>%
    filter(income %in% income_groups)
}

memoised_wdi <- memoise::memoise(
  f = WDI::WDI,
  cache = memoise::cache_filesystem("~/wdiquickplots_cache")
)


#' Get latest data for a single indicator per country
#'
#' @inheritParams download_wdi_ind
#'
#' @return data.frame with columns country, year, plot_ind, region, income,
#'         highlight. highlight = NA, except for highlight countries in which
#'         case takes the value of plot_ind
#'
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' \dontrun{
#' latest_wdi_ind("NY.GDP.PCAP.KD", c("Colombia", "Germany"), 2019, 2019)
#' }
#'
latest_wdi_ind <- function(indicator = "NY.GDP.PCAP.KD",
                           highlight_countries = c(""),
                           start = lubridate::year(Sys.Date()) - 10,
                           end = lubridate::year(Sys.Date()),
                           country = "all",
                           regions = wdiquickplots:::regions,
                           income_groups = wdiquickplots:::income_groups) {

  year <- plot_ind <- region <- income <- NULL # or use the .data pronoun

  wdi_data <- download_wdi_ind(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  wdi_data %>%
    group_by(country) %>%
    dplyr::slice_max(order_by = year) %>%
    ungroup()
}

#' Plot a WDI indicator
#'
#' @param wdi_data data.frame as returned by latest_wdi_ind
#' @param ind variable in wdi_data to plot
#' @param facets variable in wdi_data to use as facets (either region or income)
#' @param country variable in wdi_data to use as country names
#' @param highlight variable in wdi_data with the value of countries to highglight
#' @param year variable in wdi_data with the value of the year for each point
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#'
#' @examples
#' \dontrun{
#' wdi_data <- latest_wdi_ind(indicator, highlight_countries, start, end, country)
#' plot_dist_wdi_ind_ggpdef(wdi_data, plot_ind, {{ facets }}, country, highlight, p)
#' }
plot_dist_wdi_ind_ggpdef <- function(wdi_data, ind, facets, country, highlight,
                                     year, p = 1) {

  # wdi_data <- wdi_data %>%
  #   group_by({{facets}}) %>%
  #   mutate(custom_hjust = scales::rescale(-{{highlight}}, to = c(0, 1))) %>%
  #   ungroup()

  the_plot <- ggplot(aes(x = {{ ind }}, fill = {{ facets }}), data = wdi_data) +
    facet_wrap(vars({{ facets }}), ncol = 1, scales = "free_y") +
    geom_density(alpha = 0.7, color = NA, adjust = 0.25) + # TODO: bw per facet
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
      # direction = "y", # only let ggrepel to adjust horizontally
      point.padding = NA, # never repel if there is only 1 highlight in a facet
      vjust = 1,
      hjust = 0.5,
      lineheight = 0.75,
      fontface = "bold"
    ) +
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
        wdi_data %>% pull({{ year }}) %>% vctrs::vec_slice(i = 1), " *"
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
    ylab("Density") +
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
#' @inheritParams download_wdi_ind
#' @param facets whether to show facets per region or income
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dist_wdi_ind()
#' }
plot_dist_wdi_ind <- function(indicator = "NY.GDP.PCAP.KD",
                              highlight_countries = c("Colombia", "Germany"),
                              facets = region,
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              regions = wdiquickplots:::regions,
                              income_groups = wdiquickplots:::income_groups,
                              p = 1) {
  plot_ind <- region <- highlight <- year <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi_ind(indicator, highlight_countries, start, end,
                             country, regions, income_groups)

  plot_dist_wdi_ind_ggpdef(wdi_data, plot_ind, {{ facets }}, country, highlight,
                           year, p)
}

#' Plot a WDI indicator as an interactive bar-plot (powered by plotly)
#'
#' @inheritParams download_wdi_ind
#' @param base_color fill color for the bars for most countries
#' @param highlight_color fill color for the bars for highlighted countries
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bar_wdi_ind()
#' }
plot_bar_wdi_ind <- function(indicator = "NY.GDP.PCAP.KD",
                             highlight_countries = c("Colombia", "Germany"),
                             start = lubridate::year(Sys.Date()) - 10,
                             end = lubridate::year(Sys.Date()),
                             country = "all",
                             regions = wdiquickplots:::regions,
                             income_groups = wdiquickplots:::income_groups,
                             base_color = "skyblue",
                             highlight_color = "red") {

  # TODO: refactor plot
  # TODO: let customize transformed scales (currently using plotly's auto mode)
  # TODO: apply transformation. Perhaps use a button or dropdown to let the
  #       user customize the transformation parameter p interactively
  #       https://plotly.com/r/dropdowns/
  # TODO: let the user decide whether horizontal or vertical bar plot
  # https://plotly.com/r/reference/

  plot_ind <- region <- highlight <- year <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi_ind(indicator, highlight_countries, start, end,
                             country, regions, income_groups)

  wdi_data <- wdi_data %>%
    mutate(text = dplyr::case_when(
      !is.na(highlight) ~ paste0(country, " [", scales::comma(highlight), "]")
    )) %>%
    mutate(color = dplyr::case_when(!is.na(highlight) ~ highlight_color,
                                    TRUE ~ base_color))

  plotly::plot_ly(
    data = wdi_data,
    x = ~plot_ind,
    y = ~reorder(country, plot_ind),
    text = ~text,
    textposition = 'outside',
    textfont = list(face = "bold", color = '#000000', size = 14),
    hovertemplate = paste('%{y}<br>%{x:,.0f}<br>'), # TODO: customize scale
    # https://github.com/d3/d3-3.x-api-reference/blob/master/Formatting.md#d3_format
    type = 'bar',
    orientation = 'v',
    marker = list(color = ~color)
  ) %>%
    plotly::layout(
      # using textfont above would not let you override text size limit
      # https://stackoverflow.com/questions/62094773/is-it-possible-to-override
      # -font-size-limit-on-labels-in-r-plotly-bar-charts
      uniformtext = list(minsize  =14, mode = "show"),
      xaxis = list(title = paste0(
        attr(wdi_data$plot_ind, "label"), "\nYear ",
        wdi_data$year %>% vctrs::vec_slice(i = 1), " *"
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
#' @inheritParams download_wdi_ind
#' @param facets variable to use for facets. Either region or income
#' @param p Transformation exponent, <U+03BB>, as in scales::modulus_trans
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_time_facets_wdi_ind <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              facets = region,
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              regions = wdiquickplots:::regions,
                              income_groups = wdiquickplots:::income_groups,
                              p = 1) {

  region <- year <- plot_ind <- highlight <- NULL

  wdi_data <- download_wdi_ind(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  ggplot(aes(x = year, y = plot_ind, color = country), data = wdi_data) +
    geom_point() +
    geom_line(aes(group = country), size = 1.5) +
    facet_wrap(vars({{facets}})) +
    gghighlight::gghighlight(
      !is.na(highlight),
      calculate_per_facet = TRUE,
      use_direct_label = TRUE,
      label_params = list(fill = "white", point.padding = 0.1, direction = "y")
    ) +
    scale_y_continuous(
      name = attr(wdi_data$plot_ind, "label"),
      trans = scales::modulus_trans(p),
      labels = get_formatter(wdi_data$plot_ind),
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
#' @inheritParams download_wdi_ind
#'
#' @return plotly object
#' @export
#'
#' @examples
plot_time_wdi_ind <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              regions = wdiquickplots:::regions,
                              income_groups = wdiquickplots:::income_groups) {

  # TODO: allow transformation passing p as in otherss

  year <- plot_ind <- highlight <- NULL

  # TODO: here you could actually download only the highlight countries
  wdi_data <- download_wdi_ind(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  custom_formatter <- get_formatter(wdi_data$plot_ind)

  wdi_data <- wdi_data %>%
    filter(!is.na(highlight)) %>%
    group_by(country) %>%
    mutate(direct_labels = case_when(
      year == max(year, na.rm = TRUE) ~ paste0(country, "\n", custom_formatter(plot_ind)),
      year == min(year, na.rm = TRUE) ~ custom_formatter(plot_ind)
    )) %>%
    dplyr::arrange(country, year) # I hate that plotly makes you arrange it

  plotly::plot_ly(
    data = wdi_data,
    x = ~ year,
    y = ~ plot_ind,
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
      yaxis = list(title = attr(wdi_data %>% pull(plot_ind), "label")),
      yaxis = list(title = NA)
    )
}

#' Spaghetti plot that no-one really want to see (nor should want to see, ever)
#'
#' @inheritParams download_wdi_ind
#'
#' @return a dygraphs plot
#' @export
#'
#' @examples
plot_spaghetti_wdi_ind <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              regions = wdiquickplots:::regions,
                              income_groups = wdiquickplots:::income_groups) {

  # TODO: allow transformation passing p as in otherss

  year <- plot_ind <- highlight <- region <- income <- NULL

  wdi_data <- download_wdi_ind(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  wdi_data_wide <- wdi_data %>%
    select(-region, -income, -highlight) %>%
    tidyr::pivot_wider(names_from = country, values_from = plot_ind) %>%
    dplyr::arrange(year)

  dy_spaghetti <- dygraphs::dygraph(
    data = wdi_data_wide,
    xlab = "",
    ylab = attr(wdi_data$plot_ind, "label")
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
#' @inheritParams download_wdi_ind
#' @param p transformation exponent, as in scales::modulus_trans
#'
#' @return gganimate
#' @export
#'
#' @examples
plot_race_wdi_ind <- function(indicator = "SI.POV.GINI",
                              highlight_countries = c("Colombia", "Germany"),
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              regions = wdiquickplots:::regions,
                              income_groups = wdiquickplots:::income_groups,
                              p = 1) {

  # TODO: allow transformation passing p as in otherss

  year <- plot_ind <- highlight <- region <- income <- highlight_country <-
    highlight_country_label <- highlight_dummy <- plot_ind_fill <- NULL

  wdi_data <- download_wdi_ind(indicator, highlight_countries, start, end,
                               country, regions, income_groups)

  custom_formatter <- get_formatter(wdi_data$plot_ind)

  wdi_race_data <- wdi_data %>%
    # most probably, there will be missing values in some countries for some years
    # so here's a controversial decision to make the animation look good
    # let's fill missing values for each country, by interpolating values in
    # the gaps and filling with the first or last value available
    tidyr::complete(country, year) %>% # not there will be NA in plot_ind
    group_by(country) %>%
    arrange(country, year) %>%
    mutate(plot_ind_fill = zoo::na.approx(plot_ind, na.rm = FALSE)) %>%
    tidyr::fill(plot_ind_fill, .direction = "downup") %>%
    mutate(highlight = zoo::na.approx(highlight, na.rm = FALSE)) %>%
    tidyr::fill(highlight, .direction = "downup") %>%
    ungroup() %>%
    # Try to be transparent about it and signal the country name with an *
    # whenever there was a missing value
    mutate(country_label = ifelse(is.na(plot_ind), paste0(country, "*"), country)) %>%
    # leave unhighlighted countries as NA, then the fill color will be grey
    mutate(highlight_country = case_when(!is.na(highlight) ~ country)) %>%
    mutate(highlight_country_label = case_when(
      !is.na(highlight) ~ paste0(country_label, " (", custom_formatter(highlight), ")"),
      TRUE ~ ""
    )) %>%
    mutate(highlight_dummy = case_when(!is.na(highlight) ~ 1, TRUE ~ 0.77)) %>%
    # Now calculate country rank in every year, for the animated plot
    group_by(year) %>%
    arrange(year, -plot_ind_fill) %>%
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
      xmax = plot_ind_fill,
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
      x = plot_ind_fill
    ), hjust = 0) +
    scale_y_discrete("", expand = c(0, 0)) +
    scale_x_continuous(
      name = attr(wdi_data$plot_ind, "label"),
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
    breaks_trans <- scales::modulus_trans(p)$inverse(breaks_trans)
    # And also calculate breaks on the original scale
    breaks_notrans <- labeling::extended(min(limits), max(limits), n.breaks)
    breaks_combined <- sort(c(breaks_trans, breaks_notrans))
    # breaks calculated on the original  scale are already pretty, but the
    # breaks calculated on the transformed scale are not (more precisely, they
    # are only pretty on the transformed scale, but we want them in the end on
    # the original scale)
    breaks_final <- purrr::map_dbl(breaks_combined, ~ dplyr::first(pretty(.x, n = 1)))
    breaks_final
  }
}

get_formatter <- function(plot_data) {
  if (all(plot_data >= 0 & plot_data <= 1)) {
    return(scales::percent_format(accuracy = 1))
  }
  return(scales::comma_format(accuracy = 1))
}

# TODO: bring the bubble plot
