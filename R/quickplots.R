#' Plot a WDI indicator
#'
#' @param wdi_data data.frame as returned by latest_wdi_ind
#' @param ind variable in wdi_data to plot
#' @param groups variable in wdi_data to use as facets (either region or income)
#' @param country variable in wdi_data to use as country names
#' @param highlight variable in wdi_data with the value of countries to highglight
#' @param p Transformation exponent, λ, as in scales::modulus_trans
#'
#' @return a ggplot2 object
#' @import ggplot2
#' @importFrom dplyr pull
#'
#' @examples
#' \dontrun{
#'  wdi_data <- latest_wdi_ind(indicator, highlight_countries, start, end, country)
#'  plot_dist_wdi_ind_ggpdef(wdi_data, plot_ind, {{groups}}, country, highlight, p)
#' }
plot_dist_wdi_ind_ggpdef <- function(wdi_data, ind, groups, country, highlight, p = 0) {

  wdi_data <- wdi_data %>%
    group_by({{groups}}) %>%
    mutate(custom_hjust = scales::rescale(-{{highlight}}, to = c(0, 1))) %>%
    ungroup()

  ggplot(aes(x = {{ ind }}, fill = {{ groups }}), data = wdi_data) +
  facet_wrap(vars({{ groups }}), ncol = 1, scales = "free_y") +
  geom_density(alpha = 0.7, color = NA, adjust = 0.25) + # TODO: bw per facet
  geom_rug() +
  geom_vline(aes(xintercept = {{ highlight }}), linetype = "dotted") +
  ggrepel::geom_text_repel(
    aes(
      x = {{ highlight }},
      y = Inf,
      # using this, two highlights in a facet would never be in the line center
      #hjust = custom_hjust,
      label = paste0(
        {{ country }}, "\n",
        # tailor the scale function using all the data in {ind} but apply it
        # only to highlight data
        tailor_scales(pull(wdi_data, {{ ind }}))({{ highlight }})
      )
    ),
    #direction = "y", # only let ggrepel to adjust horizontally
    point.padding = NA, # do not repel if there is only 1 highlight in a facet
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
  #     label = tailor_scales(pull(wdi_data, {{ ind }}))({{ highlight }})
  #   ),
  #   vjust = -0.1,
  #   hjust = 0,
  #   fontface = "bold"
  # ) +
  ggthemes::theme_tufte() +
  scale_x_continuous(
    name = attr(wdi_data %>% pull({{ ind }}), "label"),
    trans = scales::modulus_trans(p),
    labels = tailor_scales(pull(wdi_data, {{ ind }})),
    breaks = modulus_breaks(p),
    guide = guide_axis(check.overlap = TRUE),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  ylab("Density") +
  labs(caption = ifelse(p != 1, glue::glue("* Transformed scale modulus({p})"), ""))
}

#' Download WDI::WDI() data for a single indicator and get the latest value per
#' country
#'
#' @param indicator character of length 1 with the indicator code
#' @param highlight_countries character vector with country names to highlight
#' @param start first year to download data
#' @param end last year to download data
#' @param country subset of countries to download data. Default to "all"
#'
#' @return data.frame with columns country, year, plot_ind, region, income,
#'         highlight. highlight = NA, except for highlight countries in which
#'         case takes the value of plot_ind
#'
#' @importFrom dplyr select filter mutate group_by ungroup slice_max
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
                           country = "all") {

  year <- plot_ind <- region <- income <- NULL # or use the .data pronoun

  wdi_data <- WDI::WDI(
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
    group_by(country) %>%
    slice_max(order_by = year) %>%
    ungroup() %>%
    mutate(highlight = ifelse(country %in% highlight_countries, plot_ind, NA)) %>%
    mutate(income = factor(income, levels = c(
      "Aggregates", "High income", "Upper middle income", "Lower middle income",
      "Low income", NA
    )))
}

#' Plot the distribution (density) of a single indicator using WDI data,
#' downloading the data as necessary for the period indicated by start and end,
#' and keeping only the latest data point available for each country
#'
#' @inheritParams latest_wdi_ind
#' @param groups whether to show facets per region or income
#' @param p Transformation exponent, λ, as in scales::modulus_trans
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
                              groups = region,
                              start = lubridate::year(Sys.Date()) - 10,
                              end = lubridate::year(Sys.Date()),
                              country = "all",
                              p = 0) {

  plot_ind <- region <- highlight <- NULL # or use the .data pronoun

  wdi_data <- latest_wdi_ind(indicator, highlight_countries, start, end, country)

  plot_dist_wdi_ind_ggpdef(wdi_data, plot_ind, {{groups}}, country, highlight, p)
}


modulus_breaks <- function(p_default, n.breaks_default = 10) {

  function(limits, p = p_default, n.breaks = n.breaks_default) {
    limits_trans <- scales::modulus_trans(p)$transform(limits)
    breaks_notrans <- labeling::extended(min(limits), max(limits), n.breaks)
    breaks_trans <- labeling::extended(
      dmin = min(limits_trans),
      dmax = max(limits_trans),
      m = n.breaks,
      Q = scales::modulus_trans(p)$transform(c(1, 5, 2, 2.5, 4, 3))
    )
    breaks_final <- scales::modulus_trans(p)$inverse(breaks_trans)
    c(breaks_final, breaks_notrans)
  }
}

tailor_scales <- function(plot_data) {
  if (all(plot_data >= 0 & plot_data <= 1)) {
    return(scales::percent_format(accuracy = 1))
  }
  return(scales::comma_format(accuracy = 1))
}
