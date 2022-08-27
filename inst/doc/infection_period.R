## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

knitr::knit_hooks$set(
  source = function(x, options) {
    hook.r <- function(x, options) {
      fence <- "```"
      language <- tolower(options$engine)
      if (language == 'node') language <- 'javascript'
      if (!options$highlight) language <- 'text'
      if(!is.null(options$foldcode)) {
      paste0('\n\n', "<details><summary>Source</summary>\n", fence, language, '\n', x, fence,  '\n\n', "</details>\n")
      } else {
              paste0('\n\n', fence, language, '\n', x, fence,  '\n\n')
      }
    }
    x <- knitr:::hilight_source(x, 'markdown', options)
    hook.r(
      paste(c(
        x, 
        ''
      ), collapse = '\n'), 
      options
    )
  }
)

Sys.setlocale("LC_TIME", "C")


## ----setup, echo = FALSE, message = FALSE-------------------------------------
library(smidm)
library(ggplot2)
library(dplyr)
library(hdrcde)


## ----get_infection_date_density_statement-------------------------------------
symptom_begin_date <- as.Date("2021-12-28")
max_incubation_days <- 18
meanlog <- 1.69
sdlog <- 0.55

infec_date_df <- get_infection_density(symptom_begin_date,
                                       max_incubation_days,
                                       meanlog,
                                       sdlog)


## ----get_infection_density_result, echo = FALSE, message = FALSE--------------
knitr::kable(infec_date_df[100:109, ],
             caption = "values 100 to 109 of resulting data frame")


## ----get_misc_infection_date_density------------------------------------------
symptom_begin_dates <- c(as.Date("2021-12-24"), as.Date("2021-12-28"))
persons <- c(1, 2)
max_incubation_days <- 18

misc_infec_date_df <- get_misc_infection_density(symptom_begin_dates,
                                                 persons,
                                                 max_incubation_days)


## ----get_mic_infection_density_result, echo = FALSE, message = FALSE----------
knitr::kable(misc_infec_date_df[100:109, ],
             caption = "values 100 to 109 of resulting data frame")


## ----.calculate_qstart_qend, foldcode = TRUE----------------------------------
.calculate_qstart_qend <- function(probability, df) {
    hdr_df <- hdr(den = data.frame(x = 1:length(df$distribution), y = df$distribution),
                  p = probability * 100)$hdr
    qstart <- (hdr_df[1:(length(hdr_df) / 2) * 2] - 1) / 24
    qend  <- (hdr_df[1:(length(hdr_df) / 2) * 2 - 1] - 1) / 24
  return(list("qstart" = qstart, "qend" = qend))
}


## ----.shade_curve, foldcode = TRUE--------------------------------------------
.shade_curve <- function(df, qstart, qend, fill = "red", alpha = 0.4) {
  subset_df <- df[floor(qstart * 24):ceiling(qend * 24), ]
  geom_area(data = subset_df,
            aes(x = x, y = y), 
            fill = fill, 
            color = NA, 
            alpha = alpha)
}


## ----parameters for visualization of get_infection_density, foldcode = TRUE----
  symptom_begin_date <- as.Date("2021-12-28")

  df <- infec_date_df
  period_80 <- .calculate_qstart_qend(0.8, df) 
  period_95 <- .calculate_qstart_qend(0.95, df)
  
  symp_date_posixct_start <- as.POSIXct(format(as.POSIXct(symptom_begin_date, tz = "CET"), "%Y-%m-%d"))
  symp_date_posixct_end <- as.POSIXct(format(as.POSIXct(symptom_begin_date + 1, tz = "CET"), "%Y-%m-%d"))
  symp_date_posixct_mid <- symp_date_posixct_start - as.numeric(difftime(symp_date_posixct_start,
                           symp_date_posixct_end, units = "hours")) / 2 * 3600 

## ----visualization of get_infection_density, foldcode = TRUE------------------
  g <- ggplot() +

    scale_x_datetime(breaks = scales::date_breaks("1 days"), labels = scales::date_format("%d %b")) +
    theme(axis.text.x = element_text(angle = 90)) +
    # scale_x_continuous(breaks = x_tick,
    #                    labels = x_label) +
    # theme(axis.ticks.x = element_line(color = c(rbind(rep("black", length(x_label) / 2), rep(NA, length(x_label) / 2))),        linetype = 2, size = 1))+
    geom_path(aes(x = df$dates, y = df$distribution, color = "red")) +
    .shade_curve(df = data.frame(x = df$dates, y = df$distribution),
                 period_80$qstart,
                 period_80$qend) +
    .shade_curve(df = data.frame(x = df$dates, y = df$distribution),
                 period_95$qstart,
                 period_95$qend,
                 alpha = 0.2) +
    geom_rect(data = data.frame(xmin = symp_date_posixct_start,
                            xmax = symp_date_posixct_end,
                            ymin = -Inf,
                            ymax = Inf),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "brown", alpha = 0.3) +
    geom_label(aes(x = symp_date_posixct_mid, y = 0.9*max(df$distribution), label = "symptom\nonset"),
              colour = "brown", fill = "white", size = 5, label.size = NA) +
    ylab("probability") +
    xlab("timeline") +
    labs(color = 'Verteilung') +
    # ggtitle("Visualization of get_infection_density ") +
    theme(legend.position = "none", text = element_text(size = 16*5/5)) +
    theme(axis.text.x = element_text(colour = "black", face = "bold", angle = 30, hjust = 1)) +
    theme(axis.title.x = element_text(colour = "black", face = "bold")) +
    theme(axis.text.y = element_text(colour = "gray50")) +
    theme(axis.title.y = element_text(colour = "gray50"))


  g
  

## ----parameters for visualization of get_misc_infection_density, foldcode = TRUE----
  df <- misc_infec_date_df
  
  period_80 <- .calculate_qstart_qend(0.8, df) 
  period_95 <- .calculate_qstart_qend(0.95, df)
  
  symp_date_posixct_start <- as.POSIXct(format(as.POSIXct(symptom_begin_date, tz = "CET"), "%Y-%m-%d"))
  symp_date_posixct_end <- as.POSIXct(format(as.POSIXct(symptom_begin_date + 1, tz = "CET"), "%Y-%m-%d"))
  symp_date_posixct_mid <- symp_date_posixct_start - as.numeric(difftime(symp_date_posixct_start,
                           symp_date_posixct_end, units = "hours")) / 2 * 3600 
  

## ----visualization of get_misc_infection_density, foldcode = TRUE-------------
  g <- ggplot() +

    scale_x_datetime(breaks = scales::date_breaks("1 days"), labels = scales::date_format("%d %b")) +
    theme(axis.text.x = element_text(angle = 90)) +
    # scale_x_continuous(breaks = x_tick,
    #                    labels = x_label) +
    # theme(axis.ticks.x = element_line(color = c(rbind(rep("black", length(x_label) / 2), rep(NA, length(x_label) / 2))),        linetype = 2, size = 1))+
    geom_path(aes(x = df$dates, y = df$distribution, color = "red")) +
    .shade_curve(df = data.frame(x = df$dates, y = df$distribution),
                 period_80$qstart,
                 period_80$qend) +
    .shade_curve(df = data.frame(x = df$dates, y = df$distribution),
                 period_95$qstart,
                 period_95$qend,
                 alpha = 0.2) +
    ylab("probability") +
    xlab("timeline") +
    labs(color = 'Verteilung') +
    # ggtitle("Visualization of get_infection_density") +
    theme(legend.position = "none", text = element_text(size = 16 * 5 / 5)) +
    theme(axis.text.x = element_text(colour = "black", face = "bold", angle = 30, hjust = 1)) +
    theme(axis.title.x = element_text(colour = "black", face = "bold")) +
    theme(axis.text.y = element_text(colour = "gray50")) +
    theme(axis.title.y = element_text(colour = "gray50"))

  g
  

