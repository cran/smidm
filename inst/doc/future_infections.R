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


## ----get_expected_total_infections--------------------------------------------
group_size <- 25
last_day_reported_infections <- 5 # day 0 = event day
total_reported_infections <- 4
meanlog <- 1.69
sdlog <- 0.55

predicted_total_infections <- get_expected_total_infections(group_size,
                                                            last_day_reported_infections,
                                                            total_reported_infections,
                                                            meanlog,
                                                            sdlog)
print(predicted_total_infections)


## ----predict_future_infections------------------------------------------------
last_day_reported_infections <- 5 # day 0 = event day
total_reported_infections <- 4
total_expected_infections <- get_expected_total_infections(25, 5, 4)
meanlog <- 1.69
sdlog <- 0.55

predicted_daily_infections <- predict_future_infections(last_day_reported_infections,
                                                        total_reported_infections,
                                                        total_expected_infections,
                                                        meanlog,
                                                        sdlog)
print(predicted_daily_infections)


## ----libraries, foldcode = TRUE, message = FALSE------------------------------
data <- data.frame("Erkrankungsdatum" = as.Date("2022-03-15") + 0:5,
                   "Neue_Faelle" = c(0, 0, 0, 1, 0, 3))
expected <- data.frame("Erkrankungsdatum" = as.Date("2022-03-15") + 1:(length(predicted_daily_infections)),
                       "ErwarteteWeitereFaelle" = predicted_daily_infections)
g <- ggplot(expected) +
    geom_bar(
      data = data,
      aes(Erkrankungsdatum,
          Neue_Faelle,
          fill = "observation"),
      stat = 'identity'
    ) +
    geom_bar(
      data = expected,
      aes(Erkrankungsdatum,
          ErwarteteWeitereFaelle,
          fill = "prediction"
      ),
      stat = 'identity'
    ) +
    geom_vline(xintercept = expected$Erkrankungsdatum[1]) +
    geom_label(aes(x = expected$Erkrankungsdatum[1], y = data$Neue_Faelle[1] + 4, label = "event"), 
               colour = "black", fill = "white", vjust = 1, size = 7) +
    scale_y_continuous(breaks = function(x) unique(
      floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d %b", minor_breaks = NULL) +
    ylab("infected") +
    xlab("timeline") +
    labs(fill = 'type of cases') +
    theme(legend.position = c(0.75,0.85), text = element_text(size = 16),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1))
  return(g)



