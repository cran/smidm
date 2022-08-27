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


## ----calculate_posterior_no_infections_statement------------------------------

negative_persons <- 23
infected_persons <- 2
event <- "school"

test_infos <- matrix(nrow = 2, ncol = 3)
test_infos[1, 1] <- 1
test_infos[1, 2] <- 2
test_infos[2, 1] <- 2
test_infos[2, 2] <- 4
test_infos[2, 3] <- 6

test_types <- matrix(nrow = 2, ncol = 2)
test_types[1, 1] <- "PCR"
test_types[2, 1] <- "PCR"
test_types[2, 2] <- "Antigen"


subgroup_size <- c(3, 5)

prob <- calculate_posterior_no_infections(negative_persons,
                                          infected_persons,
                                          event,
                                          test_infos,
                                          test_types,
                                          subgroup_size)

print(prob)


## ----calculate_likelihood_negative_tests--------------------------------------
test_infos <- matrix(nrow = 2, ncol = 3)
test_infos[1, 1] <- 1
test_infos[1, 2] <- 2
test_infos[2, 1] <- 2
test_infos[2, 2] <- 4
test_infos[2, 3] <- 6

test_types <- matrix(nrow = 2, ncol = 2)
test_types[1, 1] <- "PCR"
test_types[2, 1] <- "PCR"
test_types[2, 2] <- "Antigen"

negative_persons <- 23
subgroup_size <- c(3, 5)

likelihood <- calculate_likelihood_negative_tests(test_infos,
                                                  test_types,
                                                  negative_persons,
                                                  subgroup_size)

print(likelihood)


## ----calculate_prior_infections-----------------------------------------------
negative_persons <- 23
infected_persons <- 2
event <- "school"

prior <- calculate_prior_infections(negative_persons,
                                    infected_persons,
                                    event)
print(prior)


## ----foldcode = TRUE----------------------------------------------------------
date <- as.Date("2021-10-05")
tests_df <- data.frame(c("2021-10-06 PCR-test", "2021-10-07 antigen-test", "2021-10-07 antigen-test", "2021-10-08 antigen-test", "2021-10-10 antigen-test"), c(2, 2, 3, 5, 1), c(1, 2, 2, 3, 5))


## ----foldcode = TRUE----------------------------------------------------------
 # Convert test data frame into arrays
   test_dates <- c()
   test_colors <- c()
   test_positions <- c()
   test_text_positions <- c()
   test_labels <- c("event")
   test_label_positions_x <- c(date)
   test_label_positions_y <- c(0.6)
   test_label_colours <- c("1")
   if (nrow(tests_df > 0)) {
      for (i in 1:nrow(tests_df)) {
         test_list <- strsplit(tests_df[i, 1], ", ")[[1]]
         dates_group <- c()
         for (j in 1:length(test_list)) {
            test <- strsplit(test_list[j], " ")[[1]]
            test_dates <- c(test_dates, test[1])
            dates_group <- c(dates_group, as.Date(test[1]))
            test_colors <- c(test_colors, as.character(i + 1) )
            test_positions <- c(test_positions, i * 0.9 / nrow(tests_df))
            test_text_positions <- c(test_text_positions, i * 0.9 / nrow(tests_df) + 0.1)
         }
         test_label <- paste(as.character(tests_df[i, 2]), " persons")
         if(tests_df[i, 2] == 1)
         {
            test_label <- "1 person"
         }
         test_labels <- c(test_labels, test_label)
         test_label_positions_x <- c(test_label_positions_x, mean.Date(dates_group))
         test_label_positions_y <- c(test_label_positions_y, i*0.9/nrow(tests_df) + 0.1)
         test_label_colours <- c(test_label_colours, as.character(i+1) )
      }
   }
   
   df_label <- data.frame(Date = test_label_positions_x, 
                          Position = test_label_positions_y, 
                          Label = test_labels, 
                          col = test_label_colours)
   df <- data.frame(Date = c(date, test_dates), 
                    Position = c(0.5, test_positions),
                    Text_pos = c(0.6, test_text_positions),
                    col      = c("1", test_colors))
   df <- df %>% arrange(desc(Position))
   
   timeline_plot <- ggplot(df, aes(x = Date, y = Position, color = col)) +
       scale_color_brewer(palette = "Set1") +
       theme_classic() +
       geom_hline(yintercept = 0, color = "black", size = 0.3) +
       geom_segment(aes(y = Position, yend = 0, xend = Date, color = col), 
                    size = 0.2) +
       geom_point(aes(y = Position), size = 3) +
       theme(axis.line.y = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.ticks.y = element_blank(),
             # axis.text.x = element_blank(),
             # axis.ticks.x = element_blank(),
             axis.line.x = element_blank(),
             legend.position = "none",
             legend.title = element_blank(),
             text = element_text(size = 16),
             axis.text.x = element_text(angle = 30, hjust = 1)
       ) +
       geom_label(aes(x = Date, y = Position, label = Label), df_label, size = 5, label.size = NA) +
       scale_x_date(date_labels = "%d %b", date_breaks = "1 day", limits = c(min(df$Date) - 1, max(df$Date) + 1)) 
       

   timeline_plot
   

