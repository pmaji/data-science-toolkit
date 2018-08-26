# libraries
library(cowplot)

# function to create side-by-side PDF and ECDF
# requirements: dataframe with one categorical and one numeric variable
# fill out all parameters below for best results
gen_sidebyside_pdf_ecdf <- 
  function(
    # parameters for function:
    dataset,
    continuous_variable,
    categorical_variable,
    alpha_for_density = 0.4,
    ref_line_thickness = 0.75,
    size_of_legend_title = 12,
    size_of_legend_text = 12,
    main_title_text = "PDF and ECDF",
    pdf_subtitle_text = "PDF",
    ecdf_subtitle_text = "ECDF",
    fill_text,
    colour_text,
    x_text,
    y_pdf_text = "Concentration Density",
    y_ecdf_text = "Cumulative Concentration Density",
    decimal_place_for_agg_stats = 2,
    size_bottom_annotation = 14
  ) {
    
    # builds the PDF chart
    pdf_viz <-
      eval(as.symbol(dataset)) %>%
      dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
      dplyr::mutate(
        mean_pledged = mean(eval(as.symbol(continuous_variable))),
        median_pledged = median(eval(as.symbol(continuous_variable)))
      ) %>%
      # ungroup prior to the viz code
      ungroup() %>%
        ggplot(data = ., aes(x=eval(as.symbol(continuous_variable)))) +
        # color needs to always be a factor, although this is redundant here
        # alpha moderates the opacity of the color
        geom_density(aes(fill=factor(eval(as.symbol(categorical_variable)))), alpha = alpha_for_density) +
        # adding reference lines for the mean and the median
        geom_vline(aes(xintercept=mean_pledged, colour=factor(eval(as.symbol(categorical_variable)))),
                  linetype="dashed", size=ref_line_thickness) +
        geom_vline(aes(xintercept=median_pledged, colour=factor(eval(as.symbol(categorical_variable)))),
                  linetype="dotted", size=ref_line_thickness) +
        # puts the legend on top of the view
        theme(
          legend.position = "top",
          legend.title = element_text(size=size_of_legend_title),
          legend.text = element_text(size=size_of_legend_text)
        ) +
        # takes care of all labeling
        labs(
          title = pdf_subtitle_text,
          y = y_pdf_text,
          x = x_text,
          fill = fill_text,
          colour = colour_text
        ) +
        guides(
          # ensures the country of origin is listed first in legends
          fill = guide_legend(order=1),
          color = FALSE
        )
    
    # builds the ECDF chart
    ecdf_viz <-
      eval(as.symbol(dataset)) %>%
      dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
      dplyr::mutate(
        mean_pledged = mean(eval(as.symbol(continuous_variable))),
        median_pledged = median(eval(as.symbol(continuous_variable)))
      ) %>%
      # ungroup prior to the viz code
      ungroup() %>%
      # color needs to always be a factor, although this is redundant here
      ggplot(data = ., aes(x=eval(as.symbol(continuous_variable)), colour = factor(eval(as.symbol(categorical_variable))))) +
      stat_ecdf() +
      # adding reference lines for the mean and the median
      geom_vline(aes(xintercept=mean_pledged, colour=factor(eval(as.symbol(categorical_variable)))),
                 linetype="dashed", size=ref_line_thickness) +
      geom_vline(aes(xintercept=median_pledged, colour=factor(eval(as.symbol(categorical_variable)))),
                 linetype="dotted", size=ref_line_thickness) +
      # puts the legend on top of the view
      theme(
        legend.position = "top",
        legend.title = element_text(size=size_of_legend_title),
        legend.text = element_text(size=size_of_legend_text)
      ) +
      # takes care of all labeling
      labs(
        title = ecdf_subtitle_text,
        y = y_ecdf_text,
        x = x_text,
        fill = fill_text,
        colour = colour_text
      ) +
      guides(
        # ensures the country of origin is listed first in legends
        fill = guide_legend(order=1),
        color = guide_legend(order=2)
      )
    
    # now combining the views into one 
    merged_plot <- cowplot::plot_grid(pdf_viz, ecdf_viz)
    
    # creating the master title
    master_title <- cowplot::ggdraw() + cowplot::draw_label(main_title_text, fontface = 'bold')
    
    # adding the title to the merged plot
    # rel_heights values control title margins
    titled_merged_plot <- cowplot::plot_grid(master_title, merged_plot, ncol = 1, rel_heights = c(0.1, 1)) 
    
    # adding first subtitle to explain reference line meaning
    sub1_titled_merged_plot <- cowplot::add_sub(
      plot = titled_merged_plot, 
      label = "References lines: dashed = mean; dotted = median", 
      y  = 0, vjust = 0, size = size_bottom_annotation
      )
    
    # adding second subtitle to give aggregate metrics
    sub1n2_titled_merged_plot <- cowplot::add_sub(
      plot = sub1_titled_merged_plot, 
      label = paste0(
        "Mean = ", 
        (eval(as.symbol(dataset)) 
         %>% summarize(ret = round(mean(eval(as.symbol(continuous_variable))), 
                                   digits = decimal_place_for_agg_stats))),
        "    Median = ",
        (eval(as.symbol(dataset)) 
         %>% summarize(ret = round(median(eval(as.symbol(continuous_variable))), 
                                   digits = decimal_place_for_agg_stats)))
        ),
      size = size_bottom_annotation)
    
    # drawing and returning the final combined viz
    final_combined_viz <- cowplot::ggdraw(sub1n2_titled_merged_plot)
    return(final_combined_viz)
  }
