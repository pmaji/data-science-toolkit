# libraries
# library(cowplot)

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
    
    # building temp dataset for ease of use
    main_dataset <- eval(as.symbol(dataset))
    category_names <- as.character(unique(main_dataset[,categorical_variable]))
    
    # builds the PDF chart
    pdf_viz <-
      main_dataset %>%
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
      main_dataset %>%
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
      y  = 0, vjust = 0, size = size_bottom_annotation, fontface = "bold")
    
    # adding second subtitle to give aggregate metrics for var 1
    sub1n2_titled_merged_plot <- cowplot::add_sub(
      plot = sub1_titled_merged_plot,
      label = paste0(
        "Mean for ",
        category_names[1],
        " = ",
        round(
          main_dataset %>%
            dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
            dplyr::select(noquote(continuous_variable)) %>%
            dplyr::summarise_all(mean) %>%
            dplyr::filter(.[[1]]==category_names[1]) %>%
            dplyr::select(2),
          digits = decimal_place_for_agg_stats),
        "    Median for  ",
        category_names[1],
        " = ",
        round(
          main_dataset %>%
            dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
            dplyr::select(noquote(continuous_variable)) %>%
            dplyr::summarise_all(median) %>%
            dplyr::filter(.[[1]]==category_names[1]) %>%
            dplyr::select(2),
          digits = decimal_place_for_agg_stats)
      ),
      y  = 0.25, vjust = 0, size = size_bottom_annotation)
    
    # adding third subtitle to give aggregate metrics for var 1
    sub3_titled_merged_plot <- cowplot::add_sub(
      plot = sub1n2_titled_merged_plot,
      label = paste0(
        "Mean for ",
        category_names[2],
        " = ",
        round(
          main_dataset %>%
            dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
            dplyr::select(noquote(continuous_variable)) %>%
            dplyr::summarise_all(mean) %>%
            dplyr::filter(.[[1]]==category_names[2]) %>%
            dplyr::select(2),
          digits = decimal_place_for_agg_stats),
        "    Median for  ",
        category_names[2],
        " = ",
        round(
          main_dataset %>%
            dplyr::group_by(eval(as.symbol(categorical_variable))) %>%
            dplyr::select(noquote(continuous_variable)) %>%
            dplyr::summarise_all(median) %>%
            dplyr::filter(.[[1]]==category_names[2]) %>%
            dplyr::select(2),
          digits = decimal_place_for_agg_stats)
      ),
      y  = 0.75, vjust = 0, size = size_bottom_annotation)


    # drawing and returning the final combined viz
    final_combined_viz <- cowplot::ggdraw(sub3_titled_merged_plot)
    return(final_combined_viz)
  }


# function to compute and create visualization for KS test
# requirements: two numeric vectors for two distributions to be compared
# fill out all parameters below for best results
gen_ks_test_viz_and_results <-
  function(
    # parameters for function:
    # inputs for KS Test
    numeric_vector_1,
    label_for_numeric_vector_1 = "var1",
    numeric_vector_2,
    label_for_numeric_vector_2 = "var2",
    # options within ks.test()
    alternative_for_test = "two.sided",
    pval_calctype_for_test = NULL,
    # options for visuals
    alpha_for_density = 0.4,
    ref_line_thickness = 0.75,
    size_of_legend_title = 12,
    size_of_legend_text = 12,
    main_title_text = "KS Test Results",
    ecdf_subtitle_text = "ECDF",
    fill_text,
    colour_text,
    x_text,
    y_text = "Cumulative Concentration Density",
    decimal_place_for_agg_stats = 2,
    decimal_place_for_p_value = 5,
    decimal_place_for_test_stat = 3,
    size_bottom_annotation = 14
  ) {
    # first we run the ks test and store the results
    # turning off warnings pre-KS test
    op <- options(warn = (-1))
    # running the ks test itself
    ks_test_results <-
      ks.test(
        # x and y are the two vectors that compose the distributions to be compared
        # order does matter here but only when the alternative hypothesis chosen is not two-sided
        x = numeric_vector_1,
        y = numeric_vector_2,
        # specifies the null hypothesis that the true distribution function of x is equal to
        # this can be a bit tricky and should be catered to the problem at hand; read the docs for more info
        alternative = alternative_for_test,
        # parameter to decide whether or not to compute exact p-values
        # NULL here doesn't mean no; it is instead conditional upon sample size as described in the docs
        exact = pval_calctype_for_test
      ) %>%
        # uses the broom package to tidy up the output of the test
        broom::tidy()
    # resets the options back to default setting post-performance of ks test
    options(op)

    # next, we break out they key components of the test separately
    ks_test_stat     <- ks_test_results$statistic
    ks_test_pval     <- ks_test_results$p.value
    ks_test_method   <- ks_test_results$method
    ks_test_alt      <- ks_test_results$alternative

    # now we move on to the ECDF

    # first we need to construct the right type of dataset
    df_1 <- as.data.frame(numeric_vector_1) %>%
      dplyr::mutate(category_label = label_for_numeric_vector_1)
    # standardizing column names to faciliate append later
    colnames(df_1)[1] <- 'numeric_column'
    # getting mean and median for first set


    df_2 <- as.data.frame(numeric_vector_2) %>%
      mutate(category_label = label_for_numeric_vector_2)
    # standardizing column names to faciliate append later
    colnames(df_2)[1] <- 'numeric_column'

    # appending 2 dfs together for viz_creation
    x <- bind_rows(df_1, df_2)

    # now we can create the viz for the ECDF itself
    viz <- x %>%
      dplyr::group_by(category_label) %>%
      dplyr::mutate(
        mean_pledged = mean(numeric_column),
        median_pledged = median(numeric_column)
      ) %>%
      # ungroup prior to the viz code
      ungroup() %>%
      ggplot(data = ., aes(x=numeric_column, colour = factor(category_label))) +
        stat_ecdf() +
        # adding reference lines for the mean and the median
        geom_vline(aes(xintercept=mean_pledged, colour=factor(category_label)),
                 linetype="dashed", size=ref_line_thickness) +
        geom_vline(aes(xintercept=median_pledged, colour=factor(category_label)),
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
          y = y_text,
          x = x_text,
          colour = colour_text
        ) +
        guides(
          # ensures the country of origin is listed first in legends
          fill = guide_legend(order=1),
          color = guide_legend(order=2)
        )

    # making use of cowplot package to add titles and annotations
    master_title <- cowplot::ggdraw() + cowplot::draw_label(main_title_text, fontface = 'bold')

    # adding the title to the main
    # rel_heights values control title margins
    titled_main_plot <- cowplot::plot_grid(master_title, viz, ncol = 1, rel_heights = c(0.1, 1))

    # adding first subtitle to explain reference line meaning
    sub1_merged_plot <- cowplot::add_sub(
      plot = titled_main_plot,
      label = "References lines: dashed = mean; dotted = median",
      y  = 0, vjust = 0, size = size_bottom_annotation
    )

    # adding second subtitle to give aggregate metrics for var 1
    sub1n2_titled_merged_plot <- cowplot::add_sub(
      plot = sub1_merged_plot,
      label = paste0(
        "Mean for ",
        label_for_numeric_vector_1,
        " = ",
        round(
          x %>%
            group_by(category_label) %>%
            summarise_all(mean) %>%
            filter(category_label==label_for_numeric_vector_1) %>%
            select(numeric_column),
          digits = decimal_place_for_agg_stats),
        "    Median for  ",
        label_for_numeric_vector_1,
        "= ",
        round(
          x %>%
            group_by(category_label) %>%
            summarise_all(median) %>%
            filter(category_label==label_for_numeric_vector_1) %>%
            select(numeric_column),
          digits = decimal_place_for_agg_stats)
      ),
      y  = 0.25, vjust = 0, size = size_bottom_annotation)

    # adding third subtitle to give aggregate metrics for var 2
    sub3_titled_merged_plot <- cowplot::add_sub(
      plot = sub1n2_titled_merged_plot,
      label = paste0(
        "Mean for ",
        label_for_numeric_vector_2,
        " = ",
        round(
          x %>%
            group_by(category_label) %>%
            summarise_all(mean) %>%
            filter(category_label==label_for_numeric_vector_2) %>%
            select(numeric_column),
          digits = decimal_place_for_agg_stats),
        "    Median for  ",
        label_for_numeric_vector_2,
        "= ",
        round(
          x %>%
            group_by(category_label) %>%
            summarise_all(median) %>%
            filter(category_label==label_for_numeric_vector_2) %>%
            select(numeric_column),
          digits = decimal_place_for_agg_stats)
      ),
      y  = 0.75, vjust = 0, size = size_bottom_annotation)

    # adding fourth subtitle to add ks test results
    sub4_merged_plot <- cowplot::add_sub(
      plot = sub3_titled_merged_plot,
      label = paste0(
        "stat=",
        round(
          ks_test_stat,
          digits = decimal_place_for_test_stat
          ),
        " ; p-value=",
        round(
          ks_test_pval,
          digits = decimal_place_for_p_value
          ),
        " ; alternative=",
        ks_test_alt
      ),
      y  = 0.75, vjust = 0, size = size_bottom_annotation, fontface='bold')

    # drawing and returning the final combined viz
    final_combined_viz <- cowplot::ggdraw(sub4_merged_plot)
    return(final_combined_viz)

  }

