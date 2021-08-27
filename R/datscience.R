########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)
#' Rename specific columns of a data.frame
#'
#' Replace the column names of a data.frame entrywise for each element
#' a given vector (old) with the element of another vector (new)
#'
#' @param df data.frame.
#' @param old Character vector.
#' @param new Character vector.
#'
#' @return df data.frame
#'
#' @examples
#' iris.renamed <- cols_rename(iris, "Sepal.Length", "Sepal Length")
#' iris.renamed2 <- cols_rename(
#'   iris,
#'   c("Sepal.Length", "Sepal.Width"),
#'   c("SLength", "SWidth")
#' )
#' @export
cols_rename <- function(df, old, new) {
  if (!is.null(new)) {
    for (i in seq_along(old)) {
      names(df)[names(df) == old[i]] <- new[i]
    }
  }
  return(df)
}


#' Seaches Column Names Starting with an [Reg]Expression
#'
#' Prints out all Cols that start with a given string
#' no need to at .. to the regex. Helpful for very long data.frames
#'
#' @param df data.frame.
#' @param regex Character vector.
#'
#'
#' @examples
#' colstartsw(iris, "Sepal")
#' @export
colstartsw <- function(regex = "", df) {
  s_phrase <- paste0("^", regex, "..")
  print(names(df)[grepl(s_phrase, names(df))])
}

#'  Creates Boxplots with Significance Makers
#' @description
#' Based on the following: Guide
#' https://www.datanovia.com/en/blog/how-to-perform-multiple-t-test-in-r-for-different-variables/
#' Given a vector of dependent variables (DV), creates boxplots
#' with facetwrap for all DVs and calculates an independent sample T-Test
#' to include significance bars
#'
#' @param df data.frame.
#' @param dependentvars Character vector.
#' @param group Character vector.
#' @param adjust_p Character vector.
#' @param ylimits Numeric vector.
#'
#'
#' @examples
#' \dontrun{
#' boxplot_t_test(mtcars, c("mpg", "hp"), group = "am")
#' }
#'
#' @return List(Plot and stats)
#'
#' @export
#' @import dplyr
#' @import rstatix
#' @import ggplot2
#' @importFrom ggpubr ggboxplot
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr theme_pubr
#' @importFrom magrittr "%>%"
boxplot_t_test <-
  function(df,
           dependentvars,
           group,
           adjust_p = "BH",
           ylimits = c(0, 150)) {
    variables <- NULL
    df_s <- df %>%
      dplyr::select(dependentvars, group) %>%
      dplyr::as_tibble()

    # Pivot Table
    df_p <- df_s %>%
      tidyr::pivot_longer(-all_of(group),
        names_to = "variables", values_to =
          "value"
      )

    # Calc T Test
    stat.test <- df_p %>%
      dplyr::group_by(`variables`) %>%
      rstatix::t_test(stats::as.formula(paste("value ~", group))) %>%
      rstatix::adjust_pvalue(method = adjust_p) %>%
      rstatix::add_significance()
    # stat.test

    # Creating the Plot
    p1 <-
      ggpubr::ggboxplot(
        df_p,
        x = group,
        y = "value",
        fill = group,
        palette = "npg",
        legend = "none",
        ggtheme = ggpubr::theme_pubr(border = TRUE)
      ) +
      # ylim(ylimits)+
      ggplot2::facet_wrap(~variables)

    stat.test <- stat.test %>% rstatix::add_xy_position(x = group)
    p1 <-
      p1 + ggpubr::stat_pvalue_manual(stat.test, label = "p.adj.signif")
    return(list(plot = p1, stats = stat.test))
  }

#' Creates APA Tables in Rmarkdown Templates
#' @description
#' Given a data.frame creates an APA Table utilizing knitr::kable
#' This is also possible in Templates for RMarkdown
#'
#' @param df data.frame.
#'
#'
#' @examples
#' my_apa(iris)
#' @return Table
#'
#' @export
#' @importFrom kableExtra kable_styling row_spec column_spec
#' @import knitr xtable
my_apa <- function(df) {
  kableExtra::kable(df,
    format = "html",
    algin = "1",
    booktabs = TRUE
  ) %>%
    kableExtra::kable_styling(full_width = TRUE, position = "left") %>%
    kableExtra::row_spec(0,
      extra_css =
        "border-top:1.5px solid black; border-bottom:1.5px solid black;"
    ) %>%
    kableExtra::row_spec(nrow(df),
      extra_css = "border-bottom:1.5px solid black;"
    ) %>%
    kableExtra::row_spec(0:nrow(df),
      align = "c",
      background = "#FFFFFF"
    ) %>%
    kableExtra::column_spec(1,
      extra_css = "text-align: left;"
    ) %>%
    kableExtra::column_spec(seq_along(df),
      extra_css = "border-right:0;border-top:0;"
    )
}



#' Corstars - Correaltions in Console
#' @description  Creates a pretty console correlation table (by Dominik Vogel)
#' method : correlation method. "pearson"" or "spearman"" is supported
#' the results will be displayed in html or latex format# labels_rows and labels_cols are character vectors for labeling rows and columns
#' https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R
#'
#' @param x a matrix containing the data
#' @param method correlation method. "pearson"" or "spearman"" is supported
#' @param removeTriangle remove upper or lower triangle, or FALSE for not removing any triangle
#' @param rmDiag if one triangle of the matrix is removed, should the diagonal be kept = FALSE; or removed = TRUE
#' @param result Print result in Console ("none"), generate HTML file ("html"), generate latex file ("latex")
#' @param labels_rows Labels for the rows (i.e., variable names). Length musst be same as number of variables
#' @param labels_cols Labels for columns. Length musst be same as number of variables - 1
#' @param sig.level Significance level (.1 or .05)
#' @param caption Caption for the table
#' @param filename File name to save output to
#'
#' @return Correlation table in console or file
#'
#' @author Dominik Vogel (Debugged by Björn Büdenbender)
#'
#'
#' @examples
#' # Console output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "none",
#'   caption = "Correlations",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#'
#' # HTML output
#' corstars(mtcars,
#'   method = "pearson", removeTriangle = "upper", result = "html",
#'   caption = "Correlations", filename = "corr.html",
#'   sig.level = 0.1,
#'   labels_rows = c(
#'     "(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'     "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'     "(9) am", "(10) gear",
#'     "(11) carb"
#'   ),
#'   labels_cols = 1:10
#' )
#' @export
#' @import xtable
#' @importFrom Hmisc rcorr
corstars <- function(x,
                     method = c("pearson", "spearman"),
                     removeTriangle = c("upper", "lower",FALSE),
                     rmDiag = TRUE,
                     result = c("none", "html", "latex"),
                     labels_rows = colnames(x),
                     labels_cols = labels_rows[1:(length(labels_rows) - 1)],
                     sig.level = 0.05,
                     caption = c("Correlation"),
                     filename = "") {
  requireNamespace("Hmisc", quietly = TRUE)
  requireNamespace("xtable", quietly = TRUE)
  stopifnot(length(labels_rows) == ncol(x))
  stopifnot(length(labels_cols) == ncol(x) - 1)
  # Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix <- Hmisc::rcorr(x, type = method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  # ifelse(sig.level == 0.1,
  #        mystars <- ifelse(p < .01,"**", ifelse(p < .05, "* ", "  ")),
  #        ifelse(sig.level == 0.05,
  #               mystars <- ifelse(p < .05, "*", " "),""))

  mystars <-
    ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(
    x
  )), R), 2))[, -1]
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")
  ## remove upper triangle of correlation matrix
  if (removeTriangle[1] == "upper") {
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = rmDiag)] <- ""
    Rnew <- as.data.frame(Rnew)
    ## remove lower triangle of correlation matrix
  } else if (removeTriangle[1] == "lower") {
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = rmDiag)] <- ""
    Rnew <- as.data.frame(Rnew)
  } else {
    Rnew <- as.data.frame(as.matrix(Rnew))
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  rownames(Rnew) <- labels_rows
  colnames(Rnew) <- labels_cols
  if (result[1] == "none") {
    return(Rnew)
  } else {
    if (result[1] == "html") {
      print(xtable::xtable(Rnew, caption = caption),
        type = "html",
        file = filename
      )
    } else {
      print(xtable::xtable(Rnew, caption = caption),
        type = "latex"
      )
    }
  }
}


#' R Package Citations
#' @description
#' Function to create a package table with the following columns: Packagename | Version | Maintainer | Citation
#   - Creates two files .csv for the table and a .bib for the citations
#   - Requires some manual processing: Importing of the CSV in the Word Processing Program and Importing the .bib in
#     In the Reference Managementtool, e.g. Mendeley
#' @param outdirectory A character vector for the output directory (for the two files, .bib and .csv). Default ist ./
#'
#' @return Two Files for a Package List and Citations for the Appendix of the Paper
#'
#' @author Björn Büdenbender
#'
#'
#' @examples
#' citations_appendix()
#' @export
#' @importFrom utils write.csv maintainer packageVersion
#' @importFrom dplyr add_row
#' @importFrom knitr write_bib
#' @importFrom magrittr "%>%"
#' @importFrom pacman p_loaded
citations_appendix <- function(outdirectory = "./") {
  # Helper Function extract the last n chars of a string
  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  # Controlling if the ending slash is present
  if (outdirectory != "./") {
    if (substrRight(outdirectory, 1) != "/") {
      outdirectory <- paste0(outdirectory, "/")
    }
  }

  # Check if Outdirectory Exist, Else Create it
  if (!file.exists(outdirectory)) {
    dir.create(outdirectory)
  }

  # Loading required Packages
  #### Write the Bibliography with all Citations for the Packages
  knitr::write_bib(file = paste0(outdirectory, "Bibliography Packages.bib"))

  #### Create a Table with all Important Information
  appendix_packages <- data.frame(
    Packagename = character(),
    Version = character(),
    Maintainer = character()
  )


  for (pkg in pacman::p_loaded()) {
    appendix_packages <- appendix_packages %>% dplyr::add_row(
      Packagename = pkg,
      Version = as.character(utils::packageVersion(pkg)),
      Maintainer = utils::maintainer(pkg)
    )
  }

  utils::write.csv(
    x = appendix_packages,
    file = paste0(outdirectory, "Appendix Table Packages.csv"),
    row.names = FALSE
  )
}




#' Pretty Scree
#' @description
#' Creates an APA Plot of Parallel Analysis (Horn)
#' Check the original Source: https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/#psych
#' @param parallel an parallel object returned by psych::fa.parallel
#' @param fa either "pc" or "fa" the factor methods used for the parallel analysis
#' @param quant default = .95 the quantile of the simulated values used to plot
#'
#' @return APA Ready Plot of Parallel Analyssis
#'
#' @author John Sakaluk (Wrapped in a Function by Björn Büdenbender)
#'
#'
#' @export
#' @import ggplot2

pretty_scree <- function(parallel, fa, quant = .95) {
  num <- eigenvalue <- type <- NULL
  # Abbreviations
  #   - noi = Number of Interest, Components or Factors determined by Parallel-Analysis after Horn
  #   - cf = Common Factor
  #   - pc = Prinicipal Components
  # Calculate quantiles for eigenvalues, for sim. pc and cf
  percentile <- apply(parallel$values, 2, function(x) {
    stats::quantile(x, quant)
  })

  # If Screeplot should be prettified for principal components
  if (fa == "pc") {
    index <- grep("CSim", names(percentile))
    if (length(index) == 0) {
      index <- grep("Sim", names(percentile))
    }
    if (length(index) == 0) {
      index <- grep("C", names(percentile))
    }
    obs <- data.frame(parallel$pc.values)
    percentile1 <- percentile[index]
    noi <- parallel$ncomp
  } else if (fa == "fa") {
    index <- grep("Fsim", names(percentile))
    if (length(index) == 0) {
      index <- grep("Sim", names(percentile))
    }
    if (length(index) == 0) {
      index <- grep("F", names(percentile))
    }
    obs <- data.frame(parallel$fa.values)
    percentile1 <- percentile[index]
    noi <- parallel$nfact
  }
  # Create Data Frame for Observed Eigenvalues
  obs$type <- c("Observed Data")
  obs$num <- c(row.names(obs))
  obs$num <- as.numeric(obs$num)
  colnames(obs) <- c("eigenvalue", "type", "num")
  # Create data frame called with simulated eigenvalue data
  sim <- data.frame(percentile1)
  sim$type <- c("Simulated Data (95th %ile)")
  sim$num <- c(row.names(obs))
  sim$num <- as.numeric(sim$num)
  colnames(sim) <- c("eigenvalue", "type", "num")
  # Merge the two data frames (obs and sim) together into data frame called eigendat
  eigendat <- rbind(obs, sim)

  # Create an APA Theme for the Plot
  apatheme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Arial"),
      legend.title = ggplot2::element_blank(),
      legend.position = c(.7, .8),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.line.y = ggplot2::element_line(color = "black")
    )

  # Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
  p <- ggplot2::ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
    # Add lines connecting data points
    ggplot2::geom_line() +
    # Add the data points.
    ggplot2::geom_point(size = 4) +
    # Label the y-axis 'Eigenvalue'
    ggplot2::scale_y_continuous(name = "Eigenvalue") +
    # Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
    ggplot2::scale_x_continuous(
      name = "Factor Number",
      breaks = min(eigendat$num):max(eigendat$num)
    ) +
    # Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
    ggplot2::scale_shape_manual(values = c(16, 1)) +
    # Add vertical line indicating parallel analysis suggested max # of factors to retain
    ggplot2::geom_vline(xintercept = noi, linetype = "dashed") +
    # Apply our apa-formatting theme
    apatheme
  # Call the plot. Looks pretty!
  return(p)
}


#' Booted Eigenvalues
#' Uses boot::boot to create x resampled Eigenvalues
#' Eigenvalues can be extracted for PCA as well as EFA
#' Different Factor Methods are available for more details, see ?psych::fa()
#' Allows for extraction of Eigenvalues based on Pearson
#' as well as Polychoric Correlation Matrices
#'
#' @param df = the complete data.frame
#' @param iterations = number of resamples for the bootstrap
#' @param cor =  either "pearson" or "poly" for polychoric correlations,
#'  defaults to "pearson"
#' @param fa = either "pc" for prinicipal component or
#' "fa" for [common] factor analysis, defaults to "pc"
#' @param fm = factor method to use, irrelevant for pca.
#' For available factor methods check psych::fa for more details, defaults to minres
#'
#' @return A boot object (boot::boot()),
#' that contains SE for all Eigenvalues in DF, can be passed to getCIs() to creacte Confidence Intervalls
#'
#' @author Björn Büdenbender
#'
#' @export
#' @importFrom boot boot
#' @importFrom psych fa polychoric

booted_eigenvalues <-
  function(df,
           iterations = 1000,
           cor = "pearson",
           fa = "pc",
           fm = "minres") {
    eigenvalues_extractor <- function(d, i, cor, fa, fm) {
      d2 <- d[i, ]
      if (cor == "pearson") {
        rx <- cor(d2, use = "pairwise")
        nobs <- NA
      } else if (cor == "poly") {
        poly_cor <- psych::polychoric(d2, correct = FALSE)
        rx <- poly_cor$rho
        nobs <- poly_cor$n.obs
      }

      if (fa == "pc") {
        res <- eigen(rx)$values
      } else if (fa == "fa") {
        res <-
          psych::fa(
            rx,
            nfactors = 1,
            rotate = "none",
            fm = fm,
            warnings = FALSE,
            n.obs = nobs
          )$values
      }
      return(res)
    }

    boot_obj <- boot::boot(
      d = df,
      # Helper Function to Extract Eigenvalues
      eigenvalues_extractor,
      # Number of Resamples
      R = iterations,
      # Parameters for the Eigenvalue Extraction
      fa = fa,
      fm = fm,
      cor = cor
    )
    return(boot_obj)
  }


#' Get Boot Strapped CIs
#'
#' @description
#' Determines the CI for a vector of statistics / mutliple stats.
#' As boot::boot.ci only returns CI for the first statistic in a boot_object
#' getCIs (given a boot::boot object), creates CI for all statistics.
#' Code by Ben Bolker, Check the
#' original Source: https://stackoverflow.com/a/31818160/7318488
#'
#' @param boot_obj boot::boot object
#'
#' @return A data.frame with Confidence intervalls for
#' all statistics, as well as the observed value
#'
#' @author Ben Bolker
#'
#' @export
#' @importFrom boot boot.ci
#' @importFrom utils tail

getCIs <- function(boot_obj) {
  getCI <- function(x, w) {
    b1 <- boot::boot.ci(x, index = w)
    ## extract info for all CI types
    tab <- t(sapply(b1[-(1:3)], function(x) {
      tail(c(x), 2)
    }))
    ## combine with metadata: CI method, index
    tab <- cbind(w, rownames(tab), as.data.frame(tab), x$t0[w])
    colnames(tab) <- c("index", "method", "lwr", "upr", "observed")
    tab
  }
  return(do.call(rbind, lapply(c(1:ncol(boot_obj$t)), getCI, x = boot_obj)))
}


#' Add CI to Pretty Screeplot
#'
#' @description
#' After CIs are determined by a combination of booted_eigenvalues() & getCIs()
#' This function provided with the plot object and the data.frame with CIs
#' adds CI around the observd eigenvalues
#'
#' @param plot The plot object as obtained by pretty_scree()
#' @param CIs Data.frame obtained from getCIs() and booted_eigenvalues()
#' @param met the type of CI to be used default "normal"
#' | other options: "basic", "stud", "perc", "bca"
#' @param color If type of visual display of the CI = "band"
#' the color of the area between Upper and Lower
#' @param outline Color of the Outline for the Area
#' @param transparency Alpha value, how transparent shall the filling color be
#' @param type either "band" or "errorbars", defaults to "band"
#' @param lt Linetype, 0 for no outline around the band, other options include: "dotted", "dashed", "solid"
#'
#' @return A new pretty_plot with the Parallel Analysis,
#' including CIs around the observed Eigenvalues.
#'
#' @author Björn Büdenbender
#'
#' @export
#' @importFrom dplyr rename select mutate
#' @importFrom ggplot2 geom_ribbon geom_errorbar aes
#' @importFrom magrittr "%>%"

add_ci_2plot <- function(plot,
                         CIs,
                         met = "normal",
                         color = "darkseagreen3",
                         outline = "black",
                         transparency = 0.5,
                         type = "band",
                         lt = "dotted") {

  # globalVariables(c("mpg", "hp", "mpg_div_hp"))
  method <- index <- observed <- lwr <- upr <- NULL

  cis <- CIs %>%
    filter(method == met) %>%
    dplyr::rename(num = index) %>%
    dplyr::select(-method) %>%
    dplyr::mutate(
      type = "Observed Data",
      eigenvalue = observed
    )
  if (type == "band") {
    new_plot <-
      plot + ggplot2::geom_ribbon(
        data = cis,
        ggplot2::aes(ymin = lwr, ymax = upr),
        fill = color,
        color = outline,
        alpha = transparency,
        linetype = lt
      )
  } else if (type == "errorbars") {
    new_plot <-
      plot + ggplot2::geom_errorbar(
        data = cis,
        ggplot2::aes(ymin = lwr, ymax = upr),
        color = outline,
        alpha = transparency,
        width = .15
      )
  }

  return(new_plot)
}
