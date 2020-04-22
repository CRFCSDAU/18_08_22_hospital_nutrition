
# Extract labels from class = labelled data

  extract.labels <- function(x){attributes(attributes(x)$labels)$names}


# Table functions


  tests.1 <- function(data, ...) {

    tests.list <- list()

    require(dplyr)
    require(broom)

    for (j in seq_along(data)) {

      if(is.numeric(data[[j]])){

        t <- aov(data[[j]] ~ arm, data) %>%
          tidy()

        tests.list[[j]] <- round(t$p.value[1], 2)
      }

      if(is.factor(data[[j]])){

        c <- table(data[[j]], data$arm) %>%
          chisq.test() %>%
          tidy()

        tests.list[[j]] <- c(round(c$p.value[1], 2),
                             rep("", length(levels(data[[j]]))))
      }

    }
    unlist(tests.list)
  }



  tests.2 <- function(data, ...) {

    tests.list <- list()

    require(dplyr)
    require(broom)

    for (j in seq_along(data)) {

      if(is.numeric(data[[j]])){

        k <- kruskal.test(data[[j]] ~ arm, data) %>%
          tidy()

        tests.list[[j]] <- round(k$p.value[1], 2)
      }

      if(is.factor(data[[j]])){

        c <- table(data[[j]], data$arm) %>%
          chisq.test() %>%
          tidy()

        tests.list[[j]] <- c(round(c$p.value[1], 2),
                             rep("", length(levels(data[[j]]))))
      }

    }
    unlist(tests.list)
  }



# Generate the list of names for the table


  name.1 <- function(x, ...) {

    var.names <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        var.names[[i]] <- names(x[i])
      }

      if(is.factor(x[[i]])){
        var.names[[i]] <- c(names(x[i]), levels(x[[i]]))
      }
    }

    unlist(var.names)
  }



# Means(sds) or counts(%)

  summary.1 <- function(x, ...) {

    summary.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        summary.list[[i]] <- paste0(round(mean(x[[i]], na.rm = TRUE), 1),
                                    " \u00B1 ",
                                    round(sd(x[[i]],   na.rm = TRUE), 1))
      }

      if(is.factor(x[[i]])){
        summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                          " (",
                                          round(table(x[[i]]) /
                                                  sum(table(x[[i]])), 3) * 100,
                                          "%)"))
      }

    }
    unlist(summary.list)
  }


  summary.2 <- function(x, ...) {

    summary.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        summary.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.50),
                                                   na.rm = TRUE), 1),
                                    " [",
                                    round(quantile(x[[i]], probs = c(0.25),
                                                   na.rm = TRUE), 1),
                                    ", ",
                                    round(quantile(x[[i]], probs = c(0.75),
                                                   na.rm = TRUE), 1),
                                    "]")
      }

      if(is.factor(x[[i]])){
        summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                          " (",
                                          round(table(x[[i]]) /
                                                  sum(table(x[[i]])), 3) * 100,
                                          "%)"))
      }

    }
    unlist(summary.list)
  }

# Missing observations

  n.miss <- function(x, ...) {

    miss.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        miss.list[[i]] <- length(x[[i]][!is.na(x[[i]])])
      }

      if(is.factor(x[[i]])){
        miss.list[[i]] <- c(length(x[[i]][!is.na(x[[i]])]),
                            rep("", length(levels(x[[i]]))))
      }

    }
    unlist(miss.list)
  }


# Min and max

  min.max <- function(x, ...) {

    min.max.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        min.max.list[[i]] <- paste0("(",
                                    round(min(x[[i]], na.rm = TRUE), 1),
                                    ", ",
                                    round(max(x[[i]], na.rm = TRUE), 1),
                                    ")")
      }

      if(is.factor(x[[i]])){
        min.max.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
      }

    }
    unlist(min.max.list)
  }


# Quartiles

  tiles <- function(x, ...) {

    quantiles.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        quantiles.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.25),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.50),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.75),
                                                     na.rm = TRUE), 1))
      }

      if(is.factor(x[[i]])){
        quantiles.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
      }

    }
    unlist(quantiles.list)
  }


# Median, IQR

  med.iqr <- function(x, ...) {

    quantiles.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        quantiles.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.5),
                                                     na.rm = TRUE), 1),
                                      " (",
                                      round(quantile(x[[i]], probs = c(0.25),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.75),
                                                     na.rm = TRUE), 1),
                                      ")")
      }

      if(is.factor(x[[i]])){
        quantiles.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
      }

    }
    unlist(quantiles.list)
  }

