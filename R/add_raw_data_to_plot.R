
#' @keywords internal
.add_raw_data_to_plot <- function(p,
                                  x,
                                  rawdat,
                                  label.data,
                                  ci.style,
                                  dot.alpha,
                                  dot.size,
                                  dodge,
                                  jitter,
                                  jitter.miss,
                                  colors,
                                  verbose = TRUE) {
  insight::check_if_installed("ggplot2", reason = "to produce plots of adjusted predictions")
  .data <- NULL

  # we need an own aes for this
  # we plot rawdata first, so it doesn't overlay the
  # dots / lines for marginal effects

  if (!is.null(rawdat)) {
    # recode binary response to numeric? if so, make sure it starts with 0
    if (identical(attributes(x)$logistic, "1")) {
      lowest <- 0
    } else {
      lowest <- NULL
    }
    # make sure response is numeric
    rawdat$response <- .factor_to_numeric(rawdat$response, lowest = lowest)

    # transform response when offset is used, to match predictions
    offset_term <- attr(x, "offset", exact = TRUE)
    if (!is.null(offset_term)) {
      fixed_offset <- attributes(x)$condition
      if (offset_term %in% names(fixed_offset)) {
        fixed_value <- fixed_offset[[offset_term]]
        offset_values <- attributes(x)$offset_values
        rawdat$response <- (rawdat$response / offset_values) * fixed_value
      }
    }

    # check if we have a group-variable with at least two groups
    if (.obj_has_name(rawdat, "group")) {

      # we need to make sure that scale of raw data matches scale of predictions
      if (isTRUE(attr(x, "continuous.group"))) {
        rawdat$group_col <- as.numeric(as.character(rawdat$group))
      } else {
        rawdat$group_col <- rawdat$group
      }

      rawdat$group <- as.factor(rawdat$group)
      grps <- .n_distinct(rawdat$group) > 1
    } else {
      grps <- FALSE
    }

    # check if we have only selected values for groups, in this case
    # filter raw data to match grouping colours
    if (grps && isFALSE(attr(x, "continuous.group")) && .n_distinct(rawdat$group) > .n_distinct(x$group)) {
      rawdat <- rawdat[which(rawdat$group %in% x$group), , drop = FALSE]
    }

    # if we have groups, add colour aes, to map raw data to grouping variable
    if (grps) {
      mp <- ggplot2::aes(
        x = .data[["x"]],
        y = .data[["response"]],
        colour = .data[["group_col"]],
        shape = .data[["group_col"]],
      )
    } else {
      mp <- ggplot2::aes(
        x = .data[["x"]],
        y = .data[["response"]]
      )
    }

    # no jitter? Tell user about overlap
    if ((is.null(jitter) || isTRUE(all(jitter == 0))) && verbose) {
      insight::format_alert("Data points may overlap. Use the `jitter` argument to add some amount of random variation to the location of data points and avoid overplotting.") # nolint
    }

    # for binary response, no jittering by default
    if ((attr(x, "logistic", exact = TRUE) == "1" && jitter.miss) || is.null(jitter)) {
      p <- p + ggplot2::geom_point(
        data = rawdat,
        mapping = mp,
        alpha = dot.alpha,
        size = dot.size,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else {

      # no jitter
      if (is.null(jitter) || isTRUE(all(jitter == 0))) {
        jitter <- c(0, 0)
      }

      # if we have error bars, these are dodged, so we need to dodge the
      # data points as well
      if (ci.style == "errorbar") {
        if (grps) {
          p <- p + ggplot2::geom_point(
            data = rawdat,
            mapping = ggplot2::aes(x = .data[["x"]], y = .data[["response"]], colour = .data[["group_col"]]),
            alpha = dot.alpha,
            size = dot.size,
            position = ggplot2::position_jitterdodge(
              jitter.width = jitter[1],
              jitter.height = jitter[2],
              dodge.width = dodge
            ),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        } else {
          p <- p + ggplot2::geom_point(
            data = rawdat,
            mapping = ggplot2::aes(x = .data[["x"]], y = .data[["response"]], fill = .data[["group_col"]]),
            alpha = dot.alpha,
            size = dot.size,
            position = ggplot2::position_jitterdodge(
              jitter.width = jitter[1],
              jitter.height = jitter[2],
              dodge.width = dodge
            ),
            show.legend = FALSE,
            inherit.aes = FALSE,
            color = colors[1]
          )
        }
      } else {
        p <- p + ggplot2::geom_jitter(
          data = rawdat,
          mapping = mp,
          alpha = dot.alpha,
          size = dot.size,
          width = jitter[1],
          height = jitter[2],
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      }
    }
    if (label.data) {
      if (grps) {
        mp2 <- ggplot2::aes(
          x = .data[["x"]],
          y = .data[["response"]],
          label = .data[["rowname"]],
          colour = .data[["group_col"]]
        )
      } else {
        mp2 <- ggplot2::aes(
          x = .data[["x"]],
          y = .data[["response"]],
          label = .data[["rowname"]]
        )
      }
      if (insight::check_if_installed("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          data = rawdat,
          mapping = mp2,
          alpha = dot.alpha,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      } else {
        p <- p + ggplot2::geom_text(
          data = rawdat,
          mapping = mp2,
          alpha = dot.alpha,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      }
    }
  } else if (verbose) {
    message("Raw data not available.")
  }

  p
}
