# Mailroom ----------------------------------------------------------------

# library(rules)
# library(plsmod)
# library(finetune)
# library(Cubist)
# library(stacks)

# Extra Code ----------------------------------------------------------------


# nnet_spec <-
#   mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
#   set_engine("nnet", MaxNWts = 2600) |>
#   set_mode("classification")

# nnet_param <-
#   nnet_spec %>%
#   extract_parameter_set_dials() %>%
#   update(hidden_units = hidden_units(c(1, 27)))
#
# c5boost_spec <- boost_tree(trees = tune(), min_n = tune(), sample_size = tune()) %>%
#   set_engine("C5.0") %>%
#   set_mode("classification")
#
# c5_spec <- C5_rules(trees = tune(),min_n = tune()) %>%
#   set_engine("C5.0") %>%
#   set_mode("classification")
#
# naive_spec <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>%
#   set_engine("naivebayes")
#
# pls_spec <- pls(num_comp = tune(), predictor_prop = tune()) %>%
#   set_engine("mixOmics") %>%
#   set_mode("classification")
#
# rf_spec <- rand_forest(mtry = tune(), trees = tune(),min_n = tune()) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")
#
# workflows <-
#   workflow_set(
#     preproc = list(full = slots_predict_recipe),
#     models = list(xgb = xgb_spec, nnet = nnet_spec, c5boost = c5boost_spec,
#                   c5 = c5_spec, naive = naive_spec, pls = pls_spec, rf = rf_spec))



# Sets --------------------------------------------------------------------

# doParallel::registerDoParallel(cores = 5)
# race_ctrl <-
#    control_race(
#       save_pred = TRUE,
#       parallel_over = "everything",
#       save_workflow = TRUE
#    )
#
# race_results <-
#    workflows  |>
#    workflow_map(
#       "tune_race_anova",
#       seed = 11042004,
#       resamples = folds,
#       grid = 25,
#       control = race_ctrl
#    )
# stacks <- stacks() |>
#   add_candidates(race_results)
#
# set.seed(11042004)
# ensemble <- blend_predictions(stacks)
#
# autoplot(ensemble, "weights") +
#   geom_text(aes(x = weight + 0.01, label = model), hjust = 0) +
#   theme(legend.position = "none") +
#   lims(x = c(-0.01, 0.8))
#
# beepr::beep("fanfare")
#
# autoplot(
#   race_results,
#   rank_metric = "roc_auc",
#   metric = "roc_auc",
#   select_best = TRUE
# ) +
#   geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
#   lims(y = c(3.0, 9.5)) +
#   theme(legend.position = "none")
#
# ensemble <- fit_members(ensemble)
#
# ens_test_pred <-
#   predict(ensemble, slots_predict_test) |>
#   bind_cols(slots_predict_test)
# reg_metrics <- metric_set(roc_auc)
# ens_test_pred |>
#   reg_metrics(direction_break, .pred_class)




ggplot_imp <- function(...) {
  {
    obj <- list(...)
    metric_name <- attr(obj[[1]], "loss_name")
    metric_lab <- paste(
      metric_name,
      "after permutations\n(higher indicates more important)"
    )

    full_vip <- bind_rows(obj) %>%
      filter(variable != "_baseline_")

    perm_vals <- full_vip %>%
      filter(variable == "_full_model_") %>%
      group_by(label) %>%
      summarise(dropout_loss = mean(dropout_loss))

    p <- full_vip %>%
      filter(variable != "_full_model_") %>%
      mutate(variable = fct_reorder(variable, dropout_loss)) %>%
      ggplot(aes(dropout_loss, variable))
    if (length(obj) > 1) {
      p <- p +
        facet_wrap(vars(label)) +
        geom_vline(
          data = perm_vals, aes(xintercept = dropout_loss, color = label),
          linewidth = 1.4, lty = 2, alpha = 0.7
        ) +
        geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
    } else {
      p <- p +
        geom_vline(
          data = perm_vals, aes(xintercept = dropout_loss),
          linewidth = 1.4, lty = 2, alpha = 0.7
        ) +
        # geom_boxplot(fill = "#91CBD765", alpha = 0.4)
        geom_boxplot(fill = "#B80A50", alpha = 0.4)
    }
    p +
      theme(legend.position = "none") +
      labs(
        x = metric_lab,
        y = NULL, fill = NULL, color = NULL
      )
  } +
    theme_bw() +
    theme(axis.text.y = element_text(size = 13))
}
