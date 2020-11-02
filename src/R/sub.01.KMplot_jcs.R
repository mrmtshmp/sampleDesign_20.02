# Kaplain-Meyer curve.<<JCS>> ----------

# df.working_data_completed.propensityScores_IPW$jcs_num <-
#   nchar(gsub('(^[0-9]{1,3}).+','\\1',df.working_data_completed.propensityScores_IPW$jcs))

data_result <- list()

df.working_data_completed.propensityScores_IPW$ID = 
  df.working_data_completed.propensityScores_IPW[,1]

df.working_data_completed.propensityScores_IPW$var.cens_timetoevent = 
  df.working_data_completed.propensityScores_IPW[,var.cens_timetoevent]

df.working_data_completed.propensityScores_IPW$var.event_for_Surv =
  as.numeric(
    as.character(
      factor(
        df.working_data_completed.propensityScores_IPW[,var.event],
        c("censored","event"),
        c(0,1)
        )
      )
    )


data_result <- mf.Simple_input(
  df.working_data_completed.propensityScores_IPW,
  SNPs = df.col_info[
    df.col_info$col_name==var.exposure & 
      !is.na(df.col_info$col_name),
    "col_name"
    ],
  gt = list(unique(df.working_data_completed.propensityScores_IPW[,var.exposure])),
  gt_levels = list(unique(df.working_data_completed.propensityScores_IPW[,var.exposure])),
  surv= c(
    'var.cens_timetoevent',
    'var.event_for_Surv'
  ),
  xlim=c(0, 600),
  x.label = "Days since diagnosis",
  y.label = "Survival probability (%)",
  break.x.by = 30,
  plot.theme = 
    theme_bw() + 
    theme(
      text = element_text(colour = "black",family = "Ariel",size = 16)
    )
)

KM_plot <- 
  data_result[[2]][[1]]
KM_risk.table <- 
  data_result[[2]][[2]]

result.plot <-
  ggarrange(
    KM_plot,
    KM_risk.table,
    nrow=2,
    heights = c(2,1/2)
  )

list.result.plot <- list(result.plot)
# 
# 
# data_result <- mf.Simple_input(
#   df.ADS.propensityScores_IPW,
#   SNPs = c("treatment"),
#   gt = list(c(1,2)),
#   gt_levels = list(c("AMB+5FC","AMB")),
#   surv= c(
#     "survival_30_days",
#     "X30_days_death0"
#   ),
#   xlim=c(0,31),
#   x.label = "Days since diagnosis",
#   y.label = "Survival probability (%)",
#   break.x.by = 3,
#   plot.theme = 
#     theme_bw() + 
#     theme(
#       text = element_text(colour = "black",family = "Ariel",size = 16)
#     )
# )
# 
# 
# KM_plot <-
#   data_result[[2]][[1]]
# 
# KM_risk.table <-
#   data_result[[2]][[2]]
# 
# result.plot <-
#   ggarrange(
#     KM_plot,
#     KM_risk.table,
#     nrow=2,
#     heights = c(2,1/2)
#   )
# 
# list.result.plot <- list(list.result.plot, result.plot)

quartz(
  type = "pdf",
  file = 
    sprintf(
      fmt = "%s/%s",
      'output',
      "KM_plot_200924.pdf"
    ),
  family = "Arial",
  width  = 4.76*5,
  height = 4.76*5
)
list.result.plot
dev.off()