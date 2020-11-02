#' Weighted Cox proportional hazard regression.
#' 2020/10/16

Bibtex <- TRUE

dir.sub <- "./src/R/sub"
fn.sub <- list.files(dir.sub)

for(fn.sub.i in fn.sub) source(sprintf("%s/%s",dir.sub,fn.sub.i))

##```
  
load(
  file = sprintf("%s/%s", dir.RData, fn.imported_data)
  )

load(
  file = sprintf("%s/%s",dir.RData,"res.cutoff_for_alloc.RData")
  )

df.working_data <- df.imported_data.completed


df.working_data$pres.cutoff_for_alloc <- predict(res.cutoff_for_alloc)[,2]

# Propensity score model ----------

fml.ps_model <-
  sprintf(
    '%s ~ %s',
    var.exposure,
    paste(
      var.Psmodel,
      collapse = "+"
      )
    )

for(i in 1:nrow(var.label)){
  df.working_data[,var.label[i,"col_name"]] <-
  factor(
    df.working_data[,var.label[i,"col_name"]],
    unique(df.working_data[,var.label[i,"col_name"]])[
      order(
        unique(df.working_data[,var.label[i,"col_name"]])
        )
      ],
    unlist(
      strsplit(
        gsub("^\\{(.+)\\}$","\\1",var.label[i,"var.label"]),
        split = "\\}\\{"
        )
      )
    )
  }

# Load source files of functions.

for(i in 1:nrow(var.strata_in)){
  df.working_data <-
    df.working_data[
      as.character(df.working_data[
        ,
        var.strata_in[i,"col_name"]
        ]) %in% 
        unlist(
          strsplit(
            gsub("^\\{(.+)\\}$","\\1",var.strata_in[i,"strata_in"]),
            split = "\\}\\{"
            )
          )
    ,]
  }


for(i in 1:nrow(var.strata_in)){
  df.working_data <-
    df.working_data[
      df.working_data[
        ,
        var.strata_in[i,"col_name"]
        ] %in% 
        unlist(
          strsplit(
            gsub("^\\{(.+)\\}$","\\1",var.strata_in[i,"strata_in"]),
            split = "\\}\\{"
            )
          )
    ,]
  }

df.working_data_completed <-
  df.working_data[
    !is.na(df.working_data[,var.cens_timetoevent]),
    ]

df.working_data_completed[,var.exposure] <-
  droplevels(df.working_data_completed[,var.exposure])


## Construct a table
tabUnmatched <-
  CreateTableOne(
    vars = var.smd, 
    strata = var.exposure,
    data = 
      df.working_data_completed, 
    test = FALSE
    )
## Show table with SMD
sink(
  "output/TableOne.txt"
  )
print(tabUnmatched, smd = TRUE)
sink()


# Propensity score model ---------------

propensityScoreModel <-
  glm(
    fml.ps_model,
    family  = binomial(link = "logit"),
    data    = 
      df.working_data_completed,
    na.action = na.exclude
    )

df.working_data_completed.propensityScores <-
  df.working_data_completed %>%
  data.frame()

df.working_data_completed.propensityScores$propensity_score <- 
  predict(
    propensityScoreModel,
    type= "response",
    # type= "prob",
    na.action = na.exclude()
    ) %>%
    # )[,2] %>%
  unlist()

df.working_data_completed.propensityScores_IPW <-
  IPW_weights(
    treatment = 
      as.numeric(
        df.working_data_completed.propensityScores[,var.exposure]
        ),
    propensity_score = 
      df.working_data_completed.propensityScores$propensity_score,
    dat = df.working_data_completed.propensityScores
    ) %>%
  dplyr::filter(
    !is.na(propensity_score)
    )

res.roc.propensity_score <- roc(
  response = 
    as.factor(
      df.working_data_completed.propensityScores[,var.exposure]
      ),
  predictor = 
    df.working_data_completed.propensityScores$propensity_score
  )

# Plot distribution of propensity score and weighted counts. ----------------
#'
#'

df.working_data_completed.propensityScores_IPW$y_for_dots <-
  (
    as.numeric(
      df.working_data_completed.propensityScores_IPW[,var.exposure]
      )/4
    )*5

df.working_data_completed.propensityScores_IPW$var.exposure <-
  df.working_data_completed.propensityScores_IPW[,var.exposure]
df.working_data_completed.propensityScores_IPW$factor.var.exposure <- 
  df.working_data_completed.propensityScores_IPW[,var.exposure]

ggdata.propensityScores <-
  ggplot(
    data =
      df.working_data_completed.propensityScores_IPW,
    aes(
      x = propensity_score
      )
    )

ggdata.propensityScores.weighted_count <-
  ggplot(
    data =
      df.working_data_completed.propensityScores_IPW %>%
      pivot_longer(
        cols =
          c(starts_with("w_at")),
        values_to = "Weight",
        names_to = "target_pop"
      ) %>%
      dplyr::filter(
        target_pop == "w_ato"
        ),
    aes(
      x = propensity_score,
      weight = Weight
    )
  )

pdf(
  file = 
    "output/IPWcount.pdf",
  width = 21
  )
plot(
  ggdata.propensityScores + 
    geom_density(
      aes(
        fill = factor.var.exposure
        ),
      bw="SJ",
#      binwidth = FD,
      alpha=0.5,
      position="identity"
    ) +
  geom_point(
    aes(
      y=y_for_dots,
      x=propensity_score,
      color=factor.var.exposure
      ),
    size=1
    ) +
  labs(
    color =
      df.col_info[
        df.col_info$col_name == var.exposure &
          !is.na(df.col_info$col_name),
        "col_label"
        ],
    fill =
      df.col_info[
        df.col_info$col_name == var.exposure &
          !is.na(df.col_info$col_name),
        "col_label"
        ]
    ) +
  theme_bw()
)

ggdata.propensityScores + 
  stat_ecdf(
    aes(
      color = factor.var.exposure
      ),
    alpha=0.5,
    position="identity"
  ) +
  geom_point(
    aes(
      y=as.numeric(var.exposure)/4,
      x=propensity_score,
      color=factor.var.exposure
      ),
    size=1
    ) +
  labs(
    color =
      df.col_info[
        df.col_info$col_name == var.exposure &
          !is.na(df.col_info$col_name),
        "col_label"
        ]
    ) +
  theme_bw()

plot(
  ggdata.propensityScores.weighted_count +
    geom_density(
      aes(
        fill= factor.var.exposure
        ),
      bw="SJ",
      alpha=0.5
      ) +
    facet_grid(~target_pop) +
    theme_bw()
  )
plot(
  res.roc.propensity_score,
  print.thres=TRUE 
  )
legend(
  x = 0.6, y=0.5,
  cex = 0.7, 
  legend = c(
    sprintf(
      "AUC = %s (0.95CI: %s, %s)",
      round(auc(res.roc.propensity_score),3),
      round(ci(auc(res.roc.propensity_score))[1],3),
      round(ci(auc(res.roc.propensity_score))[3],3)
    )
  ),
  bty = "n"
)
dev.off()


# Balancing assessment ----------------------------------------------------

#' The standardized mean differences between the two groups of the patients 
#' in each of covariates those which 
#' The tableone package (tableone_CRAN.bib)


#' Reference:
#' https://cran.r-project.org/web/packages/tableone/vignettes/smd.html
#' (Many lines were snipped from above website [accessed:2020/08/31])

res.svydesign.w_ato <- 
  survey::svydesign(
    ids = ~ 1, 
    data =
      df.working_data_completed.propensityScores_IPW[
        !is.na(df.working_data_completed.propensityScores_IPW$propensity_score),
        ],
    weights = ~ w_ato
    )
  
## Construct a table
tabWeighted.w_ato <- 
  svyCreateTableOne(
    vars = var.smd,
    strata = var.exposure, 
    data = res.svydesign.w_ato, 
    test = FALSE
    )
## Show table with SMD

sink(
  "output/TableOne.txt"
  )
print(tabWeighted.w_ato, smd = TRUE)
sink()


## Construct a data frame containing variable name and SMD from all methods
dataPlot <- 
  data.frame(
    variable  = rownames(ExtractSmd(tabUnmatched)),
    rawdata = as.numeric(ExtractSmd(tabUnmatched)),
    weighted_data = as.numeric(ExtractSmd(tabWeighted.w_ato))
    )
  
## Create long-format data for ggplot2
dataPlotMelt <-
  melt(
    data          = dataPlot,
    id.vars       = c("variable"),
    variable.name = "Method",
    value.name    = "SMD"
    ) %>%
  left_join(
    df.col_info,
    by=c("variable"="col_name")
    )

## Order variable names by magnitude of SMD
varNames <- unique(
  as.character(
    dataPlotMelt[
      dataPlotMelt$Method=="rawdata",
      "col_label"]
    )[
    order(
      dataPlotMelt[
        dataPlotMelt$Method=="rawdata",
        "SMD"]
      )
    ]
  )


## Order factor levels in the same order
dataPlotMelt$col_label <- 
  factor(
    dataPlotMelt$col_label,
    levels = varNames
    )

## Plot using ggplot2

quartz(
  family = "Arial",type = "pdf",
  file =   "output/smd.pdf")
ggplot(
  data = dataPlotMelt,
  mapping = aes(x = col_label, y = SMD, group = Method, color = Method)
  ) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() + theme(legend.key = element_blank())
ggplot(
  data = dataPlotMelt,
  mapping = aes(x = col_label, y = SMD, group = Method, color = Method)
  ) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() + theme(legend.key = element_blank())
dev.off()

#' Treatment effect infference.
#'

df.working_data_completed.propensityScores_IPW$time <-
  df.working_data_completed.propensityScores_IPW[,var.cens_timetoevent]

df.working_data_completed.propensityScores_IPW$event <-
  as.numeric(
    as.character(
      factor(
        df.working_data_completed.propensityScores_IPW[,var.event],
        c("censored","event"),
        c(0,1)
        )
      )
    )

df.working_data_completed.propensityScores_IPW$exposure <-
    df.working_data_completed.propensityScores_IPW[,var.exposure]

res.svydesign.w_ato <- 
  survey::svydesign(
    ids = ~ 1, 
    data =
      df.working_data_completed.propensityScores_IPW[
        !is.na(df.working_data_completed.propensityScores_IPW$propensity_score),
        ],
    weights = ~ w_ato
    )

svy.cox.fit <- 
  svycoxph(
    formula = 
      Surv(
        time = time,
        event= event
        ) ~ exposure,
    x = TRUE, 
    design = res.svydesign.w_ato
    )

res.coxph.Weighted <-
  coxph(
    formula = 
      Surv(
        time = 
          df.working_data_completed.propensityScores_IPW[,var.cens_timetoevent],
        event= 
          as.numeric(
            as.character(
              factor(
                df.working_data_completed.propensityScores_IPW[,var.event],
                c("censored","event"),
                c(0,1)
                )
              )
            )
        ) ~ df.working_data_completed.propensityScores_IPW[,var.exposure],
    weights = 
      df.working_data_completed.propensityScores_IPW[
        ,
        "w_ato"
        ]
    )

sink("output/res.survey_CoxPH.txt")
summary(svy.cox.fit)
sink()

#```