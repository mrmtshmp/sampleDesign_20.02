#' import Excel data.
#' 2020/10/15

Bibtex <- TRUE

dir.sub <- "./src/R/sub"
fn.sub <- list.files(dir.sub)

for(fn.sub.i in fn.sub) 
  source(sprintf("%s/%s",dir.sub,fn.sub.i))


df.col_info <- 
  read_excel(
    path = sprintf("%s/%s",dir.import_data,fn.import_data),
    sheet = "col_info", na = "NA"
    ) %>%
  data.frame()


var.strata_in <- df.col_info[
  !is.na(df.col_info$strata_in),
  c("col_name",'strata_in')
  ]

var.exposure <- df.col_info[
  df.col_info$exposure=="categorical" & !is.na(df.col_info$exposure),
  "col_name"
  ]

var.exposure.conti <- df.col_info[
  df.col_info$exposure=="conti" & !is.na(df.col_info$exposure),
  "col_name"
  ]

var.trans <- df.col_info[
  !is.na(df.col_info$transform),
  c("transform","col_name")
  ]

var.cutoff <- df.col_info[
  !is.na(df.col_info$cutoff),
  c("col_name","cutoff")
  ]

var.label <- df.col_info[
  !is.na(df.col_info$var.label),
  c("col_name","var.label")
  ]

var.event  <- df.col_info[
  df.col_info$outcome=="event" & !is.na(df.col_info$outcome),
  "col_name"
  ]

var.timetoevent <- df.col_info[
  df.col_info$outcome=="time" & !is.na(df.col_info$outcome),
  "col_name"
  ]

var.cens_timetoevent  <- df.col_info[
  df.col_info$outcome=="censored.time" & !is.na(df.col_info$outcome),
  "col_name"
  ]

var.smd <- df.col_info[
  df.col_info[,StdMeanDiff]=="1" & !is.na(df.col_info[,StdMeanDiff]),
  "col_name"
  ]

var.Psmodel <- df.col_info[
  df.col_info$Psmodel=="1" & !is.na(df.col_info$Psmodel),
  "col_name"
  ]

df.imported_data.completed <- 
  read_excel(
    path = sprintf("%s/%s",dir.import_data,fn.import_data),
    sheet = "data", na = "NA",
    skip=2,
    col_names = df.col_info[!is.na(df.col_info$orig_name),"col_name"],
    col_types = df.col_info[!is.na(df.col_info$orig_name),"col_type"]
    ) %>%
  data.frame()

# for(
#   i in 1:length(var.trans$col_name)
#   ) 
#   df.imported_data.completed[
#     ,
#     var.trans$col_name[i]
#     ] <- try(
#       call(
#         var.trans$transform[i],
#         df.imported_data.completed[,var.trans$col_name[i]]
#         ) %>%
#         eval()
#       )


res.summ.tableOne <-
  tableone::CreateTableOne(
    data= 
      df.imported_data.completed[
        ,
        df.col_info[
          is.na(df.col_info$ID) & !is.na(df.col_info$col_name)& !is.na(df.col_info$orig_name),
          "col_name"
        ]
      ]
  )
  

save(
  list = ls(pattern = "^var\\."),
  df.col_info,
  df.imported_data.completed, 
  res.summ.tableOne,
  file = sprintf("%s/%s",dir.RData,fn.imported_data)
  )

sink('output/Table_1.txt')
print(res.summ.tableOne)
sink()

# quartz(
#   family = 'Arial',
#   type = 'pdf',
#   file = sprintf("output/cov_rel.pairwise.pdf"),
#   width=35,
#   height=35
#   )
# GGally::ggpairs(
#   df.imported_data.completed[
#     ,
#     df.col_info[
#       is.na(df.col_info$ID) & !is.na(df.col_info$col_name)& !is.na(df.col_info$orig_name),
#       "col_name"
#       ]
#     ]
#   )
# dev.off()