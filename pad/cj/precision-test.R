library(xgboost)
library(jsonlite)
library(float)

# set display options to show 12 digits
options(digits=12)


dates <- c(30, 30, 30,
           30, 30, 30,
           31, 31, 31,
           31, 31, 31,
           31, 31, 31,
           34, 34, 34)

labels <- c(1, 1, 1,
            1, 1, 1,
            0, 0, 0,
            0, 0, 0,
            0, 0, 0,
            0, 0, 0)

data <- data.frame(dates = dates, labels=labels)

bst <- xgboost(
  data = as.matrix(data$dates),
  label = labels,
  nthread = 2,
  nrounds = 1,
  objective = "binary:logistic",
  missing = NA,
  max_depth = 1
)
bst_preds <- predict(bst,as.matrix(data$dates))
table(bst_preds)

# display the json dump string
cat(xgb.dump(bst, with_stats = FALSE, dump_format='json'))

#dump to json, import the json model
bst_json <- xgb.dump(bst, with_stats = FALSE, dump_format='json')
bst_from_json <- jsonlite::fromJSON(bst_json, simplifyDataFrame = FALSE)
node <- bst_from_json[[1]]
bst_from_json_preds <- 1/(1+exp(-1*ifelse(data$dates<node$split_condition,node$children[[1]]$leaf,node$children[[2]]$leaf)))
table(bst_from_json_preds)

# test that values are equal
bst_preds - bst_from_json_preds
stopifnot(bst_preds - bst_from_json_preds == 0)
stopifnot(all(fl(bst_preds) == fl(bst_from_json_preds)))



bst_from_json_flpreds <- ifelse(as.numeric(fl(data$dates))<as.numeric(fl(node$split_condition)),
                                as.numeric(fl(1)/(fl(1)+exp(fl(-1)*fl(node$children[[1]]$leaf)))),
                                as.numeric(fl(1)/(fl(1)+exp(fl(-1)*fl(node$children[[2]]$leaf))))
)
table(bst_from_json_flpreds)
bst_preds == bst_from_json_flpreds


