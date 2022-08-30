path <- file.path(get_golem_wd(), "data")
files <- list.files(path)[!str_detect(list.files(path), "blocks|behavioral|code|0")] %>%
    sort() %>%
    str_replace_all(".rda", "")

for (f in files) {
    load(file.path(get_golem_wd(), "data", paste0(f, ".rda")))
}

blocks <- lapply(files, function(x) as.data.frame(get(x)))
names(blocks) <- toupper(files)

# Keep only the level A and rename the rows
omics <- c("ELISA", "MS", "RNA")
for (i in omics) {
    blocks[[i]] <- filter(blocks[[i]], detect_row(blocks[[i]], "A"))
    rownames(blocks[[i]]) <- get_patient_id(blocks[[i]])
}

blocks[c("CLINIC")] <- blocks[c("CLINIC_TRANSF")]
# blocks <- blocks[c("CLINIC")]
diseases <- c("control", "Still", "juvenile", "osteitis")
inds <- (clinic %>% filter(str_detect(disease, paste(c(diseases[2:3]), collapse = "|"))))$immun_aid_identifier
blocks[["CLINIC"]] <- blocks[["CLINIC"]][rownames(blocks[["CLINIC"]]) %in% inds, ]

blocks <- blocks[c("RNA", "CLINIC")]
common_rows <- Reduce(intersect, lapply(blocks, row.names))
blocks <- lapply(blocks, function(x) clean_data(x[common_rows, ], FALSE))

# Select the numerical variables
blocks[["ELISA"]] <- select(blocks[["ELISA"]], c(4, 5))
levels(blocks[["CLINIC"]]$gender) <- c(0, 1)
blocks[["CLINIC"]] <- get_name_num(blocks[["CLINIC"]]) %>%
    select(blocks[["CLINIC"]], .)

# All cofonding variables in numeric
blocks[["CLINIC"]]$gender <- as.numeric(as.character(blocks[["CLINIC"]]$gender))
row_names <- rownames(blocks[["CLINIC"]])
# Transform
blocks[["CLINIC"]][, -seq(1)] <- as.data.frame(sapply(blocks[["CLINIC"]][, -seq(1)], log1p))
rownames(blocks[["CLINIC"]]) <- row_names

# Remove missing samples from cofonding variables
# vars <- c("gender", "age_at_inclusion_tim", "bmi_automatic")
vars <- c("gender", "age_at_inclusion_time", "BMI")
to_remove <- sapply(vars, function(i) which(is.na(blocks[["CLINIC"]][, i])))
to_remove <- unique(Reduce(c, to_remove))
if (length(to_remove) > 0)
    blocks <- lapply(blocks, function(i) i[-to_remove, ])
# clinic[to_remove, ] %>% select(1:3, vars)
cl <- blocks[["CLINIC"]]

# Weight by the cofunding effect residuals
blocks.df <- lapply(
    blocks,
    function(x) {
        lapply(
            seq(ncol(x)),
            function(y) {
                  lm(x[, y] ~ cl$age_at_inclusion_time + cl$BMI, na.action = "na.exclude")$residuals
              }
        )
    }
)

# Position of the NA values in blocks
for (k in seq(length(blocks))) {
    listNA <- which(is.na(blocks[[k]]), arr.ind = TRUE)

    # Insert NA
    for (i in unique(listNA[, 2])) {
        for (j in listNA[which(listNA[, 2] == i), 1] - 1) {
              blocks.df[[k]][[i]] <- append(
                  blocks.df[[k]][[i]],
                  NA,
                  j
              )
          }
    }

    # Convert in matrix
    blocks.df[[k]] <- as.data.frame(
        matrix(
            unlist(blocks.df[[k]]),
            nrow = nrow(blocks[[k]]),
            ncol = ncol(blocks[[k]]),
            dimnames = list(row.names(blocks[[k]]), colnames(blocks[[k]]))
        )
    )
}
blocks <- blocks.df
blocks[["CLINIC"]] <- select(blocks[["CLINIC"]], -seq(6))

blocks <- lapply(blocks, function(x) clean_data(x, FALSE))
# blocks_clinic <- blocks
use_data(blocks, overwrite = TRUE)

# plot_violin(blocks[["CLINIC"]] %>% select(1:4), colors = rep("blue", 200))
# plot_histo(blocks[["CLINIC"]], colors = rep("blue", 250))
# plot_network(blocks, colors =  c("#eee685", "white"))
