# Extract all the Rdata
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

# Select by disease
blocks[c("CLINIC")] <- blocks[c("CLINIC_TRANSF")]
diseases <- c("control", "Still", "juvenile", "osteitis")
inds <- (clinic %>% filter(str_detect(disease, paste(c(diseases[2:3]), collapse = "|"))))$immun_aid_identifier
blocks[["CLINIC"]] <- blocks[["CLINIC"]][rownames(blocks[["CLINIC"]]) %in% inds, ]

# Intersection between blocks
blocks <- blocks[c("CLINIC")]
common_rows <- Reduce(intersect, lapply(blocks, row.names))
blocks <- lapply(blocks, function(x) clean_data(x[common_rows, ], FALSE))

# All cofonding variables in numeric
levels(blocks[["CLINIC"]]$gender) <- c(0, 1)
blocks[["CLINIC"]]$gender <- as.numeric(as.character(blocks[["CLINIC"]]$gender))

# Select the numerical variables
blocks[["ELISA"]] <- select(blocks[["ELISA"]], c(4, 5))
blocks[["CLINIC"]] <- get_name_num(blocks[["CLINIC"]]) %>%
    select(blocks[["CLINIC"]], .)

# Remove missing samples from cofonding variables
vars <- c("gender", "age_at_inclusion_time", "BMI")
vars2 <- c("immun_aid_identifier", "weight", "height")
# vars <- c("gender", "age_at_inclusion_time", "BMI")
to_remove <- sapply(vars, function(i) which(is.na(blocks[["CLINIC"]][, i])))
to_remove <- unique(Reduce(c, to_remove))
if (length(to_remove) > 0)
    blocks <- lapply(blocks, function(i) i[-to_remove, ])

# clinic[to_remove, ] %>% select(1:3, vars)
 blocks[["CLINIC"]] <- blocks[["CLINIC"]] %>%
    best_na_percent(50) %>%
    select(-c(vars2))
cl <- blocks[["CLINIC"]]

# Transform
row_names <- rownames(blocks[["CLINIC"]])
blocks[["CLINIC"]][, -seq(1)] <- as.data.frame(sapply(blocks[["CLINIC"]][, -seq(1)], log1p))
rownames(blocks[["CLINIC"]]) <- row_names

# Weight by the cofunding effect residuals
blocks.df <- lapply(
    blocks,
    function(x) {
        lapply(
            seq(ncol(x)),
            function(y) {
                  lm(x[, y] ~ cl$gender + cl$age_at_inclusion_time + cl$BMI, na.action = "na.exclude")$residuals
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
blocks[["CLINIC"]] <- select(blocks[["CLINIC"]], -all_of(vars))

blocks <- lapply(blocks, function(x) clean_data(x, FALSE))
blocks_clinic <- blocks
use_data(blocks_clinic, overwrite = TRUE)

# plot_violin(blocks[["CLINIC"]] %>% select(1:4), colors = rep("blue", 200))
# plot_histo(blocks[["CLINIC"]], colors = rep("blue", 250))
# plot_network(blocks, colors =  c("#eee685", "white"))
