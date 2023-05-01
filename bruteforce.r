get_rmse <- function(observed, prediction) {
    return(sqrt(mean((observed - prediction)^2)))
}

fit_summary <- function(formula, train, test) {
    data_fit <- lm(formula, data=train)

    data_prediction <- predict(data_fit, test)

    data_rmse <- get_rmse(test$Members, data_prediction)

    summary_list <- list()

    summary_list$AIC <- AIC(data_fit)
    summary_list$RMSE <- data_rmse
    summary_list$fit <- data_fit

    return(summary_list)
}

join_formulae <- function(formula1, formula2, operator) {
    f1_str <- as.character(formula1)
    f2_str <- as.character(formula2)

    dependant_variable <- f1_str[2]
    predictors1 <- f1_str[3]
    predictors2 <- f2_str[3]

    return(as.formula(paste(dependant_variable, '~', predictors1, operator, predictors2, sep=' ')))
}

stringify_formula <- function(formula) {
    f_str <- as.character(formula)

    return(paste(f_str[2], '~', f_str[3]))
}

random_formula <- function() {
    formulae <- c(Members ~ TV, Members ~ Internet, Members ~ Mailing, Members ~ Region)

    set.seed(Sys.time())
    random_num <- runif(n=1, min=1, max=length(formulae))

    return(formulae[round(random_num)][[1]])
}

random_operator <- function() {
    ops <- c('+', '*')

    set.seed(Sys.time())
    random_num <- runif(n=1, min=1, max=length(ops))

    return(ops[round(random_num)])
}

find_best_fit <- function(seed, checks) {
    marketing <- read.csv(file.choose())

    train_sample_size <- floor(0.8 * nrow(marketing))

    set.seed(seed)
    split_index <- sample(seq_len(nrow(marketing)), size=train_sample_size)

    train <- marketing[split_index, ]
    test <- marketing[-split_index, ]   

    tested <- c()
    best_model <- list()
    best_model$RMSE <- 100000

    i <- 0
    while (i < checks) {
        new_formula <- random_formula()
        previous_formula <- stringify_formula(new_formula)

        while (stringify_formula(new_formula) %in% tested) {
            second_formula <- random_formula()
            op <- random_operator()

            while (stringify_formula(second_formula) == previous_formula) {
                second_formula <- random_formula()
            }

            previous_formula <- stringify_formula(second_formula)
            new_formula <- join_formulae(new_formula, second_formula, op)
        }

        tested <- append(tested, stringify_formula(new_formula))

        model <- fit_summary(new_formula, train, test)

        if (model$RMSE < best_model$RMSE) {
            best_model <- model
            best_model$formula <- new_formula
        }

        print(paste(i, '/', checks))
        i = i + 1
    }

    return(best_model)
}
