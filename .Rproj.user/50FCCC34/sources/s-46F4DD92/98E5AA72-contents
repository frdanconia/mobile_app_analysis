VarSummary <- function(dataset) {
    
    var.names.summary.df <- data.frame()
    
    for (i in colnames(dataset)) {
      var.class <- class(dataset[, i])
      
      if (any(var.class == c("integer", "numeric"))) {
        cat("\n\t", i, "\n\n")
        
        cat("Variable type / class:", tmp.class <- var.class, "\n")
        
        cat(
          "Number of NA:",
          tmp.NA <-
            sum(is.na(dataset[, i]) &
                  !is.nan(dataset[, i])),
          " (",
          round(tmp.NA.pct <-
                  tmp.NA / length(dataset[, i]) * 100, digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of NaN:",
          tmp.NaN <-
            sum(is.nan(dataset[, i])),
          " (",
          round(tmp.NaN.pct <-
                  tmp.NaN / length(dataset[, i]) * 100, digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of Inf:",
          tmp.Inf <- sum(is.infinite(dataset[, i])),
          " (",
          round(tmp.Inf.pct <-
                  tmp.Inf / length(dataset[, i]) * 100, digits = 0),
          "% )",
          "\n"
        )
        
        cat("Number of unique values / categories:",
            tmp.unique <- length(unique(dataset[, i])),
            "\n")
        
        tbl.hlp <-
          as.data.frame(table(factor(dataset[, i]), useNA = "ifany"))
        
        rnms.hlp <- as.character(tbl.hlp[, 1])
        
        rnms.hlp[is.na(rnms.hlp)] <- "NA"
        
        tbl.hlp <-
          cbind(tbl.hlp[, 2], round(tbl.hlp[, 2] / sum(tbl.hlp[, 2]), digits = 2))
        
        newordr.hlp <- order(tbl.hlp[, 1], decreasing = TRUE)
        
        if (length(newordr.hlp) > 1) {
          tbl.hlp <- data.frame(tbl.hlp[newordr.hlp,])
          
          rnms.hlp <- rnms.hlp[newordr.hlp]
        }
        
        colnames(tbl.hlp) <- c("Freq", "Prop")
        
        row.names(tbl.hlp) <- rnms.hlp
        
        cat("Dominant value:", tmp.dominant.name <-
              rnms.hlp[1], "\n")
        
        cat(
          "Number of dominant value: ",
          tmp.dominant <- tbl.hlp[1, 1],
          " (",
          round(
            tmp.dominant.pct <- tmp.dominant / length(dataset[, i]) * 100,
            digits = 0
          ),
          "% )",
          "\n"
        )
        
        var.hlp <- dataset[, i]
        
        var.hlp <- var.hlp[!is.na(var.hlp) &
                             !is.nan(var.hlp) &
                             !is.infinite(var.hlp)]
        
        qnt.hlp <-
          quantile(
            var.hlp,
            probs = c(0.001, 0.01, 0.05, 0.95, 0.99, 0.999),
            na.rm = TRUE,
            type = 3
          )
        
        cat("Summary:", "\n")
        
        print(summary(var.hlp))
        
        print(qnt.hlp)
        
      }
      
      else if (var.class == "factor") {
        cat("\n\t", i, "\n\n")
        
        cat("Variable type / class:", tmp.class <-
              var.class,
            "\n")
        
        cat(
          "Number of NA:",
          tmp.NA <-
            sum(is.na(dataset[, i])),
          " (",
          round(tmp.NA.pct <-
                  tmp.NA / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of NaN:",
          tmp.NaN <-
            sum(!is.na(dataset[, i]) &
                  as.character(dataset[, i]) == "NaN"),
          " (",
          round(tmp.NaN.pct <-
                  tmp.NaN / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of Inf:",
          tmp.Inf <-
            sum(
              !is.na(dataset[, i]) &
                (dataset[, i] == -Inf | dataset[, i] == Inf)
            ),
          " (",
          round(tmp.Inf.pct <-
                  tmp.Inf / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat("Number of unique values / categories:",
            tmp.unique <- length(unique(dataset[, i])),
            "\n")
        
        tbl.hlp <-
          as.data.frame(table(dataset[, i], useNA = "ifany"))
        
        rnms.hlp <-
          as.character(tbl.hlp[, 1])
        
        rnms.hlp[is.na(rnms.hlp)] <- "NA"
        
        tbl.hlp <-
          cbind(tbl.hlp[, 2], round(tbl.hlp[, 2] / sum(tbl.hlp[, 2]), digits = 2))
        
        newordr.hlp <- order(tbl.hlp[, 1], decreasing =
                               TRUE)
        
        if (length(newordr.hlp) > 1) {
          tbl.hlp <-
            data.frame(tbl.hlp[newordr.hlp,])
          
          rnms.hlp <-
            rnms.hlp[newordr.hlp]
        }
        
        tbl.hlp <-
          cbind(tbl.hlp, cumsum(tbl.hlp[, 2]))
        
        tbl.hlp <-
          rbind(tbl.hlp, c(sum(tbl.hlp[, 1]), sum(tbl.hlp[, 2]), NA))
        
        colnames(tbl.hlp) <-
          c("Freq", "Prop", "Cum Prop")
        
        row.names(tbl.hlp) <- c(rnms.hlp, "Sum")
        
        cat("Dominant value:",
            tmp.dominant.name <- rnms.hlp[1],
            "\n")
        
        cat(
          "Number of dominant value:",
          tmp.dominant <- tbl.hlp[1, 1],
          " (",
          round(
            tmp.dominant.pct <-
              tmp.dominant / length(dataset[, i]) * 100,
            digits = 0
          ),
          "% )",
          "\n"
        )
        
        cat("Summary:", "\n")
        
        print(tbl.hlp)
        
      }
      
      else {
        cat("\n\t", i, "\n\n")
        
        cat("Variable type / class:",
            tmp.class <- var.class,
            "\n")
        
        cat(
          "Number of NA:",
          tmp.NA <- sum(is.na(dataset[, i])),
          " (",
          round(tmp.NA.pct <-
                  tmp.NA / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of NaN:",
          tmp.NaN <-
            sum(!is.na(dataset[, i]) &
                  as.character(dataset[, i]) == "NaN"),
          " (",
          round(tmp.NaN.pct <-
                  tmp.NaN / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat(
          "Number of Inf:",
          tmp.Inf <-
            sum(
              !is.na(dataset[, i]) &
                (dataset[, i] == -Inf |
                   dataset[, i] == Inf)
            ),
          " (",
          round(tmp.Inf.pct <-
                  tmp.Inf / length(dataset[, i]) * 100,
                digits = 0),
          "% )",
          "\n"
        )
        
        cat("Number of unique values / categories:",
            tmp.unique <- length(unique(dataset[, i])),
            "\n")
        
        tbl.hlp <- as.data.frame(table(dataset[, i], useNA
                                       = "ifany"))
        
        rnms.hlp <- as.character(tbl.hlp[, 1])
        
        rnms.hlp[is.na(rnms.hlp)] <- "NA"
        
        tbl.hlp <-
          cbind(tbl.hlp[, 2], round(tbl.hlp[, 2] / sum(tbl.hlp[, 2]), digits = 2))
        
        newordr.hlp <- order(tbl.hlp[, 1], decreasing =
                               TRUE)
        
        if (length(newordr.hlp) > 1) {
          tbl.hlp <-
            data.frame(tbl.hlp[newordr.hlp,])
          
          rnms.hlp <- rnms.hlp[newordr.hlp]
        }
        
        tbl.hlp <- cbind(tbl.hlp, cumsum(tbl.hlp[, 2]))
        
        tbl.hlp <-
          rbind(tbl.hlp, c(sum(tbl.hlp[, 1]), sum(tbl.hlp[, 2]), NA))
        
        colnames(tbl.hlp) <- c("Freq", "Prop", "Cum Prop")
        
        row.names(tbl.hlp) <- c(rnms.hlp, "Sum")
        
        cat("Dominant value:",
            tmp.dominant.name <- rnms.hlp[1],
            "\n")
        
        cat(
          "Number of dominant value:",
          tmp.dominant <-
            tbl.hlp[1 , 1],
          " (",
          round(
            tmp.dominant.pct <-
              tmp.dominant / length(dataset[, i]) * 100,
            digits = 0
          ),
          "% )",
          "\n"
        )
        
        cat("Summary:",
            "\n")
        
        print(tbl.hlp)
        
        cat("Variable is not of integer,
                      numeric nor factor
                      type",
            "\n")
      }
      
      var.names.summary.df <-
        rbind(
          var.names.summary.df,
          data.frame(
            i,
            tmp.class,
            tmp.NA,
            tmp.NA.pct,
            tmp.NaN,
            tmp.NaN.pct,
            tmp.Inf,
            tmp.Inf.pct,
            tmp.unique,
            tmp.dominant.name,
            tmp.dominant,
            tmp.dominant.pct
          )
        )
    }
    
    colnames(var.names.summary.df) <-
      c(
        "Variable.Name",
        "Variable.Type",
        "NA.num",
        "NA.pct",
        "NaN.num",
        "NaN.pct",
        "Inf.num",
        "Inf.pct",
        "Unique.Values.num",
        "Dominant",
        "Dominant.num",
        "Dominant.pct"
      )
    
    return(var.names.summary.df)
  }
