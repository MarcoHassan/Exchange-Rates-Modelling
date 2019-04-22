####################
# Useful functions #
####################

## Function 1 ##

# helps to evalue paste0 expressions and to evalue them directly without 
# interpreting them as text.
evalue <- function(text){eval(parse(text = text))}

#################################################################################

## Function 2 ##

#write the data to a csv in the working directory

write <- function(dat, name)
{
  write.csv(as.data.frame(dat), file = paste0("../Written\ Data/", name)) 
  ## as.data.frame is necessary to get the date index out of the xts
}

#################################################################################

## Function 3 ##

# Specify helper function to extract data leveraging quantmod API interaction.

get_Symbols <- function(x)
{
  lapply(x, function(sym)
  {getSymbols(sym, src = "FRED",
              return.class = "xts",
              auto.assign = F)  ## auto.assign specifies that not the ticker should be passed
  }) 
}

#################################################################################

## Function 4 ##

# Prints all of the series for which the adf test rejects stationarity with 95% confidence

adfTest <- function(x)
{
  dd <- adf.test(na.trim(x))
  if(dd$p.value >= 0.05) dd
}

#################################################################################

## Function 5 ##

# Test for seasonality roots at the defined frequency

seasonality <- function(x, freq)
{
  ## test for unit root for given seasonality
  dat <- ts(x, frequency = freq) 
  
  # Test for the presence of seasonal patterns at the determined frequency
  res <- nsdiffs(dat)
}

#################################################################################

## Function 6 ##

# Analyse the most suspicious seasonality patterns for each series

suspectFrequency <- function(x)
{
  p = periodogram(x, plot = F) ## short lag determinant
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  top = head(order, 1)
  
  # convert frequency to time periods
  time = round(1/top$f)
  
  time[1]
}

#################################################################################

## Function 7 ##

# Perform a superior predictive ability check based on Hansen, 2005 JBES,
# Subsequently it extract p-values for the spa with different benchmark models based 
# on the bootstrap technique explaind by Hansen in his paper.
# Given the different p-values performed with each different benchmark model 
# a model confidence set as in Hansen, Lunde, Nason - 2011 Econometrica - is easily obtained

spa <-function(per=perf, column_benchmark_model, num_predmodels, number_predictions,q=0.25, iter = 1, periodogram=T) 
{
  e<-column_benchmark_model # specify benchmark model
  d<-matrix(NA ,number_predictions, num_predmodels-1) # create empty matrix containing the error
  
  s<-0
  for (i in seq(1,num_predmodels,1)[-e])
  {
    s<-s+1
    d[,s]<-per[,e]-per[,i] ## compute the difference loss function
  }
  
  #colMeans(d)
  
  ## Below get a reasonable estimation for the standard deviation of the difference of the loss function.
  
  w<-rep(0,num_predmodels-1)
  for (k in 1:(num_predmodels-1))
  {
  e<-acf(d[,k],lag.max=number_predictions-1,type="covariance",plot=F)$acf
    if (periodogram==F)
    {
      w[k]<-sqrt(e[1]+2*sum(((number_predictions-seq(1,number_predictions-1,1))/number_predictions*(1-q)^{seq(1,number_predictions-1,1)}+seq(1,number_predictions-1,1)/number_predictions*(1-q)^{number_predictions-seq(1,number_predictions-1,1)})*e[2:number_predictions]))
    }
    else if (periodogram==T)
    {
      w[k]<-sqrt(spectrum(d[,k],plot=F)$spec[1]) 
    }
  }
  
  stat<-max(0,max(sqrt(number_predictions)*colMeans(d)/w)) 
  
  #Bootstrap:
  
  stat.boos<-rep(0,iter)
  for (r in 1:iter)
  {
    tau<-rep(0,number_predictions)
    tau[1]<-as.integer(number_predictions*runif(1))+1
    for (i in 2:number_predictions)
    {
      s<-runif(1)
      tau[i]<-(as.integer(number_predictions*runif(1))+1)*(s<q)+((tau[i-1]<number_predictions)*tau[i-1]+1)*(s>=q)
    }
    
    d.boos<-d[tau,] ## select the bootstrapped difference in the loss function.
    
    e<-d
    for (k in 1:(num_predmodels-1))
    {
      e[,k]<-d.boos[,k]-mean(d[,k])*(mean(d[,k])>= - sqrt(w[k]^2/number_predictions*2*log(log(number_predictions))))
    }
    
    stat.boos[r]<-max(0,max(sqrt(number_predictions)*colMeans(e)/w))
  }
  
  p.value<-mean((stat.boos>stat))
  
  list(p.value=p.value,stat.boos=stat.boos,stat=stat)
}

######################################################################

## Function 8

# Compute the Log Likelihood of normal distributed data.
logLikelihood <- function(data)
                                {return(-(nrow(data)/2)*log(2*pi) -  
                                          nrow(data)*log(sqrt(var(data))) - 
                                          (1/(2*var(data)))*sum(data^2))
}

#######################################################################

## Function 9

# Create a grid matrix of partitioned exogenous parameters

partitionSpace <- function(dat, grid_level = 16)    
                  {
                    partitioned_space <- matrix(NA, ncol(dat) , grid_level+1)
                    
                    rownames(partitioned_space) <- colnames(dat)
                    
                    partitioned_space <- t(sapply(dat, quantile, 
                                                   probs = seq(0,1, 1/grid_level)))
                    
                    partitioned_space <- partitioned_space[-nrow(partitioned_space), c(-1, -(grid_level+1))]
                    
                    return(partitioned_space)
                  }


#######################################################################

## Function 10

# Select the optimal branch for a generalized tree structred model

fork <- 
  function(dat, split_model, total_residual = 0, grid_level = 16, node_residual = 0)
  {
    improvement <- FALSE
    
    Log_likelihood <- split_model$loglik
    
    partitioned_space <<- partitionSpace(dat, grid_level = grid_level)
    
    ## Insert partitioned space
    
    for (iVariable in c(1:nrow(partitioned_space)))
    {
      
      for(jBreak in c(1:ncol(partitioned_space)))
      {
        dat <- as.data.frame(dat)
        
        part_right <- sqldf(
          sprintf(  "SELECT * 
                    FROM dat 
                    GROUP BY %s 
                    HAVING %s > %f", 
                    rownames(partitioned_space)[iVariable], 
                    rownames(partitioned_space)[iVariable],
                    partitioned_space[iVariable,jBreak]
          )
        )
        
        part_left <- sqldf(
          sprintf("SELECT * 
                  FROM dat
                  GROUP BY %s 
                  HAVING %s <= %f", 
                  rownames(partitioned_space)[iVariable], 
                  rownames(partitioned_space)[iVariable],
                  partitioned_space[iVariable,jBreak]))
        
        ## Check that the minimum number of observations for the safe estimation of the parameters
        
        ## If partition contains less than 40 observations don't even try to estimate the model.
        if( nrow(part_right) < 40 ||  nrow(part_left) < 40 ) 
        {
          next
        }
        
        
        ## OLS model is guaranteed after partitioning.
        #if( nrow(part_right) < 50 ||  nrow(part_left) < 50 ) 
        #{
        #  warning(
        #    sprintf(
        #     "One Partition containes less than 50  observations [i.e. %d obs.]",
        #      min(nrow(part_right), nrow(part_left))
        #    )
        #  )
        #}

        ## Compute the likelihood measure for the model an keep the model just if it imporved compared to the current benchmark model.
        dat <- as.xts(dat, order.by = as.Date(index(dat)), frequency = 12)
        
        right_res <- arimax(part_right[,ncol(dat)], 
                            xreg = part_right[,-(ncol(dat))])$residuals
        
        left_res <- arimax(part_left[,ncol(dat)], 
                           xreg = part_left[,-(ncol(dat))])$residuals
        
        # Obtain residuals for the partitioned model:
        if(length(total_residual) == 1)
        {
          ress <- as.vector( c(right_res, left_res) )
        }
        
        else
        {
            terminal_residuals <- total_residual[!c(total_residual %in% node_residual)]
              
            ress <- as.vector( c(right_res, left_res, terminal_residuals) )              
        }
        
        # Compute Likelihood for the model:
        part_logLik <- logLikelihood(as.data.frame(ress))
        
        ## Keep just the highest log likelihood model. 
        
        if(part_logLik > Log_likelihood)
        {
          improvement <- TRUE
          
          Log_likelihood <- part_logLik
          
          opt_variable <- rownames(partitioned_space)[iVariable]
          
          opt_break <- colnames(partitioned_space)[jBreak]
          
          best_ress <- ress
          
          right_model <- arimax(part_right[,ncol(dat)], 
                               xreg = part_right[,-(ncol(dat))])$coef
          
          left_model  <- arimax(part_left[,ncol(dat)], 
                               xreg = part_left[,-(ncol(dat))])$coef
          
          best_right <- right_res
          
          best_left <- left_res
          
          best_partitioned_space <- partitionSpace(dat)
        }
      }
      
    }
    
    if(improvement == TRUE)
    {
      
      result_List <- list("loglik" = Log_likelihood, "optVar" = opt_variable, 
                          "optBreak" = opt_break, "right_residual" = best_right,
                          "left_residual" = best_left, "total_residuals" = best_ress,
                          "node_residuals" = node_residual, 
                          "partitioned_space" = best_partitioned_space,
                          "right_model" = right_model, 
                          "left_model" = left_model, "input_data" = dat)
      
      return(result_List)
    }
    
    return(list("node_residuals" = node_residual, "input_data" = dat))
  }

################################################################

## Function 11 - bestFit

## Compares the different partition possibilities and returns the one that yields the highest likelihood improvement.

bestFit <- 
          function(models)
          {
            ## Initiate helper Variable
            empty_model = 0;
            Log_likelihood = 0
            indx = 1
            mylist <<- list()
            
            for (iModel in models)
            {
              if (length(iModel) == 2)
              {
                empty_model = empty_model + 1;
                
                mylist[[indx]] <<- iModel
                
                indx = indx + 1
                
                if(empty_model == length(models))
                {
                  return()
                }
              }
              
              else
              {
                if(iModel$loglik > Log_likelihood)
                {
                  Log_likelihood <- iModel$loglik
                  best_fit <- iModel
                }
                
                else
                {
                  mylist[[indx]] <<- iModel
                  
                  indx = indx + 1
                }
                
              }
            } 
            
            data_split <<- best_fit$input_data
            
            partitioned_space <<- partitionSpace(data_split)
            
            return(best_fit)
          }

#######################################################################

## Function 12 - GTS

## Finds the optimal tree based on the likelihood measure.

GTS <- function(dat, max_Partition = 8)
{
  ##########################
  ## Initiating the model ##
  ##########################
  
  dat <- as.data.frame(dat)
  
  global <<- arimax(dat[,ncol(dat)], xreg = dat[,-(ncol(dat))])
  
  mPartition = 1
  max_lik1 <<- fork(dat, global)
  
  variable <- max_lik1$optVar
  brek <- max_lik1$optBreak
  
  print(sprintf("For partition %d the optimal split is at %s %s [i.e. %f]", mPartition, variable, 
                  brek, max_lik1$partitioned_space[variable, brek]))
  
  
  ######################
  ## Second Partition ##
  ######################
  
  mPartition = 2
  
  part_right1 <- list(
    sqldf(
      sprintf(  "SELECT * 
                FROM dat 
                GROUP BY %s 
                HAVING %s > %f",
                max_lik1$optVar,
                max_lik1$optVar,
                partitioned_space[max_lik1$optVar, max_lik1$optBreak]
      )
    ), max_lik1$right_residual
  )
  
  part_left1 <- list(
    sqldf(
      sprintf(  "SELECT * 
                FROM dat
                GROUP BY %s 
                HAVING %s <= %f",
                max_lik1$optVar,
                max_lik1$optVar,
                partitioned_space[max_lik1$optVar, max_lik1$optBreak]
      )
    ), max_lik1$left_residual
  )
  
  max_lik2right <- fork(part_right1[[1]], max_lik1, 
                        max_lik1$total_residuals, node_residual = part_right1[[2]])
  
  
  max_lik2left <- fork(part_left1[[1]], max_lik1, 
                       max_lik1$total_residuals, node_residual = part_left1[[2]])
  
  ## Just keep the best fit
  max_lik2 <<- bestFit(list(max_lik2left, max_lik2right)) ## GOOD
  
  if(is.null(max_lik2)) 
    { 
    max_Partition <<- 1
    return()
    }
  
  variable = max_lik2$optVar
  brek = max_lik2$optBreak
  
  print(sprintf("For partition %d the optimal split is at %s %s [i.e. %f]", 
                  mPartition, variable, brek, max_lik2$partitioned_space[variable, brek]))
  
  rm(max_lik2right, max_lik2left)
  
  
  #####################
  ## Iterative part  ##
  #####################
  
  for(mPartition in 2:max_Partition)
  {
    model <- get(paste0("max_lik", mPartition))
    
    assign(paste0("part_left", mPartition),  
           list(
             sqldf(
               sprintf(  "SELECT * 
                         FROM data_split 
                         GROUP BY %s 
                         HAVING %s <= %f",
                         model$optVar,
                         model$optVar,
                         partitioned_space[model$optVar, model$optBreak]
               )
             ), model$left_residual
           )
    )
    
    assign(paste0("part_right", mPartition),  
           list(
             sqldf(
               sprintf(  "SELECT * 
                         FROM data_split 
                         GROUP BY %s 
                         HAVING %s > %f",
                         model$optVar,
                         model$optVar,
                         partitioned_space[model$optVar, model$optBreak]
               )
             ), model$right_residual
           )
    )
    
    ## After two partitions you are already out of sample.
    assign(paste0("max_lik", mPartition+1 ,"right"), fork(get(paste0("part_right", mPartition))[[1]], model, 
                                                          model$total_residuals, node_residual = get(paste0("part_right", mPartition))[[2]]))
    
    assign(paste0("max_lik", mPartition +1,"left"),fork(get(paste0("part_left", mPartition))[[1]], model, 
                                                        model$total_residuals, node_residual = get(paste0("part_left", mPartition))[[2]]))
    
    new_list <- list(get(paste0("max_lik", mPartition+1 ,"right")), get(paste0("max_lik", mPartition +1,"left")))
    
    ## Partition all of the other terminal nodes
    i = 3
    for(iNode in mylist)
    {
      new_list[[i]] <- fork(iNode$input_data, model, model$total_residuals, 
                            node_residual = iNode$node_residuals)
      i=i+1
    }

    assign(paste0("max_lik", mPartition+1), bestFit(new_list),  envir=globalenv())
    
    if(is.null(get(paste0("max_lik", mPartition+1))))
    {
      out <- sprintf("Maximum Partition = %d, as further partitions lack the sample size for reliable parameter estimation", mPartition)
      cat("\n")
      message(out)
      max_Partition <<- mPartition
      break
    }
    
    variable <- get(paste0("max_lik", mPartition+1))$optVar
    brek <- get(paste0("max_lik", mPartition+1))$optBreak
    
    print(sprintf("For partition %d the optimal split is at %s %s [i.e. %f]", mPartition+1, 
                    variable, brek,
                    get(paste0("max_lik", mPartition+1))$partitioned_space[variable, brek]))
    
    rm(list = c(paste0("max_lik", mPartition+1 ,"right"), paste0("max_lik", mPartition +1,"left")))    
  }
}

###################################################################################### 

## Function 13 - AICcSmall

## Prunes via AIC small sample information criteria.

AICcSmall <- function(log_likelihood, parameter, observation)
{
  return ((2*parameter - 2*log_likelihood) + (2*(parameter**2)+2*parameter)/(observation- parameter -1))
}

#######################################################################################

## Function 14 - prunedTree

## Returns the model with the highest Information Criteria

prunedTree <- function()
              {
                help <- str_detect(ls(globalenv()), "^max_lik")
                help <- str_detect(ls(globalenv()), "^max_lik")
                
                partitionSolutions <- lapply(ls(globalenv())[help], get)
                
                parameters <- length(global$coef)
                
                Akaike <- AICcSmall(global$loglik, parameters, nrow(data))
                
                ## Benchmark is general model
                best_index = 1
                
                ## Helper index
                idx = 2
                
                for(mPartition in partitionSolutions)
                {
                  if(is.null(mPartition)) break
                  
                  new_Akaike <- AICcSmall(mPartition$loglik, idx*parameters, nrow(data))
                  
                  if(new_Akaike < Akaike)
                  {
                    Akaike <- new_Akaike
                    
                    best_model <- mPartition
                    
                    best_index <- idx - 1 
                  }
                
                  idx = idx+1
                }  
                
                print(sprintf("Highest Infromation Criteria is at Partition %d", best_index))
                
                return(best_index)
              }

#################################################################################################

## Function 15 - createTree

createTree <- function(iterate, plot = F)
{
  if(iterate == 1) return(NULL)
  
  ## Save the resutls as partykit objects and plot the partitions
  for (mPartition in 1:iterate)
  {
    opt_var <- get(paste0("max_lik", mPartition))$optVar
    opt_break <- get(paste0("max_lik", mPartition))$optBreak
    
    assign(paste0("break", mPartition), get(paste0("max_lik", mPartition))$partitioned_space[opt_var, opt_break])
  }
  
  # For partition 2
  i = 2
  
  helper <- get(paste0("max_lik", i-1))$optVar
  if(all(get(paste0("max_lik", i))$partitioned_space[helper, ] > break1)) 
  { 
    kids3 <- c(4,5)
  } else 
  {
    kids2 <- c(4,5)
  }
  
  ## For partition 2
  node1 <- list(id = 1L, split = partysplit(match(max_lik1$optVar, names(max_lik1$input_data)), 
                                            breaks = max_lik1$partitioned_space[max_lik1$optVar, max_lik1$optBreak]),
                kids = 2:3) 
  
  terminal1 <- list("terminal" = FALSE)
  
  if("kids2" %in% ls(environment()))
  {
    node2 <- list(id = 2L, split = partysplit(match(max_lik2$optVar, names(max_lik1$input_data)), 
                                              breaks = max_lik2$partitioned_space[max_lik2$optVar, max_lik2$optBreak]),
                  kids = kids2)
    
    terminal2 <- list("terminal" = FALSE) 
  } else
  {
    out2 <- max_lik1$left_model
    node2 <- list(id = 2L, info = "terminal")
    terminal2 <-  list("terminal" = TRUE, "split" = "smaller", "brek" = break1, "var" = helper) 
  }
  
  
  if("kids3" %in% ls(environment()))
  {
    node3 <- list(id = 3L, split = partysplit(match(max_lik2$optVar, names(max_lik1$input_data)), 
                                              breaks = max_lik2$partitioned_space[max_lik2$optVar, max_lik2$optBreak]),
                  kids = kids3)
    terminal3 <- list("terminal" = FALSE) 
  } else
  {
    out3 <- max_lik1$right_model
    node3 <- list(id = 3L, info = "terminal")
    terminal3 <- list("terminal" = TRUE, "split" = "bigger", "brek" = break1, "var" = helper) 
  }
  
  out4 <- max_lik2$left_model
  node4 <- list(id = 4L, info = "terminal")
  
  out5 <-  max_lik2$right_model
  node5 <- list(id = 5L, info = "terminal")
  
  ## No matter what with 2 partitions the 4-5 nodes will be terminal and there will exist exactly 5 nodes.
  
  terminal4 <- list("terminal" = TRUE, "split" = "smaller", "brek" = break2, "var" = max_lik2$optVar) 
  terminal5 <- list("terminal" = TRUE, "split" = "bigger", "brek" = break2, "var" = max_lik2$optVar)
  
  
  ### Generalize
  
  ###############################
  ## Update the new split node ##
  ###############################
  if(iterate > 2)
  {
    for(mPartition in 3:iterate)
    {

      for(i in 1:((mPartition-2)*2+3))
      {
        term <- get(paste0("terminal", i))
        if(term$terminal)
        {
          if(term$split == "bigger")
          {
            if(all(get(paste0("max_lik", mPartition))$partitioned_space[term$var, ] > term$brek))
            {
              assign(paste0("kids", i), c((mPartition-1)*2+2, (mPartition-1)*2+3)) ## NOTICE: here i is correct as being terminal kidsi will not exist.
              
              assign(paste0("node", i) ,list(id = i, split = partysplit(match(get(paste0("max_lik", mPartition))$optVar, names(max_lik1$input_data)),## qui quello nuovo
                                                                        breaks = get(paste0("max_lik", mPartition))$partitioned_space[get(paste0("max_lik", mPartition))$optVar, get(paste0("max_lik", mPartition))$optBreak]),
                                             kids = get(paste0("kids", i))))
              
              assign(paste0("terminal", i), list("terminal" = FALSE))
            }
            
          } else
          {
            if(all(get(paste0("max_lik", mPartition))$partitioned_space[term$var, ] <= term$brek))
            {
              assign(paste0("kids", i), c((mPartition-1)*2+2, (mPartition-1)*2+3)) ## NOTICE: here i is correct as being terminal kidsi will not exist.
              assign(paste0("node", i), list(id = i, split = partysplit(match(get(paste0("max_lik", mPartition))$optVar, names(max_lik1$input_data)),## qui quello nuovo
                                                                        breaks = get(paste0("max_lik", mPartition))$partitioned_space[get(paste0("max_lik", mPartition))$optVar, get(paste0("max_lik", mPartition))$optBreak]),
                                             kids = get(paste0("kids", i))))
              
              assign(paste0("terminal", i), list("terminal" = FALSE))
            }
          }
          
        }
      }
      
      ## No matter for 3 partitions 7 nodes, if 4 part 9 nodes etc...
      assign(paste0("terminal", (mPartition-1)*2+2), list("terminal" = TRUE, "split" = "smaller", "brek" = get(paste0("break", mPartition)), "var" = get(paste0("max_lik", mPartition))$optVar))
      assign(paste0("terminal", (mPartition-1)*2+3), list("terminal" = TRUE, "split" = "bigger", "brek" = get(paste0("break", mPartition)), "var" = get(paste0("max_lik", mPartition))$optVar))
      
      assign(paste0("out", (mPartition-1)*2+2),  get(paste0("max_lik", mPartition))$left_model)
      assign(paste0("out", (mPartition-1)*2+3), get(paste0("max_lik", mPartition))$right_model)
      assign(paste0("node", (mPartition-1)*2+2), list(id = ((mPartition-1)*2+2), info = "terminal"))
      assign(paste0("node", (mPartition-1)*2+3), list(id = ((mPartition-1)*2+3), info = "terminal"))
    }
    
    nodel <- list()
    for (i in 1:((iterate-1)*2+3))
    {
      nodel[[i]] <- get(paste0("node", i))
    }
    
    model <- list()
    for (i in 1:((iterate-1)*2+3))
    {
      if(!paste0("out", i) %in% ls(environment())) ## if not in environment than not terminal
       { model[[i]] <- NA } else
       { model[[i]] <- get(paste0("out", i)) }
    }
    
    prova <<- list("model" = model, "nodes" = nodel)
    
    ## convert to a recursive structure
    nodels <<- as.partynode(nodel)
    
    ### To finally convert to a tree
    
    tree <- party(nodels, data = max_lik1$input_data)
    
    if(plot == T)
    {
      plot(tree)
    }
    
    return("NOT NULL")
  }
  
  else
  {
    nodel <- list(node1, node2, node3, node4, node5)
    
    model <- list()
    for (i in 1:5)
    {
      if(!paste0("out", i) %in% ls(environment())) ## if not in environment than not terminal
      { model[[i]] <- NA } else
      { model[[i]] <- get(paste0("out", i)) }
    }
    
    prova <<- list("model" = model, "nodes" = nodel)
    
    ## convert to a recursive structure
    nodels <<- as.partynode(nodel)
    
    ### To finally convert to a tree
    
    tree <- party(nodels, data = max_lik1$input_data)
    
    if(plot == T)
    {
      plot(tree)
    }
    
    return("NOT NULL")
  }
}

############################################################################################

## Function 16 - GTSEstimate

## This estimates a forecast based on the GTS model

GTSEstimate <- function(new_data)
{
  if(is.null(condition))
    {
      model <- arimax(xshort[, ncol(xshort)], 
                       xreg = xshort[, -ncol(xshort)])
      
      return(new_data %*% model$coef)
      }
  
  ## Check in which branch the observation falls [Condition 1]
  if(new_data[, (prova$nodes[1][[1]]$split$varid+1)] >
     prova$nodes[1][[1]]$split$breaks)
  {
    ## Save New Node
    next_node_id <- prova$nodes[1][[1]]$kids[2]
  } else 
  { ## else condition in case the variable is smaller than the break
    next_node_id <- prova$nodes[1][[1]]$kids[1]
  }
  
  ## Check if the node is terminal
  if(!is.null(prova$nodes[next_node_id][[1]]$info))
  {
    ## If it is return the forcasted value with the parametric model 
    ## at the end node
    return(new_data %*% prova$model[next_node_id][[1]])
  } 
  
  ## If it is not iterate the process above from [Condition 1]
  loop <- TRUE
  while(loop)
  {
    if(new_data[, (prova$nodes[next_node_id][[1]]$split$varid+1)] > 
       prova$nodes[next_node_id][[1]]$split$breaks)
    {
      next_node_id <- prova$nodes[next_node_id][[1]]$kids[2]
    } else 
    { ## else condition in case the variable is smaller than the break
      next_node_id <- prova$nodes[next_node_id][[1]]$kids[1]
    }
    
    ## Check if the node is terminal
    if(!is.null(prova$nodes[next_node_id][[1]]$info))
    {
      ## If it is return the forcasted value with the parametric model at 
      ## the end node
      return(new_data %*% prova$model[next_node_id][[1]])
    } 
  }
}

###################################################################################



