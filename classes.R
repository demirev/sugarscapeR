library(R6)

Sugarscape <- R6Class(
  "sugarscape world",
  public = list(
    dimensions = NULL, # rows, columns
    sugar_cap  = NULL, # capacity of each field - rows x columns
    sugar_val  = NULL, # current value of each field - rows x columns
    occupied   = NULL, # agent positions - rows x columns
    agents     = NULL, # agent list
    sugar_grow = NULL, # growth rate for sugar
    
    initialize = function(lout, peaks = list(c(15,15),c(35,35)),
                          maxc = 4, suggr = 1, nagents = 150, 
                          agent_params = NULL) {
      self$dimensions <- lout
      self$sugar_cap  <- self$gen_capacity(peaks, maxc)
      self$sugar_val  <- self$sugar_cap # initialize full
      self$sugar_grow <- suggr
      self$occupied   <- matrix(0, nrow = lout[1], ncol = lout[2])
      self$gen_agents(nagents, agent_params)
    },
    
    gen_capacity = function(peaks = NULL, maxc = 4, breaks = c(21,16,11,6)) {
      
      if (is.null(peaks)) {
        
        randomCap <- sample(0:4, prod(self$dimensions))
        return(matrix(randomCap, nrow = self$dimensions[1]))
      
      } else {
        
        rows <- 1:self$dimensions[1]
        cols <- 1:self$dimensions[2]
        allCels <- expand.grid(rows, cols)
        
        allDist <- sapply(
          peaks,
          function(pk) {
            sqrt((allCels$Var1 - pk[1])^2 + (allCels$Var2 - pk[2])^2)
          }
        )
        dist <- apply(allDist, 1, min)
        
        caps <- sapply(
          seq_along(breaks), 
          function(i) {
            capi <- rep(0, nrow(allCels))
            capi[dist < breaks[i]] <- i
            capi
          }
        )
        caps <- apply(caps,1,max)
        return(matrix(caps, nrow = self$dimensions[1]))
        
      }
      
    },
    
    gen_agents = function(n, params = NULL) {
      if (is.null(params)) {
        # some default values
        params <- list(
          values = list(
            vision      = c(3,5,10),
            sugarbolism = c(1,2,3,4),
            lifespan    = c(40,60,80),
            capacity    = Inf,
            endowment   = c(0,5,10)
          ),
          probs  = list(
            vision      = c(1/3,1/3,1/3),
            sugarbolism = c(0.25,0.25,0.25,0.25),
            lifespan    = c(1/3,1/3,1/3),
            capacity    = 1,
            endowment   = c(0.7,0.2,0.1)
          )
        )
      }
      
      # Population distribution of parameters
      visions      <- sample(params$values$vision, 
                             n, prob = params$probs$vision, replace = T)
      sugarbolisms <- sample(params$values$sugarbolism, 
                             n, prob = params$probs$sugarbolism, replace = T)
      lifespans    <- sample(params$values$lifespan, 
                             n, prob = params$probs$lifespan, replace = T)
      capacities   <- sample(params$values$capacity, 
                             n, prob = params$probs$capacity, replace = T)
      endowments   <- sample(params$values$endowment, 
                             n, prob = params$probs$endowment, replace = T)
      
      # Possible locatiosn
      locs <- which(self$occupied == 0, arr.ind = T)
      locs <- locs[sample(1:nrow(locs), size = n), ]
      
      self$agents <- lapply(seq(n), function(i) {
        thisloc <- as.numeric(locs[i, ])
        self$occupied[thisloc[1],thisloc[2]] <- 1
        Agent$new(thisloc, visions[i], sugarbolisms[i],
                  lifespans[i], endowments[i], capacities[i])
      })
      
      
    },
    
    grow = function() {
      sugar_val <- self$sugar_val + self$sugar_grow
      sugar_cap <- self$sugar_cap
      sugar_val[sugar_val > sugar_cap] <- sugar_cap[sugar_val > sugar_cap]
      self$sugar_val <- sugar_val
    },
    
    move = function() {
     
      turn_order <- sample(1:length(self$agents))
      
      for (agent in turn_order) {
        oldpos <- self$agents[[agent]]$loc
        self$occupied[oldpos[1], oldpos[2]] <- 0
        
        newpos <- self$agents[[agent]]$move(self$sugar_val, self$occupied)
        
        self$occupied[newpos[1], newpos[2]] <- 1
        self$agents[[agent]]$add_sugar(self$harvest(newpos))
        
        if (self$agents[[agent]]$is_old() | 
            self$agents[[agent]]$is_starved()) {
          self$occupied[newpos[1], newpos[2]] <- 0 # remove corpse
          # to do - replace ?
        }
      }
      
      self$grow()
      self$cleanse()
    },
    
    harvest = function(loc) {
      yield <- self$sugar_val[loc[1],loc[2]]
      if (length(yield) == 0) {
        browser()
      }
      self$sugar_val[loc[1],loc[2]] <- 0
      return(yield)
    },
    
    cleanse = function() {
      # remove dead agents from list
      self$agents <-  self$agents[!sapply(self$agents, function(a){a$dead})]
    }
    
  )
)

Agent <- R6Class(
  "sugarscape agent",
  public = list(
    loc          = NULL, # location
    vision       = NULL, # how far to see
    sugarbolism  = NULL, # rate of eating sugar
    lifespan     = NULL, # longevity
    sugar        = NULL, # holdings of sugar,
    capacity     = NULL, # max sugar to carry
    dead         = NULL, # dead or not
    age          = NULL, # current age
    
    initialize = function(loc, vision = 4, sugarbolism = 1,
                          lifespan = 60, sugar = 5, capacity = Inf) {
      self$dead        <- FALSE # it's alive
      self$loc         <- loc
      self$vision      <- vision
      self$sugarbolism <- sugarbolism
      self$lifespan    <- lifespan
      self$sugar       <- sugar
      self$capacity    <- capacity
      self$age         <- 0
    },
    
    move = function(sugarfield, otheragents) {
      loc <- self$loc
      vis <- self$vision

      # visible sugar
      rbound1 <- max(1, loc[1] - vis) # horizontal boundary of vision
      rbound2 <- min(nrow(sugarfield), loc[1] + vis) # notice no wrap-around
      cbound1 <- max(1, loc[2] - vis)
      cbound2 <- min(ncol(sugarfield), loc[2] + vis) # and square vision
      
      vissugar <- sugarfield[rbound1:rbound2, cbound1:cbound2]
      visagnts <- otheragents[rbound1:rbound2, cbound1:cbound2]
      
      # choose max
      maxsugar <- which(vissugar == max(vissugar) & visagnts == 0, arr.ind = T)
      maxsugar <- as.numeric(maxsugar[sample(1:nrow(maxsugar),1), ])
      
      # change loc
      loc[1] <- rbound1 + maxsugar[1] - 1 # double counting the start
      loc[2] <- cbound1 + maxsugar[2] - 1
      
      self$loc <- loc
      return(loc)
    },
    
    is_starved = function() {
      self$sugar <- self$sugar - self$sugarbolism
      if (self$sugar == 0) {
        self$dead <- TRUE
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    is_old = function() {
      self$age <- self$age + 1
      if (self$age > self$lifespan) {
        self$dead <- TRUE
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    add_sugar = function(sug) {
      self$sugar <- self$sugar + sug
      if (self$sugar > self$capacity) {
        self$sugar = self$capacity
      }
    }
  )
)


#   -----------------------------------------------------------------------
testClass <- Sugarscape$new(c(50,50))
testClass$move()
