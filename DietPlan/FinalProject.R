#Franz Saragena  CMSC 150 - B3L Final Project 

library(shiny)
library(bslib)

ui = fluidPage(
  tags$head( # Import custom font Lexend Deca
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lexend+Deca&display=swap")
  ),#For CSS
  tags$style(HTML("  
    body { /*For Main Body*/
      color: #004aad;
      font-family: 'Lexend Deca', sans-serif;
      font-size: 12px;
    }
    
    .tableau-container { /*For tables*/
      background-color: #004aad;
      color: white;
      border: 1px solid #004aad;
      font-size: 12px;
      height: 300px;
      overflow: auto;
    }
    
    .tableau-iteration{ /*For all iteration tables*/
      border: 1px solid #004aad;
      background-color: #004aad;
      color: white;
      font-size: 10px;
      height: 200px;
      margin-bottom: 10px; 
      border-bottom: 1px solid #ddd; 
      padding-bottom: 10px;
      overflow: auto;
      border-collapse: collapse;
    }
  ")), 
  
  # Title
  titlePanel("Diet With A Smile"),
  
  tabsetPanel( 
    tabPanel("Home", #First tab, Main Page
             fluidRow(
               # First column for buttons and checkboxes
               column(6,
                      navset_card_tab(
                        tabPanel(
                          # Action buttons (Select All, Clear All, Submit)
                          fluidRow( 
                            column(4, actionButton("selectall", "Select All", style = "background-color: #004aad; color: white;")),
                            column(4, actionButton("clearall", "Clear All", style = "background-color: #004aad; color: white;")),
                            column(4, actionButton("submitall", "Submit", style = "background-color: #004aad; color: white;"))
                          ), # End of fluid row
                          uiOutput("checkboxes_ui") # Food Choices
                        ) #End of tabpanel
                      ) #End of navset
               ), #End of first column
               
               # Second column for displaying main output
               column(6,
                      h5("Food Selected"), # For selected foods
                      verbatimTextOutput("value"), 
                      h5("Initial Tableau"), # For the initial tableau
                      div(class = "tableau-container", tableOutput("int_tab")),
                      h5("Final Tableau"), # For the final tableau
                      verbatimTextOutput("feasible"),
                      div(class = "tableau-container", tableOutput("final_tab")),
                      verbatimTextOutput("basic_solution"),
                      h5("Solution and Cost Breakdown"), # Cost Breakdown
                      tableOutput("interp_tab")
               )# End of column
             ) # End of fluidRow
    ), # End of Main page
    
    # Second tab (Food info)
    tabPanel( 
      "Food Info",
      tableOutput("food_info"),
    ), #End of tab
    #Third (Iteration Breakdown)
    tabPanel(
      "Iteration Breakdown",
      textOutput("num_iter"),
      uiOutput("iteration_matrix")
    ) #End of tab
    
  ) # End of tabsetPanel
) #End of ui



# Server Side
server = function(input, output, session) {
  
  #Import food matrix
  data = read.csv("foodsheet.csv")
  matrix_data = as.matrix(data)
  matrix_data[, 2] = gsub("\\$", "", matrix_data[, 2])  # Remove dollar sign
  matrix_data[, 2] = as.numeric(matrix_data[, 2])    
  food_info = matrix_data[,-3] # Matrix for food without serving size

  output$value = renderText("You Selected:") # For foods selected
  output$food_info = renderTable(food_info) # Render Food Info
  output$checkboxes_ui = renderUI({ # Render all food names
    tagList(
      lapply(1:nrow(matrix_data), function(i) { #I terate and create unchecked(default) boxes
        checkboxInput(paste0("checkbox", i), label = matrix_data[i, 1], value = FALSE) 
      }) # End lapply
    )} # End taglist
  ) # End render
  
  
  # Select all
  observeEvent(input$selectall, {
    for (i in 1:nrow(matrix_data)) { # Check all boxes
      updateCheckboxInput(session, paste0("checkbox", i), value = TRUE)}
    }) #End Select
  
  # Clear all
  observeEvent(input$clearall, {
    for (i in 1:nrow(matrix_data)) { # Uncheck all boxes
      updateCheckboxInput(session, paste0("checkbox", i), value = FALSE)
    }
    #Reset outputs
    output$int_tab = NULL
    output$final_tab = NULL
    output$feasible = NULL
    output$basic_solution = NULL
    output$interp_tab = NULL
    output$iteration_matrix = NULL
    output$num_iter = NULL
    output$value = renderText({ paste("You selected: ")})
  }) #End Clear
  
  
  # Submit Event
  observeEvent(input$submitall, {
    selected_indices = c() #To store the selected indices
    #Table for minmax nutrients
    minmax = matrix(c(1900, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10, 2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30), nrow = 11, ncol = 2)
    
    #Append all selected index in the list
    for(i in 1:nrow(matrix_data)){
      if(input[[paste0("checkbox", i)]]){
        selected_indices = c(selected_indices,i) 
      }
    }# End of for
    
    # Checked if user selected none
    if(length(selected_indices)==0){
      output$value <- renderText({"No foods selected." })
    } # End if
    else {
      # Calculate the initial tableau, final tableau
      initial_tab = int_tableau(length(selected_indices),matrix_data,minmax, selected_indices)
      final_tab = Simplex(initial_tab, FALSE, length(selected_indices))
      
      # The initial tableau to be displayed as a table
      output$int_tab <- renderTable(initial_tab)
      
      output$value <- renderText({ # Update selected food items
        food_picked = c() # Convert selected indices to its food equivalent
        for(i in 1:length(selected_indices)){ # Iterate and store 
          food_picked = c(food_picked,matrix_data[selected_indices[i],1])
        }
        paste("You selected: ", paste(food_picked, collapse = ", ")) # Print the list
      })
      
      # Print optimal cost and final tableau if it exists
      if (!is.null(final_tab)) {
        interp = interp_table(final_tab$foodOnly, matrix_data, selected_indices) # Calculate the interpretation of results
        matrices = final_tab$iterations #Extract all matrix iterations
        basicSol = final_tab$iterSol #Extract all sol per iterations
      
        output$num_iter <- renderText(paste0("Number of Iterations: ",final_tab$numIter)) # Print the num of iterations
        output$final_tab <- renderTable(final_tab$finalTableau) # Render final tableau
        
        # Dynamically render tables for each iteration
        output$iteration_matrix <- renderUI({
          lapply(1:length(matrices), function(i) {
            local({
              iteration <- i # Store and ensure index at that iteration
              tagList(
                h6(paste("Iteration", iteration)), 
                div(class = "tableau-iteration", tableOutput(paste0("iteration", iteration)) ), # Table for the matrix
                verbatimTextOutput(paste0("basicSol", iteration))  # Text for the basic solution
              ) # End taglist
            }) # End local
          }) # End loop
        }) # End render
        
        
        # Create individual table outputs for each iteration matrix
        for (i in 1:length(matrices)) {  
          local({
            iteration <- i  # Store and ensure index at that iteration 
            output[[paste0("iteration", iteration)]] <- renderTable({
              matrices[[iteration]]  # Render the matrix for this iteration
            }) # End table
            output[[paste0("basicSol", iteration)]] <- renderText({
              basicSol[[iteration]]  # Render the basic solution for this iteration
            }) # End text
          }) # End local
        } # End loop
        
        output$interp_tab <- renderTable ( interp ,rownames = TRUE) # Output cost breakdown 
        output$feasible <- renderText( paste( "The optimal cost is $", sprintf("%.2f", final_tab$Z))) # Output optimal cost
        output$basic_solution <- renderText( paste("Basic Solution:", toString(final_tab$basicSolution))) # Output basic solution
      } # End if
      else { # If simplex returns null (not feasible)
        output$feasible <- renderText("Selection not feasible.")
      } # End else
      
      print(selected_indices) # Print the selected indices to the console for debugging
    } # End else
}) # End Submit
  
  
  
  #simplex function
  Simplex = function (mat, isMax, numFood){
    labeled_list = list(finalTableau="", basicSolution = "", Z="", foodOnly ="", iterations = list(), iterSol = list(), numIter="" ) # initialize labeled list
    allMat = c(); # To store all matrix iterations
    m = nrow(mat); # Gets row number
    n = ncol(mat); # Gets column number
    numIter = 0; # Initializa number of iterations 
    
    while (any(mat[m, ] < 0)){ # Gauss Jordan, checks if there's a negative element in the last row
      numIter = numIter + 1; #Update number of iterations
      pivot_col = which.min(mat[m,]) # Gets the column of the smallest number in the last row
      
      TR = mat[,n] / mat[,pivot_col]; # Gets test ratio
      TR = ifelse(mat[,pivot_col] > 0, TR, Inf) # Filter test ratio to positive  else it's infinite 
      
      
      if (all(is.infinite(TR))) { # If all test ratios are infinite, problem is infeasible
        print("Not feasible")
        return(NULL) # Exit the function
      }
      
      PE_index = which.min(TR); # Gets row of pivot element (smallest TR)
      PE = mat[PE_index, pivot_col]; # Get the pivot element
      
      # Normalize 
      mat[PE_index,] = mat[PE_index,] / PE;
      for (i in 1:m){  # Iterates until last row
        if(i != PE_index){ # Exclude the pivot element
          
          multiplier = mat[i,pivot_col]; # Get multiplier
          NR = multiplier * mat[PE_index,]; # Get normalized row
          mat[i,] = mat[i,] - NR; # Update row of the multiplier
          
        } # End of if
      } # End of for
      
      #Calculate final basic solution
      basic_solution = vector("numeric", n-1) # initialize a basic solution vector excluding the sol'n column
      if(isMax==TRUE) # for maximization
      {
        for (j in 1:n-1){
          if((sum(mat[, j]) == 1) == 1){ # checks if columns has exactly one "1"
            basic_row = which(mat[, j] == 1)  
            basic_solution[j] = mat[basic_row,n] # only fill the columns with exactly one "1" with the sol value, otherwise it is set to 0
          } 
        } # end of for
      } # end of if
      else{ # for minimization
        for(j in 1:(n-1)){
          if(j==(n-1)){ # if it's in the last column, get the last row of the sol'n column
            basic_solution[j] = mat[m,n];
          }
          else{
            basic_solution[j] = mat[m,j]; #fill it with the last rows in that column
          } # End of else
        } # End of else
        
        basic_solutionfinal = matrix(basic_solution,nrow = 1, byrow= TRUE); #makes a matrix for the basic solution
        
      } # End of if
      # Get basic sol and matrix per iteration
      labeled_list$iterSol[[numIter]] = basic_solutionfinal
      labeled_list$iterations[[numIter]] = mat; 
    } # End of while
    
    #Update labeled list
    labeled_list$finalTableau = mat; 
    labeled_list$basicSolution = basic_solutionfinal; 
    labeled_list$Z = basic_solution[n-1];
    labeled_list$numIter = numIter;
    labeled_list$foodOnly = matrix(basic_solutionfinal[1, (ncol(basic_solutionfinal)-numFood):(ncol(basic_solutionfinal)-1)], nrow =1) # Extract the x variables only
    # Give column names to the foods
    dimnames(labeled_list$foodOnly) = list(NULL,colnames(labeled_list$finalTableau)[(ncol(labeled_list$finalTableau)-(numFood+1)):(ncol(labeled_list$finalTableau)-2)]) 
    
    return (labeled_list)
  }
  
  
  # For interpretation
  interp_table = function (mat, refMat, selected_indices){
    
    servings = as.numeric(refMat[selected_indices,2]) # Extract the serving price
  
    rowSkip = c(); # To store food to skip (0 serving value)
    for(i in 1:ncol(mat)){
      if(mat[1,i]==0){  rowSkip = c(rowSkip, i)} 
    } # End of for
    
    # Eliminate the foods with zero servings
    servings = servings [-rowSkip]
    mat = mat[1,-rowSkip]
    servings = as.numeric(servings)  # Convert to numeric vector if necessary
    mat_row = as.numeric(mat)   # Simplify extraction of serving size to a vector
    servings = servings * mat_row # Calculate price
    
    #Fill interp table
    interp_table = matrix(NA, nrow = length(servings), ncol = 2)
    
    interp_table[,1] = mat
    interp_table[,2] = servings
    dimnames(interp_table) = list(names(mat), c("Serving", "Cost"))
    return(round(interp_table,2))
  }
  
  # For initial tableau 
  int_tableau = function(n, mat, minmax, vectorIndex){ # n is the num of food selected ,mat is the total food matrix, minmax are nutri constraints and vectorIndex are the selected foods
    row_names = c("minCal", "maxCal","minChol", "maxChol", "minTotFat", 
                  "maxTotFat", "minSodium", "maxSodium", "minCarb", "maxCarb",
                  "minFiber", "maxFiber","minProtein", "maxProtein", "minVit A", 
                  "maxVit A","minVit C","maxVit C",
                  "minCalcium","maxCalcium","minIron", "maxIron") # For the minMaxConstraints
    
    #Initialization
    foodServingNames = c() # For the min/max food serving
    RHSvector = c();# Vector for RHS
    slackvar_colnames = c(); # For slackvar names
    nutriVal = 11; # Initialize number of nutritional values
    sums = matrix(0,nrow=11,ncol=n)  # Initialize matrix for sum of nutritional values
    minMaxConstraints = matrix(0, nrow = nutriVal*2, ncol =n+1)  # Initialize a matrix of empty minMaxConstraints
    foodServing = matrix(0,nrow=n*2, ncol=n+1) # Initialize matrix for food serving
    
    #Updating foodServing vector
    for (i in seq(1, 2 * n, by = 2)){
      k=i;
      if(k>n){k=k/2} # In case index is out of bounds
      foodServingNames[i] = paste0("min",mat[vectorIndex[k],1])  # Access the food names for serving
      foodServingNames[i+1] = paste0("max",mat[vectorIndex[k],1])
    }
    
    #For RHSvector/ price serving  
    for (i in 1:n){ 
      RHSvector = c(RHSvector, as.numeric(mat[vectorIndex[i],2]))
      if (i==n){
        RHSvector = c(RHSvector, 1) # Ensure z has a 1 coefficient
      }
    } # End of for
    
    
    # For getting the coefficient of the nutri val per food
    for (i in 1:nutriVal){ # Iterate per column
      for (j in 1:n){ # Update
        sums [i,j] = as.numeric(mat[vectorIndex[j],3+i])
      } # End of for
    } # End of for
    
    
    row_names = c(row_names,foodServingNames) # Combine the two names
    k = 1; # For original indexing
    
    #For the minMax constraints
    for (i in seq(1, 2 * nutriVal, by = 2)){ # Set i to increment by 2 for min and max value
      minMaxConstraints [i,n+1] = minmax[k,1] # Minimum value
      minMaxConstraints [i+1,n+1] = -1*minmax[k,2] # Maximum value
      for (j in 1:n){
        minMaxConstraints[i,j] = sums[k,j] # To ensure all coeff of x1..xn are aligned for min 
        minMaxConstraints[i+1,j] = sums[k,j]*(-1) # To ensure all coeff of x1..xn are aligned for max
      }
      k = k+1; #Increment k
    } # End loop
    
    
    #For minFoodServing and max foodServing
    for (i in seq(1, 2 * n, by = 2)){ # Set i to increment by 2 for min and max value
      foodServing[i,n+1] = 0; # Min serving
      foodServing[i+1,n+1] = -10 # Max serving
      k = (i + 1) / 2 # Calc k for proper indexing
      for(j in 1:n){
        if(k==j){
          foodServing[i,j] = 1; # To ensure all coeff of x1..xn are aligned for min
          foodServing[i+1,j] = -1  # To ensure all coeff of x1..xn are aligned for max
        } # End if 
      } # End inner loop
    } # End loop
    
    augmatrix = rbind(minMaxConstraints,foodServing) # Bind minMax nutri and foodServing constraints
    dimnames(augmatrix) = list(row_names,NULL) # Update their row names
    augmatrix = rbind(augmatrix,RHSvector) # Add the RHSvector value
    
    augmatrix = t(augmatrix) # Transpose
    RHS = augmatrix[,ncol(augmatrix)] # Extract RHS
    Zval = c(rep(0,nrow(augmatrix)-1),1) # Create z-values, assign 1 to obj function
    RHS[length(RHS)] = 0; # Set to 0 for the obj func soln
    
    # Extract the last row
    obj_func = -1*augmatrix[nrow(augmatrix),]
    obj_func[length(obj_func)] = 0; # Assign RHS for obj to 0
    
    
    augmatrix = augmatrix[,-ncol(augmatrix)] # Eliminate the RHS first

    for (i in 1:n){ # For naming convention in foods/slackvar
      slackvar_colnames[i] = mat[vectorIndex[i],1] 
      }
    
    #Make slackVar
    slackVar = matrix(0, nrow = nrow(augmatrix), ncol = n)
    for(i in 1:nrow(slackVar)){
      for (j in 1:n){
        if(i==j){
          slackVar[i,j] = 1; # Assign positive coefficients
        }}}
    
    dimnames(slackVar) = list( NULL,slackvar_colnames) # Assign names to slackVar
    obj_func = c(obj_func,rep(0,n+1)) # Initialize slack variables
    obj_func[length(obj_func)-1] = 1;
    augmatrix = cbind(augmatrix, slackVar, Zval, RHS) # Add slack, Z value and RHS
    augmatrix[nrow(augmatrix), ] = obj_func # Add obj func to the last row
    
    return(augmatrix)
  } # End of func
  
} # End server

# Run the app
shinyApp(ui, server)
