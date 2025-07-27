# loading_module.R - Loading Screen and Progress Tracking

#' Create loading screen UI with new progressive approach
#' @return HTML elements for loading screen
create_loading_screen <- function() {
  div(
    id = "loading-screen",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
             background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             z-index: 9999; display: flex; flex-direction: column; 
             justify-content: center; align-items: center; color: white;",
    
    # Logo/Title
    div(
      style = "text-align: center; margin-bottom: 40px;",
      h1("🌱 Cascade-Siskiyou Soil Explorer", 
         style = "font-size: 2.5em; margin-bottom: 10px; font-weight: 300;"),
      p("Loading soil data and preparing interactive map...", 
        style = "font-size: 1.2em; opacity: 0.9;")
    ),
    
    # Simple loading message
    div(
      style = "text-align: center; margin-bottom: 30px;",
      h3("Loading Application...", style = "margin-bottom: 10px;"),
      p("Please wait while we load the soil data and create the interactive map.", 
        style = "font-size: 1em; opacity: 0.8;")
    ),
    
    # Continue button (initially hidden)
    div(
      id = "continue-button-container",
      style = "margin-top: 30px; text-align: center; display: none;",
      actionButton(
        "user_ready_to_continue",
        "Continue to Interactive Map",
        style = "background: linear-gradient(45deg, #4CAF50, #45a049); 
                 border: none; color: white; padding: 15px 30px; 
                 font-size: 1.1em; border-radius: 8px; cursor: pointer;
                 box-shadow: 0 4px 8px rgba(0,0,0,0.2);"
      )
    ),
    
    # Loading animation
    div(
      id = "loading-spinner",
      style = "margin-top: 30px;",
      div(
        class = "loading-spinner",
        style = "width: 40px; height: 40px; border: 4px solid rgba(255,255,255,0.3); 
                 border-top: 4px solid white; border-radius: 50%; animation: spin 1s linear infinite;"
      )
    ),
    
    # CSS for spinner animation and styling
    tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      #loading-screen {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      .fade-out {
        opacity: 0;
        transition: opacity 0.8s ease;
      }
      
      .fade-in {
        opacity: 1;
        transition: opacity 0.8s ease;
      }
    "))
  )
}

#' Update loading progress with new approach
#' @param session Shiny session object
#' @param overall_progress Numeric overall progress (0-100)
#' @param current_task Character description of current task
#' @param status_message Character status message to add
#' @param show_continue Boolean to show continue button
update_loading_progress <- function(session, overall_progress, current_task = NULL, 
                                   status_message = NULL, show_continue = FALSE) {
  
  # Check if session is valid
  if (is.null(session) || !inherits(session, "ShinySession")) {
    cat("Warning: Invalid session object in update_loading_progress\n")
    return()
  }
  
  tryCatch({
    # Show continue button if requested
    if (show_continue) {
      session$sendCustomMessage("showContinueButton", TRUE)
    }
    
    # Force UI update
    session$flushReact()
  }, error = function(e) {
    cat("Error in update_loading_progress:", e$message, "\n")
  })
}

#' Hide loading screen
#' @param session Shiny session object
hide_loading_screen <- function(session) {
  if (is.null(session) || !inherits(session, "ShinySession")) {
    cat("Warning: Invalid session object in hide_loading_screen\n")
    return()
  }
  
  tryCatch({
    session$sendCustomMessage("hideLoadingScreen", TRUE)
  }, error = function(e) {
    cat("Error in hide_loading_screen:", e$message, "\n")
  })
}

#' Show loading screen
#' @param session Shiny session object
show_loading_screen <- function(session) {
  if (is.null(session) || !inherits(session, "ShinySession")) {
    cat("Warning: Invalid session object in show_loading_screen\n")
    return()
  }
  
  tryCatch({
    session$sendCustomMessage("showLoadingScreen", TRUE)
  }, error = function(e) {
    cat("Error in show_loading_screen:", e$message, "\n")
  })
}

#' Add JavaScript for loading screen interactions
#' @return HTML script tag
get_loading_javascript <- function() {
  tags$script(HTML("
    // Simple loading screen JavaScript
    function showContinueButton() {
      var container = document.getElementById('continue-button-container');
      var spinner = document.getElementById('loading-spinner');
      
      if (container && spinner) {
        container.style.display = 'block';
        container.classList.add('fade-in');
        spinner.style.display = 'none';
      }
    }
    
    function hideLoadingScreen() {
      var loadingScreen = document.getElementById('loading-screen');
      if (loadingScreen) {
        loadingScreen.classList.add('fade-out');
        setTimeout(function() {
          loadingScreen.style.display = 'none';
        }, 800);
      }
    }
    
    // Make functions globally available
    window.showContinueButton = showContinueButton;
    window.hideLoadingScreen = hideLoadingScreen;
    
    // Add Shiny custom message handlers
    Shiny.addCustomMessageHandler('showContinueButton', function(value) {
      showContinueButton();
    });
    
    Shiny.addCustomMessageHandler('hideLoadingScreen', function(value) {
      hideLoadingScreen();
    });
    
    Shiny.addCustomMessageHandler('showLoadingScreen', function(value) {
      var loadingScreen = document.getElementById('loading-screen');
      if (loadingScreen) {
        loadingScreen.style.display = 'flex';
        loadingScreen.classList.remove('fade-out');
        loadingScreen.classList.add('fade-in');
      }
    });
  "))
} 