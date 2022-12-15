


user_base <- tibble(
  user = c("ecopetrol","ecopetrol2"), ##add more users here
  password = c("pass_ecopetrol","pass_ecopetrol"), 
  password_hash = sapply(c("pass_ecopetrol","pass_ecopetrol"), sodium::password_store), 
  permissions = c("admin", "admin"),
  name = c("youruserhere1", "youruserhere2")
)



credentials <- callModule(shinyauthr::login, "login", 
                          data = user_base,
                          user_col = user,
                          pwd_col = password_hash,
                          sodium_hashed = TRUE,
                          log_out = reactive(logout_init()))

logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))


user_info <- reactive({credentials()$info})


observe({
  if(credentials()$user_auth) {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  } else {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  }
})

