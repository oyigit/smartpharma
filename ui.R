
dashboardPage(
  dashboardHeader(title = "Smart Pharma"),
  dashboardSidebar(
    uiOutput('sidebar')
  ),
  dashboardBody(
    uiOutput('welcome')
  )
)