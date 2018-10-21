
dashboardPage(
  dashboardHeader(title = "Smart Pharma"),
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("sidebar")
    )
  ),
  dashboardBody(
    uiOutput('welcome')
  )
)