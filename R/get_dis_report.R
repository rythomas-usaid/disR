# The AJAX payload is like this:

# payload <- list(
#   smartobject = list(
#     guid = "b7570d14-a355-4f8f-bee1-ad6c3fbf47e9",
#     resultName = "External_e94e993e-4a64-4490-a3aa-96b2b9748df6_b7570d14-a355-4f8f-bee1-ad6c3fbf47e9",
#     method = list(
#       name = "X3nkjlwzz8ikfaEbRg02KsvsiEZgd4vDwaKrWac777ofr3In-Parpm7ffoiQGCZv_BuZI1aF9I6q7cLivP8iQxVdo41cYz6lt9gVoacuH8FGZI9Vrmdh0FHC7ZVO67i-0",
#       sorters = list(
#         list(
#           OrderBy = "SortOrder",
#           OrderByResultName = "External_e94e993e-4a64-4490-a3aa-96b2b9748df6_b7570d14-a355-4f8f-bee1-ad6c3fbf47e9",
#           Direction = "Ascending"
#         ),
#         list(
#           OrderBy = "CountryName",
#           OrderByResultName = "External_e94e993e-4a64-4490-a3aa-96b2b9748df6_b7570d14-a355-4f8f-bee1-ad6c3fbf47e9",
#           Direction = "Ascending"
#         )
#       )
#     ),
#     parameters = list(
#       organizationroid = list(value = "da89f822-9fc8-41d4-aa6f-70522a2305d7"),
#       organizationouid = list(value = "49d294c1-c4d4-4e95-bdc1-a4e020f64b0b")
#     ),
#     results = list(
#       list(
#         targetID = "32c46818-6243-493a-adeb-7f1f89cf23c7_e94e993e-4a64-4490-a3aa-96b2b9748df6",
#         targetType = "Control",
#         sourceType = "Result",
#         sourceID = "b7570d14-a355-4f8f-bee1-ad6c3fbf47e9",
#         sourceInstanceID = "32c46818-6243-493a-adeb-7f1f89cf23c7",
#         targetInstanceID = "32c46818-6243-493a-adeb-7f1f89cf23c7"
#       )
#     )
#   ),
#   metadata = list(
#     id = "34251048-3e3a-46d8-981f-91ab073c00be",
#     methodexecuted = "X3nkjlwzz8ikfaEbRg02KsvsiEZgd4vDwaKrWac777ofr3In-Parpm7ffoiQGCZv_BuZI1aF9I6q7cLivP8iQxVdo41cYz6lt9gVoacuH8FGZI9Vrmdh0FHC7ZVO67i-0",
#     typeofview = "Capture",
#     idofcontrol = "32c46818-6243-493a-adeb-7f1f89cf23c7_e94e993e-4a64-4490-a3aa-96b2b9748df6",
#     instanceid = "32c46818-6243-493a-adeb-7f1f89cf23c7",
#     objectid = "b7570d14-a355-4f8f-bee1-ad6c3fbf47e9"
#   )
# )
#
#
# url <- "https://dis2.usaid.gov/Runtime/Runtime/Form/InSysDIS3.IndicatorDataRpt.Form/?ouid=5e9dddea-f095-4e12-9129-678d7ba2d13f&ref=https://dis.usaid.gov/"
#
#
#
# ### start from chatgpt
# library(httr)
#
# url <- "https://example.com/api/endpoint"  # Replace with the actual URL of the AJAX server
# payload <- list(
#   key1 = "value1",
#   key2 = "value2"
# )
#
# # If you need to send JSON data, set the content type to 'application/json'
# headers <- c(
#   "Content-Type" = "application/json",
#   # Add any additional headers if needed
#   "Authorization" = "Bearer your_token"
# )
#
# response <- POST(url, body = payload, encode = "json", add_headers(headers))
#
# # Check the status code
# status_code <- status_code(response)
#
# if (status_code == 200) {
#   # Successful request, parse the response content
#   response_content <- content(response, "text")
#   # Do something with the response_content
# } else {
#   # Handle error
#   cat("Error:", status_code, "\n")
#   print(content(response, "text"))
# }
#
