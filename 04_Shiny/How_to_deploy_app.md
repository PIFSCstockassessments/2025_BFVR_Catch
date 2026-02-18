To deploy the shiny app on NOAA Fisheries Posit Connect site use the following steps: 

1. Make sure you have an account with [NOAA Fisheries Posit Connect](https://connect.fisheries.noaa.gov/)
2. Create an API Key in your profile
3. In VS Code, install `rsconnect` R package
4. Add a server using `addServer(url = "https://connect.fisheries.noaa.gov/")
5. Connect your user account using `connectApiUser()` and supply it with your Posit Connect account user name, and the API Key you generated in step 2. Since you have already added the Posit Connect server, you can leave the server argument blank. **Note** this only needs to be done one time on your first app deployment. For apps following your first one, you can skip this step. 
6. Use the command `deployApp(appDir = "./04_Shiny/",appName = "Deep_7_Non_Commerical_Catch_BFVR", appTitle = "Deep 7 Non Commercial Catch BFVR")` to deploy the app, where appDir is the directory with your shiny `app.r` script and files needed for the app, appName is unique name that will be on the posit connect server and appTitle is what will be displayed. 
