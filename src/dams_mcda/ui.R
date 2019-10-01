# This is the user-interface definition of a Shiny web application

library(shiny)
library(dplyr, warn.conflicts=FALSE)
library(shinyjs, warn.conflicts=FALSE)
library(Cairo)
library(ggplot2)
library(RColorBrewer)
library(spatstat)
library(rgdal)
library(abind)
library(leaflet)
library(DT)

options(shiny.usecairo=TRUE)

source("server.R")

# default graph dimensions
graph_width <- "100%"
graph_height <- 500

# default map dimensions
map_width <- "100%"
map_height <- 500

# multiple used labels
fishSurvivalLabel <- "Sea-run fish habitat area is measured in hundreds of square meters. It is a proxy criteria estimated as possible upstream \
sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) functional habitat (Roy et al., 2018). "
riverRecLabel <- "River recreation is measured in square kilometers. It is the estimated downstream area of river that may increase \
or decrease with a dam decision alternative, represents functional area for whitewater recreation defined by Roy et al. (2018)."
resStorageLabel <- "Reservoir storage is measured in cubic kilometers. It is the estimated storage potential of the reservoir, based\ 
on its volume (Roy et al., 2018)."
annuitizedProjCostsLabel <- "Annuitized project cost is measured in 2018 $USD/year. It is the estimated total project costs (capital \
and operation & maintenance) on an annual basis using a 6.2% discount rate and a 20-year lifetime."
breachDamageLabel <- "Breach damange potential is a unitless proxy for safety based on the State hazard rating, which indicates the \
potential for downstream property damage, injury, and death in the case of dam breach (Roy et al., 2018)."
numPropertiesLabel <- "Number of properties is the estimated number of properties impacted near the dam, based on potential changes \
in viewshed or property value (Roy et al., 2018). "
annualElectricityLabel <- "Annual electricity generation is measured in GWh/year. It is the average estimate based on nameplate \
capacity from FERC licenses for each hydropower project."
GHGEmissionsLabel <- "Annual carbon dioxide (CO2) emissions reduction is measured in metric kilotonnes of CO2/year. It is an  \
estimate of avoided carbon dioxide emissions from annual hydropower-generated electricity production (reservoir or diversion-design \
dams); based on decreasing generation from the State's electricity generation mix; includes life cycle emissions impacts."
indigenousLifewaysLabel <- "Indigenous cultural traditions and lifeways is a unitless rating to convey the importance of preserving\
or restoring the cultural traditions and lifeways of indigenous people."
communityIdentityLabel <- "Community identity is a unitless rating to convey the importance of preserving the existing identity of \
the community residents living along or on islands within the river."
industrialHistoryLabel <- "Industrial historical importance is a unitless rating to convey the importance of preserving or restoring\
the industrial history of the site. "
aestheticsLabel <- "Aesthetic value is a rating to convey the importance of improving or preserving the aesthetics (e.g, appearance,\
scenic value, smell, sound)."
healthLabel <- "Public health is a unitless rating to convey the importance of public health connected to air, water, and land\
pollution."
justiceLabel <- "Socio-environmental justice is a unitless rating to convey the importance of socio-environmental justice issues\
(e.g. negative environmental effects that target disadvantaged groups - people of lower socio-economic status or with less political \
or economic power)."



# Define UI for Shiny web application
ui <- shinyUI(fluidPage(
	# used for toggling display of elements
	shinyjs::useShinyjs(),

	# next 5 lines for testing data passing through javascript
	includeScript("www/dams_mcda.js"),

	# link css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "dams_mcda.css")
	),

	# page banner/title + logout link
	HTML('<div id="app-logout"><a href="/logout/"><i class="fas fa-sign-out-alt"></i>&nbsp;Logout</a></div>\
		 <div id="page-title">Dam Decision Support Tool</div> \
	'),

	# bottom of page total progress tracker
	div(id="progress-tracker", htmlOutput("TotalProgress")),

	# navigation buttons
	div(id="nav-buttons",
		actionButton("Prev_Tab", "Previous"),
		actionButton("Next_Tab", "Next")
	),

	# tablist/tabcontent
	navlistPanel(
		id="navListPanel",
		# Define layout widths
		widths = c(2,10),

		# --------------------------------------------------------------------------------
		# Define Instructions tab
		# --------------------------------------------------------------------------------
		HTML("<li class='step-label'>Step 1: Start Here</li>"),
		tabPanel("Start Here",
			icon=icon("home"),
			h2("Welcome!"),

			img(src = 'River1.jpg', width="50%", align = "right", style="margin:0.2em 0 0.2em 1em;"),

			HTML(
				"<p>This <b>free and open source</b> tool was developed to aid people like you in considering tradeoffs associated with dams. It can help support your consideration of possible decision alternatives (e.g. keep and maintain dam, \
				 improve hydropower generation, improve fish passage) for hydropower dams, and could potentially be tailored toward other types of dam decisions. The tool generates a data-driven recommendation for Federal Energy Regulatory Commission \
				 (FERC) licensed hydropower dams in Maine's Penobscot River, based on your preferences. This tool is based on the Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision-maker preferences for decision criteria \
				 (e.g., annuitized cost, CO2 equivalent emissions reductions, sea-run fish habitat area, etc.) for hydropower dams with FERC license expiration dates in the next 10 years. The tool gathers user inputs and calculates a ranked set of decision \
				 alternatives for each dam. Then, the tool optimizes across the set of dams coming up for relicensing to suggest a coordinated set of decision alternatives for multiple dams. The purpose of the coordinated multi-dam recommendation\
				 is to encourage you to consider dams on the river as a system, in addition to one at a time.\ <br>
				 <br><b>NOTE:</b> The results from this decision support tool are not official to any FERC licensing process and do not in any way represent the ruling of FERC.<br></p>"
			),

			helpText(
			  HTML('<a href = "BackgroundDamDecisionSupportTool.pdf" target="_blank">Click for background on the Dam Decision Support Tool</a>')
			  ),
			helpText(
			  HTML('<a href = "Tool_Instructions.pdf" target="_blank">Click for instructions on how to use the Dam Decision Support Tool</a>')
			),
			helpText(
			  HTML('<a href = "https://www.ferc.gov/industries/hydropower/gen-info/licensing/ilp.asp" target="_blank">Click for more information about the FERC Integrated Licensing Process</a>')
			  ),

			HTML(
				"<h4>More Information:</h4>\

				The Penobscot River is home to many dams and supports valuable ecosystem services. The watershed inclues pristine natural lakes, clean water sources, and significant biodiversity, including several sea-run fish species (e.g. Atlantic salmon, American eel, \
				Blueback herring, and Alewife). Dams also provide important services: reservoirs for drinking water and recreation, flood protection, and generation of reliable, on-demand renewable hydropower, critical to reducing fossil-fuel emissions that contribute \
				to climate change and poor human health. However, all dams need regular maintenance and older dams may need more extensive repairs as they age. Dams may interrupt flows and prevent sea-run fish passage, contributing to large population declines. \
				They may also contribute to poor water quality downstream, increased predation, and climate change (decaying plant matter in reservoirs release methane into the atmosphere). Dams have long threatened indigenous cultural traditions, while at the same \
				time helping to shape post-industrial town or city identities over the last two centuries.<br>"
			 ),

			helpText(
			  HTML(
				  '<a href = "http://www.pnas.org/content/early/2018/10/31/1807437115" target="_blank">Click HERE for more information about the tradeoffs involved in dam decision making</a>')
			  )
		  ),


		# --------------------------------------------------------------------------------
		# Interactive Dam Map Tab
		# --------------------------------------------------------------------------------
		HTML("<li class='step-label'> Step 2: View Dam Map </li>"),
		tabPanel("View Dam Map",
			htmlOutput("Map1"), # status and title
			h2("View Existing FERC Dams Map"),
			HTML("Please consider the following dams on the Penobscot River. These non-federally owned dams are coming up for FERC relicensing within the next 10 years. These are the dams you will focus on \
				for the rest of the activity. Note: although the Penobscot Mills Project dams are licensed together under a single FERC license, we separate them here for consistency. \
				Hover over the dams on the map for more information on each site.<br>"
			),
			helpText(
				HTML('<a href="DecisionAlternativesDescriptions.pdf" target="_blank">Click for more information about dam decision alternatives</a><br>')
			),
			helpText(
			  HTML('<a href="DecisionCriteriaDescriptions.pdf" target="_blank">Click for more information about decision criteria</a><br>')
			),
			helpText(
			  HTML('<a href="DecisionMatrices_All.xlsx" target="_blank">Click to download Dam Decision Matrices</a><br>')
			),
			HTML("You may wish to refer to the resource links above and the watershed map below throughout the activity. <br>"
			),
			leafletOutput("dam_map", width=map_width, height=map_height),
			h2("Example Map Output"),
			HTML(
				"Below is an example of what the multi-dam map output will look like. For example, if no change is recommended based on site-specific data and user preference inputs, all dam sites will be marked KEEP AND MAINTAIN . <br>"
			),
			img(src = 'Penobscot_MO_14_443.png', width = "75%", align = "center")
		),


		# --------------------------------------------------------------------------------
		# Preference Elicitation Tool
		# --------------------------------------------------------------------------------
		HTML("<li class='step-label'> Step 3: Enter Preferences </li>"),
		# ----------------------------------------
		# West Enfield Dam
		# ----------------------------------------
		tabPanel(
			 # tab button text
			 htmlOutput("Dam1"),

			 # tab content
			h2("West Enfield Dam (FERC No. P-2600)"),
			HTML('Please consider and rate your preference for the decision criteria listed below for West Enfield Dam. <a href="Factsheet_WestEnfield.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_WestEnfield.pdf" target="_blank">Open in new tab</a>. \
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
         bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "WestEnfield_RawDecisionMatrix.pdf" target="_blank">Click to view West Enfield Data</a>.<br>\
         <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				 increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
				 <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam1Progress"),

			#----------------------------------------
			# Criteria Inputs for West Enfield Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-1",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat1", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-1",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec1", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-1",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir1", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-1",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost1", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-1",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage1", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-1",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties1", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-1",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration1", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-1",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions1", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-1",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways1", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-1",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory1", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-1",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity1", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-1",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics1", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-1",
			    h3("Public Health"),
			    sliderInput(inputId = "Health1", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-1",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice1", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			# update alt 1 score
			htmlOutput("UpdateDam1Progress"),
			actionButton("updateBtn1", "Update")

		), # End West Enfield Dam Tab

		# ----------------------------------------
		# Medway Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam2"),

			# tab content
			h2("Medway Dam (FERC No. P-2666)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for Medway Dam. <a href="Factsheet_Medway.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_Medway.pdf" target="_blank">Open in new tab</a>.<br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "Medway_RawDecisionMatrix.pdf" target="_blank">Click to view Medway Data</a>. <br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam2Progress"),

			#----------------------------------------
			# Criteria Inputs for Alt 2
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-2",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat2", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-2",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec2", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-2",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir2", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-2",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost2", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-2",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage2", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-2",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties2", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-2",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration2", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-2",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions2", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-2",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways2", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-2",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory2", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-2",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity2", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-2",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics2", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-2",
			    h3("Public Health"),
			    sliderInput(inputId = "Health2", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-2",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice2", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam2Progress"),
			actionButton("updateBtn2", "Update")

		), # End Medway Dam Tab


		# ----------------------------------------
		# East Millinocket Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam3"),

			# tab content
			h2("East Millinocket Dam (FERC No. P-2458)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for East Millinocket Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a>. <br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete.<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>. <br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam3Progress"),

			#----------------------------------------
			# Criteria Inputs for East Millinocket Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-3",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat3", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-3",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec3", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-3",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir3", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-3",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost3", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-3",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage3", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-3",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties3", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-3",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration3", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-3",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions3", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-3",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways3", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-3",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory3", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-3",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity3", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-3",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics3", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-3",
			    h3("Public Health"),
			    sliderInput(inputId = "Health3", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-3",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice3", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam3Progress"),
			actionButton("updateBtn3", "Update")

		), # End East Millinocket Dam


		# ----------------------------------------
		# DOlby Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam4"),

			# tab content
			h2("Dolby Dam (FERC No. P-2458)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for Dolby Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a><br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete.<br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data (PDF has information on all 5 dams, scroll to view)</a>')
			),
			htmlOutput("Dam4Progress"),

			#----------------------------------------
			# Criteria Inputs for Dolby Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-4",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat4", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-4",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec4", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-4",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir4", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-4",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost4", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-4",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage4", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-4",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties4", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-4",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration4", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-4",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions4", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-4",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways4", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-4",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory4", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-4",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity4", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-4",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics4", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-4",
			    h3("Public Health"),
			    sliderInput(inputId = "Health4", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-4",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice4", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam4Progress"),
			actionButton("updateBtn4", "Update")

		), # End Dolby Dam Tab


		# ----------------------------------------
		# North Twin Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam5"),

			# tab content
			h2("North Twin Dam (FERC No. P-2458)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for North Twin Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a>. <br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data (PDF has information on all 5 dams, scroll to view)</a><br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam5Progress"),

			#----------------------------------------
			# Criteria Inputs for North Twin Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-5",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat5", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-5",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec5", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-5",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir5", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-5",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost5", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-5",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage5", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-5",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties5", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-5",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration5", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-5",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions5", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-5",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways5", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-5",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory5", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-5",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity5", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-5",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics5", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-5",
			    h3("Public Health"),
			    sliderInput(inputId = "Health5", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-5",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice5", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam5Progress"),
			actionButton("updateBtn5", "Update")

		), # End North Twin Dam Tab


		#----------------------------------------
		# Millinocket/Quakish Dam
		#----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam6"),

			# tab content
			h2("Millinocket/Quakish Dam (FERC No. P-2458)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for Millinocket/Quakish Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a>. <br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data (PDF has information on all 5 dams, scroll to view)</a>. <br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam6Progress"),

		  #----------------------------------------
		  # Criteria Inputs for Millinocket/Quakish Dam
		  #----------------------------------------
		  #Fish Survival
		  div(id="fish-survival-6",
		      h3("Sea-Run Fish Habitat Area"),
		      sliderInput(inputId = "FishHabitat6", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #River Recreation
		  div(id="river-rec-6",
		      h3("River Recreation"),
		      sliderInput(inputId = "RiverRec6", label = riverRecLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Reservoir Storage
		  div(id="res-storage-6",
		      h3("Reservoir Storage"),
		      sliderInput(inputId = "Reservoir6", label = resStorageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annuitized Project Costs
		  div(id="ann-proj-costs-6",
		      h3("Annuitized Project Costs"),
		      sliderInput(inputId = "ProjectCost6", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Breach Damage Potential
		  div(id="breach-damage-6",
		      h3("Breach Damage Potential"),
		      sliderInput(inputId = "BreachDamage6", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Number of Properties
		  div(id="num-prop-6",
		      h3("Number of Properties"),
		      sliderInput(inputId = "NumProperties6", label = numPropertiesLabel, value=0, min=0, max=100, step =5)
		  ),
		  #Annual Electricity Generation
		  div(id="ann-elec-gen-6",
		      h3("Annual Electricity Generation"),
		      sliderInput(inputId = "ElectricityGeneration6", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #GHGEmissions
		  div(id="ghg-emissions-6",
		      h3("Annual CO2 Emissions Reductions"),
		      sliderInput(inputId = "AvoidEmissions6", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndigenousLifeways
		  div(id="indig-lifeways-6",
		      h3("Indigenous Cultural Traditions and Lifeways"),
		      sliderInput(inputId = "IndigenousLifeways6", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndustrialHistory
		  div(id="industrial-6",
		      h3("Industrial Historical Importance"),
		      sliderInput(inputId = "IndustrialHistory6", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #CommunityIdentity
		  div(id="community-6",
		      h3("Community Identity"),
		      sliderInput(inputId = "CommunityIdentity6", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Aesthetics
		  div(id="aesthetics-6",
		      h3("Aesthetic Value"),
		      sliderInput(inputId = "Aesthetics6", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #PublicHealth
		  div(id="health-6",
		      h3("Public Health"),
		      sliderInput(inputId = "Health6", label = healthLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Justice
		  div(id="justice-6",
		      h3("Socio-Environmental Justice"),
		      sliderInput(inputId = "Justice6", label = justiceLabel, value=0, min=0, max=100, step = 5)
		  ),

		  htmlOutput("UpdateDam6Progress"),
		  actionButton("updateBtn6", "Update")

		), # End Millinocket/Quakish Dam Tab


		# ----------------------------------------
		# Millinocket Lake Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam7"),

			# tab content
			h2("Millinocket Lake Dam (FERC No. P-2458)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for Millinocket Lake Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a>. <br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data (PDF has information on all 5 dams, scroll to view)</a>. <br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam7Progress"),

		  #----------------------------------------
		  # Criteria Inputs for Millinocket Lake Dam
		  #----------------------------------------
		  #Fish Survival
		  div(id="fish-survival-7",
		      h3("Sea-Run Fish Habitat Area"),
		      sliderInput(inputId = "FishHabitat7", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #River Recreation
		  div(id="river-rec-7",
		      h3("River Recreation"),
		      sliderInput(inputId = "RiverRec7", label = riverRecLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Reservoir Storage
		  div(id="res-storage-7",
		      h3("Reservoir Storage"),
		      sliderInput(inputId = "Reservoir7", label = resStorageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annuitized Project Costs
		  div(id="ann-proj-costs-7",
		      h3("Annuitized Project Costs"),
		      sliderInput(inputId = "ProjectCost7", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Breach Damage Potential
		  div(id="breach-damage-7",
		      h3("Breach Damage Potential"),
		      sliderInput(inputId = "BreachDamage7", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Number of Properties
		  div(id="num-prop-7",
		      h3("Number of Properties"),
		      sliderInput(inputId = "NumProperties7", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annual Electricity Generation
		  div(id="ann-elec-gen-7",
		      h3("Annual Electricity Generation"),
		      sliderInput(inputId = "ElectricityGeneration7", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #GHGEmissions
		  div(id="ghg-emissions-7",
		      h3("Annual CO2 Emissions Reductions"),
		      sliderInput(inputId = "AvoidEmissions7", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndigenousLifeways
		  div(id="indig-lifeways-7",
		      h3("Indigenous Cultural Traditions and Lifeways"),
		      sliderInput(inputId = "IndigenousLifeways7", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndustrialHistory
		  div(id="industrial-7",
		      h3("Industrial Historical Importance"),
		      sliderInput(inputId = "IndustrialHistory7", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #CommunityIdentity
		  div(id="community-7",
		      h3("Community Identity"),
		      sliderInput(inputId = "CommunityIdentity7", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Aesthetics
		  div(id="aesthetics-7",
		      h3("Aesthetic Value"),
		      sliderInput(inputId = "Aesthetics7", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #PublicHealth
		  div(id="health-7",
		      h3("Public Health"),
		      sliderInput(inputId = "Health7", label = healthLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Justice
		  div(id="justice-7",
		      h3("Socio-Environmental Justice"),
		      sliderInput(inputId = "Justice7", label = justiceLabel, value=0, min=0, max=100, step = 5)
		  ),


			htmlOutput("UpdateDam7Progress"),
			actionButton("updateBtn7", "Update")

		), # End Millinocket Lake Dam Tab


		# ----------------------------------------
		# Ripogenus Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam8"),

			# tab content
			h2("Ripogenus Dam (FERC No. P-2572)"),

			HTML('Please consider and rate your preference for the decision criteria listed below for Ripogenus Dam. <a href="Factsheet_Ripogenus.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a>. <br>\
				 Move the slider bar for each decision criterion you care about to a position that represents the relative amount of preference you have for that decision criterion compared to others in the list. Once you have made your selections, click UPDATE at the \
			     bottom of the page when you are done moving the slider bars to mark this tab Complete. <a href = "Ripogenus_RawDecisionMatrix.pdf" target="_blank">Click to view Ripogenus Data</a>. <br>\
			     <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\
			     increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100). <br>\
			     <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			htmlOutput("Dam8Progress"),

			#----------------------------------------
			# Criteria Inputs for Ripogenus Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-8",
				h3("Sea-Run Fish Habitat Area"),
				sliderInput(inputId = "FishHabitat8", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-8",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec8", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-8",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir8", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-8",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost8", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-8",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "BreachDamage8", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-8",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties8", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-8",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration8", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-8",
				h3("Annual CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions8", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-8",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways8", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-8",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory8", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-8",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity8", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-8",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics8", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-8",
				h3("Public Health"),
				sliderInput(inputId = "Health8", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-8",
				h3("Socio-Environmental Justice"),
				sliderInput(inputId = "Justice8", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam8Progress"),
			actionButton("updateBtn8", "Update")

		), # End Ripogenus Dam Tab

		# --------------------------------------------------------------------------------
		# Combined Results Tab
		# --------------------------------------------------------------------------------
		HTML("<li> Step 4: Multi-Dam Results </li>"),

		tabPanel("Combined Results",

		  h2('Multi-Dam Results'),

		  HTML("<br>Click GENERATE to get MCDA results graphs.<br>"),
		  actionButton("generateOutput", "Generate", icon=icon("chart-bar")), # generate event

			div(id="combined-output",
			  h2('Overview'),
			  HTML(
				   "<br>Based on the preference values you entered for each dam in Step 3 and the data we collected/generated previously about what will happen to each decision criterion if each of the five decision alternatives is adopted, we \
				   have generated a coordinated recommendation for the entire set of dams, which you can view in the Map Recommendation link to the left. In the figures below, you can compare the numeric score for each dam across decision \
				   criterion and decision alternatives. Step 5 (individual dams) results provide more detail about each individual dam. However, bear in mind that data for sea-run fish habitat area and river recreation are network-dependent, \
				   meaning that a change to one dam may affect results at another dam. Therefore, the numeric results in Step 5 may differ from what is presented here due to the site-specific nature of the fish habitat and river recreation decision \
				   criteria data estimates. The outcome presented here is a recommendation designed to support the coordinated consideration of multiple dams. This recommendation is intended to support brainstorming about possibilities for the river. \
				   This recommendation is not representative of any federal agency prescription or license ruling from FERC.<br>\
				   <br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the dam tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each tab. Then, return to this page and click GENERATE\
				   once more to see how your results change (note: you may want to download your results from this session, first).\
				   <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one criterion increases, the value for another \
				   criterion must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br>"
			  ),

			  h3('Figure 1. Comparison of Top 5 Decision Scenarios by Total MCDA Score and Dam'),
			  plotOutput("CombinedPlot4", height="35em"),
			  downloadButton("DownloadCombinedPlot4", "Download Graph", style="width:100%"),
			  HTML("<br><br><b>Results Interpretation</b> for Figure 1. This graph shows the Total MCDA Score of the top 5 decision scenarios broken down by the contribution from each dam/decision alternative combination. In this context, the \"Decision Scenario\" \
				   refers to the combination of dams and decision alternatives (e.g., the action that is recommended for each dam). The decision scenario with the highest score is presented in the mapped result, which you can access by clicking on Map Recommendation \
				   to the left. The Final MCDA score for each Decision Scenario is calculated as the sum of the Final MCDA Score for highest-scoring decision alterative for each dam. Actual numbers in Scenario 1 may differ from top-ranked decision alternative results \
				   for individual dams in Figure 2 because Figure 2 was calculated using static values for river recreation and sea-run fish habitat area, while Figure 1 shows the results of the multi-dam optimization that involves dynamic interactions between dam sites \
				   for these two decision criteria. Use Figure 1 to see how close the final MCDA scores for top-ranked decisions are. This could give you some ideas, in the event that you do not agree with what is presented in the Map Recommendation (Scenario 1), about \
				   what the next-best options may be for your decision.<br>"),

			  HTML("<br>The following downloadable supplementary tables can be used to see more detail relted to Figure 1. Note: there are 1885 possile multi-dam 'scenarios' in this dataset, where each of 8 dams has 5 possible decision alternatives. <br>"),
			  downloadButton("DownloadDecisions", "Download Decisions", style="width:49%; display:inline-block"),
			  downloadButton("DownloadRankedScenarios", "Download Top Ranking Scenarios", style="width:49%; display:inline-block;"),
			  HTML(
			    "<br>Information supporting a more precise interpretation of these supplementary tables is forthcoming.<br>"
			  ),

				h3('Figure 2. Comparison of Final MCDA Score for each Decision Alternative at each Dam'),
				plotOutput("CombinedPlot3", height="35em"),
				downloadButton("DownloadCombinedPlot3", "Download Graph", style="width:100%"),
				HTML("<br><br><b>Results Interpretation</b> for Figure 2. This graph shows the final MCDA score for each decision alternative at each dam. The taller the bar, the more preferred the alternative is. Use this graph to quickly see which decision\
					 alternative was selected by the tool as the most highly recommended for each dam and to see how close the \"next-best\" alternative was to the top-ranked alternative. Small differences between scores for decision alternatives may signal \
					 a need to carefully consider multiple top-ranked alternatives for a specific dam for your final decision. Actual numbers for individual dams in Figure 2 may differ from Scenario 1 in Figure 1 because Figure 2 was calculated using static \
					 values for river recreation and sea-run fish habitat area, while Figure 1 shows the results of the multi-dam optimization that involves dynamic interactions between dam sites for these two decision criteria. Use Figure 1 to see how close \
					 the final MCDA scores for top-ranked decisions are.<br>"),

				h3('Figure 3. Contribution of Decision Criteria to Final MCDA Score for Top-Ranked Dam Decision Alternatives'),
				plotOutput("CombinedPlot2", height="35em"),
				downloadButton("DownloadCombinedPlot2", "Download Graph", style="width:100%;"),
				HTML("<br><br><b>Results Interpretation</b> for Figure 3. This graph shows the final MCDA score for the top-ranked decision alternative for each dam, broken down by the relative contribution of each decision criterion to the total score. \
					 Use this graph to give you an idea of how decision criteria scores (normalized data values x user-defined preference values) contribute to the final decision alternative selection for each dam. Consider this graph kind of a \"zoom-in\" \
					 on the tallest bar for each dam in Figure 2.<br>"),

				h3('Figure 4. Comparison of User-Defined Preferences for each Dam'),
				plotOutput("CombinedPlot1", height="35em"),
				downloadButton("DownloadCombinedPlot1", "Download Graph", style="width:100%;"),
				HTML("<br><br><b>Results Interpretation</b> for Figure 4. This graph shows the results of the Step 3 Preference Elicitation. The numbers presented on the graph correspond to the position of the slider bars you moved in Step 3. \
					 Use this graph to see how your preferences for each decision criterion vary across dam sites. For example, did you mark the same preference value for River Recreation for Ripogenus as for West Enfield? Are your fish \
					 habitat preferences consistent across all dams? This graph is a good reality check to help you make sure you entered the right preferences in Step 3 for you. If you did not move the slider for a specific decision criterion \
					 for any dam, it will not show up in this graph. For example, if you left the slider for River Recreation at zero for all dams, River Recreation will not appear in the graph or legend on this page. The numbers in this graph \
					 do not include any researched data values for decision criteria (i.e., the data we compiled and/or calculated ahead of time and presented in the decision matrices for each dam). The graph only shows your own stated preference \
					 values from Step 3.<br>"),

				# download preferences (for UPLOAD DATA)
				h3("Download Preferences"),
				HTML(
				  "It is a good idea to download your preferences for your records if you plan to use the Dam Decision Support Tool again.<br>"
				),
				downloadButton("downloadPreferenceSelection", "Download Preference Inputs (Step 3)", style="width:100%;"),

				# save preferences to server
				h3("Save Preferences"),
				HTML("Saving your preferences will load them automatically when you visit again. If you are using group mode, saving will add your preferences to the group average. Be aware: saving again will overwrite the old save.<br>"),
				actionButton("saveResultsToDjango", "Save Preferences", icon=icon("save"))
			)
		),


		# --------------------------------------------------------------------------------
		# Map Recommentation Tab
		# --------------------------------------------------------------------------------
		tabPanel("Map Recommendation",
			h2("Optimized Result"),
			HTML('<div id="MapRecommendation"></div>'),
			downloadButton("downloadMapRecommendation", "Download Map", style="width:100%;"),
			HTML("This mapped result is a geographic representation of the coordinated, multi-dam results presented in Figures 1 of the Multi-Dam Results tab. It shows which decision alternatives were selected for which dams based on your \
			     preferences, the decision criteria data we collected/generated, and the multi-objective optimization that includes network-dependent estimates of river recreation and fish habitat (i.e., changes at one dam affect other dams) within the\
				 set of possible decision alternatives at each of the 8 dams. This network-dependency (i.e., connection to dams both upstream and downstream) means that individual dam results in Step 5, as well as Figures 2 and 3 of the Multi-Dam Results \
			     Tab may differ from what this map recommends. <br>"
			)
		),


		# --------------------------------------------------------------------------------
		# Individual Dam Result Tabs
		# --------------------------------------------------------------------------------
		HTML("<li class='step-label'> Step 5: View Dam Specific Results </li>"),

		tabPanel("Dam 1: West Enfield",
			h2("Results: West Enfield Dam"),
			HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
            Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
            values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
            results.<br>"),

			# output post generate
			div(id="generated-output-1",
				h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
				plotOutput("WSMPlot1b", height=600, width="100%"),
				downloadButton("DownloadDam1Plotb", "Download Graph", style="width:100%;"),
				HTML(
					 "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
					 preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
					 preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
					 <br>"
				),

				h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
				plotOutput("WSMPlot1a", height=600, width="100%"),
				downloadButton("DownloadDam1Plota", "Download Graph", style="width:100%;"),
				HTML(
					"<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
					of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
					drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
					that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
					this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
					preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
					removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
					<br>"
				)
			),

			h2('Data Inputs to these Results'),
		    HTML(
		      "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		      component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		      (raw and normalized) that lead to the final calculation.<br>"
		    ),

			div(id="dam-1-output",

				h3("Figure 3. User-Defined Preference Scores for West Enfield Dam"),
				plotOutput("PrefPlot1", height=graph_height, width=graph_width),
				HTML(
				  "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
				  under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
				),

				h3("Table 1. User-Defined Preference Scores for West Enfield Dam"),
				DT::dataTableOutput("RawPrefsDam1"),
				HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
					  above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

				#raw data table/Matrix
				h3("Table 2. Data Values for West Enfield Dam"),
				DT::dataTableOutput("Dam1RawTable"),
				HTML(
				  "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
				  remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
				  more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
				  in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
				  changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
				  modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
				),

				#normalized data table/Matrix
				h3("Table 3. Normalized Data Values for West Enfield Dam"),
				DT::dataTableOutput("Dam1NormTable"),
				HTML(
				  "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
				  using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
				  in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
				  damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
				  data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
				),

				#weighted score data table/Matrix
				h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for West Enfield Dam"),
				DT::dataTableOutput("Dam1ScoreTable"),
		        downloadButton("DownloadDam1ScoreTable", "Download Table", style="width:100%;"),
				HTML(
				  "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
				  If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
				),

				h3('Download West Enfield Results'),
				HTML(
					"<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
				),
				downloadButton("downloadData1", "Download Table", style="width:100%;")
			)
		),

		tabPanel("Dam 2: Medway",

		         h2("Results: Medway Dam"),
		         HTML(
               "Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
               Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		           values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		           results.<br>"
             ),

		         # output post generate
		         div(id="generated-output-2",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot2b", height=600, width="100%"),
		             downloadButton("DownloadDam2Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot2a", height=600, width="100%"),
		             downloadButton("DownloadDam2Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),

		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-2-output",

		             h3("Figure 3. User-Defined Preference Scores for Medway Dam"),
		             plotOutput("PrefPlot2", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for Medway Dam"),
		             DT::dataTableOutput("RawPrefsDam2"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

		             #raw data table/Matrix
		             h3("Table 2. Data Values for Medway Dam"),
		             DT::dataTableOutput("Dam2RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
    				      remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
    				      more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
    				      in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
    				      changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
    				      modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),

		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for Medway Dam"),
		             DT::dataTableOutput("Dam2NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
				      using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
				      in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
				      damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
				      data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),

		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for Medway Dam"),
		             DT::dataTableOutput("Dam2ScoreTable"),
		             downloadButton("DownloadDam2ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
				           If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		             h3('Download Medway Results'),
                 HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData2", "Download Medway")
				 )
		),

		tabPanel("Dam 3: East Millinocket",
		         h2("Results: East Millinocket Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
                  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-3",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot3b", height=600, width="100%"),
		             downloadButton("DownloadDam3Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot3a", height=600, width="100%"),
		             downloadButton("DownloadDam3Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),
		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-3-output",

		             h3("Figure 3. User-Defined Preference Scores for East Millinocket Dam"),
		             plotOutput("PrefPlot3", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for East Millinocket Dam"),
		             DT::dataTableOutput("RawPrefsDam3"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

		             #raw data table/Matrix
		             h3("Table 2. Data Values for East Millinocket Dam"),
		             DT::dataTableOutput("Dam3RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
		               remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
		               more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
		               in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
		               changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
		               modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),

		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for East Millinocket Dam"),
		             DT::dataTableOutput("Dam3NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
		               using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
		               in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
		               damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
		               data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),

		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for East Millinocket Dam"),
		             DT::dataTableOutput("Dam3ScoreTable"),
		             downloadButton("DownloadDam3ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
		               If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		             h3('Download East Millinocket Results'),
					 HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData3", "Download East Millinocket", style="width:100%;")
				 )
		),

		tabPanel("Dam 4: Dolby",
		         h2("Results: Dolby Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
					  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-4",
		             h3('Figure 4. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot4b", height=600, width="100%"),
		             downloadButton("DownloadDam4Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot4a", height=600, width="100%"),
		             downloadButton("DownloadDam4Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),
		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),
		         
		         div(id="dam-4-output",
		             
		             h3("Figure 3. User-Defined Preference Scores for Dolby Dam"),
		             plotOutput("PrefPlot4", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),
		             
		             h3("Table 1. User-Defined Preference Scores for Dolby Dam"),
		             DT::dataTableOutput("RawPrefsDam4"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),
		             
		             #raw data table/Matrix
		             h3("Table 2. Data Values for Dolby Dam"),
		             DT::dataTableOutput("Dam4RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
				      remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
				      more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
				      in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
				      changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
				      modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),
		             
		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for Dolby Dam"),
		             DT::dataTableOutput("Dam4NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
				      using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
				      in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
				      damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
				      data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),
		             
		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for Dolby Dam"),
		             DT::dataTableOutput("Dam4ScoreTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
				      If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),
		             downloadButton("DownloadDam4ScoreTable", "Download Table", style="width:100%;"),
		             h3('Download Dolby Results'),
		             HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData4", "Download Dolby", style="width:100%;")
				 )
		 ),

		tabPanel("Dam 5: North Twin",
		         h2("Results: North Twin Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
					  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-5",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot5b", height=600, width="100%"),
		             downloadButton("DownloadDam5Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot5a", height=600, width="100%"),
		             downloadButton("DownloadDam5Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),
		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-5-output",

		             h3("Figure 3. User-Defined Preference Scores for North Twin Dam"),
		             plotOutput("PrefPlot5", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for North Twin Dam"),
		             DT::dataTableOutput("RawPrefsDam5"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),
		             
		             #raw data table/Matrix
		             h3("Table 2. Data Values for North Twin Dam"),
		             DT::dataTableOutput("Dam5RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
		               remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
		               more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
		               in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
		               changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
		               modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),
		             
		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for North Twin Dam"),
		             DT::dataTableOutput("Dam5NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
		               using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
		               in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
		               damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
		               data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),
		             
		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for North Twin Dam"),
		             DT::dataTableOutput("Dam5ScoreTable"),
		             downloadButton("DownloadDam5ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
		               If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		             h3('Download North Twin Results'),
		             HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData5", "Download North Twin", style="width:100%;")
				 )
		 ),

		tabPanel("Dam 6: Millinocket/Quakish",
		         h2("Results: Millinocket/Quakish Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
					  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-6",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot6b", height=600, width="100%"),
		             downloadButton("DownloadDam6Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot6a", height=600, width="100%"),
		             downloadButton("DownloadDam6Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),

		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-6-output",

		             h3("Figure 3. User-Defined Preference Scores for Millinocket/Quakish Dam"),
		             plotOutput("PrefPlot6", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for Millinocket/Quakish Dam"),
		             DT::dataTableOutput("RawPrefsDam6"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

		             #raw data table/Matrix
		             h3("Table 2. Data Values for Millinocket/Quakish Dam"),
		             DT::dataTableOutput("Dam6RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
				      remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
				      more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
				      in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
				      changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
				      modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),

		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for Millinocket/Quakish Dam"),
		             DT::dataTableOutput("Dam6NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
				      using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
				      in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
				      damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
				      data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),

		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for Millinocket/Quakish Dam"),
		             DT::dataTableOutput("Dam6ScoreTable"),
		             downloadButton("DownloadDam6ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
				      If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		            h3('Download Millinocket Results'),
		            HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		            ),
		            downloadButton("downloadData6", "Download Millinocket", style="width:100%;")
				 )
		 ),

		tabPanel("Dam 7: Millinocket Lake",
		         h2("Results: Millinocket Lake Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
					  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-7",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot7b", height=600, width="100%"),
		             downloadButton("DownloadDam7Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot7a", height=600, width="100%"),
		             downloadButton("DownloadDam7Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),

		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-7-output",

		             h3("Figure 3. User-Defined Preference Scores for Millinocket Lake Dam"),
		             plotOutput("PrefPlot7", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for Millinocket Lake Dam"),
		             DT::dataTableOutput("RawPrefsDam7"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

		             #raw data table/Matrix
		             h3("Table 2. Data Values for Millinocket Lake Dam"),
		             DT::dataTableOutput("Dam7RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
		               remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
		               more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
		               in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
		               changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
		               modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),

		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for Millinocket Lake Dam"),
		             DT::dataTableOutput("Dam7NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
		               using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
		               in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
		               damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
		               data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),

		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for Millinocket Lake Dam"),
		             DT::dataTableOutput("Dam7ScoreTable"),
		             downloadButton("DownloadDam7ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
		               If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		             h3('Download Millinocket Lake Results'),
		             HTML(
		               "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData7", "Download Millinocket Lake", style="width:100%;")
			 )
		 ),

		tabPanel("Dam 8: Ripogenus",
		         h2("Results: Ripogenus Dam"),
		         HTML("Now that you have seen the coordinated multi-dam alternative recommendation, we will drill down to explore the MCDA results for each individual dam.\
					  Remember that these results have been estimated using site-specific data values, so the network-dependent criteria (e.g. sea-run fish habitat area, river recreation)\
		              values are the average for the possible range, which is actually dependent on the network of dams. You should expect that these results will differ somewhat from the multi-dam\
		              results.<br>"),

		         # output post generate
		         div(id="generated-output-8",
		             h3('Figure 1. Comparison of Final MCDA Scores for Each Decision Alternative'),
		             plotOutput("WSMPlot8b", height=600, width="100%"),
		             downloadButton("DownloadDam8Plotb", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 1: This graph shows the final MCDA score for each decision alternative for this specific dam, based on the MCDA calculation that includes the\
		               preferences you entered in Step 3 and the decision criteria data we collected/generated through our research. The taller the bar, the more preferred the decision alternative us under the \
		               preferences you supplied. Use this graph for a quick comparison between decision alternatives for a single dam.<br>\
		               <br>"
		             ),

		             h3('Figure 2. Contribution of Decision Criteria to Final MCDA Score for Dam Decision Alternatives'),
		             plotOutput("WSMPlot8a", height=600, width="100%"),
		             downloadButton("DownloadDam8Plota", "Download Graph", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Figure 2: This graph displays a zoomed-in version of Figure 1, with the final MCDA score bars for each decision alternative divided up by the contribution \
		               of each decision criterion to the total score. Similar to Figure 1, this graph includes your preference information and the researcher-defined data for each decision criterion. Use this graph to \
		               drill down and see which decision criteria are making up the largest portion of the final score for each decision alternative and whether you agree that is the way it should be. Remember, however, \
		               that these data take into account not only your preferences but also the research data. So, even if you gave a decision criterion a low rating in Step 3, it could still make up a large portion of \
		               this graph (unless you marked it as zero) because the data values may be larger relative to the full set of data values for that decision criteria for this dam. For example, if you assigned a 0.1 \
		               preference value to sea-run fish habitat area, you may be surprised to see a large segment for sea-run fish habitat in the Remove Dam decision alternative bar in this graph, but that could be because \
		               removing the dam increases fish habitat the most out of all of the decision alternatives for this dam site.<br>\
		               <br>"
		             )
				 ),

		         h2('Data Inputs to these Results'),
		         HTML(
		           "The results presented in Figures 1 and 2 were calculated by multiplying your preference inputs from Step 3 by the normalized researcher-defined decision criteria data. This section displays these \
		           component data sets, reminding you in table and graph form of the preferences you entered in Step 3 for each decision criterion and showing you the researcher-defined decision criteria data sets \
		           (raw and normalized) that lead to the final calculation.<br>"
		         ),

		         div(id="dam-8-output",

		             h3("Figure 3. User-Defined Preference Scores for Ripogenus Dam"),
		             plotOutput("PrefPlot8", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 3: This graph shows you the preferences you entered in Step 3 for each decision criterion. The scores are pulled directly from your slider bar settings \
		               under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing. Remember to click GENERATE under Step 5. Multi-Dam Results. <br>"
		             ),

		             h3("Table 1. User-Defined Preference Scores for Ripogenus Dam"),
		             DT::dataTableOutput("RawPrefsDam8"),
		             HTML( "<br>This table just shows the same thing as Figure 3 but in table form. If you would like to see all decision criteria preferences values at once, please select Show 25 entries from the drop-down menu \
		                   above the table. Use the search bar to filter the table to a specific decision alternative  (e.g. Keep and Maintain Dam).<br>"),

		             #raw data table/Matrix
		             h3("Table 2. Data Values for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 2. This table displays the raw data values we collected and/or calculated/generated through our research for each decision criterion and alternative. You may \
		               remember seeing these data when you clicked on the link for the data matrix for this dam during the preference elicitation in Step 3. We include the raw data values again here to help make the MCDA calculations \
		               more transparent, so you can clearly see what goes into the final calculation that produces Figures 1 and 2 above. In addition, you can use this table to sort decision alternatives in ascending or descending order\
		               in each column by clicking on the arrow next to the column header . Note: fish survival values shown here are discrete, but in reality, the values are network-dependent and would be impacted by upstream or downstream\
		               changes. They are presented here as the average of a range of possible values for this dam, depending on what happens at other dams. This interaction between decisions at other dams and these decision criteria are\
		               modeled in the multi-objective optimization that leads to the final Map Recommendation.<br>"
		             ),

		             #normalized data table/Matrix
		             h3("Table 3. Normalized Data Values for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 3. This table shows the data values from Table 2, normalized to be a score between 0 and 1 to make them comparable across different units. Normalization was performed \
		               using a min/max procedure: each raw data value was subtracted from the ideal value in the set (e.g., the maximum fish habitat area, for example) and divided by the difference between the maximum and minimum values\
		               in the set. The highest normalized values for most decision criteria, then, equal 1, and the lowest values equal 0. For decision criteria where lower values are more preferable (e.g. annuitized project cost, breach \
		               damage potential, number of properties impacted), the highest values equal 0, and the lowest values equal 1. This allows us to indicate that, for example, high costs are less desirable than low costs. The normalized \
		               data values in this table are multiplied by the preference weights displayed in Figure 3 and Table 1 to calculate the weighted scores in Table 4.<br>"
		             ),

		             #weighted score data table/Matrix
		             h3("Table 4. Weighted Scores for Individual Decision Criteria and Alternatives for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8ScoreTable"),
		             downloadButton("DownloadDam8ScoreTable", "Download Table", style="width:100%;"),
		             HTML(
		               "<br><br><b>Results Interpretation</b> for Table 4. This table shows the result of multiplying the preference scores from Table 1 (and Figure 3) by the normalized decision criteria data values displayed in Table 3. \
		               If you add together all numbers in one row in this table you will get the final MCDA score for that decision alternative, the same results that are presented in Figures 1-2 above.<br>"
		             ),

		             h3('Download Ripogenus Results'),
		             HTML(
		                 "<br><b>Next Steps</b>: You may download and save your results for personal reference. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),
		             downloadButton("downloadData8", "Download Ripogenus", style="width:100%;")
				 )
		),


		# Developer and aknowledgements secion
		HTML("<li class='step-label'>About</li>"),

		tabPanel("Developers",
			 h2("Developers"),
			 HTML(
				"<b>Emma Fox </b>- Lead Developer (Ph.D. candidate, University of Maine Ecology and Environmental Science Program) for the Dam Decision Support Tool. Designed user interface and initial server functionality. Adjusted WSM function for new dam decision application and advised model-related changes. \
				Designed and wrote app text, and designed accompanying multi-dam decision example fact sheets, designed and wrote text for Dam Toolbox.<br> \
				<br><b>Dr. Sharon J. W. Klein </b>- Development Advisor (Associate Professor, University of Maine School of Economics). Helped develop and advise concept for the Dam Decision Support Tool, advised user-friendliness enhancements to the Dam Decision Support Tool and user interface/features, refined criteria \
				definitions, revised app text.<br> \
				<br><b>Dr. Samuel G. Roy </b>- Feature Developer (Postdoctoral Researcher, Senator George J. Mitchell Center for Sustainability Solutions, University of Maine) for the Dam Decision Support Tool. Created original Matlab scripts for multi-rank calculation, fitness functions for network-dependent criteria, and all maps.<br>\
				<br><b>William Winslow </b>- Developer (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire). Deployment (Docker, Apache), server code reorganization, debugging/bug fixes, misc. feature implementations for user interface and app function.<br>"
			 )
		 ),


		tabPanel("Acknowledgements",
			 h2("Acknowledgments"),
			 HTML(
				 "We would like to thank Garrett Raymond for his technical consultation on R Shiny design.<br> \
				 <br> This tool was developed by researchers in the Future of Dams project. Support for the Future of Dams project is provided by the National Science Foundation\'s Research Infrastructure Improvement NSF #IIA-1539071, \
				 USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, \
				 U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.<br>\
				 <br> The Data Discovery Center at the University of New Hampshire is the host for this Dam Decision Support Tool. https://ddc.unh.edu <br>"
			 ),
			 h2("Citations"),
			 HTML(
				 "Roy, S.G., Uchida, E., de Souza, S.P., Blachly, B., Fox, E., Gardner, K., Gold, A.J., Jansujwicz, J., Klein, S., McGreavy, B., Mo, W., Smith, S.M.C., Vogler, E., Wilson, K., Zydlewski, J., & Hart, D. (2018). A multiscale approach to balance trade-offs among dam infrastructure, river restoration, and cost. Proceedings of the National \
				 Academy of Sciences, 201807437. doi:10.1073/pnas.1807437115.<br>\
				 "
			 )
		)
	) # end of navigation panel
))

# create the application with ui in this file and imported server from server.R
shinyApp(ui, server)
