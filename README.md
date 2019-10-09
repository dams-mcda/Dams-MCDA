Dam Decision Support Tool
==========================

Project Description
--------------------------

* R / Shiny Web Application for dam decision support

	+ Located in src/dams_mcda/*

* Python / Django Application for user administration

	+ Located in django-src/shinyadmin/*

* Docker for orchestration

* Apache Configuration for Proxying to Docker


Developers
-------------------------

**Emma Fox**

Lead Developer (Ph.D. student, University of Maine Ecology and Environmental Science Program)

**Involvement**

Designed initial UI/UX and server functionality (early version based on Raymond & Klein (2018)). Wrote weighted-sum (WSM) function for single dam decisions, adapted WSM for multi-dam decisions, and advised model-related changes. Wrote app text.  

**Contact**

<emma.fox@maine.edu>

***

**Sharon J. W. Klein**

Development Advisor (Associate Professor, University of Maine School of Economics)

**Involvement**

Advised UI/UX enhancements to WSM model, helped develop and advised concept for MCDA tool, advised user-friendliness enhancements to MCDA tool and UI/UX, refined decision criteria and alternative definitions, revised app text.

**Contact**

<sharon.klein@maine.edu>

***

**Samuel G. Roy**

Feature Developer (Assistant Professor, jointly appointed through the Senator George J. Mitchell Center for Sustainability Solutions and the School of Earth and Climate Sciences, University of Maine)

**Involvement**

Developed Matlab scripts for multi-dam ranking calculation (later adapted for R), fitness functions for network-dependent criteria, and multi-dam preference weighting (later adapted for R).

**Contact**

<samuel.g.roy@maine.edu>

***

**William Winslow**

Developer (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire)

**Involvement**

* Deployment (Docker, Apache)

* Server code reorganization

* Debugging/Bug fixes

* Misc. feature implementations (UI/UX)

**Contact**

<william.winslow@unh.edu>

***

Acknowledgements
--------------------------
+ We would like to thank Garrett Raymond for his thorough technical consultation on initial R Shiny app design.

+ This tool was developed by researchers in the [Future of Dams project](https://www.newenglandsustainabilityconsortium.org/dams). 
Support for the Future of Dams project is provided by the National Science Foundation's Research Infrastructure Improvement NSF #IIA-1539071, USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.   

+ [Data Discovery Center](https://ddc.unh.edu) of the University of New Hampshire


Colors
--------------------------
color pallete used for tabs
(in order: red, yellow, green, blue, dark grey)
#F0C0AB
#F0D8A8
#A8C090
#789090
#787878
