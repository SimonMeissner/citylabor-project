# citylabor-project

This repository contains the project of the cityLabor seminar course at WWU Münster WiSe 21/22.

## Project Information

In this project, a web application was developed for urban gardeners. It aims to help them in two ways: based on the user's location and garden size, the app helps to 1) determine what plants are suitable to plant, and 2) determine at what time of the year a plant should be planted. The recommendations for __"What to plant"__ are based on a _Survival Analysis_ function with an _ROC_ of about __74%__ and a _Köppen-Geiger Climate Map_. The recommendations for __"When to plant"__ are solely based on filtering our extensive [dataset](https://github.com/SimonMeissner/citylabor-project/blob/main/src/20211214-plants-scraped.csv), which was webscraped from a [north-american gardenplanner website](https://gardenplanner.almanac.com/plants/us-and-canada/) featuring 259 popular plants for gardening. The application covers a major extent of the European area. [Feel free to visit here](https://simonmeissner.shinyapps.io/urbangardentool/)

## Motivation

New people who are trying to get into the urban gardening world often don't have a background in this field and lack the knowledge of how to grow their desired plants. This is a difficult issue, because growing plants is an activity that requires awareness of many parameters, such as e.g. the available space and climate influence. Our application makes it easy and intuitive for users to make important decisions in terms of what and when to plant. Just by specifying some fields the user gets an overview of what is possible for his location and can choose from our recommendations. Users of our application need no preliminary experience with growing plants and are able to inform themselves in a very quick and easy manner. 

## Contributors

- Huriel Reichel
- Simon Meissner
- Merel Vogel
- Eesha Ahluwalia
- Jonas Raabe

## Local Development

Our app was developed only using the _R_ programming language and the _Shiny_ package to build our web dashboard. 
By following these instructions you can install this repository to edit code locally and test our app in localhost. 

1. `git clone https://github.com/SimonMeissner/citylabor-project.git`
2. Navigate to the cloned directory in your preferred environment, e.g. RStudio
3. `shiny::runApp()`

### Disclaimer

__We are not liable for any of the received recommendations or outcomes that are based on them__

### License

[MIT](https://en.wikipedia.org/wiki/MIT_License)
