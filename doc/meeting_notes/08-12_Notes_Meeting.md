# Meeting 08-12

## Agenda
1. Set agenda
2. Update user stories
3. Review To-Do List
4. Data acquisition
5. DB structure
6. Model
7. Model implementation
8. Next sprint: presentation 
9. Update To-Do's

## Notes 
1. Set agenda
2. Update user stories
* we updated the user stories together. 
3. Review To-Do List
* On survival analysis: we will use Huriel's code from now on. Huriel and Jonas will have a paired programming session. Would be nice if you use different arguments. 
* Right now, if a plants has a long growing time it has a low survival probability. 
* We need to start setting up the "reverse"-code, the when-to-plant. Maybe Jonas can focus on this. 
* We need a way to validate the analysis. 
* @ Jonas: tasks 1) when-to-plant and 2) run Huriel's code. 
4. Data acquisition
* Huriel sent a link via WhatsApp with a database with many things to plant. It's basically doing what we aim to do. 
* There is this dataset with a 190 rows (it also includes flowers). In this website you can write your address and it will return when-to-plant and when-to-harvest,
but with images. 
* Huriel will try to webscrap this website, so we can use it for different climates. This should solve all of our data problems, including our climate problem.
We can reverse the Köpen-Geiger, and we would have 4 different climate types. 
* @ Simon will look at climate classification thing (if subgroups match the one in Canada/North America). 
* @ Simon+Merel will look up 3+3 addresses in the different climate regions, either in US or Canada. 
* There are 6 possible climate classifications, C with 3 options and D with 3 options. 
5. DB structure
* use the one from the website
6. Model
* @ Everyone: test Huriel's data with different parameters. 
7. Frontend
* Is it best if we already try merging it? -> Would be better to separately finish the model before implementing it in the frontend. 
8. Planning
* Jan 31st is the final presentation!
* We should make a timeline. 
* Backend: **deadline: before X-Mas**
  * scraping data 
  * update survival analysis
  * test model
  * reverse code (when to plant)
* Frontend: 
  * additional option to retrieve location: pinpoint your location on a map. This allows remote access. **deadline: before X-Mas**
  * merging connection with backend. **deadline: 2nd weekend of Jan**
  * @ Merel: send e-mail to Auriol about server capacity RShiny 
* Presentation:
  * stakeholders
  * 'clients' 
  * @ Eesha will start working on final presentation
9. Update To-Do's
