from selenium import webdriver
from webdriver_manager.firefox import GeckoDriverManager
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup
import requests
import time
import pandas as pd
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)

def scrap(address, climate):

    driver = webdriver.Firefox(executable_path=GeckoDriverManager().install())
    space = list(range(len(links)))
    imgs = list(range(len(links)))
    months = [[0] * 12 for i in range(len(links))]
    wtp = [[0] * 12 for i in range(len(links))]
    wth = [[0] * 12 for i in range(len(links))]
    
    for i in range(len(links)):

        driver.get(links[i])
    
        space[i] = driver.find_elements_by_id("ctl00_main_lblSpacingRows")[0].text

        time.sleep(5)
        
        driver.find_element_by_class_name("planting-times-button").click()

        time.sleep(2)

        if i == 0:
            
            driver.find_element_by_id("planting-times-txtLocation").send_keys(address)
        
            time.sleep(2)
            
            driver.find_element_by_id("planting-times-btnLocate").click()

            time.sleep(2)

            driver.find_element_by_id("planting-times-btnSetLocation").click()

            time.sleep(5)

        imgs[i] = driver.find_element_by_class_name("planting-times-plant-list-row").find_elements_by_class_name("planting-times-month")

        for j in range(12):

            months[i][j] = int(imgs[i][j].find_element_by_tag_name('img').get_attribute('src').replace("https://www.growveg.com/assets/images/plant-list-bars/", "").replace(".jpg", ""))

            if months[i][j] == 0:

                wtp[i][j] = 0
                wth[i][j] = 0

            elif months[i][j] < 100 and months[i][j] >= 1:

                wtp[i][j] = j+1
                wth[i][j] = 0

            elif months[i][j] == 300 or months[i][j] == 200 or months[i][j] == 100:

                wtp[i][j] = 0
                wth[i][j] = j+1
            else:

                wtp[i][j] = j+1
                wth[i][j] = j+1

    imgs = []
    months = []
    driver.close()

    min_g_t = []
    max_g_t = []
    wtp_list = []
    wth_list = []
    for i in range(len(wtp)):

        when_plant = wtp[i]
        when_harvest = wth[i]

        when_plant = list(filter(lambda a: a !=0, when_plant))
        when_harvest = list(filter(lambda a: a !=0, when_harvest))

        try:
            
            min_g_t.append( abs((min(when_plant) - min(when_harvest) ) * 30) )
            max_g_t.append( abs((min(when_plant) - max(when_harvest) ) * 30) )

        except:

            min_g_t.append(0)
            max_g_t.append(0)

        wtp_list.append((str(when_plant)).replace("[", "").replace("]", ""))
        wth_list.append((str(when_harvest)).replace("[", "").replace("]", ""))

    wtp = []
    wth = []
    size = list(range(len(links)))
    for i in range(len(space)):

        try:

            size[i] = int(space[i].split("(")[1][:2]) * int(space[i].split("(")[2][:-2][:2])

        except:

            size[i] = 0

    space = []
    clim = [climate for i in range(len(links))]

    df = pd.DataFrame({'plant_name':names,'links':links, 'required_space':size, 'when_to_plant': wtp_list, 'min_growing_time': min_g_t, 'max_growing_time': max_g_t,
                       'when_to_harvest': wth_list, 'climate':clim})

    return df
# end of function

# main

url = "https://gardenplanner.almanac.com/plants/us-and-canada/"
response = requests.get(url)
soup = BeautifulSoup(response.content, "lxml")

# main cards for iteration
card = soup.find_all('article')

names = []
links = []

for plants in card:

    print(plants)
    name_append = plants.text
    link_append = list(plants.a.attrs.values())
        
    names.append(name_append)
    links.append(link_append)


links = list(map(''.join, zip((['https://gardenplanner.almanac.com'] * len(links)), [item for sublist in links for item in sublist])))

df01 = scrap("Baldwin Street, Jacksonville, USA", "Cfa")
df01.to_csv('plants_01.csv')
df02 = scrap("South May Street, Chicago, USA", "Dfa")
df02.to_csv('plants_02.csv')
df03 = scrap("Lincoln Park Drive, Duluth, USA", "Dfb")
df03.to_csv('plants_03.csv')
df04 = scrap("Simpson Avenue, Churchill, Canada", "Dfc")
##df04.to_csv('plants_04.csv')
df05 = scrap("Rue Principale, Ivuljivik, Canada", "ET")
##df05.to_csv('plants_05.csv')
df06 = scrap("South Austin Street, Amarillo, USA", "BSk")
df06.to_csv('plants_06.csv')
df07 = scrap("Lyon Street, Albany, USA", "Csb")
df07.to_csv('plants_07.csv')
df08 = scrap("East Lindo Ave, Chico, USA", "Csa")
df08.to_csv('plants_08.csv')
df09 = scrap("Nola Street, Fallon, USA", "BWk")
df09.to_csv('plants_09.csv')
##df10 = scrap("Calle Carlos Romo, Tlapacoyan, MEX", "Cfb")
##df10.to_csv('plants_10.csv')

#df_scrap = pd.concat([df01, df02, df03, df04, df05, df06, df07, df08, df09, df10])
#df_scrap.to_csv('plants_scrapped.csv')

print("done")

