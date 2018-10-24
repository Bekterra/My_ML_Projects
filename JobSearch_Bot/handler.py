#!/usr/bin/env python
# -*- coding: utf-8 -*-


from telegram import (ReplyKeyboardMarkup, ReplyKeyboardRemove)
from telegram.ext import (Updater, CommandHandler, RegexHandler,
                          ConversationHandler)

# load the library
import logging
import requests
#import bs4
from bs4 import BeautifulSoup
#import urllib.request
import pandas as pd

# Enable logging
logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                    level=logging.INFO)

logger = logging.getLogger(__name__)

job, city, state, URL_B, URL_Building,Next_Result = range(6)
input_quote = False

sign = "+"


def facts_to_str(user_data):
    facts = list()
 
    for key, value in user_data.items():
        facts.append('%s - %s' % (key, value))
 
    return "\n".join(facts).join(['\n', '\n'])


def start(bot, update):
    reply_keyboard = [['Data Scientist', 'Data Analyst', 'Data Engineer']]

    update.message.reply_text(
        'Hi! My name is Job Web scrapper Bot. I will help to find a ML job for you. '
        'Send /cancel to stop working with me.\n\n'
        'What position are you looking for?',
        reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True))
    
    return job


def position(bot, update):
    global input_job
    reply_keyboard = [['San Francisco', 'New York', 'Bay Area']]
    
    user = update.message.from_user
    logger.info("Position name %s: %s  ", user.first_name, update.message.text)
    update.message.reply_text('I see! Please choose prefered city or region, '
                              'so I know where to look for a job, or send /skip if you don\'t want to.',
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True))
    input_job = update.message.text

    print("Input Job: ",input_job)    
     
    return city

def skip_position(bot, update):
    user = update.message.from_user
    logger.info("User %s did not choose a position.", user.first_name)
    update.message.reply_text('I bet you are sleeping! Pick a position, please, '
                              'or send /skip.')
    
    return start


def city(bot, update):
    global input_city
    reply_keyboard = [['CA', 'NY']]
    
    user = update.message.from_user
    logger.info("City name for %s: %s", user.first_name, update.message.text)
    update.message.reply_text('Looks good! Please choose prefered state, '
                              'so I know where to look for a job, or send /skip if you don\'t want to.',
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True))
    input_city = update.message.text

    print("Input City: ",input_city)   
                
    return state

def skip_city(bot, update):
    user = update.message.from_user
    logger.info("User %s did not choose a city.", user.first_name)
    update.message.reply_text('I bet you are sleeping! Pick a city, please, '
                              'or send /skip.')
    return city

def state(bot, update):
    global input_state
    reply_keyboard = [['http://www.indeed.com', 'http://www.dice.com']]
    
    user = update.message.from_user
    logger.info("Link name for %s: %s", user.first_name, update.message.text)
    update.message.reply_text('Yahoo! Please choose prefered job website, '
                              'so I know where to look for a job, or send /skip if you don\'t want to.',
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True,resize_keyboard=True))
    
    input_state = update.message.text

    print("Input State: ",input_state)   

    return URL_B

def skip_state(bot, update):
    user = update.message.from_user
    logger.info("User %s did not choose a state.", user.first_name)
    update.message.reply_text('I bet you are lazy! Pick a State, please, '
                              'or send /skip.')
    return state


def website(bot, update):
    global BASE_URL
    reply_keyboard = [['Yes','No']]

    user = update.message.from_user
    logger.info("Link name for %s: %s", user.first_name, update.message.text)
    update.message.reply_text('You wanna see a full link ?'
                              ', or send /skip if you don\'t want to.',
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True,resize_keyboard=True))
    
    BASE_URL = update.message.text

    print("URL: ",BASE_URL)   
    
    return URL_Building

def transform(input,sign, quote = False):
    syntax = str(input).replace(" ", sign)
    if quote == True:
        syntax = ''.join(['"', syntax, '"'])
    return(syntax)

def parse_indeed(url):
        #user = update.message.from_user
        #url = update.message.text
    html = requests.get(url)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
    df = pd.DataFrame(columns=["Title","Location","Company","Salary", "Job_Summary","Job Link"])
    for each in soup.find_all(class_= "result" ):
            try: 
                title = each.find(class_='jobtitle').text.replace('\n', '')
            except:
                title = 'None'
            try:
                location = each.find('span', {'class':"location" }).text.replace('\n', '')
            except:
                location = 'None'
            try: 
                company = each.find(class_='company').text.replace('\n', '')
            except:
                company = 'None'
            try:
                salary = each.find('span', {'class':'no-wrap'}).text
            except:
                salary = 'None'
            try: 
                job_link = "%s%s" %  (BASE_URL,each.find('a').get('href')) #"%s%s" % (home_url,elem.find('a').get('href'))
            except:
                job_link = 'None'    
            job_summary = each.find('span', {'class':'summary'}).text.replace('\n', '')
            df = df.append({'Title':title, 'Location':location, 'Company':company, 'Salary':salary, 'Job_Summary':job_summary,'Job Link':job_link}, ignore_index=True)
            print(type(df))
            print("Final msg :",df)
        
    return df 

def parse_dice(url):
        rawcode = requests.get(url)
        soup = BeautifulSoup(rawcode.text, "lxml")
        df = pd.DataFrame(columns=["Title","Location","Company","Job_Summary","Job Link"])
        # pick out all the "div" with "class="job-row"
        divs = soup.findAll("div")
        job_divs = [jp for jp in divs if not jp.get('class') is None
                        and 'complete-serp-result-div' in jp.get('class')]
        # loop for each div chunk
        for job in job_divs:
            try:
                link = "%s%s" %  (BASE_URL,job.find('ul', {'class': 'list-inline'}).find('li').find('h3').find('a').attrs['href'])
                # job title
                title = job.find('ul', {'class': 'list-inline'}).find('li').find(
                                    'h3').find('a').attrs['title']
                # job company
                company = job.find('ul', {'class': 'list-inline details row'}).find(
                        'li', {'class':'employer col-sm-3 col-xs-12 col-md-2 col-lg-3 text-ellipsis'}).find(
                        'span', {'class':'hidden-xs'}).find('a').text.strip()
                # job location
                location = job.find('ul', {'class': 'list-inline details row'}).find(
                        'li', {'class':'location col-sm-3 col-xs-12 col-md-2 col-lg-3 margin-top-3 text-ellipsis'}).find(
                        'span',{'itemprop':'address'}).find('span',{'class':'jobLoc'}).text.strip()
                #job summary
                job_desc = job.find('div', {'class':'shortdesc col-sm-3 col-xs-8 col-md-8 col-lg-8 text-ellipsis hidden-xs hidden-sm'}).find(
                        'span',{'itemprop':'description'}).text.strip()
            except:
                continue

            df = df.append({'Title':title, 'Location':location, 'Company':company, 'Job_Summary':job_desc,"Job Link":link}, ignore_index=True)
        return df

#def get_msg(x):
#    msg = "\n\n\n"
#    for column in x.iteritems():
#        title = column['Title']
#        location = column['Location']
#        job_link = column['Job Link']
#        msg += title+location +'  [<a href="'+job_link+'">source</a>]'
#        msg += "\n\n"
#        
#    return msg




def url_builder(bot, update,user_data):
    reply_keyboard = [['Next Job','No']]
    #user = update.message.from_user
    
    if 'Yes' in update.message.text:

########## Generate the URL ##########
######################################
  
        if not input_city: ## if (input_city is "")
            url_list = [ BASE_URL, '/jobs?q=', transform(input_job, sign, input_quote),'&l=', input_state]
            url = ''.join(str(v) for v in url_list)
        else: # input_city is not ""
            url_list = [ BASE_URL, '/jobs?q=', transform(input_job, sign, input_quote),'&l=', transform(input_city, sign), '%2C+', input_state]
            url = ''.join(str(v) for v in url_list)
    #update.message.text = url
    print(url)
    #return url

#### Parsing built result and getting results
    global parsed_df
    if BASE_URL == 'http://www.indeed.com':
        parsed_df = parse_indeed(url)
    else:
        parsed_df = parse_dice(url)
    
    url_builder.ind=0
    user_data = parsed_df.iloc[url_builder.ind,:]
    
    #msg = get_msg(parsed_df)
    #logger.info("Link name for %s: %s", user.first_name, update.message.text)
    update.message.reply_text('First Job post : '
                              '%s'
                              ' If you wanna see a next job posting Press <Next Job> button ?'
                              ', or send /skip if you don\'t want to.' % facts_to_str(user_data),
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True,resize_keyboard=True))
    
    
    return Next_Result
    

def next_job(bot,update, user_data):
    reply_keyboard = [['Next Job','No']]
   
    #global ind
   # x = list(user_data)
    if 'Next Job' in update.message.text:
        url_builder.ind=url_builder.ind+1
        msg=parsed_df.iloc[url_builder.ind,:]
        update.message.reply_text('Next Job post : '
                              '%s'
                              ' If you wanna see a next job posting Press <Next Job> button ?'
                              ', or send /skip if you don\'t want to.' % facts_to_str(msg),
                              reply_markup=ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True))
        result = Next_Result
        
    elif 'No' in update.message.text:
        msg = "Bye !"
        update.message.reply_text(msg,reply_markup=ReplyKeyboardRemove())
        result = ConversationHandler.END
    
    return result


def skip_website(bot, update):
    user = update.message.from_user
    logger.info("User %s did not choose a website.", user.first_name)
    update.message.reply_text('No no no! You have to choose a job website, '
                              'or send /skip.')
    return website



def cancel(bot, update):
    user = update.message.from_user
    logger.info("User %s canceled the conversation.", user.first_name)
    update.message.reply_text('Bye! I hope we can talk again some day.',
                              reply_markup=ReplyKeyboardRemove())

    return ConversationHandler.END


def error(bot, update, error):
    """Log Errors caused by Updates."""
    logger.warning('Update "%s" caused error "%s"', update, error)




def main():
    # Create the EventHandler and pass it your bot's token.
    #JobTestBot Token
    updater = Updater("YOUR TOKEN")

    # Get the dispatcher to register handlers
    dp = updater.dispatcher

    # Add conversation handler with the states GENDER, PHOTO, LOCATION and BIO
    conv_handler = ConversationHandler(
        entry_points=[CommandHandler('start', start)],

        states={
            job: [RegexHandler('^(Data Scientist|Data Analyst|Data Engineer)$', position),
                    CommandHandler('skip', skip_position)],

            city: [RegexHandler('^(San Francisco|New York|Bay Area)$', city),
                    CommandHandler('skip', skip_city)],

            state: [RegexHandler('^(CA|NY)$', state),
                    CommandHandler('skip', skip_state)],

            URL_B: [RegexHandler('^(http://www.indeed.com|http://www.dice.com)$', website),
                    CommandHandler('skip', skip_website)],
            
            URL_Building: [RegexHandler('^(Yes|No)$', url_builder,pass_user_data=True)],
            
            Next_Result: [RegexHandler('^(Next Job|No)$', next_job,pass_user_data=True)],
            

        },

        fallbacks=[CommandHandler('cancel', cancel)]
    )

    dp.add_handler(conv_handler)

    # log all errors
    dp.add_error_handler(error)

    # Start the Bot
    updater.start_polling()

    # Run the bot until you press Ctrl-C or the process receives SIGINT,
    # SIGTERM or SIGABRT. This should be used most of the time, since
    # start_polling() is non-blocking and will stop the bot gracefully.
    updater.idle()


if __name__ == '__main__':
    main()