Here is my unfinished project dedicated for Web Scraping telegram bot(just need to deploy it) there is still room for improvement of my bot. It works when you run it though.

AWS Lambda, Zappa, Flask and Python 3


##### Step 1: Set up the Python environment #####

Currently I am a big fan of pipenv to manage project dependencies and the virtual environment for projects.
Open a terminal and execute the following commands. Afterwards we have a nice and clean Python installation along with some essential tools.

brew update
brew install python3
 
# install and upgrade some essential packages
pip install --upgrade pip setuptools pipenv
 
# optionally install some handy development tools
pip install flake8 autopep8 ipython cookiecutter

##### Step 2: Setup the project and create the web application #####

In this step we will setup our new project and create the trivial web application. First, we have to execute some shell commands to create our new project.

# create an empty directory for our project
mkdir lambda-hello-world
cd lambda-hello-world
 
# Init the Pipfile and a virtual environment for the project.
pipenv --three
 
# activate the virtual environment
pipenv shell
 
# install Zappa and Flask along with the AWS command line tools
pipenv install zappa flask
pipenv install --dev awscli

# optionally install some handy development tools inside the virtual environment
pipenv install --dev flake8 autopep8 ipython

Next we create a file named Bot_Server.py inside the directory lambda-hello-world with the following content. 
