README
Common Git Commands to get Repo set up in RStudios for the TDS RShiny code

#make sure you are in the correct location
cd exisiting_folder
#initialize your github locally. 
git init --initial-branch=master

#Git global Setup
git config --global user.name "name"
git config --global user.email "email"


#Connects to the repo
git remote add gitlab https://user:access_token@gitlab.afdatalab.af.mil/afotec/tds.git
#makes it so you dont have to put in pw every time i think
git config --add http.https://gitlab.afdatalab.af.mil.sslverify false

#adds everything to be deployed
git add .
git commit -m "Initial Commit"

#Push out to the gitlab
git push gitlab HEAD:master

#Pull from the gitlab repo
git pull gitlab HEAD:master --verbose

#Pull a single file down from gitlab
git fetch gitlab
git checkout your_branch_name -- your_file_name
