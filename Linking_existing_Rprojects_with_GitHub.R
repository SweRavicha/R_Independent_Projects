print("STEPS to link a pre-existing R project to GitHub:")

print("Go to GitBash and give the path of the existing project like- cd C:/Users/.......RProj")
print("Type git init")
print("This will initialize the directory as a Git repository and will add the files in the directory to your local repository")
print("Type git add [nameoffileTobeadded]")
print("This will add the specific file u want to Git. If u want all files in that flder to be moved to Git, type git add .(with the dot symbol)")
print("Type git commit -m Initial Commit( After 'm' it should be in double quotes)")
print("Now we have created an R project and linked it to Git version control")
print("Now we have to link it with GitHub. So, go to github.com and Create a new repository with the same name as your project folder")
print("In the above step, do not initialize README file or gitignore or license")
print("After this, you'll see an option to push existing repository form the cmd line. Copy the commands given there")
print("The commands are like : git remote add origin git@github.com:SweRavicha/R_Independent_Projects.git and git push -u origin master ")
print("Paste those commands in GitBash cmd line. It may ask for Username and pwd.")
print("After this, refresh github and you can see ur project there.")
print("In RStudio, right top quadrant, in the Git tab you can see the project and push from there when u make changes next time.")

print ("*****************************************************************************************************************************")

print("STEPS to link somebody else's project with your RStudio, so that you can work on it:")

print("In RStudio, go to File > New > Version Control and select Git as your control system.")
print("Paste the link of the repository that you want to clone and select a location on your computer tostore the files locally.")
print("Now, all the existing files in the repository should be stored on your local and you can now push at it from RStudio.")

