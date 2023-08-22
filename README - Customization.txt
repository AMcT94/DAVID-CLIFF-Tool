DAVID Customization

1) ui.R
- comment out unecessary states
- comment out drop down for the question "Does anyone in the home have a disability?" AKA "fam_disab" variable
- comment out SSI and SSDI options in benefits drop down menu 
- comment out all of the conditional panels underneith benefits options
- any input that requires "fam_disab" as a conditional (i.e. line 278, "input.numadults>=2 & input.fam_disab==..."), comment out that portion of the condition that requires fam_disab, or any conditional related to disability.


2) server.R
- ctrl+F the following: "Only Alabama should these uncommented and then make sure to remove fam_disab & prev_ssi as comments

3) The logo in home.html should refer to LOGO_AL.jpg
image size: height="212" width="459"

########################################################
Test: Select "All programs" and make sure no references to disability, SSI, or SSDI appear and make sure all program runs


You can update the alabama folder from 3.4 just like all other dashboards. Just make sure steps above are followed.