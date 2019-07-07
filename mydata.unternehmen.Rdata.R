# (A) Create the data frame

mydata.company = data.frame(
  companyCategory = c("Mineralölahndel", "Welthandel", "Welthandel",
                      "Welthandel", "Welthandel", "Nahrungsmittel",
                      "Mineralölahndel", "Welthandel", "Chemie",  "Chemie"),
  
  sales1 = c(253.80,207.81,126.81,119.94,106.00,91.61,86.00,64.00,
                    55.72,50.76),
  
  stringsAsFactors = FALSE  
)

# Create companies and give rows name
company = c("Vitol","GLENCORE","Cargill","TRAFIGURA","MERCURIA", 
            "Nestle", "Mineralölhandel","LouisDC","NOVARTIS",
            "INEOS")
row.names(mydata.company) = company

# (B) Add the "salesEuro" column. 
mydata.company$salesEuro = mydata.company$sales1 * 0.9163

# (C) Extract the sales of Norvatis.
print(mydata.company["NOVARTIS", ])

# (D-1) Extract companies that has category of 'Welthandel'
welth_Com = mydata.company[mydata.company$companyCategory=='Welthandel', ]

# (D) Extract companies that has sales more than 100 and put them in
mrd100chf = mydata.company[mydata.company$sales1>100, ]

# (E) Display the relationship between the business unit and revenue, 
# as well as the distribution of the business unit characteristic in a 
# suitable graph, and save the two graphs under grafik1.png or grafik2.png
# in your homework folder.

# -----E-------
# Here above is the google translated version of (e), and I can't understand
# What I have to do.

#(F) Save your finished data frame to mydata.unternehmen.RData in your homework
# folder.




