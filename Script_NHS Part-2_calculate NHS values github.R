# Calculating the NHS value. Part 2 of 2. 
# By David H. Nguyen, PhD, www.TSG-Lab.org

### 
# ReadMe - Start

# Note: These instructions are best viewed in a plain text reader.

# This script calculate the numerized historgram score of the NHS algorithm. 
# This script creates a .csv file called "the_s-scores.csv"



                ##### General Instructions #####
                  
# You need to modify THREE things in Step 1.
 # Step 1a. Type in how many bins you want the histograms to have.
 # Step 1b. Type in the length of longest line within your image. 
    # Read Step 1b below for details. Read carefully! 
 # Step 1c. Type in the name of the file that contains your data. 
    # See immediately below for how the data should be formatted.



         #### How Your Data Should Be Formatted ####
# 1. File should be .csv format.
# 2. The things in the rows should be numbers.
# 3. There should be no missing data in the rows.
# 4. Each column containing distances MUST be named "point_X" where X is an integer.
    # For example: point_1, point_2, point_3, etc.                
      # The sample data (called "sample_NHS_data.csv") is formatted like this:              
        # who2who_1	          point_1	        who2who_2	          point_2
        # Grp1-1 to Grp2-1	  151.3439791	    Grp1-2 to Grp2-1	  112.0580207
        # Grp1-1 to Grp2-2	  147.122398	    Grp1-2 to Grp2-2	  117.9533806
        # Grp1-1 to Grp2-3	  137.8740367	    Grp1-2 to Grp2-3	  119.0094534
      # The columns called "who2who_X" are unncessary for calculating the NHS value 
        # of their corresponding "point_X" columns, but I left them in because thats how
        # script for Part 1 of the NHS algorithm outputs the data.         

      # This script does not plot the NHS values for you. I might create a Part 3 script that does this.
        # For now, you will have to combine each s.score in the output data from this script with
        # its corresponding (x,y) coordinates from when you calculated the distances in the 
        # columns called "point_X".
                
### ReadMe - End

########################
########################
########################


# Required packages. I assume that you've already installed them. 
  # These lines just activate the packages.
library(dplyr)
library(tidyr)

##################################
##################################
##################################
# Step 1 - Define how many bins you want and what the maximum distance in the system 

# Step 1a
# Replace "NN" with the number of bins you want in the histogram.
# Recommendation: at least 10, but not too many; varies depending on your data. 
# For the sample data on GitHub, use 10 bins just as a suggestion.
how.many.bins = NN

# Step 1b
# Replace "MM" with the maximum distance in your system/image. 
# For example, if your image is a rectangle, the max distance is the diagonal of rectangle. 
# Make sure that the units of this distance is the same as the units in 
  # your input data: pixels, nanameters, micrometers, millimeters, etc.
# For the sample data in GitHub, max.distance is 455
max.distance = MM

# Step 1c
# Upload your .csv data file. 
# Make sure your data is formatted as instructred in the ReadMe info.
df = read.csv("sample_NHS_data.csv")



##################################
##################################
##################################
# Step 2 - Calculate the lower and upper bounds of each bin.


# This defines the length size of each bin in the histogram
bin.spans = (max.distance)/(how.many.bins)

# this defines the how the bins will be spaced apart based on the value in bin.spans
bin.spacing = seq(0, max.distance, by = bin.spans)

# this turns bin.spacing into a 1-column data frame with a header called bin.size
df_bin.spacing = data.frame(bin.size = c(bin.spacing))

# this counts the number of items in df_bin.spacing
num.items = dim(df_bin.spacing)[1]

# this reduces the count of items in num.items by 1
one.less = num.items - 1

# this reduces the count of items in num.items by 2. 
two.less = num.items - 2

# this selects all items in df_bin.spacing except for the first item, which is 0
remove.1st = df_bin.spacing[2:num.items,1]
# Give remove.1st a new name, upper.bounds, so we can bind it with another column in a few steps later
upper.bounds = remove.1st

for (i in 1:two.less){
  K = upper.bounds[i]
  M = K+1
  upper.bounds[i] = M
}


# this selects all items in remove.1st except for the last item, which is 455
remove.last = df_bin.spacing[1:one.less,1]
lower.bounds = remove.last

# this creates a data frame in which each row defines the upper and 
# lower bounds of each bin

upper_and_lower_bounds = cbind(lower.bounds, upper.bounds)
upper_and_lower_bounds
##################################
##################################
##################################
# Step 3 - Sort the data into bins that have sizes that were defined 
# in the data frame called upper_and_lower_bounds from previous step.


# The input file should have at least one column of distances.
# Each column of distances MUST be named as follows: "point_1", "point_2", "point_3", etc.

collect.by.colName = df %>% select(contains("point_"))
collect.by.colName

num.of.histograms = length(collect.by.colName)
num.of.histograms


#####


filt.func.holder = c()

for (i in 1:num.of.histograms){
  stuff = paste0("fxn.point_",i," =function(tal, lowB, uppB) { paste0('chunk_",i,"_b', tal, ' = df %>% filter(point_",i,">', lowB,') %>% filter(point_",i,"<=', uppB,')' ) }" )
  filt.func.holder = c(filt.func.holder, stuff)
}

filt.func.holder

for (i in filt.func.holder){
  eval(parse(text=i))
} 
  
# These mapply functions apply the function "fxn.point_X" 
# Each line creates list objects called "chunk_X_bY"

# this is a numerical sequence that will be iterated across
tally = seq(1:num.items)

# This for-loop prints the following statements:
 # expand.point_1 = mapply(fxn.point_1, tally, upper_and_lower_bounds[,1], upper_and_lower_bounds[,2])
 # expand.point_2 = mapply(fxn.point_2, tally, upper_and_lower_bounds[,1], upper_and_lower_bounds[,2])
 # expand.point_3 = mapply(fxn.point_3, tally, upper_and_lower_bounds[,1], upper_and_lower_bounds[,2])
 # etc. to "num.of.histograms"

mapply.holder = c()

for (i in 1:num.of.histograms){
  stuff = paste0("expand.point_",i,"= mapply(fxn.point_",i,", tally, upper_and_lower_bounds[,1], upper_and_lower_bounds[,2])")
  mapply.holder = c(mapply.holder, stuff)
}

# This for-loop evaluates each statement in "mapply.holder"
 # It produces a series of things named "expand.point_i" (i = integer)

for (i in mapply.holder){
  eval(parse(text=i))
} 
######
# The above for-loop will result in "warning" messages that say:
  # "In mapply(fxn.point_1, tally, upper_and_lower_bounds[,  ... :
  # longer argument not a multiple of length of shorter"
# You can ignore this b/c the number of points (represented by the object "tally")
  # does not have to be the same as the number of desired bins in the histogram 
  # (represented by the object "upper_and_lower_bounds"). If you typed in 11 for 
  # "how.many.bins" in Step 1, these warning messages would not appear.
######



# This for-loop prints the following statements
 #for (i in expand.point_1){eval(parse(text=i))}
 #for (i in expand.point_2){eval(parse(text=i))}
 #for (i in expand.point_3){eval(parse(text=i))}
 # etc. to 10

bin.filt.holder = c()
for (i in 1:num.of.histograms){
  stuff = paste0( "for (i in expand.point_",i,") { eval(parse(text=i)) } "  )
  bin.filt.holder = c(bin.filt.holder, stuff)
}


# This for-loop evaluates each statement in the list called "bin.filt.holder"
for (i in bin.filt.holder){
  eval(parse(text=i))
} 


### This for-loop prints the following example functions"
# snickers_1 = function(i){paste0("chunk1_b",i,"_r",i, "= chunk_1_b",i,"$point_",i)}
# snickers_2 = function(i){paste0("chunk2_b",i,"_r",i, "= chunk_1_b",i,"$point_",i)}
# snickers_3 = function(i){paste0("chunk3_b",i,"_r",i, "= chunk_1_b",i,"$point_",i)}
# etc. to "num.of.histograms"

column.extraction = c()

for (i in 1:num.of.histograms){
  stuff = paste0("snickers_",i, "= function(i){paste0('kitkat_",i,"_b',i,'_r',i, '= chunk_",i,"_b',i,'$point_',i)} " )
  column.extraction = c(column.extraction, stuff)
} 


# This for-loop creates each of the snicker_X functions created in the previous for-loop
for (i in column.extraction){
  eval(parse(text=i))
} 


# This is a counter that will be iterated across
tickmarks = seq(1:num.of.histograms)

# This for-loop prints the following example phrases:
# new.objects_1 = sapply(tickmarks, snickers_1)
# new.objects_2 = sapply(tickmarks, snickers_2)
# new.objects_3 = sapply(tickmarks, snickers_3)
# new.objects_4 = sapply(tickmarks, snickers_4)
# etc. to "num.of.histograms"


new.objects.holder = c()

for (i in 1:num.items){
  stuff = paste0("new.objects_",i, " = sapply(tickmarks, snickers_",i,")")
  new.objects.holder = c(new.objects.holder, stuff)
} 

new.objects.holder

# This for-loop creates the objects contained in new.objects.holder
for (i in new.objects.holder){
  eval(parse(text=i))
} 


# This for-loop prints the following phrases:
  # for(i in new.objects_1){eval(parse(text=i))}
  # for(i in new.objects_2){eval(parse(text=i))}
  # for(i in new.objects_3){eval(parse(text=i))}
  # for(i in new.objects_4){eval(parse(text=i))}
  # etc. to "num.of.histograms"

final.holder = c()

for (i in 1:num.items){
  stuff = paste0("for(i in new.objects_",i,"){eval(parse(text=i))}")
  final.holder = c(final.holder, stuff)
}

final.holder

# This for-loop evaluates each of the statements in final.holder
 # For each in in final.holder, this loop executes the eval-parse function within the item.
 # The result is that a bunch of "kitkat_N_bM_rM" (N and M are integers) objects are produced.
for (i in final.holder){
  eval(parse(text=i))
}

# Combine each kitkat list of the same group into a master list


# This prints the following statements:
 # "crate_1 = seq(1:how.many.bins)"
 # "crate_2 = seq(1:how.many.bins)"
 # "crate_3 = seq(1:how.many.bins)"
 # etc.

cup1 = c()
for (i in 1:num.items){
  spoon = paste0("crate_",i,"= rep(",i,", times=how.many.bins)")
  cup1 = c(cup1, spoon)
}

# This creates items called "crate_X" (X = integers)
for (i in cup1){
  eval(parse(text=i))
}

crate_1

# This prints the following statements:
 # "crate_1 = data.frame(crate_1)"
 # "crate_2 = data.frame(crate_2)"
 # "crate_3 = data.frame(crate_3)"
 # etc.

dish = c()
for (i in 1:num.items){
  fork = paste0("crate_",i,"= data.frame(crate_",i,")")
  dish = c(dish, fork)
}

# This turns each "crate_X" item into a data frame
for (i in dish){
  eval(parse(text=i))
}

shoebox = seq(1:how.many.bins)
shoebox

# This prints the following statements:
 # "costco_1 = cbind(crate_1, shoebox)"
 # "costco_2 = cbind(crate_2, shoebox)"
 # "costco_3 = cbind(crate_3, shoebox)"

ladle = c()
for (i in 1:num.items){
  knife = paste0("costco_",i,"=cbind(crate_",i,", shoebox)")
  ladle = cbind(ladle, knife)
}

# This creates objects called "costco_X" (X = integers)
for (i in ladle){
  eval(parse(text=i))
}


# This function prints the phrase using two different inputs
amalgam = function(A,B){
  paste0("kitkat_",A,"_b",B,"_r",B)
}

# This prints the following statements:
 # "statement_1 = mapply(amalgam, costco_1[,1], costco_1[,2])"   
 # "statement_2 = mapply(amalgam, costco_2[,1], costco_2[,2])"   
 # "statement_3 = mapply(amalgam, costco_3[,1], costco_3[,2])"  
 # etc.

gimle = c()
for (i in 1:num.items){
  elrond = paste0("statement_",i," = mapply(amalgam, costco_",i,"[,1], costco_",i,"[,2])")
  gimle = c(gimle, elrond)
}



# This creates objects called "statement_X" (X = integer)
for (i in gimle){
  eval(parse(text=i))
}


##

unroll = function(i){
  eval(parse(text=i))
}

# This prints the following statements:
  # "unpacked_1= lapply(statement_1, unroll)"  
  # "unpacked_2= lapply(statement_2, unroll)" 
  # etc.

pot = c()
for (i in 1:num.items){
  stuff = paste0("unpacked_",i,"= lapply(statement_",i,", unroll)")
  pot = c(pot, stuff)
}

pot

# This creates list objects called unpacked_X (X=integer)
for (i in pot){
  eval(parse(text=i))
}

##
# This turns a list of lists into a data frame.
# It prints the following statements:
 # "the.df_1 = lapply(unpacked_1, 'length<-', max(lengths(unpacked_1)))"   
 # "the.df_2 = lapply(unpacked_2, 'length<-', max(lengths(unpacked_2)))" 
 # etc.

peaches = c()
for(i in 1:num.items){
  stuff = paste0("the.df_",i," = lapply(unpacked_",i, ", 'length<-', max(lengths(unpacked_",i,")))")
  peaches = c(peaches, stuff)
}
peaches

# This creates data frames called the.df_X (X = integers)
for (i in peaches){
  eval(parse(text=i))
}

##

# This prints the following statements:
 # "the.df_1 = as.data.frame(the.df_1)"  
 # "the.df_2 = as.data.frame(the.df_2)" 
 # etc.

apples = c()
for (i in 1:num.items){
 stuff = paste0("the.df_",i," = as.data.frame(the.df_",i,")")
 apples = c(apples, stuff)
}

# This turns each list called the.df_X into a data frame called the.df_X (X = integers)
for (i in apples){
  eval(parse(text=i))
}


##

# Now count all non-NA elements in each column of each data frame called the.df_X (X = integers)
  # Each column of a the.df_N will have one number, representing the sum of all items in that column.

# This prints the following statements:
 # "count.fxn_1 = function(A){length(which(!is.na(the.df_1[,A])))}"
 # "count.fxn_2 = function(A){length(which(!is.na(the.df_2[,A])))}"

#tracker = seq(1:num.items)
#tracker


#board = c()
#for (i in 1:length(tracker)){
#  stuff = paste0("count.fxn_",i," = function(A){length(which(!is.na(the.df_",i,"[,A])))}")
#  board = c(board, stuff)
#}
#board


#####################################################

# A general eval(parse(text=i)) function that can be called on.
eval.it = function(i){ eval(parse(text=i)) }

###

# Turn each the.df_N into a data frame
shake = c()
for (i in 1:num.items){
  stuff = paste0("the.df_",i," = as.data.frame(the.df_",i,")")
  shake = c(shake, stuff)
}
shake

for (i in shake){eval.it}

###
# Rename column names of the.df_X

# Name series for renaming - a1, a2, a3, etc.
p1 = rep("a", times = dim(the.df_1)[2]) 
p2 = seq(1:dim(the.df_1)[2])
p3 = paste0(p1, p2);p3


juice = c()
for (i in 1:num.items){
  stuff = paste0("names(the.df_",i,") = c(p3)") 
  juice = c(juice, stuff)
}
juice

# Evaluate items in juice to execute them as commands
for (i in juice){
  eval(parse(text=i))
}



###
# Paste a command that creates a function called f_X for the.df_X

# This prints the following:
# "f_1 = function(B){stuff = paste0('length(which(!is.na(the.df_1$a',B,')))')}"  
# "f_2 = function(B){stuff = paste0('length(which(!is.na(the.df_2$a',B,')))')}"  
# etc.

coffee = c()
for (i in 1:num.items){
  stuff = paste0("f_",i," = function(B){stuff = paste0('length(which(!is.na(the.df_",i,"$a',B,')))')}")
  coffee = c(coffee, stuff)
}
coffee

# Evaluate the items in coffee to instantiate objects called f_X
for (i in coffee){ 
  eval(parse(text=i)) 
}


###

# This is a range of numbers to be iterated over. It represents the number of bins in each histogram.
someSeq = seq(1:how.many.bins); someSeq

# Print commands called l_X for future appliation of each f_X on someSeq
# These l_X ojbects are lists that contain statements that need to be evaluated individually
bacon = c()
for (i in 1:num.items){
  stuff = paste0("l_",i," = lapply(someSeq, f_",i,")")
  bacon = c(bacon, stuff)
}

# Evaluate the items in bacon to instantiate objects called l_X
for (i in bacon){
  eval(parse(text=i))
}

# Evaluate each item in each l_X 
count.each = function(i){
  eval(parse(text=i))
}

###

# Each item in l_X is a statment similar to: length(which(!is.na(the.df_1$a',B,'))),
# so each needs to be evaluated and the numerical result stored.

exe = function(i){
  eval(parse(text=i)) 
}

sugar = c()
for (i in 1:num.items){
  stuff = paste0("c.l_",i," = lapply(l_",i,", exe)")
  sugar = c(sugar, stuff)
}

# Evaluate the items in sugar to instantiate objects called c.l_X
# These c.l_x objects are lists of numbers.
for (i in sugar){ eval(parse(text=i)) }


###
# Each c.l_X object is a list, so turn each into a data frame for easy binding to other data frames

# This prints commands that will turn each c.l_X into a data frame
spice = c()
for (i in 1:num.items){
  stuff = paste0("c.l_",i," = as.data.frame(c.l_",i,")")
  spice = c(spice, stuff)
}

# This evaluates each item in spice to turn each c.l_X into a data frame of the same name
for (i in spice){
  eval(parse(text=i))
}

# Since the objects called c.l_X are horizontal data frames, transpose them.
salt = c()
for (i in 1:num.items){
  stuff = paste0("c.l_",i," = t(c.l_",i,")")
  salt = c(salt, stuff)
}  

for (i in salt){
  eval(parse(text=i))
}




##################################
##################################
##################################
# Step 4 - Define the weight scale by which to weigh the bins of the histograms 
# called plottable_X (X = integer)

# The weight scale depends on how many bins you choose in Step 1.
# That number is automatically imported from what you typed in Step 1a.
wt.4each.bin = (1.0)/(how.many.bins)
wt.increment = seq(wt.4each.bin, 1, by = wt.4each.bin)
wt.scale = sort(wt.increment, decreasing = TRUE)

wt.scale


##################################
##################################
##################################
# Step 5 - Bind each data frame with wt.scale. The result is a bunch of 2-column data frames.

# For each object called c.l_X (from the end of Step 3), 
# bind it with wt.scale (from Step 4) into a matrix.

# This prints the following statements:
 # mordor_1 = cbind(c.l_1, wt.scale)
 # mordor_2 = cbind(c.l_2, wt.scale)
 # etc.
cart = c()
for (i in 1:num.items){
  stuff = paste0("mordor_",i," = cbind(c.l_",i,", wt.scale)")
  cart = c(cart, stuff)
}
cart

# This creates matrices called mordor_X (X=integers)
for (i in cart){
  eval(parse(text=i))
}



##################################
##################################
##################################
# Step 6 - Multiply the counts for each bin with the weight for that bin.
# This means mutliply the wt.scale column by the c.l_X column in each mordor_X object

# This function multiplies it's two inputs
multiply = function (A,B){
  prod = A*B
}

# This prints the following statements:
# "products_1 = mapply(multiply, mordor_1[,1], morder_1[,2])"
# "products_2 = mapply(multiply, mordor_2[,1], morder_2[,2])"
# etc.

melons = c()
for (i in 1:num.items){
  stuff = paste0("products_",i," = mapply(multiply, mordor_",i,"[,1], mordor_",i,"[,2])")
  melons = c(melons, stuff)
}

# This creates vectors called products_X (X=integers)
for (i in melons){
  eval(parse(text=i))
}

products_2

##################################
##################################
##################################
# Step 7 - Sum the items in each vector from Step 6.

# This prints the following statements:
 # "q_1 = sum(products_1)"   
 # "q_2 = sum(products_2)" 
 # etc.

stroller = c()
for (i in 1:num.items){
  stuff = paste0("q_",i," = sum(products_",i,")")
  stroller = c(stroller, stuff)
}


# This creates variables called q_X (X=integers)
for (i in stroller){
  eval(parse(text=i))
}


##################################
##################################
##################################
# Step 8 - - Calculate the maximum saturation (this is the M variable in the NHS equation)

# This prints the following statements:
 # "total.elem_1 = sum(c.l_1)"  
 # "total.elem_2 = sum(c.l_2)"  
 # etc.

chicago = c()
for (i in 1:num.items){
  stuff = paste0("total.elem_",i," = sum(c.l_",i,")")
  chicago = c(chicago, stuff)
}
chicago
# This creates variables called total.elem_X (X=integer)
for (i in chicago){
  eval(parse(text=i))
}

# This prints the following statements:
 # "max.sat_1 = (total.elem_1)*(max(wt.scale))"  
 # "max.sat_2 = (total.elem_2)*(max(wt.scale))"  
 # etc.

bsb = c()
for (i in 1:num.items){
  stuff = paste0("max.sat_",i," = (total.elem_",i,")*(max(wt.scale))")
  bsb = c(bsb, stuff)
}


# This creates variables called max.sat_X (X=integers)
for (i in bsb){
  eval(parse(text=i))
}

##################################
##################################
##################################
# Step 9 - Calculate the S score

# Divide each q_X (from Step 7) by it's corresponding max.sat_X (from Step 8) 

# This prints the following statements:
 # "s.score_1 = (q_1)/(max.sat_1)"
 # "s.score_2 = (q_2)/(max.sat_2)"
 # etc.

nsync = c()
for (i in 1:num.items){
  stuff = paste0("s.score_",i," = (q_",i,")/(max.sat_",i,")")
  nsync = c(nsync, stuff)
}
nsync

# This creates variables called s.score_X (X=integers)
for (i in nsync){
  eval(parse(text=i))
}

s.score_2

##################################
##################################
##################################
# Step 10 - Gather all s.scores into a one-column data frame called "s.score"


# Statement components for: s.score=(s.score_1, s.score_2,...,s.score_11)
part.start = paste("s.score=c(")
part.numb = seq(1:num.items)
part.comma = ","
part.end = paste(")")


ss.1st = rep("s.score_", times = num.items)
ss.2nd = seq(1:num.items)
ss.final = paste0(ss.1st, ss.2nd)
ss.final = t(ss.final)

stitch = cbind(part.start, ss.final, part.end)

x = seq(2, ((num.items-1)*2), by = 2)

for (i in x){
    stitch = append(stitch, part.comma, after = i)
}

stitch = t(stitch)

stitch.df = as.data.frame(stitch)

voila = unite(stitch.df, parseMe, 1:length(stitch), sep = "")


# This creates a vector called s.score
eval(parse(text=(voila$parseMe)))

# This turns s.score into a data frame
s.score = data.frame(s.score)


##################################
##################################
##################################
# Step 11 - Match the s.scores with the (x,y) coordinates of their source

item.names = names(collect.by.colName) # from Step 3: "collect.by.colName"
item.names = as.data.frame(item.names)

final.output = cbind(item.names, s.score)

write.csv(final.output, "the_s-scores.csv", row.names = F)

# This script will result in "warning" messages that say:
 # "In mapply(fxn.point_1, tally, upper_and_lower_bounds[,  ... :
 # longer argument not a multiple of length of shorter"
# You can ignore this b/c the number of points (represented by the object "tally")
 # does not have to be the same as the number of desired bins in the histogram 
 # (represented by the object "upper_and_lower_bounds"). If you typed in 11 for 
 # "how.many.bins" in Step 1, these warning messages would not appear.



