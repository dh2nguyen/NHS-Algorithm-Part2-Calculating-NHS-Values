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
