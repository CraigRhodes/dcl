# DCL
  
This is the home for DCL - a new callback-based programming language.
  
To compile:  
  
	$ cd docker  
	$ make # This puts you inside the docker container  
	$ make # This makes the executable  
	$ ./microc.native < <filename>  
  
	OR  
  	
	Run the following command inside the docker environment:  
	$ compile <filename>  
  
To run:  
	$ lli <compiled file> 
  
To compile and run inside docker container:  
	$ run <filename>
