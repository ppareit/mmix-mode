% MMIX assembly code
	LOC    Data_Segment
X      OCTA  0  % initialize X with 0
Y      OCTA  #0000FFFF00000000  % initialize Y with 0x0000FFFF00000000
Z      OCTA  #00000000FFFF0000  % initialize Z with 0x00000000FFFF0000
	
	LOC    #1000
Main	OR     $X $Y $Z  % Bitwise OR Y and Z, store the result in X
	TRAP   0 Halt 0  % stop the program
	
