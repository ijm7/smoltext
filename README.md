# smoltext  
> smoltext is a simple text editor written in Erlang. It can run as a standalone executable or on a running BEAM instance. smoltext is cross-platform, allowing execution on all operating systems which support BEAM and the Erlang implementation of the wxWidgets GUI library.

This software is only known to work using OTP 22.
  
## Build  
Run the following commands to clone the repository and build/run the program:    
```  
git clone https://github.com/ijm7/smoltext.git
cd smoltext  
make
./smoltext.out  
```  
To clean the build:  
```  
make clean  
```  
