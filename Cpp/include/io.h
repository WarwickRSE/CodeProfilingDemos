#include <iostream>
#include <fstream>
#include <thread> // For sleeping

#include "sleep.h"

#ifndef __io_mod
#define __io_mod
namespace io{

  inline void painfully_slow_intro_message(std::string filename, int delay=10){

    char tmp;
    std::ifstream file;
    try{
      file.open(filename);
    } catch (std::ifstream::failure & e){
      std::cerr << "Error opening file " << filename << std::endl;
    }
    file>>std::noskipws; //Set stream to read whitespace as chars
    while(!file.eof()){
      file>>tmp;
      std::cout<<tmp;
      flush(std::cout);
      //Proper yielding sleep at system level - mimics being blocked
      // by network or file access
      std::this_thread::sleep_for(std::chrono::milliseconds(delay));
    }
  }

  inline int get_selection(int argc, char** argv){

    int sel = 1;
    //Looking for"N" from "run=N"
    // Slightly manky string handling here, because this is a small code
    // Nothing _wrong_ with this, but it wont scale up well at all
    for(int i = 0; i < argc; i++){
      std::string arg = std::string(argv[i]); //Simplicity...
      auto pos = arg.find_first_of('=');
      if(pos !=std::string::npos){
        if(arg.substr(0, pos)=="run"){
	  std::string sub = arg.substr(pos+1);
	  sel = std::stoi(sub);
	}
      }
    }
    return sel;
  }

};
#endif
