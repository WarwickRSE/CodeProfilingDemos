#include <iostream>

#include "sleep.h"
#include "io.h"
#include "numerics.h"

int main(int argc, char** argv){


  double t, t2, t3, scal=1.0;
  int sel, x, x2, x3, y, io_del;

  sel = io::get_selection(argc, argv);
  if(sel < 1 || sel > 4) sel = 1; //Default to 1
  std::cout<<"Running setup number "<<sel<<std::endl;

  switch(sel){

    case 1:
      io_del = 1; // Delay between prints in init
      t = 0.1;    // Sleep (~ seconds) in CALCULATING step (finagle_widgets)
      t2 = 1.0;   // Sleep (~ seconds) in PROCESSING step (reticulate_splines)
      t3 = 1.0;   // Sleep (~ seconds) in FINALIZING step (square_the_circle) (TOTAL for all repeats)
      x  = 50;    // Repeats (internal) in CALCULATING step
      x2 = 1;     // Repeats (internal) in PROCESSING step
      x3 = 1;     // Repeats (internal) in FINALIZING step
      y  = 5;     // Repeats (external) of PROCESSING step
      break;
    case 2:
      io_del = 1;
      t  = 0.01;
      t2 = 0.1;
      t3 = 1.0;
      x  = 200;
      x2 = 10;
      x3 = 20;
      y  = 10;
      break;
    case 3:
      io_del = 0;
      t  = 0.1;
      t2 = 0.01;
      t3 = 5.0;
      x  = 50;
      x2 = 10;
      x3 = 10;
      y  = 100;
      break;
    case 4:
      io_del = 20;
      t  = 0.1;
      t2 = 0.01;
      t3 = 1.0;
      x  = 1;
      x2 = 1;
      x3 = 1;
      y  = 1;
      break;
  };

  // Apply optional overall scaling
  t *= scal;
  t2 *= scal;
  t3 *= scal;
  io_del = ceil(io_del * scal); // Default to min of 1 UNLESS exactly 0

  /*--------------------------------------------------------------------------
  ---- here follows the "real" "working" code -----------------------------
  ---- If you look really closely you might notice its actually all dummy
  ---- code, though!
  */

  io::painfully_slow_intro_message("../banner.txt", io_del);

  // Doing one thing
  numerics::finagle_widgets(t, x);

  //Repeatedly doing another thing
  for(int i=0; i < y; i++){
    std::cout<<"Wibbling data set: "<<i<<std::endl;//Flush stream
    numerics::reticulate_splines(t2, x2);
  }
  
  //Doing a thrid thing
  std::cout<<"Finalising";
  numerics::square_the_circle(t3, x3);
  std::cout<<std::endl;
};
