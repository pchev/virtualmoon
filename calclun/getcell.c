/*

   Program getcell

   create and return a spice cell that cannot be create in Pascal language

   TODO: Create array of cell ???
*/

  #include "SpiceUsr.h"

  #define  MAXIVL  1000
  #define  MAXWIN  ( 2 * MAXIVL )

  SPICEDOUBLE_CELL ( doublecell1, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell2, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell3, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell4, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell5, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell6, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell7, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell8, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell9, MAXWIN );
  SPICEDOUBLE_CELL ( doublecell10, MAXWIN );

  SPICEINT_CELL ( intcell1, MAXIVL );
  SPICEINT_CELL ( intcell2, MAXIVL );
  SPICEINT_CELL ( intcell3, MAXIVL );
  SPICEINT_CELL ( intcell4, MAXIVL );
  SPICEINT_CELL ( intcell5, MAXIVL );
  SPICEINT_CELL ( intcell6, MAXIVL );
  SPICEINT_CELL ( intcell7, MAXIVL );
  SPICEINT_CELL ( intcell8, MAXIVL );
  SPICEINT_CELL ( intcell9, MAXIVL );
  SPICEINT_CELL ( intcell10, MAXIVL );
  
  SpiceCell * getdoublecell ( int * i )
  { 
     if (i==1) { 
      return &doublecell1; 
     }   
     else if (i==2) { 
      return &doublecell2; 
     }   
     else if (i==3) {
       return &doublecell3;
      }  
     else if (i==4) {
       return &doublecell4;
      }  
     else if (i==5) {
       return &doublecell5;
     }  
     else if (i==6) {
       return &doublecell6;
     }  
     else if (i==7) {
       return &doublecell7;
     }  
     else if (i==8) {
       return &doublecell8;
     }  
     else if (i==9) {
       return &doublecell9;
     }  
     else if (i==10) {
       return &doublecell10;
     }  
   }
 
  SpiceCell * getintcell ( int * i )
  { 
     if (i==1) { 
      return &intcell1; 
     }   
     else if (i==2) { 
      return &intcell2; 
     }   
     else if (i==3) {
       return &intcell3;
      }  
     else if (i==4) {
       return &intcell4;
      }  
     else if (i==5) {
       return &intcell5;
     }  
     else if (i==6) {
       return &intcell6;
     }  
     else if (i==7) {
       return &intcell7;
     }  
     else if (i==8) {
       return &intcell8;
     }  
     else if (i==9) {
       return &intcell9;
     }  
     else if (i==10) {
       return &intcell10;
     }  
   }

   SpiceInt spice_cell_elem_i( SpiceCell * c, int i)
   {
      return SPICE_CELL_ELEM_I( c, i ); 
   }
