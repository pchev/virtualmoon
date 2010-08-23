//
//  Authors: Fridger Schrempp & Robert Skuridin
//
//  fridger.schrempp@desy.de
//  skuridin1@mail.ru
//
// The program reads samples with signed 16-bit integers from STDIN
// and outputs to STDOUT a signed 16-bit integer sample 
// of width reduced to the nearest power of 2.

// Specify byteswap = 1 if the byte ordering 
// of input file and computer differ

// The main application refers to raw binary 16bit elevation maps

#include <iostream>
#include <cstdio>
#include <math.h>
#include <string>
#include <vector>

#ifdef _WIN32
	#include <io.h>
	#include <fcntl.h>
#endif 

using namespace std;

const string version = "1.0, June 2006, authors: F. Schrempp and R. Skuridin\n";

int byteSwap = 0;

int IsLittleEndian()
{
   short word = 0x4321;
   if((*(char *)& word) != 0x21 )
       return 0;
   else 
       return 1;
}

short readS16(FILE *in)
{
    short b2;
    
    fread(&b2, 2, 1, in);

    if (byteSwap == 1)
    	b2 = ((b2 & 0xff00) >> 8) | ((b2 & 0x00ff) << 8); 
    
    return (short) b2; 
}

short* readRowS16(FILE* in, int width)
{
    short* row = new short[width];
    for (int i = 0; i < width; i++)
        row[i] = readS16(in);

    return row;
}

inline double min(double a, double b)
{
	return a < b ? a : b;
}

inline double max(double a, double b)
{
	return a > b ? a: b;
}

inline int nint( double x)
{
	return (int)(x < 0.0 ? ceil(x-0.5) : floor(x+0.5));
} 


int main(int argc, char* argv[])
{
    int width0   = 0;
    int height0  = 0;
	vector<string> pcOrder(2);
	
	pcOrder[0] = "Big-endian (MAC...)"; 
	pcOrder[1] = "Little-endian (Intel)";
	    
    if (argc < 2 || argc > 3)   
    {
        cerr << "\nUsage: resc2pow2 <inputwidth> [<byteswap>]\n\n";
        cerr << "Version "<< version << endl;
        cerr << "Assume : inputwidth : inputheight = 2 : 1\n";
        cerr << "Default: byte ordering of input file and computer are equal \n";
        cerr << "Else   : specify <byteswap> = 1\n\n";
        cerr << "-------------------------------------------------------\n";
        cerr << "The program reads samples from STDIN in signed 16-bit integer\n";
        cerr << "raw format. It outputs to STDOUT a signed 16-bit sample of \n"; 
        cerr << "width reduced to the nearest power of 2. The main application \n";
        cerr << "refers to binary 16-bit elevation maps.\n";
        cerr << "-------------------------------------------------------\n\n"; 
        cerr <<"Computer and output to STDOUT have +++ "<<pcOrder[IsLittleEndian()]<<" +++ byte order!\n\n";  
        return 1;    
    }
    else
    {

    	if (sscanf(argv[1], " %d", &width0) != 1)
    	{
    		cerr << "Bad image dimensions.\n";
        	return 1;
    	}    
    	height0 = width0 / 2;
    
    	if (argc == 3)
    	{
    		if (sscanf(argv[2], " %d", &byteSwap) != 1)
    		{
    			cerr << "Bad byteorder specs.\n";
        		return 1;
    		};
    	}
    }		
        
	#ifdef _WIN32
		if (_setmode(_fileno(stdin), _O_BINARY) == -1 )
    		{
    			cerr<<"Binary read mode from STDIN failed\n";
    			return 1;
    		}

		if (_setmode(_fileno(stdout), _O_BINARY) == -1 )
    		{
    			cerr<<"Binary write mode via STDOUT failed\n";
    			return 1;
    		}
   	#endif
      
    // find nearest power-of-two width < width0
    int width = 1;
    while ( width < width0)
    	width <<= 1;
    width >>= 1; 
    int height = width / 2;
    cerr << "\nReducing to nearest power-of-two width: " << width << "\n\n";
    
    short** h = new short* [height0];
    
    double mr = (double) width / (double) width0;
    int j0, j00 = 0;    
    
    h[0] = readRowS16(stdin, width0);
    h[1] = readRowS16(stdin, width0);
        
    for (int j = 0; j < height; j++)
    {
    	if (j % 1024 == 0)
     		cerr << '[' << j << ']';
    	j0 = nint(height0 * (j + 0.5)/(double)height - 0.5);
      	for (int k = j00 + 1; k <= j0; k++)
     	{
	     	if (k > 1)
    	 		delete[] h[k-2];
       	    if(k < height0 - 1)
         		h[k + 1] = readRowS16(stdin, width0);         	
     	}
     	j00 = j0;
     	
     	double y00 = mr * j0;
        double y01 = mr * (j0 + 1);
        double y0  = (double)j;
        double y1  = (double)(j + 1);
        
        for (int i = 0; i < width; i++)
        {
        	int i0 = nint(width0 * (i + 0.5)/(double)width - 0.5);
         	double x00 = mr * i0;
            double x01 = mr * (i0 + 1);
            double x0  = (double)i;
            double x1  = (double)(i + 1);
                
            double s1 = (x00 - min(x00,x0)) * (y00 - min(y0,y00));
            double s2 = (min(x01,x1) - max(x00,x0)) * (y00 - y0);
            double s3 = (max(x1,x01) - x01) * (y00 - min(y0,y00));
         	double s4 = (x00 - x0) * (min(y1,y01) - max(y00,y0));
         	double s5 = (min(x01,x1) - max(x00,x0)) * (min(y1,y01) - max(y0,y00));
         	double s6 = (x1 - x01) * (min(y1,y01) - max(y0,y00));
         	double s7 = (x00 - min(x00,x0)) * (max(y1,y01) - y01);
         	double s8 = (min(x01,x1) - max(x00,x0)) * (y1 - y01);
         	double s9 = (max(x01,x1) - x01) * (max(y1,y01) - y01);

         	double hh = s5 * h[j0][i0];         	
         	if (s1 > 0) 
          		hh += s1 * h[j0 - 1][i0 - 1];
           	if (s2 > 0) 
          		hh += s2 * h[j0 - 1][i0];
           	if((s3 > 0) && (i0 < width0 -1)) 
          		hh += s3 * h[j0 - 1][i0 + 1];
           	if (s4 > 0) 
          		hh += s4 * h[j0][i0 - 1];
         	if((s6 > 0) &&  (i0 < width0 -1)) 
          		hh += s6 * h[j0][i0 + 1];
         	if((s7 > 0) && (j0 < height0 -1)) 
          		hh += s7 * h[j0 + 1][i0 - 1];
         	if((s8 > 0) && (j0 < height0 -1)) 
          		hh += s8 * h[j0 + 1][i0];
         	if((s9 > 0) && (i0 < width0 -1) && (j0 < height0 -1)) 
          		hh += s9 * h[j0 + 1][i0 + 1];
         
          	short hi = (short)nint(hh);
           	fwrite(&hi, 2, 1, stdout); 	
        }
    }
   	return 0;            
}

