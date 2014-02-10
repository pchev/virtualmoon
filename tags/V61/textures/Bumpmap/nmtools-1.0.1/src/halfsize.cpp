//
//  Authors: Fridger Schrempp & Robert Skuridin
//
//  fridger.schrempp@desy.de
//  skuridin1@mail.ru
//
// The program reads textures of even height in signed 16-bit integer raw 
// format from STDIN. It outputs to STDOUT a texture of size reduced by 
// a factor of two in the same 16 bit raw format.

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

int* rebinRowCol(FILE* in, int width)
{
    int* newRowCol = new int[width / 2];
    for (int i = 0; i < width / 2; i++)
        newRowCol[i] = readS16(in) + readS16(in);
    for (int i = 0; i < width / 2; i++)
        newRowCol[i] += readS16(in) + readS16(in);
    return newRowCol;
}

inline int nint( double x)
{
 return (int)(x < 0.0 ? ceil(x - 0.5) : floor(x + 0.5));
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
        cerr << "\nUsage: halfsize <inputwidth> [<byteswap>]\n\n";
        cerr << "Version "<< version << endl;
        cerr << "Assume : inputwidth : inputheight = 2 : 1\n\n";
        cerr << "Default: byte ordering of input file and computer are equal \n";
        cerr << "Else   : specify <byteswap> = 1\n";
        cerr << "-------------------------------------------------------\n";
        cerr << "The program reads textures of even height in signed 16-bit int\n";
        cerr << "raw format from STDIN. It outputs to STDOUT a texture of size\n"; 
        cerr << "reduced by a factor of two in the same 16 bit raw format.\n"; 
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
   

    int** h = new int* [height0 / 2];
    cerr << "\nOld size: " << width0 <<" x "<<height0<<endl;
    cerr << "New size: " << width0 / 2 <<" x "<<height0 / 2<< "\n\n";
    for (int j = 0; j < height0 / 2; j++)
    {
        if (j > 0)
        	delete[] h[j-1];
     	h[j]     =  rebinRowCol(stdin, width0); 
     	for (int i = 0; i < width0 / 2; i++)
     	{
         	short hs = (short) nint(0.25 * h[j][i]);
      		fwrite(&hs, 2, 1, stdout);
     	}
    }
}
