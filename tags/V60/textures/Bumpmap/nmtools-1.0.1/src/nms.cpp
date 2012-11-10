//
//  Authors:  Chris Laurel, Fridger Schrempp & Robert Skuridin
//
//  claurel@gmail.com
//  fridger.schrempp@desy.de
//  skuridin1@mail.ru
//
// The program reads an elevation map in signed 16-bit raw format 
// from STDIN. It outputs to STDOUT a normalmap (PPM format), 
// corrected for spherical geometry. 


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

const string version = "1.0, June 2006, authors: C. Laurel, F. Schrempp and R. Skuridin\n";

int byteSwap = 0;
int kernelSize = 2;
int halfKernelSize = 1;

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

int main(int argc, char* argv[])
{
    const double pi = 3.1415926535897932385;
    int width  = 0;
    int height = 0;
    double radius        = 0.0;
    double exag 	     = 1.0;
    double heightmapunit = 1.0;
 	vector<string> pcOrder(2);
	
	pcOrder[0] = "Big-endian (MAC...)"; 
	pcOrder[1] = "Little-endian (Intel)";
	           
    if (argc < 3 || argc > 6)
    {
        cerr << "\nUsage: nms  <bodyradius> <width> [<exag> [<byteswap> [ <heightmapunit>]]]\n\n";
        cerr << "Version "<< version << endl;
        cerr << "-------------------------------------------------------\n";
        cerr << "The program reads an elevation map in signed 16-bit integer \n"; 
        cerr << "raw format from STDIN. It outputs to STDOUT a normalmap (PPM),\n"; 
        cerr << "corrected for spherical geometry\n\n";
        cerr << "Units    : bodyradius[km], width[pixel]\n";
        cerr << "Assume   : aspect ratio = width : height = 2 : 1\n\n";
        cerr << "Defaults : exag = 1.0 (exaggeration factor, recommended: exag ~ 2.5)\n";
        cerr << "           byteswap = 0 <=> byte ordering of input file and computer are equal\n";
        
        cerr << "           heightmapunit = 1.0 (length of one heightmap unit in meters)\n\n";
        cerr << "-------------------------------------------------------\n\n"; 
        cerr << "You computer uses +++ "<<pcOrder[IsLittleEndian()]<<" +++ byte order!\n";
        cerr << "If different from byte order of input file, use byteswap = 1\n\n";  
        cerr << "Reference: bodyradius = 6378.140 (Earth)\n";
        cerr << "                      = 3396.0   (Mars)\n\n";
        return 1;
    }
	else
	{
    	if (sscanf(argv[1], "%lf", &radius)  != 1)
    	{
    		cerr<<"Read error: body_radius\n";
    		return 1;
    	}
    			
    	if (sscanf(argv[2], "%d", &width)  != 1)
    	{
    		cerr << "Bad image dimensions.\n";
        	return 1;
    	}
    	height = width / 2;
    	
    	if (argc > 3)
    	{    	
    		if (sscanf(argv[3], "%lf", &exag)  != 1)    		
    		{
    			cerr<<"Read error: exag\n";
    			return 1;
    		}
    	
    		if (argc > 4)
    		{
    			if (sscanf(argv[4], "%d", &byteSwap) != 1)
    			{
    				cerr << "Bad byteorder specs.\n";
        			return 1;
    			};

	    		if (argc == 6)
    			{    		
    				if (sscanf(argv[5], "%lf", &heightmapunit) != 1)    		    	
    				{
    					cerr<<"Read error: heightmapunit\n";
    					return 1;
    				}
				}
			}
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

	// Binary 8-bit/channel RGB header
    fprintf(stdout, "P6\n");
    fprintf(stdout, "%d %d\n255\n", width, height);

    short** h = new short* [height+1];
    unsigned char* rgb = new unsigned char[width * 3];

    for (int i = 0; i < kernelSize - 1; i++)
        h[i] = readRowS16(stdin, width);
    double bumpheight = heightmapunit * exag * width / (2000 * pi * radius); 
    double hp = pi / (double) height;

    for (int y = 0; y < height; y++)
    {
	if (y % 1024 == 0)
	    cerr << '[' << y << ']';

        // Out with the old . . .
        if (y > halfKernelSize)
            delete[] h[y - halfKernelSize - 1];

        // . . . and in with the new.
        if (y < height - halfKernelSize)
            h[y + halfKernelSize] = readRowS16(stdin, width);
       
        double spherecorr = 1.0 / sin(hp * (y + 0.5));
        for (int x = 0; x < width; x++)
        {
            double dx, dy;

            // use forward differences throughout!
			dx = (double)(h[y][x] - h[y][(x + 1) % width]);
            // the pixel x in the row nearest to South pole uses the
            // pixel across the pole at position width/2 + x for the dy gradient
            if (y == height - 1)
            	dy = (double)(h[y][x] - h[y][((width >> 1) + x) % width]);
            else			
                dy = (double)(h[y][x] - h[y + 1][x]);
                	
	    	dx *= bumpheight * spherecorr;
	    	dy *= bumpheight;
	    	double mag = sqrt(dx * dx + dy * dy + 1.0);
	    	double rmag = 127.0 / mag;

		    rgb[x * 3 + 0] = (unsigned char) (128.5 + dx * rmag);
		    rgb[x * 3 + 1] = (unsigned char) (128.5 + dy * rmag);
	    	rgb[x * 3 + 2] = (unsigned char) (128.5 + rmag);
        }
		fwrite(rgb, width * 3, 1, stdout);
    }

    return 0;
}


