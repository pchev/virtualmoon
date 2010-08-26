//
//  Authors: Fridger Schrempp & Robert Skuridin
//
//  fridger.schrempp@desy.de
//  skuridin1@mail.ru
//
// The program reads an elevation map in signed 16-bit raw format  
// from STDIN. It outputs normalmap tiles (PPM format), corrected for
// spherical geometry and including many optimizations

// It is assumed that the input raw file is of power-of-two size



#include <iostream>
#include <cstdio>
#include <math.h>
#include <sstream>
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

string int2String(const int& number)
{
   ostringstream oss;
   oss << number;
   return oss.str();
}

int main(int argc, char* argv[])
{
	const double pi = 3.1415926535897932385;
    int height = 0;
    int width  = 0;
    int level  = 0;
    double radius         = 0.0;
    double exag 		  = 1.0;
    double heightmapunit  = 1.0;
	vector<string> pcOrder(2);
	
	pcOrder[0] = "Big-endian (MAC...)"; 
	pcOrder[1] = "Little-endian (Intel)";
	        
    if (argc < 4 || argc > 6)
    {
        cerr << "\nUsage: nmstiles  <level> <bodyradius> <width> [<exag> [<byteswap> [<heightmapunit>]]]\n\n";
        cerr << "Version "<< version << endl;
        cerr << "-------------------------------------------------------\n";
        cerr << "The program reads an elevation map in signed 16-bit integer raw\n"; 
        cerr << "format from STDIN. It outputs normalmap tiles (PPM format),\n"; 
        cerr << "for bodies of ~spherical geometry, with various crucial optimizations!\n\n";
        cerr << "Units    : tilesize[pixel] = width/2^(level+1)\n";
        cerr << "           bodyradius[km], width[pixel]\n\n";
        cerr << "Assume   : input file has width : height = 2 : 1, power-of-two size\n\n";
        cerr << "Defaults : exag = 1.0 (exaggeration factor, recommended: exag ~ 2.5)\n";
        cerr << "           byteswap = 0 <=> byte ordering of input file and computer are equal\n";        
        cerr << "           heightmapunit = 1.0 (length of one heightmap unit in meters)\n\n";
        cerr << "-------------------------------------------------------\n\n"; 
        cerr << "You computer uses +++ "<<pcOrder[IsLittleEndian()]<<" +++ byte order!\n";
        cerr << "If different from byte order of input file, use byteswap = 1\n\n";          
        cerr<<"Reference: bodyradius = 6378.140 (Earth)\n";
        cerr<<"                      = 3396.0   (Mars)\n\n";
                                 
        return 1;
    }   
	else
	{
    	if (sscanf(argv[1], " %d", &level)  != 1)
    	{
    		cerr << "Bad level input.\n";
        	return 1;
    	}
    	if (sscanf(argv[2], "%lf", &radius)  != 1)
    	{
    		cerr<<"Read error: body_radius\n";
    		return 1;
    	}
    			
    	if (sscanf(argv[3], "%d", &width)  != 1)
    	{
    		cerr << "Bad image dimensions.\n";
        	return 1;
    	}
    	height = width / 2;
    	
    	if (argc > 4)
    	{    	
    		if (sscanf(argv[4], "%lf", &exag)  != 1)    		
    		{
    			cerr<<"Read error: exag\n";
    			return 1;
    		}
    	
    		if (argc > 5)
    		{
    			if (sscanf(argv[5], "%d", &byteSwap) != 1)
    			{
    				cerr << "Bad byteorder specs.\n";
        			return 1;
    			};

	    		if (argc == 7)
    			{    		
    				if (sscanf(argv[6], "%lf", &heightmapunit) != 1)    		    	
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
	#endif

    double bumpheight = heightmapunit * exag * width / (2000 * pi * radius); 
    double hp = pi / (double) height;
    int    hlevel = 1 << level;
    int    hj = height/hlevel;
	
	
    short** h = new short* [height + 1];
	unsigned char* rgb0 = new unsigned char [4 * 4 * 3];
	
	FILE* fp = NULL;
    
    cerr<<"\nOriginal size    16 bit raw elevation file: "<<width<<" x "<<height<<"\n\n";
    cerr<<"Generating  "<< width * height/(hj * hj) <<" resolution-optimized VT tiles (PPM format)\n";
    
    int reductionFak = (1 << (int)(log(1.0/sin(hp * hj))/log(2.0)));
    reductionFak = level > 0? reductionFak: 1;
    
    cerr<<"of size from "<<hj/reductionFak<<" x "<<hj<<" to "<<hj<<" x "<<hj<<" for level "<<level<<"\n\n";
	
	// partition into hlevel cells of tile height hj (hj * hlevel = height)
	h[0] = readRowS16(stdin, width);
	for (int jl = 0; jl < hlevel; jl++)
	{
		int jtile = jl * hj;
		int jma = hj + 1;
		if (jl == hlevel - 1)
			jma = hj;
        for (int j = 1; j < jma; j++)
        {
            // delete previous cell
        	if (jl > 0)
         		delete[] h[jtile - hj + j - 1];
        	// read in next cell	
        	h[jtile + j] = readRowS16(stdin, width);
        }
        // calculate reduction of horizontal resolution depending on latitude 	
        int k = 1;
        if (level > 0)
        {
        	if (jl < hlevel / 2)
        		k = 1 << (int)(log(1.0/sin(hp * (jtile + hj)))/log(2.0));
            else
        		k = 1 << (int)(log(1.0/sin(hp * jtile))/log(2.0));
     	}
     	
        unsigned char* rgb  = new unsigned char [3 * hj/k * hj];
        
        // partition cells horizontally into 2 * hlevel tiles
        for (int il = 0; il < 2 * hlevel; il++)
        {
        	bool flag  = true;
            int  itile = il * hj;
             
    		string filename = "tx_" + int2String(il) + "_" + int2String(jl) + ".ppm";
    		fprintf(stderr, "%s\t ", filename.c_str() );
            
            fp = fopen(filename.c_str(), "wb");

       		// calculate normalmap for tile (il, jl -1)
        	for (int j = 0; j < hj; j++)
        	{
                int jj = jtile + j;	
                double spherecorr = 1.0 / sin(hp * (jj + 0.5));
                
                for (int i = 0; i < hj/k; i++)
                {
                	double dx = 0.0;
                	double dy = 0.0;
                	for (int ii = itile + i * k; ii < itile + (i + 1)  * k; ii++)
                	{
                        dx += (double)(h[jj][ii] - h[jj][(ii + k) % width]);
                		// the pixel ii in the row nearest to South pole uses the
                		// pixel across the pole at position width/2 +ii for the dy gradient
                		if (jj == height - 1)
                			dy += (double)(h[jj][ii] - h[jj][((width >> 1) + ii) % width]);
                		else			
                			dy += (double)(h[jj][ii] - h[jj + 1][ii]);	
                	} 
                	dx *= bumpheight * spherecorr / (double)(k * k);
                	dy *= bumpheight / (double) k;
                	double mag =  sqrt(dx * dx + dy * dy + 1.0);
	    	    	double rmag = 127.0 / mag;
		        	int ij = 3 * (j * hj / k + i); 
		        	rgb[ij + 0] = (unsigned char) (128.5 + dx * rmag);
		        	rgb[ij + 1] = (unsigned char) (128.5 + dy * rmag);
	    	    	rgb[ij + 2] = (unsigned char) (128.5 + rmag);
                	
                	// check whether normalmap is monochrome! => flag = true
                	flag = ((rgb[ij + 0] == rgb[0]) && (rgb[ij + 1] == rgb[1]) && (rgb[ij + 2] == rgb[2]) && flag);
                }
           	}
           	// PPM output
           	fprintf(fp, "P6\n");
           	if (flag)
        	{
        		// replace monochrome tiles by smallest one (4x4) accepted by DXT format
    			fprintf(fp, "%d %d\n255\n", 4, 4);
        		for (int i = 0; i < 16; i++)
        		{
        			rgb0[i * 3 + 0] = rgb[0];
        			rgb0[i * 3 + 1] = rgb[1];
        			rgb0[i * 3 + 2] = rgb[2];
        		}	
        		fwrite(rgb0, 3 * 4, 4, fp);
	       	}
        	else
        	{
    			fprintf(fp, "%d %d\n255\n", hj / k, hj);
           		fwrite(rgb, 3 * hj / k, hj, fp);
        	}	
        	fclose(fp);            
    	}
  	}
    return 0;
}



