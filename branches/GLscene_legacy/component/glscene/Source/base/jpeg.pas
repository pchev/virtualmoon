//
// this unit is part of the glscene project, http://glscene.org
//
{: jpeg wrapper just for delphi compatibility.<p>
      $log: jpeg.pas,v $
      revision 1.1  2006/01/09 21:01:42  z0m3ie
      *** empty log message ***

      Revision 1.5  2005/12/04 16:52:59  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.4  2005/12/01 21:24:10  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

}
unit jpeg;

interface

uses
  // The lazjpeg unit got merged into graphics unit during laz-0.9.25 development
  // If you have trouble here, comment the following line:
//  lazjpeg;
  // and uncomment the next one:
  graphics;

  // you might also need to remove "jpegforlazarus" from "Required packages" section
  // of glscenelazarus package.
  // This will be cleaned up as soon as lazarus-0.9.26 is released.

implementation

end.

