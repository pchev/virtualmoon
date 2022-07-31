unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring
rstranslator='Translation: P. Chevalley / C. Legrand';
rshelp_prefix='UK';
rst_1='File';
rst_2='Quit';
rst_3='Selection';
rst_4='Columns';
rst_5='All';
rst_6='None';
rst_7='Close';
rst_8='Ok';
rst_9='Cancel';
rst_10='Clear';
rst_11='Last';
rst_12='Enter selection and order criteria using SQL syntax.';
rst_13='All (Default)';
rst_14='Database maintenance';
rst_15='Near Side Named Formation';
rst_16='Near Side Indexed Craters';
rst_17='Input File';
rst_18='Far Side Named Formation';
rst_19='Far Side Indexed Craters';
rst_20='Historical Sites';
rsHistorical='Historical';
rst_21='Predefined Selections';
rst_22='SQL Request';
rst_23='Select File';
rst_24='Select Data Field';
rst_25='Load to Database';
rst_26='Field separator:';
rst_27='Text separator:';
rst_28='CSV file to import:';
rst_29='Database Fields Map';
rst_30='You are now ready to load the database';
rst_31='Constant Value';
rst_32='Load Database';
rst_33='Export Selection to CSV';
rst_34='Import from CSV';
rst_35='Delete Selection from Database';
rst_36='Mark Selection on Map';
rst_37='Reload a previously saved setting';
rst_38='Load Setting';
rst_39='Save all the definition for later retrival';
rst_40='Save Setting';
rst_41='Find in';
rst_42='Display on Map';
rst_43='Sort by';
rst_44='Column Value';
rst_45='Between';
rst_46='and';
rst_47='Assign input field value';
rst_48='Select Database';
rst_49='Expert Mode';
rst_50='Default Selection';
rst_51='Pyroclastic deposits';
rst_52='Help';
rst_53='About';
rst_54='Lunar Domes';
rsEdit = 'Edit';

rsm_1='DBN must be a constant value!';
rsm_2='No row selected!';
rsm_3='Selection:';
rsm_4='Row:';
rsm_5='not found in';
rsm_6='No current selection, cannot delete all!';
rsm_7='Are you sure you want to delete all the data corresponding to the current selection from the database?';
rsm_8='Records found';
rsm_9='Key';
rsm_10='Value';
rsm_11='Select a database field first!';
rsm_12='Select an input file field first!';
rsm_13='Uninitialised';
rsm_14='Constant value';
rsm_15='Input file field';
rsm_16='Do some validation:';
rsm_17='is checked but no value is defined!';
rsm_18='All checked field are defined.';
rsm_19='Missing required field';
rsm_20='All requiered field are present.';
rsm_21='Start insert process to database';
rsm_22='Database update successfully terminated!';
rsm_23='You can now close this window.';
rsm_24='Database insert failed!';
rsm_25='Rollback completed.';
rsm_26='Database not modified.';
rsloadcsv01='This program let you to import new objects to the Virtual Moon Atlas database from a CSV file. There is no provision to merge data field or to do any calculation.';
rsloadcsv02='If you need that use first a software like OpenOffice.org Calc or Excel to format your data and save them to a CSV file.';

rsloadcsv03='From this page indicate first the character your file use to separate the different columns ( ; or TAB ) and if a " surround the text. Then open you CSV data file.';
rsloadcsv04='If the format is recognised a new tab appear at the top of the page, click this tab to continue.';
rsloadcsv05='If you already record this file setting you can use the "Load Setting" button to retrieve them.';

rsloadcsv06='The next step is to give a relation between your file columns and the different database fields. Refer to the main help file for explanation about each database field.';

rsloadcsv07='Click one of the value of your file and check the corresponding database field. When ready press the "<-->" button.';
rsloadcsv08='It is also possible to give a constant value to a database field. Enter the value at the bottom of the page and press the "Constant Value" button.';

rsloadcsv09='DBN is an identifier for your file. It range from 100 to 199 and allow to identify your data in the database. It is use from the main program to display or not the label for your objects.';
rsloadcsv10='You can also quickly select all your objects by using DBN=100 in the SQL selection window, and delete the data using the File - Delete menu.';

rsloadcsv11='It is mandatory to give a value for the field: DBN, NAME, LONGIN and LATIN.';
rsloadcsv12='When all the required field are defined the next tab appear at the top of the page.';

rsloadcsv13='Press "Load Database" button to load your data to the database.';
rsloadcsv14='The "Save Setting" button allow you to save the setting for a later retrieval.';

rsUnnamedForma = 'Unnamed Formation';
rsFarSideUnnam = 'Far side Unnamed Formation';
rsThisProgramL = 'This program let you import new objects to the Virtual Moon Atlas database from a CSV file.';
rsTheSimplifie = 'The simplified procedure is to add object with information about the name, position and size of the formation.';
rsIfYouNeedToA = 'If you need to add more data, please click the "Expert mode" button now.';
rsTheFormatOfT = 'The format of the simplified file must be:';
rsIsTheNameYou = 'is the name you can search for later.';
rsIsTheFormati = 'is the formation longitude with positive value to the East and negative to the West.';
rsIsTheFormati2 = 'is the formation latitude from -90 to 90 degree.';
rsIsTheSizeOfT = 'is the size of the formation in kilometer. This is used to filter the label for small formation on wide map.';
rsIsAnyTextInf = 'is any text information.';
rsTheColumnSep = 'The column separator is ; and there is no "" to enclose the text. Be sure to not use one of this character anywhere in the file';
rsClicTheButto = 'Clic the button "Create template" to create an empty file with the column headers.';
rsIncorrectNum = 'Incorrect number of column:';
rsMustBe = 'Must be';
rsErrorReading = 'Error reading file';
rsError = 'Error';
rsNext = 'Next';
rsSimpleMode = 'Simple mode';
rsCreateTempla = 'Create template';
rsAdd = 'Add';

implementation

function GetDefaultLanguage:string;
var buf1,buf2: string;
begin
 GetLanguageIDs(buf1,buf2);
 if buf2<>'' then result:=buf2
    else result:=buf1;
end;

function Translate(lang : string = ''; lang2 : string = ''):string;
var pofile: string;
begin
 if lang='' then lang:=GetDefaultLanguage;
 // translate LCL messages
 TranslateUnitResourceStrings('LCLStrConsts',slash(appdir)+slash('language')+'lclstrconsts.%s.po',lang,lang2);
 // translate messages
 pofile:=format(slash(appdir)+slash('language')+'datlun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'datlun.%s.po',lang,lang2);
end;

end.

