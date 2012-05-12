//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCrossXML<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GLSCrossXML;

interface

uses
  Classes,
  SysUtils,
{$IFNDEF FPC}
  Variants,
  XMLIntf,
  XMLDoc,
  XMLDom;
{$ELSE}
  DOM,
  XMLRead,
  XMLWrite;
{$ENDIF}

{$IFNDEF FPC}
type
  GLSXMLDocument = IXMLDocument;
  GLSXMLNode = IXMLNode;
  GLSDOMNode = IDOMNode;
{$ENDIF}

{$IFDEF FPC}
type
  GLSXMLDocument = TXMLDocument;
  GLSXMLNode = TDOMNode;
  GLSDOMNode = TDOMNode;
{$ENDIF}
function GLSNewXMLDocument: GLSXMLDocument;
procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean; overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode; overload;
procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string); overload;
{$IFNDEF FPC}
procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string); overload;
{$ENDIF}
function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;

implementation

{$IFNDEF FPC}

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := NewXMLDocument();
end;

procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
begin
  ADoc := nil;
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  ADoc.SaveToStream(AStream);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  ADoc.LoadFromStream(AStream);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
begin
  ADoc.SaveToFile(AFileName);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
begin
  ADoc.LoadFromFile(AFileName);
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  attr: OleVariant;
begin
  attr := 0;
  attr := XMLNode.Attributes[AttrName];
  Result := not VarIsNull(attr);
  if Result then
    Value := attr;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
begin
  XMLNode.Attributes[AttrName] := Value;
end;

procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string);
var
  E: IDOMElement;
begin
  E := DOMNode as IDOMElement;
  E.SetAttribute(AttrName, Value);
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
begin
  ChildNode := ParentNode.ChildNodes.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
begin
  DOMNode.AppendChild(DOMNode.ownerDocument.createTextNode(AText));
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;
begin
  AText := XMLNode.Text;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
begin
  Result := XMLNode.AttributeNodes.Count;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
begin
  Result := XMLNode.AttributeNodes[Idx];
end;

{$ENDIF}

{$IFDEF FPC}

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := TXMLDocument.Create;
end;

procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
begin
  FreeAndNil(ADoc);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  XMLWrite.WriteXMLFile(ADoc, AStream);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  XMLRead.ReadXMLFile(ADoc, AStream);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string);
begin
  XMLWrite.WriteXMLFile(ADoc, AFileName);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string);
begin
  XMLRead.ReadXMLFile(ADoc, AFileName);
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Value := E[AttrName];
  Result := Length(Value) > 0;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  E[AttrName] := Value;
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
var
  E: TDOMElement;
begin
  E := ParentNode as TDomElement;
  ChildNode := E.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
begin
  DOMNode.AppendChild(DOMNode.ownerDocument.createTextNode(AText));
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  AText := E.TextContent;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes.Length;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes[Idx];
end;

{$ENDIF}

end.
