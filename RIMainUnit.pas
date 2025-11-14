unit RIMainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 ComCtrls, SynHighlighterAny, SynEdit, StrUtils, SynEditTypes;

type
 RIByteArray = array of Byte;
 { TRIMainForm }

 TRIMainForm = class(TForm)
  btnConvert: TButton;
  btnSaveAsText: TButton;
  btnLoadFile: TButton;
  btnSaveAsToken: TButton;
  btnRecompile: TButton;
  cbTidyCode: TCheckBox;
  lblFileName: TLabel;
  OutputDisplay: TMemo;
  OpenDialog: TOpenDialog;
  Characters: TPageControl;
  StatusBar: TStatusBar;
  Summary: TTabSheet;
  TopPanel: TPanel;
  SaveDialog: TSaveDialog;
  Image1: TImage;
  Reptol: TSynAnySyn;
  SynEdit1: TSynEdit; //FOR TESTING
  procedure btnConvertClick(Sender: TObject);
  function CreateOutput: TStringList;
  procedure btnRecompileClick(Sender: TObject);
  procedure btnSaveAsTextClick(Sender: TObject);
  procedure btnLoadFileClick(Sender: TObject);
  procedure btnSaveAsTokenClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  function Detokenise(C: Integer; AData: RIByteArray): TStringList;
  procedure FormShow(Sender: TObject);
  function CreateEditBox(Lparent: TObject): TSynEdit;
  function SetUpHighlighter(section: Byte): TStringArray;
  function Tokenise(Input: TStrings): RIByteArray;
  function Decompile(Char: Integer; AData: RIByteArray; Elk: Boolean=False;
                                           TidyCode: Boolean=True): TStringList;
  function Convert(RIData: RIByteArray;toBBC: Boolean): RIByteArray;
  function ValidateCode(Input: TStrings; out Errors: TStringList;
                                                    BBC: Boolean=True): Boolean;
  function SplitString(Input: String): TStringArray;
  function IsValidFloat(Input: String): Boolean;
  function Compile(Input: TStrings;out Errors: TStringList;
                                                BBC: Boolean=True): RIByteArray;
 private
  LoadedData: RIByteArray;
  LoadedFile: String;
  CharTab   : array[0..31] of TTabSheet;
  CharDisp  : array[0..31] of TSynEdit;
  FCharTerm : String;
  const
   //Used with StringReplace
   replace = [rfReplaceAll, rfIgnoreCase];
   //All the Reptol commands in order of token
   FRITokens: array[$80..$AB] of String = (
   'NAME','HITBY','LOOK(','DEFINE','CREATE(','IF','MOVING','ELSE','ENDIF','GOTO',
   'NOT','KILLREPTON','CHANGE(','END','SCORE(','SOUND(','FLIP','EFFECT(','FLASH(',
   'CHANCE(','KEY','One','Two','Four','TYPE','ACTION','HITS','MOVE(','STATE(',
   'LABEL','EVENT(','CONTENTS','Animate','RED','GREEN','YELLOW','BLUE','MAGENTA',
   'CYAN','WHITE','WESTOF','SOUTHOF','EASTOF','NORTHOF'); //Keywords (that are not in system flags or colours), minus '('
   //System flags
   FRISysFlags: array[0..1] of array[0..7] of String=(
   ('invalid0','invalid1','One','Two','Four','invalid5','Repton','Animate'),
   ('Transport','Squash','Cycle','Under','VPush','HPush','Deadly','Solid')); //Constants (except for 'invalid*' 
   //Directions for 'MOVE' command
   FRIMove: array[0..7] of String=('E','S','W','N','F','R','B','L');
   //Directions for 'LOOK' command
   FRILook: array[0..11] of String=('E','S','W','N','F','R','B','L',
                                    'NW','NE','SW','SE');
   //Directions for 'CREATE' command (minus tokens
   FRICreate: array[0..8] of String=('E','S','W','N','curr','NW','NE','SW','SE');
   //Standard colours
   FRIColours: array[0..7] of String=(
                'BLACK','RED','GREEN','YELLOW','BLUE','MAGENTA','CYAN','WHITE');//Objects
   //Directions for 'CREATE' command
   FRICreTok: array[0..12] of array[0..1] of String=(('$42','curr'),
                        ('$41','W'), ('$43','E'), ('$62','S'), ('$22','N'),
                        ('$FF','W'), ('$01','E'), ('$E0','N'), ('$20','S'),
                        ('$63','SE'),('$23','NE'),('$61','SW'),('$21','NW'));
   //Compile/Decompile translations - BBC Version
   FRIBBCCmds: array[0..66] of array[0..1] of String=(
                ('A9xx8533A9yy853420431DB0zz','IF NOT CHANCE(&yy&xx) GOTO %zz'),
                ('A9xx8533A9yy853420431D90zz','IF CHANCE(&yy&xx) GOTO %zz'),
		('A2xxA0yy4C8219','CHANGE(Character#xx,Character#yy)'),
		('A2xxA0yy208219','CHANGE(Character#xx,Character#yy)'),
		('A0yyA9xx4CEB19','CREATE(Character#xx,@yy)'),
		('A0yyA9xx20EB19','CREATE(Character#xx,@yy)'),
		('A0yyA9xx4C541A','CREATE(Character#xx,@yy)'),
		('A0yyA9xx20541A','CREATE(Character#xx,@yy)'),
                ('A0xxA51D4CEB19','CREATE(CONTENTS,@xx)'),
                ('A0xxA51D20EB19','CREATE(CONTENTS,@xx)'),
		('B15849409158','FLIP'),
		('A50E29xxD0yy','IF NOT EVENT(^xx) GOTO %yy'),
		('A50E29xxF0yy','IF EVENT(^xx) GOTO %yy'),
		('B1582940D0xx','IF NOT STATE(0) GOTO %xx'),
		('B1582940F0xx','IF NOT STATE(1) GOTO %xx'),
                ('B15829BF9158','STATE(0)'),
                ('B15809409158','STATE(1)'),
                ('A56429xxD0yy','IF UserFlag!xx GOTO %yy'),
                ('A56429xxF0yy','IF NOT UserFlag!xx GOTO %yy'),
                ('A9xx4C131C','EFFECT(#xx)'),
                ('A9xx20131C','EFFECT(#xx)'),
                ('A9xx4CAE19','MOVE(*xx)'),
                ('A9xx20AE19','MOVE(*xx)'),
		('A9xx4C371B','SCORE(#xx)'),
		('A9xx20371B','SCORE(#xx)'),
		('A9xx4C1D1C','SOUND(#xx)'),
		('A9xx201D1C','SOUND(#xx)'),
                ('20A51890xx','IF NOT EASTOF GOTO %xx'),
                ('20A518B0xx','IF EASTOF GOTO %xx'),
		('20B31890xx','IF NOT NORTHOF GOTO %xx'),
		('20B318B0xx','IF NORTHOF GOTO %xx'),
		('20BD1890xx','IF NOT SOUTHOF GOTO %xx'),
		('20BD18B0xx','IF SOUTHOF GOTO %xx'),
		('209B1890xx','IF NOT WESTOF GOTO %xx'),
		('209B18B0xx','IF WESTOF GOTO %xx'),
                ('20CB18D0xx','IF NOT KEY GOTO %xx'),
                ('20CB18F0xx','IF KEY GOTO %xx'),
		('C9xxD0yy','IF NOT CONTENTS Character#xx GOTO %yy'),
		('C9xxF0yy','IF CONTENTS Character#xx GOTO %yy'),
		('A9xx8569','FLASH(?xx)'),
		('A949D0xx','IF NOT HITBY(Character0) GOTO %xx'),
		('A949F0xx','IF HITBY(Character0) GOTO %xx'),
                ('A549D0xx','IF NOT HITBY(Character0) GOTO %xx'),
                ('A549F0xx','IF HITBY(Character0) GOTO %xx'),
		('B15810xx','IF NOT MOVING GOTO %xx'),
		('B15830xx','IF MOVING GOTO %xx'),
                ('A56410xx','IF NOT UserFlag7 GOTO %xx'),
                ('A56430xx','IF UserFlag7 GOTO %xx'),
                ('20EA17','LOOK(F)'),
                ('20EE17','LOOK(R)'),
                ('20F217','LOOK(B)'),
                ('20F617','LOOK(L)'),
                ('200C18','LOOK(NW)'),
                ('201818','LOOK(NE)'),
                ('202618','LOOK(SW)'),
                ('203418','LOOK(SE)'),
                ('204418','LOOK(W)'),
                ('205018','LOOK(E)'),
                ('205A18','LOOK(S)'),
                ('206418','LOOK(N)'),
		('4Cyyxx','GOTO $xx$yy'),
		('20yyxx','GOTO $xx$yy'),
                ('E610','KILLREPTON'),
                ('A549','IF HITBY'),
                ('A042','UNKNOWN COMMAND'), //LDY #&42
                ('A51D','UNKNOWN COMMAND'), //LDA &1D
                ('60','END'));
   //Compile/Decompile translations - Electron Version
   FRIElkCmds: array[0..66] of array[0..1] of String=(
                ('A9xx8533A9yy853420E01CB0zz','IF NOT CHANCE(&yy&xx) GOTO %zz'),
                ('A9xx8533A9yy853420E01C90zz','IF CHANCE(&yy&xx) GOTO %zz'),
		('A2xxA0yy4C9E19','CHANGE(Character#xx,Character#yy)'),
		('A2xxA0yy209E19','CHANGE(Character#xx,Character#yy)'),
		('A0yyA9xx4C701A','CREATE(Character#xx,@yy)'),
		('A0yyA9xx20701A','CREATE(Character#xx,@yy)'),
		('A0yyA9xx4C071A','CREATE(Character#xx,@yy)'),
		('A0yyA9xx20071A','CREATE(Character#xx,@yy)'),
                ('A0xxA51D4C071A','CREATE(CONTENTS,@xx)'),
                ('A0xxA51D20071A','CREATE(CONTENTS,@xx)'),
		('B15849409158','FLIP'),
		('A50E29xxD0yy','IF NOT EVENT(^xx) GOTO %yy'),
		('A50E29xxF0yy','IF EVENT(^xx) GOTO %yy'),
		('B1582940D0xx','IF NOT STATE(0) GOTO %xx'),
		('B1582940F0xx','IF NOT STATE(1) GOTO %xx'),
                ('B15829BF9158','STATE(0)'),
                ('B15809409158','STATE(1)'),
                ('A56429xxD0yy','IF UserFlag!xx GOTO %yy'),
                ('A56429xxF0yy','IF NOT UserFlag!xx GOTO %yy'),
                ('A9xx4C221C','EFFECT(#xx)'),
                ('A9xx20221C','EFFECT(#xx)'),
                ('A9xx4CCA19','MOVE(*xx)'),
                ('A9xx20CA19','MOVE(*xx)'),
		('A9xx4C511B','SCORE(#xx)'),
		('A9xx20511B','SCORE(#xx)'),
		('A9xx4C2C1C','SOUND(#xx)'),
		('A9xx202C1C','SOUND(#xx)'),
                ('203F1990xx','IF NOT EASTOF GOTO %xx'),
                ('203F19B0xx','IF EASTOF GOTO %xx'),
		('204C1990xx','IF NOT NORTHOF GOTO %xx'),
		('204C19B0xx','IF NORTHOF GOTO %xx'),
		('20551990xx','IF NOT SOUTHOF GOTO %xx'),
		('205519B0xx','IF SOUTHOF GOTO %xx'),
		('20361990xx','IF NOT WESTOF GOTO %xx'),
		('203619B0xx','IF WESTOF GOTO %xx'),
                ('20CB18D0xx','IF NOT KEY GOTO %xx'),
                ('20CB18F0xx','IF KEY GOTO %xx'),
		('C9xxD0yy','IF NOT CONTENTS Character#xx GOTO %yy'),
		('C9xxF0yy','IF CONTENTS Character#xx GOTO %yy'),
		('A9xx8569','FLASH(?xx)'),
		('A949D0xx','IF NOT HITBY(Character0) GOTO %xx'),
		('A949F0xx','IF HITBY(Character0) GOTO %xx'),
                ('A549D0xx','IF NOT HITBY(Character0) GOTO %xx'),
                ('A549F0xx','IF HITBY(Character0) GOTO %xx'),
		('B15810xx','IF NOT MOVING GOTO %xx'),
		('B15830xx','IF MOVING GOTO %xx'),
                ('A56410xx','IF NOT UserFlag7 GOTO %xx'),
                ('A56430xx','IF UserFlag7 GOTO %xx'),
                ('208518','LOOK(F)'),
                ('208918','LOOK(R)'),
                ('208D18','LOOK(B)'),
                ('209118','LOOK(L)'),
                ('20A718','LOOK(NW)'),
                ('20B318','LOOK(NE)'),
                ('20C118','LOOK(SW)'),
                ('20CF18','LOOK(SE)'),
                ('20DF18','LOOK(W)'),
                ('20EB18','LOOK(E)'),
                ('20F518','LOOK(S)'),
                ('20FF18','LOOK(N)'),
		('4Cyyxx','GOTO $xx$yy'),
		('20yyxx','GOTO $xx$yy'),
                ('E610','KILLREPTON'),
                ('A549','IF HITBY'),
                ('A042','UNKNOWN COMMAND'),
                ('A51D','UNKNOWN COMMAND'),
                ('60','END'));
 public

 end;

var
 RIMainForm: TRIMainForm;

implementation

{$R *.lfm}

{ TRIMainForm }

procedure TRIMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of string);
var
 F    : TFileStream;
 fname: String;
 Index: Integer;
 Output: TStringList;
 procedure PopulateDisplay;
 begin
  if Index>=0 then
  begin
   CharDisp[Index].Clear;
   CharDisp[Index].Lines.AddStrings(Output);
   CharTab[Index].Caption:='Character '+IntToStr(Index);
   if Output.Count>1 then
    if LeftStr(Output[0],5)='NAME ' then
     CharTab[Index].Caption:=Copy(Output[0],6);
  end
  else StatusBar.Panels[1].Text:=Output[0];
  Characters.ActivePage:=CharTab[0];
 end;
begin
 //Open and load the file
 F:=TFileStream.Create(FileNames[0],fmOpenRead OR fmShareDenyNone);
 SetLength(LoadedData,F.Size);
 F.ReadBuffer(LoadedData[0],F.Size);
 F.Free;
 //Clear the output container, ready for the output
 //Output:=TStringList.Create;
 OutputDisplay.Clear;
 fname:=ExtractFileName(FileNames[0]);
 LoadedFile:=FileNames[0];
 StatusBar.Panels[0].Text:=ExtractFileName(LoadedFile);
 //Tokenised source file                                  DETOKENISE
 if(LeftStr(fname,2)='T.')      //BBC version
 or(LeftStr(fname,3)='eT.')then //Electron version
  for Index:=-1 to 31 do
  begin
   Output:=Detokenise(Index,LoadedData);
   PopulateDisplay;
  end;
 //Compiled object file                                   DECOMPILE
 if(LeftStr(fname,2)='O.')      //BBC version
 or(LeftStr(fname,3)='eO.')then //Electron version
  for Index:=-1 to 31 do
  begin
   Output:=Decompile(Index,LoadedData,LeftStr(fname,3)='eO.',cbTidyCode.Checked);
   PopulateDisplay;
  end;
 Output.Free;
end;

procedure TRIMainForm.btnConvertClick(Sender: TObject);
var
 SaveData: RIByteArray;
 F       : TFileStream;
begin
 //Convert currently loaded O file to either BBC or Electron
 SetLength(SaveData,0);
 //File is not an object file, so exit
 if(LeftStr(ExtractFileName(LoadedFile),2)<>'O.')
 and(LeftStr(ExtractFileName(LoadedFile),3)<>'eO.')then exit;
 //BBC Object file - convert to Electron
 if LeftStr(ExtractFileName(LoadedFile),2)='O.' then
  SaveData:=Convert(LoadedData,False);
 //Electron Object file - convert to BBC
 if LeftStr(ExtractFileName(LoadedFile),3)='eO.' then
  SaveData:=Convert(LoadedData,True);
 //Save the data
 if Length(SaveData)>0 then
 begin
  SaveDialog.InitialDir:=ExtractFilePath(LoadedFile);
  //Convert the filename from BBC to Electron
  if LeftStr(ExtractFileName(LoadedFile),2)='O.' then
   SaveDialog.FileName:='e'+ExtractFileName(LoadedFile);
  //Convert the filename from Electron to BBC
  if LeftStr(ExtractFileName(LoadedFile),3)='eO.' then
   SaveDialog.FileName:=Copy(ExtractFileName(LoadedFile),2);
  //Show Save dialogue box and save file
  if SaveDialog.Execute then
  begin
   F:=TFileStream.Create(SaveDialog.FileName,fmCreate OR fmShareDenyNone);
   F.WriteBuffer(SaveData[0],Length(SaveData));
   F.Free;
  end;
 end;
end;

function TRIMainForm.CreateOutput: TStringList;
var
 Index   : Integer;
begin
 Result:=TStringList.Create;
 Result.Add(StatusBar.Panels[1].Text);
 for Index:=0 to 31 do
 begin
  Result.Add(FCharTerm);
  Result.AddStrings(CharDisp[Index].Lines);
  if LeftStr(CharDisp[Index].Lines[0],5)='NAME ' then
   CharTab[Index].Caption:=Copy(CharDisp[Index].Lines[0],6);
 end;
end;

procedure TRIMainForm.btnRecompileClick(Sender: TObject);
var
 SaveData: RIByteArray;
 F       : TFileStream;
 E       : TStringList;
 Output  : TStringList;
begin
 Output:=CreateOutput;
 if Output.Count>33 then
 begin
  SaveData:=Compile(Output,E);
  if E.Count=0 then
  begin
   if SaveDialog.Execute then
   begin
    F:=TFileStream.Create(SaveDialog.FileName,fmCreate OR fmShareDenyNone);
    F.WriteBuffer(SaveData[0],Length(SaveData));
    F.Free;
   end;
  end
  else
  begin
   OutputDisplay.Clear;
   OutputDisplay.Lines.AddStrings(E);
   Characters.ActivePage:=Summary;
  end;
  E.Free;
 end;
 Output.Free;
end;

procedure TRIMainForm.btnSaveAsTextClick(Sender: TObject);
var
 Output  : TStringList;
begin
 Output:=CreateOutput;
 if Output.Count>33 then
 begin
  SaveDialog.InitialDir:=ExtractFilePath(LoadedFile);
  SaveDialog.FileName:=ExtractFileName(LoadedFile)+'.txt';
  if SaveDialog.Execute then Output.SaveToFile(SaveDialog.FileName);
 end;
 Output.Free;
end;

procedure TRIMainForm.btnLoadFileClick(Sender: TObject);
var
 Files: array of String;
begin
 if OpenDialog.Execute then
 begin
  SetLength(Files,1);
  Files[0]:=OpenDialog.FileName;
  FormDropFiles(Sender,Files);
 end;
end;

procedure TRIMainForm.btnSaveAsTokenClick(Sender: TObject);
var
 fname   : String;
 SaveData: RIByteArray;
 F       : TFileStream;
 Output  : TStringList;
begin
 Output:=CreateOutput;
 if Output.Count>33 then
 begin 
  SaveDialog.InitialDir:=ExtractFilePath(LoadedFile);
  fname:=ExtractFileName(LoadedFile);
  if LeftStr(fname,2)='O.' then fname:='T'+Copy(fname,2);
  if LeftStr(fname,3)='eO.' then fname:='eT'+Copy(fname,3);
  SaveDialog.FileName:=fname;
  if SaveDialog.Execute then
  begin
   SaveData:=Tokenise(Output);
   F:=TFileStream.Create(SaveDialog.FileName,fmCreate OR fmShareDenyNone);
   F.WriteBuffer(SaveData[0],Length(SaveData));
   F.Free;
  end;
 end;
 Output.Free;
end;

procedure TRIMainForm.FormCreate(Sender: TObject);
var
 Index: Integer;
begin
 for Index:=0 to 31 do
 begin
  CharTab[Index]                :=Characters.AddTabSheet;
  CharTab[Index].Caption        :='Character '+IntToStr(Index);
  CharDisp[Index]               :=CreateEditBox(CharTab[Index]);
 end;
end;

procedure TRIMainForm.FormShow(Sender: TObject);
begin
 Characters.ActivePage          :=Summary;
 FCharTerm                      :=StringOfChar('-',40);
 Reptol.Keywords.Clear;
 Reptol.KeyAttri.Foreground     :=clBlue;
 Reptol.KeyAttri.Style          :=[fsBold];
 Reptol.Keywords.AddStrings( SetUpHighlighter(0));
 Reptol.Objects.Clear;
 Reptol.ObjectAttri.Foreground  :=clMaroon;
 Reptol.ObjectAttri.Style       :=[fsBold];
 Reptol.Objects.AddStrings(  SetUpHighlighter(1));
 Reptol.Constants.Clear;
 Reptol.ConstantAttri.Foreground:=clRed;
 Reptol.ConstantAttri.Style     :=[fsBold];
 Reptol.Constants.AddStrings(SetUpHighlighter(2));
 Reptol.NumberAttri.Foreground  :=clBlue;
 Reptol.SymbolAttri.Foreground  :=clPurple;
end;

{-------------------------------------------------------------------------------
Create an Editor box
-------------------------------------------------------------------------------}
function TRIMainForm.CreateEditBox(Lparent: TObject): TSynEdit;
begin
 Result               :=TSynEdit.Create(Lparent as TComponent);
 Result.Parent        :=Lparent as TWinControl;
 Result.Highlighter   :=Reptol;
 Result.Align         :=alClient;
 Result.ScrollBars    :=ssAutoBoth;
 Result.Font.Name     :='Courier New';
 Result.Font.Quality  :=fqAntialiased;
 Result.Font.Size     :=12;
 Result.Options       :=[eoBracketHighlight,
                                  eoGroupUndo,
                                  eoScrollPastEol,
                                  eoSmartTabs,
                                  eoTabsToSpaces,
                                  eoTrimTrailingSpaces];
 Result.RightEdgeColor:=clNone;
 Result.Color         :=clForm;
 Result.TabWidth      :=1;
 Result.BracketMatchColor.Background:=clYellow;
 Result.BlockIndent   :=1;
end;

{-------------------------------------------------------------------------------
Setup the Reptol Highlighter
-------------------------------------------------------------------------------}
function TRIMainForm.SetUpHighlighter(section: Byte): TStringArray;
 procedure AddString(const S: String);
 begin
  SetLength(Result,Length(Result)+1);
  Result[Length(Result)-1]:=S;
 end;
var
 entry: String='';
 index: Integer=0;
begin
 Result:=nil;
 case section of
  0:                                                       //Keywords
   for index:=Low(FRITokens) to High(FRITokens) do
   begin
    entry:=FRITokens[index];
    if entry.EndsWith('(') then entry:=LeftStr(entry,Length(entry)-1);
    if (not(entry in FRISysFlags[0]))
    and(not(entry in FRISysFlags[1]))
    and(not(entry in FRIColours))then AddString(entry);
   end;
  1: for entry in FRIColours do AddString(entry);          //Objects
  2: for entry in FRISysFlags do
     if LeftStr(entry,7)<>'invalid' then AddString(entry); //Constants
 end;
end;

{-------------------------------------------------------------------------------
Detokeniser - for 'T.' files
-------------------------------------------------------------------------------}
function TRIMainForm.Detokenise(C: Integer; AData: RIByteArray): TStringList;
var
 lC   : Integer=0;
 Index: Cardinal=0;
 line : String='';
begin
 //Create the output container
 Result:=TStringList.Create;
 line  :='';
 //Get the author name
 if C=-1 then
 begin
  Index:=$0;
  while(Index<$10)and(AData[Index]<>$0D)do
  begin
   if(AData[Index]>=32)and(AData[Index]<=126)then line:=line+chr(AData[Index]);
   inc(Index);
  end;
  Result.Add(line);
 end;
 //Specific Character
 if(C>=0)and(C<32)then
 begin
  Index:=$10;
  //Find the start of the character
  lC:=0;
  while(Index<Length(AData))and(lC<>C)do
  begin
   if AData[Index]=$FE then inc(lC);
   inc(Index);
  end;
  //Detokenise the character
  while Index<Length(AData) do
  begin
   //New line
   if AData[Index]=$0D then
   begin
    Result.Add(line);
    line:='';
   end;
   //ASCII character
   if(AData[Index]>=32)and(AData[Index]<=126)then line:=line+chr(AData[Index]);
   //Tokenised command
   if(AData[Index]>=Low(FRITokens))and(AData[Index]<=High(FRITokens))then
    line:=line+FRITokens[AData[Index]];
   //Indentation
   if(AData[Index]>=$C8)and(AData[Index]<$FE)then
    line:=line+StringOfChar(' ',AData[Index]-$C8);
   //End of definition for this character
   if AData[Index]=$FE then Index:=Length(AData);
   //Next byte
   inc(Index);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Tokeniser for text file to 'T' file
-------------------------------------------------------------------------------}
function TRIMainForm.Tokenise(Input: TStrings): RIByteArray;
var
 Index : Integer=0;
 match : Integer=0;
 found : Integer=0;
 ptr   : Integer=0;
 line  : String='';
 procedure AddByte(b: Byte);
 begin
  SetLength(Result,Length(Result)+1);
  Result[Length(Result)-1]:=b;
 end;
begin
 //Setup the output container
 Result:=nil;
 SetLength(Result,0);
 //Read in the author
 line:='';
 if Length(Input[0])>9 then
  for Index:=9 to Length(Input[0]) do line:=line+Input[0][Index];
 line:=LeftStr(line+#$0D+StringOfChar(#00,$10),$10);
 for Index:=1 to Length(line) do AddByte(Ord(line[Index]));
 //Starting after the initial divider, read in each line
 Index:=2;
 while Index<Input.Count do
 begin
  //Empty line? Just add a CR
  if Input[Index]='' then AddByte($0D)
  else
   //Divider? Add the character separator
   if Input[Index]=FCharTerm then AddByte($FE)
   else
   begin
    //Count the number of spaces at the start of the line
    ptr:=1;
    while(Input[Index][ptr]=' ')and(ptr<=Length(Input[Index]))do inc(ptr);
    //And add the token
    if ptr>1 then AddByte($C7+ptr);
    //Read in the rest of the line
    while ptr<=Length(Input[Index]) do
    begin
     //Look through the tokens
     found:=0;
     match:=Low(FRITokens);
     while(found=0)and(match<=High(FRITokens))do
     begin
      //Found one?
      if Copy(Input[Index],ptr,Length(FRITokens[match]))=FRITokens[match] then
       found:=match; //Make a note
      inc(match);
     end;
     //If we have, then add it to the data
     if found>=Low(FRITokens) then
     begin
      AddByte(found);
      //Move along to after the command
      inc(ptr,Length(FRITokens[found])-1);
     end
     else //We haven't, so just add the characters
      if(Ord(Input[Index][ptr])>31)and(Ord(Input[Index][ptr])<127)then
       AddByte(Ord(Input[Index][ptr]));
     //Next character
     inc(ptr);
    end;
    //End of line, so add CR
    AddByte($0D);
   end;
  //Next line
  inc(Index);
 end;
 //Add the extra character definitions (not sure why these are there)
 for Index:=0 to $F do
 begin
  AddByte($0D);
  AddByte($FE);
 end;
end;

{-------------------------------------------------------------------------------
Decompiler for 'O.' files
-------------------------------------------------------------------------------}
function TRIMainForm.Decompile(Char: Integer; AData: RIByteArray; Elk: Boolean;
                                           TidyCode: Boolean=True): TStringList;
var
 B       : Byte=0;
 D       : Byte=0;
 E       : Byte=0;
 base    : Word=0;
 Offset  : Integer=0;
 EndOff  : Integer=0;
 ThisOff : Integer=0;
 Index2  : Integer=0;
 Index   : Integer=0;
 line    : String='';
 cmds    : array[0..133] of array[0..1] of String;
 //Check a stream of bytes against a known stream
 function Check(tocheck,cmd: String): String;
 var
  I  : Integer=0;
  T  : Integer=0;
  ok : Boolean=False;
  S  : String='';
  R  : Cardinal=0;
  P  : Real=0;
 begin
  //We already have found something, so exit
  if line<>'' then exit;
  //Empty result
  Result:='';
  I     :=1;
  ok    :=True;
  while I<Length(tocheck) do
  begin
   //Get each byte from the string
   S:=Copy(tocheck,I,2);
   //Does it match?
   if IntToHex(StrToIntDef('$'+S,0),2)=S then
    if StrToIntDef('$'+S,0)<>AData[Offset+(I-1)div 2] then ok:=False;
   //Does not match, so check to see if it is a wildcard - only check if OK so far
   if(IntToHex(StrToIntDef('$'+S,0),2)<>S)and(ok)then
   begin
    R:=AData[Offset+(I-1)div 2];
    //Straight integer
    if Pos('#'+S,cmd)>0 then
     cmd:=StringReplace(cmd,'#'+S,IntToStr(R),replace);
    //Hex value
    if Pos('$'+S,cmd)>0 then
     cmd:=StringReplace(cmd,'$'+S,'0x'+IntToHex(R,2),replace);
    //Floating point value
    if Pos('&'+S,cmd)>0 then
     cmd:=StringReplace(cmd,'&'+S,'fp'+IntToHex(R,2),replace);
    //Colour
    if Pos('?'+S,cmd)>0 then
     cmd:=StringReplace(cmd,'?'+S,FRIColours[R mod (High(FRIColours)+1)],replace);
    //MOVE direction
    if Pos('*'+S,cmd)>0 then
     cmd:=StringReplace(cmd,'*'+S,FRIMove[R mod (High(FRIMove)+1)],replace);
    //Event - just count the number of bits that are set
    if Pos('^'+S,cmd)>0 then
    begin
     B:=0;
     for T:=0 to 7 do if R=(1<<(T+1))-1 then B:=T;
     cmd:=StringReplace(cmd,'^'+S,IntToStr(B),replace);
    end;
    //Relative offset
    if Pos('%'+S,cmd)>0 then
    begin
     if R<$80 then
      cmd:=StringReplace(cmd,'%'+S,'0x'+IntToHex(Offset+R+1+(I-1)div 2,4),replace)
     else
      cmd:=StringReplace(cmd,'%'+S,'0x'+IntToHex(Offset-($100-R)+(I-1)div 2,4),replace);
    end;
    //Userflag - which bit is set
    if Pos('!'+S,cmd)>0 then
    begin
     B:=0;
     if R>0 then while(B<8)and(R<>1<<B)do inc(B);
     if B<8 then
      cmd:=StringReplace(cmd,'!'+S,IntToStr(B),replace);
    end;
    //CREATE direction
    if Pos('@'+S,cmd)>0 then
    begin
     for T:=0 to High(FRICreTok) do
      if StrToInt(FRICreTok[T,0])=R then
       cmd:=StringReplace(cmd,'@'+S,FRICreTok[T,1],replace);
     //This is an optional parameter, so 'curr' needs to be removed
     if Pos(',curr',cmd)>0 then
      cmd:=StringReplace(cmd,',curr','',replace);
    end;
   end;
   inc(I,2);
  end;
  //All matched
  if ok then
  begin
   //Replace 0xXX0xYY with relative hex address
   if Pos('0x',cmd)>0 then
    if Copy(cmd,Pos('0x',cmd)+4,2)='0x' then
    begin
     R:=StrToIntDef('$'+Copy(cmd,Pos('0x',cmd)+2,2),0)<<8
       +StrToIntDef('$'+Copy(cmd,Pos('0x',cmd)+6,2),0);
     dec(R,base);
     cmd:=StringReplace(cmd,Copy(cmd,Pos('0x',cmd),8),'0x'+IntToHex(R,4),replace);
    end;
   //Replace fpXXfpYY with a floating point number (usually CHANCE percentage)
   if Pos('fp',cmd)>0 then
    if Copy(cmd,Pos('fp',cmd)+4,2)='fp' then
    begin
     R:=StrToIntDef('$'+Copy(cmd,Pos('fp',cmd)+2,2),0)<<8
       +StrToIntDef('$'+Copy(cmd,Pos('fp',cmd)+6,2),0);
     P:=Round((R/32768)*10000)/100;
     cmd:=StringReplace(cmd,Copy(cmd,Pos('fp',cmd),8),FloatToStr(P)+'%',replace);
    end;
   //Put this in the result, along with the offset address
   Result:=IntToHex(Offset,4)+': '+cmd;
   //And move the offset pointer to after this command
   inc(Offset,(Length(tocheck)div 2)-1);
  end;
 end;
 //Add an 'ENDIF'
 procedure AddEndif;
 begin
  line:=RightStr(Result[Index],
                 Length(Result[Index])-(Pos('GOTO',
                                            Result[Index])+6));
  Result[Index]:=StringReplace(Result[Index],' GOTO 0x'+line,'',replace);
  Index2:=Index+1;
  while(Index2<Result.Count)and(LeftStr(Result[Index2],4)<>line)do inc(Index2);
  Result.Insert(Index2,line+': ENDIF');
 end;
//Main function definition
begin
 //Setup the command translations
 for Index:=0 to 66 do
 begin
  cmds[Index*2  ]:=FRIBBCCmds[Index];
  cmds[Index*2+1]:=FRIElkCmds[Index];
 end;
 //Create the output container
 Result:=TStringList.Create;
 line:='';
 //Get the author name
 if Char=-1 then
 begin
  Index:=$0;
  while(Index<$10)and(AData[Index]<>$0D)do
  begin
   if(AData[Index]>=32)and(AData[Index]<=126)then
    line:=line+chr(AData[Index]);
   inc(Index);
  end;
  Result.Add(line);
 end;
 if(Char>=0)and(Char<32)then
 begin
  //Set the base address (minus $F0 for where the code starts)
  base:=$5AC0;//BBC is 5BB0-F0
  if Elk then base:=$4990;//Electron is 4A80-F0
  //Iterate through each of the 32 characters
  //Header
  Result.Add('NAME Character'+IntToStr(Char)); //This is optional
  Result.Add('DEFINE TYPE'); //There could be none, so this is also optional
  //Add the flags
  for E:=0 to 7 do
  begin
   //System flags
   if AData[$90+Char]AND(1<<E)=1<<E then Result.Add(FRISysFlags[0,E]);
   if AData[$B0+Char]AND(1<<E)=1<<E then Result.Add(FRISysFlags[1,E]);
   //User flags
   if AData[$D0+Char]AND(1<<E)=1<<E then Result.Add('UserFlag'+IntToStr(E));
  end;
  //Find the ACTION or HITS offset for this character
  E:=$10; //Should be $10 for 'O' files, $2450 for 'G' files and $1250 for 'eG' files
  Offset:=0;
  while(Offset=0)and(E<>0)do
  begin
   Offset:=AData[E+Char]+AData[E+$20+Char]<<8;
   if Offset<base+$F0 then if E=$10 then E:=$50 else if E=$50 then E:=0;
  end;
  //If the offset is zero, then there is no definition
  if(Offset<>0)and(E<>0)then
  begin
   if E=$10 then Result.Add('DEFINE ACTION') else Result.Add('DEFINE HITS');
   dec(Offset,base);
   //Now need to find the end of the definition
   EndOff:=0;
   D:=Char;
   while(EndOff=0)and(D<$1F)do
   begin
    inc(D);
    EndOff:=AData[$10+D]+AData[$30+D]<<8;
   end;
   if EndOff=0 then EndOff:=Length(AData)-1 else dec(EndOff,base+1);
   //However, the end of the definition could be beyond another char's HITS
   ThisOff:=0;
   D:=0;
   while((ThisOff<Offset)or(ThisOff>EndOff))and(D<$1F)do
   begin
    if Char<>D then ThisOff:=(AData[$50+D]+AData[$70+D]<<8)-base;
    inc(D);
   end;
   if(ThisOff>Offset)and(ThisOff<EndOff)then EndOff:=ThisOff-1;
   //So, we can now decompile the code
   while Offset<=EndOff do
   begin
    //We are adding the 'DEFINE HITS' section
    if(Offset=(AData[$50+Char]+AData[$70+Char]<<8)-base)and(E=$10)then
     Result.Add('DEFINE HITS');
    //Start with an empty line
    line:='';
    Index:=0;
    //Check each command until we reach the end or find a match
    while(Index<=High(cmds))and(line='')do
    begin
     line:=Check(cmds[Index,0],cmds[Index,1]);
     inc(Index);
    end;
    //No match found, so add the original hex code for later inspection
    if line='' then line:=IntToHex(Offset,4)+': '+IntToHex(AData[Offset],2);
    //Add to the output container
    Result.Add(line);
    //And move onto the next byte
    inc(Offset);
   end;
  end;
  //Decompile done, now we need to make a number of passes to tidy it up
  //First is changing 'IF NOT' to 'IF' and 'IF' to 'IF NOT', removing the 'GOTO'
  //and adding the 'ENDIF'
  if TidyCode then
  begin
   Index:=0;
   while Index<Result.Count do
   begin
    if(Copy(Result[Index],7,6)='IF NOT')and(Pos('GOTO',Result[Index])>0)then
    begin
     Result[Index]:=StringReplace(Result[Index],' NOT','',replace);
     AddEndif;
    end
    else
    if(Copy(Result[Index],7,3)='IF ')and(Pos('GOTO',Result[Index])>0)then
    begin
     Result[Index]:=StringReplace(Result[Index],'IF ','IF NOT ',replace);
     AddEndif;
    end;
    //We also need to change 'IF HITBY IF CONTENTS char' to 'IF HITBY char'
    if Index>0 then
    begin
     if(Copy(Result[Index-1],7)='IF HITBY')
     and(Copy(Result[Index],7,11)='IF CONTENTS')then
     begin
      Result[Index]:=StringReplace(Result[Index],'CONTENTS','HITBY',replace);
      Result[Index-1]:='';
     end;
    end;
    inc(Index);
   end;
   //Second pass - change all the GOTOs to point to a label, and add the labels
   Index:=0;
   while Index<Result.Count do
   begin
    if Pos('GOTO',Result[Index])>0 then
    begin
     line:=RightStr(Result[Index],Length(Result[Index])-(Pos('GOTO',Result[Index])+6));
     Result[Index]:=StringReplace(Result[Index],'0x'+line,'Label'+line,replace);
     Index2:=0;
     while(Index2<Result.Count)and(LeftStr(Result[Index2],4)<>line)do inc(Index2);
     if Result[Index2-1]<>'LABEL Label'+line then
      Result.Insert(Index2,'LABEL Label'+line);
    end;
    inc(Index);
   end;
   //Third pass - replace GOTO label ENDIF with ELSE, and add the extra ENDIF at the label
   index:=0;
   while Index<Result.Count do
   begin
    if(Copy(Result[Index],7,4)='GOTO')
    and(Copy(Result[Index+1],7,5)='ENDIF')then
    begin
     line:=Copy(Result[Index],12);
     Result[Index]:='';
     Result[Index+1]:=LeftStr(Result[Index+1],4)+': ELSE';
     Index2:=Index;
     while(Index2<Result.Count)and(Result[Index2]<>line)do inc(Index2);
     if Index2<Result.Count then
      if Result[Index2]=line then Result.Insert(Index2,'ENDIF');
    end;
    inc(Index);
   end;
   //Fourth pass - remove the offset address at the start
   Index:=0;
   while Index<Result.Count do
   begin
    if Length(Result[Index])>4 then
     if Result[Index][5]=':' then Result[Index]:=Copy(Result[Index],7);
    inc(Index);
   end;
   //Fifth pass - remove any labels that are not referenced
   Index:=0;
   while Index<Result.Count do
   begin
    if LeftStr(Result[Index],5)='LABEL' then
    begin
     line:=Copy(Result[Index],7);
     Result[Index]:='';
     Index2:=0;
     while(Index2<Result.Count)and(Pos(line,Result[Index2])=0)do
      inc(Index2);
     if Index2<>Result.Count then Result[Index]:='LABEL '+line;
    end;
    //We'll also remove the 'UNKNOWN COMMAND'
    if Result[Index]='UNKNOWN COMMAND' then Result[Index]:='';
    inc(Index);
   end;
   //Sixth pass - delete empty lines
   Index:=0;
   while Index<Result.Count do
   begin
    if Result[Index]='' then
    begin
     Result.Delete(Index);
     if Index>0 then dec(Index);
    end;
    inc(Index);
   end;
   //Seventh pass - ensure there are enough matching IF's and ENDIF's
   Index:=0;
   Index2:=0;
   while Index<Result.Count do
   begin
    if Result[Index]='DEFINE TYPE' then Index2:=0;
    if LeftStr(Result[Index],3)='IF ' then inc(Index2);
    if(LeftStr(Result[Index],5)='ENDIF')and(Index2>0)then dec(Index2)
    else
     if(LeftStr(Result[Index],5)='ENDIF')and(Index2=0)then
     begin
      Result.Delete(Index);
      dec(Index);
     end;
    if(LeftStr(Result[Index],6)='DEFINE')
    or(LeftStr(Result[Index],5)='LABEL')
    or(Index=Result.Count-1)then
    begin
     while Index2>0 do
     begin
      Result.Insert(Index,'ENDIF');
      inc(Index);
      dec(Index2);
     end;
    end;
    inc(Index);
   end;
   //Does the definition finish with an 'END'?
   if Result[Result.Count-1]='END' then Result.Delete(Result.Count-1);
   //Eighth pass - indent it all and add some blank lines
   Index:=0;
   Index2:=2;
   while Index<Result.Count do
   begin
    if(LeftStr(Result[Index],6)='DEFINE')
    or(LeftStr(Result[Index],4)='NAME')
    or(LeftStr(Result[Index],6)='Author')
    or(LeftStr(Result[Index],5)='LABEL')then
     Index2:=2
    else
    begin
     if Pos('ENDIF',Result[Index])>0 then dec(Index2,2);
     if Index2>0 then
      if Pos('ELSE',Result[Index])>0 then
       Result[Index]:=StringOfChar(' ',Index2-2)+Result[Index]
      else
       Result[Index]:=StringOfChar(' ',Index2)+Result[Index];
     if Pos('IF ',Result[Index])>0 then inc(Index2,2);
    end;
    if(LeftStr(Result[Index],6)='DEFINE')
    or(LeftStr(Result[Index],5)='LABEL')then
    begin
     Result.Insert(Index,'');
     inc(Index);
    end;
    inc(Index);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Convertor - converts object files from BBC to Electron or vice versa
-------------------------------------------------------------------------------}
function TRIMainForm.Convert(RIData: RIByteArray;toBBC: Boolean): RIByteArray;
var
 C    : Integer=0;
 D    : Integer=0;
 L    : Integer=0;
 F    : Integer=0;
 Index: Integer=0;
 found: Boolean=False;
 ok   : Boolean=False;
const
 cmds: array[0..57] of array[0..1] of String=( //(BBC,Electron)
                ('A9##8533A9##853420431DB0##','A9##8533A9##853420E01CB0##'),
                ('A9##8533A9##853420431D90##','A9##8533A9##853420E01C90##'),
		('A2##A0##208219','A2##A0##209E19'),
		('A2##A0##4C8219','A2##A0##209E19'),
		('A0##A9##20EB19','A0##A9##20071A'),
		('A0##A9##20541A','A0##A9##20701A'),
		('A0##A9##4CEB19','A0##A9##4C071A'),
		('A0##A9##4C541A','A0##A9##4C701A'),
                ('A0##A51D20EB19','A0##A51D20071A'),
                ('A0##A51D4CEB19','A0##A51D4C071A'),
                ('A9##20131C','A9##20221C'),
                ('A9##4C131C','A9##4C221C'),
                ('A9##20AE19','A9##20CA19'),
                ('A9##4CAE19','A9##4CCA19'),
		('A9##20371B','A9##20511B'),
		('A9##4C371B','A9##4C511B'),
		('A9##201D1C','A9##202C1C'),
		('A9##4C1D1C','A9##4C2C1C'),
                ('20A51890##','203F1990##'),
                ('20A518B0##','203F19B0##'),
		('20B31890##','204C1990##'),
		('20B318B0##','204C19B0##'),
		('20BD1890##','20551990##'),
		('20BD18B0##','205519B0##'),
		('209B1890##','20361990##'),
		('209B18B0##','20361990##'),
                ('20EA17','208518'),
                ('20EE17','208918'),
                ('20F217','208D18'),
                ('20F617','209118'),
                ('200C18','20A718'),
                ('201818','20B318'),
                ('202618','20C118'),
                ('203418','20CF18'),
                ('204418','20DF18'),
                ('205018','20EB18'),
                ('205A18','20F518'),
                ('206418','20FF18'),
 //This next block are to avoid getting changed when JMP or JSR are looked for
		('B15849409158','B15849409158'),
		('A50E29##D0##','A50E29##D0##'),
		('A50E29##F0##','A50E29##F0##'),
		('B1582940D0##','B1582940D0##'),
		('B1582940F0##','B1582940F0##'),
                ('B15829BF9158','B15829BF9158'),
                ('B15809409158','B15809409158'),
                ('A56429##D0##','A56429##D0##'),
                ('A56429##F0##','A56429##F0##'),
		('C9##D0##','C9##D0##'),
		('C9##F0##','C9##F0##'),
		('A9##8569','A9##8569'),
		('A949D0##','A949D0##'),
		('A949F0##','A949F0##'),
                ('A549D0##','A549D0##'),
                ('A549F0##','A549F0##'),
		('B15810##','B15810##'),
		('B15830##','B15830##'),
                ('A56410##','A56410##'),
                ('A56430##','A56430##'));
begin
 Result:=nil;
 //Copy the original data across
 SetLength(Result,Length(RIData));
 for Index:=0 to Length(RIData)-1 do Result[Index]:=RIData[Index];
 //Which way are we converting?
 if toBBC then
 begin
  L:=1;
  F:=0;
 end
 else
 begin
  L:=0;
  F:=1;
 end;
 //Start
 Index:=$F0;
 while Index<Length(RIData) do
 begin
  C:=0;
  found:=False;
  //We will first try and match known commands that can be directly converted
  while(C<High(cmds))and(not found)do
  begin
   D:=1;
   ok:=True;
   for D:=0 to (Length(cmds[C,L])div 2)-1 do
    if Copy(cmds[C,L],1+D*2,2)<>'##' then
     if StrToIntDef('$'+Copy(cmds[C,L],1+(D*2),2),0)<>RIData[Index+D]then
      ok:=False;
   if ok then found:=True else inc(C);
  end;
  //Found a command, so copy the equivalent across
  if found then
  begin
   for D:=0 to (Length(cmds[C,F])div 2)-1 do
    if Copy(cmds[C,F],(D*2)+1,2)<>'##'then
     Result[Index+D]:=StrToIntDef('$'+Copy(cmds[C,F],(D*2)+1,2),0);
   inc(Index,(Length(cmds[C,L])div 2)-1);
  end
  else //Not found, but is it a JMP or JSR?
   if(RIData[Index]=$4C)or(RIData[Index]=$20)then
   begin
    D:=RIData[Index+1]+RIData[Index+2]<<8;
    if toBBC then inc(D,$1130) else dec(D,$1130); //Adjust by $1130 bytes
    Result[Index+1]:=D AND $FF;
    Result[Index+2]:=D>>8;
    inc(Index,2);
   end;
  //Next byte
  inc(Index);
 end;
 //Adjust the pointers by $1130 (minus for BBC->Elk and plus for Elk->BBC)
 D:=$10;
 while D<$90 do
 begin
  for Index:=0 to $1F do
  begin
   C:=RIData[D+Index]+RIData[D+$20+Index]<<8;
   if(toBBC)and(C>=$4A80)then
   begin
    inc(C,$1130);
    Result[D+Index]:=C AND $FF;
    Result[D+$20+Index]:=C>>8;
   end;
   if(not toBBC)and(C>=$5BB0)then
   begin
    dec(C,$1130);
    Result[D+Index]:=C AND $FF;
    Result[D+$20+Index]:=C>>8;
   end;
  end;
  inc(D,$40);
 end;
end;

{-------------------------------------------------------------------------------
Validate the source code
-------------------------------------------------------------------------------}
function TRIMainForm.ValidateCode(Input: TStrings; out Errors: TStringList;
                                                    BBC: Boolean=True): Boolean;
 procedure ReportError(error: String; char,line: Integer);
 begin
  if char>=0 then error:=error+' in character '+IntToStr(char);
  if line>=0 then error:=error+' line '+IntToStr(line);
  Errors.Add(error);
 end;
 function ValidateName(name: String): Boolean;
 var
  index: Integer;
 const
  invalidname: array of String = (
   'NAME','HITBY','LOOK','DEFINE','CREATE','IF','MOVING','ELSE','ENDIF','GOTO',
   'NOT','KILLREPTON','CHANGE','END','SCORE','SOUND','FLIP','EFFECT','FLASH',
   'CHANCE','KEY','One','Two','Four','TYPE','ACTION','HITS','MOVE','STATE',
   'LABEL','EVENT','CONTENTS','Animate','RED','GREEN','YELLOW','BLUE','MAGENTA',
   'CYAN','WHITE','WESTOF','SOUTHOF','EASTOF','NORTHOF','Transport','Squash',
   'Cycle','Under','VPush','HPush','Deadly','Solid');
 begin
  if name.IsEmpty then Result:=False
  else
  begin
   Result:=True;
   //Can't start with number
   if(name[1]>='0')and(name[1]<='9')then Result:=False;
   //Need to contain letters, numbers and/or underscore
   for index:=1 to Length(name) do
    if((name[index]>='0')and(name[index]<='9'))
    or((name[index]>='A')and(name[index]<='Z'))
    or((name[index]>='a')and(name[index]<='z'))
    or( name[index] ='_')then else Result:=False;
   //Can't be a known command or system flag
   if name in invalidname then Result:=False;
  end;
 end;
var
 Output  : TStrings;
 i       : Integer=0;
 j       : Integer=0;
 lineno  : Integer=0;
 ifcount : Integer=0;
 char    : Integer=0;
 section : Integer=-1;
 noflags : Integer=0;
 line    : String='';
 linecmds: array of String=();
 charname: array of String=();
 flags   : array of String=();
 labels  : array of array of String=();
const
 //Commands allowed in the 'DEFINE ACTIONS' section
 actions : array of String=('CHANCE(','CHANGE(','CONTENTS','CREATE(','EASTOF',
                            'EFFECT(','ELSE','END','ENDIF','EVENT(','FLASH(',
                            'FLIP','GOTO','IF','KEY','KILLREPTON','LABEL',
                            'LOOK(','MOVE(','NORTHOF','NOT','SCORE(','SOUND(',
                            'SOUTHOF','STATE(','WESTOF');
 //Commands allowed in the 'DEFINE HITS' section
 hits    : array of String=('CHANGE(','CREATE(','EFFECT(','ELSE','END','ENDIF',
                            'FLASH(','GOTO','HITBY','IF','KILLREPTON','LABEL',
                            'NOT','SCORE(','SOUND(');
 //Valid colour numbers
 colnums : array of String=('0','1','2','3','4','5','6','7');
begin
 Result:=False;
 if Input.Count=0 then
 begin
  ReportError('No code',-1,-1);
  exit;
 end;
 //Keep a track of any errors
 Errors:=TStringList.Create;
 Output:=TStringList.Create;
 //De-indent and remove all blank lines
 for i:=0 to Input.Count-1 do
 begin
  line:=Trim(Input[i]);          //De-indent
  if line<>'' then Output.Add(line);
 end;
 //Is there actually anything to validate/compile?
 if Output.Count=0 then
 begin
  ReportError('No code',-1,-1);
  exit;
 end;
 //Build the flags array. Start with the system flags
 for line in FRISysFlags do
  if LeftStr(line,7)<>'invalid' then
  begin
   SetLength(flags,Length(flags)+1);
   flags[Length(flags)-1]:=line;
  end;
 //Validation starts here
 ifcount:=0; //Number of IFs (reduces when ENDIF is encountered)
 char   :=-1;//Character number
 lineno :=0; //Line number within a character
 //We'll do this on a few passes - first is to get the character names
 for i:=0 to Output.Count-1 do
 begin
  linecmds:=SplitString(Output[i]); //***************** need to check we have enough elements
  //First line - NAME
  if(lineno=0)and(linecmds[0]='NAME')then
   if Length(linecmds)>1 then
    if linecmds[1] in charname then
     ReportError('Name already used',char,-1)
    else
     if ValidateName(linecmds[1])then
     begin
      SetLength(charname,Length(charname)+1);
      charname[Length(charname)-1]:=linecmds[1];
     end
     else ReportError('Bad name',char,-1)
   else ReportError('Bad name',char,-1);
  inc(lineno);
  //End of character definition
  if Output[i]=FCharTerm then
  begin
   lineno:=0;
   inc(char);
  end;
 end;
 //Next pass is to get the user flags
 char   :=-1;
 noflags:=Length(flags);
 lineno :=0;
 for i:=0 to Output.Count-1 do
 begin
  linecmds:=SplitString(Output[i]);
  if Output[i]=FCharTerm then
  begin
   inc(char);
   section:=-1;
  end
  else
  begin
   if linecmds[0]='DEFINE' then //Make sure they're in the correct section
    if Length(linecmds)>1 then
     if linecmds[1]='TYPE' then section:=0 else section:=-1
    else ReportError('Bad definition',-1,-1);
   if(linecmds[0]<>'DEFINE')and(section=0)and(Length(linecmds)=1)then
   begin
    if(not(linecmds[0]in flags))and(Length(flags)<noflags+8)
    and(ValidateName(linecmds[0]))then
    begin
     SetLength(flags,Length(flags)+1);
     flags[Length(flags)-1]:=linecmds[0];
    end;
    if (not(linecmds[0]in flags))and(Length(flags)<noflags+8)
    and(not ValidateName(linecmds[0]))then
     ReportError('Bad flag name',char,lineno);
    if linecmds[0]='Animate'then //Can't animate all sprites
     if(char<5)or(char>17)then
      ReportError('Can''t animate this sprite',char,lineno);
    if(not(linecmds[0]in flags))and(Length(flags)>=noflags+8)then
     ReportError('Too many flags',-1,-1);
   end;
   if(linecmds[0]<>'DEFINE')and(section=0)and(Length(linecmds)>1)then
    ReportError('Mistake',-1,-1);
  end;
 end;
 //Third pass is to get the labels
 char  :=-1;
 lineno:=0;
 SetLength(labels,32);
 for i:=0 to Output.Count-1 do
 begin
  if Output[i]=FCharTerm then
  begin
   inc(char);
   lineno:=0;
  end
  else
  if char>=0 then
   begin
    linecmds:=SplitString(Output[i]);
    if linecmds[0]='LABEL' then
     if Length(linecmds)>1 then
      if linecmds[1] in labels[char] then
       ReportError('Label already used',char,lineno)
      else
       if ValidateName(linecmds[1]) then
       begin
        SetLength(labels[char],Length(labels[char])+1);
        labels[char,Length(labels[char])-1]:=linecmds[1];
       end
       else
        ReportError('Bad label',char,lineno)
     else
      ReportError('Syntax Error',char,lineno);
    inc(lineno);
   end;
 end;
 //Reset the variables for the final pass
 char  :=-1;
 lineno:=0;
 for i:=0 to Output.Count-1 do
 begin
  if(char>=0)and(Output[i]<>FCharTerm)then
  begin
   linecmds:=SplitString(Output[i]);
   //NAME has appeared somewhere other than at the start
   if(linecmds[0]='NAME')and(lineno<>0)then ReportError('Mistake',char,lineno);
   //Get the section, and validate the section type
   if linecmds[0]='DEFINE' then
   begin
    section:=-1;
    if Length(linecmds)>1 then
    begin
     if linecmds[1]='TYPE'  then section:=0;
     if linecmds[1]='ACTION'then section:=1;
     if linecmds[1]='HITS'  then section:=2;
    end;
    if(section=-1)OR(Length(linecmds)>2)then
     ReportError('Bad definition',char,lineno);
   end;
   if linecmds[0]<>'DEFINE' then
    //Check the command is in the correct section
    if((section=0)AND((linecmds[0]in actions)or(linecmds[0]in hits)))
    or((section=1)AND(not(linecmds[0]in actions))AND(linecmds[0]in hits))
    or((section=2)AND(not(linecmds[0]in hits))AND(linecmds[0]in actions))then
     ReportError('Wrong section',char,lineno);
   //Check it is a valid command
   if (linecmds[0]<>'DEFINE')
   and(linecmds[0]<>'NAME')
   and(not(linecmds[0]in actions))
   and(not(linecmds[0]in hits))
   and(section<>0)then ReportError('Syntax Error',char,lineno);
   //ACTION or HITS section
   if section>0 then
   begin
    //Count brackets
    if Output[i].CountChar('(')<Output[i].CountChar(')') then
     ReportError('Missing (',char,lineno);
    if Output[i].CountChar(')')<Output[i].CountChar('(') then
     ReportError('Missing )',char,lineno);
    //Check commands
    case linecmds[0] of
    'IF':
     begin
      inc(ifcount);
      //Too many IFs
      if ifcount>8 then ReportError('Too many IFs',char,lineno);
      //Compensate for a NOT
      if Length(linecmds)>1 then
       if linecmds[1]='NOT' then j:=1 else j:=0;
      //Check conditional commands
      if Length(linecmds)>j+1 then
      begin
       case linecmds[j+1] of
        'CHANCE(':
         //Check it has a '%'
         if Length(linecmds)>j+2 then
          if not linecmds[j+2].EndsWith('%') then
           ReportError('Missing %',char,lineno)
          else //And is a valid float between 0.01 and 100 inclusive
          begin
           if not IsValidFloat(linecmds[j+2].TrimRight('%'))then
            ReportError('Bad %',char,lineno);
          end
         else
          ReportError('Syntax Error',char,lineno);
        'CONTENTS',
        'HITBY':
         if Length(linecmds)>j+2 then
         begin
          if not(linecmds[j+2] in charname) then
           ReportError('No such sprite',char,lineno);
         end
         else
          ReportError('Syntax Error',char,lineno);
        'EVENT(':
         if Length(linecmds)>j+2 then
         begin
          if(StrToIntDef(linecmds[j+2],0)<1)or(StrToIntDef(linecmds[j+2],0)>7)then
           ReportError('Bad EVENT',char,lineno);
         end
         else
          ReportError('Syntax Error',char,lineno);
        'STATE(':
         if Length(linecmds)>j+2 then
         begin
          if(linecmds[j+2]<>'0')and(linecmds[j+2]<>'1')then
           ReportError('Bad STATE',char,lineno);
         end
         else
          ReportError('Syntax Error',char,lineno);
        'EASTOF',
        'KEY',
        'MOVING',
        'NORTHOF',
        'SOUTHOF',
        'WESTOF':if Length(linecmds)>j+2 then ReportError('Mistake',char,lineno);
        otherwise //flags
         if not(linecmds[j+1] in flags)then ReportError('No such flag',char,lineno);
       end;
       //Check not too many entries
       case linecmds[j+1] of
        'CHANCE(',
        'CONTENTS',
        'HITBY',
        'EVENT(',
        'STATE(':if Length(linecmds)>j+3 then ReportError('Mistake',char,lineno);
       end;
      end
      else ReportError('Syntax Error',char,lineno);
     end;
    'CHANGE(':
     if Length(linecmds)<3 then
      ReportError('Missing ,',char,lineno)
     else
      if(linecmds[1] in labels[char])or(linecmds[2] in labels[char])then
       ReportError('Type mismatch',char,lineno)
      else
       if(not(linecmds[1] in charname))or(not(linecmds[2] in charname))then
        ReportError('No such sprite',char,lineno);
    'CREATE(': //char,dir (optional)
     if Length(linecmds)>1 then
      begin
       if linecmds[1] in labels[char] then
        ReportError('Type mismatch',char,lineno)
       else
        if not(linecmds[1] in charname) then
         ReportError('No such sprite',char,lineno);
       if Length(linecmds)=3 then
        if not(linecmds[2] in FRICreate)then
         ReportError('Bad direction',char,lineno);
      end
      else
       ReportError('Syntax Error',char,lineno);
    'FLASH(':
     if Length(linecmds)>1 then
     begin
      if(not(linecmds[1] in ColNums))and(not(linecmds[1]in FRIColours))then
       ReportError('Bad Colour',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'GOTO':
     if Length(linecmds)>1 then
     begin
      if(not(linecmds[1] in labels[char]))and(not(linecmds[1] in flags))then
       ReportError('No such label',char,lineno);
      if(not(linecmds[1] in labels[char]))and(linecmds[1] in flags)then
       ReportError('Type mismatch',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'LOOK(':
     if Length(linecmds)>1 then
     begin
      if not(linecmds[1] in FRILook)then
       ReportError('Bad direction',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'MOVE(':
     if Length(linecmds)>1 then
     begin
      if not(linecmds[1] in FRIMove)then
       ReportError('Bad MOVE direction',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'EFFECT(',
    'SCORE(',
    'SOUND(':
     if Length(linecmds)>1 then
     begin
      if IntToStr(StrToIntDef(linecmds[1],256))<>linecmds[1]then
       ReportError('Bad numeric parameter',char,lineno)
      else
       if(StrToInt(linecmds[1])>255)or(StrToInt(linecmds[1])<0)then
        ReportError('Number too big',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'STATE(':
     if Length(linecmds)>1 then
     begin
      if(linecmds[1]<>'0')and(linecmds[1]<>'1')then
       ReportError('Bad STATE',char,lineno);
     end
     else
      ReportError('Syntax Error',char,lineno);
    'ENDIF':
     begin
      dec(ifcount);
      if Length(linecmds)>1 then ReportError('Mistake',char,lineno);
     end;
    'ELSE',
    'END',
    'FLIP',
    'KILLREPTON':if Length(linecmds)>1 then ReportError('Mistake',char,lineno);
    'LABEL',
    'DEFINE':;//This is dealt with above, but is here to make sure it doesn't throw an error
    otherwise //unknown
     ReportError('Syntax Error '+linecmds[0],char,lineno);
    end;
    case linecmds[0] of
    'CHANGE(',
    'CREATE(':if Length(linecmds)>3 then ReportError('Mistake',char,lineno);
    'EFFECT(',
    'FLASH(',
    'GOTO',
    'LABEL',
    'LOOK(',
    'MOVE(',
    'SCORE(',
    'SOUND(',
    'STATE(':if Length(linecmds)>2 then ReportError('Mistake',char,lineno);
    end;
   end;
   inc(lineno);
  end;
  //End of character definition
  if Output[i]=FCharTerm then
  begin
   if ifcount<>0 then
   begin
    if ifcount<0 then ReportError('No IF',char,-1);
    if ifcount>0 then ReportError('No ENDIF',char,-1);
    ifcount:=0; //Reset the IF/ENDIF counter
   end;
   inc(char);
   section:=-1;//Reset the section
   lineno :=0; //Reset the line number
  end;
 end;
 //Check that we have 32 characters
 if char<31 then ReportError('Not enough characters ('+IntToStr(char+1)+')',-1,-1);
 if char>31 then ReportError('Too many characters ('+IntToStr(char+1)+')',-1,-1);
 //Return a result
 Result:=Errors.Count=0;
end;

{-------------------------------------------------------------------------------
Split a command line at space or (, but keep the (
-------------------------------------------------------------------------------}
function TRIMainForm.SplitString(Input: String): TStringArray;
var
 index: Integer=0;
 cmd  : String='';
begin
 //Split the input about the space and opening bracket
 Result:=Input.Split([' ','(',',']);
 //Has this yealed anything?
 if Length(Result)>0 then
 begin
  //Iterate through each result
  for index:=0 to Length(Result)-1 do
  begin
   //Grab each entry
   cmd:=Result[index];
   //Make sure it's not empty
   if not cmd.IsEmpty then
   begin
    //Add the split character back in (only for '(')
    if Pos(cmd,Input)+Length(cmd)<=Length(Input) then
     if Input[Pos(cmd,Input)+Length(cmd)]='(' then Result[index]:=cmd+'(';
    //Remove any closing brackets
    if(RightStr(cmd,1)=')')and(Length(cmd)>1)then
     Result[index]:=Copy(cmd,1,Length(cmd)-1);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Check to see if the input string is a valid floating point number
-------------------------------------------------------------------------------}
function TRIMainForm.IsValidFloat(Input: String): Boolean;
var
 s: Real=0;
begin
 s:=StrToFloatDef(Input,101);
 Result:=(FormatFloat('0.00',s)<>'101.00')AND(s>0);
end;

{-------------------------------------------------------------------------------
Compile an 'O' file from source
-------------------------------------------------------------------------------}
function TRIMainForm.Compile(Input: TStrings;out Errors: TStringList;
                                                BBC: Boolean=True): RIByteArray;
var
 i        : Integer=0;
 j        : Integer=0;
 c        : Integer=0;
 x        : Integer=0;
 y        : Integer=0;
 z        : Integer=0;
 xx       : Integer=0;
 yy       : Integer=0;
 zz       : Integer=0;
 soc      : Integer=0;
 Tmp0     : String='';
 Tmp1     : String='';
 Tmp2     : String='';
 OLbl     : String='';
 NLbl     : String='';
 Output   : TStringList=nil;
 Chrs,
 Actions,
 Hits     : array[0..31] of String;
 Flags    : array[0..2] of array[0..31] of Byte;
 UserFlags: array[0..7] of String;
 InType   : Boolean=False;
 address  : Word=0;
 define   : Byte=0;
 //Extract a literal integer (#)
 function ExtractInteger(var s: Integer): Integer;
 begin
  Result:=-1;
  if s>0 then
   if Tmp2[s-1]='#' then
   begin
    c:=s;
    while(c<Length(Tmp1))and(Ord(Tmp1[c])>47)and(Ord(Tmp1[c])<58)do inc(c);
    Result:=StrToInt(Copy(Tmp1,s-1,c-(s-1)));
    Tmp2[s-1]:='-';
    s:=-1;
   end;
 end;
 //Get the position of the direction
 procedure GetDirPos(s: Integer);
 begin
  if s>0 then if Tmp2[s-1]='@' then
  begin
   c:=Pos(',',Tmp1)+1;
   j:=Pos(')',Tmp1);
  end;
 end;
begin
 Result:=nil;
 SetLength(Result,0);
 //Before we do an compiling, validate the code
 if ValidateCode(Input,Errors,BBC) then
 begin
  //This is what we will use to work on
  Output:=TStringList.Create;
  Output.AddStrings(Input); //Can't work on 'Input' as this will change what was passed
  //Initialise the containers
  for c:=0 to 31 do
  begin
   Chrs[c]   :='';
   Actions[c]:='';
   Hits[c]   :='';
   //Repton has certain system flags by default
   if c<>1 then Flags[0,c]:=0 else Flags[0,c]:=$C4;
   Flags[1,c]:=0;
   Flags[2,c]:=0;
  end;
  for c:=0 to 7 do UserFlags[c]:='';
  //Remove empty lines
  i:=1;
  while i<Output.Count do
   if Length(Output[i])=0 then Output.Delete(i) else inc(i);
  //De-indent
  for i:=0 to Output.Count-1 do Output[i]:=Trim(Output[i]);
  //Turn 'IF HITBY CharacterX' into 'IF HITBY' & 'IF NOT CONTENTS CharacterX GOTO',
  //make sure each DEFINE section has an END, and add a label after any ELSE's
  i:=1;
  define:=0;
  while i<Output.Count do
  begin
   //Turn IF HITBY into separate lines
   if LeftStr(Output[i],9)='IF HITBY ' then
   begin
    Output.Insert(i+1,'IF CONTENTS '+Copy(Output[i],10));
    Output[i]:='IF HITBY';
   end;
   //Add END to the end of a DEFINE section
   if((Output[i]='DEFINE ACTION')and(define=3))
   or((Output[i]='DEFINE HITS')  and(define=2))
   or((Output[i]=FCharTerm)and(define>1))then
    if Output[i-1]<>'END' then Output.Insert(i,'END');
   if Output[i]=FCharTerm then define:=0;
   if Output[i]='DEFINE TYPE' then define:=1;
   if Output[i]='DEFINE ACTION' then define:=2;
   if Output[i]='DEFINE HITS' then define:=3;
   //Add a label after an ELSE
   if Output[i]='ELSE' then Output.Insert(i+1,'LABEL ELSE');
   inc(i);
  end;
  //Add line numbers
  for i:=1 to Output.Count-1 do Output[i]:=IntToHex(i,4)+': '+Output[i];
  //Add the terminating ENDs and character numbers
  c:=-1;//Character number
  i:=0;
  while i<Output.Count do
  begin
   if Copy(Output[i],7,40)=FCharTerm then
   begin
    if c>=0 then Output[i]:=Copy(Output[i],1,6)+'END'
    else //Unless it is the first one, in which case just delete it
    begin
     Output.Delete(i);
     dec(i);
    end;
    inc(c);
    Output.Insert(i+1,'Char: '+IntToStr(c));
   end;
   inc(i);
  end;
  //Add the final END
  if Copy(Output[Output.Count-1],7)<>'END' then
  begin
   //Last line number
   i:=StrToIntDef('$'+LeftStr(Output[Output.Count-1],4),Output.Count-1)+1;
   Output.Add(IntToHex(i,4)+': END');
  end;
  //Find the ELSE's and the associated IF
  for i:=1 to Output.Count-1 do
  begin
   if Copy(Output[i],7)='ELSE' then
   begin
    //Change the next line for a proper label
    Output[i+1]:=StringReplace(Output[i+1],
                               'ELSE',
                               'Label'+LeftStr(Output[i+1],4),
                               replace);
    //Track back to find the associated IF
    c:=0; //IF/ENDIF counter
    j:=i; //Position in the text
    InType:=False; //Use this to flag when found
    while(j>1)and(not InType)and(LeftStr(Output[j],4)<>'Char')do
    begin
     dec(j);
     if(c=0)and(Copy(Output[j],7,3)='IF ')then InTYpe:=True; //Found
     if(c<>0)and(Copy(Output[j],7,3)='IF ')then inc(c);
     if Copy(Output[j],7)='ENDIF' then dec(c);
    end;
    //Make a note of the associated IF
    if InType then Output[i]:=Output[i]+' '+LeftStr(Output[j],4);
   end;
  end;
  //Turn 'IF' into 'IF NOT' and 'IF NOT' into 'IF', with GOTO to the ENDIF
  for i:=1 to Output.Count-1 do
  begin
   //Find ENDIF
   if Copy(Output[i],7,5)='ENDIF' then
   begin
    Output[i]:=LeftStr(Output[i],6)+'LABEL Label'+LeftStr(Output[i],4);
    j:=i;
    //Track back to find the matching IF
    repeat
     dec(j);
     while(j>0)and(Copy(Output[j],7,3)<>'IF ')do dec(j);
    until(j=0)or((Copy(Output[j],7,3)='IF ')and(Pos('GOTO Label',Output[j])=0));
    //Replace IF NOT with IF and IF with IF NOT
    if Copy(Output[j],7,7)='IF NOT ' then
     Output[j]:=StringReplace(Output[j],'IF NOT ','IF ',replace)
    else if Copy(Output[j],7,3)='IF ' then
     Output[j]:=StringReplace(Output[j],'IF ','IF NOT ',replace);
    //Add the GOTO
    if Copy(Output[j],7,3)='IF ' then
     Output[j]:=Output[j]+' GOTO Label'+LeftStr(Output[i],4);
   end;
   //While we are here, turn 'CREATE(chr)' into 'CREATE(chr,curr)'
   if(Copy(Output[i],7,7)='CREATE(')and(Pos(',',Output[i])=0)then
    Output[i]:=LeftStr(Output[i],Length(Output[i])-1)+',curr)';
  end;
  //Now sort the ELSEs out
  for i:=1 to Output.Count-1 do
  begin
   //Find the ELSE, again
   if Copy(Output[i],7,5)='ELSE ' then
   begin
    //Get the line number of the associated IF
    OLbl:=RightStr(Output[i],4);
    //Find it
    j:=i;
    while LeftStr(Output[j],4)<>OLbl do dec(j);
    //Get the GOTO destination
    NLbl:=RightStr(Output[j],4);
    //Change the GOTO destination to point to the next line
    Output[j]:=StringReplace(Output[j],NLbl,LeftStr(Output[i+1],4),replace);
    //Change the ELSE to a GOTO with the original destination of the IF
    Output[i]:=LeftStr(Output[i],6)+'GOTO Label'+NLbl;
   end;
  end;
  //Loop round and perform multiple tasks
  i:=0;
  while i<Output.Count do
  begin
   //Make a note of the start of the character
   if Copy(Output[i],1,5)='Char:' then soc:=i+1;
   //Concatenate the LABELs
   while(Copy(Output[i],7,6)='LABEL ')and(Copy(Output[i-1],7,6)='LABEL ')do
   begin
    for j:=0 to Output.Count-1 do
     if Pos('GOTO '+Copy(Output[i],13),Output[j])>0 then
      Output[j]:=StringReplace(Output[j],
                               Copy(Output[i],13),
                               Copy(Output[i-1],13),
                               replace);
    Output.Delete(i);
    dec(i);
   end;
   //Remove the 'NAME's
   if Copy(Output[i],7,5)='NAME ' then
   begin
    if Copy(Output[i-1],1,5)='Char:' then
     c:=StrToInt(Copy(Output[i-1],7))
    else
     c:=-1;
    if(c>=0)and(c<=31)then Chrs[c]:=Copy(Output[i],12);
    Output.Delete(i);
    dec(i);
   end;
   //Change any 'LABEL's to a Label####
   if(Copy(Output[i],7,6)='LABEL ')and(Copy(Output[i],13,5)<>'Label')then
   begin
    OLbl:=Copy(Output[i],13); //Old label
    NLbl:='Label'+Copy(Output[i],1,4); //New label
    Output[i]:=StringReplace(Output[i],'LABEL '+OLbl,'LABEL '+NLbl,replace);
    //Start from the start of the character and find the GOTOs
    c:=soc;
    while(c<Output.Count)and(Copy(Output[c],1,5)<>'Char')do
    begin
     if Pos('GOTO '+OLbl,Output[c])>0 then
      Output[c]:=StringReplace(Output[c],'GOTO '+OLbl,'GOTO '+NLbl,replace);
     inc(c);
    end;
   end;
   inc(i);
  end;
  //Now we go through it again and remove all LABELs
  i:=0;
  while i<Output.Count do
  begin
   if Copy(Output[i],7,6)='LABEL ' then
   begin
    NLbl:=LeftStr(Output[i+1],4);//Next line number
    for j:=0 to Output.Count-1 do
     if Pos('GOTO '+Copy(Output[i],13),Output[j])>0 then
      Output[j]:=StringReplace(Output[j],Copy(Output[i],13),NLbl,replace);
    Output.Delete(i);
   end else inc(i);
  end;
  //We can now replace the names and userflags
  soc:=-1;
  for i:=0 to Output.Count-1 do
  begin
   //Make a note of the ACTION and HITS sections
   if Copy(Output[i],1,5)='Char:' then //Need the character number first
     soc:=StrToInt(Copy(Output[i],7));
   if soc<>-1 then
   begin
    if Copy(Output[i],7)='DEFINE ACTION' then
     Actions[soc]:=LeftStr(Output[i+1],4);
    if Copy(Output[i],7)='DEFINE HITS' then
     Hits[soc]:=LeftStr(Output[i+1],4);
   end;
   //Substitute the Flags - first make a note of all the UserFlags first
   if Copy(Output[i],7)='DEFINE TYPE' then
   begin
    j:=i+1;
    while(Copy(Output[j],7,7)<>'DEFINE ')and(Copy(Output[j],7)<>'END')do
    begin
     OLbl:=Copy(Output[j],7);
     if (not MatchStr(OLbl,FRISysFlags[0]))
     and(not MatchStr(OLbl,FRISysFlags[1]))then
     begin
      NLbl:='';
      c:=IndexStr(OLbl,UserFlags);
      if c=-1 then
      begin
       c:=7;
       while(UserFlags[c]<>'')and(c>0)do dec(c);
       if c>-1 then UserFlags[c]:=OLbl;
      end;
      if c>-1 then
      begin
       NLbl:='UserFlag'+IntToStr(c);
       if soc>-1 then Flags[2,soc]:=Flags[2,soc]OR(1<<c);
      end;
      Output[j]:=StringReplace(Output[j],OLbl,NLbl,replace);
     end;
     //System Flags
     for c:=0 to 1 do
      if MatchStr(OLbl,FRISysFlags[c]) then
      begin
       Output[j]:=StringReplace(Output[j],
                                OLbl,
                                'SystemFlag'+IntToStr(c)
                                             +IntToStr(IndexStr(OLbl,FRISysFlags[c])),
                                replace);
       if soc>-1 then
        Flags[c,soc]:=Flags[c,soc]OR(1<<IndexStr(OLbl,FRISysFlags[c]));
      end;
     inc(j);
    end;
   end;
   //Now replace any names in the code with generic names
   for j:=0 to 31 do
    if Chrs[j]<>'' then
     if(Pos(' '+Chrs[j]+' ',Output[i])>0)
     or(Pos('('+Chrs[j]+',',Output[i])>0)
     or(Pos(','+Chrs[j]+')',Output[i])>0)
     or(Pos('('+Chrs[j]+')',Output[i])>0)then
      Output[i]:=StringReplace(Output[i],
                               Chrs[j],
                               'Character'+IntToStr(j),
                               replace);
  end;
  //And now remove all DEFINE lines
  i:=0;
  InType:=False;
  while i<Output.Count do
  begin
   if Copy(Output[i],7)='DEFINE TYPE'   then InType:=True;
   if Copy(Output[i],7)='DEFINE HITS'   then InType:=False;
   if Copy(Output[i],7)='DEFINE ACTION' then InType:=False;
   if LeftStr(Output[i],4)='Char'       then InType:=False;
   //Remove the entire DEFINE TYPE section
   if InType then
   begin
    //The end of the TYPE section
    if Copy(Output[i],7)='END' then InType:=False;
    Output.Delete(i);
    dec(i);
   end;
   //Remove an END directly after a Char:
   if i>0 then
   begin
    if(Copy(Output[i],7)='END')and(LeftStr(Output[i-1],4)='Char')then
    begin
     Output.Delete(i);
     dec(i);
    end;
    //Remove the DEFINEs
    if Copy(Output[i],7,7)='DEFINE ' then
    begin
     Output.Delete(i);
     dec(i);
    end;
   end;
   inc(i);
  end;
  //Loop through and remove the 'Char' we added earlier, as it is no longer required
  i:=1;
  while i<Output.Count do
   if LeftStr(Output[i],4)='Char' then Output.Delete(i) else inc(i);
  //We can now replace the userflag and system flag names in the code
  for i:=0 to Output.Count-1 do
   for j:=0 to 7 do
   begin
    for c:=0 to 1 do
     if FRISysFlags[0,j]<>'' then
      if(Pos(' '+FRISysFlags[c,j]+' ',Output[i])>0)
      or(Pos('('+FRISysFlags[c,j]+',',Output[i])>0)
      or(Pos(','+FRISysFlags[c,j]+')',Output[i])>0)
      or(Pos('('+FRISysFlags[c,j]+')',Output[i])>0)then
       Output[i]:=StringReplace(Output[i],
                                FRISysFlags[c,j],
                                'SystemFlag'+IntToStr(c)+IntToStr(j),
                                replace);
    if UserFlags[j]<>'' then
     if(Pos(' '+UserFlags[j]+' ',Output[i])>0)
     or(Pos('('+UserFlags[j]+',',Output[i])>0)
     or(Pos(','+UserFlags[j]+')',Output[i])>0)
     or(Pos('('+UserFlags[j]+')',Output[i])>0)then
      Output[i]:=StringReplace(Output[i],
                               UserFlags[j],
                               'UserFlag'+IntToStr(j),
                               replace);
   end;
  //Let's do some compiling
  if BBC then address:=$5BB0 else address:=$4A80; //Base address
  //Create the headers
  SetLength(Result,$F0);
  //Author - terminated by $0D and padded with $00
  for i:=0 to $1F do
  begin
   if i<$10 then
   begin
    if i<Length(Output[0])then Result[i]:=Ord(Output[0][i+1]);
    if i=Length(Output[0])then Result[i]:=$0D;
    if i>Length(Output[0])then Result[i]:=$00;
   end;
   //System Flags
   Result[$90+i]:=Flags[0,i];
   Result[$B0+i]:=Flags[1,i];
   //User flags
   Result[$D0+i]:=Flags[2,i];
  end;
  //First pass - substitute all we can. Leave the GOTOs for the second pass.
  for i:=1 to Output.Count-1 do
  begin
   j:=0;
   //Using these as flags to show we've got a match
   Tmp0:='';
   //The command we are looking for
   Tmp1:=Copy(Output[i],7);
   while(j<67)and(Tmp0='')do
   begin
    //Exact match
    if FRIBBCCmds[j,1]=Tmp1 then
     if BBC then Tmp0:=FRIBBCCmds[j,0] else Tmp0:=FRIElkCmds[j,0];
    //Not an exact match
    if Tmp0='' then
    begin
     if(Pos('#',FRIBBCCmds[j,1])>0)
     or(Pos('%',FRIBBCCmds[j,1])>0)
     or(Pos('^',FRIBBCCmds[j,1])>0)
     or(Pos('*',FRIBBCCmds[j,1])>0)
     or(Pos('@',FRIBBCCmds[j,1])>0)
     or(Pos('$',FRIBBCCmds[j,1])>0)
     or(Pos('!',FRIBBCCmds[j,1])>0)
     or(Pos('&',FRIBBCCmds[j,1])>0)
     or(Pos('?',FRIBBCCmds[j,1])>0)then
     begin
      //Get the first of these to occur
      c:=Pos('#',FRIBBCCmds[j,1]); // # always comes first, if it is there
      if c=0 then c:=Pos('^',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('@',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('*',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('$',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('!',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('&',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('?',FRIBBCCmds[j,1]);
      if c=0 then c:=Pos('%',FRIBBCCmds[j,1]); // % always comes last (or first)
      if c>0 then
       if LeftStr(Tmp1,c-1)=LeftStr(FRIBBCCmds[j,1],c-1) then
        if BBC then Tmp0:=FRIBBCCmds[j,0] else Tmp0:=FRIElkCmds[j,0];
     end;
    end;
    inc(j);
   end;
   //Did we find a match? Then put it in the code
   if Tmp0<>'' then
   begin
    //The original command
    Tmp2:=FRIBBCCmds[j-1,1];
    //Get the positions of the variables - those that don't exist will be 0
    x:=Pos('xx',Tmp2);
    y:=Pos('yy',Tmp2);
    z:=Pos('zz',Tmp2);
    //Do we need to replace anything?
    if(Pos('xx',Tmp0)>0)or(Pos('yy',Tmp0)>0)or(Pos('xx',Tmp0)>0)then
    begin
     //The new values of xx,yy and zz
     xx:=-1;
     yy:=-1;
     zz:=-1;
     // ^ is only ever used with xx. This is a number representing an EVENT.
     if(Pos('^',Tmp2)>0)and(x>0)then
     begin
      xx:=(1<<(StrToInt(Copy(Tmp1,x-1,Pos(')',Tmp1)-(x-1)))+1))-1;
      x:=-1;
     end;
     // * is only ever used with xx. This is a number (0-7) representing a direction to MOVE to.
     if(Pos('*',Tmp2)>0)and(x>0)then
     begin
      xx:=IndexStr(Copy(Tmp1,x-1,1),FRIMove);
      x:=-1;
     end;
     // ? only appears by itself in one command - FLASH. It is a number (0..7) representing a colour.
     if(Pos('?',Tmp2)>0)and(x>0)then
     begin
      xx:=IndexStr(Copy(Tmp1,x-1,Pos(')',Tmp1)-(x-1)),FRIColours);
      x:=-1;
     end;
     // ! only ever used with xx. It is a USERFLAG and is one of 7 values.
     if(Pos('!',Tmp2)>0)and(x>0)then
     begin
      xx:=1<<StrToInt(Copy(Tmp1,x-1,1));
      x:=-1;
     end;
     // # Sometimes occurs twice. Can be either xx or yy. This is a literal integer.
     if(Pos('#',Tmp2)>0)and((x>0)or(y>0))then
      while Pos('#',Tmp2)>0 do
      begin
       xx:=ExtractInteger(x);
       yy:=ExtractInteger(y);
      end;
     // & always occurs twice. Is only used as &yy&xx with CHANCE and is a 16bit number representing a floating point number.
     if(Pos('&',Tmp2)>0)and(x>0)and(y>0)then
      if Pos('%',Tmp1)>0 then
      begin
       c:=StrToInt(Copy(Tmp1,y-1,Pos('%',Tmp1)-(y-1)));
       c:=Round(((c/100)*32768)-1);
       xx:=c AND$FF;
       yy:=(c>>8)AND$FF;
       x:=-1;
       y:=-1;
      end else Errors.Add('Incorrect CHANCE parameter');
     // @ is always the last thing on the line, but is followed by a ). Can be either xx or yy. Only used with CREATE, it is a number representing where to create the character.
     if(Pos('@',Tmp2)>0)and((x>0)or(y>0))then
     begin
      c:=-1;
      j:=-1;
      //Get the position of the direction
      GetDirPos(x);
      GetDirPos(y);
      if(c>-1)and(j>-1)and(j>c)then
      begin
       OLbl:=Copy(Tmp1,c,j-c);
       c:=0;
       // if Msg in Devices then WriteLn(IndexStr(Msg,Devices));
       while(c<Length(FRICreTok))and(FRICreTok[c,1]<>OLbl)do inc(c);
       if c<Length(FRICreTok) then c:=StrToInt(FRICreTok[c,0])
       else Errors.Add('Invalid Create direction in "'+Tmp1+'"');
       if x>0 then if Tmp2[x-1]='@' then
       begin
        x:=-1;
        xx:=c;
       end;
       if y>0 then if Tmp2[y-1]='@' then
       begin
        y:=-1;
        yy:=c;
       end;
      end;
     end;
     // $ and % - absolute and relative addressing for GOTO
     if(Pos('$',Tmp2)>0)or(Pos('%',Tmp2)>0)then
     begin
      //We'll deal with these ones in a second pass
      if x>0 then if(Tmp2[x-1]='$')or(Tmp2[x-1]='%')then x:=-1;
      if y>0 then if(Tmp2[y-1]='$')or(Tmp2[y-1]='%')then y:=-1;
      if z>0 then if(Tmp2[z-1]='$')or(Tmp2[z-1]='%')then z:=-1;
     end;
     //Replace XX
     if(xx<>-1)and(x=-1)then
      Tmp0:=StringReplace(Tmp0,'xx',IntToHex(xx,2),replace);
     //Replace YY
     if(yy<>-1)and(y=-1)then
      Tmp0:=StringReplace(Tmp0,'yy',IntToHex(yy,2),replace);
     //Replace ZZ
     if(zz<>-1)and(z=-1)then
      Tmp0:=StringReplace(Tmp0,'zz',IntToHex(zz,2),replace);
    end;
    //Create the new line
    Output[i]:=LeftStr(Output[i],4)              //Original line number
              +'['+IntToHex(address,4)+']'       //6502 address
              +':'+Tmp0;                         //The 6502 code
    //Update the ACTION and HITS tables
    for j:=0 to 31 do
    begin
     //ACTIONs ($10 LSB and $30 MSB)
     if Actions[j]=LeftStr(Output[i],4) then
     begin
      Result[$10+j]:=address AND $FF;
      Result[$30+j]:=address>>8 AND $FF;
     end;
     //HITS ($50 LSB and $70 MSB)
     if Hits[j]=LeftStr(Output[i],4) then
     begin
      Result[$50+j]:=address AND $FF;
      Result[$70+j]:=address>>8 AND $FF;
     end;
    end;
    //Add the code still to be dealt with on the second pass
    if(Pos('$',Tmp2)>0)or(Pos('%',Tmp2)>0)then
     Output[i]:=Output[i]+'{'+Tmp1+'}<'+Tmp2+'>';
    //Move the address pointer on
    inc(address,Length(Tmp0)div 2);
    //There have been some errors
    if x>0 then Errors.Add('Unable to replace xx');
    if y>0 then Errors.Add('Unable to replace yy');
    if z>0 then Errors.Add('Unable to replace zz');
   end else Errors.Add('Unable to compile "'+Tmp1+'"');
  end;
  //Second pass - resolve the GOTOs and convert the strings into bytes
  // $ always occurs twice and is only on it's own. Is only used as $xx$yy. This is a 16bit hex number, used by GOTO so we need the base address
  // % is always the last thing on the line. Can be xx, yy or zz. It is an 8 bit number used as a relative offset to GOTO to.
  for i:=1 to Output.Count-1 do
  begin
   //Resolve the GOTO references
   if Pos('GOTO',Output[i])>0 then //Only lines with a GOTO
   begin
    //Get the original script
    x :=Pos('{',Output[i])+1;
    xx:=Pos('}',Output[i])-x;
    y :=Pos('<',Output[i])+1;
    yy:=Pos('>',Output[i])-y;
    Tmp0:=Copy(Output[i],x,xx);
    Tmp1:=Copy(Output[i],y,yy);
    //Remove the original script
    Tmp2:=Output[i];
    Output[i]:=LeftStr(Output[i],Pos('{',Output[i])-1);
    //Find the line
    j:=0;
    while(j<Output.Count)and(LeftStr(Output[j],4)<>RightStr(Tmp0,4))do inc(j);
    //Get the absolute address
    if j<Output.Count then zz:=StrToInt('$'+Copy(Output[j],6,4)) else
    begin
     zz:=0;
     Errors.Add('Unable to locate GOTO reference at line '+IntToStr(i));
    end;
    //Relative addressing
    if Pos('%',Tmp1)>0 then
    begin
     //Get the address of the branch instruction
     z:=StrToInt('$'+Copy(Output[i],6,4))+((x-12)div 2);
     OLbl:='';
     if Pos('xx',Output[i])>0 then OLbl:='xx';
     if Pos('yy',Output[i])>0 then OLbl:='yy';
     if Pos('zz',Output[i])>0 then OLbl:='zz';
     if z<zz then //This is wrong **********************************************
      Output[i]:=StringReplace(Output[i],OLbl,IntToHex(zz-z,2),replace)
     else
      Output[i]:=StringReplace(Output[i],OLbl,IntToHex($100-(z-zz)),replace);
    end;
    //Absolute addressing
    if Pos('$',Tmp1)>0 then
    begin
     //Place it in the code
     Output[i]:=StringReplace(Output[i],'xx',IntToHex(zz>>8,2),replace);
     Output[i]:=StringReplace(Output[i],'yy',IntToHex(zz AND$FF,2),replace);
    end;
   end;
   //Convert the strings into bytes
   Tmp1:=Copy(Output[i],12);
   x:=Length(Result);
   SetLength(Result,x+Length(Tmp1)div 2);
   for xx:=0 to (Length(Tmp1)div 2)-1 do
    Result[x+xx]:=StrToIntDef('$'+Copy(Tmp1,(xx*2)+1,2),$00)
  end;
  //Compiled code should be no bigger than 0x2D0+0xF0
  if Length(Result)>$2D0+$F0 then
  begin
   Errors.Add('Can''t make small enough (0x'+IntToHex(Length(Result),4)+')');
   SetLength(Result,0);
  end;
  Output.Free;
 end;
end;

end.
