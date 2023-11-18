unit RIMainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls,StrUtils;

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
  TopPanel: TPanel;
  SaveDialog: TSaveDialog;
  procedure btnConvertClick(Sender: TObject);
  procedure btnRecompileClick(Sender: TObject);
  procedure btnSaveAsTextClick(Sender: TObject);
  procedure btnLoadFileClick(Sender: TObject);
  procedure btnSaveAsTokenClick(Sender: TObject);
  procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  function Detokenise(C: Integer): TStringList;
  function Tokenise(Input: TStrings): RIByteArray;
  function Decompile(C: Integer;Elk: Boolean=False;
                                           TidyCode: Boolean=True): TStringList;
  function Convert(RIData: RIByteArray;toBBC: Boolean): RIByteArray;
  function Compile(Input: TStrings;out Errors: TStringList;
                                           BBC: Boolean=True): RIByteArray;
 private
  LoadedData: array of Byte;
  LoadedFile: String;
  const
   //All the Reptol commands in order of token
   tokens: array[$80..$AB] of String = (
   'NAME','HITBY','LOOK(','DEFINE','CREATE(','IF','MOVING','ELSE','ENDIF','GOTO',
   'NOT','KILLREPTON','CHANGE(','END','SCORE(','SOUND(','FLIP','EFFECT(','FLASH(',
   'CHANCE(','KEY','One','Two','Four','TYPE','ACTION','HITS','MOVE(','STATE(',
   'LABEL','EVENT(','CONTENTS','Animate','RED','GREEN','YELLOW','BLUE','MAGENTA',
   'CYAN','WHITE','WESTOF','SOUTHOF','EASTOF','NORTHOF');
   //System flags
   SysFlags: array[0..1] of array[0..7] of String=(
   ('invalid0','invalid1','One','Two','Four','invalid5','Repton','Animate'),
   ('Transport','Squash','Cycle','Under','VPush','HPush','Deadly','Solid'));
   //Used with StringReplace
   replace = [rfReplaceAll, rfIgnoreCase];
   //Compile/Decompile translations - BBC Version
   BBCCmds: array[0..66] of array[0..1] of String=(
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
                ('A042','UNKNOWN COMMAND'),
                ('A51D','UNKNOWN COMMAND'),
                ('60','END'));
   //Compile/Decompile translations - Electron Version
   ElkCmds: array[0..66] of array[0..1] of String=(
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
   //Directions for 'MOVE' command
   Move: array[0..7] of String=('E','S','W','N','F','R','B','L');
   //Standard colours
   Colours: array[0..7] of String=(
                'BLACK','RED','GREEN','YELLOW','BLUE','MAGENTA','CYAN','WHITE');
   //Directions for 'CREATE' command
   Creat: array[0..12] of array[0..1] of String=(('$42','curr'),
                        ('$41','W'), ('$43','E'), ('$62','S'), ('$22','N'),
                        ('$FF','W'), ('$01','E'), ('$E0','N'), ('$20','S'),
                        ('$63','SE'),('$23','NE'),('$61','SW'),('$21','NW'));
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
begin
 //Open and load the file
 F:=TFileStream.Create(FileNames[0],fmOpenRead OR fmShareDenyNone);
 SetLength(LoadedData,F.Size);
 F.ReadBuffer(LoadedData[0],F.Size);
 F.Free;
 //Clear the output container, ready for the output
 OutputDisplay.Clear;
 fname:=ExtractFileName(FileNames[0]);
 LoadedFile:=FileNames[0];
 lblFileName.Caption:='Current file: '+ExtractFileName(LoadedFile);
 //Tokenised source file                                  DETOKENISE
 if(LeftStr(fname,2)='T.')      //BBC version
 or(LeftStr(fname,3)='eT.')then //Electron version
  for Index:=-1 to 31 do
  begin
   if Index>=0 then OutputDisplay.Lines.Add(StringOfChar('-',40));
   OutputDisplay.Lines.AddStrings(Detokenise(Index));
 end;
 //Compiled object file                                   DECOMPILE
 if(LeftStr(fname,2)='O.')      //BBC version
 or(LeftStr(fname,3)='eO.')then //Electron version
  for Index:=-1 to 31 do
  begin
   if Index>=0 then OutputDisplay.Lines.Add(StringOfChar('-',40));
   OutputDisplay.Lines.AddStrings(Decompile(Index,LeftStr(fname,3)='eO.',cbTidyCode.Checked));
  end;
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

procedure TRIMainForm.btnRecompileClick(Sender: TObject);
var
 SaveData: RIByteArray;
 F       : TFileStream;
 E       : TStringList;
begin
 if OutputDisplay.Lines.Count>0 then
  if SaveDialog.Execute then
  begin
   SaveData:=Compile(OutputDisplay.Lines,E);
   if E.Count=0 then
   begin
    F:=TFileStream.Create(SaveDialog.FileName,fmCreate OR fmShareDenyNone);
    F.WriteBuffer(SaveData[0],Length(SaveData));
    F.Free;
   end
   else ShowMessage('Compilation failed - there were some errors');
  end;
end;

procedure TRIMainForm.btnSaveAsTextClick(Sender: TObject);
begin
 if OutputDisplay.Lines.Count>0 then
 begin
  SaveDialog.InitialDir:=ExtractFilePath(LoadedFile);
  SaveDialog.FileName:=ExtractFileName(LoadedFile)+'.txt';
  if SaveDialog.Execute then OutputDisplay.Lines.SaveToFile(SaveDialog.FileName);
 end;
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
begin
 if OutputDisplay.Lines.Count>0 then
 begin 
  SaveDialog.InitialDir:=ExtractFilePath(LoadedFile);
  fname:=ExtractFileName(LoadedFile);
  if LeftStr(fname,2)='O.' then fname:='T'+Copy(fname,2);
  if LeftStr(fname,3)='eO.' then fname:='eT'+Copy(fname,3);
  SaveDialog.FileName:=fname;
  if SaveDialog.Execute then
  begin
   SaveData:=Tokenise(OutputDisplay.Lines);
   F:=TFileStream.Create(SaveDialog.FileName,fmCreate OR fmShareDenyNone);
   F.WriteBuffer(SaveData[0],Length(SaveData));
   F.Free;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Detokeniser - for 'T.' files
-------------------------------------------------------------------------------}
function TRIMainForm.Detokenise(C: Integer): TStringList;
var
 lC: Integer;
 Index: Cardinal;
 line : String;
begin
 //Create the output container
 Result:=TStringList.Create;
 line:='';
 //Get the author name
 if C=-1 then
 begin
  Index:=$0;
  while(Index<$10)and(LoadedData[Index]<>$0D)do
  begin
   if(LoadedData[Index]>=32)and(LoadedData[Index]<=126)then
    line:=line+chr(LoadedData[Index]);
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
  while(Index<Length(LoadedData))and(lC<>C)do
  begin
   if LoadedData[Index]=$FE then inc(lC);
   inc(Index);
  end;
  //Detokenise the character
  while Index<Length(LoadedData) do
  begin
   //New line
   if LoadedData[Index]=$0D then
   begin
    Result.Add(line);
    line:='';
   end;
   //ASCII character
   if(LoadedData[Index]>=32)and(LoadedData[Index]<=126)then
    line:=line+chr(LoadedData[Index]);
   //Tokenised command
   if(LoadedData[Index]>=Low(tokens))and(LoadedData[Index]<=High(tokens))then
    line:=line+tokens[LoadedData[Index]];
   //Indentation
   if(LoadedData[Index]>=$C8)and(LoadedData[Index]<$FE)then
    line:=line+StringOfChar(' ',LoadedData[Index]-$C8);
   //End of definition for this character
   if LoadedData[Index]=$FE then Index:=Length(LoadedData);
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
 Index,
 match,
 found,
 ptr   : Integer;
 line  : String;
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
   if Input[Index]=StringOfChar('-',40) then AddByte($FE)
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
     match:=Low(tokens);
     while(found=0)and(match<=High(tokens))do
     begin
      //Found one?
      if Copy(Input[Index],ptr,Length(tokens[match]))=tokens[match] then
       found:=match; //Make a note
      inc(match);
     end;
     //If we have, then add it to the data
     if found>=Low(tokens) then
     begin
      AddByte(found);
      //Move along to after the command
      inc(ptr,Length(tokens[found])-1);
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
function TRIMainForm.Decompile(C: Integer;Elk: Boolean;
                                           TidyCode: Boolean=True): TStringList;
var
 B,D,E   : Byte;
 base    : Word;
 Offset,
 EndOff,
 ThisOff,
 Index2,
 Index   : Integer;
 line    : String;
 cmds    : array[0..133] of array[0..1] of String;
//Check a stream of bytes against a known stream
function Check(tocheck,cmd: String): String;
var
 I,T: Integer;
 ok : Boolean;
 S  : String;
 R  : Cardinal;
 P  : Real;
begin
 //We already have found something, so exit
 if line<>'' then exit;
 //Empty result
 Result:='';
 I:=1;
 ok:=True;
 while I<Length(tocheck) do
 begin
  //Get each byte from the string
  S:=Copy(tocheck,I,2);
  //Does it match?
  if IntToHex(StrToIntDef('$'+S,0),2)=S then
   if StrToIntDef('$'+S,0)<>LoadedData[Offset+(I-1)div 2] then ok:=False;
  //Does not match, so check to see if it is a wildcard - only check if OK so far
  if(IntToHex(StrToIntDef('$'+S,0),2)<>S)and(ok)then
  begin
   R:=LoadedData[Offset+(I-1)div 2];
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
    cmd:=StringReplace(cmd,'?'+S,colours[R mod (High(colours)+1)],replace);
   //MOVE direction
   if Pos('*'+S,cmd)>0 then
    cmd:=StringReplace(cmd,'*'+S,move[R mod (High(move)+1)],replace);
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
    for T:=0 to High(creat) do
     if StrToInt(creat[T,0])=R then
      cmd:=StringReplace(cmd,'@'+S,creat[T,1],replace);
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
 line:=RightStr(Result[Index],Length(Result[Index])-(Pos('GOTO',Result[Index])+6));
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
  cmds[Index*2  ]:=BBCCmds[Index];
  cmds[Index*2+1]:=ElkCmds[Index];
 end;
 //Create the output container
 Result:=TStringList.Create;
 line:='';
 //Get the author name
 if C=-1 then
 begin
  Index:=$0;
  while(Index<$10)and(LoadedData[Index]<>$0D)do
  begin
   if(LoadedData[Index]>=32)and(LoadedData[Index]<=126)then
    line:=line+chr(LoadedData[Index]);
   inc(Index);
  end;
  Result.Add(line);
 end;
 if(C>=0)and(C<32)then
 begin
  //Set the base address (minus $F0 for where the code starts)
  base:=$5AC0;//BBC is 5BB0-F0
  if Elk then base:=$4990;//Electron is 4A80-F0
  //Iterate through each of the 32 characters
  //Header
  Result.Add('NAME Character'+IntToStr(C)); //This is optional
  Result.Add('DEFINE TYPE'); //There could be none, so this is also optional
  //Add the flags
  for E:=0 to 7 do
  begin
   //System flags
   if LoadedData[$90+C]AND(1<<E)=1<<E then Result.Add(SysFlags[0,E]);
   if LoadedData[$B0+C]AND(1<<E)=1<<E then Result.Add(SysFlags[1,E]);
   //User flags
   if LoadedData[$D0+C]AND(1<<E)=1<<E then Result.Add('UserFlag'+IntToStr(E));
  end;
  //Find the ACTION or HITS offset for this character
  E:=$10;
  Offset:=0;
  while(Offset=0)and(E<>0)do
  begin
   Offset:=LoadedData[E+C]+LoadedData[E+$20+C]<<8;
   if Offset<base+$F0 then if E=$10 then E:=$50 else if E=$50 then E:=0;
  end;
  //If the offset is zero, then there is no definition
  if(Offset<>0)and(E<>0)then
  begin
   if E=$10 then Result.Add('DEFINE ACTION') else Result.Add('DEFINE HITS');
   dec(Offset,base);
   //Now need to find the end of the definition
   EndOff:=0;
   D:=C;
   while(EndOff=0)and(D<$1F)do
   begin
    inc(D);
    EndOff:=LoadedData[$10+D]+LoadedData[$30+D]<<8;
   end;
   if EndOff=0 then EndOff:=Length(LoadedData)-1 else dec(EndOff,base+1);
   //However, the end of the definition could be beyond another char's HITS
   ThisOff:=0;
   D:=0;
   while((ThisOff<Offset)or(ThisOff>EndOff))and(D<$1F)do
   begin
    if C<>D then ThisOff:=(LoadedData[$50+D]+LoadedData[$70+D]<<8)-base;
    inc(D);
   end;
   if(ThisOff>Offset)and(ThisOff<EndOff)then EndOff:=ThisOff-1;
   //So, we can now decompile the code
   while Offset<=EndOff do
   begin
    //We are adding the 'DEFINE HITS' section
    if(Offset=(LoadedData[$50+C]+LoadedData[$70+C]<<8)-base)and(E=$10)then
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
    if line='' then line:=IntToHex(Offset,4)+': '+IntToHex(LoadedData[Offset],2);
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
 C,D,L,F,
 Index: Integer;
 found,
 ok   : Boolean;
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
Compile an 'O' file from source
-------------------------------------------------------------------------------}
function TRIMainForm.Compile(Input: TStrings;out Errors: TStringList;
                                           BBC: Boolean=True): RIByteArray;
var
 i,j,c,
 x,y,z,
 xx,yy,zz,
 soc      : Integer;
 Tmp0,
 Tmp1,
 Tmp2,
 OLbl,
 NLbl     : String;
 Output   : TStringList;
 Chrs,
 Actions,
 Hits     : array[0..31] of String;
 Flags    : array[0..2] of array[0..31] of Byte;
 UserFlags: array[0..7] of String;
 InType   : Boolean;
 address  : Word;
 define   : Byte;
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
 //Before we do an compiling, validate the code
{                                                                              }
 //This is what we will use to work on
 Output:=TStringList.Create;
 Output.AddStrings(Input); //Can't work on 'Input' as this will change what was passed
 //Keep a track of any errors
 Errors:=TStringList.Create;
 //Initialise the containers
 for c:=0 to 31 do
 begin
  Chrs[c]:='';
  Actions[c]:='';
  Hits[c]:='';
  //Repton has certain system flags by default
  if c<>1 then Flags[0,c]:=0 else Flags[0,c]:=$C4;
  Flags[1,c]:=0;
  Flags[2,c]:=0;
 end;
 for c:=0 to 7 do UserFlags[c]:='';
 Result:=nil;
 //Remove empty lines
 i:=1;
 while i<Output.Count do
  if Length(Output[i])=0 then Output.Delete(i) else inc(i);
 //De-indent
 for i:=0 to Output.Count-1 do
  while(Length(Output[i])>0)and(Output[i][1]=' ')do
   Output[i]:=Copy(Output[i],2);
 //Turn 'IF HITBY CharacterX' into 'IF HITBY' & 'IF NOT CONTENTS CharacterX GOTO'
 //and make sure each DEFINE section has an END
 i:=1;
 Output.SaveToFile('/Users/geraldholdsworth/Desktop/DEBUG1.txt');//DEBUG
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
  or((Output[i]=StringOfChar('-',40))and(define>1))then
   if Output[i-1]<>'END' then Output.Insert(i,'END');
  if Output[i]=StringOfChar('-',40) then define:=0;
  if Output[i]='DEFINE TYPE' then define:=1;
  if Output[i]='DEFINE ACTION' then define:=2;
  if Output[i]='DEFINE HITS' then define:=3;
  inc(i);
 end;
 //Add line numbers
 for i:=1 to Output.Count-1 do Output[i]:=IntToHex(i,4)+': '+Output[i];
 //Check to ensure there are the same number of ENDIFs as IFs
 //Also, add the terminating ENDs and character numbers
 j:=0;//Number of IFs (reduces when ENDIF is encountered)
 c:=-1;//Character number
 i:=0;
 while i<Output.Count do
 begin
  if Copy(Output[i],7,5)='ENDIF' then dec(j);
  if(Copy(Output[i],7,3)='IF ')and(Copy(Output[i],7)<>'IF HITBY')then inc(j);
  if Copy(Output[i],7,40)=StringOfChar('-',40) then
  begin
   if j<>0 then
   begin
    if j<0 then Errors.Add('Too many ENDIFs in character '+IntToStr(c));
    if j>0 then Errors.Add('Not enough matching ENDIFs in character '+IntToStr(c));
    j:=0; //Reset the IF/ENDIF counter
   end
   else //Replace the line with an 'END'
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
 if j<>0 then
  if j<0 then Errors.Add('Too many ENDIFs in character '+IntToStr(c))
         else Errors.Add('Not enough matching ENDIFs in character '+IntToStr(c));
 if c<>31 then Errors.Add('Not enough characters ('+IntToStr(c)+')');
 //Only continue of no errors
 if Errors.Count=0 then
 begin
  //Add the final END
  if Copy(Output[Output.Count-1],7)<>'END' then
  begin
   //Last line number
   i:=StrToIntDef('$'+LeftStr(Output[Output.Count-1],4),Output.Count-1)+1;
   Output.Add(IntToHex(i,4)+': END');
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
  //Remove the ELSEs ----- change these to GOTO LABEL and point to the ENDIF corresponding to the IF
  {
   Start counter with 0 and track back. When an ENDIF is found, subtract 1.
   When an IF is found, and the counter is less than 0, add one.
   When an IF is found, and the counter is zero, then this is the corresponding IF to the ELSE.
   This will need to be moved before the above section, but the GOTO won't have been created at this stage.
   So, we will need to have a secondary section afterwards.
   This means, first we find the ELSE, track back to find the IF then add a note after the ELSE to find the
   corresponding IF. Finally, after the above section, replace the ELSE with the appropriate GOTO, copying
   the GOTO from the referenced IF.
  }
  i:=0;
  while i<Output.Count do
   if Copy(Output[i],7,4)='ELSE' then Output.Delete(i) else inc(i);
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
     if (not MatchStr(OLbl,SysFlags[0]))
     and(not MatchStr(OLbl,SysFlags[1]))then
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
      if MatchStr(OLbl,SysFlags[c]) then
      begin
       Output[j]:=StringReplace(Output[j],
                                OLbl,
                                'SystemFlag'+IntToStr(c)
                                             +IntToStr(IndexStr(OLbl,SysFlags[c])),
                                replace);
       if soc>-1 then
        Flags[c,soc]:=Flags[c,soc]OR(1<<IndexStr(OLbl,SysFlags[c]));
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
     if SysFlags[0,j]<>'' then
      if(Pos(' '+SysFlags[c,j]+' ',Output[i])>0)
      or(Pos('('+SysFlags[c,j]+',',Output[i])>0)
      or(Pos(','+SysFlags[c,j]+')',Output[i])>0)
      or(Pos('('+SysFlags[c,j]+')',Output[i])>0)then
       Output[i]:=StringReplace(Output[i],
                                SysFlags[c,j],
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
    if BBCCmds[j,1]=Tmp1 then
     if BBC then Tmp0:=BBCCmds[j,0] else Tmp0:=ElkCmds[j,0];
    //Not an exact match
    if Tmp0='' then
    begin
     if(Pos('#',BBCCmds[j,1])>0)
     or(Pos('%',BBCCmds[j,1])>0)
     or(Pos('^',BBCCmds[j,1])>0)
     or(Pos('*',BBCCmds[j,1])>0)
     or(Pos('@',BBCCmds[j,1])>0)
     or(Pos('$',BBCCmds[j,1])>0)
     or(Pos('!',BBCCmds[j,1])>0)
     or(Pos('&',BBCCmds[j,1])>0)
     or(Pos('?',BBCCmds[j,1])>0)then
     begin
      //Get the first of these to occur
      c:=Pos('#',BBCCmds[j,1]); // # always comes first, if it is there
      if c=0 then c:=Pos('^',BBCCmds[j,1]);
      if c=0 then c:=Pos('@',BBCCmds[j,1]);
      if c=0 then c:=Pos('*',BBCCmds[j,1]);
      if c=0 then c:=Pos('$',BBCCmds[j,1]);
      if c=0 then c:=Pos('!',BBCCmds[j,1]);
      if c=0 then c:=Pos('&',BBCCmds[j,1]);
      if c=0 then c:=Pos('?',BBCCmds[j,1]);
      if c=0 then c:=Pos('%',BBCCmds[j,1]); // % always comes last (or first)
      if c>0 then
       if LeftStr(Tmp1,c-1)=LeftStr(BBCCmds[j,1],c-1) then
        if BBC then Tmp0:=BBCCmds[j,0] else Tmp0:=ElkCmds[j,0];
     end;
    end;
    inc(j);
   end;
   //Did we find a match? Then put it in the code
   if Tmp0<>'' then
   begin
    //The original command
    Tmp2:=BBCCmds[j-1,1];
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
      xx:=IndexStr(Copy(Tmp1,x-1,1),Move);
      x:=-1;
     end;
     // ? only appears by itself in one command - FLASH. It is a number (0..7) representing a colour.
     if(Pos('?',Tmp2)>0)and(x>0)then
     begin
      xx:=IndexStr(Copy(Tmp1,x-1,Pos(')',Tmp1)-(x-1)),Colours);
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
       while(c<Length(Creat))and(Creat[c,1]<>OLbl)do inc(c);
       if c<Length(Creat) then c:=StrToInt(Creat[c,0])
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
    Result[x+xx]:=StrToIntDef('$'+Copy(Tmp1,(xx*2)+1,2),$00);
  end;
 end;
 //Output the results                                      *********************
 //This will, evenutally, get deleted to output an array of bytes - the code
 OutputDisplay.Lines.Add(StringOfChar('*',40));
 OutputDisplay.Lines.Add('Compiled code follows');
 OutputDisplay.Lines.Add(StringOfChar('*',40));
 if Errors.Count>0 then OutputDisplay.Lines.AddStrings(Errors)
 else OutputDisplay.Lines.AddStrings(Output);
end;

end.
