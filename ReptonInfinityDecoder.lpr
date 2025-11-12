program ReptonInfinityDecoder;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
 {$IFDEF HASAMIGA}
 athreads,
 {$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, RIMainUnit
 { you can add units after this };

{$R *.res}

begin
 RequireDerivedFormResource:=True;
 Application.Title:='Repton Infinity BluePrint';
 Application.Scaled:=True;
 Application.Initialize;
 Application.CreateForm(TRIMainForm, RIMainForm);
 Application.Run;
end.

