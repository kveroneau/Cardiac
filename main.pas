unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, math;

type

  { TCardiacForm }

  TCardiacForm = class(TForm)
    AsciiButton: TButton;
    OpenDialog: TOpenDialog;
    ResetButton: TButton;
    MaxSpeed: TCheckBox;
    Label3: TLabel;
    Output: TListBox;
    StepCardiac: TButton;
    RunCardiac: TButton;
    LoadDeck: TButton;
    CardiacLogo: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Reader: TListBox;
    Deck: TMemo;
    MemoryGroup: TGroupBox;
    procedure AsciiButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadDeckClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure RunCardiacClick(Sender: TObject);
    procedure StepCardiacClick(Sender: TObject);
  private
    Memory: Array of TEdit;
    FPC: Byte;
    Acc: Integer;
    Running: Boolean;
    procedure CreateMemory;
    procedure ResetMemory;
    function GetInput: string;
    function GetMemory(addr: Byte): string;
    function GetMemoryInt(addr: Byte): Integer;
    procedure SetMemory(addr: Byte; value: Integer);
    procedure SetMemory(addr: Byte; value: String);
    procedure ProcessOp;
    procedure SetPC(const value: Byte);
    procedure AddString(const s: string);
    property PC: Byte read FPC write SetPC;
  end;

var
  CardiacForm: TCardiacForm;

implementation

{$R *.lfm}

{ TCardiacForm }

procedure TCardiacForm.FormCreate(Sender: TObject);
begin
  FPC:=0;
  Acc:=0;
  CreateMemory;
  ResetMemory;
  Deck.Text:='';
end;

procedure TCardiacForm.AsciiButtonClick(Sender: TObject);
var
  s: String;
  i,c: Integer;
begin
  if Output.Items.Count = 0 then
  begin
    ShowMessage('No Output to display as ASCII!');
    Exit;
  end;
  s:='';
  for i:=0 to Output.Items.Count-1 do
  begin
    c:=StrToInt(Output.items.Strings[i]);
    if c < 128 then
      s:=s+chr(c);
  end;
  ShowMessage('ASCII of Output: '+s);
end;

procedure TCardiacForm.LoadDeckClick(Sender: TObject);
var
  i, bc: Integer;
  line: String;
begin
  if Deck.Lines.Text = '' then
  begin
    if OpenDialog.Execute = False then
      Exit;
    Deck.Lines.LoadFromFile(OpenDialog.FileName);
  end;
  Reader.Items.Clear;
  for i:=0 to Deck.Lines.Count-1 do
  begin
    line:=Deck.Lines.Strings[i];
    if Length(line) <> 3 then
    begin
      if Length(line) = 0 then
        Exit;
      ShowMessage('Line is not 3 character: '+line);
      Exit;
    end
    else if TryStrToInt(line, bc) = False then
    begin
      ShowMessage('Lines should only contain numbers: '+line);
      Exit;
    end;
    Reader.Items.Add(line);
  end;
end;

procedure TCardiacForm.ResetButtonClick(Sender: TObject);
begin
  ResetMemory;
end;

procedure TCardiacForm.RunCardiacClick(Sender: TObject);
begin
  if Running = True then
  begin
    Running:=False;
    Exit;
  end;
  RunCardiac.Caption:='Stop';
  StepCardiac.Enabled:=False;
  Output.Items.Clear;
  PC:=0;
  Running:=True;
  repeat
    ProcessOp;
    Application.ProcessMessages;
    if MaxSpeed.Checked = False then
      Sleep(100);
    if PC > 99 then
      Running:=False;
  until Running = False;
  RunCardiac.Caption:='Run';
  StepCardiac.Enabled:=True;
end;

procedure TCardiacForm.StepCardiacClick(Sender: TObject);
begin
  if PC > 99 then
    PC:=0;
  ProcessOp;
end;

procedure TCardiacForm.CreateMemory;
var
  i: Byte; // We don't need more than 100.
  row, col: Integer;
begin
  SetLength(Memory, 100);
  row:=10;
  col:=10;
  for i:=0 to 99 do
  begin
    Memory[i]:=TEdit.Create(MemoryGroup);
    Memory[i].SetBounds(col,row,60,24);
    Memory[i].ReadOnly:=True;
    Memory[i].Text:='';
    Memory[i].MaxLength:=3;
    Memory[i].Hint:=IntToStr(i);
    Memory[i].ShowHint:=True;
    Memory[i].Parent:=MemoryGroup;
    row:=row+25;
    if row > (25*10) then
    begin
      row:=10;
      col:=col+65;
    end;
  end;
end;

procedure TCardiacForm.ResetMemory;
begin
  Memory[0].Text:='001';
  Memory[99].Text:='8--';
  PC:=0;
  Running:=False;
end;

function TCardiacForm.GetInput: string;
var
  tmp: Integer;
begin
  if Reader.Count = 0 then
  begin
    repeat
      Result:=InputBox('Cardiac Manual Input','Please enter a number or string?','');
    until Result <> '';
    if TryStrToInt(Result, tmp) = True then
    begin
      Result:=RightStr('000'+Result,3);
      Exit;
    end;
    AddString(Result);
  end;
  Result:=Reader.Items.Strings[0];
  Reader.Items.Text:=RightStr(Reader.Items.Text, Length(Reader.Items.Text)-4);
end;

function TCardiacForm.GetMemory(addr: Byte): string;
begin
  Result:='000';
  if addr > 99 then
    Exit;
  Memory[addr].Color:=$ffffaa;
  Result:=Memory[addr].Text;
end;

function TCardiacForm.GetMemoryInt(addr: Byte): Integer;
begin
  if TryStrToInt(GetMemory(addr), Result) = False then
    Result:=0;
end;

procedure TCardiacForm.SetMemory(addr: Byte; value: Integer);
begin
  SetMemory(addr, IntToStr(value));
end;

procedure TCardiacForm.SetMemory(addr: Byte; value: String);
begin
  if addr > 99 then
    Exit;
  Memory[addr].Color:=$aaffff;
  Memory[addr].Text:=value;
end;

procedure TCardiacForm.ProcessOp;
var
  op, data, x, y, i: Byte;
  ir: Integer;
begin
  ir:=GetMemoryInt(PC);
  PC:=PC+1;
  op:=floor(ir div 100);
  data:=ir mod 100;
  case op of
    0: SetMemory(data, GetInput);
    1: Acc:=GetMemoryInt(data);
    2: Inc(Acc,GetMemoryInt(data));
    3: begin
      if Acc < 0 then
        PC:=data;
    end;
    4: begin
      x:=floor(data div 10);
      y:=data mod 10;
      for i:=0 to x do
        Acc:=(Acc * 10) mod 10000;
      for i:=0 to y do
        Acc:=floor(Acc div 10);
    end;
    5: Output.Items.Add(GetMemory(data));
    6: SetMemory(data, Acc);
    7: Dec(Acc, GetMemoryInt(data));
    8: begin
      if data <> 99 then
        SetMemory(99, PC+800); // Checking if data <> 99 is not standard.
      PC:=data;
    end;
    9: ResetMemory;
  end;
end;

procedure TCardiacForm.SetPC(const value: Byte);
begin
  if value > 99 then
    Exit;
  Memory[FPC].Color:=$FFFFFF;
  FPC:=value;
  Memory[FPC].Color:=$99FFCC;
end;

procedure TCardiacForm.AddString(const s: string);
var
  i: Integer;
  c: string;
begin
  for i:=1 to Length(s) do
  begin
    c:='000'+IntToStr(ord(s[i]));
    Reader.Items.Add(RightStr(c,3));
  end;
end;

end.

