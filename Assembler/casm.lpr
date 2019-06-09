program casm;

{$mode objfpc}{$H+}

uses strutils, Classes, sysutils;

const
  BOOTLOADER: Array[0..9] of String = ('001','189','200','689','198','700','698','301','889','SIZE');

var
  addr, pc, size: Integer;
  buffer: TStringList;

function getString(var src: string; symb: char): string;
begin
  src:=RightStr(src, Length(src)-1);
  Result:=Copy2SymbDel(src, symb);
end;

function getToken(var src: string): string;
begin
  if src[1] = '"' then
    Result:=getString(src, '"')
  else
    Result:=Copy2SpaceDel(src);
end;

function generateOpMap: TStringList;
begin
  Result:=TStringList.Create;
  Result.DelimitedText:='inp,cla,add,tac,sft,out,sto,sub,jmp,hrs';
end;

function ptr: Integer;
begin
  if addr > -1 then
    Result:=addr
  else
    Result:=pc;
end;

procedure writeCards(card_list: Array of String);
var
  i: Integer;
begin
  for i:=0 to Length(card_list)-1 do
    buffer.Add(card_list[i]);
  pc:=pc+Length(card_list);
end;

procedure writeAddr;
var
  a: String;
begin
  if addr > -1 then
  begin
    a:='000'+IntToStr(addr);
    writeCards([a]);
    Inc(addr);
  end;
  Inc(size);
end;

var
  sfile, op_map, var_cards, labels, ofile: TStringList;
  i, opcode: Integer;
  line, op, param, param2, start: String;
  value, a: Byte;
  final: Boolean;

begin
  if ParamCount = 0 then
  begin
    WriteLn('Missing file to assemble.');
    Halt(1);
  end;
  final:=False;
  op_map:=generateOpMap;
  buffer:=TStringList.Create;
  buffer.Delimiter:=#10;
  sfile:=TStringList.Create;
  sfile.LoadFromFile(ParamStr(1));
  var_cards:=TStringList.Create;
  labels:=TStringList.Create;
  start:='-1';
  addr:=-1;
  pc:=0;
  size:=0;
  for i:=0 to sfile.Count-1 do
  begin
    param:='';
    line:=sfile.Strings[i];
    if (line = '') or (line[1] = '#') then
      Continue;
    op:=lowercase(getToken(line));
    if op = 'var' then
    begin
      param:=getToken(line);
      if getToken(line) <> '=' then
      begin
        WriteLn('Invalid var usage: ',sfile.Strings[i]);
        Break;
      end;
      param2:=getToken(line);
      if param2[1] = '*' then
        var_cards.Add(param+'='+param2)
      else
        var_cards.Add(param+'=000'+param2);
    end
    else if op = 'label' then
    begin
      param:=getToken(line);
      if labels.IndexOf(param) > -1 then
      begin
        WriteLn('Label has been declared twice: ',param);
        Break;
      end;
      labels.Add(param+'=00'+IntToStr(ptr));
    end
    else if op = 'bootstrap' then
    begin
      addr:=3;
      start:='SET';
      writeCards(['002','800']);
      pc:=3;
    end
    else if op = 'bootloader' then
    begin
      writeCards(['002','800']);
      a:=89;
      for value:=0 to 9 do
      begin
        writeCards(['0'+IntToStr(a), BOOTLOADER[value]]);
        Inc(a);
      end;
      writeCards(['002','889']);
      pc:=1;
    end
    else if op = 'data' then
    begin
      param:=getToken(line);
      repeat
        param2:=Copy2SymbDel(param, ',');
        writeAddr;
        writeCards([RightStr('000'+param2,3)]);
      until param = '';
    end
    else if op = 'rts' then
    begin
      writeAddr;
      writeCards(['899']);
    end
    else if op = 'end' then
      final:=True;
    opcode:=op_map.IndexOf(op);
    if opcode > -1 then
    begin
      param:='00';
      if line <> '' then
      begin
        param:=getToken(line);
        if param[1] = '$' then
          param:='*'+RightStr(param, Length(param)-1);
      end;
      if start = 'SET' then
        start:=IntToStr(opcode)+param
      else
      begin
        writeAddr;
        writeCards([IntToStr(opcode)+param]);
      end;
    end;
  end;
  if final = True then
  begin
    ofile:=TStringList.Create;
    ofile.Delimiter:=#10;
    for i:=0 to var_cards.Count-1 do
    begin
      param:='000'+IntToStr(ptr);
      writeAddr;
      op:=var_cards.ValueFromIndex[i];
      if op[1] <> '*' then
        writeCards([RightStr(op,3)])
      else
        writeCards(['1'+op]);
      labels.Add(var_cards.Names[i]+'='+RightStr(param,3));
    end;
    if start <> '-1' then
      writeCards(['002',start]);
    for i:=0 to buffer.Count-1 do
    begin
      op:=buffer.Strings[i];
      if op[2] = '*' then
      begin
        param:=RightStr(op,Length(op)-2);
        ofile.Add(op[1]+RightStr(labels.Values[param],2));
      end
      else if RightStr(op,4) = 'SIZE' then
      begin
        param:='000'+IntToStr(size-1);
        ofile.Add(RightStr(param,3));
      end
      else
        if Length(op) = 1 then
          ofile.Add(op+'00')
        else if Length(op) = 2 then
          ofile.Add(op[1]+'0'+op[2])
        else
          ofile.Add(RightStr('000'+op,3));
    end;
    for i:=0 to ofile.Count-1 do
      WriteLn(ofile.Strings[i]);
    ofile.Free;
  end;
  var_cards.Free;
  labels.Free;
  sfile.Free;
  op_map.Free;
  buffer.Free;
end.

