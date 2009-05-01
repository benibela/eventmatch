unit sortfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;
type TInData = array of array of longint;
     TOutData =array of array of array of longint;
procedure writeState(const data: TInData; const cMin,cMax: TOutData;
                    starMarkL:longint=-1;starMarkI:longint=-1;
                    leftMarkL:longint=-1;leftMarkI:longint=-1;
                    rightMarkL:longint=-1;rightMarkI:longint=-1);

procedure sort(const data: TInData;out clocksMin,clocksMax: TOutData);

var debugTotalCrossChanges:longint=0;
    debugTotalWipes:longint=0;
implementation

function intersect(min1,max1,min2,max2:array of longint):boolean;
var i:longint;
begin
  result:=true;
  for i:=0 to high(min1) do
    result:=result and (min1[i]<=max2[i]) and (min2[i]<=max1[i]);
end;

procedure writeState(const data: TInData; const cMin,cMax: TOutData;
                    starMarkL:longint=-1;starMarkI:longint=-1;
                    leftMarkL:longint=-1;leftMarkI:longint=-1;
                    rightMarkL:longint=-1;rightMarkI:longint=-1);
var i,j,k:longint;
begin
  for i:=0 to high(data) do begin
    for j:=0 to high(data[i]) do begin
      if (i=starMarkL) and (j=starMarkI) then write('*');
      if (i=leftMarkL) and (j=leftMarkI) then write('>');
      if data[i,j]>0 then write(chr(ord('A')+data[i,j]-1))
      else write(chr(ord('a')-data[i,j]-1));
      if (i=starMarkL) and (j=starMarkI) then write('*');
      if (i=rightMarkL) and (j=rightMarkI) then write('<');
      write(#9);
    end;
    writeln;
    for k:=0 to high(data) do begin
      for j:=0 to high(data[i]) do
        write(cMin[i,j,k],'-',cMax[i,j,k],#9);
      writeln;
    end;
  end;
  writeln;
end;

procedure sort(const data: TInData; out clocksMin,clocksMax: TOutData);
  function crossChanged(l,i,slog,sfirst,slast:longint):boolean;
  var j:longint;
  begin
    result:=false;
    for j:=0 to high(data) do begin
      if clocksMin[l,i,j]<clocksMin[slog,sfirst,j] then begin
        result:=true;
        clocksMin[l,i,j]:=clocksMin[slog,sfirst,j];
      end;
      if clocksMax[l,i,j]>clocksMax[slog,slast,j] then begin
        result:=true;
        clocksMax[l,i,j]:=clocksMax[slog,slast,j];
      end;

      if clocksMax[slog,sfirst,j]>clocksMax[l,i,j] then begin
        result:=true;
        clocksMax[slog,sfirst,j]:=clocksMax[l,i,j];
      end;
      if clocksMin[slog,slast,j]<clocksMin[l,i,j] then begin
        result:=true;
        clocksMin[slog,slast,j]:=clocksMin[l,i,j];
      end;
    end;
  end;

var
    senderCache: array of longint; //sender=>log
    i,j,k,l,s,slog,sfirst,slast,maxS:longint;
    changed:boolean;
begin
  setlength(clocksMin,length(data));
  setlength(clocksMax,length(data));
  debugTotalCrossChanges:=0;
  debugTotalWipes:=0;
  for i:=0 to high(clocksMin) do begin
    setlength(clocksMin[i],length(data[i]),length(data));
    setlength(clocksMax[i],length(data[i]),length(data));
    for j:=0 to high(clocksMin[i]) do begin
      for k:=0 to high(clocksMin) do begin
        clocksMin[i,j,k]:=0;
        clocksMax[i,j,k]:=high(data[k]);
      end;
      clocksMin[i,j,i]:=j;
      clocksMax[i,j,i]:=j;
    end;
  end;
  maxS:=0;
  for l:=0 to high(data) do
    maxS:=max(maxvalue(data[l]),maxS);
  setlength(senderCache,maxS+1);
  for l:=0 to high(data) do
    for i:=0 to high(data[l]) do
      if data[l,i]>0 then senderCache[data[l,i]]:=l;
  writeState(data,clocksMin,clocksMax);
  repeat
    changed:=false;
    //crosschange
    for l:=0 to high(data) do
      for i:=0 to high(data[l]) do begin
        if data[l,i]<0 then begin
          s:=-data[l,i];
          slog:=senderCache[s];
          sfirst:=high(senderCache[s]);
          for j:=0 to high(data[slog]) do
            if (data[slog,j]=s) and intersect(clocksMin[l,i],clocksMax[l,i],clocksMin[slog,j],clocksMax[slog,j]) then begin
              sfirst:=min(sfirst,j);
              slast:=max(sfirst,j);
            end;
          if slast<sfirst then raise Exception.Create('Impossible connection');
          if crossChanged(l,i,slog,sfirst,slast) then begin
            changed:=true;
            writeln('crosschanged: ',l,' ',i,' ',slog,' ',sfirst,' ',slast);
            writeState(data,clocksMin,clocksMax,l,i,slog,sfirst,slog,slast);
            debugTotalCrossChanges+=1;
          end;
        end;
      end;
    //wiping
    if changed then begin
      for l:=0 to high(data) do begin
        //wipe ltr
        for i:=0 to high(data[l])-1 do
          for j:=0 to high(clocksMin[l,i]) do
            if clocksMin[l,i,j]>clocksMin[l,i+1,j] then begin
              changed:=true;
              clocksMin[l,i+1,j]:=clocksMin[l,i,j];
            end;
        //wipe rtl
        for i:=high(data[l])-1 downto 0 do
          for j:=0 to high(clocksMin[l,i]) do
            if clocksMax[l,i,j]>clocksMax[l,i+1,j] then begin
              changed:=true;
              clocksMax[l,i,j]:=clocksMax[l,i+1,j];
            end;
      end;
      writeln('wiped:');
      writeState(data,clocksMin,clocksMax);
      debugTotalWipes+=1;
    end;
  until not changed;
end;

end.

