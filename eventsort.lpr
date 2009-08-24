program eventSort;

{$mode objfpc}{$H+}

uses sysutils, sortfunc;

var data:array of array of longint;//array von logs, jedes ein array von ereignisse (<0 empfänger, >0 sender)
    cMin,cMax:array of array of array of longint;
    s:string;
    i,j,n: Integer;
begin
  debugVerbose:=paramcount()>0;
  //einlesen
  readln(n);
  setlength(data,n);
  for i:=0 to n-1 do begin
    readln(s);
    s:=Trim(s);
    setlength(data[i],length(s));
    for j:=1 to length(s) do begin
      if s[j] in ['A'..'Z'] then data[i,j-1]:=ord(s[j])-ord('A')+1
      else if s[j] in ['a'..'z'] then data[i,j-1]:=-(ord(s[j])-ord('a')+1)
      else raise exception.create('Invalid character '+s[j]+' in '+s);
    end;
  end;
  //heuristik ausführen
  sort(data,cMin,cMax);
  //ausgeben
  writeln(n);
  WriteState(data,cMin,cMax);
  if debugVerbose then begin
    writeln(stderr,'debugTotalCrossChanges:',debugTotalCrossChanges);
    writeln(stderr,'debugTotalWipes (=repeats):',debugTotalWipes);
  end;
end.


Form:
2
AAAAAAAAAAAAAAAA... (infinity)
aaaa... (n)

n | crosschanges
2:3    2
3:6   ,3
4:10
5:15  ,5
6:21
=> 0.5n(n+1)  cc
   n          w

------------
ABCDE... (inf)
abcde... (n)
1:0
2:2
3:3
4:4
5:5
=>n crosschanges
  1 wipe
