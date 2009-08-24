program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,sysutils
  { you can add units after this };

{$IFDEF WINDOWS}{$R eventgenerator.rc}{$ENDIF}

var
  logN: LongInt;
  packetTypes: LongInt;
  totalSendEvents: LongInt;
  prefix: string;
  senderCache: array of longint;
  //alle möglichen Zeitabläufe werden durchprobiert, aktueller Zeitablauf in time:
  time: array of record
    event: longint; //event art
    receiver: longint;  //ein kleines bitfeld, das die empfangenen logs speichert (nutzt nur logN-1 bits, senderlog nicht enthalten)
  end;
  output: array of string; //berechnete ausgabe für jedes log
  strlength: array of longint;
  i: Integer;
  t: Integer;
  maxreceiver: longint;
  sl: LongInt;
  rec:longint;
  outfile: TextFile;
  sendTypUsed: array of boolean; //has this type of packet been used?
  lastLogTime:array of longint;//for every log, last time of receiving/sending events
  lastSendTime: array of longint; //for every packet type the last time of sending
  skipit: boolean; //created log would be duplicate
begin
  if Paramcount()<4 then begin
    writeln('ARGS: <total logs> <packet types> <total send event> <prefix> [options]');
		//writeln('OPTIONS: (todo:) --no-partial-receiving');
    exit;
  end;
  logN :=strtoint(ParamStr(1));
  packetTypes :=strtoint(ParamStr(2));
	totalSendEvents :=strtoint(ParamStr(3));
  if packetTypes>totalSendEvents then packetTypes:=totalSendEvents; //optimization/remove duplicates
	prefix :=ParamStr(4);
	//noPartialReceiving = args.contains("--no-partial-receiving");

	setlength(senderCache,packetTypes);
  for i:=0 to packetTypes-1 do senderCache[i]:=0;
  SetLength(time, totalSendEvents);
  maxreceiver:=1 shl (logN-1)-1;
  SetLength(output, logN);
  for i:=0 to high(output) do SetLength(output[i],totalSendEvents+1);
  setlength(sendTypUsed,packetTypes);
  SetLength(strlength, logN);
  SetLength(lastLogTime, logN);
  SetLength(lastSendTime, packetTypes);
  //backtracking schleife über alle möglichen typen<->sender zuordnungen
  while (senderCache[0]=0) do begin //having the first sender in the first log removes swapping duplicates
    FillChar(time[0],sizeof(time[0])*length(time),0);
	//backtracking schleife, über alle möglichen zeitabläufe
    while (time[0].event=0) do begin
      //gen output
      skipit:=false;
      FillChar(sendTypUsed[0],SizeOf(sendTypUsed[0])*length(sendTypUsed),0);
      for t:=0 to totalSendEvents-1 do
        sendTypUsed[time[t].event]:=true;
      for i:=0 to packetTypes-1 do
        if not sendTypUsed[i] and (senderCache[i]<>0) then begin
          skipit:=true; //not used sender typ must be in log 0, otherwise duplicates are created
          break;
        end;
      if not skipit then begin
        FillChar(strlength[0],SizeOf(strlength[0])*length(strlength),0);
        FillChar(lastLogTime[0],sizeof(lastLogTime[0])*length(lastLogTime),$FF);
        FillChar(lastSendTime[0],sizeof(lastSendTime[0])*length(lastSendTime),$FF);
		//Ausgabe berechnen, wenn es sich nicht als Duplikat herausstellt (verlange eine gewisse ordnung)
        for t:=0 to totalSendEvents-1 do begin //sende alle ereignisse im zeitablauf
          sl:=senderCache[time[t].event];
          strlength[sl]+=1;
          output[sl][strlength[sl]]:=chr(time[t].event+ord('A')); //zur senderlogausgabe hinzufügen
          rec:=time[t].receiver;
		  //erste hälfte der empfänger vor dem sendelog
          for i:=0 to sl-1 do
            if (1 shl i) and rec<>0 then begin
              if lastLogTime[i] < lastSendTime[time[t].event] then begin
                skipit:=true;//das ereignis wurde zweimal gesendet und nur einmal empfangen
                break;       //erster empfang ununterscheidbar vom zweiten => einen streichen
              end;
			  //wurde empfangen
              strlength[i]+=1;
              output[i][strlength[i]]:=chr(time[t].event+ord('a'));
              lastLogTime[i]:=t;
            end;
		  //zweite hälfte der empfänger nach dem sendelog
          rec:=rec shl 1;
          for i:=sl+1 to logN-1 do
            if (1 shl i) and rec<>0 then begin
              if lastLogTime[i] < lastSendTime[time[t].event] then begin
                skipit:=true;
                break;
              end;
              strlength[i]+=1;
              output[i][strlength[i]]:=chr(time[t].event+ord('a'));
              lastLogTime[i]:=t;
            end;
          if skipit then break;
          lastSendTime[time[t].event]:=t;
        end;
		//ausgeben
        if not skipit then begin
          outfile:=StdOut;
          for i:=0 to logN-1 do begin
            output[i][strlength[i]+1]:=#0;
            writeln(outfile,pchar(output[i]));
          end;
          writeln();
        end;
      end;
	  //nächster empfängerkombination => nächster zeitablauf
      time[totalSendEvents-1].receiver+=1;
      if time[totalSendEvents-1].receiver>maxreceiver then begin
	    //nächstes ereignis, empfänger 0
        time[totalSendEvents-1].receiver:=0;
        time[totalSendEvents-1].event+=1; 
        t:=totalSendEvents-1;
        while (time[t].event>=packetTypes)
              or (time[t].event>t) do begin //time[t].event <= t remove some renaming duplicates
          if t <= 0 then break;
          time[t].event:=0;
          t-=1;
          time[t].receiver+=1;
          if time[t].receiver>maxreceiver then begin
            time[t].event+=1;
            time[t].receiver:=0;
          end;
        end;
    //    if t<0 then t:=0;
//        for t:=t+1 to totalSendEvents-1 do
  //        if time[t].event<time[t-1].event then time[t].event:=time[t-1].event;
      end;
    end;
	//nächste mögliche senderzuordnung
	senderCache[packetTypes-1]+=1;
    for i:=packetTypes-1 downto 1 do
      if (senderCache[i]>=logN) or (senderCache[i]>i) then begin
        senderCache[i]:=0;
        senderCache[i-1]+=1;
      end else break;
	//senderzuordnen für debugzwecke ausgeben
    write('New Sender: ');
    for i:=0 to packetTypes-1 do
      write(chr(ord('A')+i),':',senderCache[i],'  ');
    WriteLn();
  end;
end.

