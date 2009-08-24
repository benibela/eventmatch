import scala.io._

//Kann benutzt werden um jeden Schritt zu überwachen
trait  EventMatchNotifier{
  def initiated(logs:Logs){};
  def beginCrossChangePhase(){};
  def beginWipingPhase(){};
  def endAllPhases(){};
  def searchCrossChange(l:Log,i:Int){};
  def preCrossChange(l:Log,i:Int){};
  def wantToCrossChange(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int){};
  def crossChanged(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int){};
  def searchWipePlace(l:Log,i:Int,ltr:Boolean){};
}

class DefaultEventMatchNotifier extends Object with EventMatchNotifier{}

//Ein Log mit Ereignissen und Intervalvektoruhren
class Log(l: Int, n:Int){
  val length = n;
  var data=new Array[Int](n);
  var clocksMin=new Array[Array[Int]](n,l);
  var clocksMax=new Array[Array[Int]](n,l);
  var id:Int = -1;
  def apply (i: Int) = data(i);
  override def toString():String = {
    var sb= new StringBuilder();
	for (i <- 0 until length) {
	  sb.append(data(i).toString);
	  sb append "\t";
	}
	sb append "\n";
	for (j <- 0 until l){
	  for (i <- 0 until n) {
	    sb append clocksMin(i)(j).toString;
		sb append "-";
		sb append clocksMax(i)(j).toString;
		sb append "\t";
	  }
	  sb append "\n";
	}
	return sb.toString();
  }
  override def clone(): Log = {
	var newLog = new Log(l,n);
	newLog.id=id;
	newLog.data = data map {i=>i};
	newLog.clocksMin = clocksMin map {i => i map {j => j}};
	newLog.clocksMax = clocksMax map {i => i map {j => j}};
	return newLog;
  }
}

//Mehrere Logs
class Logs(_logs: Array[Log]){
	var logs=_logs;
	var maxS=0;

	for (l <- 0 until logs.length) {
		logs(l).data.foreach(d => if (d>maxS) maxS=d);
		logs(l).id = l;
	}
	
	//speichert für jeden Typ die id des sendenden Log
    var senderCache=new Array[Int](maxS+1);
    for (l <- 0 until logs.length)
      logs(l).data.filter(_>0).foreach( d => senderCache(d) = l);

	def apply(i:Int): Log = logs(i);
	def length: Int = logs.length;
	def foreach(a: (Log => Unit)) = logs.foreach(a);
	def senderLog(typ: Int): Log = logs(senderCache(typ)); //log das diesen Typ versendet
	def indices = logs.indices;
	//Nach der Vektoruhr letzter und erster Sender als Log/Ereignisid
	def lastSender(l:Int, i:Int): Tuple2[Log,Int] = new Tuple2(senderLog(-logs(l)(i)), logs(l).clocksMax(i)(senderCache(-logs(l)(i))));
	def firstSender(l:Int, i:Int): Tuple2[Log,Int] = new Tuple2(senderLog(-logs(l)(i)), logs(l).clocksMin(i)(senderCache(-logs(l)(i))));
	//Min/Max-Vektoruhr des letzten Senders
	def lastSenderMinClock(l:Int, i:Int): Array[Int] = lastSender(l,i)._1.clocksMin(lastSender(l,i)._2);
	def lastSenderMaxClock(l:Int, i:Int): Array[Int] = lastSender(l,i)._1.clocksMax(lastSender(l,i)._2);
	
	//Überprüft ob ein Kreuztausch nötig ist
	def crossChangeWithFirstNeeded(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int):Boolean = 
		(0 until logs.length).exists(j=>
			l.clocksMin(i)(j)<sl.clocksMin(sfirst)(j) ||
			sl.clocksMax(sfirst)(j)>l.clocksMax(i)(j)
		);	
	def crossChangeWithLastNeeded(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int):Boolean = 
		(0 until logs.length).exists(j=>
			l.clocksMax(i)(j)>sl.clocksMax(slast)(j) ||
			sl.clocksMin(slast)(j)<l.clocksMin(i)(j)
		);	
}

//Führt die Heuristik aus
class Matcher(){
  var notifier: EventMatchNotifier = new DefaultEventMatchNotifier(); //default does nothing
  def matchEvents(logs: Logs){
	var debugTotalCrossChanges=0;
	var debugTotalWipes=0;

	  //Überprüft ob sich die Intervalvektoruhren der beiden Ereignisse in allen Koordinaten schneiden
	  def intersect(l1:Log,i1:Int,l2: Log,i2:Int): Boolean = {
		for (j <- 0 until l1.clocksMin(i1).length)
		  if (l1.clocksMin(i1)(j)>l2.clocksMax(i2)(j) || l2.clocksMin(i2)(j)>l1.clocksMax(i1)(j))
			return false;
		return true;
	  }

	  //Führt einen Kreuztausch aus und gibt zurück, ob die Uhren geändert wurden
	  def crossChanged(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int):Boolean = {
		var result=false;
		var L = l.clocksMin(i).length;
		notifier.wantToCrossChange(l,i,sl,sfirst,slast);
		for (j <- 0 until L) {
		  if (l.clocksMin(i)(j)<sl.clocksMin(sfirst)(j)) {
			result=true;
			l.clocksMin(i)(j)=sl.clocksMin(sfirst)(j);
		  }
		  if (l.clocksMax(i)(j)>sl.clocksMax(slast)(j)) {
			result=true;
			l.clocksMax(i)(j)=sl.clocksMax(slast)(j);
		  }

		  if (sl.clocksMax(sfirst)(j)>l.clocksMax(i)(j)){
			result=true;
			sl.clocksMax(sfirst)(j)=l.clocksMax(i)(j);
		  }
		  if (sl.clocksMin(slast)(j)<l.clocksMin(i)(j)){
			result=true;
			sl.clocksMin(slast)(j)=l.clocksMin(i)(j);
		  }
		}
		if (result) notifier.crossChanged(l,i,sl,sfirst,slast);
		return result;
	  }

   val L=logs.length;
	
	//Initialisiert Intervallvektoruhren
    //logs.foreach(println);  
    for (l <- 0 until L) 
      for (i <- 0 until logs(l).length){
	    for (j <- 0 until L) logs(l).clocksMin(i)(j) = 0; 
		for (j <- 0 until L) logs(l).clocksMax(i)(j) = logs(j).length-1; 
        logs(l).clocksMin(i)(l)=i;
		logs(l).clocksMax(i)(l)=i;
	  }
	  
	//logs.foreach(println);  
	var changed=true;
	notifier.initiated(logs);
	//Heuristikschleife
    while (changed) {
      changed=false;
	  notifier.beginCrossChangePhase();
      //crosschanges
      logs.foreach (log => {
        for (j <- 0 until log.length) {
		  notifier.searchCrossChange(log,j);
          if (log(j)<0) {
            val s = - log(j);
			//erstes/letztes sendeereignis suchen
            val slog=logs.senderLog(s);
            var sfirst=slog.length;
			var slast=0;
            for (k <- 0 until slog.length)
			  if (slog(k)==s && intersect(log,j,slog,k)) {
			    sfirst=Math.min(sfirst,k);
			    slast=Math.max(sfirst,k);
			  }
            if (slast<sfirst) throw new Exception("Impossible connection");
			//kreuztauschen
			if (crossChanged(log,j,slog,sfirst,slast)) {
				changed=true;
				debugTotalCrossChanges+=1;
				logs.foreach(println);  
			}
          }
		}
      });

	  //wipings
      if (changed) {
	    notifier.beginWipingPhase();
        //wipe ltr
		logs.foreach (log => {
		  for (i <- 1 until log.length) {
		    notifier.searchWipePlace(log,i,true);
		    for (j <- 0 until logs.length){
			  if (log.clocksMin(i-1)(j) > log.clocksMin(i)(j)){
			    changed=true;
				log.clocksMin(i)(j) = log.clocksMin(i-1)(j);
			  }
			}
		  }
		});
        //wipe rtl
		logs.foreach (log => {
		  for (i <- (1 until log.length).reverse) {
		    notifier.searchWipePlace(log,i,false);
		    for (j <- 0 until logs.length)
			  if (log.clocksMax(i-1)(j) > log.clocksMax(i)(j)){
			    changed=true;
				log.clocksMax(i-1)(j) = log.clocksMax(i)(j);
			  }
			}
		});
		
		notifier.endAllPhases();
		
		println("wiped:");
		debugTotalWipes+=1;
		logs.foreach(println);  
		
      /*writeln('wiped:');
      writeState(data,clocksMin,clocksMax);
      debugTotalWipes+=1;*/
      }
	}
	notifier.endAllPhases();
    println("debugTotalCrossChanges: "+debugTotalCrossChanges.toString());
    println("debugTotalWipes: "+debugTotalWipes.toString());
  }	  
}					

//List die Logs von stdin in 2D-Array
class BaseLogReader (lines: List[String]) {
	val nl = lines(0).trim().toInt;
	val stdinLines = lines.drop(1);
	val data=stdinLines.map ( line =>
		line.trim() map ( c => //.split("[ \t]").filter(w => w.length>0 && w(0) > 13);
		if (c >= 'A' && c <= 'Z') c-'A'+1
		else if (c >= 'a' && c <= 'z') -c+'a'-1
		else throw new Exception ("Unknown word: "+c)
	))
	val maxS = data.foldLeft(0)( (a,b) => Math.max(a,b.foldLeft(0)(Math.max(_,_))));
    var senderCache=new Array[Int](maxS+1); //sendertyp => log map
    for (l <- 0 until nl)
      data(l).filter(_>0).foreach( d => senderCache(d) = l);
	def length(i:Int)=data(i).length;
	def apply (i: Int) = data(i);
}


//List die Logs in Logobjekte
class LogReader (lines: List[String]) extends BaseLogReader(lines) {	
	var logs = new Array[Log](nl);
	for (i <- 0 until nl) {
		logs(i) = new Log(stdinLines.length,data(i).length);
		for (j <- 0 until data(i).length) 
			logs(i).data(j) = data(i)(j);
	}
}

//Ausführbares Programm
object EventMatcher {
  def main(args: Array[String]) {
	val matcher = new Matcher();
	val logs = new Logs(new LogReader(Source.fromInputStream(System.in).getLines.toList).logs);
	matcher.matchEvents(logs);
	logs.foreach(println);
  }
}
