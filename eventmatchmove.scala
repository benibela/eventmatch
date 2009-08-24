import collection._;
import io._;
class MoveMatcher(){
	var order:List[Tuple2[Int,Int]]=Nil;
	private def moveEventLeftToRightSide(from:Int,  to:Int){
	println("melrs"+from+"->"+to);
		//LLLLLLTXXXSXSXSXSXXXXFRRRRRR => LLLLLLSSSSTFXXXXXXXXXXRRRRRR | Same log, From, To, Left, Right
		val fromEvent = order(from);
		val toEvent = order(to);
		val unchangedLeft = order.slice(0,to);
		val unchangedRight = order.slice(from+1,order.length+1);
		val totalMid = order.slice(to+1,from);
		order = unchangedLeft ::: totalMid.filter(i=>i._2 == fromEvent._2) ::: List(toEvent, fromEvent) ::: 
		                          totalMid.filter(i=>i._2 != fromEvent._2) ::: unchangedRight;
	}
	private def moveEventRightToLeftSide(from:Int,  to:Int){
	println("merls"+from+"->"+to);
		//LLLLLLFXXXSXSXSXSXXXXTRRRRRR => LLLLLLXXXXXXXXXXFTSSSSRRRRRR | Same log, From, To, Left, Right
		val fromEvent = order(from);
		val toEvent = order(to);
		val unchangedLeft = order.slice(0,from);
		val unchangedRight = order.slice(to+1,order.length+1);
		val totalMid = order.slice(from+1,to);
		order = unchangedLeft ::: totalMid.filter(i=>i._2 != fromEvent._2) ::: List(fromEvent,toEvent) ::: 
		                          totalMid.filter(i=>i._2 == fromEvent._2) ::: unchangedRight;
	}
	private def moveEventRightToRightSide(from:Int,  to:Int){
	println("merrs"+from+"->"+to);
		//LLLLLLFXXXSXSXSXSXXXXTRRRRRR => LLLLLLXXXXXXXXXXTFSSSSRRRRRR | Same log, From, To, Left, Right
		val fromEvent = order(from);
		val toEvent = order(to);
		val unchangedLeft = order.slice(0,from);
		val unchangedRight = order.slice(to+1,order.length+1);
		val totalMid = order.slice(from+1,to);
		order = unchangedLeft ::: totalMid.filter(i=>i._2 != fromEvent._2) ::: List(toEvent,fromEvent) ::: 
		                          totalMid.filter(i=>i._2 == fromEvent._2) ::: unchangedRight;
	}
	private def findLeftMatch(i:Int):Int={
		var j=i;
		var noRival = true;
		var leftestMatch = -1;
		while (j>0 && noRival){
			j-=1;
			if (order(i)._2 == order(j)._2) {
				if (order(j)._1 < 0) noRival=false; //can't move over rec
				else if (order(j + 1)._1 < 0 && j+1!=i) //rec-ev must be assigned to sender=>can't move over sen
					noRival = false;
			} else if (order(i)._1 == -order(j)._1) leftestMatch=j;//noMatch=false;
		}
		return leftestMatch;
	}
	def matchEvents(logs: BaseLogReader){
		for (i <- 0 until logs.nl){
			for (j<- 0 until logs.length(i)) 
				order=order ::: List(new Tuple2[Int,Int](logs(i)(j),i));
		}
		
		var complete=false;
		var changeLog: List[List[Tuple2[Int,Int]]]=Nil; 
		while (!complete){
			var lastType = 0; //0: none, type=-1..-inf typ -type
			var lastTypeId = -1;
			var receivingLogs=new mutable.HashSet[Int]();
			var i=0;
			var noChange=true;
			while (i < order.length && noChange)	{
				if (order(i)._1 > 0){
					lastType = -order(i)._1;
					lastTypeId = i;
					receivingLogs.clear();
				} else if (order(i)._1 < 0){
					if (order(i)._1 == lastType && !receivingLogs.contains(order(i)._2)) {
						receivingLogs+= order(i)._2;
						var leftMatch = findLeftMatch(i);
						if (leftMatch!=lastTypeId) 
							moveEventLeftToRightSide(i, leftMatch);
					} else { //found receive event without matching
						noChange=false;
						var leftMatch = findLeftMatch(i);
						if (leftMatch!= -1){ 
							//found possible sender, assign (move to the left)
							moveEventLeftToRightSide(i, leftMatch);
							//.filter( t => t._2 == sl);
						} else {
							val rSenderId = order.drop(i+1).findIndexOf(t=> - t._1 == order(i)._1); //search sender on right side
							if (rSenderId!= -1) {
								val senderId = rSenderId + i+1;
								moveEventRightToRightSide(i,senderId);
							} else {
								var best= -1; //why is there no findLastIndexOf????
								for (k <- 0 until i) 
									if (order(k)._1 == -order(i)._1) 
										best=k;
								if (best== -1) throw new Exception("No matching sender");
								moveEventRightToLeftSide(best, i);
							}
						}
/*								var canBeUsed=false;
								j+=1;
								while (j< order.length && order(i)._1==order(j)._i && canBeUsed )
									if (order(i)._2 == order[2]._2)  
							} else j+=1;
						}
						for (j <- 0 until order.length) {
							
						}*/
					}
				}
				i+=1;
			}
			complete=noChange;
			println(order map (t => (if (t._1 < 0) - t._1+'a'-1 else t._1+'A'-1).toChar +" " + (t._2 +1)) mkString(" "));
			if (!complete){
				if (changeLog.contains(order)) throw new Exception("Looping!!! step "+changeLog.indexOf(order)+" to "+changeLog.length);
				changeLog=changeLog:::List(order);
			}
		}
	}
}

object EventMatcherMove {
  def main(args: Array[String]) {
	val matcher = new MoveMatcher();
	val logs = new BaseLogReader(Source.fromInputStream(System.in).getLines.toList);
	matcher.matchEvents(logs);
	//logs.foreach(println);
  }
}
