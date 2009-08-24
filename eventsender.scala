object EventSender{
  def main(args: Array[String]) {
	if (args.length==0) {
		println ("ARGS: <total logs> <packet types> <total send event> [options]");
		println ("OPTIONS: --no-partial-receiving: if every send event is received by all");
	}
	//read args
    val intArgs = args filter (strArg => !strArg.startsWith("-")) map Integer.parseInt;
	val logN = intArgs(0);
	val packetTypes = intArgs(1);
	val totalSendEvents = intArgs(2);
	val noPartialReceiving = args.contains("--no-partial-receiving");
	
	var logs = new Array[StringBuffer](logN) map {ignore => new StringBuffer()};
	var random = new Random();
	var typeSendeMap = new Array[Int](packetTypes) map {ignore => random.nextInt(logN)}; //random sender for every type
	//randomly send types from the logs which are received by random logs
	for (i <- 0 to totalSendEvents) {
	  val typ=random.nextInt(packetTypes); //random type
	  val sender:Int=typeSendeMap(typ);
	  logs(sender).append(('A'+typ).toChar);
	  if (!noPartialReceiving || random.nextBoolean()) 
	    for (i <- 0 until logN) 
		  if (i != sender && (noPartialReceiving || random.nextBoolean())) //random receive
		    logs(i).append(('a'+typ).toChar);
	}
	//output
	println(logN);
	for (i <- 0 until logN) 
	  println(logs(i));
  }
}