import java.awt._;
import java.awt.event._;
import scala.io._
//class EventMatchCanvas extends java.awt.Canvas{
//}
//GUI  Fenster
class EventMatchDialog (var logs: Logs) extends javax.swing.JFrame{
	private val canvas = new EventMatchCanvas(logs);
	private val matcher = new Matcher();
	matcher.notifier=canvas;
	
	getContentPane().setLayout(new GridBagLayout()); 
	private val button = new javax.swing.JButton("start");
	button.addActionListener(new ActionListener{def actionPerformed(e:ActionEvent ){
		if (veryfastCB.isSelected) canvas.mode=2;
		else if (fastCB.isSelected) canvas.mode=1;
		else canvas.mode=0;
		matcher.matchEvents(logs);
		repaint();
	}});
	private val fastCB = new javax.swing.JCheckBox("fast");
	private val veryfastCB = new javax.swing.JCheckBox("very fast");
	private val latexexport = new javax.swing.JButton("latex export");
	latexexport.addActionListener(new ActionListener{def actionPerformed(e:ActionEvent ){
		val latexpainter=new LatexGraphics(canvas.getWidth(),canvas.getHeight());
		canvas.paintLogs(latexpainter,canvas.logs);
		println(latexpainter.getLatex());
	}});
	def gridBag(x:Int, y:Int):GridBagConstraints={ var gbc=new GridBagConstraints(); gbc.gridx=x; gbc.gridy=y; return gbc; }
	getContentPane().add(button, gridBag(0,0));
	getContentPane().add(fastCB, gridBag(1,0));
	getContentPane().add(veryfastCB, gridBag(2,0));
	getContentPane().add(latexexport, gridBag(3,0));
	var temp=gridBag(0,1); temp.gridwidth=4;temp.weightx=0.9;temp.weighty=0.9;temp.fill=GridBagConstraints.BOTH;
	getContentPane().add(canvas, temp);
    setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
    pack();
	setVisible(true);
}

//Zeigt live den animierten Ablauf der Heuristik
class EventMatchCanvas(var logs: Logs) extends javax.swing.JComponent with EventMatchNotifier{
	
	println("Using logs:");
	logs.foreach(println);
	this.setIgnoreRepaint(true);
    this.setPreferredSize(new Dimension(800, 400));

	var mode=0;
	
	private var paintHighlightClock1=new Tuple2[Log,Int](null,-1);
	private var paintHighlightClock2=new Tuple2[Log,Int](null,-1);
	private var paintHighlightClock3=new Tuple2[Log,Int](null,-1);
	private var paintHighlightLine=new Tuple2[Log,Float](null,0);
	private var paintHighlightLog: Float = -1;
	private var paintCrossChangeProgress = 0.0f; //-1 .. <0 for first , >0 .. 1 for last
	private var highlightLineMode = -1;  //0: cc, 1: wipe ltr, 2: wipe rtl
	
	//neuzeichnen und warten
	private def paintStep(){paintStep(500);}
	private def paintStep(waitTime:Int){
		if (mode!=2) paintImmediately(0,0,getWidth(),getHeight());
		//val graph = this.getGraphics().asInstanceOf[Graphics2D];
		//paintAll(graph);
	//	graph.dispose;
		if (mode==0) Thread.sleep(waitTime);
		//repaint();
		//Toolkit.getDefaultToolkit().sync();  // sync the display on some systems
	}
	def highlightLog(id:Int, left: Boolean){
		if (Math.abs(id-paintHighlightLog)<0.001) return;
		if (paintHighlightLog>id) {
			while (paintHighlightLog>id) {
				paintHighlightLog-=0.05f;
				paintStep(20);
			}
		} else while (paintHighlightLog<id) {
			paintHighlightLog+=0.05f;
			paintStep(20);
		}
	}
	
	override def initiated(logs:Logs){};
	override def beginCrossChangePhase(){};
	override def beginWipingPhase(){
		paintHighlightClock1=new Tuple2(null,0);
		paintHighlightClock2=new Tuple2(null,0);
		paintHighlightClock3=new Tuple2(null,0);
		highlightLineMode= -1;
		paintStep(0);
	};
	override def endAllPhases(){
		paintHighlightClock1=new Tuple2(null,0);
		paintHighlightClock2=new Tuple2(null,0);
		paintHighlightClock3=new Tuple2(null,0);
		paintHighlightLine= new Tuple2(null,0);
		highlightLineMode= -1;
		paintHighlightLog= -1;
		paintStep(500);
	};
	override def searchCrossChange(l:Log,i:Int){
		if ((highlightLineMode==0) && paintHighlightLine._1 == l && paintHighlightLine._2 < i) {
			while (paintHighlightLine._2+0.001f<i) {
				paintHighlightLine= new Tuple2[Log,Float](l,paintHighlightLine._2+0.05f);
				paintStep(20);
			}
			paintStep(200);
		} else {
			highlightLog(l.id,true);
			highlightLineMode=0;
			paintHighlightLine= new Tuple2[Log,Float](l,i);
			paintStep();
		}
	};
	//override def preCrossChange(l:Log,i:Int){};
	override def wantToCrossChange(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int){
		paintCrossChangeProgress=0;
		paintHighlightClock1=new Tuple2(l,i);
		paintHighlightClock2=new Tuple2(sl,sfirst);
		paintHighlightClock3=new Tuple2(sl,slast);
		paintStep(200);
		paintHighlightClock1=new Tuple2(null,0);
		paintHighlightClock2=new Tuple2(null,0);
		paintHighlightClock3=new Tuple2(null,0);
		paintStep(200);
		if (logs.crossChangeWithFirstNeeded(l,i,sl,sfirst,slast)||
			logs.crossChangeWithLastNeeded(l,i,sl,sfirst,slast)){
			paintHighlightClock1=new Tuple2(l,i);
			paintHighlightClock2=new Tuple2(sl,sfirst);
			paintHighlightClock3=new Tuple2(sl,slast);
			paintStep(100);
			if (logs.crossChangeWithFirstNeeded(l,i,sl,sfirst,slast)){
				for (i <- 0 until 50) {
					paintCrossChangeProgress= -i/50.0f;
					paintStep(20);
				}
				paintStep(600);
			}
			if (logs.crossChangeWithLastNeeded(l,i,sl,sfirst,slast)){
				for (i <- 0 until 50) {
					paintCrossChangeProgress=i/50.0f;
					paintStep(20);
				}
				paintStep(600);
			}			
		}
	};
	override def crossChanged(l: Log, i:Int, sl:Log, sfirst:Int,slast:Int){
		paintStep(1000);
		paintHighlightClock1=new Tuple2(null,0);
		paintHighlightClock2=new Tuple2(null,0);
		paintHighlightClock3=new Tuple2(null,0);
		paintCrossChangeProgress=0;
		paintStep(100);
	}
	override def searchWipePlace(l:Log,i:Int,ltr:Boolean){
		highlightLog(l.id,ltr);
		if (highlightLineMode==1 && paintHighlightLine._1 == l && paintHighlightLine._2 < i && ltr) {
			while (paintHighlightLine._2+0.001f<i) {
				paintHighlightLine= new Tuple2[Log,Float](l,paintHighlightLine._2+0.05f);
				paintStep(20);
			}
			paintStep(200);
		} else if (highlightLineMode==2 && paintHighlightLine._1 == l && paintHighlightLine._2 > i && !ltr) {
			while (paintHighlightLine._2-0.001f>i) {
				paintHighlightLine= new Tuple2[Log,Float](l,paintHighlightLine._2-0.05f);
				paintStep(20);
			}
			paintStep(200);
		}
		highlightLineMode=if (ltr) 1 else 2;
		paintHighlightLine= new Tuple2[Log,Float](l,i);
		paintStep();
	};
	
	def data2String(d:Int)= if (d>0) ('A'+d-1).toChar.toString else ('a'-d-1).toChar.toString;
	def data2Color(d:Int)= Color.getHSBColor(Math.abs(d)/Math.max(logs.maxS,1.0f),1f,0.7f);
	def data2ColorGrayed(d:Int)= Color.getHSBColor(Math.abs(d)/Math.max(logs.maxS,1.0f),0.3f,0.6f);
	def data2ColorVeryGrayed(d:Int)= Color.getHSBColor(Math.abs(d)/Math.max(logs.maxS,1.0f),0.1f,0.8f);
	def data2ColorDark(d:Int)= Color.getHSBColor(Math.abs(d)/Math.max(logs.maxS,1.0f),1f,0.5f);	

	def paintLogs( g:GraphicsTrait, logs: Logs){
		val f = new Font(null, 0, 10);	
		val fb = new Font(null, Font.BOLD, 12);	
		val lineHeight = g.getLineHeight(f);
		val lineHeightB = g.getLineHeight(fb);
		val blockHeight = lineHeight*(logs.length+1);
		val blockWidth = 60;
		val blockStart = 30;
		val blockDistance= 20;//x
		val logDistance = if (logs.length==2) 70 else if (logs.length==3) 30 else 15;//y 
		val someGap = 3;
		val bracketIndent = 5;
		def getLogY(l:Int): Int = (logDistance+blockHeight)*l;
		def getBlockX(i:Int): Int = (blockDistance+blockWidth)*i+blockStart;
		val bs1 = new BasicStroke(1);
		val bsn = new BasicStroke(2);
		val bsh = new BasicStroke(4);
		for (i<-0 until logs.length){
			val logY = getLogY(i);
			for (j <- 0 until logs(i).length) if (logs(i)(j)<0) {
				val blockXMid=getBlockX(j)+blockWidth/2;
				//draw arrows
				if (paintHighlightClock1._1 == logs(i) && paintHighlightClock1._2 == j) {
					g.setColor(Color.RED);
					g.setStroke(bsn);
				} else {
					g.setColor(data2ColorGrayed(logs(i)(j)));
					g.setStroke(bs1);
				}
				val firstSender = logs.firstSender(i,j);
				val lastSender = logs.lastSender(i,j);
				if (firstSender._1.id < i) g.drawVector(getBlockX(firstSender._2)+blockWidth/2,getLogY(firstSender._1.id)+blockHeight,blockXMid,logY);
				else g.drawVector(getBlockX(firstSender._2)+blockWidth/2,getLogY(firstSender._1.id),blockXMid,logY+blockHeight);
				if (lastSender._1.id < i) g.drawVector(blockXMid,logY,getBlockX(lastSender._2)+blockWidth/2,getLogY(lastSender._1.id)+blockHeight);
				else g.drawVector(blockXMid,logY+blockHeight,getBlockX(lastSender._2)+blockWidth/2,getLogY(lastSender._1.id));
			}
		}
		g.setStroke(bsn);
		for (i<-0 until logs.length){
			val logY = getLogY(i);
			for (j <- 0 until logs(i).length) {
				g.setColor(data2ColorDark(logs(i)(j)));
				val blockX = getBlockX(j);
				g.setFont(fb);
				g.drawString(data2String(logs(i)(j)),blockX+blockWidth/2,logY+lineHeight,GraphicsStatic.CENTER);
				
				//draw values
				val dataColor=data2Color(logs(i)(j));
				g.setFont(f);
				
				if (highlightLineMode!=2) g.setColor(dataColor);
				else g.setColor(data2ColorVeryGrayed(logs(i)(j)));
				for (k <- 0 until logs.length) 
					g.drawString(logs(i).clocksMin(j)(k).toString, blockX+blockWidth/2-someGap,logY+lineHeight*(k+2)-someGap,GraphicsStatic.RIGHT);
				if (highlightLineMode!=1) g.setColor(dataColor);
				else g.setColor(data2ColorVeryGrayed(logs(i)(j)));
				for (k <- 0 until logs.length) 
					g.drawString(logs(i).clocksMax(j)(k).toString, blockX+blockWidth-someGap,logY+lineHeight*(k+2)-someGap,GraphicsStatic.RIGHT);

				//draw brackets
				if ((paintHighlightClock1._1 == logs(i) && paintHighlightClock1._2 == j) ||
					(paintHighlightClock2._1 == logs(i) && paintHighlightClock2._2 == j && paintCrossChangeProgress<=0) ||
					(paintHighlightClock3._1 == logs(i) && paintHighlightClock3._2 == j && paintCrossChangeProgress>=0)) {
					//highlighted for cross change
					g.setColor(Color.RED);
					g.setStroke(bsh);
				} else if ((paintHighlightClock2._1 == logs(i) && paintHighlightClock2._2 == j && paintCrossChangeProgress>0) ||
					(paintHighlightClock3._1 == logs(i) && paintHighlightClock3._2 == j && paintCrossChangeProgress<0)) {
					//a little bit highlighted for cross change (sender which is currently not cross changed but sender of changed receiver)
					g.setColor(dataColor);
					g.setStroke(bsh);
				} else if (highlightLineMode==0 && logs(i)(j)>0) {
					//sender grayed while cross change search
					g.setColor(data2ColorVeryGrayed(logs(i)(j)));
					g.setStroke(bsn);
				} else {
					g.setColor(dataColor);
					g.setStroke(bsn);
				}
				g.drawLine(blockX+blockWidth/2,logY+lineHeight+someGap,blockX+blockWidth/2,logY+blockHeight-someGap);
				g.drawLine(blockX,logY+lineHeight,blockX,logY+blockHeight);
				g.drawLine(blockX,logY+lineHeight,blockX+bracketIndent,logY+lineHeight);
				g.drawLine(blockX,logY+blockHeight,blockX+bracketIndent,logY+blockHeight);
				g.drawLine(blockX+blockWidth,logY+lineHeight,blockX+blockWidth,logY+blockHeight);
				g.drawLine(blockX+blockWidth,logY+lineHeight,blockX+blockWidth-bracketIndent,logY+lineHeight);
				g.drawLine(blockX+blockWidth,logY+blockHeight,blockX+blockWidth-bracketIndent,logY+blockHeight);
				
			}
		}
		
		def drawFloatingClock(xpos: Int, ypos: Int, clock: Array[Int], rightSide: Boolean){
			g.setColor(Color.black);
			for (i <- 0 until logs.length) {
				val c=clock(i).toString;
				if (rightSide) g.drawString(c, xpos + someGap,ypos+lineHeight*(i+2)-someGap);
				else g.drawString(c, xpos - someGap ,ypos+lineHeight*(i+2)-someGap,GraphicsStatic.RIGHT);
			}
		}
		def drawLinedFloatingClock(xpos: Int, ypos: Int, clock: Array[Int], rightSide: Boolean){
			g.drawLine(xpos,ypos,xpos,ypos+blockHeight);
			drawFloatingClock(xpos,ypos,clock,rightSide);
		}
		
		if (paintHighlightLog>= -0.9) {
			if (highlightLineMode==0) g.setColor(Color.RED);
			else g.setColor(Color.orange);
			g.setStroke(bsn);
			
			val ypos=(getLogY(paintHighlightLog.toInt)+(lineHeight+blockHeight)/2+(logDistance+blockHeight)*(paintHighlightLog-paintHighlightLog.toInt)).toInt;
			g.drawVector(0,ypos,20,ypos);
		}
		
		if (paintHighlightLine._1 != null) {
			if (highlightLineMode==0) g.setColor(Color.RED);
			else g.setColor(Color.orange);
			g.setStroke(bsn);
			val ypos=getLogY(paintHighlightLine._1.id);
			var xpos=getBlockX(paintHighlightLine._2.toInt)+Math.round((blockWidth+blockDistance)*(paintHighlightLine._2-paintHighlightLine._2.toInt));
			if (highlightLineMode==2) xpos -= blockDistance;
			g.drawLine(xpos,ypos,xpos,ypos+blockHeight);
			val hl = paintHighlightLine._1;
			if (highlightLineMode==1) {
				val id=Math.floor(paintHighlightLine._2).toInt;
				if (id-1<0 || id>=hl.length) return;
				val cm1=paintHighlightLine._1.clocksMin(id);
				val cm2=if (id<=0) cm1 else hl.clocksMin(id-1);
				drawFloatingClock(xpos,ypos,cm1 zip cm2 map (i=>Math.max(i._1,i._2)),false);
			} else if (highlightLineMode==2) {
				val id = Math.ceil(paintHighlightLine._2).toInt;
				if (id<0 || id>=hl.length) return;
				val cm1=hl.clocksMax(id);
				val cm2=if (id<=0) cm1 else hl.clocksMax(id-1);
				drawFloatingClock(xpos,ypos,cm1 zip cm2 map (i=>Math.min(i._1,i._2)),true);
			}
		}

		//cross change clocks
		if (paintCrossChangeProgress!=0) { 
			g.setStroke(bsn);
			val ry = getLogY(paintHighlightClock1._1.id); //receiver y
			val rx = getBlockX(paintHighlightClock1._2); //receiver x
			
			{
				val progress:Float = if (paintCrossChangeProgress<0) -paintCrossChangeProgress else 1;
				val sy = getLogY(paintHighlightClock2._1.id); //sender y
				val sx = getBlockX(paintHighlightClock2._2); //sender x
				g.setColor(Color.blue);
				drawLinedFloatingClock((rx+(sx+blockWidth-rx)*progress+0.5).toInt,(ry+(sy-ry)*progress+0.5).toInt,
				                  paintHighlightClock1._1.clocksMax(paintHighlightClock1._2),true);
				g.setColor(Color.blue);
				drawLinedFloatingClock((sx+(rx-sx)*progress+0.5).toInt,(sy+(ry-sy)*progress+0.5).toInt,
				                  paintHighlightClock2._1.clocksMin(paintHighlightClock2._2),false);
			} 
			if (paintCrossChangeProgress>0) {
				val sy = getLogY(paintHighlightClock3._1.id); //last sender y
				val sx = getBlockX(paintHighlightClock3._2); //last sender x
				g.setColor(Color.blue);
				drawLinedFloatingClock((rx+(sx-rx)*(paintCrossChangeProgress)+0.5).toInt,(ry+(sy-ry)*(paintCrossChangeProgress)+0.5).toInt,
				                  paintHighlightClock1._1.clocksMin(paintHighlightClock1._2),false);
				g.setColor(Color.blue);
				drawLinedFloatingClock((sx+(rx+blockWidth-sx)*(paintCrossChangeProgress)+0.5).toInt,(sy+(ry-sy)*(paintCrossChangeProgress)+0.5).toInt,
				                  paintHighlightClock3._1.clocksMax(paintHighlightClock3._2),true);
			}
		};	
	}
	
	def paintAll( g: Graphics2D){
		g.translate(10,30);
		paintLogs(new Graphics2DWrapper(g),logs);
	}
	
	override def paintComponent( gb:Graphics){
		val g = gb.asInstanceOf[Graphics2D];
		paintAll(g);
	}
}


object EventMatchGUI{
	def main(args: Array[String]) {
		println ("starting... with file "+args(0));
		val fileContent = Source.fromFile(args(0)).reset.getLines.toList;
		//fileContent.foreach(print);
		var evg = new EventMatchDialog(new Logs(new LogReader(fileContent).logs));
 		
	}
}