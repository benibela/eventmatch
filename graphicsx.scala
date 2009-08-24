/* Graphics To Latex Converter

  Copyright (C) 2009 Benito van der Zander <benito@benibela.de>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*/


import java.awt._;
object GraphicsStatic{
	final val LEFT=0;
	final val CENTER=1;
	final val RIGHT=2;
}
trait GraphicsTrait{
	def getWrappedObject():Any;

	def getLineHeight(f:Font):Int;
	
	def setFont(f: Font);
	def setColor(c:Color);
	def setStroke(s: BasicStroke);
	
	def drawLine(x1:Int, y1: Int, x2: Int, y2: Int);
	def drawString(s:String, x:Int, baseLineY:Int);
	def drawString(s:String, x:Int, baseLineY:Int, align: Int);
	
	def drawVector(p1x:Int, p1y:Int, p2x: Int, p2y:Int){
		drawVector(p1x,p1y,p2x,p2y,10/*Math.sqrt((p1x-p2x)*(p1y-p2y)+(p1y-p2y)*(p1y-p2y))/10*/,(45*Math.Pi/180).toFloat,false);
	}
	protected def drawVector(p1x:Int, p1y:Int, p2x: Int, p2y:Int, filled: Boolean){
		drawVector(p1x,p1y,p2x,p2y,10/*(Math.sqrt((p1x-p2x)*(p1x-p2x)+(p1y-p2y)*(p1y-p2y))/10)*/.toFloat,(45*Math.Pi/180).toFloat,filled);
	}
	def drawVector(p1x:Int, p1y:Int, p2x: Int, p2y:Int, arrowLen: Float, arrowAngleRad: Float, filled: Boolean){
		val lineAngle = Math.atan2(p2y-p1y,p2x-p1x);
		val arrowPoint1x=Math.round(arrowLen*Math.cos(Math.Pi+lineAngle-arrowAngleRad)).toInt+p2x;
		val arrowPoint1y=Math.round(arrowLen*Math.sin(Math.Pi+lineAngle-arrowAngleRad)).toInt+p2y;
		val arrowPoint2x=Math.round(arrowLen*Math.cos(Math.Pi+lineAngle+arrowAngleRad)).toInt+p2x;
		val arrowPoint2y=Math.round(arrowLen*Math.sin(Math.Pi+lineAngle+arrowAngleRad)).toInt+p2y;
		
		drawLine(p1x,p1y,p2x,p2y);
		if (!filled){ //val x = 0; //canvas.Polygon([ArrowPoint1,p2,ArrowPoint2]);
		//else {
			drawLine(p2x,p2y,   arrowPoint1x, arrowPoint1y);
			drawLine(p2x,p2y,   arrowPoint2x, arrowPoint2y);
		}
	}
}

//using a wrapper is faster than implicit def and you can pass GraphicsTrait around
class Graphics2DWrapper(wrapAroung: Graphics2D) extends GraphicsTrait {
	val g = wrapAroung;
	var fontMetric: java.awt.FontMetrics = null;
	def getWrappedObject()=g;

	def getLineHeight(f:Font) = g.getFontMetrics(f).getHeight();
	
	def setFont(f: Font){
		g.setFont(f);
		fontMetric=null;
	}
	def setColor(c:Color)=g.setColor(c);
	def setStroke(s: BasicStroke)=g.setStroke(s);
	
	def drawLine(x1:Int, y1: Int, x2: Int, y2: Int)=g.drawLine(x1,y1,x2,y2);
	def drawString(s:String, x:Int, baseLineY:Int)=g.drawString(s,x,baseLineY);
	def drawString(s:String, x:Int, baseLineY:Int, align: Int){
		if (fontMetric==null && align!=GraphicsStatic.LEFT) fontMetric = g.getFontMetrics(g.getFont());
		align match {
			case GraphicsStatic.LEFT => g.drawString(s,x,baseLineY);
			case GraphicsStatic.CENTER => g.drawString(s,x-fontMetric.stringWidth(s)/2,baseLineY);
			case GraphicsStatic.RIGHT => g.drawString(s,x-fontMetric.stringWidth(s),baseLineY);
		}
	}
}

class LatexGraphics(w:Int,h:Int) extends GraphicsTrait {
	//coordinates are mapped like this:
	// 1 px => 1 pt (important for font selection)
	// x => x 
	// y => h - y   (latex has (0,0) bottom left, java top left; both uses bottom left as text position, so keeping y not only mirrors but breaks all text)

	var fitSize = true;
	var fitOrigin = true;
	
	
	//private var sb_header = new StringBuilder("\\setlength{\\unitlength}{1pt}\n"+"\\begin{picture}("+w.toString+","+h.toString+")\n");
	private var sb_precontent= new StringBuilder;
	private var sb= new StringBuilder;
	private var curFontSelect =""; //TODO: check if this can change the coordinate base if used globally 
	private var curColor ="";     //WTF is this necessary???? \color and \linethickness set the value persistent and globally, 
	private var curThickness =""; //BUT if you use it globally, all following coordinates are changed (however \color doesn't change them
	                              //and \linethickness also doesn't change them. BUT \linethickness + \color + \linethickness changes them!!!????)
	
	private var curFontLineHeight=0; 
	private var maxX= -9999999; //used to set size and origin according to the drawn points
	private var minX=9999999;
	private var maxY= -9999999;
	private var minY=9999999;
	
	private var definedcolors= new java.util.HashSet[String];
	//private val indent="\t";
	def getLatex():String = {
		if (maxX<minX || maxY<minY) return "%nothing drawn so far";
		var res = new StringBuilder("\\setlength{\\unitlength}{1pt}\n");
		val width=if (fitSize && fitOrigin) (maxX-minX) else if (fitSize) (maxX) else (w);
		val height=if (fitSize && fitOrigin) (maxY-minY) else if (fitSize) (maxY) else (h);
		res.append("\\begin{picture}("+width.toString+","+height.toString+")");
		if (fitOrigin) res.append("("+minX+","+minY+")\n");
		else res.append("\n");
		res.append(sb_precontent);
		res.append(sb);
		res.append("\\end{picture}\n");
		return res.toString;
	}
	
	def getWrappedObject()=this;

	def getLineHeight(f:Font)=(f.getSize2D()*1.2f).toInt;
	
	def setFont(f: Font){
	//TODO: use float or int (<-current) size?? 
		curFontSelect="";
		if (f.getName()!="Default") 
			if (f.getFamily()=="Monospaced") curFontSelect+=("\\fontfamily{cmtt}");
			else if (f.getFamily()=="SansSerif") curFontSelect+=("\\fontfamily{cmss}");
			//else if (f.getFamily()=="Monospaced") curFontSelect+=("\\fontfamily{cmtt}\n");
			else curFontSelect+=("\\fontfamily{"+f.getName()+"}")
		if (f.getStyle()!=Font.PLAIN) {
			if ((f.getStyle()&Font.BOLD)!=0) curFontSelect+=("\\fontseries{b}");
			if ((f.getStyle()&Font.ITALIC)!=0) curFontSelect+=("\\fontshape{it}");
		}
		curFontLineHeight=(f.getSize2D()*1.2f).round;
		curFontSelect+=("\\fontsize{"+f.getSize()+"}{"+curFontLineHeight+"}");
		curFontSelect+=("\\selectfont  ");
	}
	def setColor(c:Color){
		val colors = c.getRGBComponents(null);
		val id= "R"+((colors(0)*255).toInt).toString + "G"+((colors(1)*255).toInt).toString + "B"+((colors(2)*255).toInt).toString;
		if (!this.definedcolors.contains(id)) {
			sb_precontent.append("\\definecolor{"+id+"}{rgb}{"+colors(0).toString+","+colors(1).toString+","+colors(2).toString+"}\n");
			this.definedcolors.add(id);
		}
		curColor="\\color{"+id+"}";
	}
	def setStroke(s: BasicStroke){
		curThickness="\\linethickness{"+s.getLineWidth().toString+"pt}"; //does only apply to vertical/horizontal lines if no extra package (pict2e,eepic)
	}
	
	//this returns the line length as used by latex, horizontal delta only iff the line is not vertical, 
	//otherwise vertical delta
	protected def latexLineLength(x1:Int, y1: Int, x2: Int, y2: Int)=
		if (x1>x2) x1-x2
		else if (x1<x2) x2-x1
		else if (y1>y2) y1-y2
		else y2-y1;
	//updates minX/Y and maxX/Y (Carefully: x,y are in input coordinates, not latex output coordinates)
	protected def includePointInBoundary(x:Int,y:Int){
		if (x<minX) minX=x;
		if (x>maxX) maxX=x;
		if (h-y<minY) minY=h-y;
		if (h-y>maxY) maxY=h-y;
	}
	
	def drawLine(x1:Int, y1: Int, x2: Int, y2: Int){
		includePointInBoundary(x1,y1);
		includePointInBoundary(x2,y2);
		sb.append("\\put("+x1+","+(h-y1)+"){"+curColor+curThickness+"\\line("+(x2-x1)+","+(y1-y2)+"){"+latexLineLength(x1,y1,x2,y2)+"}}\n");
	}
	def drawString(s:String, x:Int, baseLineY:Int){
		includePointInBoundary(x,baseLineY);
		if (curFontLineHeight!=0) includePointInBoundary(x,baseLineY-curFontLineHeight);
		sb.append("\\put("+x+","+(h-baseLineY)+"){"+curFontSelect+s+"}\n");
	}
	def drawString(s:String, x:Int, baseLineY:Int, align: Int){
		includePointInBoundary(x,baseLineY);
		if (curFontLineHeight!=0) includePointInBoundary(x,baseLineY-curFontLineHeight);
		val widthguess=s.length()*20;
		val heightguess=20;
		align match {
			case GraphicsStatic.LEFT => sb.append("\\put("+x+","+(h-baseLineY)+"){"+curFontSelect+s+"}\n");
			case GraphicsStatic.CENTER => sb.append("\\put("+(x-widthguess/2)+","+(h-baseLineY)+"){\\makebox("+widthguess+","+heightguess+")[b]{"+curFontSelect+s+"}}\n");
			case GraphicsStatic.RIGHT => sb.append("\\put("+(x-widthguess)+","+(h-baseLineY)+"){\\makebox("+widthguess+","+heightguess+")[br]{"+curFontSelect+s+"}}\n");
		}
	}
	
	override def drawVector(x1:Int, y1:Int, x2: Int, y2:Int){
		includePointInBoundary(x1,y1);
		includePointInBoundary(x2,y2);
		sb.append("\\put("+x1+","+(h-y1)+"){"+curColor+curThickness+"\\vector("+(x2-x1)+","+(y1-y2)+"){"+latexLineLength(x1,y1,x2,y2)+"}}\n");
	}
	override def drawVector(x1:Int, y1:Int, x2: Int, y2:Int, filled: Boolean){
		includePointInBoundary(x1,y1);
		includePointInBoundary(x2,y2);
		sb.append("\\put("+x1+","+(h-y1)+"){"+curColor+curThickness+"\\vector("+(x2-x1)+","+(y1-y2)+"){"+latexLineLength(x1,y1,x2,y2)+"}}\n");
	}
}

