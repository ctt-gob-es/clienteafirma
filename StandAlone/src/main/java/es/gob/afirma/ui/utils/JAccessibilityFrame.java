package es.gob.afirma.ui.utils;
import javax.swing.JFrame;

public abstract class JAccessibilityFrame extends JFrame {

	private ResizingAdaptor resizingAdaptor;
	
	public JAccessibilityFrame(){
		super();
		this.resizingAdaptor = new ResizingAdaptor(this, null, null);
		this.addComponentListener(this.resizingAdaptor);
	}
	
	public abstract int getInitialWidth();
	public abstract int getInitialHeight();
	public abstract int getMinimumRelation();
	
	protected final void callResize(){
		this.resizingAdaptor.adjustWindowFonts();
	}
}