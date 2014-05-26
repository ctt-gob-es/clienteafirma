package es.gob.afirma.ui.utils;

import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;

/** Configuraci&oacute;n del modo gr&aacute;fico de accesibilidad. */
public final class ConfigureCaret extends DefaultCaret {

	private static final long serialVersionUID = 1L;

	@Override
    protected synchronized void damage(final Rectangle r) {
	    if (r == null) {
	        return;
	    }

	    // give values to x,y,width,height (inherited from java.awt.Rectangle)
	    this.x = r.x;
	    this.y = r.y;
	    this.height = 100;
	    // A value for width was probably set by paint(), which we leave alone.
	    // But the first call to damage() precedes the first call to paint(), so
	    // in this case we must be prepared to set a valid width, or else paint()
	    // will receive a bogus clip area and caret will not get drawn properly.
	    if (this.width <= 0) {
	        this.width = getComponent().getWidth();
	    }

	    repaint(); // calls getComponent().repaint(x, y, width, height)
	  }

	@Override
    public void paint(final Graphics g) {
		setBlinkRate(500);
		final JTextComponent comp = getComponent();
	    if (comp == null) {
	        return;
	    }
	    final int dot = getDot();
	    final Rectangle r;
	    try {
	    	r = comp.modelToView(dot);
	    	if (r == null) {
	    	    return;
	    	}
	    }
	    catch (final BadLocationException e) { return; }

	    if ( this.x != r.x || this.y != r.y ) {
	      // paint() has been called directly, without a previous call to
	      // damage(), so do some cleanup. (This happens, for example, when the
	      // text component is resized.)
	    	repaint(); // erase previous location of caret
	    	this.x = r.x;   // Update dimensions (width gets set later in this method)
	    	this.y = r.y;
	    	this.height = 100;
	    }

	    g.setColor(comp.getCaretColor());
	    g.setXORMode(comp.getBackground()); // do this to draw in XOR mode

	    if (isVisible()){
	    	g.fillRect(this.x,this.y,20,100);
	    }
	  }
}
