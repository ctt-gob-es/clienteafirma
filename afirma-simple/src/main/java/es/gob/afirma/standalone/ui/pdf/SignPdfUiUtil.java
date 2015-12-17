package es.gob.afirma.standalone.ui.pdf;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;

final class SignPdfUiUtil {

	private SignPdfUiUtil() {
		// No instanciable
	}

	public static Rectangle getScreenBounds(final Component wnd) {

	    final Rectangle sb;
	    if(wnd == null || wnd.getGraphicsConfiguration() == null) {
	        sb = GraphicsEnvironment
	           .getLocalGraphicsEnvironment()
	           .getDefaultScreenDevice()
	           .getDefaultConfiguration()
	           .getBounds();
        }
	    else {
	        sb = wnd.getGraphicsConfiguration().getBounds();
        }

	    final Insets si = getScreenInsets(wnd);

	    sb.x     +=si.left;
	    sb.y     +=si.top;
	    sb.width -=si.left+si.right;
	    sb.height-=si.top+si.bottom;

	    return sb;
    }

	private static Insets getScreenInsets(final Component wnd) {
	    if(wnd == null || wnd.getToolkit() == null || wnd.getGraphicsConfiguration() == null) {
	        return Toolkit.getDefaultToolkit().getScreenInsets(GraphicsEnvironment
	           .getLocalGraphicsEnvironment()
	           .getDefaultScreenDevice()
	           .getDefaultConfiguration());
        }
		return wnd.getToolkit().getScreenInsets(wnd.getGraphicsConfiguration());
    }

}
