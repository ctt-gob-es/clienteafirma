package es.gob.afirma.crypto.handwritten;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JDialog;

/** Dispositivo gen&eacute;rico de captura de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class SignaturePad extends JDialog {

	protected List<SignaturePadListener> signatureListeners = new ArrayList<SignaturePadListener>();

	private static final long serialVersionUID = 2921672399678043077L;

	protected SignaturePad(final Frame frame,
			               final boolean b) {
		super(frame, b);
	}

	/** Obtiene el ancho de la superficie &uacute;til para im&aacute;genes
	 * y captura de la pantalla del dispositivo.
	 * @return Ancho de la superficie &uacute;til de la pantalla del dispositivo. */
	public abstract int getAvailableWidth();

	/** Obtiene el alto de la superficie &uacute;til para im&aacute;genes
	 * y captura de la pantalla del dispositivo.
	 * @return Alto de la superficie &uacute;til de la pantalla del dispositivo. */
	public abstract int getAvailableHeight();

	/** A&ntilde;ade una clase a la que notificar cuando se finaliza una firma.
	 * @param sl Clase a la que notificar cuando se finaliza una firma. */
	public synchronized void addSignatureListener(final SignaturePadListener sl) {
		if (sl != null) {
			this.signatureListeners.add(sl);
		}
	}

	/** Eliminamos el listado de clases a las que notificar los eventos. */
	public synchronized void removeSignatureListeners() {
		this.signatureListeners.clear();
	}

}
