package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Frame;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.JFrame;

import es.gob.afirma.crypto.handwritten.JseUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePadException;
import es.gob.afirma.crypto.handwritten.SignaturePadListener;

/** Panel con el aspecto de una tableta Wacom sobre el que se replica la superficie de firma. */
public class WacomSignatureWindow extends JFrame {

	private static final long serialVersionUID = -2946927576518582468L;

	final WacomSignaturePad signatureDialog;

	BufferedImage signatureImage;

	/**
	 *
	 * @param parent
	 * @param htmlTemplate
	 * @param signAreaRect
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n de la plantilla de la tableta
	 * @throws SignaturePadException Cuando no se ha podido inicializar el dispositivo de creacion de firmas.
	 */
	public WacomSignatureWindow(final Object parent, final String htmlTemplate, final Rectangle signAreaRect) throws IOException, SignaturePadException {
		this.signatureDialog = new WacomSignaturePad(
				parent instanceof Frame ? (Frame) parent : null);

		this.signatureDialog.init(
				JseUtil.html2Image(
						htmlTemplate,
						this.signatureDialog.getAvailableWidth(),
						this.signatureDialog.getAvailableHeight()),
				signAreaRect);
	}

	/**
	 *
	 * @param parent
	 * @param imageTemplate
	 * @param signAreaRect
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n de la plantilla de la tableta.
	 * @throws SignaturePadException Cuando no se ha podido inicializar el dispositivo de creacion de firmas.
	 */
	public WacomSignatureWindow(final Object parent, final byte[] imageTemplate, final Rectangle signAreaRect) throws SignaturePadException, IOException {

		this.signatureDialog = new WacomSignaturePad(
				parent instanceof Frame ? (Frame) parent : null);

		this.signatureDialog.init(imageTemplate, signAreaRect);
	}

	public void addSignatureListener(final SignaturePadListener listener) {
		this.signatureDialog.addSignatureListener(listener);
	}

	public void captureSign() {

		this.signatureDialog.setVisible(true);
	}

	public void clearScreen() {
		this.signatureDialog.dispose();
	}
}
