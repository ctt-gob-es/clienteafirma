package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Frame;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.JFrame;

import es.gob.afirma.crypto.handwritten.JseUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePadException;
import es.gob.afirma.crypto.handwritten.SignaturePadListener;
import es.gob.afirma.crypto.handwritten.SignerInfoBean;

/** Panel con el aspecto de una tableta Wacom sobre el que se replica la superficie de firma. */
public class WacomSignatureWindow extends JFrame {

	private static final long serialVersionUID = -2946927576518582468L;

	final WacomSignaturePad signatureDialog;

	BufferedImage signatureImage;

	/** Crea el panel con el aspecto de una tableta Wacom sobre el que se replica la superficie de firma.
	 * @param parent Componente padre para la modalidad.
	 * @param htmlTemplate Plantilla HTML a mostrar como fondo en la pantalla de la r&eacute;plica de
	 *                     la tableta de captura.
	 * @param signAreaRect Coordenadas del rect&aacute;ngulo de firma en la pantalla de la
	 *                     r&eacute;plica de la tableta.
	 * @param signer Informaci&oacute;n del firmante que va a usar la tableta.
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n de la plantilla de la tableta
	 * @throws SignaturePadException Cuando no se ha podido inicializar el dispositivo de creacion de firmas. */
	public WacomSignatureWindow(final Object parent,
			                    final String htmlTemplate,
			                    final Rectangle signAreaRect,
			                    final SignerInfoBean signer) throws IOException, SignaturePadException {
		this.signatureDialog = new WacomSignaturePad(
			parent instanceof Frame ? (Frame) parent : null,
			signer
		);

		this.signatureDialog.init(
			JseUtil.html2Image(
				htmlTemplate,
				this.signatureDialog.getAvailableWidth(),
				this.signatureDialog.getAvailableHeight()
			),
			signAreaRect
		);
	}

	/** Crea el panel con el aspecto de una tableta Wacom sobre el que se replica la superficie de firma.
	 * @param parent Componente padre para la modalidad.
	 * @param imageTemplate Imagen a mostrar como fondo en la pantalla de la r&eacute;plica de
	 *                      la tableta de captura.
	 * @param signAreaRect Coordenadas del rect&aacute;ngulo de firma en la pantalla de la
	 *                     r&eacute;plica de la tableta.
	 * @param signer Informaci&oacute;n del firmante que va a usar la tableta.
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n de la imagen de
	 *                     fondo de la r&eacute;plica de la tableta.
	 * @throws SignaturePadException Cuando no se ha podido inicializar la r&eacute;plica
	 *                               del dispositivo de creacion de firmas. */
	public WacomSignatureWindow(final Object parent,
			                    final byte[] imageTemplate,
			                    final Rectangle signAreaRect,
			                    final SignerInfoBean signer) throws SignaturePadException, IOException {

		this.signatureDialog = new WacomSignaturePad(
			parent instanceof Frame ? (Frame) parent : null,
			signer
		);

		this.signatureDialog.init(imageTemplate, signAreaRect);
	}

	/** Indica una clase a la que notificar los eventos que se produzcan en la r&eacute;plica de la
	 * tableta de captura (sus botones son comletamente funcionales).
	 * @param listener Instancia a la que notificar los eventos que se produzcan en la r&eacute;plica de la
	 *                 tableta de captura. */
	public void addSignatureListener(final SignaturePadListener listener) {
		if (listener != null) {
			this.signatureDialog.addSignatureListener(listener);
		}
	}

	/** Inicia el proceso de firma en la r&eacute;plica de la tableta de captura. */
	public void captureSign() {
		this.signatureDialog.setVisible(true);
	}

	/** Borra la pantalla de la r&eacute;plica de la tableta. */
	public void clearScreen() {
		this.signatureDialog.dispose();
	}
}
