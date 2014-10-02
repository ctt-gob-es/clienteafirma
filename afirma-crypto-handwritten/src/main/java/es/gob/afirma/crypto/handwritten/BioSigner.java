package es.gob.afirma.crypto.handwritten;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.crypto.handwritten.wacom.WacomSignatureWindow;

/** Firmador de documentos PDF con firma manuscrita digitalizada biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BioSigner {

	/** Logger para la impresi&oacute;n de trazas. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	private Rectangle signatureAreaOnPad = null;
	private WacomSignatureWindow signatureWindow = null;

	/** Obtiene una firma biom&eacute;trica.
	 * @param parent Componente padre sobre el que mostrar los elementos visuales.
	 * @param signatureId Identificador de la firma a obtener.
	 * @param spl Componente al que notificar el resultado de la firma.
	 * @param template  Plantilla en formato HTML a mostrar en la tableta de firma.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @throws IOException Cuando ocurre un error en la descarga de los datos o la codificacion
	 *                     de la plantilla a mostrar en la tableta de firma.
	 * @throws SignaturePadException Cuando no se ha podido inicializar la tableta de firma. */
	public void sign(final Object parent,
			         final String signatureId,
					 final SignaturePadListener spl,
					 final String template,
					 final Rectangle signatureArea) throws IOException, SignaturePadException {

		this.signatureAreaOnPad = signatureArea;
		this.signatureWindow = new WacomSignatureWindow(
			parent,
			signatureId,
			template,
			this.signatureAreaOnPad
		);
		this.signatureWindow.addSignatureListener(spl);
		this.signatureWindow.captureSign();
	}

	/** Obtiene una firma biom&eacute;trica.
	 * @param parent Componente padre sobre el que mostrar los elementos visuales.
	 * @param signatureId Identificador de la firma a obtener.
	 * @param spl Componente al que notificar el resultado de la firma.
	 * @param jpegImage Imagen a mostrar en la tableta de firma.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @throws IOException Cuando ocurre un error en la descarga de los datos o la codificacion
	 * de la plantilla a mostrar en la tableta de firma.
	 * @throws SignaturePadException Cuando no se ha podido inicializar la tableta de firma. */
	public void sign(final Object parent,
					 final String signatureId,
					 final SignaturePadListener spl,
					 final byte[] jpegImage,
					 final Rectangle signatureArea) throws IOException, SignaturePadException {

		if (spl == null) {
			throw new IllegalArgumentException(
				"Es necesario indicar una clase a la que notificar el resultado de la firma" //$NON-NLS-1$
			);
		}
		if (jpegImage == null) {
			LOGGER.info("No se ha indicado una imagen de fondo para la tableta de captura"); //$NON-NLS-1$
		}
		if (signatureArea == null) {
			LOGGER.info(
				"No se ha indicado un recuadro de firma en la tableta de captura, se usara toda la pantalla para firmar" //$NON-NLS-1$
			);
		}

		this.signatureAreaOnPad = signatureArea;
		this.signatureWindow = new WacomSignatureWindow(
			parent,
			signatureId,
			jpegImage,
			this.signatureAreaOnPad
		);
		this.signatureWindow.addSignatureListener(spl);
		this.signatureWindow.captureSign();
	}

}
