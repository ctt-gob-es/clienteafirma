package es.gob.afirma.triphase.signer.processors;

/** Procesador de firmas trif&aacute;sicas XAdES FacturaE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class FacturaETriPhasePreProcessor extends XAdESTriPhasePreProcessor {

	/** Construye un procesador de firmas trif&aacute;sicas XAdES FacturaE.
	 * @param installXmlDSigProvider Indica si se debe instalar expresamente un proveedor de firmas XML. */
	public FacturaETriPhasePreProcessor(final boolean installXmlDSigProvider) {
		super(true, installXmlDSigProvider);
	}

}
