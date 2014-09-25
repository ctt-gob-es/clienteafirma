package es.gob.afirma.crypto.handwritten;

import java.util.EventListener;

/** Clase a la que notificar cuando se finaliza una firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface SignaturePadListener extends EventListener {

	/** Finalizaci&oacute;n correcta de una firma.
	 * @param sr resultado de la firma. */
	void signatureFinished(SignatureResult sr);

	/** Cancelaci&oacute;n de una firma.
	 * @param signatureId Identificador de la firma cancelada. */
	void signatureCancelled(String signatureId);

	/**Finalizaci&oacute;n con errores de una firma.
	 * @param e Errores que causaron el aborto de la firma.
	 * @param signatureId Identificador de la firma abortada. */
	void signatureAborted(Throwable e, String signatureId);

}
