package es.gob.afirma.cert.signvalidation;

import java.io.IOException;

/** Valida una firma del tipo del validador instanciado.
 * @author Sergio Mart&iacute;nez Rico. */
public interface SignValider {

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
     * @return Validez de la firma. 
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    SignValidity validate(final byte[] sign) throws IOException;
}
