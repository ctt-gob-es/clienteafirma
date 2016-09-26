package es.gob.afirma.envelopers.cms;

import es.gob.afirma.core.AOException;

/**
 * Excepci&oacute;n que declara un error en un operaci&oacute;n de ensobrado o
 * desensobrado originada por un problema con el PKCS#11 de un dispositivo
 * criptogr&aacute;fico.
 */
public class Pkcs11WrapOperationException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 5896939695525368842L;

	/**
	 * Construye la excepci&oacute;n almacenando como mensaje el tipo de error devuelto por el PKCS#11.
	 * @param msg Mensaje que identifica el origen del error en el PKCS#11.
	 * @param e Excepci&oacute;n generada por el PKCS#11.
	 */
	public Pkcs11WrapOperationException(final String msg, final Exception e) {
		super(msg, e);
	}
}
