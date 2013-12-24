package es.gob.afirma.keystores;

import es.gob.afirma.core.MissingLibraryException;

/** Indica que el JRE carece de la biblioteca SunPKCS11 para el acceso a almacenes de claves y certificados
 * PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class MissingSunPKCS11Exception extends MissingLibraryException {

	private static final long serialVersionUID = -338521022809698613L;

	/** Crea una nueva instancia de la excepci&oacute;n.
	 * @param e Excepci&oacute;n previa en la cadena */
	public MissingSunPKCS11Exception(final Exception e) {
		super("No esta instalada la biblioteca SunPKCS11", e); //$NON-NLS-1$
	}

}
