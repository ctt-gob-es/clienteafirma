package es.gob.afirma.keystores;

import es.gob.afirma.core.MissingLibraryException;

/** Indica que el JRE carece de la biblioteca SunMSCAPI para el acceso al almac&eacute;n de claves y certificados
 * de Windows
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class MissingSunMSCAPIException extends MissingLibraryException {

	private static final long serialVersionUID = -338521022809698613L;

	/** Crea una nueva instancia de la excepci&oacute;n.
	 * @param e Excepci&oacute;n previa en la cadena */
	public MissingSunMSCAPIException(final Exception e) {
		super("No esta instalada la biblioteca SunMSCAPI", e); //$NON-NLS-1$
	}

}
