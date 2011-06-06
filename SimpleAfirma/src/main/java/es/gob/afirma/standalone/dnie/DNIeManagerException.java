package es.gob.afirma.standalone.dnie;

/**
 * Excepci&oacute;n en la gesti&oacute;n de almac&eacute;n DNIe v&iacute;a PKCS#11 y JSR-268.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class DNIeManagerException extends Exception {

	private static final long serialVersionUID = 9198656551956236883L;
	
	DNIeManagerException(final String msg, final Throwable t) {
		super(msg, t);
	}
	
	DNIeManagerException(final String msg) {
		super(msg);
	}
	
}
