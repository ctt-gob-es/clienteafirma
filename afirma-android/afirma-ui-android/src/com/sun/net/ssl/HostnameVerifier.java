package com.sun.net.ssl;

/** Esqueleto de la clase original de Sun, solo para habilitar la compilaci&oacute;n en Android.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@Deprecated
public interface HostnameVerifier {

	/** Implementaci&oacute;n vac&iacute;a solo para habilitar la compilaci&oacute;n en Android.
	 * @param arg0 No se usa.
	 * @param arg1 No se usa.
	 * @return No se usa. */
	public abstract boolean verify(String arg0, String arg1);

}
