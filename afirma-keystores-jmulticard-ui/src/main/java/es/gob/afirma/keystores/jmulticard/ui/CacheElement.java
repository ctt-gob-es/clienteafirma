package es.gob.afirma.keystores.jmulticard.ui;

/** Interfaz para identificar los elementos que cachean informaci&oacute;n del usuario
 * y que nos permite reiniciarlos si es posible.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public interface CacheElement {

	/** Reinicia los valores del elemento. */
	void reset();
}
