package es.gob.afirma.core.keystores;

import java.io.IOException;

/** Permite cambiar o refrescar un almac&eacute;n de claves actualmente en uso.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface KeyStoreRefresher {

	/** Refresca los almacenes del alma&eacute;n actual.
	 * @throws IOException En caso de errores de entrada / salida */
	void refresh() throws IOException ;

}
