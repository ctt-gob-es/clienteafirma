package es.gob.afirma.core.keystores;

import java.io.File;

/** Permite cambiar o refrescar un almac&eacute;n de claves actualmente en uso.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface KeyStoreRefresher {

	/** Cambia el alma&eacute;n actual por otr basado en fichero.
	 * @param localFile Fichero de almac&eacute;n de claves y certificados */
	void changeToFileStore(final File localFile);

	/** Refresca los almacenes del alma&eacute;n actual. */
	void refresh();

}
