package es.gob.afirma.android.crypto;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;

import android.util.Log;

/** Gestor simple de claves y certificados para dispositivos Android 2 y 3.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AndroidJcaKeyStoreManager implements MobileKeyStoreManager {

	private PrivateKeyEntry pke = null;
	private Exception pkeException = null;

	/** Construye un gestor simple de claves y certificados a partir de un almac&eacute;n JCE/JCA.
	 * @param alias Alias preseleccionado
	 * @param ks KeyStore origen, debe estar previamente inicializado y cargado
	 * @param pin Contrase&ntilde;a del almac&eacute;n */
	public AndroidJcaKeyStoreManager(final String alias, final KeyStore ks, final char[] pin) {

		if (ks == null) {
			throw new IllegalArgumentException("El almacen de claves es nulo"); //$NON-NLS-1$
		}

		if (alias == null) {
			throw new IllegalArgumentException("El alias seleccionado es nulo"); //$NON-NLS-1$
		}

		Log.i("es.go.afirma.android", "Alias seleccionado: " + alias); //$NON-NLS-1$ //$NON-NLS-2$

		try {
			this.pke = (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(pin));
		}
		catch (final Exception e) {
			Log.e("es.gob.afirma", "Error obteniendo la entrada a la clave privada: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			this.pkeException = e;
		}
	}

	/** {@inheritDoc} */
	@Override
	public void getPrivateKeyEntryAsynchronously(final PrivateKeySelectionListener pksl) {
		if (pksl == null) {
			throw new IllegalArgumentException("La clase a notificar la seleccion de clave no puede ser nula"); //$NON-NLS-1$
		}
		if (this.pkeException != null) {
			pksl.keySelected(new KeySelectedEvent(this.pkeException));
		}
		else {
			pksl.keySelected(new KeySelectedEvent(this.pke));
		}
	}

}