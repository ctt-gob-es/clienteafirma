package es.gob.afirma.android.crypto;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Collections;
import java.util.List;

import es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException;

/** Gestor simple de claves y certificados para dispositivos Android 2 y 3.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AndroidJcaKeyStoreManager implements MobileKeyStoreManager {

	private final KeyStore keyStore;
	private final char[] keyStorePwd;

	/** Construye un gestor simple de claves y certificados a partir de un almac&eacute;n JCE/JCA.
	 * @param ks KeyStore origen, debe estar previamente inicializado y cargado
	 * @param pass Contrase&ntilde;a del almac&eacute;n */
	public AndroidJcaKeyStoreManager(final KeyStore ks, final char[] pass) {
		if (ks == null) {
			throw new IllegalArgumentException("Es necesario proporcionar un KeyStore no nulo"); //$NON-NLS-1$
		}
		this.keyStore = ks;
		this.keyStorePwd = pass;
	}

	/** {@inheritDoc} */
	@Override
	public void getPrivateKeyEntryAsynchronously(final PrivateKeySelectionListener pksl) {

		if (pksl == null) {
			throw new IllegalArgumentException("La clase a notificar la seleccion de clave no puede ser nula"); //$NON-NLS-1$
		}

		final List<String> aliases;
		try {
			aliases = Collections.list(this.keyStore.aliases());
		}
		catch(final Exception e) {
			pksl.keySelected(new KeySelectedEvent(e));
			return;
		}
		if (aliases.size() < 1) {
			pksl.keySelected(new KeySelectedEvent(new AOCertificatesNotFoundException()));
			return;
		}

		KeySelectedEvent kse;
		// Si solo hay un certificado lo usamos directamente
		if (aliases.size() == 1) {
			try {
				kse = new KeySelectedEvent(
					(PrivateKeyEntry) this.keyStore.getEntry(
						aliases.get(0),
						new KeyStore.PasswordProtection(this.keyStorePwd)
					)
				);
			}
			catch (final Exception e) {
				kse = new KeySelectedEvent(e);
			}
		}
		else {
			// TODO: Mostrar el dialogo de seleccion de certificados aqui
			kse = new KeySelectedEvent(
				new UnsupportedOperationException("Hay mas de un certificado y no tenemos dialogo de seleccion") //$NON-NLS-1$
			);
		}

		pksl.keySelected(kse);
	}

}
