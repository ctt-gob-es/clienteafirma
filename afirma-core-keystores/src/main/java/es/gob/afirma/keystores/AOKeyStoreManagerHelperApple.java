package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;

import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.misc.Platform;

final class AOKeyStoreManagerHelperApple {

	private AOKeyStoreManagerHelperApple() {
		// No permitimos la instanciacion
	}

	static KeyStore initApple(final InputStream store) throws AOKeyStoreManagerException, IOException {
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			throw new InvalidOSException("Apple Mac OS X"); //$NON-NLS-1$
		}

		final KeyStore ks;

		// Inicializamos
		try {
			ks = KeyStore.getInstance(AOKeyStore.APPLE.getProviderName());
		}
		catch (final Exception e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido obtener el almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}

		try {
			ks.load(store, null);
		}
		catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"No se han podido cargar los certificados del almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido verificar la integridad del almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}
		return ks;
	}

}
