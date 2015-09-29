package es.gob.afirma.keystores;

import java.io.File;
import java.io.IOException;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Almac&eacute;n espec&iacute;fico para la tarjeta profesional CNP (FNMT-RCM TIF) accedida
 * mediante su m&oacute;dulo PKCS#11, &uacute;nicamente en Windows.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class CNPKeyStoreManager extends AOKeyStoreManager {

	private static final String P11_LIB = Platform.getSystemLibDir() + "\\TIF_P11.dll"; //$NON-NLS-1$

	CNPKeyStoreManager(final boolean forceReset, final Object parent) throws AOKeyStoreManagerException, IOException, UnsupportedOperationException {
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			throw new UnsupportedOperationException(
				"La tarjeta FNMT-RCM TIF solo se soporta como preferente en Windows, no en " + Platform.getOS() //$NON-NLS-1$
			);
		}
		if (!new File(P11_LIB).exists()) {
			throw new UnsupportedOperationException(
				"No estan instalados los controladores PKCS#11 de la tarjeta FNMT-RCM TIF" //$NON-NLS-1$
			);
		}
		init(
			AOKeyStore.PKCS11,
			null,
			new UIPasswordCallback(
				KeyStoreMessages.getString("AOKeyStore.12"), //$NON-NLS-1$
				parent
			),
			new String[] {
				P11_LIB,
				null
    		},
			forceReset
		);
	}

}
