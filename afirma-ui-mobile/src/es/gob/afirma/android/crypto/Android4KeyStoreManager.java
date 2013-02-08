package es.gob.afirma.android.crypto;

import java.security.KeyStore.PrivateKeyEntry;

import android.annotation.TargetApi;
import android.app.Activity;
import android.content.Intent;
import android.security.KeyChain;
import android.security.KeyChainAliasCallback;

/** Gestor simple de claves y certificados para dispositivos Android 4.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@TargetApi(14)
public class Android4KeyStoreManager implements MobileKeyStoreManager {

    private final Activity activity;

    Activity getActivity() {
        return this.activity;
    }

    /** Construye un gestor simple de claves y certificados para dispositivos Android 4.
     * @param act Actividad padre de la aplicaci&oacute;n padre */
    public Android4KeyStoreManager(final Activity act) {
        if (act == null) {
            throw new IllegalArgumentException("Es encesaria una actividad padre para mostrar los dialogos de seleccion de certificado" //$NON-NLS-1$
            );
        }
        this.activity = act;
    }

    /** {@inheritDoc} */
    @Override
    public void getPrivateKeyEntryAsynchronously(final PrivateKeySelectionListener pksl) {
        if (pksl == null) {
            throw new IllegalArgumentException("La clase a notificar la seleccion de clave no puede ser nula"); //$NON-NLS-1$
        }
        KeyChain.choosePrivateKeyAlias(this.activity, new KeyChainAliasCallback() {
            /** {@inheritDoc} */
            @Override
            public void alias(final String alias) {
                try {
                    pksl.keySelected(new KeySelectedEvent(new PrivateKeyEntry(KeyChain.getPrivateKey(Android4KeyStoreManager.this.getActivity(),
                                                                                                     alias),
                                                                              KeyChain.getCertificateChain(Android4KeyStoreManager.this.getActivity(),
                                                                                                           alias))));
                }
                catch (final Exception e) {
                    pksl.keySelected(new KeySelectedEvent(e));
                }
            }
        },
                                       new String[] { "RSA" }, // KeyTypes //$NON-NLS-1$
                                       null, // Issuers
                                       null, // Host
                                       -1, // Port
                                       null // Alias
        );
    }

    /** Importa un certificado al sistema (o al almac&eacute;n particular de Afirma si el sistema carece de un
     * almac&eacute;n central accesible) desde un fichero PCKS#12.
     * @param pfx Fichero PKCS#12 / Personal File Exchange
     * @param pwc Se ignora, el gestor central de Android se encarga de solicitar las contrase&ntilde;as si es necesario */
    @Override
    public void importCertificateFromPkcs12(final byte[] pfx, final CustomizablePasswordCallback pwc) {
        final Intent intent = KeyChain.createInstallIntent();
        intent.putExtra(KeyChain.EXTRA_PKCS12, pfx);
        this.activity.startActivity(intent);
    }
}