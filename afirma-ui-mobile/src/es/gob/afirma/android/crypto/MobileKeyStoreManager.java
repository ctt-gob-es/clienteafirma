package es.gob.afirma.android.crypto;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;


/** Gestor simple de claves y certificados para dispositivos m&oacute;viles.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface MobileKeyStoreManager {

    /** Inicia un proceso as&iacute;ncrono de selecci&oacute;n de una entrada que apunta a una clave privada.
     * @param e Clase a la que hay que notificar cuando se complete la selecci&oacute;n */
    void getPrivateKeyEntryAsynchronously(final PrivateKeySelectionListener e);

    /** Importa un certificado al sistema (o al almac&eacute;n particular de Afirma si el sistema carece de un
     * almac&eacute;n central accesible) desde un fichero PCKS#12.
     * @param pfx Fichero PKCS#12 / Personal File Exchange
     * @param pwc Callback para solicitar las contrase&ntilde;as
     * @throws KeyStoreException Cuando ocurre algun error durante la importaci&oacute;n */
    void importCertificateFromPkcs12(final byte[] pfx, CustomizablePasswordCallback pwc) throws KeyStoreException;

    /** Interfaz para clases que esperen una selecci&oacute;n as&iacute;ncrona de una clave privada. */
    public interface PrivateKeySelectionListener {

        /** Notifica que se ha seleccionado una entrada que apunta a una clave privada.
         * @param kse Evento de selecci&oacute;n de una entrada que apunta a una clave privada */
        void keySelected(KeySelectedEvent kse);
    }

    /** Evento de selecci&oacute;n de una entrada que apunta a una clave privada. */
    public static class KeySelectedEvent {

        private final PrivateKeyEntry pke;
        private final Throwable e;

        /** Construye un evento de selecci&oacute;n de una entrada que apunta a una clave privada.
         * @param p Entrada que apunta a una clave privada seleccionada */
        public KeySelectedEvent(final PrivateKeyEntry p) {
            this.pke = p;
            this.e = null;
        }

        /** Construye un evento de selecci&oacute;n falida de una entrada que apunta a una clave privada.
         * @param t Causa del fallo en la selecci&oacute;n */
        public KeySelectedEvent(final Throwable t) {
            this.pke = null;
            this.e = t;
        }

        /** Obtiene la entrada que apunta a una clave privada asociada al evento.
         * @return Entrada que apunta a una clave privada asiciada al evento
         * @throws Throwable Si la obtenci&oacute;n de la clave privada produjo algun error */
        public PrivateKeyEntry getPrivateKeyEntry() throws Throwable {
            if (this.e != null) {
                throw this.e;
            }
            return this.pke;
        }
    }
}