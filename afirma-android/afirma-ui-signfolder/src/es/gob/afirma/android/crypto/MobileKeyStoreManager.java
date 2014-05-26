package es.gob.afirma.android.crypto;

import java.security.KeyStore.PrivateKeyEntry;


/** Gestor simple de claves y certificados para dispositivos m&oacute;viles.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface MobileKeyStoreManager {

    /** Inicia un proceso as&iacute;ncrono de selecci&oacute;n de una entrada que apunta a una clave privada.
     * @param e Clase a la que hay que notificar cuando se complete la selecci&oacute;n */
    void getPrivateKeyEntryAsynchronously(final PrivateKeySelectionListener e);

    /** Interfaz para clases que esperen una selecci&oacute;n as&iacute;ncrona de una clave privada. */
    public interface PrivateKeySelectionListener {

        /** Notifica que se ha seleccionado una entrada que apunta a una clave privada.
         * @param kse Evento de selecci&oacute;n de una entrada que apunta a una clave privada */
        void keySelected(KeySelectedEvent kse);
    }

    /** Evento de selecci&oacute;n de una entrada que apunta a una clave privada. */
    public static class KeySelectedEvent {

        private final PrivateKeyEntry pke;
        private final String alias;
        private final Throwable e;

        /** Construye un evento de selecci&oacute;n de una entrada que apunta a una clave privada.
         * @param p Entrada que apunta a una clave privada seleccionada */
        public KeySelectedEvent(final PrivateKeyEntry p, final String alias) {
            this.pke = p;
            this.alias = alias;
            this.e = null;
        }

        /** Construye un evento de selecci&oacute;n falida de una entrada que apunta a una clave privada.
         * @param t Causa del fallo en la selecci&oacute;n */
        public KeySelectedEvent(final Throwable t) {
            this.pke = null;
            this.alias = null;
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
        
        /** Obtiene el certificado seleccionado codificado.
         * @return Certificado.
         * @throws Throwable Cuando ocurre un error en la selecci&oacute;n o la codificaci&oacute;n del certificado. */ 
        public byte[] getCertificateEncoded() throws Throwable {
            if (this.e != null) {
                throw this.e;
            }
            return this.pke.getCertificate().getEncoded();
        }
        
        /** Obtiene el alias del certificado seleccionado.
         * @return Alias del certificado o {@code null} si no tiene.
         * @throws Throwable Cuando ocurre un error en la selecci&oacute;n del certificado. */ 
        public String getCertificateAlias() throws Throwable {
            if (this.e != null) {
                throw this.e;
            }
            return this.alias;
        }
    }
}