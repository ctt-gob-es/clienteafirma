package es.gob.afirma.applet;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

public final class CryptoUtils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    /** Recupera el hash en hexadecimal de los datos de entrada del applet para
     * firma.
     * @return Hash en hexadecimal formateado o <code>null</code> si no se
     *         introdujeron o no pudieron leerse los datos. */
    public static String getHexDigestData(final String algorithm) { 

        byte[] hashData = null;
        // Si estamos firmando datos o si es una firma de hash implicita
        // (contamos con los datos)
        if (this.data != null && (this.hash == null || this.sigMode == AOSignConstants.SIGN_MODE_IMPLICIT)) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(this.data, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Si estamos firmando un fichero
        else if (this.fileUri != null) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(this.fileUri)), algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Si estamos firmando un hash en modo explicito (el caso de modo
        // implicito ya se trato)
        else if (this.hash != null) {
            hashData = this.hash;
        }
        // Si no se esta firmando nada de lo anterior (que deberia abarcar todos
        // los casos de firmar y cofirma)
        // comprobamos si se ha introducido la informacion de una firma
        // electronica (caso unicamente de
        // contrafirma ya que la cofirma debe mostrar la informacion de los
        // datos, no de la firma ya existente).
        // Comprobamos si se ha introducido la firma directamente
        else if (this.signData != null) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(this.signData, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Comprobamos si se ha introducido la localizacion de la firma
        else if (this.electronicSignatureFile != null) {
            try {
                hashData =
                        AOCryptoUtil.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(this.electronicSignatureFile)),
                                                      algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }

        if (hashData == null) {
            LOGGER.severe("No se han indicado o no se han podido leer los datos para el calculo de la huella digital"); //$NON-NLS-1$
            return null;
        }

        return AOUtil.hexify(hashData, ":"); //$NON-NLS-1$
    }
}
