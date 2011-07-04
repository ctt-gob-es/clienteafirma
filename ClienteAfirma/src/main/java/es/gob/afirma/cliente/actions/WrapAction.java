package es.gob.afirma.cliente.actions;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.cliente.EnveloperManager;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOConstants;

/** Acci&oacute;n privilegiada para el ensobrado de datos. La ejecuci&oacute;n de
 * la acci&oacute;n devuelve {@code true} o {@code false} y el resultado
 * almacenado es un array de bytes. */
public final class WrapAction extends BasicPrivilegedAction<Boolean, byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Datos que se desean ensobrar. */
    private final byte[] data;

    /** Construye la operaci&oacute;n de ensobrado de datos. Si se indican datos,
     * se ensobraran estos; si no se indican se tomar&aacute;n los configurados
     * en el manejador de ensobrado.
     * @param enveloperManager
     *        Manejador de ensobrado de datos.
     * @param data
     *        Datos que se desean ensobrar, {@code null} si se desean tomar
     *        los del manejador. */
    public WrapAction(final EnveloperManager enveloperManager, final byte[] data) {

        if (enveloperManager == null) {
            throw new NullPointerException();
        }

        this.enveloperManager = enveloperManager;
        this.data = data.clone();
    }

    public Boolean run() {

        try {
            if (enveloperManager.getCmsContentType().equals(AOConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
                if (data == null) {
                    enveloperManager.encrypt();
                }
                else {
                    enveloperManager.encrypt(data);
                }
            }
            else {
                if (data == null) {
                    enveloperManager.envelop();
                }
                else {
                    enveloperManager.envelop(data);
                }
            }
        }
        catch (AOCancelledOperationException e) {
            setError("Operacion cancelada por el usuario", e); //$NON-NLS-1$
            return false;
        }
        catch (IllegalArgumentException e) {
            setError("Modo de clave no soportado", e); //$NON-NLS-1$
            return false;
        }
        catch (NullPointerException e) {
            setError("No se ha indicado el tipo de envoltorio o los destinatarios del mismo", e); //$NON-NLS-1$
            return false;
        }
        catch (NoSuchAlgorithmException e) {
            setError("Algoritmo de ensobrado no soportado", e); //$NON-NLS-1$
            return false;
        }
        catch (IOException e) {
            setError("No se han podido leer los datos a ensobrar", e); //$NON-NLS-1$
            return false;
        }
        catch (AOException e) {
            setError("Error durante el proceso de ensobrado", e); //$NON-NLS-1$
            return false;
        }
        catch (CertificateEncodingException e) {
            setError("El certificado del remitente no es valido", e); //$NON-NLS-1$
            return false;
        }

        this.setResult(enveloperManager.getEnvelopedData());

        return true;
    }

}
