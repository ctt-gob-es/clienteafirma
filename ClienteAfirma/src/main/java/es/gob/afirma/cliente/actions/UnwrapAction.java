package es.gob.afirma.cliente.actions;

import java.io.IOException;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.cliente.EnveloperManager;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;

/** Acci&oacute;n privilegiada para el desensobrado de datos. La ejecuci&oacute;n
 * de la acci&oacute;n devuelve {@code true} o {@code false} y el resultado
 * almacenado es un array de bytes. */
public final class UnwrapAction extends BasicPrivilegedAction<Boolean, byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Envoltorio que se desea desensobrar. */
    private final byte[] envelop;

    /** Construye la operaci&oacute;n de desensobrado de datos. Si se indica un
     * sobre, se ensobrara este; si no se indica se tomar&aacute; el configurado
     * en el manejador de ensobrado.
     * @param enveloperManager
     *        Manejador de ensobrado de datos.
     * @param envelop
     *        Sobre que se desea desensobrar, {@code null} si se desean
     *        tomar los del manejador. */
    public UnwrapAction(final EnveloperManager enveloperManager, final byte[] envelop) {

        if (enveloperManager == null) {
            throw new NullPointerException();
        }

        this.enveloperManager = enveloperManager;
        this.envelop = envelop.clone();
    }

    public Boolean run() {

        try {
            enveloperManager.unwrap(envelop);
        }
        catch (AOCancelledOperationException e) {
            setError("Operacion cancelada por el usuario", e); //$NON-NLS-1$
            return false;
        }
        catch (AOInvalidRecipientException e) {
            setError("El usuario no es uno de los destinatarios del sobre", e); //$NON-NLS-1$
            return false;
        }
        catch (AOInvalidFormatException e) {
            setError("No se ha proporcionado un envoltorio soportado", e); //$NON-NLS-1$
            return false;
        }
        catch (IllegalArgumentException e) {
            setError("Modo de clave no soportado", e); //$NON-NLS-1$
            return false;
        }
        catch (IOException e) {
            setError("El envoltorio esta corrupto o no ha podido leerse", e); //$NON-NLS-1$
            return false;
        }
        catch (AOException e) {
            setError("Error durante el proceso de desensobrado", e); //$NON-NLS-1$
            return false;
        }
        catch (CertificateEncodingException e) {
            setError("El certificado del destinatario no es valido", e); //$NON-NLS-1$
            return false;
        }

        this.setResult(enveloperManager.getContentData());

        return true;
    }

}
