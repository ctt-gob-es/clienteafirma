/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.applet.actions;

import java.io.IOException;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.applet.EnveloperManager;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;

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
            throw new IllegalArgumentException("El EnveloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.envelop = envelop.clone();
    }

    public Boolean run() {

        try {
            this.enveloperManager.unwrap(this.envelop);
        }
        catch (final AOCancelledOperationException e) {
            setError("Operacion cancelada por el usuario", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final AOInvalidRecipientException e) {
            setError("El usuario no es uno de los destinatarios del sobre", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final AOInvalidFormatException e) {
            setError("No se ha proporcionado un envoltorio soportado", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final IllegalArgumentException e) {
            setError("Modo de clave no soportado", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final IOException e) {
            setError("El envoltorio esta corrupto o no ha podido leerse", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final AOException e) {
            setError("Error durante el proceso de desensobrado", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }
        catch (final CertificateEncodingException e) {
            setError("El certificado del destinatario no es valido", e); //$NON-NLS-1$
            return Boolean.FALSE;
        }

        this.setResult(this.enveloperManager.getContentData());

        return Boolean.TRUE;
    }

}
