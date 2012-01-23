/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.applet;

import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;

/** Acci&oacute;n privilegiada para agregar un nuevo remitentes a un sobre
 * electr&oacute;nico. La ejecuci&oacute;n de la acci&oacute;n devuelve {@code true} o {@code false} y el resultado almacenado es un array de bytes. */
public final class CoEnvelopAction implements PrivilegedExceptionAction<byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Envoltorio que se desea desensobrar. */
    private final byte[] envelop;

    /** Envoltorio de m&uacute;ltiples remitentes.
     * @param enveloperManager
     *        Gestor de envoltorios
     * @param envelop
     *        Envoltorio */
    public CoEnvelopAction(final EnveloperManager enveloperManager, final byte[] envelop) {
        if (enveloperManager == null) {
            throw new IllegalArgumentException("El EnverloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.envelop = envelop.clone();
    }

    /** {@inheritDoc} */
    public byte[] run() throws AOKeyStoreManagerException, AOCertificatesNotFoundException, AOInvalidFormatException, AOException {
        try {
            this.enveloperManager.coEnvelop(this.envelop);
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final AOKeyStoreManagerException e) {
            throw e;
        }
        catch (final AOCertificatesNotFoundException e) {
            throw e;
        }
        catch (final AOInvalidFormatException e) {
            throw new AOInvalidFormatException("No se ha proporcionado un envoltorio soportado", e); //$NON-NLS-1$
        }
        catch (final AOException e) {
            throw e;
        }
        return this.enveloperManager.getEnvelopedData();
    }
}
