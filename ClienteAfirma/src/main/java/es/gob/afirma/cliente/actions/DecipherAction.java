/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.actions;

import java.io.IOException;

import es.gob.afirma.cliente.CipherManager;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidKeyException;

/** Acci&oacute;n privilegiada para el descifrado de datos. */
public final class DecipherAction extends BasicPrivilegedAction<Boolean, Void> {

    /** Manejador de cifrado. */
    private final CipherManager cipherManager;

    /** Datos que se desean descifrar. */
    private final byte[] data;

    /** Construye la operaci&oacute;n de descifrado de datos. Si se indican
     * datos, se descifraran estos; si no se indican se tomar&aacute;n los
     * configurados en el manejador de cifrado.
     * @param cipherManager
     *        Manejador de cifrado de datos.
     * @param data
     *        Datos que se desean descifrar, {@code null} si se desean tomar
     *        los del manejador. */
    public DecipherAction(final CipherManager cipherManager, final byte[] data) {

        if (cipherManager == null) {
            throw new IllegalArgumentException("El CipherManager no puede ser nulo");
        }

        this.cipherManager = cipherManager;
        this.data = data;
    }

    public Boolean run() {

        try {
            if (data == null) {
                cipherManager.decipherData();
            }
            else {
                cipherManager.decipherData(data);
            }
        }
        catch (final AOCancelledOperationException e) {
            setError("Operacion cancelada por el usuario", e); //$NON-NLS-1$
            return false;
        }
        catch (final IOException e) {
            setError("No se han podido leer los datos a descifrar", e); //$NON-NLS-1$
            return false;
        }
        catch (final AOInvalidKeyException e) {
            setError("Se ha proporcionado una clave incorrecta", e); //$NON-NLS-1$
            return false;
        }
        catch (final AOException e) {
            setError("Error durante el proceso de descifrado", e); //$NON-NLS-1$
            return false;
        }
        return true;
    }
}
