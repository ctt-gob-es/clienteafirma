/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.actions;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import es.gob.afirma.cliente.CipherManager;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;

/** Acci&oacute;n privilegiada para el cifrado de datos. */
public final class CipherAction extends BasicPrivilegedAction<Boolean, Void> {

    /** Manejador de cifrado. */
    private CipherManager cipherManager;

    /** Datos que se desean cifrar. */
    private byte[] data;

    /** Construye la operaci&oacute;n de cifrado de datos. Si se indican datos,
     * se cifraran estos; si no se indican se tomar&aacute;n los configurados en
     * el manejador de cifrado.
     * @param cipherManager
     *        Manejador de cifrado de datos.
     * @param data
     *        Datos que se desean cifrar, {@code null} si se desean tomar
     *        los del manejador. */
    public CipherAction(CipherManager cipherManager, byte[] data) {

        if (cipherManager == null) {
            throw new NullPointerException();
        }

        this.cipherManager = cipherManager;
        this.data = data;
    }

    public Boolean run() {

        try {
            if (data == null) {
                cipherManager.cipherData();
            }
            else {
                cipherManager.cipherData(data);
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
        catch (NoSuchAlgorithmException e) {
            setError("Algoritmo de cifrado no soportado", e); //$NON-NLS-1$
            return false;
        }
        catch (IOException e) {
            setError("No se han podido leer los datos a cifrar", e); //$NON-NLS-1$
            return false;
        }
        catch (AOException e) {
            setError("Error durante el proceso de cifrado", e); //$NON-NLS-1$
            return false;
        }

        return true;
    }
}
