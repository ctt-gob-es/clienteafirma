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

import java.io.IOException;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;

/** Acci&oacute;n privilegiada para el ensobrado de datos. La ejecuci&oacute;n de
 * la acci&oacute;n devuelve {@code true} o {@code false} y el resultado
 * almacenado es un array de bytes. */
public final class WrapAction implements PrivilegedExceptionAction<byte[]> {

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
            throw new IllegalArgumentException("El EnveloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.data = data != null ? data.clone() : null;
    }

    /** {@inheritDoc} */
    public byte[] run() throws IllegalArgumentException, NoSuchAlgorithmException, IOException,
    AOException, CertificateEncodingException, KeyException {

    	try {
    		if (this.enveloperManager.getCmsContentType().equals(AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
    			if (this.data == null) {
    				this.enveloperManager.encrypt();
    			}
    			else {
    				this.enveloperManager.encrypt(this.data);
    			}
    		}
    		else {
    			if (this.data == null) {
    				this.enveloperManager.envelop();
    			}
    			else {
    				this.enveloperManager.envelop(this.data);
    			}
    		}
    		return this.enveloperManager.getEnvelopedData();
    	}
    	catch (final AOCancelledOperationException e) {
    		throw e;
    	}
    	catch (final IllegalArgumentException e) {
    		throw new IllegalArgumentException("Modo de clave no soportado"); //$NON-NLS-1$
    	}
    	catch (final NullPointerException e) {
    		throw new IllegalArgumentException("No se ha indicado el tipo de envoltorio o los destinatarios del mismo", e); //$NON-NLS-1$
    	}
    	catch (final NoSuchAlgorithmException e) {
    		throw new NoSuchAlgorithmException("Algoritmo de ensobrado no soportado", e); //$NON-NLS-1$
    	}
    	catch (final IOException e) {
    		throw new IOException("No se han podido leer los datos a ensobrar"); //$NON-NLS-1$
    	}
    	catch (final AOException e) {
    		throw e;
    	}
    	catch (final CertificateEncodingException e) {
    		throw new CertificateEncodingException("El certificado del remitente no es valido", e); //$NON-NLS-1$
    	} catch (final KeyException e) {
    		throw new KeyException("La clave de envoltura generada no es valida", e); //$NON-NLS-1$
    	}
    }

}
