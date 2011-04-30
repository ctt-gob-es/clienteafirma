/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.actions;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOUtil;

/**
 * Acci&oacute;n privilegiada que carga el contenido de un fichero y devuelve almacena como
 * resultado su valor en un objeto {@code byte[]}.
 */
public final class LoadFileAction extends BasicPrivilegedAction<Boolean, byte[]> {

	/** Ruta del fichero que se desea cargar. */
	private URI uri = null;

	/** Indica si los datos del fichero est&aacute;n codificados en Base 64. */
	private boolean base64Encoded = false;
	
	/** Indica si debe mostrarse el di&aacute;logo de espera durante la carga del fichero. */
	private boolean waitDialog = true;
	
	/** Componente padre sobre el que mostrar el di&aacute;logo de carga. */
	private Component parent = null;
	
	/**
	 * Construye una acci&oacute;n privilegiada para la carga del contenido de un fichero.
	 * @param strUri Ruta del fichero.
	 * @param parent Componente padre.
	 */
	public LoadFileAction(final String strUri, final Component parent) {
		
		try {
			this.uri = AOUtil.createURI(strUri);
		} 
		catch (AOException e) {
			this.setError("La URI '" + strUri + "' no es valida", e); //$NON-NLS-1$ //$NON-NLS-2$
			throw new IllegalArgumentException(this.getErrorMessage(), e);
		}
		this.parent = parent;
	}
	
	/**
	 * Construye una acci&oacute;n privilegiada para la carga del contenido de un fichero.
	 * @param fileUri Ruta del fichero.
	 * @param parent Componente padre.
	 */
	public LoadFileAction(URI fileUri, Component parent) {
		this.uri = fileUri;
		this.parent = parent;
	}
	
	/**
	 * Permite establecer si los datos del fichero est&aacute;an codificados en base 64, para
	 * que se decodifiquen y se obtenga la informaci&oacute;n original.
	 * @param isBase64Encoded {@code true} si los datos del fichero est&aacute;n en base 64.
	 */
	public void setBase64Encoded(boolean isBase64Encoded) {
		this.base64Encoded = isBase64Encoded;
	}
	
	/**
	 * Indica si debe mostrarse un di&aacute;logo de espera durante la carga del fichero.
	 * Por defecto, no se s&iacute; se mostrar&aacute;.
	 * @param showDialog <code>true</code> si se desea que se muestre el di&aacute;logo,
	 *                   <code>false</code> en caso contrario
	 */
	public void setWaitDialog(boolean showDialog) {
		this.waitDialog = showDialog;
	}
	
	public Boolean run() {
		
		InputStream is = null;
		try {
			is = AOUtil.loadFile(uri, parent, waitDialog, base64Encoded);
			this.setResult(AOUtil.getDataFromInputStream(is));
		} 
		catch (final FileNotFoundException e) {
			this.setError("El fichero '" + uri.toASCIIString() + "' no existe", e); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		} 
		catch (final Exception e) {
			this.setError("No se pudo acceder al fichero '" + uri.toASCIIString() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		} 
		catch (final Throwable e) {
			this.setError("Error grave en la lectura del fichero: " + uri.toASCIIString());
			return false;
		} 
		finally {
			if (is != null) {
				try { is.close(); } catch (final Throwable e) {}
			}
		}
		return true;
	}
}
