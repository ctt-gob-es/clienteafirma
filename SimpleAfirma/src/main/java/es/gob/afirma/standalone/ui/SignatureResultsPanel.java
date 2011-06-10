/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.security.cert.X509Certificate;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/**
 * Panel para mostrar un informe resumen sobre el resultado de una firma electr&oacute;nica. 
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SignatureResultsPanel extends JPanel {

	private static final long serialVersionUID = -1581326925023691437L;
	
	/**
	 * Crea un panel para mostrar un informe resumen sobre el resultado de una firma electr&oacute;nica.
	 * @param sign Firma o fichero firmado sobre el que queremos mostrar un resumen
	 * @param fileName Nombre del fichero en el que se ha guardado la firma o el fichero firmado
	 * @param signingCert Certificado usado para la firma
	 */
	public SignatureResultsPanel(final byte[] sign, final String fileName, final X509Certificate signingCert) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				createUI(sign, fileName, signingCert);
			}
		});
	}
	
	private void createUI(final byte[] sign, final String fileName, final X509Certificate signingCert) {
		
	}
	
}
