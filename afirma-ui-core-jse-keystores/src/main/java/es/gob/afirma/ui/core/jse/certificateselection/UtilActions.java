/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.keystores.NameCertificateBean;
import es.gob.afirma.core.misc.LoggerUtil;

final class UtilActions {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String HELP_URI;
	static {
		final Properties p = new Properties();
		try {
			p.load(UtilActions.class.getResourceAsStream("/resources/selectiondialogutil.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se han podido cargar las propiedades del dialogo de seleccion de certificados, se utilizaran los valores por defecto: " + e //$NON-NLS-1$
			);
		}
		HELP_URI = p.getProperty(
			"helpUrl", //$NON-NLS-1$
			"http://incidencias-ctt.administracionelectronica.gob.es/wiki/doku.php?id=forja-ctt_wiki:clienteafirma:adenda_-_uso_del_dialogo_grafico_de_seleccion_de_certificados" //$NON-NLS-1$
		);
	}

	private UtilActions() {
		// No instanciable
	}

	static void doHelp() {
		if (Desktop.isDesktopSupported()) {
			try {
				Desktop.getDesktop().browse(new URI(HELP_URI));
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido abrir la pagina Web de ayuda: " + e); //$NON-NLS-1$
			}
		}
		else {
			LOGGER.warning(
				"No se soporta la apertura de paginas Web, por lo que no se puede abrir la ayuda" //$NON-NLS-1$
			);
		}
	}

	static void doRefresh(final CertificateSelectionDialog selectionDialog, final Component parent) {
		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		}

		selectionDialog.refreshKeystore();

		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}

	static void doChangeView(final NameCertificateBean[] nameCertificates, final CertificateLineView view,
			final CertificateSelectionDialog selectionDialog, final Component parent) {
		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		}

		selectionDialog.refreshDialog(nameCertificates, view);

		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}

	static void doChangeKeyStore(final int keyStoreType, final CertificateSelectionDialog selectionDialog, final Component parent , final String ksName, final String ksLibPath) {
		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		}

		LOGGER.info("Se cambia al almacen de certificados " + LoggerUtil.getTrimStr(ksName)); //$NON-NLS-1$

		selectionDialog.changeKeyStore(keyStoreType, ksName, ksLibPath);

		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}
}
