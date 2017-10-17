/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

 import java.awt.Component;
import java.awt.Cursor;
import java.awt.Image;
import java.io.ByteArrayInputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.cert.certvalidation.CertificateVerifierFactory;
import es.gob.afirma.cert.certvalidation.CertificateVerifierFactoryException;
import es.gob.afirma.cert.certvalidation.ValidationResult;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

 /** Interfaz para la validaci&oacute;n de certificados.
  * @author Tom&aacute;s Garc&iacute;aMer&aacute;s */
 public final class CertValidationUi {

 	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

 	private static final String BEGIN_CERTIFICATE = "BEGIN CERTIFICATE"; //$NON-NLS-1$
 	private static final String END_CERTIFICATE = "END CERTIFICATE"; //$NON-NLS-1$

 	/** Valida un certificado, mostrando un interfaz gr&aacute;fico con el resultado.
 	 * @param certificate Certificado a validar.
 	 * @param parent Componente padre para la modalidad.
 	 * @param currentComponent Componente actual para la modalidad.
 	 * @param icon Icono a usar en las ventanas del interfaz. */
 	public static void validateCert(final X509Certificate certificate,
 			                        final Component parent,
 			                        final Component currentComponent,
 			                        final Image icon) {

 		if (parent != null) {
 			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
 		}
 		else if (currentComponent != null) {
 			currentComponent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
 		}

 		final ValidationResult vr;
 		try {
 			vr = CertificateVerifierFactory.getCertificateVerifier(certificate).validateCertificate();
 		}
 		catch (final CertificateVerifierFactoryException e) {
 			LOGGER.severe(
 				"No se conocen mecanismos de validacion para los certificados de este emisor (" + certificate.getIssuerX500Principal() + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
 			);
 			AOUIFactory.showErrorMessage(
 				icon,
 				SimpleAfirmaMessages.getString("MenuValidation.9"), //$NON-NLS-1$
 				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
 				JOptionPane.ERROR_MESSAGE
 			);
 			if (parent != null) {
 				parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
 			}
 			else if (currentComponent != null) {
 				currentComponent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
 			}
 			return;
 		}

 		if (!ValidationResult.VALID.equals(vr)) {
 			AOUIFactory.showErrorMessage(
 				icon,
 				"<html>" + SimpleAfirmaMessages.getString("MenuValidation.11") + "<br>" + vr.toString() + "</html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
 				SimpleAfirmaMessages.getString("MenuValidation.10"), //$NON-NLS-1$
 				JOptionPane.WARNING_MESSAGE
 			);
 		}
 		else {
 			AOUIFactory.showMessageDialog(
 				parent,
 				SimpleAfirmaMessages.getString(
 					"MenuValidation.14", //$NON-NLS-1$
 					AOUtil.getCN(certificate.getIssuerX500Principal().toString()),
 					AOUtil.getCN(certificate)
 				),
 				SimpleAfirmaMessages.getString("MenuValidation.15"), //$NON-NLS-1$
 				JOptionPane.INFORMATION_MESSAGE
 			);
 		}

 		if (parent != null) {
 			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
 		}
 		else if (currentComponent != null) {
 			currentComponent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
 		}
 	}

 	static void validateCert(final byte[] rawCert,
 			                 final Component parent,
 			                 final Component currentComponent,
 			                 final Image icon) {

 		final String certString = new String(rawCert);
 		final byte[] certBytes;
 		if (certString.contains(BEGIN_CERTIFICATE)) {
 			try {
 				certBytes = Base64.decode(
 					certString.substring(
 						certString.indexOf(BEGIN_CERTIFICATE) + BEGIN_CERTIFICATE.length(),
 						certString.indexOf(END_CERTIFICATE)
 					)
 				);
 			}
 			catch (final Exception e) {
 				LOGGER.severe("Certificado PEM corrupto: " + e); //$NON-NLS-1$
 				AOUIFactory.showErrorMessage(
 					icon,
 					SimpleAfirmaMessages.getString("MenuValidation.8"), //$NON-NLS-1$
 					SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
 					JOptionPane.ERROR_MESSAGE
 				);
 				return;
 			}
 		}
 		else {
 			certBytes = rawCert;
 		}
 		final X509Certificate certificate;
 		try {
 			certificate = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
 				new ByteArrayInputStream(certBytes)
 			);
 		}
 		catch (final CertificateException e) {
 			LOGGER.severe("Error en la generacion del certificado: " + e); //$NON-NLS-1$
 			AOUIFactory.showErrorMessage(
 				icon,
 				SimpleAfirmaMessages.getString("MenuValidation.8"), //$NON-NLS-1$
 				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
 				JOptionPane.ERROR_MESSAGE
 			);
 			return;
 		}

 		validateCert(certificate, parent, currentComponent, icon);

 	}
 }