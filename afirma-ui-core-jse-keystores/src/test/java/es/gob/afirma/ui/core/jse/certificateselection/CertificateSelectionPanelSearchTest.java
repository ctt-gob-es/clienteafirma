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
import java.awt.Container;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

import javax.swing.JList;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.keystores.NameCertificateBean;

/** Pruebas del filtro del dialogo de seleccion de certificados. */
public final class CertificateSelectionPanelSearchTest {

	private static final char[] CERT_PASS = "1111".toCharArray(); //$NON-NLS-1$

	/** Comprueba que el nombre se busca sin distinguir mayusculas, tildes ni separadores. */
	@SuppressWarnings("static-method")
	@Test
	public void testFilterByName() {
		final NameCertificateBean unrelated =
				new NameCertificateBean("persona", "Persona distinta", null); //$NON-NLS-1$ //$NON-NLS-2$
		final NameCertificateBean cert =
				new NameCertificateBean("empresa", "Jos\u00E9 P\u00E9rez, EMPRESA S.L.", null); //$NON-NLS-1$ //$NON-NLS-2$

		final NameCertificateBean[] result =
				CertificateSelectionPanel.filterCertificates(
						new NameCertificateBean[] { unrelated, cert }, "jose perez empresa sl"); //$NON-NLS-1$

		Assert.assertArrayEquals(new NameCertificateBean[] { cert }, result);
	}

	/** Comprueba que el NIF se busca en el Subject del certificado.
	 * @throws Exception Cuando no se puede cargar el almacen de pruebas. */
	@SuppressWarnings("static-method")
	@Test
	public void testFilterByNif() throws Exception {
		final X509Certificate cert = loadCertificate();
		final NameCertificateBean certBean =
				new NameCertificateBean("empresa", "ANF Empresa Activo", new X509Certificate[] { cert }); //$NON-NLS-1$ //$NON-NLS-2$

		final NameCertificateBean[] result =
				CertificateSelectionPanel.filterCertificates(new NameCertificateBean[] { certBean }, "B-12345674"); //$NON-NLS-1$

		Assert.assertArrayEquals(new NameCertificateBean[] { certBean }, result);
	}

	/** Comprueba que una busqueda vacia no elimina certificados. */
	@SuppressWarnings("static-method")
	@Test
	public void testEmptyFilter() {
		final NameCertificateBean first = new NameCertificateBean("first", "Primero", null); //$NON-NLS-1$ //$NON-NLS-2$
		final NameCertificateBean second = new NameCertificateBean("second", "Segundo", null); //$NON-NLS-1$ //$NON-NLS-2$
		final NameCertificateBean[] certs = new NameCertificateBean[] { first, second };

		Assert.assertArrayEquals(certs, CertificateSelectionPanel.filterCertificates(certs, "  ")); //$NON-NLS-1$
	}

	/** Comprueba que se descartan los certificados que no coinciden con la busqueda. */
	@SuppressWarnings("static-method")
	@Test
	public void testNoMatch() {
		final NameCertificateBean cert = new NameCertificateBean("empresa", "EMPRESA S.L.", null); //$NON-NLS-1$ //$NON-NLS-2$

		Assert.assertEquals(
				0,
				CertificateSelectionPanel.filterCertificates(
						new NameCertificateBean[] { cert }, "Persona distinta").length); //$NON-NLS-1$
	}

	/** Comprueba el alias seleccionado al escribir, borrar, cambiar de vista y recargar.
	 * @throws Exception Cuando falla la carga del certificado o la prueba en el hilo de Swing. */
	@SuppressWarnings("static-method")
	@Test
	public void testLiveSearchSelection() throws Exception {
		final X509Certificate[] chain = new X509Certificate[] { loadCertificate() };
		final NameCertificateBean first = new NameCertificateBean("primero", "Primero", chain); //$NON-NLS-1$ //$NON-NLS-2$
		final NameCertificateBean second = new NameCertificateBean("segundo", "Segundo", chain); //$NON-NLS-1$ //$NON-NLS-2$
		SwingUtilities.invokeAndWait(new Runnable() {
			@Override
			public void run() {
				final CertificateSelectionPanel panel = new CertificateSelectionPanel(
						new NameCertificateBean[] { first, second }, null, null, null, false, false, null);
				final JTextField search = findSearchField(panel);
				Assert.assertNotNull(search);
				Assert.assertEquals(first.getAlias(), panel.getSelectedCertificateAlias());

				search.setText("segundo"); //$NON-NLS-1$
				Assert.assertEquals(second.getAlias(), panel.getSelectedCertificateAlias());
				Assert.assertEquals(1, ((JList<?>) panel.sPane.getViewport().getView()).getModel().getSize());

				panel.setCertLineView(CertificateLineView.REPRESENTATIVE);
				panel.updateCertListInfo();
				Assert.assertEquals(second.getAlias(), panel.getSelectedCertificateAlias());

				search.setText("sin coincidencias"); //$NON-NLS-1$
				Assert.assertNull(panel.getSelectedCertificateAlias());
				Assert.assertEquals(0, ((JList<?>) panel.sPane.getViewport().getView()).getModel().getSize());
				// Sin coincidencias no significa que el almacen carezca de certificados.
				Assert.assertEquals(2, panel.getShowedCertsCount());

				search.setText(""); //$NON-NLS-1$
				Assert.assertEquals(first.getAlias(), panel.getSelectedCertificateAlias());
				Assert.assertEquals(2, ((JList<?>) panel.sPane.getViewport().getView()).getModel().getSize());

				search.setText("segundo"); //$NON-NLS-1$
				panel.refresh(new NameCertificateBean[] { first });
				Assert.assertNull(panel.getSelectedCertificateAlias());
				panel.refresh(new NameCertificateBean[] { second, first });
				Assert.assertEquals(second.getAlias(), panel.getSelectedCertificateAlias());
			}
		});
	}

	/** Comprueba que una fila que no se puede mostrar no desplaza el alias seleccionado.
	 * @throws Exception Cuando falla la carga del certificado o la prueba en el hilo de Swing. */
	@SuppressWarnings("static-method")
	@Test
	public void testSkippedCertificateDoesNotShiftAlias() throws Exception {
		final NameCertificateBean invalid = new NameCertificateBean("invalido", "Empresa invalida", null); //$NON-NLS-1$ //$NON-NLS-2$
		final NameCertificateBean valid = new NameCertificateBean(
				"valido", "Empresa valida", new X509Certificate[] { loadCertificate() }); //$NON-NLS-1$ //$NON-NLS-2$
		SwingUtilities.invokeAndWait(new Runnable() {
			@Override
			public void run() {
				final CertificateSelectionPanel panel = new CertificateSelectionPanel(
						new NameCertificateBean[] { invalid, valid }, null, null, null, false, false, null);
				panel.setCertLineView(CertificateLineView.PERSONAL);
				panel.updateCertListInfo();
				findSearchField(panel).setText("empresa"); //$NON-NLS-1$
				Assert.assertEquals(1, ((JList<?>) panel.sPane.getViewport().getView()).getModel().getSize());
				Assert.assertEquals(valid.getAlias(), panel.getSelectedCertificateAlias());
			}
		});
	}

	private static X509Certificate loadCertificate() throws Exception {
		final KeyStore keyStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (InputStream certStore =
				CertificateSelectionPanelSearchTest.class.getResourceAsStream("/multi_almacen.p12")) { //$NON-NLS-1$
			Assert.assertNotNull(certStore);
			keyStore.load(certStore, CERT_PASS);
		}
		final X509Certificate cert = (X509Certificate) keyStore.getCertificate("anf empresa activa"); //$NON-NLS-1$
		Assert.assertNotNull(cert);
		return cert;
	}

	private static JTextField findSearchField(final Container container) {
		for (final Component component : container.getComponents()) {
			if (component instanceof JTextField) {
				return (JTextField) component;
			}
			if (component instanceof Container) {
				final JTextField field = findSearchField((Container) component);
				if (field != null) {
					return field;
				}
			}
		}
		return null;
	}
}
