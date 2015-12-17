package es.gob.afirma.standalone.ui;

import java.awt.Cursor;
import java.awt.Frame;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import es.gob.afirma.cert.certvalidation.CertificateVerifierFactory;
import es.gob.afirma.cert.certvalidation.CertificateVerifierFactoryException;
import es.gob.afirma.cert.certvalidation.ValidationResult;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;

final class MenuValidation extends JMenu {

	private static final long serialVersionUID = 7740685576418222937L;

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String BEGIN_CERTIFICATE = "-----BEGIN CERTIFICATE-----"; //$NON-NLS-1$
	private static final String END_CERTIFICATE = "-----END CERTIFICATE-----"; //$NON-NLS-1$

	private final Frame parent;

	MenuValidation(final Frame p) {
		this.parent = p;
		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI();
				}
			}
		);
	}

	void createUI() {

		setText(SimpleAfirmaMessages.getString("MenuValidation.0")); //$NON-NLS-1$
        setMnemonic(KeyEvent.VK_V);

        final JMenuItem certValidationMenu = new JMenuItem(
    		SimpleAfirmaMessages.getString("MenuValidation.1") //$NON-NLS-1$
		);
        certValidationMenu.setAccelerator(
    		KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		);
        certValidationMenu.setMnemonic(KeyEvent.VK_C);
        certValidationMenu.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MenuValidation.2") //$NON-NLS-1$
		);
        certValidationMenu.addActionListener(
    		new ActionListener() {
            	/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
				    validateCert();
				}
			}
		);
        add(certValidationMenu);

        final JMenuItem signValidationMenu = new JMenuItem(
    		SimpleAfirmaMessages.getString("MenuValidation.12") //$NON-NLS-1$
		);
        signValidationMenu.setAccelerator(
    		KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		);
        signValidationMenu.setMnemonic(KeyEvent.VK_I);
        certValidationMenu.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MenuValidation.13") //$NON-NLS-1$
		);
        signValidationMenu.addActionListener(
    		new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					validateSign();
				}
			}
		);
        add(signValidationMenu);
	}

	void validateSign() {
		Image icon = null;
		try {
			icon = ImageIO.read(MenuValidation.class.getResource("/resources/afirma_ico.png")); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No ha podido cargarse el icono del dialogo: " + e //$NON-NLS-1$
			);
		}
		final File sign;
		try {
			sign = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("MenuValidation.16"), //$NON-NLS-1$
				null,
				null,
				null,
				SimpleAfirmaMessages.getString("MenuValidation.17"), //$NON-NLS-1$
				false,
				false,
				icon,
				this.parent
			)[0];
		}
		catch(final AOCancelledOperationException e) {
			return;
		}
		if (!sign.canRead()) {
			AOUIFactory.showErrorMessage(
				icon,
				SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}

		new VisorFirma(false, this.parent).initialize(false, sign);
	}

	void validateCert() {
		Image icon = null;
		try {
			icon = ImageIO.read(MenuValidation.class.getResource("/resources/certificate_16.png")); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No ha podido cargarse el icono del dialogo: " + e //$NON-NLS-1$
			);
		}
		final File cert;
		try {
			cert = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("MenuValidation.3"), //$NON-NLS-1$
				null,
				null,
				new String[] { "cert", "cer", "crt", "pem" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("MenuValidation.4"), //$NON-NLS-1$
				false,
				false,
				icon,
				this.parent
			)[0];
		}
		catch(final AOCancelledOperationException e) {
			return;
		}
		if (!cert.canRead()) {
			AOUIFactory.showErrorMessage(
				icon,
				SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		byte[] certBytes;
		try ( final InputStream fis = new FileInputStream(cert); ) {
			certBytes = AOUtil.getDataFromInputStream(fis);
		}
		catch(final Exception e) {
			LOGGER.severe("Error leyendo el fichero de certificado (" + cert.getAbsolutePath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			AOUIFactory.showErrorMessage(
				icon,
				SimpleAfirmaMessages.getString("MenuValidation.7"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		final String certString = new String(certBytes);
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
				LOGGER.severe("Certificado PEM corrupto (" + cert.getAbsolutePath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
				AOUIFactory.showErrorMessage(
					icon,
					SimpleAfirmaMessages.getString("MenuValidation.8"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return;
			}
		}
		final X509Certificate certificate;
		try {
			certificate = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
				new ByteArrayInputStream(certBytes)
			);
		}
		catch (final CertificateException e) {
			LOGGER.severe("Error en la generacion del certificado (" + cert.getAbsolutePath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			AOUIFactory.showErrorMessage(
				icon,
				SimpleAfirmaMessages.getString("MenuValidation.8"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}

		(this.parent != null ? this.parent : this).setCursor(new Cursor(Cursor.WAIT_CURSOR));

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
			(this.parent != null ? this.parent : this).setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
				this.parent,
				SimpleAfirmaMessages.getString("MenuValidation.14"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.15"), //$NON-NLS-1$
				JOptionPane.INFORMATION_MESSAGE
			);
		}

		(this.parent != null ? this.parent : this).setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

	}

}
