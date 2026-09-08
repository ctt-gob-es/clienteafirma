package es.gob.afirma.ui.core.jse;

import es.gob.afirma.core.keystores.NameCertificateBean;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;
import org.junit.Ignore;
import org.junit.Test;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.junit.Assert.*;

/**
 * Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci
 */
public class CertificateSelectionDialogTest {


    private static final String CERT_PATH = "multi_almacen.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prueba de di&aacute;logo de selecci&oacute;n de certificados.
	 * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void showCertDialogTest() throws Exception {

		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.PKCS12,
				ClassLoader.getSystemResource(CERT_PATH).toString().replace("file:/", ""), //$NON-NLS-1$ //$NON-NLS-2$
				null,
				new CachePasswordCallback(CERT_PASS.toCharArray()),
				null,
				false);

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, true, false);
		String alias;
		try {
			alias = dialog.show();
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error al cargar un certificado a traves del dialogo de seleccion", e); //$NON-NLS-1$
			e.printStackTrace();
			return;
		}

		LOGGER.info("Certificado con numero de serie:\n" + ksm.getCertificate(alias).getSerialNumber()); //$NON-NLS-1$
	}

	/** Comprueba el ciclo de vida visual del mensaje de error sin abrir una ventana. */
	@Test
	public void errorMessageIsDisplayedAndUpdatedBeforeRefresh() throws Exception {
		final Class<?> panelClass = Class.forName(
				"es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionPanel" //$NON-NLS-1$
		);
		final Constructor<?> constructor = panelClass.getDeclaredConstructor(
				NameCertificateBean[].class,
				CertificateSelectionDialog.class,
				String.class,
				String.class,
				String.class,
				boolean.class,
				boolean.class,
				es.gob.afirma.core.keystores.KeyStoreType[].class
		);
		constructor.setAccessible(true);

		final Object panel = constructor.newInstance(
				new NameCertificateBean[0],
				null,
				"Cabecera", //$NON-NLS-1$
				null,
				"error-inicial", //$NON-NLS-1$
				Boolean.FALSE,
				Boolean.FALSE,
				null
		);

		final JLabel errorLabel = findLabel(panel, "error-inicial"); //$NON-NLS-1$
		assertNotNull(errorLabel);
		assertTrue(errorLabel.isVisible());
		assertErrorLabelIsBetweenSeparatorAndList(panel, panelClass, errorLabel);

		final Method setErrorMessage = panelClass.getMethod("setErrorMessage", String.class); //$NON-NLS-1$
		final Method refresh = panelClass.getDeclaredMethod("refresh", NameCertificateBean[].class); //$NON-NLS-1$
		refresh.setAccessible(true);

		setErrorMessage.invoke(panel, "error-actualizado"); //$NON-NLS-1$
		refresh.invoke(panel, new Object[] { new NameCertificateBean[0] });
		assertEquals("error-actualizado", errorLabel.getText()); //$NON-NLS-1$
		assertTrue(errorLabel.isVisible());

		setErrorMessage.invoke(panel, new Object[] { null });
		refresh.invoke(panel, new Object[] { new NameCertificateBean[0] });
		assertFalse(errorLabel.isVisible());
		assertEquals(null, errorLabel.getText());
	}

	private static JLabel findLabel(final Object panel, final String text) {
		for (final Component component : ((JPanel) panel).getComponents()) {
			if (component instanceof JLabel && text.equals(((JLabel) component).getText())) {
				return (JLabel) component;
			}
		}
		return null;
	}

	private static void assertErrorLabelIsBetweenSeparatorAndList(final Object panel,
			final Class<?> panelClass, final JLabel errorLabel) throws Exception {
		final Field certListPanelField = panelClass.getDeclaredField("certListPanel"); //$NON-NLS-1$
		certListPanelField.setAccessible(true);
		final JPanel certListPanel = (JPanel) certListPanelField.get(panel);
		final Component[] components = ((JPanel) panel).getComponents();
		int separatorIndex = -1;
		int errorIndex = -1;
		int listIndex = -1;
		for (int i = 0; i < components.length; i++) {
			if (components[i] instanceof JSeparator) {
				separatorIndex = i;
			}
			if (components[i] == errorLabel) {
				errorIndex = i;
			}
			if (components[i] == certListPanel) {
				listIndex = i;
			}
		}
		assertTrue(separatorIndex >= 0);
		assertTrue(errorIndex > separatorIndex);
		assertTrue(listIndex > errorIndex);
	}

}
