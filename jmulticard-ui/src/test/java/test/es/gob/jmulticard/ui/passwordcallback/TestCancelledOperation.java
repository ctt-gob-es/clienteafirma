package test.es.gob.jmulticard.ui.passwordcallback;

import java.util.logging.Logger;

import javax.swing.JOptionPane;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;
import es.gob.jmulticard.ui.passwordcallback.DialogBuilder;
import es.gob.jmulticard.ui.passwordcallback.gui.CommonPasswordCallback;

/** Prueba la cancelaci&oacue;n de di&aacute;logos. */
public class TestCancelledOperation {

	/** Prueba de cancelaci&oacute;n de di&aacute;logo de confirmaci&oacute;n. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testCancelConfirmDialog() {
		JOptionPane.showMessageDialog(null, "Pulse NO en los dialogos que se mostraran al cerrar este"); //$NON-NLS-1$
		Assert.assertEquals(1, DialogBuilder.showSignatureConfirmDialog(null, true));
		Assert.assertEquals(1, DialogBuilder.showSignatureConfirmDialog(null, true));
	}

	/** Prueba de cancelaci&oacute;n de di&aacute;logo de petici&oacute;n de PIN. */
	@SuppressWarnings("static-method")
	@Test(expected=CancelledOperationException.class)
	@Ignore
	public void testCancelPinDialogRetry() {
		JOptionPane.showMessageDialog(null, "Cancele el dialogo que se mostrara al cerrar este"); //$NON-NLS-1$
		Logger.getAnonymousLogger().info(new String(CommonPasswordCallback.getDnieBadPinPasswordCallback(2).getPassword()));
	}

	/** Prueba de cancelaci&oacute;n de di&aacute;logo de petici&oacute;n de PIN. */
	@SuppressWarnings("static-method")
	@Test(expected=CancelledOperationException.class)
	@Ignore
	public void testCancelPinDialogReadCert() {
		JOptionPane.showMessageDialog(null, "Cancele el dialogo que se mostrara al cerrar este"); //$NON-NLS-1$
		Logger.getAnonymousLogger().info(new String(CommonPasswordCallback.getDniePinForCertificateReadingPasswordCallback(null).getPassword()));
	}
}
