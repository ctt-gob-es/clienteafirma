package test.es.gob.jmulticard.ui.passwordcallback;

import javax.swing.JOptionPane;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.jmulticard.ui.passwordcallback.DialogBuilder;
import es.gob.jmulticard.ui.passwordcallback.gui.CommonPasswordCallback;

/** Prueba simple del di&aacute;logo del PIN. */
public class TestPinDialog {

    /** Prueba simple del di&aacute;logo del PIN.
     * @throws Exception En caso de cualquier problema */
    @SuppressWarnings("static-method")
    @Test
    @Ignore
    public void testPinInput() throws Exception {
        JOptionPane.showMessageDialog(null,
                                      "Redimensione el dialogo siguiente de introduccion de PIN, esciba caracteres hasta alcanzar el maximo, muestre y oculte los caracteres. Maximice y minimice"); //$NON-NLS-1$
        System.out.println(new String(CommonPasswordCallback.getDniePinForCertificateReadingPasswordCallback(null).getPassword()));
    }

    /** Prueba simple del di&aacute;logo del PIN entre dos confirmaciones.
     * @throws Exception En caso de cualquier problema */
    @SuppressWarnings("static-method")
    @Test
    @Ignore
    public void testConfirm() throws Exception {
        JOptionPane.showMessageDialog(null, "Redimensione el dialogo siguiente de confirmacion de firma. Maximice y minimice"); //$NON-NLS-1$
        System.out.println(DialogBuilder.showSignatureConfirmDialog(null, true));
        System.out.println(new String(CommonPasswordCallback.getDniePinForCertificateReadingPasswordCallback(null).getPassword()));
        System.out.println(DialogBuilder.showSignatureConfirmDialog(null, true));
    }

}
