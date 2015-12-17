package es.gob.afirma.ui.utils;

import static org.junit.Assert.assertTrue;

import java.awt.event.KeyEvent;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Testeo de funcionalidades relativas a la accesibilidad de la clase CustomDialog
 * @author inteco
 *
 */
public class CustomDialogTest {

	/**
	 * Log.
	 */
	private static Logger LOGGER = Logger.getLogger(CustomDialogTest.class.getName());

	/**
	 * Comprobaci&oacute;n de las caracter&iacute;sticas de accesibilidad de los CustomDialog
	 */
	@Ignore
	@Test
	public void testShowMessageDialog(){
		LOGGER.info("testShowMessageDialog"); //$NON-NLS-1$

		try{
			final String message = new String("Test"); //$NON-NLS-1$
			final String title = new String("Test"); //$NON-NLS-1$
			CustomDialog.showMessageDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE);
			final CustomDialog cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE, false);
			assertTrue(checkComponentMessageDialog(cD));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

	/**
	 * Comprobaci&oacute;n de las caracter&iacute;sticas de accesibilidad de los CustomDialog
	 */
	@Ignore
	@Test
	public void testShowConfirmDialog(){
		LOGGER.info("testShowConfirmDialog"); //$NON-NLS-1$

		try{
			final String message = new String("Test"); //$NON-NLS-1$
			final String title = new String("Test"); //$NON-NLS-1$
			CustomDialog.showConfirmDialog(null, true, message, title,JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE);
			CustomDialog cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE, false);
			assertTrue(checkComponentMessageDialog(cD));
			CustomDialog.showConfirmDialog(null, true, message, title,JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.INFORMATION_MESSAGE);
			cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE, false);
			assertTrue(checkComponentMessageDialog(cD));
			CustomDialog.showConfirmDialog(null, true, message, title,JOptionPane.OK_CANCEL_OPTION, JOptionPane.INFORMATION_MESSAGE);
			cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE, false);
			assertTrue(checkComponentMessageDialog(cD));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

	/**
	 * Comprobaci&oacute;n de las caracter&iacute;sticas de accesibilidad de los CustomDialog
	 */
	@Ignore
	@Test
	public void testShowInputDialog(){
		LOGGER.info("testShowInputDialog"); //$NON-NLS-1$

		try{
			final String message = new String("Test"); //$NON-NLS-1$
			final String title = new String("Test"); //$NON-NLS-1$
			CustomDialog.showInputDialog(null, true, message,KeyEvent.VK_E ,title, JOptionPane.INFORMATION_MESSAGE);
			final CustomDialog cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.INFORMATION_MESSAGE, true);
			assertTrue(checkComponentMessageDialog(cD));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

	/**
	 * Comprobaci&oacute;n de las caracter&iacute;sticas de accesibilidad de los CustomDialog
	 */
	@Ignore
	@Test
	public void testShowInputPasswordDialog(){
		LOGGER.info("testShowInputPasswordDialog"); //$NON-NLS-1$

		try{
			final String message = new String("Test"); //$NON-NLS-1$
			final String title = new String("Test"); //$NON-NLS-1$
			String charSet;

			charSet = null;
			CustomDialog.showInputPasswordDialog(null, true, charSet,false,message,KeyEvent.VK_E ,title, JOptionPane.QUESTION_MESSAGE);
			final CustomDialog cD = CustomDialog.getInstanceCustomDialog(null, true, message, title, JOptionPane.QUESTION_MESSAGE, true);
			assertTrue(checkComponentMessageDialog(cD));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

	/**
	 * Comprueba si la etiqueta del CustomDialog puede recibir el foco para ser leida por los lectores de pantalla.
	 * Comprueba si los botones del CustomDialog tienen asiganado un mnem&oacute;nico.
	 * @param cD CustomDialog a comprobar
	 * @return boolean Resultado de la validaci&oacute;n
	 */
	@SuppressWarnings("static-method")
	@Ignore
	public boolean checkComponentMessageDialog(final CustomDialog cD){
		for (int i = 0; i<cD.getComponentCount();i++){
			if (cD.getComponent(i) instanceof JRootPane){
				final JRootPane jRP = (JRootPane)cD.getComponent(i);
				for (int j = 0; j<jRP.getComponentCount();j++){
					if (jRP.getComponent(j) instanceof JLayeredPane){
						final JLayeredPane jLP = (JLayeredPane)jRP.getComponent(j);
						for (int k=0;k<jLP.getComponentCount();k++){
							if (jLP.getComponent(k) instanceof JPanel){
								final JPanel jP = (JPanel)jLP.getComponent(k);
								for (int l = 0;l<jP.getComponentCount();l++){
									if (jP.getComponent(l) instanceof JPanel){
										final JPanel jP2 = (JPanel)jP.getComponent(l);
										for (int m=0;m<jP2.getComponentCount();m++){
											if (jP2.getComponent(m) instanceof InfoLabel){
												final InfoLabel iL = (InfoLabel)jP2.getComponent(m);
												if (!iL.isFocusable()){
													return false;
												}
											}
											if (jP2.getComponent(m) instanceof JPanel){
												final JPanel jP3 = (JPanel)jP2.getComponent(m);
												for (int n=0;n<jP3.getComponentCount();n++){
													if (jP3.getComponent(n) instanceof JButton){
														final JButton jB = (JButton)jP3.getComponent(n);
														if (jB.getMnemonic()==0){
															return false;
														}
													}
													if (jP3.getComponent(n) instanceof JPanel){
														final JPanel jP4 = (JPanel)jP3.getComponent(n);
														for (int c=0;c<jP4.getComponentCount();c++){
															if (jP4.getComponent(c) instanceof JButton){
																final JButton jB2 = (JButton)jP4.getComponent(c);
																if (jB2.getMnemonic()==0){
																	return false;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}

		}
		return true;
	}

}
