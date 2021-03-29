package es.gob.afirma.ui.core.jse.errors;

import java.awt.Dimension;
import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

/**
 * Di&aacute;logo con las opciones de gesti&oacute;n de plugins.
 */
public final class ErrorManagementDialog extends JDialog {

	/** Serial Id. */
	private static final long serialVersionUID = -5699987186099323728L;

	private ErrorManagementDialog(final Frame parent, final boolean modal, final Object message, final String title, final int messageType, final Throwable t) {
		super(parent, modal);
		setTitle(title);
		final ErrorManagementPanel errorPanel = new ErrorManagementPanel(this, t, message, messageType);
		add(errorPanel);
		final double screenHeight = 231;
		final Dimension preferedFrameSize = new Dimension(600, (int) Math.min(550, screenHeight * 0.8));
		setSize(preferedFrameSize);
		setResizable(false);
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}

	/**
	 * Permite mostrar el di&aacute;logo con el error
	 * @param parent frame donde mostrar el di&aacute;logo
	 * @param modal indica si es modal o no al constructor
	 * @param message mensaje de error
	 * @param title titulo para la ventana de di&aacute;logo
	 * @param messageType tipo de mensaje (error o advertencia)
	 * @param t informaci&oacute;n sobre el error
	 */
	public static void show(final Frame parent, final boolean modal, final Object message, final String title, final int messageType, final Throwable t) {
		new ErrorManagementDialog(parent, modal, message, title, messageType, t).setVisible(true);
	}

}
