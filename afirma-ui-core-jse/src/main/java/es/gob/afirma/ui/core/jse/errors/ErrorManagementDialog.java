package es.gob.afirma.ui.core.jse.errors;

import java.awt.Frame;

import javax.accessibility.Accessible;
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

		if (message instanceof Accessible) {
			getAccessibleContext().setAccessibleDescription(
					((Accessible) message).getAccessibleContext().getAccessibleDescription());
		} else if (message instanceof String) {
			getAccessibleContext().setAccessibleDescription((String) message);
		}

		setResizable(false);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setEnabled(true);
		setAlwaysOnTop(true);

		resize();
	}

	private ErrorManagementDialog(final JDialog parent, final boolean modal, final Object message, final String title, final int messageType, final Throwable t) {
		super(parent, modal);
		setTitle(title);
		final ErrorManagementPanel errorPanel = new ErrorManagementPanel(this, t, message, messageType);
		add(errorPanel);

		if (message instanceof Accessible) {
			getAccessibleContext().setAccessibleDescription(
					((Accessible) message).getAccessibleContext().getAccessibleDescription());
		} else if (message instanceof String) {
			getAccessibleContext().setAccessibleDescription((String) message);
		}

		setResizable(false);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setEnabled(true);
		setAlwaysOnTop(true);

		resize();
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

	/**
	 * Permite mostrar el di&aacute;logo con el error pero pasa un padre como JDialog
	 * @param dialog donde mostrar el di&aacute;logo
	 * @param modal indica si es modal o no al constructor
	 * @param message mensaje de error
	 * @param title titulo para la ventana de di&aacute;logo
	 * @param messageType tipo de mensaje (error o advertencia)
	 * @param t informaci&oacute;n sobre el error
	 */
	public static void show(final JDialog dialog, final boolean modal, final Object message, final String title, final int messageType, final Throwable t) {
		new ErrorManagementDialog(dialog, modal, message, title, messageType, t).setVisible(true);
	}

	/**
	 * Redimensiona el dialogo para ajustarse a su contenido y lo reposiciona en pantalla.
	 */
	public void resize() {
		pack();
		setLocationRelativeTo(getParent());
	}
}
