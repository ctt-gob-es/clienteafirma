package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

public final class RestoreConfigDialog extends JDialog {
	
	/**
	 * Attribute that represents the serial version.
	 */
	private static final long serialVersionUID = -241482490367263150L;

	/** Constructor del panel de preferencias.
	 * @param parent padre del panel de preferencias.
	 * @param modal modal del panel de preferencias.
	 */ 
	public RestoreConfigDialog(final Frame parent, final boolean modal) {
		super(parent, modal);
		setTitle(SimpleAfirmaMessages.getString("MainMenu.20")); //$NON-NLS-1$
		this.add(new RestoreConfigPanel(this));
		this.setSize(600, 550);
		setResizable(false);
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}
	
	/** Muestra el di&aacute;logo de restauracion de configuracion.
	 * @param parent Componente padre del panel de preferencias.
	 * @param modal Modalidad del panel de preferencias.
	 * @param selectedPreferencesTabIndex &Iacute;dice de la pesta&ntilde;a de configuraci&oacute;n seleccionada por defecto. */
	public static void show(final Frame parent, final boolean modal, final int selectedPreferencesTabIndex) {
		new RestoreConfigDialog(parent, modal).setVisible(true);
	}

	/** Muestra el di&aacute;logo de restauracion de configuracion.
	 * @param parent Componente padre del panel de restauracion de configuracion.
	 * @param modal Modalidad del panel de restauracion de configuracion. */
	public static void show(final Frame parent, final boolean modal) {
		new RestoreConfigDialog(parent, modal).setVisible(true);
	}

}
