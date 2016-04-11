package es.gob.afirma.standalone.ui.preferences;

import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Di&aacute;logo de preferencias de Autofirma. */
public final class PreferencesDialog extends JDialog{

	private static final long serialVersionUID = 1L;

	/** Constructor del panel de preferencias.
	 * @param parent padre del panel de preferencias.
	 * @param modal modal del panel de preferencias.
	 * @param selectedPreferencesTabIndex &Iacute;dice de la pesta&ntilde;a de configuraci&oacute;n seleccionada por defecto. */
	public PreferencesDialog(final Frame parent, final boolean modal, final int selectedPreferencesTabIndex) {
		super(parent, modal);
		setTitle(SimpleAfirmaMessages.getString("MainMenu.24")); //$NON-NLS-1$
		this.add(new PreferencesPanel(this, selectedPreferencesTabIndex));
		this.setSize(800, 715);
		setResizable(false);
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}

	/** Constructor del panel de preferencias.
	 * @param parent padre del panel de preferencias.
	 * @param modal modal del panel de preferencias. */
	public PreferencesDialog(final Frame parent, final boolean modal) {
		this(parent, modal, 0);
	}

	/** Muestra el di&aacute;logo de preferencias.
	 * @param parent Componente padre del panel de preferencias.
	 * @param modal Modalidad del panel de preferencias.
	 * @param selectedPreferencesTabIndex &Iacute;dice de la pesta&ntilde;a de configuraci&oacute;n seleccionada por defecto. */
	public static void show(final Frame parent, final boolean modal, final int selectedPreferencesTabIndex) {
		new PreferencesDialog(parent, modal, selectedPreferencesTabIndex).setVisible(true);
	}

	/** Muestra el di&aacute;logo de preferencias.
	 * @param parent Componente padre del panel de preferencias.
	 * @param modal Modalidad del panel de preferencias. */
	public static void show(final Frame parent, final boolean modal) {
		new PreferencesDialog(parent, modal, 0).setVisible(true);
	}

}
