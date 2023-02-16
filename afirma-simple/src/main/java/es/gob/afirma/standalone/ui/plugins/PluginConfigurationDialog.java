package es.gob.afirma.standalone.ui.plugins;

import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Window;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.ConfigurationPanel;
import es.gob.afirma.standalone.plugins.manager.PluginException;

/**
 * Di&aacute;logo base sobre el que se sit&uacute;a el panel para la
 * configuraci&oacute;n de un plugin.
 */
public class PluginConfigurationDialog extends JDialog {

	/** Serial Id. */
	private static final long serialVersionUID = 4377003209574629672L;

	private final ConfigurationPanel configPanel;

	boolean accepted;

	/**
	 * Construye el di&aacute;logo para mostrar la configuraci&oacute;n de un plugin.
	 * @param parent Ventana padre sobre el que se sit&uacute;a.
	 * @param plugin Plugin del que mostrar la configuraci&oacute;n.
	 * @throws PluginException Cuando ocurre un error al cargar el di&aacute;logo.
	 */
	public PluginConfigurationDialog(final Window parent, final AfirmaPlugin plugin) throws PluginException {
		super(parent);

		try {
			this.configPanel = plugin.buildConfigurationPanel();
		} catch (final Exception e) {
			throw new PluginException("Error al construir el dialogo de configuracion", e); //$NON-NLS-1$
		}
		this.accepted = false;

		setTitle(plugin.getInfo().getName());
		setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridy = 0;
		add(this.configPanel, c);
		c.gridy++;
		c.insets = new Insets(11, 0, 0, 0);
		add(createButtonsPanel(), c);
		setResizable(true);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		pack();

		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		setLocation(cp.x - getWidth()/2, cp.y - getHeight()/2);
	}

	private JPanel createButtonsPanel() {
		final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		final JButton okButton = new JButton(SimpleAfirmaMessages.getString("PluginConfigurationDialog.0")); //$NON-NLS-1$
		okButton.addActionListener(e -> {
			PluginConfigurationDialog.this.accepted = true;
			dispose();
		});
		final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("PluginConfigurationDialog.1")); //$NON-NLS-1$
		cancelButton.addActionListener(e -> {
			PluginConfigurationDialog.this.accepted = false;
			dispose();
		});

		panel.add(okButton);
		panel.add(cancelButton);

		return panel;
	}


	/**
	 * Carga en el panel de configuraci&oacute;n del plugin la configuraci&oacute;n
	 * actualmente almacenada.
	 * @param config Configuraci&oacute;n almacenada.
	 */
	public void init(final Properties config) {
		this.accepted = false;
		this.configPanel.init(config);
	}

	/**
	 * Obtiene los valores establecidos en el panel de configuraci&oacute;n para poder
	 * almacenarlos.
	 * @return Valores configurados en el panel.
	 */
	public Properties recoverConfig() {
		return this.configPanel.getConfiguration();
	}

	/**
	 * Indica si se aceptaron los cambios en el panel de configuraci&oacute;n.
	 * @return {@code true} si se aceptaron los cambios y hay que guardar la
	 * configuraci&oacute;n del panel. {@code false} en caso contrario.
	 */
	public boolean isAccepted() {
		return this.accepted;
	}
}
