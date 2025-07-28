package es.gob.afirma.standalone.ui.plugins;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginInfo;

/**
 * Di&aacute;logo para la solicitud de permisos al usuario.
 */
public class PermissionsDialog extends JDialog implements KeyListener {

	/** Serial Id. */
	private static final long serialVersionUID = -4583047726394153567L;

	private final PluginInfo pluginInfo;

	private boolean accepted;


	PermissionsDialog(final PluginInfo pluginInfo, final Window parent) {
		super(parent);
		setTitle(SimpleAfirmaMessages.getString("PermissionsDialog.0")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);

		this.pluginInfo = pluginInfo;

		final double screenHeight = LookAndFeelManager.getScreenSize().getHeight();
		final Dimension preferedFrameSize = new Dimension(400, (int) Math.min(500, screenHeight * 0.8));
		setSize(preferedFrameSize);
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

		addWindowListener(
				new java.awt.event.WindowAdapter() {
					@Override
					public void windowClosing(final java.awt.event.WindowEvent windowEvent) {
						setVisible(false);
						dispose();
					}
				});

		createUI();
	}


	@SuppressWarnings("serial")
	void createUI() {
		setIconImages(DesktopUtil.getIconImages());
		getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PermissionsDialog.1") //$NON-NLS-1$
		);

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);

        final JLabel permissionsLabel = new JLabel() {
			@Override public Dimension getPreferredSize() {
		        return new Dimension(getParent().getSize().width, super.getPreferredSize().height);
		    }
		};
        permissionsLabel.setHorizontalAlignment(SwingConstants.LEADING);
        permissionsLabel.setVerticalAlignment(SwingConstants.TOP);

        final String permissionsText = generatePermissionsText(this.pluginInfo.getPermissions());
        permissionsLabel.setText(permissionsText);

        final JScrollPane permissionsPanel = new JScrollPane(permissionsLabel);
        permissionsPanel.setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PermissionsDialog.2"))); //$NON-NLS-1$
        permissionsPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        permissionsPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);

        final JButton okButton = new JButton(SimpleAfirmaMessages.getString("PermissionsDialog.3")); //$NON-NLS-1$
        okButton.addKeyListener(this);
        okButton.addActionListener(e -> {
			PermissionsDialog.this.accepted = true;
			dispose();
		});
		final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("PermissionsDialog.4")); //$NON-NLS-1$
		cancelButton.addKeyListener(this);
		cancelButton.addActionListener(e -> {
			PermissionsDialog.this.accepted = false;
			dispose();
		});

        final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonsPanel.add(okButton);
        buttonsPanel.add(cancelButton);

        // Colocamos los componentes
        final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(11,  11,  11,  11);
        c.add(permissionsPanel, gbc);
        gbc.gridy++;
        gbc.weighty = 0.0;
        gbc.insets = new Insets(0,  11,  11,  11);
        c.add(buttonsPanel, gbc);
	}

	private static String generatePermissionsText(final Permission[] permissions) {

		final StringBuilder buffer = new StringBuilder();
		buffer.append("<html>"); //$NON-NLS-1$
		for (final Permission p : permissions) {
			buffer.append("<b> - ") //$NON-NLS-1$
				.append(SimpleAfirmaMessages.getString("permission." + p.toString())) //$NON-NLS-1$
				.append(":</b> ") //$NON-NLS-1$
				.append(SimpleAfirmaMessages.getString("permission." + p.toString() + ".desc")) //$NON-NLS-1$ //$NON-NLS-2$
				.append("<br>"); //$NON-NLS-1$
		}
		buffer.append("</html>"); //$NON-NLS-1$
		return buffer.toString();
	}


	public boolean isAccepted() {
		return this.accepted;
	}

	@Override public void keyPressed(final KeyEvent e) { /* Vacio */ }
	@Override public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.accepted = false;
			dispose();
		}
	}
}
