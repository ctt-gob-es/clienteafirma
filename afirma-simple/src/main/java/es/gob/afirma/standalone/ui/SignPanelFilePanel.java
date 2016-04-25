package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.dnd.DropTarget;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

final class SignPanelFilePanel extends JPanel {

    private static final long serialVersionUID = -8648491975442788750L;

    private final JCheckBox pdfVisible = new JCheckBox(
		SimpleAfirmaMessages.getString("SignPanel.44"), //$NON-NLS-1$
		PreferencesManager.getBoolean(
			PreferencesManager.PREFERENCE_PADES_VISIBLE,
			false
		)
	);

    boolean isVisibleSignature() {
    	return this.pdfVisible.isSelected();
    }

    SignPanelFilePanel(final SignPanelFileType fileType,
                       final String fileSize,
                       final File file,
                       final Date fileLastModified,
                       final DropTarget dropTarget) {

        super(true);

        // Puede arrastrarse un fichero a cualquiera de estos componentes para cargarlo
        setDropTarget(dropTarget);

        SwingUtilities.invokeLater(() -> createUI(
    		fileType,
    		fileSize,
    		file,
    		fileLastModified
		));
    }

    void createUI(final SignPanelFileType fileType,
                  final String fileSize,
                  final File file,
                  final Date fileLastModified) {

        setBorder(BorderFactory.createLineBorder(Color.black));
        setLayout(new GridBagLayout());

        final JLabel pathLabel = new JLabel(file.getAbsolutePath());
        pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 3f));

        final JLabel descLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.46") + fileType.getFileDescription()); //$NON-NLS-1$
        final JLabel dateLabel = new JLabel(
    		SimpleAfirmaMessages.getString("SignPanel.47") + //$NON-NLS-1$
                DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT).format(fileLastModified)
        );

        final JLabel sizeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);

        final JPanel detailPanel = new JPanel();
        detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));
        detailPanel.add(pathLabel);
        detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
        detailPanel.add(descLabel);
        detailPanel.add(dateLabel);
        detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
        detailPanel.add(sizeLabel);

        // Definimos aqui el boton para poder crear una politica de foco si fuese necesario
        final JButton openFileButton = new JButton(SimpleAfirmaMessages.getString("SignPanel.51")); //$NON-NLS-1$

        if (SignPanelFileType.PDF.equals(fileType)) {
            this.pdfVisible.setMnemonic('H');
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(this.pdfVisible);
        }

        // Establecemos la configuracion de color
        Color bgColor = Color.WHITE;
        // Configuramos los colores
        if (!LookAndFeelManager.HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
        	bgColor = LookAndFeelManager.WINDOW_COLOR;
        }
        setBackground(bgColor);
        detailPanel.setBackground(bgColor);

    	final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.ipadx = 60;
        c.ipady = 60;
        c.insets = new Insets(11, 0, 11, 11);
        c.anchor = GridBagConstraints.NORTHWEST;
        final Component icon = fileType.getIcon();
        if (icon != null) {
        	icon.setMinimumSize(new Dimension(110,  110));
        	this.add(icon, c);
        }

        c.weightx = 1.0;
        c.gridx = 1;
        c.ipadx = 0;
        c.ipady = 0;
        c.insets = new Insets(14, 0, 11, 5);
        c.anchor = GridBagConstraints.NORTH;
        this.add(detailPanel, c);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.gridx = 2;
        c.insets = new Insets(11, 6, 11, 11);
        c.anchor = GridBagConstraints.NORTHEAST;

        openFileButton.setMnemonic('v');
        openFileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(
        		final ActionEvent ae) {
                    if (file.getName().endsWith(".csig") || file.getName().endsWith(".xsig")) { //$NON-NLS-1$ //$NON-NLS-2$
                        new VisorFirma(false, null).initialize(false, file);
                    }
                    else {
                        try {
                            Desktop.getDesktop().open(file);
                        }
                        catch (final IOException e) {
                        	AOUIFactory.showErrorMessage(
                                SignPanelFilePanel.this,
                                SimpleAfirmaMessages.getString("SignPanel.53"), //$NON-NLS-1$
                                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                                JOptionPane.ERROR_MESSAGE
                            );
                            return;
                        }
                    }
                }
            }
		);
        this.add(openFileButton, c);
        pathLabel.setLabelFor(openFileButton);
        descLabel.setLabelFor(openFileButton);
        dateLabel.setLabelFor(openFileButton);
        sizeLabel.setLabelFor(openFileButton);

    }
}
