package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class DocumentInfoPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 1627010163673368615L;

	private String accesibleDescription;
	final JLabel signsDetailsLbl = new JLabel();

	public DocumentInfoPanel(final SignOperationConfig signConfig, final Color bgColor) {
		createUI(signConfig, bgColor);
	}

	private void createUI(final SignOperationConfig signConfig, final Color bgColor) {

		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
			setBackground(bgColor);
		}

		// Tipo de documento
		final JLabel descLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.46", signConfig.getFileType().getFileDescription())); //$NON-NLS-1$
		this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.46", signConfig.getFileType().getFileDescription()); //$NON-NLS-1$

		// Fecha de modificacion
		final Date fileLastModified = new Date(signConfig.getDataFile().lastModified());
		final JLabel dateLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.47",  //$NON-NLS-1$
        				DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT)
        				.format(fileLastModified))
        		);
		this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.47",  //$NON-NLS-1$
				DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT)
				.format(fileLastModified));

		// Tamano
		final String fileSize = NumberFormat.getNumberInstance().format(signConfig.getDataFile().length() / 1024);
        final JLabel sizeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
        this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        add(Box.createRigidArea(new Dimension(0, 4)));
		add(descLabel);
		add(Box.createRigidArea(new Dimension(0, 4)));
		add(dateLabel);
		add(Box.createRigidArea(new Dimension(0, 4)));
		add(sizeLabel);
	}

	public String getAccesibleDescription() {
		return this.accesibleDescription;
	}
}
