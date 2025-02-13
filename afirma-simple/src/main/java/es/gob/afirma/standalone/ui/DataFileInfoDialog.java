package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.text.NumberFormat;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Di&aacute;logo para mostrar la informaci&oacute;n de un fichero.
 */
public class DataFileInfoDialog {

	/** Descripci&oacute;n por defecto devuelta por la biblioteca de
	 * identificaci&oacute;n del formato de los datos. */
	private static final String DEFAULT_DESCRIPTION = "binary"; //$NON-NLS-1$

	private final JOptionPane optionPane;
	private final DataFileInfo dataInfo;
	final Component parent;

	private static String accessibleDescription = ""; //$NON-NLS-1$

	/**
	 * Construye el di&aacute;logo para visualizar los datos del fichero.
	 * @param dataInfo Informaci&oacute;n del fichero.
	 * @param parent Componente padre sobre el que mostrar el di&aacute;logo.
	 */
	public DataFileInfoDialog(final DataFileInfo dataInfo, final Component parent) {
		this.optionPane = new JOptionPane();
		this.dataInfo = dataInfo;
		this.parent = parent;
	}

	/**
	 * Muestra el di&aacute;logo con la informaci&oacute;n del fichero.
	 * @return Opci&oacute;n seleccionada por el usuario o {@code null} si no
	 * seleccion&oacute; ninguna.
	 */
	public Options show() {

		accessibleDescription = ""; //$NON-NLS-1$

        // Preparamos el dialogo
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setMessage(createInfoPanel(this.dataInfo));
		final boolean needOpenOption = this.dataInfo.getExtension() != null &&
				!this.dataInfo.getExtension().trim().isEmpty() &&
				!this.dataInfo.isExecutable();

		Options[] dialogOptions;

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			if (needOpenOption) {
				dialogOptions = new Options[] { Options.OPEN, Options.SAVE, Options.CANCEL};
			}
			else {
				dialogOptions = new Options[] { Options.SAVE, Options.CANCEL};
			}
		} else {
			// En macOS, el boton Cancelar va a la izquierda y en linux se invierte el
			// orden automaticamente, asi que lo configuramos al contrario para que quede
			// como en Windows
			if (needOpenOption) {
				dialogOptions = new Options[] { Options.CANCEL, Options.OPEN, Options.SAVE };
			}
			else {
				dialogOptions = new Options[] { Options.CANCEL, Options.SAVE };
			}
		}
		this.optionPane.setOptions(dialogOptions);

		// Mostramos el dialogo
		final JDialog dialog = this.optionPane.createDialog(this.parent, SimpleAfirmaMessages.getString("DataFileInfoDialog.3")); //$NON-NLS-1$
		dialog.getAccessibleContext().setAccessibleDescription(accessibleDescription);
		dialog.setIconImages(DesktopUtil.getIconImages());
		dialog.setVisible(true);

		return (Options) this.optionPane.getValue();
	}

	/**
	 * Crea el panel con la informaci&oacute;n de los datos.
	 * @param info Informaci&oacute;n de los datos.
	 * @return Panel con la informaci&oacute;n de los datos.
	 */
	private static JPanel createInfoPanel(final DataFileInfo info) {

		final JPanel mainPanel = new JPanel(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();

		final JPanel iconPanel = new JPanel();
		final BufferedImage icon = info.getIcon();
		if (icon != null) {
			final JLabel iconLabel = new JLabel(new ImageIcon(icon));
			iconPanel.add(iconLabel);
		}

		c.fill = GridBagConstraints.VERTICAL;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;

		mainPanel.add(iconPanel, c);


		final JPanel dataPanel = new JPanel(new GridBagLayout());

		final GridBagConstraints c2 = new GridBagConstraints();
		c2.fill = GridBagConstraints.HORIZONTAL;
		c2.insets = new Insets(3, 3, 3, 3);

		// Descripcion asociada al tipo de dato
		c2.gridx = 0;
		c2.gridy = 0;
		JLabel label = new JLabel(SimpleAfirmaMessages.getString("DataFileInfoDialog.4")); //$NON-NLS-1$
		dataPanel.add(label, c2);
		c2.insets = new Insets(3, 8, 3, 3);
		c2.gridx++;
		final String format = info.getDescription() != null &&
				!DEFAULT_DESCRIPTION.equals(info.getDescription()) ?
				info.getDescription() : SimpleAfirmaMessages.getString("DataFileInfoDialog.5"); //$NON-NLS-1$
		label = new JLabel(format);
		dataPanel.add(label, c2);
		accessibleDescription += SimpleAfirmaMessages.getString("DataFileInfoDialog.4") + format; //$NON-NLS-1$

		// Extension asociada al tipo de dato
		c2.insets = new Insets(3, 3, 3, 3);
		c2.gridx = 0;
		c2.gridy++;
		label = new JLabel(SimpleAfirmaMessages.getString("DataFileInfoDialog.6")); //$NON-NLS-1$
		dataPanel.add(label, c2);
		c2.insets = new Insets(3, 8, 3, 3);
		c2.gridx++;
		final String ext = info.getExtension() != null && !info.getExtension().trim().isEmpty() ?
				info.getExtension() :
					SimpleAfirmaMessages.getString("DataFileInfoDialog.7"); //$NON-NLS-1$
		label = new JLabel(ext);
		dataPanel.add(label, c2);
		accessibleDescription += SimpleAfirmaMessages.getString("DataFileInfoDialog.6") + ext; //$NON-NLS-1$

		// Tamano de los datos
		c2.insets = new Insets(3, 3, 3, 3);
		c2.gridx = 0;
		c2.gridy++;
		label = new JLabel(SimpleAfirmaMessages.getString("DataFileInfoDialog.8")); //$NON-NLS-1$
		dataPanel.add(label, c2);
		c2.insets = new Insets(3, 8, 3, 3);
		c2.gridx++;
		final String size = formatFileSize(info.getSize());
		label = new JLabel(size);
		dataPanel.add(label, c2);
		accessibleDescription += SimpleAfirmaMessages.getString("DataFileInfoDialog.8") + size; //$NON-NLS-1$

		c.insets = new Insets(0, 6, 0, 0);
		c.weightx = GridBagConstraints.REMAINDER;
		c.gridx++;

		mainPanel.add(dataPanel, c);

		return mainPanel;
	}

	private static String formatFileSize(final int numBytes) {

		// Medido en bytes
		if (numBytes < 1024 ) {
			return NumberFormat.getInstance().format(numBytes) + " bytes"; //$NON-NLS-1$
		}

		// Medido en KiloBytes
		final int kbs = numBytes / 1024;
		if (kbs < 1024) {
			return NumberFormat.getInstance().format(kbs) + " KB"; //$NON-NLS-1$
		}

		// Medido en MegaBytes
		final double mbs = (double) kbs / 1024;
		final double scale = Math.pow(10, 1);
		return new DecimalFormat("###,###.# MB").format(Math.round(mbs * scale) / scale); //$NON-NLS-1$
	}

	/**
	 * Opciones selecionables desde el di&aacute;logo.
	 */
	public enum Options {
		/** Opci&oacute;n para el guardado de los datos. */
		SAVE(SimpleAfirmaMessages.getString("DataFileInfoDialog.0")), //$NON-NLS-1$
		/** Opci&oacute;n para la apertura del fichero desde un temporal. */
		OPEN(SimpleAfirmaMessages.getString("DataFileInfoDialog.1")), //$NON-NLS-1$
		/** Opci&oacute;n para cancelar la operaci&oacute;n. */
		CANCEL(SimpleAfirmaMessages.getString("DataFileInfoDialog.2")); //$NON-NLS-1$

		private String text;

		Options(final String text) {
			this.text = text;
		}

		@Override
		public String toString() {
			return this.text;
		}
	}
}
