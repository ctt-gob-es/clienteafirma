package es.gob.afirma.standalone.ui.hash;

import java.awt.Frame;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Di&aacute;logo para la comprobaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CheckHashDialog extends JDialog {

	/** Inicia el proceso de compobaci&oacute;n de huella digital.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCheck(final Frame parent) {
		new CheckHashDialog(parent).setVisible(true);
	}

	private static final long serialVersionUID = 8075570205961862205L;

	private final JTextField textFieldHash = new JTextField();
	void setTextFieldHashText(final String text) {
		this.textFieldHash.setText(text);
	}
	String getTextFieldHashText() {
		return this.textFieldHash.getText();
	}

	private final JTextField textFieldData = new JTextField();
	void setTextFieldDataText(final String text) {
		this.textFieldData.setText(text);
	}
	String getTextFieldDataText() {
		return this.textFieldData.getText();
	}

	private CheckHashDialog(final Frame parent) {
		super(parent);
		setModalityType(ModalityType.APPLICATION_MODAL);
		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI(parent);
				}
			}
		);
	}

	void createUI(final Frame parent) {

		setLayout(new GridBagLayout());
		setSize(300, 300);
		setLocationRelativeTo(parent);
		setIconImage(
			Toolkit.getDefaultToolkit().getImage(this.getClass().getResource("/resources/afirma_ico.png")) //$NON-NLS-1$
		);
		setTitle(SimpleAfirmaMessages.getString("CheckHashDialog.0")); //$NON-NLS-1$

		final JButton checkButton = new JButton(SimpleAfirmaMessages.getString("CheckHashDialog.1")); //$NON-NLS-1$
		checkButton.setMnemonic('C');
		checkButton.setEnabled(false);
		checkButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					try {
						if (checkHash(getTextFieldHashText(), getTextFieldDataText())) {
							AOUIFactory.showConfirmDialog(
								CheckHashDialog.this,
								SimpleAfirmaMessages.getString("CheckHashDialog.2"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("CheckHashDialog.3"), //$NON-NLS-1$
								JOptionPane.OK_OPTION,
								JOptionPane.PLAIN_MESSAGE
							);
						}
						else {
							AOUIFactory.showConfirmDialog(
								CheckHashDialog.this,
								SimpleAfirmaMessages.getString("CheckHashDialog.4"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("CheckHashDialog.5"), //$NON-NLS-1$
								JOptionPane.OK_OPTION,
								JOptionPane.WARNING_MESSAGE
							);
						}
					}
					catch(final Exception ex) {
						Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
							"No ha sido posible comprobar las huellas digitales: " + ex //$NON-NLS-1$
						);
						AOUIFactory.showErrorMessage(
							CheckHashDialog.this,
							SimpleAfirmaMessages.getString("CheckHashDialog.6"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("CheckHashDialog.7"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
					}
				}
			}
		);

		this.textFieldHash.setEditable(false);
		this.textFieldHash.setFocusable(false);
		this.textFieldHash.setColumns(10);

		final JLabel textFieldHashLabel = new JLabel(SimpleAfirmaMessages.getString("CheckHashDialog.8")); //$NON-NLS-1$
		textFieldHashLabel.setLabelFor(this.textFieldHash);

		final JButton textFieldHashButton =  new JButton(SimpleAfirmaMessages.getString("CheckHashDialog.9")); //$NON-NLS-1$
		textFieldHashButton.setMnemonic('x');
		textFieldHashButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					try {
						setTextFieldHashText(
							AOUIFactory.getLoadFiles(
								SimpleAfirmaMessages.getString("CheckHashDialog.10"), //$NON-NLS-1$
								null,
								null,
								new String[] { "hash", "txt" }, //$NON-NLS-1$ //$NON-NLS-2$
								"Huellas digitales (*.txt, *.hash)", //$NON-NLS-1$,,
								false,
								false,
								null,
								CheckHashDialog.this
							)[0].getAbsolutePath()
						);
						final String dataFile = getTextFieldDataText();
						if (!(dataFile == null) && !dataFile.isEmpty()) {
							checkButton.setEnabled(true);
						}
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
					}
				}
			}
		);

		this.textFieldData.setEditable(false);
		this.textFieldData.setFocusable(false);
		this.textFieldData.setColumns(10);

		final JLabel textFieldDataLabel = new JLabel(SimpleAfirmaMessages.getString("CheckHashDialog.11")); //$NON-NLS-1$
		textFieldDataLabel.setLabelFor(this.textFieldData);

		final JButton textFieldDataButton =  new JButton(SimpleAfirmaMessages.getString("CheckHashDialog.12")); //$NON-NLS-1$
		textFieldDataButton.setMnemonic('E');
		textFieldDataButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					try {
						setTextFieldDataText(
							AOUIFactory.getLoadFiles(
								SimpleAfirmaMessages.getString("CheckHashDialog.13"), //$NON-NLS-1$
								null,
								null,
								null,
								"Todos los archivos (*.*)", //$NON-NLS-1$,,
								false,
								false,
								null,
								CheckHashDialog.this
							)[0].getAbsolutePath()
						);
						final String hashFile = getTextFieldHashText();
						if (!(hashFile == null) && !hashFile.isEmpty()) {
							checkButton.setEnabled(true);
						}
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
					}
				}
			}
		);

		add(textFieldHashLabel);
		add(this.textFieldHash);
		add(textFieldHashButton);
		add(textFieldDataLabel);
		add(this.textFieldData);
		add(textFieldDataButton);
		add(checkButton);

	}

	static boolean checkHash(final String fileNameHash, final String fileNameData) throws IOException {
		if (fileNameHash == null || fileNameHash.isEmpty() || fileNameData == null || fileNameData.isEmpty()) {
			throw new IllegalArgumentException();
		}
		byte[] hashBytes;
		try (InputStream fis = new FileInputStream(fileNameHash)) {
			hashBytes = AOUtil.getDataFromInputStream(fis);
		}
		if (AOUtil.isBase64(hashBytes)) {
			hashBytes = Base64.decode(hashBytes, 0, hashBytes.length, false);
		}
		final byte[] dataBytes;
		try (InputStream fis = new FileInputStream(fileNameData)) {
			dataBytes = AOUtil.getDataFromInputStream(fis);
		}
		try {
			return arrayEquals(
				hashBytes,
				MessageDigest.getInstance(
					getHashAlgorithm(hashBytes)
				).digest(dataBytes)
			);
		}
		catch (final NoSuchAlgorithmException e) {
			throw new IOException(e);
		}
	}

	private static String getHashAlgorithm(final byte[] hash) {
		if (hash == null || hash.length < 1) {
			throw new IllegalArgumentException("La huella no puede ser nula ni vacia"); //$NON-NLS-1$
		}
		if (hash.length == 20) {
			return "SHA-1"; //$NON-NLS-1$
		}
		return "SHA-" + Integer.toString(hash.length*8); //$NON-NLS-1$
	}

    /** Comprueba si dos arrays de octetos son iguales.
     * @param v Primer array de octetos
     * @param w Segundo array de octetos
     * @return <code>true</code> si los arrays son iguales, <code>false</code> en caso contrario */
    private static boolean arrayEquals(final byte[] v, final byte[] w) {
        return arrayEquals(v, 0, v.length, w, 0, w.length);
    }

    /** Comprueba si dos arrays de octetos son iguales.
     * @param v Primer array de octetos
     * @param vOffset Desplazamiento (<i>offset</i>) de inicio para el primer array
     * @param vLen Longitud de los datos en el primer array
     * @param w Segundo array de octetos
     * @param wOffset Desplazamiento (<i>offset</i>) de inicio para el segundo array
     * @param wLen Longitud de los datos en el segundo array
     * @return <code>true</code> si los arrays son iguales en longitudes y valores comparados desde
     *         los respectivos desplazamientos, <code>false</code> en caso contrario */
    private static boolean arrayEquals(final byte[] v, final int vOffset, final int vLen, final byte[] w, final int wOffset, final int wLen) {
        if (vLen != wLen || v.length < vOffset + vLen || w.length < wOffset + wLen) {
            return false;
        }
        for (int i = 0; i < vLen; i++) {
            if (v[i + vOffset] != w[i + wOffset]) {
                return false;
            }
        }
        return true;
    }

}
