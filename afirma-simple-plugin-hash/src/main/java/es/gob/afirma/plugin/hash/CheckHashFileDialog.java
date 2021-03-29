/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.hash;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Date;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.plugins.UIFactory;

/** Di&aacute;logo para la comprobaci&oacute;n de huellas digitales. */
public final class CheckHashFileDialog extends JDialog implements KeyListener {

	private static final long serialVersionUID = 2431770911918905439L;

	static final Logger LOGGER = Logger.getLogger(CheckHashFileDialog.class.getName());

	private static final int SIZE_WAIT = 50000000; //Tamano en bytes

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

	private CheckHashFileDialog(final Window parent) {
		super(parent);
		setModalityType(ModalityType.APPLICATION_MODAL);
		createUI(parent);
	}

	void createUI(final Window parent) {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(5,15,0,10);

		final Image icon = getDialogIcon();
		setIconImage(icon);
		setTitle(Messages.getString("CheckHashDialog.0")); //$NON-NLS-1$

		final JButton checkButton = new JButton(Messages.getString("CheckHashDialog.1")); //$NON-NLS-1$
		checkButton.addKeyListener(this);
		checkButton.setMnemonic('C');
		checkButton.setEnabled(false);
		checkButton.addActionListener(
			e -> {
				CheckHashFileDialog.this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				try {
					if (checkHash(getTextFieldHashText(), getTextFieldDataText())) {
						AOUIFactory.showMessageDialog(
							CheckHashFileDialog.this,
							Messages.getString("CheckHashDialog.2"), //$NON-NLS-1$
							Messages.getString("CheckHashDialog.3"), //$NON-NLS-1$
							JOptionPane.INFORMATION_MESSAGE
						);
					}
					else {
						AOUIFactory.showMessageDialog(
							CheckHashFileDialog.this,
							Messages.getString("CheckHashDialog.4"), //$NON-NLS-1$
							Messages.getString("CheckHashDialog.5"), //$NON-NLS-1$
							JOptionPane.WARNING_MESSAGE
						);
					}
				}
				catch(final OutOfMemoryError ooe) {
					AOUIFactory.showErrorMessage(
						Messages.getString("CreateHashDialog.18"), //$NON-NLS-1$
						Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ooe
					);
					LOGGER.log(Level.SEVERE, "Fichero demasiado grande", ooe); //$NON-NLS-1$
					return;
				}
				catch(final Exception ex) {
					LOGGER.log(Level.SEVERE, "No ha sido posible comprobar las huellas digitales", ex); //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						CheckHashFileDialog.this,
						Messages.getString("CheckHashDialog.6"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ex
					);
				}
				finally {
					CheckHashFileDialog.this.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				}
			}
		);

		this.textFieldHash.addKeyListener(this);
		this.textFieldHash.setEditable(false);
		this.textFieldHash.setFocusable(false);
		this.textFieldHash.setColumns(10);

		final JLabel textFieldHashLabel = new JLabel(Messages.getString("CheckHashDialog.8")); //$NON-NLS-1$
		textFieldHashLabel.setLabelFor(this.textFieldHash);
		textFieldHashLabel.addKeyListener(this);

		final JButton textFieldHashButton =  new JButton(Messages.getString("CheckHashDialog.9")); //$NON-NLS-1$
		textFieldHashButton.addKeyListener(this);
		textFieldHashButton.setMnemonic('x');
		textFieldHashButton.addActionListener(
			ae -> {
				try {
					setTextFieldHashText(
						AOUIFactory.getLoadFiles(
							Messages.getString("CheckHashDialog.10"), //$NON-NLS-1$
							null,
							null,
							new String[] { "hash", "hashb64", "hexhash" }, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							Messages.getString("CheckHashDialog.14"), //$NON-NLS-1$
							false,
							false,
							icon,
							CheckHashFileDialog.this
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
		);

		this.textFieldData.addKeyListener(this);
		this.textFieldData.setEditable(false);
		this.textFieldData.setFocusable(false);
		this.textFieldData.setColumns(10);

		final JLabel textFieldDataLabel = new JLabel(Messages.getString("CheckHashDialog.11")); //$NON-NLS-1$
		textFieldDataLabel.setLabelFor(this.textFieldData);
		textFieldDataLabel.addKeyListener(this);

		final JButton textFieldDataButton =  new JButton(Messages.getString("CheckHashDialog.12")); //$NON-NLS-1$
		textFieldDataButton.addKeyListener(this);
		textFieldDataButton.setMnemonic('E');
		textFieldDataButton.addActionListener(
			ae -> {
				try {
					setTextFieldDataText(
						AOUIFactory.getLoadFiles(
							Messages.getString("CheckHashDialog.13"), //$NON-NLS-1$
							null,
							null,
							null,
							Messages.getString("CheckHashDialog.14"), //$NON-NLS-1$
							false,
							false,
							icon,
							CheckHashFileDialog.this
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
		);

		final JButton exitButton = new JButton(
			Messages.getString("CheckHashDialog.15") //$NON-NLS-1$
		);

		exitButton.setMnemonic('C');
		exitButton.addActionListener(
			e -> {
				CheckHashFileDialog.this.setVisible(false);
				CheckHashFileDialog.this.dispose();
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("CheckHashDialog.16") //$NON-NLS-1$
		);
		exitButton.addKeyListener(this);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(exitButton);
			panel.add(checkButton);
		}
		else {
			panel.add(checkButton);
			panel.add(exitButton);
		}

		c.add(textFieldDataLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(5,10,0,10);
		c.add(this.textFieldData, gbc);
		gbc.weightx = 0;
		c.add(textFieldDataButton, gbc);
		gbc.insets = new Insets(30,15,0,10);
		gbc.weightx = 1.0;
		gbc.gridy++;
		c.add(textFieldHashLabel, gbc);
		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.textFieldHash, gbc);
		gbc.weightx = 0;
		c.add(textFieldHashButton, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(30,10,0,10);
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		c.add(panel, gbc);

	}

	static boolean checkHash(final String fileNameHash, final String fileNameData)
			throws InterruptedException, ExecutionException {
		if (fileNameHash == null || fileNameHash.isEmpty() || fileNameData == null || fileNameData.isEmpty()) {
			throw new IllegalArgumentException();
		}

		// Se crea la ventana de espera
		JDialog dialog;
		try {
			dialog = UIFactory.getWaitingDialog(
					null,
					Messages.getString("CreateHashFiles.21"), //$NON-NLS-1$
					Messages.getString("CreateHashFiles.22") //$NON-NLS-1$
					);
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el dialogo de espera: " + e); //$NON-NLS-1$
			dialog = null;
		}

		return checkHash(new File(fileNameHash), new File(fileNameData), dialog);
	}

	/**
	 * Comprueba que el hash de un fichero sea correcto.
	 * @param hashFile Fichero con el hash del fichero de datos.
	 * @param dataFile Fichero de datos del que comprobar el hash.
	 * @param waitingDialog Di&aacute;logo gr&aacute;fico de espera.
	 * @return Devuelve {@code true} si el hash es correcto, {@code false} en caso contrario.
	 * @throws InterruptedException Si se interrumpe la comprobaci&oacute;n.
	 * @throws ExecutionException Si ocurre un error durante la comprobaci&oacute;n.
	 */
	public static boolean checkHash(final File hashFile, final File dataFile, final Dialog waitingDialog)
			throws InterruptedException, ExecutionException {

		// Arrancamos el proceso en un hilo aparte
		final SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

			@Override
			protected Boolean doInBackground() throws Exception {

				byte[] hashBytes;
				try (InputStream fis = new FileInputStream(hashFile)) {
					hashBytes = AOUtil.getDataFromInputStream(fis);
				}
				if (Base64.isBase64(hashBytes)) {
					hashBytes = Base64.decode(hashBytes, 0, hashBytes.length, false);
				}
				else if(HexUtils.isHexadecimal(hashBytes)) {
					hashBytes = HexUtils.hexStringToByteArray(
						new String(hashBytes).substring(0, hashBytes.length -1)
					);
				}

				final long startTime = new Date().getTime();

				final byte[] calculatedHash;
				try {
					calculatedHash = HashUtil.getFileHash(
							getHashAlgorithm(hashBytes),
							dataFile);
				}
				catch (final NoSuchAlgorithmException e) {
					throw new IOException(e);
				}

				final long processTime = new Date().getTime() - startTime;
				LOGGER.info("Tiempo total de comprobacion del hash: " + processTime / 1000.0 + " seg"); //$NON-NLS-1$ //$NON-NLS-2$

				return Boolean.valueOf(Arrays.equals(hashBytes, calculatedHash));

			}
			@Override
			protected void done() {
				super.done();
				if (waitingDialog != null) {
					waitingDialog.dispose();
				}
			}
		};
		worker.execute();

		if (waitingDialog != null && dataFile.length() > SIZE_WAIT) {
			// Se muestra la ventana de espera
			waitingDialog.setVisible(true);
		}

		return worker.get().booleanValue();
	}

	static String getHashAlgorithm(final byte[] hash) {
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
    static boolean arrayEquals(final byte[] v, final byte[] w) {
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

	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			CheckHashFileDialog.this.setVisible(false);
			CheckHashFileDialog.this.dispose();
		}
	}

	/** Inicia el proceso de comprobaci&oacute;n de huella digital.
	 * @param parent Componente padre para la modalidad. */
	public static void launch(final Window parent) {
		final CheckHashFileDialog chkd = new CheckHashFileDialog(parent);
		chkd.setSize(600, 250);
		chkd.setResizable(false);
		chkd.setLocationRelativeTo(parent);
		chkd.setVisible(true);
	}

	private static Image getDialogIcon() {
		Image icon;
		try {
			icon = UIFactory.getDefaultDialogIcon();
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el icono del dialogo: " + e); //$NON-NLS-1$
			icon = null;
		}
		return icon;
	}
}
