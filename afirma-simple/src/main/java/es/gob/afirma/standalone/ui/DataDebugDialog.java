package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Di&aacute;logo para la muestra de datos a firmar.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class DataDebugDialog extends JDialog {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 5993877132056846401L;

	private static final int DIALOG_WIDTH = 800;
	private static final int DIALOG_HEIGHT = 600;

	private byte[] data;

	/** Construye un di&aacute;logo para la muestra de datos a firmar.
	 * @param dataToSign Datos a firmar. */
	public DataDebugDialog(final byte[] dataToSign) {
		if (dataToSign == null || dataToSign.length < 1) {
			throw new IllegalArgumentException(
				"Los datos a firmar no pueden ser nulos ni vacios" //$NON-NLS-1$
			);
		}
		this.data = dataToSign.clone();

		final JTextArea display = new JTextArea();
		final JScrollPane scroll = new JScrollPane(display);

		fillDisplay(display, dataToSign);

		display.setEditable(false);
		display.setFont(new Font("monospaced", Font.PLAIN, 12)); //$NON-NLS-1$

		final JButton cont = new JButton(SimpleAfirmaMessages.getString("DataDebugDialog.8")); //$NON-NLS-1$
		cont.addActionListener(e -> DataDebugDialog.this.setVisible(false));
		cont.setMnemonic('C');
		cont.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DataDebugDialog.9") //$NON-NLS-1$
		);

		display.addKeyListener(
			new KeyAdapter() {
		        @Override
				public void keyPressed(final KeyEvent e) {
		            if (e.getKeyCode() == KeyEvent.VK_R && e.isControlDown()) {
		                final File newDataFile = AOUIFactory.getLoadFiles(
	                		SimpleAfirmaMessages.getString("DataDebugDialog.7"), //$NON-NLS-1$
	                		null,
	                		null,
	                		null,
	                		SimpleAfirmaMessages.getString("DataDebugDialog.6"), //$NON-NLS-1$
	                		false,
	                		false,
	                		null,
	                		this
                		)[0];
		                try (
	                		final InputStream is = new FileInputStream(newDataFile)
                		) {
		                	setData(AOUtil.getDataFromInputStream(is));
		                }
		                catch (final IOException e1) {
		                	LOGGER.warning("Ha fallado la carga de los datos a firmar: " + e1); //$NON-NLS-1$
							display.setText(null);
							display.setText(SimpleAfirmaMessages.getString("DataDebugDialog.5")); //$NON-NLS-1$
							return;
						}
		                fillDisplay(display, getData());
		            }
		        }
			}
		);

		final JPanel panel = new JPanel();

		//TODO: Hacer que esto se vea mas bonito
		panel.setLayout(new BorderLayout());
		panel.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
		panel.add(scroll, BorderLayout.CENTER);
		panel.add(cont, BorderLayout.PAGE_END);

		setModalityType(ModalityType.APPLICATION_MODAL);
		setTitle(SimpleAfirmaMessages.getString("DataDebugDialog.4")); //$NON-NLS-1$
    	setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
    	setResizable(true);
    	final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		setLocation(cp.x - DIALOG_WIDTH/2, cp.y - DIALOG_HEIGHT/2);
		add(panel);
		setVisible(true);
	}

	static void fillDisplay(final JTextArea display, final byte[] dataToSign) {

		display.setText(null);

		final MimeHelper mh = new MimeHelper(dataToSign);

		String desc;
		try {
			desc = mh.getDescription();
		}
		catch (final IOException e1) {
			LOGGER.warning(
				"No se ha podido determinar el tipo de los datos a firmar, se usara binario: " + e1 //$NON-NLS-1$
			);
			desc = SimpleAfirmaMessages.getString("DataDebugDialog.0"); //$NON-NLS-1$
		}
		display.append(SimpleAfirmaMessages.getString("DataDebugDialog.1",  desc) + '\n'); //$NON-NLS-1$
		display.append(SimpleAfirmaMessages.getString("DataDebugDialog.2", Integer.toString(dataToSign.length)) + '\n'); //$NON-NLS-1$
		String mime;
		try {
			mime = mh.getMimeType();
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No se ha podido determinar el tipo de los datos a firmar, se mostraran en Base64: " + e //$NON-NLS-1$
			);
			mime = "application/octet-stream"; //$NON-NLS-1$
		}
		display.append(SimpleAfirmaMessages.getString("DataDebugDialog.3") + '\n'); //$NON-NLS-1$
		if (mime.contains("text") || mime.contains("xml")) { //$NON-NLS-1$ //$NON-NLS-2$
			display.append(new String(dataToSign));
		}
		else {
			display.append(
				new String(
					java.util.Base64.getMimeEncoder().encode(dataToSign) // Con retornos de carro para que se vea bien
				)
			);
		}
	}

	/** Obtiene los datos a firmar.
	 * @return Datos a firmar. */
	public byte[] getData() {
		return this.data;
	}

	void setData(final byte[] d) {
		this.data = d;
	}

}
