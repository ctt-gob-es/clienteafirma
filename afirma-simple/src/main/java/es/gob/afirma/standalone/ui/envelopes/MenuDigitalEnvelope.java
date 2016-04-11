package es.gob.afirma.standalone.ui.envelopes;

import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.logging.Logger;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Men&uacute; de creaaci&oacute;n y apertura de sobre digital.
 * @author Juliana Marulanda. */
public final class MenuDigitalEnvelope extends JMenu {

	private static final long serialVersionUID = -2837810688321728252L;
	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private final Frame parent;

	/**
	 * Crea el men&uacute; de creaaci&oacute;n de un sobre digital.
	 * @param parent Componente padre.
	 */
	public MenuDigitalEnvelope(final Frame parent) {
		this.parent = parent;
		createUI();
	}

	/**
	 * Crea un submen&uacute; que se desspliega al pulsar en la opci&oacute;n
	 * 'Sobre digital'.
	 */
	private void createUI() {
		final boolean isMac = Platform.OS.MACOSX.equals(Platform.getOS());
		setText(SimpleAfirmaMessages.getString("MenuDigitalEnvelope.0")); //$NON-NLS-1$
		setMnemonic(KeyEvent.VK_D); // Atajos de teclado -> 'Ctrl' + 'D'

		// Crear sobre digital
		final JMenuItem createDigitalEnvelopeMenu = new JMenuItem(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.1") //$NON-NLS-1$
		);
		createDigitalEnvelopeMenu.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_R, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		);

		createDigitalEnvelopeMenu.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.11") //$NON-NLS-1$
		);
		createDigitalEnvelopeMenu.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				createDigitalEnvelope();
			}
		});
		add(createDigitalEnvelopeMenu);

		// Abrir un sobre digital
		final JMenuItem openDigitalEnvelopeMenu = new JMenuItem(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.10") //$NON-NLS-1$
		);
		openDigitalEnvelopeMenu.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_O, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
		);
		openDigitalEnvelopeMenu.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.12") //$NON-NLS-1$
		);
		openDigitalEnvelopeMenu.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				openDigitalEnvelope();
			}
		});
		if (!isMac) {
			createDigitalEnvelopeMenu.setMnemonic(KeyEvent.VK_R);
			openDigitalEnvelopeMenu.setMnemonic(KeyEvent.VK_O);
		}
		add(openDigitalEnvelopeMenu);
	}

	/**
	 * Abre la ventana de creaci&oacute;n de un certificado digital.
	 */
	void createDigitalEnvelope() {
		DigitalEnvelopePresentation.startDigitalEnvelopePresentation(this.parent);
	}

	/**
	 * Abre los sobres digitales que esten ya creados.
	 */
	void openDigitalEnvelope() {
		OpenDigitalEnvelopeDialog.startOpenDigitalEnvelopeDialog(this.parent);
	}
}
