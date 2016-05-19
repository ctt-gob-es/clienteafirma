package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.lang.reflect.Method;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.AutoFirmaUtil;

/** Di&aacute;logo de espera indeterminada.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class CommonWaitDialog extends JDialog {

	private static final int PREFERRED_WIDTH = 300;
	private static final int PREFERRED_HEIGHT = 100;

	private static final long serialVersionUID = -6137113502471587689L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JLabel labelProgress = new JLabel();

	/** Crea un di&aacute;logo de espera indeterminada.
	 * @param parent Marco padre para la modalidad.
	 * @param message Mesaje del di&aacute;logo.
	 * @param title T&iacute;tulo del di&aacute;logo. */
	public CommonWaitDialog(final Frame parent, final String message, final String title) {

		super(parent, ModalityType.APPLICATION_MODAL);

		final JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.anchor = GridBagConstraints.CENTER;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1;
		panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		this.labelProgress.setHorizontalAlignment(SwingConstants.CENTER);
		this.labelProgress.setText(message);
		panel.add(this.labelProgress, c);

		final JProgressBar jpb = new JProgressBar();
		jpb.setIndeterminate(true);
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            final Method putCLientPropertyMethod;
			try {
				putCLientPropertyMethod = JProgressBar.class.getMethod("putClientProperty", Object.class, Object.class); //$NON-NLS-1$
				putCLientPropertyMethod.invoke(jpb, "JProgressBar.style", "circular"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se ha podido establecer el estilo OS X en el dialogo de espera: " + e //$NON-NLS-1$
				);
			}
        }
		this.labelProgress.setLabelFor(jpb);
		c.gridy++;
		c.gridy++;
		panel.add(jpb, c);

		setContentPane(panel);
		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		setPreferredSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		setLocation(cp.x - PREFERRED_WIDTH/2, cp.y - PREFERRED_HEIGHT/2);
		setResizable(false);
		setTitle(title);
        try {
            setIconImage(
        		AutoFirmaUtil.getDefaultDialogsIcon()
            );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"No se ha podido cargar el icono del dialogo de espera de firma: " + e  //$NON-NLS-1$
    		);
        }

        pack();
	}

	/** Establece el mensaje del di&aacute;logo.
	 * @param message Mensaje del di&aacute;logo. */
	public void setMessage(final String message) {
		this.labelProgress.setText(message);
		pack();
		revalidate();
		repaint();
	}

	/** Obtiene el mensaje del di&aacute;logo.
	 * @return Mensaje del di&aacute;logo. */
	public String getMessage() {
		return this.labelProgress.getText();
	}
}
