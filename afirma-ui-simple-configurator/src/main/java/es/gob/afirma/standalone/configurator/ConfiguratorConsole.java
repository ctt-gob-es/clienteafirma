package es.gob.afirma.standalone.configurator;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/**
 * Pantalla principal de la aplicaci&oacute;n. Muestra una consola para la notificaci&oacute;n de las accesiones
 * llevadas a cabo por el navegador.
 */
public class ConfiguratorConsole extends JFrame {

	/** Serial Id. */
	private static final long serialVersionUID = 398187262022150395L;

	private final JTextArea console;

	/**
	 * Crea la pantalla.
	 */
	public ConfiguratorConsole() {
    	this.console = new JTextArea();
	}

	/** Muestra la pantalla principal de la aplicaci&oacute;n.
     * @param wlist WindowListener para el control del cierre de la ventana
     * @param firstPanel Primera pantalla de la aplicaci&oacute;n.
     * @param width Ancho de la ventana
     * @param height Alto de la ventana */
    public void showConsole(final WindowListener wlist, final int width, final int height) {
    	SwingUtilities.invokeLater(new Runnable() {
    		@Override
    		public void run() {
    			createUI(wlist, width, height);
    		}
    	});
    }

    void createUI(final WindowListener wlist, final int width, final int height) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setTitle(Messages.getString("ConfiguratorConsole.0")); //$NON-NLS-1$
        this.setSize(width, height);
        this.setLayout(new GridBagLayout());
        this.setLocationRelativeTo(null);
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        if (wlist != null) {
            this.addWindowListener(wlist);
        }

        this.console.setMargin(new Insets(5,  5,  5,  5));
        this.console.setEditable(false);

        final JScrollPane scrollPane = new JScrollPane(this.console);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.insets = new Insets(11,  11,  11,  11);

        this.add(scrollPane, c);

        try {
            setIconImage(
        		Toolkit.getDefaultToolkit().getImage(this.getClass().getResource("/afirma_ico.png")) //$NON-NLS-1$
            );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de la aplicacion: " + e);  //$NON-NLS-1$//$NON-NLS-2$
        }

        this.setVisible(true);
    }

    /**
     * Muestra un texto por consola.
     * @param text Texto a mostrar.
     */
    void print(final String text) {
    	this.console.append(text);
    	this.console.append("\n"); //$NON-NLS-1$
    }
}
