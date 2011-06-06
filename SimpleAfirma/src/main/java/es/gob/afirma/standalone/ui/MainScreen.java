package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.ui.AOUIManager;

/**
 * Pantalla principal de la aplicaci&oacute;n de Firma F&aacute;cil con AFirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class MainScreen extends JFrame {

	private static final long serialVersionUID = -3288572031446592104L;
		
	/**
	 * Construye la pantalla principal de la aplicaci&oacute;n.
	 * @param wlist WindowListener para el control del cierre de la ventana
	 * @param firstPanel Primer panel que debe mostrar la aplicaci&oacute;n
	 */
	public MainScreen(final WindowListener wlist,
			          final JPanel firstPanel) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				createUI(wlist, firstPanel);
			}
		});
	}
	
	private void createUI(final WindowListener wlist, final JPanel firstPanel) {

		this.setBackground(SimpleAfirma.WINDOW_COLOR);
		this.setSize(new Dimension(700,500));
		this.setLayout(new BorderLayout());
		this.setLocationRelativeTo(null);
		this.setTitle("Firma electr\u00F3nica f\u00E1cil con @firma");
		this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		if (wlist != null) this.addWindowListener(wlist);
		
		this.add(firstPanel, BorderLayout.CENTER);
		
		try {
			setIconImage(
				Toolkit.getDefaultToolkit().getImage(AOUIManager.class.getResource("/resources/afirma_ico.png")) //$NON-NLS-1$
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de la aplicacion: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		
		this.setVisible(true);
	}
	
}
