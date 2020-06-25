package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Icon;
import javax.swing.JButton;

/** Bot&oacute;n con desplegable. */
public class JDropDownButton extends JButton {

	/** Serial Id. */
	private static final long serialVersionUID = -3983995903253627847L;

	/**
	 * Construye el bot&oacute;n con un icono.
	 * @param icon Icono que mostrar en el bot&oacute;n.
	 */
	public JDropDownButton(final Icon icon) {
		super(icon);

		addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				if (getComponentPopupMenu() != null) {
					getComponentPopupMenu().show(JDropDownButton.this, 0, getHeight());
				}
			}
		});
	}

	@Override
	public Point getPopupLocation(final MouseEvent event) {
		return new Point(0, getHeight());
	}
}
