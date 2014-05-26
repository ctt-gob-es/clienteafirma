package es.gob.afirma.ui.utils;

import javax.swing.JComponent;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

/**
 * Clase para dar el foco a un componente determinado.
 */
public class RequestFocusListener implements AncestorListener
{
	private final boolean removeListener;


	/**
	 * Constructor por defecto.
	 */
	public RequestFocusListener()
	{
		this(true);
	}

	/**
	 * Constructor.
	 * @param removeListener
	 */
	public RequestFocusListener(final boolean removeListener)
	{
		this.removeListener = removeListener;
	}

	/**
	 * Asigna el foco al componente que ha lanzado el evento.
	 */
	@Override
	public void ancestorAdded(final AncestorEvent e)
	{
		final JComponent component = e.getComponent();
		component.requestFocusInWindow();

		if (this.removeListener) {
			component.removeAncestorListener( this );
		}
	}

	@Override
	public void ancestorMoved(final AncestorEvent e) { /** No implementado */ }

	@Override
	public void ancestorRemoved(final AncestorEvent e) { /** No implementado */ }
}
