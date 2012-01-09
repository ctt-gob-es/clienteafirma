package es.gob.afirma.ui.utils;

import javax.swing.*;
import javax.swing.event.*;

/**
 * Clase para dar el foco a un componente determinado.
 */
public class RequestFocusListener implements AncestorListener
{
	private boolean removeListener;


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
	public RequestFocusListener(boolean removeListener)
	{
		this.removeListener = removeListener;
	}

	/**
	 * Asigna el foco al componente que ha lanzado el evento.
	 */
	@Override
	public void ancestorAdded(AncestorEvent e)
	{
		JComponent component = e.getComponent();
		component.requestFocusInWindow();

		if (this.removeListener)
			component.removeAncestorListener( this );
	}

	@Override
	public void ancestorMoved(AncestorEvent e) { /** No implementado */ }

	@Override
	public void ancestorRemoved(AncestorEvent e) { /** No implementado */ }
}
