package es.gob.afirma.ui.core.jse.errors;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Manejador de eventos de ErrorManagementPanel.
 */
public class ErrorManagementHandler {

	private final ErrorManagementPanel view;

	/**
	 * Construye el objeto para la gesti&oacute;n de los eventos del di&aacute;logo de
	 * errores.
	 * @param view Panel sobre en el que encuentra el error a mostrar.
	 */
	public ErrorManagementHandler(final ErrorManagementPanel view) {
		this.view = view;
	}

	/**
	 * Establece el comportamiento sobre los componentes del panel.
	 */
	void registerComponents() {

		//Boton de cierre del dialogo
		this.view.getCloseButton().addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				getView().getWindow().dispose();
			}
		});
	}

	/**
	 * Devuelve el componente padre donde se encuentran todos los componentes hijos.
	 * @return Devuelve el componente padre.
	 */
	public ErrorManagementPanel getView() {
		return this.view;
	}
}
