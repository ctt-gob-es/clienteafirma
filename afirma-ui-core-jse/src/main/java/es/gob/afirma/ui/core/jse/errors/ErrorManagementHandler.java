package es.gob.afirma.ui.core.jse.errors;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import es.gob.afirma.ui.core.jse.JSEUIMessages;

/**
 * Manejador de eventos de ErrorManagementPanel.
 */
public class ErrorManagementHandler {

	private final ErrorManagementPanel view;
	private final ErrorManagementDialog dialog;

	/**
	 * Construye el objeto para la gesti&oacute;n de los eventos del di&aacute;logo de
	 * errores.
	 * @param view Panel sobre en el que encuentra el error a mostrar.
	 */
	public ErrorManagementHandler(final ErrorManagementPanel view, final ErrorManagementDialog dialog) {
		this.view = view;
		this.dialog = dialog;
	}

	/**
	 * Establece el comportamiento sobre los componentes del panel.
	 */
	void registerComponents() {

		// Boton para abrir o cerrar detalles del error, en caso de que existan detalles
		if (this.view.getDetailsButton() != null) {
			this.view.getDetailsButton().addActionListener( new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent arg0) {
					resizeScrollpane();
				}
			});
		}

		//Boton de cierre del dialogo
		this.view.getCloseButton().addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				getView().getWindow().dispose();
			}
		});
	}

	/**
	 * Redimensiona el panel si se pulsa en el bot&oacute;n de detalles
	 */
	void resizeScrollpane() {

		if (this.view.isExpandedDetails()) {
			this.view.setExpandedDetails(false);
			this.view.getScrollPane().setVisible(false);
			this.view.getDetailsButton().setText(JSEUIMessages.getString("JSEUIManager.90")); //$NON-NLS-1$
		} else {
			this.view.setExpandedDetails(true);
			this.view.getScrollPane().setVisible(true);
			this.view.getDetailsButton().setText(JSEUIMessages.getString("JSEUIManager.91")); //$NON-NLS-1$
		}

		this.dialog.resize();
	}

	/**
	 * Devuelve el componente padre donde se encuentran todos los componentes hijos
	 * @return Devuelve el componente padre
	 */
	public ErrorManagementPanel getView() {
		return this.view;
	}
}
