package es.gob.afirma.ui.core.jse.errors;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import es.gob.afirma.ui.core.jse.JSEUIMessages;

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

		// Boton para abrir o cerrar detalles del error, en caso de que existan detalles
		if(this.view.getDetailsButton() != null) {
		this.view.getDetailsButton().addActionListener( new ActionListener(){
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				resizeScrollpane();
			}
		});
		}

		//Boton de cierre del dialogo
		this.view.getCloseButton().addActionListener( new ActionListener(){
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

		boolean visible = false;
		int screenHeight = 465;

		if(this.view.isExpandedDetails()) {
			this.view.setExpandedDetails(false);
			screenHeight = 231;
			this.view.getDetailsButton().setText(JSEUIMessages.getString("JSEUIManager.90")); //$NON-NLS-1$
		}else {
			this.view.setExpandedDetails(true);
			visible = true;
			this.view.getDetailsButton().setText(JSEUIMessages.getString("JSEUIManager.91")); //$NON-NLS-1$
		}

		this.view.getWindow().setSize(new Dimension(600, (int) Math.min(550, screenHeight * 0.8)));
		this.view.getScrollPane().setVisible(visible);
	}

	/**
	 * Devuelve el componente padre donde se encuentran todos los componentes hijos
	 * @return Devuelve el componente padre
	 */
	public ErrorManagementPanel getView() {
		return this.view;
	}
}
