package es.gob.afirma.ui.wizardUtils;

import javax.swing.JLabel;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.Utils;

/**
 * Etiquetas de presentación de texto.
 * @author lmerayo
 *
 */
public class InfoLabel extends JLabel{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor de la clase
	 * 
	 * @param text
	 *            Texto a mostrar
	 * @param opaque
	 *            Indica si el componente será opaco
	 */
	public InfoLabel(String text, boolean opaque) {
		super("<HTML>" + text + "</HTML>");
		this.setFocusable(true);
		this.setOpaque(opaque);
		 if(GeneralConfig.isRemarked()){
	        	Utils.remarcar(this);
	        }
		 Utils.setContrastColor(this);
		
	}
}
