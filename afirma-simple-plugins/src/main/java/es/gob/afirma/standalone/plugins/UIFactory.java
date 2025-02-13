package es.gob.afirma.standalone.plugins;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Image;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import javax.swing.JDialog;

/**
 * Clase para la construcci&oacute;n de di&aacute;logos y recursos gr&aacute;ficos para su uso en Autofirma.
 */
public class UIFactory {

	/**
	 * Crea un di&aacute;logo de espera.
	 * @param parent Componente padre.
	 * @param message Mensaje del di&aacute;logo.
	 * @param title T&iacute;tulo del di&aacute;logo.
	 * @return Di&aacute;logo gr&aacute;fico.
	 * @throws IOException Cuando no se puede crear el di&aacute;logo.
	 */
	public static JDialog getWaitingDialog(final Component parent, final String message, final String title)
			throws IOException {

		try {
			final Class<?> commonWaitDialogClass = Class.forName("es.gob.afirma.standalone.ui.CommonWaitDialog"); //$NON-NLS-1$

			final Constructor<?> commonWaitDialogConstructor =
					commonWaitDialogClass.getConstructor(Frame.class, String.class, String.class);

			return (JDialog) commonWaitDialogConstructor.newInstance(parent, message, title);
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido cargar el dialogo de espera", e); //$NON-NLS-1$
		}
	}

	public static Image getDefaultDialogIcon() throws IOException {
		try {
			final Class<?> autoFirmaUtilClass = Class.forName("es.gob.afirma.standalone.DesktopUtil"); //$NON-NLS-1$

			final Method getDefaultDialogIconMethod =
					autoFirmaUtilClass.getMethod("getDefaultDialogsIcon"); //$NON-NLS-1$

			return (Image) getDefaultDialogIconMethod.invoke(null);
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido cargar el icono para los dialogos", e); //$NON-NLS-1$
		}
	}
}
