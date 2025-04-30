package es.gob.afirma.keystores.jmulticard.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/** CallbackHandler que gestiona los Callbacks de petici&oacute;n de informaci&oacute;n al usuario.
 * @author Sergio Mart&iacute;nez Rico. */
public final class DnieCallbackHandler implements CallbackHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public void handle(final Callback[] callbacks) throws UnsupportedCallbackException {
		if (callbacks != null) {
			for (final Callback cb : callbacks) {
				if (cb != null) {
					if (
						"es.gob.jmulticard.callback.CustomTextInputCallback".equals(cb.getClass().getName()) || //$NON-NLS-1$
						"javax.security.auth.callback.TextInputCallback".equals(cb.getClass().getName()) //$NON-NLS-1$
					) {
						LOGGER.info("Solicitamos codigo CAN del DNIe"); //$NON-NLS-1$
						final UIPasswordCallbackCan uip = new UIPasswordCallbackCan(
							Messages.getString("CanPasswordCallback.0"), //$NON-NLS-1$
							null,
							Messages.getString("CanPasswordCallback.0"), //$NON-NLS-1$
							Messages.getString("CanPasswordCallback.2") //$NON-NLS-1$
						);
						try {
							final Method m = cb.getClass().getMethod("setText", String.class); //$NON-NLS-1$
							m.invoke(cb, new String(uip.getPassword()));
						}
						catch (final NoSuchMethodException    |
							         SecurityException        |
							         IllegalAccessException   |
							         IllegalArgumentException |
							         InvocationTargetException e) {
							throw new UnsupportedCallbackException(
								cb,
								"No se ha podido invocar al metodo 'setText' de la callback: " + e //$NON-NLS-1$
							);
						}
					}
					else if (cb instanceof CustomAuthorizeCallback) {
						LOGGER.info("Solicitamos autorizacion para el uso del DNIe"); //$NON-NLS-1$
						DialogBuilder.showSignatureConfirmDialog((CustomAuthorizeCallback)cb);
					}
					else if (cb instanceof PasswordCallback) {
						LOGGER.info("Solicitamos codigo PIN del DNIe"); //$NON-NLS-1$
						final CommonPasswordCallback uip = new CommonPasswordCallback(
							((PasswordCallback)cb).getPrompt(),
							Messages.getString("CommonPasswordCallback.1"), //$NON-NLS-1$
							true
						);
						((PasswordCallback)cb).setPassword(uip.getPassword());
					}
					else {
						throw new UnsupportedCallbackException(cb);
					}
				}
			}
		}
		else {
			LOGGER.warning("Se ha recibido un array de Callbacks nulo"); //$NON-NLS-1$
		}
	}
}
