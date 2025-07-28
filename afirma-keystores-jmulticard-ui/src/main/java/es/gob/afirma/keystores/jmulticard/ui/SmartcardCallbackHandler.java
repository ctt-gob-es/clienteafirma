package es.gob.afirma.keystores.jmulticard.ui;

import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/** CallbackHandler que gestiona los <i>Callbacks</i> de petici&oacute;n de informaci&oacute;n
 * al usuario en tarjetas inteligentes.
 * @author Sergio Mart&iacute;nez Rico. */
public final class SmartcardCallbackHandler implements CallbackHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	@Override
	public void handle(final Callback[] callbacks) throws UnsupportedCallbackException {
		if (callbacks != null) {
			for (final Callback cb : callbacks) {
				if (cb != null) {
					if (cb instanceof PasswordCallback) {
						final CommonPasswordCallback uip = new CommonPasswordCallback(
							((PasswordCallback)cb).getPrompt(),
							Messages.getString("CommonPasswordCallback.2"), //$NON-NLS-1$
							false
						);
						((PasswordCallback)cb).setPassword(uip.getPassword());
						return;
					}
					LOGGER.severe("Callback no soportada: " + cb.getClass().getName()); //$NON-NLS-1$
				}
			}
		}
		else {
			LOGGER.warning("Se ha recibido un array de Callbacks nulo"); //$NON-NLS-1$
		}
		throw new UnsupportedCallbackException(null);
	}
}
