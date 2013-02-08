package es.gob.afirma.signfolder;

import javax.security.auth.callback.PasswordCallback;

/** @author Alberto Mart&iacute;nez */
public abstract class CustomizablePasswordCallback extends PasswordCallback {

    /** Serial version UID */
    private static final long serialVersionUID = -6307987785481122679L;

    /** Construye un PasswordCallback con un prompt y un booleano especificando si la contraseña debe ser mostrada mientras se escribe
     * @param prompt El texto a mostrar para pedir la contraseña
     * @param echoOn Verdadero si la contraseña debería ser mostrada mientras se escribe */
    public CustomizablePasswordCallback(final String prompt, final boolean echoOn) {
        super(prompt, echoOn);
    }

    /** @param prompt
     * @return */
    public abstract char[] getPassword(final String prompt);
}