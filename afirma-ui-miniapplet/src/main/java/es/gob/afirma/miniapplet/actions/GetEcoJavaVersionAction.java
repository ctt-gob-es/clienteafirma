package es.gob.afirma.miniapplet.actions;

import java.security.PrivilegedAction;

/**
 * Acci&oacute;n privilegiada que permite obtener la versi&oacute;n de la m&aacute;quina
 * virtual de Java en un formato claramente identificable: JX, en donde X es un n&uacute;mero
 * entero que identifica la versi&oacute;n. X ser&aacute; 4 para Java 1.4 o invferior, 5 para Java 5,
 * 6 para Java 6 y 7 para cualquier otro. 
 */
public class GetEcoJavaVersionAction implements PrivilegedAction<String> {

	@Override
	public String run() { 

		final String jreVersion = System.getProperty("java.version");  //$NON-NLS-1$
		String javaVersion = null; 
		if (jreVersion.startsWith("1.0") || jreVersion.startsWith("1.1") || //$NON-NLS-1$ //$NON-NLS-2$
				jreVersion.startsWith("1.2") || jreVersion.startsWith("1.3") || //$NON-NLS-1$ //$NON-NLS-2$
				jreVersion.startsWith("1.4")) javaVersion = "J4";  //$NON-NLS-1$ //$NON-NLS-2$
		else if (jreVersion.startsWith("1.5")) javaVersion = "J5";  //$NON-NLS-1$ //$NON-NLS-2$
		else if (jreVersion.startsWith("1.6")) javaVersion = "J6";  //$NON-NLS-1$ //$NON-NLS-2$
		else javaVersion = "J7";  //$NON-NLS-1$

		return javaVersion; 
	} 
}
