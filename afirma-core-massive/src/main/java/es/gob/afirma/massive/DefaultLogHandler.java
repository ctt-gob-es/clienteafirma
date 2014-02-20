package es.gob.afirma.massive;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DefaultLogHandler implements LogHandler {
	
	private final OutputStream os;
	
	public DefaultLogHandler(final OutputStream os) {
		this.os = os;
	}
	
	@Override
	public void addLog(int level, String msg, String inputData, String outputSign) throws IOException {
		try { 		
			this.os.write(("\r\n" + getLevel(level) + ": " + msg + " - " + (outputSign != null ? outputSign : (inputData != null ? inputData : ""))).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se pudo insertar una entrada en el log de la operacion masiva: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
	
	@Override
	public void close(Properties params) throws IOException {
		
		final String warningsCount = params != null ? params.getProperty("warningsCount", "0") : "0"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		final String errorsCount = params != null ? params.getProperty("errorsCount", "0") : "0"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		this.os.write(("\r\n\r\n" + MassiveSignMessages.getString("DirectorySignatureHelper.25") + ": " + warningsCount).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		this.os.write(("\r\n" + MassiveSignMessages.getString("DirectorySignatureHelper.26") + ": " + errorsCount).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$		
	}
	
	private static String getLevel(final int level) {
		if (level == Level.INFO.intValue()) {
			return Level.INFO.getName();
		}
		else if (level == Level.WARNING.intValue()) {
			return Level.WARNING.getName();
		}
		else {
			return Level.SEVERE.getName();
		}
	}
}
