package es.gob.afirma.standalone;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Logger;

/** Obtiene el modelo del dispositivo y verifica si tiene HDPI.
 * @author Sergio Mart&iacute;nez Rico */
public class HDPIManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
    static String executeWmicCommand() {
    	Runtime rt = Runtime.getRuntime();
    	String[] commands = {"wmic", "csproduct", "get", "name"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    	Process proc = null;
    	String modelName = null;
		try {
			proc = rt.exec(commands);
			BufferedReader stdInput = new BufferedReader(new 
					InputStreamReader(proc.getInputStream()));
			String s = null;
			// Obtenemos la segunda linea que ser el nombre de modelo 
			// (la primera es el nombre de la columna)
			while ((s = stdInput.readLine()) != null) {
			    if(!s.equals("Name") && s.length() > 0) { //$NON-NLS-1$
			    	modelName = s.trim();
			    }
			}
		} catch (IOException e) {
			LOGGER.warning(
                       "Error en la ejecucion del comando "+ commands.toString() + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
                );
		}
		return modelName;
    }


	/**
	 * @return Devuelve true si se est&aacute; ejecutando sobre un dispositivo HDPI.
	 */
	public static boolean isHDPIDevice() {
		String modelName = executeWmicCommand();
		// En caso de ser una surface con HDPI se utiliza el Look&Feel Metal. 
    	// En caso contrario se utiliza Nimbus.
    	boolean hdpiDevice = false;
		BufferedReader br = new BufferedReader(new InputStreamReader(HDPIManager.class.getResourceAsStream("/lookandfeel/hdpi_devices"))); //$NON-NLS-1$

	    try {
	    	String line = br.readLine();
	           while (line != null) {
	       		if(line.equals(modelName)) {
	           		hdpiDevice = true;
	           	}
	       		line = br.readLine();
	       	}
	    }
	    catch(IOException e) { 
	    	LOGGER.warning(
	                      "No se ha podido abrir el fichero de dispositivos HDPI: " + e //$NON-NLS-1$
	               );
	         }
    	return hdpiDevice;
	}
    
}
