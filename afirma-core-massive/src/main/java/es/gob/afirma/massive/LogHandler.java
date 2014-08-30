package es.gob.afirma.massive;

import java.io.IOException;
import java.util.Properties;

/** Gestor de registro para la firma masiva. */
public abstract class LogHandler {

	/** Nivel de registro informativo. */
	public static final int LEVEL_INFO = 800;

	/** Nivel de registro de advertencias. */
	public static final int LEVEL_WARNING = 900;

	/** Nivel de registro de errores. */
	public static final int LEVEL_SEVERE = 1000;

	/** Da por finalizado el registro, a&ntilde;adiendo como pie de este las propiedades indicadas.
	 * No cierra el flujo de escritura del registro, este cierre debe hacerse externamente.
	 * @param params Propiedades a a&ntilde;adir en el pie del regitro al cierre de este
	 * @throws IOException En caso de errores de entrada / salida */
	public abstract void close(Properties params) throws IOException;

	/** A&ntilde;ade una l&iacute;nea de registro-
	 * @param level Nivel de registro
	 * @param msg mensaje Mensaje a registrar
	 * @param inputData Datos enviados para firmar
	 * @param outputSign Firma resultante
	 * @throws IOException En caso de errores de entrada / salida */
	public abstract void addLog(int level, String msg, String inputData, String outputSign) throws IOException;

}
