package es.gob.afirma.massive;

import java.io.IOException;
import java.util.Properties;

public interface LogHandler {

	public final static int LEVEL_INFO = 800;
	public final static int LEVEL_WARNING = 900;
	public final static int LEVEL_SEVERE = 1000;

	void close(Properties params) throws IOException;

	void addLog(int level, String msg, String inputData, String outputSign) throws IOException;
	
}
