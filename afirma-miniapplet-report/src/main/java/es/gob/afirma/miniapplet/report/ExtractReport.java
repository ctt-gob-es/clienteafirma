package es.gob.afirma.miniapplet.report;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;


public class ExtractReport {

	// JDBC driver name and database URL
	static final String JDBC_DRIVER = "com.mysql.jdbc.Driver"; //$NON-NLS-1$
	static final String DB_URL = "jdbc:mysql://172.24.30.87:3306/miniapplet_db"; //$NON-NLS-1$
	static final String DB_NAME = "stored_data"; //$NON-NLS-1$

	//  Database credentials
	static final String USER = "prueba"; //$NON-NLS-1$
	static final String PASS = "prueba"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String ID = "id"; //$NON-NLS-1$
	static final String DATE = "date"; //$NON-NLS-1$
	static final String TEXT_NAME = "text_name"; //$NON-NLS-1$
	static final String RESULT = "result"; //$NON-NLS-1$
	static final String DATA = "data"; //$NON-NLS-1$
	static final String UA = "user_agent"; //$NON-NLS-1$
	static final String MINIAPPLET_VERSION = "miniapplet_version"; //$NON-NLS-1$
	static final String BROWSER = "browser"; //$NON-NLS-1$
	static final String BROWSER_VERSION = "browser_version"; //$NON-NLS-1$
	static final String OS = "os"; //$NON-NLS-1$
	static final String OS_VERSION = "os_version"; //$NON-NLS-1$
	static final String JAVA_VERSION = "java_version"; //$NON-NLS-1$
	static final String JAVA_ARCH = "java_arch"; //$NON-NLS-1$


	// Posiciones de las columnas de la base de datos
	static final int ID_SQL = 1;
	static final int DATE_SQL = 2;
	static final int TEXT_NAME_SQL = 3;
	static final int RESULT_SQL = 4;
	static final int DATA_SQL = 5;
	static final int UA_SQL = 6;

	public ExtractReport() {
		// Vacio
	}

	public static byte[] getReport() {
		byte[] data = null;
		Connection conn = null;
		Statement stmt = null;
		ResultSet res = null;
		try{
			// Registrando controlador JDBC
			LOGGER.info("Registrando controlador JDBC..."); //$NON-NLS-1$
			Class.forName(JDBC_DRIVER);

			// Abrir conexion con la BD
			LOGGER.info("Abriendo conexion con la BD..."); //$NON-NLS-1$
			conn = DriverManager.getConnection(DB_URL,USER,PASS);
			LOGGER.info("Conexion establecida."); //$NON-NLS-1$

			LOGGER.info("Creando estructura SQL..."); //$NON-NLS-1$
		    stmt = conn.createStatement();

		    res = stmt.executeQuery("SELECT * FROM " + DB_NAME + ";"); //$NON-NLS-1$ //$NON-NLS-2$
		    List<String> list = new ArrayList<String>();

		    // Primera fila del csv: los nombres de las columnas sql
		    while(res.next()) {
		    	String row = res.getString(ID_SQL) + ";" + //$NON-NLS-1$
			    		new SimpleDateFormat("dd/MM/yyyy hh:mm").format(new Date(res.getLong(DATE_SQL))) + ";" + //$NON-NLS-1$ //$NON-NLS-2$
			    		"\"" + res.getString(TEXT_NAME_SQL) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
						Boolean.valueOf(res.getString(RESULT_SQL).equals("1")).toString() + ";" +  //$NON-NLS-1$ //$NON-NLS-2$
						"\"" + res.getString(UA_SQL) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(MINIAPPLET_VERSION) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(BROWSER) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(BROWSER_VERSION) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(OS) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(OS_VERSION) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(JAVA_VERSION) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + res.getString(JAVA_ARCH) + "\";" + //$NON-NLS-1$ //$NON-NLS-2$
		    			"\"" + Base64.encode(res.getString(DATA_SQL).getBytes()) + "\"\n" ; //$NON-NLS-1$ //$NON-NLS-2$
		    	list.add(
		    		row
		    	);
		    }
		    res.close();
		    stmt.close();
		   	conn.close();
			LOGGER.info("Conexion cerrada."); //$NON-NLS-1$

			// convertimos la lista en una cadena de bytes
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			DataOutputStream oos = new DataOutputStream(bos);
			for(int i = 0; i < list.size(); i++) {
				oos.writeBytes(list.get(i));
			}

			data = bos.toByteArray();

		}catch(Exception e) {
			 try {
				res.close();
				stmt.close();
				conn.close();
				LOGGER.info("Conexion cerrada."); //$NON-NLS-1$
			} catch (SQLException e1) {
				LOGGER.severe("Error cerrando BD: " + e); //$NON-NLS-1$
			}
			 LOGGER.severe("ERROR: " + e); //$NON-NLS-1$
			 return null;
		}
		return data;
	}
}
