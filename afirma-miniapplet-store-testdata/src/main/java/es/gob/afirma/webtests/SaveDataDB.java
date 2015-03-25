package es.gob.afirma.webtests;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.logging.Logger;

public class SaveDataDB {

	// JDBC driver name and database URL
	static final String JDBC_DRIVER = "com.mysql.jdbc.Driver"; //$NON-NLS-1$
	static final String DB_URL = "jdbc:mysql://servidorcentral:3306/miniapplet_db?relaxAutoCommit=true"; //$NON-NLS-1$
	static final String DB_NAME = "stored_data"; //$NON-NLS-1$

	//  Database credentials
	static final String USER = "prueba"; //$NON-NLS-1$
	static final String PASS = "prueba"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**Estructura de la inserci&oacute;n SQL:
	 * 		INSERT INTO stored_data
	 * 		VALUES ( id, date_test, name_test, result, data_test, user_agent,
	 * 					miniapplet_version, browser, browser_version,
	 * 					os, os_version, java_version, java_arch)
	 * */
	static boolean storedData (final List<StoreBean> storedDataList) {

		Connection conn = null;
		Statement stmt = null;
		try{
			// Controlador JDBC
			LOGGER.info("Registrando controlador JDBC..."); //$NON-NLS-1$
			Class.forName(JDBC_DRIVER);

			// Conexion a la BD
			LOGGER.info("Conectando con la BD..."); //$NON-NLS-1$
			conn = DriverManager.getConnection(DB_URL,USER,PASS);
			LOGGER.info("Conexion establecida."); //$NON-NLS-1$


			LOGGER.info("Creando estructura SQL..."); //$NON-NLS-1$
		    stmt = conn.createStatement();
		    LOGGER.info("Estructura creada."); //$NON-NLS-1$

		    for(int i = 0; i < storedDataList.size(); i++ ) {

		    	StoreBean sb = storedDataList.get(i);

		    	LOGGER.info("Registrando resultado del navegador: " + sb.getBrowser()); //$NON-NLS-1$

		    	String query =
		    		"INSERT INTO " + DB_NAME + //$NON-NLS-1$
		    		" VALUES (" + //$NON-NLS-1$
		    			sb.getId() + "," + //$NON-NLS-1$
		    			"'" + sb.getDate() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		    			"'" + sb.getTest() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				sb.getResult() + "," + //$NON-NLS-1$
		   				"'" + sb.getData().replace("'", "") +"'," + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		   				"'" + sb.getUserAgent() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getMiniappletVersion() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getBrowser() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getBrowserVersion() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getOs() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getOsVersion() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getJavaVersion() + "'," + //$NON-NLS-1$ //$NON-NLS-2$
		   				"'" + sb.getJavaArch() + "'" + //$NON-NLS-1$ //$NON-NLS-2$
	    			")"; //$NON-NLS-1$
		    		LOGGER.fine("Consulta: \n" + query); //$NON-NLS-1$
				    int res = stmt.executeUpdate(query);

				LOGGER.info("Datos insertados en la BD. Response: " + res); //$NON-NLS-1$

		    }

		    stmt.close();

		    conn.commit();

			conn.close();
			LOGGER.info("Conexion con la BD cerrada."); //$NON-NLS-1$

		}catch(Exception e) {
			LOGGER.severe("ERROR: " + e); //$NON-NLS-1$
			try {
				stmt.close();
				conn.close();
			} catch (SQLException e1) {
				LOGGER.severe("Error cerrando la BD: " + e1); //$NON-NLS-1$
			}
			return false;
		}
		return true;
	}
}
