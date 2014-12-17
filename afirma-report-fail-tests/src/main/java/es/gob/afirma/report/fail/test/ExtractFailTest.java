package es.gob.afirma.report.fail.tests;

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

/** M&eactue;todo para extraer las pruebas que han fallado. **/
public class ExtractFailTest {

	// JDBC driver name and database URL
	static final String JDBC_DRIVER = "com.mysql.jdbc.Driver"; //$NON-NLS-1$
	static final String DB_URL = "jdbc:mysql://172.24.30.87:3306/miniapplet_db"; //$NON-NLS-1$
	static final String DB_NAME = "stored_data"; //$NON-NLS-1$

	//  Database credentials
	static final String USER = "prueba"; //$NON-NLS-1$
	static final String PASS = "prueba"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	// Posiciones de las columnas de la base de datos
	static final int ID_SQL = 1;
	static final int DATE_SQL = 2;
	static final int TEXT_NAME_SQL = 3;
	static final int RESULT_SQL = 4;
	static final int DATA_SQL = 5;
	static final int UA_SQL = 6;

	static final String DATE = "date_test"; //$NON-NLS-1$
	static final String UA = "user_agent"; //$NON-NLS-1$
	static final String ID = "id"; //$NON-NLS-1$
	static final String TEXT_NAME = "text_name"; //$NON-NLS-1$
	static final String RESULT = "result"; //$NON-NLS-1$

	// Si un test ha fallado, en la base de datos se almacena FALSE (0)
	static final String T_FAIL = "0"; //$NON-NLS-1$

	/** Extrae de la base de datos <br>store_data</br> la lista de pruebas que han sido err&oacute;neas.
	 * 	@returnlista de pruebas que han sido err&oacute;neas. */
	public static List<StoreBean> extractData() {

		Connection conn = null;
		Statement stmt = null;
		ResultSet res = null;

		// Lista de los test err&oacute;neos.
		List<StoreBean> list = null;

		try{
			// Registro del controlador JDBC
			LOGGER.info("Registrando el controlador JDBC..."); //$NON-NLS-1$
			Class.forName(JDBC_DRIVER);

			// Abrir conexion con la BD
			LOGGER.info("Abriendo conexion con la base de datos..."); //$NON-NLS-1$
			conn = DriverManager.getConnection(DB_URL,USER,PASS);

		    stmt = conn.createStatement();
		    // Consulta a la base de datos store_test para extraer los test que han fallado en la ultima prueba realizada a cada test
		    String query = "SELECT * FROM " + //$NON-NLS-1$
		    					"(SELECT * FROM "+ DB_NAME + //$NON-NLS-1$
		    					" GROUP BY " + ID + //$NON-NLS-1$
		    					" ORDER BY " + "MAX("+ DATE +") ) AS failtest" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		    				" WHERE " + RESULT + " = " + T_FAIL +" ;"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		    LOGGER.info("QUERY: " + query ); //$NON-NLS-1$
		    res = stmt.executeQuery(query );

		    list = new ArrayList<StoreBean>();
		    while(res.next()) {
		    	StoreBean st = new StoreBean(
		    			Integer.parseInt(res.getString(ID_SQL)),
		    			res.getString(TEXT_NAME_SQL),
		    			res.getString(RESULT_SQL),
		    			res.getString(RESULT_SQL),
		    			res.getString(UA_SQL),
		    			new SimpleDateFormat("dd/MM/yyyy hh:mm").format(new Date(res.getLong(DATE_SQL))) //$NON-NLS-1$
		    		);
		    	list.add(st);
		    }

		    res.close();
		    stmt.close();
			conn.close();

		} catch(Exception e) {
			LOGGER.severe("ERROR: " + e); //$NON-NLS-1$
		    try {
		    	res.close();
				stmt.close();
				conn.close();
			} catch (SQLException e1) {
				LOGGER.severe("ERROR: " + e); //$NON-NLS-1$
			}
			return null;
		}
		return list;
	}

}
