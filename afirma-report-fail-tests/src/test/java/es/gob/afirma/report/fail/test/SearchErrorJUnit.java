package es.gob.afirma.report.fail.tests;

import java.util.List;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

public final class SearchErrorJUnit {

	static final String ID = "id"; //$NON-NLS-1$
	static final String DATE = "date"; //$NON-NLS-1$
	static final String TEXT_NAME = "text_name"; //$NON-NLS-1$
	static final String RESULT = "result"; //$NON-NLS-1$
	static final String DATA = "data"; //$NON-NLS-1$
	static final String UA = "user_agent"; //$NON-NLS-1$

	// Posiciones de las columnas de la base de datos
	static final int ID_SQL = 1;
	static final int DATE_SQL = 2;
	static final int TEXT_NAME_SQL = 3;
	static final int RESULT_SQL = 4;
	static final int DATA_SQL = 5;
	static final int UA_SQL = 6;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@SuppressWarnings("static-method")
	@Test
	public void testExtractFailTests() {
		// Extraemos la lista de pruebas que han dado error.
		List<StoreBean> list = ExtractFailTest.extractData();
		if(list == null || list.isEmpty()) {
			LOGGER.info("La lista esta vacia. Ningun test ha fallado"); //$NON-NLS-1$
			return;
		}
		Assert.fail("Fallo en el las siguientes pruebas: \n" + list.toString()); //$NON-NLS-1$
	}



}
