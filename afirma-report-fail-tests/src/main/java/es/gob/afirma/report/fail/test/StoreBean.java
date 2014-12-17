package es.gob.afirma.report.fail.tests;


public class StoreBean {


	 int id;
	 String test;
	 String result;
	 String data;
	 String userAgent;
	 String date;

	public StoreBean( final int identifier,
					 final String testName,
					 final String res,
					 final String info,
					 final String ua,
					 final String dateDB) {
		this.id = identifier;
		this.test = testName;
		this.result = res;
		this.data = info;
		this.userAgent = ua;
		this.date = dateDB;
	}

	public int getId() {
		return this.id;
	}

	public String getTest() {
		return this.test;
	}

	public String getResult() {
		return this.result;
	}

	public String getData() {
		return this.data;
	}

	public String getUserAgent() {
		return this.userAgent;
	}

	public String getDate() {
		return this.date;
	}

	@Override
	public String toString () {
		return 	"ID: " + this.id +  "  " + //$NON-NLS-1$ //$NON-NLS-2$
				"NOMBRE DEL TEST: " + this.test + "  " +  //$NON-NLS-1$ //$NON-NLS-2$
				"RESULT: " + this.result + "  " + //$NON-NLS-1$ //$NON-NLS-2$
				"DATOS RECIBIDOS: " + this.data +  "  " + //$NON-NLS-1$ //$NON-NLS-2$
				"USER AGENT: " + this.userAgent +  "  " + //$NON-NLS-1$ //$NON-NLS-2$
				"FECHA DEL TEST: " + this.date +  "\n"; //$NON-NLS-1$ //$NON-NLS-2$
	}

}
