package es.gob.afirma.webtests;



public class StoreBean {


	 int id;
	 String test;
	 boolean result;
	 String data;
	 String userAgent;
	 long date;
	 String miniappletVersion;
	 String browser;
	 String browserVersion;
	 String os;
	 String osVersion;
	 String javaVersion;
	 String javaArch;

	public StoreBean( final int identifier,
					 final String testName,
					 final boolean res,
					 final String info,
					 final String ua,
					 final long testDate,
					 final String miniappletVersionTst,
					 final String browserTst,
					 final String browserVersionTst,
					 final String osTst,
					 final String osVersionTst,
					 final String javaVersionTst,
					 final String javaArchTst) {
		this.id = identifier;
		this.test = testName;
		this.result = res;
		this.data = info;
		this.userAgent = ua;
		this.date = testDate;
		this.miniappletVersion = miniappletVersionTst;
		this.browser = browserTst;
		this.browserVersion = browserVersionTst;
		this.os = osTst;
		this.osVersion = osVersionTst;
		this.javaVersion = javaVersionTst;
		this.javaArch = javaArchTst;
	}

	public int getId() {
		return this.id;
	}

	public String getTest() {
		return this.test;
	}

	public boolean getResult() {
		return this.result;
	}

	public String getData() {
		return this.data;
	}

	public String getUserAgent() {
		return this.userAgent;
	}

	public long getDate() {
		return this.date;
	}

	 public String getMiniappletVersion() {
		 return this.miniappletVersion;
	 }

	 public String getBrowser() {
		 return this.browser;
	 }

	 public String getBrowserVersion() {
		 return this.browserVersion;
	 }

	 public String getOs() {
		 return this.os;
	 }

	 public String getOsVersion() {
		 return this.osVersion;
	 }

	 public String getJavaVersion() {
		 return this.javaVersion;
	 }

	 public String getJavaArch() {
		 return this.javaArch;
	 }
	public String toHTML() {
		return 	"<table style=\"width:100%\">" + //$NON-NLS-1$
				 	"<tr><td><h3> TEST " + this.id +  "</h3></td></tr>" + //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>NOMBRE DEL TEST: " + this.test + "</td></tr>" +  //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>RESULT: " + this.result +  "</td></tr>" + //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>DATOS RECIBIDOS: " + this.data +  "</td></tr>" + //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>USER AGENT: " + this.userAgent +  "</td></tr>" + //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>FECHA DEL TEST: " + this.date +  "</td></tr>" + //$NON-NLS-1$ //$NON-NLS-2$
					"<tr><td>DATOS GENERALES: miniappletVersion:" + this.miniappletVersion + //$NON-NLS-1$
											" browser: " + this.browser + //$NON-NLS-1$
											" browserVersion: " + this.browserVersion + //$NON-NLS-1$
											" os: " + this.os + //$NON-NLS-1$
											" osVersion: " + this.osVersion + //$NON-NLS-1$
											" javaVersion: " + this.javaVersion + //$NON-NLS-1$
											" javaArch: " +this.javaArch + "</tr><t/d>" + //$NON-NLS-1$ //$NON-NLS-2$
				"</table>"; //$NON-NLS-1$
	}
}
