package es.gob.afirma.signfolder.server.proxy;

import java.util.Vector;

/**
 * Datos identificados de un fichero para su descarga o previsualizaci&oacute;n.
 */
public class RequestDetail {

	private final String id;

	private int priority = 1;

	private boolean workflow = false;

	private boolean forward = false;

	private String subject = null;

	private String[] senders = null;

	private String date = null;

	private String app = null;

	private String ref = null;

	private Vector[] signLines;

	private SignRequestDocument[] docs;

	/**
	 * Construye el detalle de una petici&oacute;n de firma.
	 * @param id Identificador de la petici&oacute;n.
	 */
	public RequestDetail(final String id) {
		this.id = id;
	}

	/**
	 * Recupera el identificador de la petici&oacute;n de firma.
	 * @return Identificador de la petici&oacute;n.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Recupera la prioridad de la petici&oacute;n. Cuando m&aacute;s alto el valor, m&aacute;s prioritario.
	 * @return Prioridad.
	 */
	public int getPriority() {
		return this.priority;
	}

	/**
	 * Establece la prioridad de la petici&oacute;n. Cuando m&aacute;s alto el valor, m&aacute;s prioritario.
	 * @param priority Prioridad
	 */
	public void setPriority(final int priority) {
		this.priority = priority;
	}

	/**
	 * Indica si el proceso de firma forma parte de un flujo de trabajo.
	 * @return {@code true} cuando procede de un flujo de trabajo, {@code false} en caso contrario.
	 */
	public boolean isWorkflow() {
		return this.workflow;
	}

	/**
	 * Establece si el proceso de firma forma parte de un flujo de trabajo.
	 * @param workflow {@code true} cuando procede de un flujo de trabajo, {@code false} en caso contrario.
	 */
	public void setWorkflow(final boolean workflow) {
		this.workflow = workflow;
	}

	/**
	 * Indica si a petici&oacute;n se reenvi&oacute;n.
	 * @return {@code true} si se reenvi&oacute;, {@code false} en caso contrario.
	 */
	public boolean isForward() {
		return this.forward;
	}

	/**
	 * Establece si a petici&oacute;n se reenvi&oacute;n.
	 * @param forward {@code true} si se reenvi&oacute;, {@code false} en caso contrario.
	 */
	public void setForward(final boolean forward) {
		this.forward = forward;
	}

	/**
	 * Recupera el asunto de la petici&oacute;n.
	 * @return Asunto.
	 */
	public String getSubject() {
		return this.subject;
	}

	/**
	 * Establece el asunto de la petici&oacute;n.
	 * @param subject Asunto.
	 */
	public void setSubject(final String subject) {
		this.subject = subject;
	}

	/**
	 * Recupera el listado de usuarios que enviaron la petici&oacute;n.
	 * @return Listado de usuarios.
	 */
	public String[] getSenders() {
		return this.senders;
	}

	/**
	 * Establece el listado de usuarios que enviaron la petici&oacute;n.
	 * @param senders Listado de usuarios.
	 */
	public void setSenders(final String[] senders) {
		this.senders = senders;
	}

	/**
	 * Recupera la fecha de la petici&oacute;n.
	 * @return Fecha.
	 */
	public String getDate() {
		return this.date;
	}

	/**
	 * Establece la fecha de la petici&oacute;n.
	 * @param date Fecha.
	 */
	public void setDate(final String date) {
		this.date = date;
	}

	/**
	 * Recupera el nombre de la aplicaci&oacute;n que solicit&oacute; la firma.
	 * @return Nombre de la aplicaci&oacute;n.
	 */
	public String getApp() {
		return this.app;
	}

	/**
	 * Establece el nombre de la aplicaci&oacute;n que solicit&oacute; la firma.
	 * @param app Nombre de la aplicaci&oacute;n.
	 */
	public void setApp(final String app) {
		this.app = app;
	}

	/**
	 * Recupera la referencia de la petici&oacute;n.
	 * @return Referencia de la aplicaci&oacute;n.
	 */
	public String getRef() {
		return this.ref;
	}

	/**
	 * Establece la referencia de la petici&oacute;n.
	 * @param ref Referencia de la aplicaci&oacute;n.
	 */
	public void setRef(final String ref) {
		this.ref = ref;
	}

	/**
	 * Recupera el listado de l&iacute;neas de firma de la petici&oacute;n. Las l&iacute;neas de firma
	 * se componen de un listado de nombre de usuarios por los que debe pasar o ha pasado la firma.
	 * @return Listado de l&iacute;neas de firma.
	 */
	public Vector[] getSignLines() {
		return this.signLines;
	}

	/**
	 * Establece el listado de l&iacute;neas de firma de la petici&oacute;n. Las l&iacute;neas de firma
	 * se componen de un listado de nombre de usuarios por los que debe pasar o ha pasado la firma.
	 * @param signLines Listado de l&iacute;neas de firma.
	 */
	public void setSignLines(final Vector[] signLines) {
		this.signLines = signLines;
	}

	/**
	 * Recupera el listado de documentos que componen la petici&oacute;n de firma. La informaci&oacute;n
	 * de cada documento se almacena en objetos de tipo {@link es.gob.afirma.signfolder.server.proxy.SignRequestDocument}.
	 * @return Listado de documentos.
	 */
	public SignRequestDocument[] getDocs() {
		return this.docs;
	}

	/**
	 * Establece el listado de documentos que componen la petici&oacute;n de firma. La informaci&oacute;n
	 * de cada documento se almacena en objetos de tipo {@link es.gob.afirma.signfolder.server.proxy.SignRequestDocument}.
	 * @param docs Listado de documentos.
	 */
	public void setDocs(final SignRequestDocument[] docs) {
		this.docs = docs;
	}
}
