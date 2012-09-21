package es.gob.afirma.signfolder.proxy;


/** Petici&oacute;n de firma.
 * @author Carlos Gamuci */
public class SignRequest {

	/** Referencia &uacute;nica de la petici&oacute;n. */
	private final String ref;

	/** Asunto de la petici&oacute;n. */
	private final String subject;

	/** Peticionario de la petici&oacute;n. */
	private final String sender;

	/** Estado que se debe mostrar de la petici&oacute;n (sin abrir, vista,...). */
	private final String view;

	/** Fecha de la petici&oacute;n. */
	private final String date;

	/** Prioridad de la petici&oacute;n. */
	private final int priority;

	/** Indica si la petici&oacute;n forma parte de un flujo e trabajo. */
	private final boolean worflow;

	/** Indica si alguien nos reenvi&oacute;n esta petici&oacute;n. */
	private final boolean forward;

	/** Listado de documentos de la petici&oacute;n. */
	private final SignRequestDocument[] docs;

	/** Construye la petici&oacute;n de firma.
	 * @param ref Referencia &uacute;nica.
	 * @param subject Asunto.
	 * @param sender Peticionario.
	 * @param view Vista que se debe mostrar (sin revisar, vista...
	 * @param date Fecha.
	 * @param priority Prioridad.
	 * @param workflow Si pertenece a un flujo de trabajo.
	 * @param forward Si se ha reenviado esta petici&oacute;n.
	 * @param docs Documentos de la petici&oacute;n. */
	public SignRequest(final String ref,
			           final String subject,
			           final String sender,
			           final String view,
			           final String date,
			           final int priority,
			           final boolean workflow,
			           final boolean forward,
			           final SignRequestDocument[] docs) {
		this.ref = ref;
		this.subject = subject;
		this.sender = sender;
		this.view = view;
		this.date = date;
		this.priority = priority;
		this.worflow = workflow;
		this.forward = forward;
		this.docs = docs; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}

	/** Recupera la referencia de la petici&oacute;n.
	 * @return Referencia de la petici&oacute;n. */
	public String getRef() {
		return this.ref;
	}

	/** Recupera el asunto de la petici&oacute;n.
	 * @return Asunto de la petici&oacute;n. */
	public String getSubject() {
		return this.subject;
	}

	/** Recupera el peticionario de la petici&oacute;n.
	 * @return Peticionario de la petici&oacute;n. */
	public String getSender() {
		return this.sender;
	}

	/** Recupera la vista que debe ofrecerse de la petici&oacute;n: Nueva peticion (new), vista (viewed), etc.
	 * @return Referencia de la petici&oacute;n. */
	public String getView() {
		return this.view;
	}

	/** Recupera la fecha de la petici&oacute;n.
	 * @return Fecha de la petici&oacute;n. */
	public String getDate() {
		return this.date;
	}

	/** Recupera la prioridad de la petici&oacute;n: 1(Normal), 2 (Alta), 3 (Muy alta) o 4 (Urgente).
	 * @return Peticionario de la petici&oacute;n. */
	public int getPriority() {
		return this.priority;
	}

	/** Indica si la firma de la petici&oacute;n forma parte de un flujo de trabajo.
	 * @return {@code true} cuando la petici&oacute;n forme parte de un flujo de trabajo,
	 * {@code false} en caso contrario. */
	public boolean isWorflow() {
		return this.worflow;
	}

	/** Indica si alguien nos ha reenviado esta petici&oacute;n.
	 * @return {@code true} cuando nos han reenviado la petici&oacute;n, {@code false} en caso contrario. */
	public boolean isForward() {
		return this.forward;
	}

	/** Recupera el listado de documentos de la petici&oacute;n de firma.
	 * @return Listado de documentos. */
	public SignRequestDocument[] getDocs() {
		return this.docs; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}
}
