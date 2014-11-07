package es.gob.afirma.android.signfolder.proxy;


/** Petici&oacute;n de firma.
 * @author Carlos Gamuci */
public class SignRequest {

	/** Estado de solicitud de firma pendiente de firmar. */
	public static final String STATE_UNRESOLVED = "unresolved"; //$NON-NLS-1$

	/** Estado de solicitud de firma ya firmada. */
	public static final String STATE_SIGNED = "signed"; //$NON-NLS-1$

	/** Estado de solicitud de firma rechazada. */
	public static final String STATE_REJECTED = "rejected"; //$NON-NLS-1$

	/** Identificador correspondiente a las peticiones aun no vistas. */
	public static final String VIEW_NEW = "NUEVO"; //$NON-NLS-1$

	/** Identificador correspondiente a las peticiones ya vistas. */
	public static final String VIEW_READED = "LEIDO"; //$NON-NLS-1$

	/** Referencia &uacute;nica de la petici&oacute;n. */
	private final String id;

	/** Asunto de la petici&oacute;n. */
	private String subject;

	/** Peticionario de la petici&oacute;n. */
	private String[] senders;

	/** Estado que se debe mostrar de la petici&oacute;n (sin abrir, vista,...). */
	private String view;

	/** Para listados que admitan selecci&oacute;n, si la petici&oacute;n est&aacute; seleccionada o no. */
	private boolean selected;

	/** Fecha de la petici&oacute;n. */
	private String date;

	/** Prioridad de la petici&oacute;n. */
	private int priority;

	/** Indica si la petici&oacute;n forma parte de un flujo e trabajo. */
	private boolean workflow;

	/** Indica si alguien nos reenvi&oacute;n esta petici&oacute;n. */
	private boolean forward;

	/** Tipo de la petici&oacute;n. */
	private RequestType type;

	/** Tipos de petici&oacute;n */
	public enum RequestType {
		/** Firma. */
		SIGNATURE,
		/** Visto bueno. */
		APPROVE
	}

	/** Listado de documentos de la petici&oacute;n. */
	private SignRequestDocument[] docs;

	/** Construye la petici&oacute;n de firma.
	 * @param id Identificador &uacute;nico. */
	public SignRequest(final String id) {
		this.id = id;
	}

	/** Construye la petici&oacute;n de firma.
	 * @param id Identificador &uacute;nico.
	 * @param subject Asunto.
	 * @param sender Peticionario.
	 * @param view Vista que se debe mostrar (sin revisar, vista...
	 * @param date Fecha.
	 * @param priority Prioridad.
	 * @param workflow Si pertenece a un flujo de trabajo.
	 * @param forward Si se ha reenviado esta petici&oacute;n.
	 * @param type Tipo de petici&oacute;n.
	 * @param docs Documentos de la petici&oacute;n. */
	public SignRequest(final String id,
			           final String subject,
			           final String sender,
			           final String view,
			           final String date,
			           final int priority,
			           final boolean workflow,
			           final boolean forward,
			           final SignRequest.RequestType type,
			           final SignRequestDocument[] docs) {
		this.id = id;
		this.subject = subject;
		this.senders = new String[] { sender };
		this.view = view;
		this.date = date;
		this.priority = priority;
		this.workflow = workflow;
		this.forward = forward;
		this.type = type;
		this.docs = docs; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}

	/** Recupera el identificador de la petici&oacute;n.
	 * @return Identificador de la petici&oacute;n. */
	public String getId() {
		return this.id;
	}

	/** Recupera el asunto de la petici&oacute;n.
	 * @return Asunto de la petici&oacute;n. */
	public String getSubject() {
		return this.subject;
	}

	/** Recupera el peticionario de la petici&oacute;n.
	 * @return Peticionario de la petici&oacute;n. */
	public String getSender() {
		return this.senders[0];
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

	/**
	 * Recupera el listado de usuarios que enviaron la petici&oacute;n.
	 * @return Listado de usuarios.
	 */
	public String[] getSenders() {
		return this.senders;
	}

	/** Recupera la prioridad de la petici&oacute;n: 1(Normal), 2 (Alta), 3 (Muy alta) o 4 (Urgente).
	 * @return Peticionario de la petici&oacute;n. */
	public int getPriority() {
		return this.priority;
	}

	/** Indica si la firma de la petici&oacute;n forma parte de un flujo de trabajo.
	 * @return {@code true} cuando la petici&oacute;n forme parte de un flujo de trabajo,
	 * {@code false} en caso contrario. */
	public boolean isWorkflow() {
		return this.workflow;
	}

	/** Indica si alguien nos ha reenviado esta petici&oacute;n.
	 * @return {@code true} cuando nos han reenviado la petici&oacute;n, {@code false} en caso contrario. */
	public boolean isForward() {
		return this.forward;
	}

	/** Devuelve el tipo de petici&oacute;n de firma.
	 * @return Tipo de petici&oacute;n. */
	public RequestType getType() {
		return this.type;
	}

	/** Recupera el listado de documentos de la petici&oacute;n de firma.
	 * @return Listado de documentos. */
	public SignRequestDocument[] getDocs() {
		return this.docs; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}

	/** Indica si el elemento esta seleccionado en el listado presentado al usuario.
	 * @return {@ocde true} si el elemento est&aacute; selecionado, {@code false} en caso contrario.
	 */
	public boolean isSelected() {
		return this.selected;
	}

	/**
	 * Establece si el elemento del listado debe mostrarse seleccionado o no.
	 * @param selected {@code true} si se debe mostrar seleccionado, {@code false} en caso contrario.
	 */
	public void setSelected(final boolean selected) {
		this.selected = selected;
	}

	/**
	 * Invierte el estado de selecci&oacute;n de la petici&oacute;n.
	 */
	public void check() {
		this.selected = !this.selected;
	}

	public void setViewed(final boolean viewed) {
		this.view = viewed ? VIEW_READED : VIEW_NEW;
	}

	/**
	 * Establece el objeto de la petici&oacute;n.
	 * @param subject Motivo de la petici&oacute;n.
	 */
	public void setSubject(final String subject) {
		this.subject = subject;
	}

	/**
	 * Establece quienes enviaron la petici&oacute;n.
	 * @param senders Solicitantes de la petici&oacute;n.
	 */
	public void setSenders(final String[] senders) {
		this.senders = senders;
	}

	/**
	 * Establece la fecha.
	 * @param date Fecha.
	 */
	public void setDate(final String date) {
		this.date = date;
	}

	/**
	 * Estable el listado de documentos que hay que firmar.
	 * @param docs Listado de documentos.
	 */
	public void setDocs(final SignRequestDocument[] docs) {
		this.docs = docs;
	}

	/**
	 * Establece la prioridad de la petici&oacute;n.
	 * @param priority Prioridad: 1 (menor) - 4 (mayor)
	 */
	public void setPriority(final int priority) {
		this.priority = priority;
	}

	/**
	 * Establece si el proceso de firma forma parte de un flujo de trabajo.
	 * @param workflow {@code true} cuando procede de un flujo de trabajo, {@code false} en caso contrario.
	 */
	public void setWorkflow(final boolean workflow) {
		this.workflow = workflow;
	}

	/**
	 * Establece si a petici&oacute;n se reenvi&oacute;n.
	 * @param forward {@code true} si se reenvi&oacute;, {@code false} en caso contrario.
	 */
	public void setForward(final boolean forward) {
		this.forward = forward;
	}

	/**
	 * Establece el tipo de petici&oacute;n.
	 * @param type Tipo de petici&oacute;n.
	 */
	public void setType(final RequestType type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return this.subject + " (" + this.id + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
