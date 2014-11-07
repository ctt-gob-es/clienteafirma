package es.gob.afirma.android.signfolder;

import es.gob.afirma.android.signfolder.proxy.RequestResult;

/** Escucha las operaciones realizadas sobre las peticiones del usuario y act&uacute;a seg&uacute;n el resultado. */
interface OperationRequestListener {

	public final static int REJECT_OPERATION = 1;
	public final static int SIGN_OPERATION = 2;
	public final static int APPROVE_OPERATION = 3;

	/** M&eacute;todo a ejecutar cuando termina la operaci&oacute;n.
	 * @param operation Tipo de operaci&oacute;n.
	 * @param requestResult Resultado de la operaci&oacute;n. */
	void requestOperationFinished(int operation, RequestResult requestResult);

	/** M&eacute;todo a ejecutar cuando ocurri&oacute; un error en la operaci&oacute;n.
	 * @param operation Tipo de operaci&oacute;n.
	 * @param requestResult Resultado de la petici&oacute; procesada.
	 * @param t Excepcion/Error que hizo fallar el procesamiento de las peticiones. */
	void requestOperationFailed(int operation, RequestResult requestResult, Throwable t);
}
