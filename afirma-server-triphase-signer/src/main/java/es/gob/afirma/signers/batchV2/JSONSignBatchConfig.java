package es.gob.afirma.signers.batchV2;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.signers.batch.SingleSignConstants.SignAlgorithm;
import es.gob.afirma.signers.batchV2.JSONSingleSignConstants.SignFormat;
import es.gob.afirma.signers.batchV2.JSONSingleSignConstants.SignSubOperation;

/** Configuraci&oacute;n para la firma de un lote.*/
class JSONSignBatchConfig {

	/**
	 * Tiempo de espera que, por defecto, se aplicar&aacute;a a las distintas
	 * operaciones de firma concurrente de datos.
	 * */
	private static final long DEFAULT_TIMEOUT = 30;

	private String id;

	private boolean stopOnError;

	private SignAlgorithm algorithm;

	private long concurrentTimeout;

	private final SignFormat format;

	private final SignSubOperation subOperation;

	private final List<JSONSingleSign> signs;

	/** * Construye un lote vac&iacute;o. */
	public JSONSignBatchConfig() {
		this.id = null;
		this.stopOnError = true;
		this.concurrentTimeout = DEFAULT_TIMEOUT;
		this.signs = new ArrayList<>();
		this.format = null;
		this.subOperation = null;
	}

	/**
	 * Obtiene el ID del lote.
	 * @return Identificador del lote o {@code null} si no est&aacute; definido.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Indica si se debe detener la ejecuci&oacute;n del lote al detectar un error.
	 * @return {@code true}, valor por defecto, si se debe detener la ejecuci&oacute;n,
	 * {@code false} en caso contrario.
	 */
	public boolean isStopOnError() {
		return this.stopOnError;
	}

	/**
	 * Devuelve el algoritmo de firma.
	 * @return Algoritmo de firma.
	 */
	public SignAlgorithm getAlgorithm() {
		return this.algorithm;
	}

	/**
	 * Devuelve el tiempo de espera m&aacute;ximo para la ejecuci&oacute;n del lote.
	 * Por defecto, devuelve el valor 0 (se espera indefinidamente).
	 * @return Tiempo de espera en milisegundos o 0 si se espera indefinidamente.
	 */
	public long getConcurrentTimeout() {
		return this.concurrentTimeout;
	}

	/**
	 * Establece el identificador del lote.
	 * @param id Identificador del lote.
	 */
	public void setId(final String id) {
		this.id = id;
	}

	/**
	 * Establece si debe detenerse la ejecuci&oacute;n del lote en caso de error.
	 * @param stopOnError {@code true} si se debe detener la ejecuci&oacute;n,
	 * {@code false} en caso contrario.
	 */
	public void setStopOnError(final boolean stopOnError) {
		this.stopOnError = stopOnError;
	}

	/**
	 * Establece el algoritmo de firma para los documentos del lote.
	 * @param algorithm Algoritmo de firma.
	 */
	public void setAlgorithm(final SignAlgorithm algorithm) {
		this.algorithm = algorithm;
	}

	/**
	 * Establece el tiempo m&aacute;ximo de espera para la firma del lote.
	 * @param concurrentTimeout Tiempo m&aacute;ximo de espera o 0 si se
	 * espera indefinidamente.
	 */
	public void setConcurrentTimeout(final long concurrentTimeout) {
		this.concurrentTimeout = concurrentTimeout;
	}

	/**
	 * Obtiene el formato configurado
	 * @return Formato configurado en la operaci&oacute;n batch
	 * */
	public SignFormat getFormat() {
		return this.format;
	}

	/**
	 * Obtiene la operacion configurada
	 * @return Operaci&oacute;n configurada en la ejecuci&oacute;n batch
	 * */
	public SignSubOperation getSubOperation() {
		return this.subOperation;
	}

	/**
	 * Agrega un nuevo documento al lote.
	 * @param sign Informaci&oacute;n necesaria para la firma del documento.
	 */
	public void addSingleSign(final JSONSingleSign sign) {
		this.signs.add(sign);
	}

	/**
	 * Obtiene el listado de configuraciones de los documentos a firmar.
	 * @return Listado de configuraciones de los documentos a firmar.
	 */
	public List<JSONSingleSign> getSingleSigns() {
		return this.signs;
	}

}
