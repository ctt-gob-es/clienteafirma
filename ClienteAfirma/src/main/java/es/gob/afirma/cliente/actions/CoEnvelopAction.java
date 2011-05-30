package es.gob.afirma.cliente.actions;

import es.gob.afirma.cliente.EnveloperManager;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificatesNotFoundException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;

/**
 * Acci&oacute;n privilegiada para agregar un nuevo remitentes a un sobre
 * electr&oacute;nico. La ejecuci&oacute;n de la acci&oacute;n devuelve
 * {@code true} o {@code false} y el resultado almacenado es un array de bytes.
 */
public final class CoEnvelopAction extends
		BasicPrivilegedAction<Boolean, byte[]> {

	/** Manejador de ensobrado. */
	private EnveloperManager enveloperManager;

	/** Envoltorio que se desea desensobrar. */
	private byte[] envelop;

	/**
	 * Envoltorio de m&uacute;ltiples remitentes.
	 * 
	 * @param enveloperManager
	 *            Gestor de envoltorios
	 * @param envelop
	 *            Envoltorio
	 */
	public CoEnvelopAction(EnveloperManager enveloperManager, byte[] envelop) {
		if (enveloperManager == null) {
			throw new NullPointerException();
		}

		this.enveloperManager = enveloperManager;
		this.envelop = envelop;
	}

	public Boolean run() {
		try {
			enveloperManager.coEnvelop(envelop);
		} catch (final AOCancelledOperationException e) {
			setError("Operacion cancelada por el usuario", e); //$NON-NLS-1$
			return false;
		} catch (final AOKeyStoreManagerException e) {
			setError(
					"No se ha podido acceder al almac&eacute;n de certificados seleccionado", e); //$NON-NLS-1$
			return false;
		} catch (final AOCertificatesNotFoundException e) {
			setError(
					"No se han encontrado certificados en el almacen seleccionado", e); //$NON-NLS-1$
			return false;
		} catch (final AOInvalidFormatException e) {
			setError("No se ha proporcionado un envoltorio soportado", e); //$NON-NLS-1$
			return false;
		} catch (final AOException e) {
			setError("Error al agregar el nuevo remitente", e); //$NON-NLS-1$
			return false;
		}

		this.setResult(enveloperManager.getEnvelopedData());

		return true;
	}
}
