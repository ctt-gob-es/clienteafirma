package es.gob.afirma.miniapplet.actions;

import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;

/**
 * Acci&oacute;n para la seleccion de un manejador de firma.
 * @author Carlos Gamuci Mill&aacute;n
 */
public final class SelectSignerAction implements PrivilegedExceptionAction<AOSigner> {
    
    private final String format;
    private final byte[] data;
    
    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con el formato de firma indicado.
     * @param format Formato de firma.
     */
    public SelectSignerAction(final String format) {
        this.format = format;
        this.data = null;
    }
    
    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con la firma indicada.
     * @param data Firma electr&oacute;nica para la que se desea el manejador.
     */
    public SelectSignerAction(final byte[] data) {
        this.data = data;
        this.format = null;
    }
    
    /**
     * Selecciona el manejador de firma adecuado para el formato o los datos indicados.
     * Si no se encuentra un manejador compatible, se devuelve {@code null}.
     * @return Manejador de firma.
     */
	@Override
	public AOSigner run() {
		if (this.format != null) {
			return AOSignerFactory.getSigner(this.format);	
		} else if (this.data != null) {
			return AOSignerFactory.getSigner(this.data);
		}
        return null;
	}
}
