package es.gob.afirma.signers.cms;

/** Par&aacute;metros adicionales para firmas CMS. */
public final class AOCMSExtraParams {

    /** Modo de firma. */
    static final String MODE = "mode"; //$NON-NLS-1$

    /** Algoritmo de huella digital cuando este se proporciona precalculada. */
    public static final String PRECALCULATED_HASH_ALGORITHM = "precalculatedHashAlgorithm";//$NON-NLS-1$

    /** <code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma,
     * <code>false</code> en caso contrario. */
    static final String APPLY_SYSTEM_DATE = "applySystemDate"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private AOCMSExtraParams(){
        // No instanciable
    }
}
