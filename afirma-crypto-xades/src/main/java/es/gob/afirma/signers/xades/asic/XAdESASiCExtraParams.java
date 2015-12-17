package es.gob.afirma.signers.xades.asic;

final class XAdESASiCExtraParams {

    /** Determina el nombre del fichero de datos dentro del contenedor.
     * Si no se estabkece este par&aacute;metro se usa el nombre <code>dataobject</code> con la extensi&oacute;n correspondiente al tipo de datos,
     * o <code>.bin</code> si esta primera no puede determinarse de forma autom&aacute;tica. */
    public static final String ASICS_FILENAME = "asicsFilename"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private XAdESASiCExtraParams(){
        // No instanciable
    }
}
