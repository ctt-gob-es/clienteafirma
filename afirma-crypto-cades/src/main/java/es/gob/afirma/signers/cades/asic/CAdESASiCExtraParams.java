package es.gob.afirma.signers.cades.asic;

final class CAdESASiCExtraParams {

    /** En las firmas simples CAdES ASiC-S, determina el nombre del fichero de datos dentro del contenedor.<br>
     * Si no se estabkece este par&aacute;metro se usa el nombre <i>dataobject</i> con la extensi&oacute;n correspondiente
     * al tipo de datos, o <i>.bin</i> si esta primera no puede determinarse de forma autom&aacute;tica. */
    static final String ASICS_FILENAME = "asicsFilename"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private CAdESASiCExtraParams(){
        // No instanciable
    }
}
