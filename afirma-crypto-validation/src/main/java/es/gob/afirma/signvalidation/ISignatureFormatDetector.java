package es.gob.afirma.signvalidation;

/**
 * <p>Interface that defines the recognized signature formats.</p>
 * <b>Project:</b><p>Library for the integration with the services of @Firma, eVisor and TS@.</p>
 * @version 1.2, 14/03/2017.
 */
public interface ISignatureFormatDetector {

    /**
     * Constant attribute that identifies the unrecognized signature format.
     */
    String FORMAT_UNRECOGNIZED = "No reconocido"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-A signature format.
     */
    String FORMAT_CADES_A = "CAdES-A"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-XL1 signature format.
     */
    String FORMAT_CADES_XL1 = "CAdES-XL1"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-XL2 signature format.
     */
    String FORMAT_CADES_XL2 = "CAdES-XL2"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-X1 signature format.
     */
    String FORMAT_CADES_X1 = "CAdES-X1"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-X2 signature format.
     */
    String FORMAT_CADES_X2 = "CAdES-X2"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-C signature format.
     */
    String FORMAT_CADES_C = "CAdES-C"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-T signature format.
     */
    String FORMAT_CADES_T = "CAdES-T"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-EPES signature format.
     */
    String FORMAT_CADES_EPES = "CAdES-EPES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES-BES signature format.
     */
    String FORMAT_CADES_BES = "CAdES-BES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CMS signature format.
     */
    String FORMAT_CMS = "CMS"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CMS-T signature format.
     */
    String FORMAT_CMS_T = "CMS-T"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-A signature format.
     */
    String FORMAT_XADES_A = "XAdES-A"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-XL1 signature format.
     */
    String FORMAT_XADES_XL1 = "XAdES-XL1"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-XL2 signature format.
     */
    String FORMAT_XADES_XL2 = "XAdES-XL2"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-X1 signature format.
     */
    String FORMAT_XADES_X1 = "XAdES-X1"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-X2 signature format.
     */
    String FORMAT_XADES_X2 = "XAdES-X2"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-C signature format.
     */
    String FORMAT_XADES_C = "XAdES-C"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-T signature format.
     */
    String FORMAT_XADES_T = "XAdES-T"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-EPES signature format.
     */
    String FORMAT_XADES_EPES = "XAdES-EPES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES-BES signature format.
     */
    String FORMAT_XADES_BES = "XAdES-BES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PDF signature format.
     */
    String FORMAT_PDF = "PDF"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES-Basic signature format.
     */
    String FORMAT_PADES_BASIC = "PAdES-Basic"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES-BES signature format.
     */
    String FORMAT_PADES_BES = "PAdES-BES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES-EPES signature format.
     */
    String FORMAT_PADES_EPES = "PAdES-EPES"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES-LTV signature format.
     */
    String FORMAT_PADES_LTV = "PAdES-LTV"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES B-Level signature format.
     */
    String FORMAT_CADES_B_LEVEL = "CAdES B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES B-B-Level signature format.
     */
    String FORMAT_CADES_B_B_LEVEL = "CAdES B-B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES T-Level signature format.
     */
    String FORMAT_CADES_T_LEVEL = "CAdES T-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES LT-Level signature format.
     */
    String FORMAT_CADES_LT_LEVEL = "CAdES LT-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies CAdES LTA-Level signature format.
     */
    String FORMAT_CADES_LTA_LEVEL = "CAdES LTA-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES B-Level signature format.
     */
    String FORMAT_XADES_B_LEVEL = "XAdES B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES B-Level signature format.
     */
    String FORMAT_XADES_B_B_LEVEL = "XAdES B-B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES T-Level signature format.
     */
    String FORMAT_XADES_T_LEVEL = "XAdES T-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES LT-Level signature format.
     */
    String FORMAT_XADES_LT_LEVEL = "XAdES LT-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies XAdES LTA-Level signature format.
     */
    String FORMAT_XADES_LTA_LEVEL = "XAdES LTA-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES B-Level signature format.
     */
    String FORMAT_PADES_B_LEVEL = "PAdES B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES B-B-Level signature format.
     */
    String FORMAT_PADES_B_B_LEVEL = "PAdES B-B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES T-Level signature format.
     */
    String FORMAT_PADES_T_LEVEL = "PAdES T-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES LT-Level signature format.
     */
    String FORMAT_PADES_LT_LEVEL = "PAdES LT-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies PAdES LTA-Level signature format.
     */
    String FORMAT_PADES_LTA_LEVEL = "PAdES LTA-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies ASiC-S B-Level signature format.
     */
    String FORMAT_ASIC_S_B_LEVEL = "ASiC-S B-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies ASiC-S T-Level signature format.
     */
    String FORMAT_ASIC_S_T_LEVEL = "ASiC-S T-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies ASiC-S LT-Level signature format.
     */
    String FORMAT_ASIC_S_LT_LEVEL = "ASiC-S LT-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that identifies ASiC-S LTA-Level signature format.
     */
    String FORMAT_ASIC_S_LTA_LEVEL = "ASiC-S LTA-Level"; //$NON-NLS-1$

    /**
     * Constant attribute that represents the string to identify the CAdES basic profile.
     */
    String CADES_BASIC_FORMAT = "CADES"; //$NON-NLS-1$

    /**
     * Constant attribute that represents the string to identify the <i>PKCS7</i> basic format.
     */
    String PKCS7_BASIC_FORMAT = "PKCS7"; //$NON-NLS-1$

}
