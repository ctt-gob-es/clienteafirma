package es.gob.afirma.signers.xades;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Pruebas de obtenci&oacute;n de estructura de firmantes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestGetSignersStructure {

	private static final String TEST_COFIRMA_A_1 = "/firma_cofirma_xades_longeva.xml"; //$NON-NLS-1$
	private static final String TEST_COFIRMA_A_2 = "/firma_cofirma_xades_longeva2.xml"; //$NON-NLS-1$

	/** Prueba de obtenci&oacute;n de estructura de firmantes de una firma XAdES-A.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testGetSignersStructureXadesA() throws Exception {
		final byte[] sign = AOUtil.getDataFromInputStream(
			TestGetSignersStructure.class.getResourceAsStream(TEST_COFIRMA_A_1)
		);
		final AOXAdESSigner signerXades = new AOXAdESSigner();
		final AOTreeModel arbol = signerXades.getSignersStructure(sign, true);
		System.out.println(arbol);
	}

	/** Prueba de obtenci&oacute;n de estructura de firmantes de una contrafirma XAdES-A.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testGetSignersStructureXadesACounter() throws Exception {
		final byte[] sign = AOUtil.getDataFromInputStream(
				TestGetSignersStructure.class.getResourceAsStream(TEST_COFIRMA_A_2)
			);
		final AOXAdESSigner signerXades = new AOXAdESSigner();
		final AOTreeModel arbol = signerXades.getSignersStructure(sign, true);

		Assert.assertEquals("No se ha detectado las firmas esperadas", 2, AOTreeModel.getChildCount(arbol.getRoot())); //$NON-NLS-1$
	}

}
