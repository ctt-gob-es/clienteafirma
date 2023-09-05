package es.gob.afirma.cert.signvalidation;

import java.io.InputStream;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.ValidatePdfSignature;

/**
 * Pruebas de deteccion de cambios en documentos PDF firmados.
 */
public class TestPdfMods {

	private static final String PDF_FORM_MOD_PATH = "RFE023 - Formulario_prueba_PDF_mod.pdf"; //$NON-NLS-1$
	private static final String PDF_FORM_PLAIN_MOD_PATH = "RFE023 - shadow form.pdf"; //$NON-NLS-1$
	private static final String PDF_SHADOW_ATTACK_PATH = "Ejemplo_PDF_Shadow_Attack_1.pdf"; //$NON-NLS-1$

	/**
	 * Caso de prueba de deteccion de cambios en un formulario PDF corriente despu&eacute;s de
	 * haber modificado el documento.
	 * @throws Exception Cuando ocurre cualquier error no esperado en la prueba.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testComprobarFormularioModificado() throws Exception {
		try (
				final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_FORM_MOD_PATH);
			) {
				final byte[] pades = AOUtil.getDataFromInputStream(is);
				Assert.assertEquals("La firma no ha generado un error o genera uno distinto al esperado.", SignValidity.VALIDITY_ERROR.MODIFIED_FORM, new ValidatePdfSignature().validate(pades, false).getError()); //$NON-NLS-1$
			}
	}

	/**
	 * Caso de prueba de deteccion de cambios en un formulario PDF con campos mal declarados
	 * despu&eacute;s de haber modificado el documento.
	 * @throws Exception Cuando ocurre cualquier error no esperado en la prueba.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testComprobarFormularioModificadoConCamposSinDeclarar() throws Exception {
		try (
				final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_FORM_PLAIN_MOD_PATH);
			) {
				final byte[] pades = AOUtil.getDataFromInputStream(is);
				Assert.assertEquals("La firma no ha generado un error o genera uno distinto al esperado.", SignValidity.VALIDITY_ERROR.MODIFIED_FORM, new ValidatePdfSignature().validate(pades, false).getError()); //$NON-NLS-1$
			}
	}

	/**
	 * Caso de prueba de deteccion de cambios en un formulario PDF con campos mal declarados
	 * despu&eacute;s de haber modificado el documento.
	 * @throws Exception Cuando ocurre cualquier error no esperado en la prueba.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testComprobarPdfShadowAttack() throws Exception {
		try (
				final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_SHADOW_ATTACK_PATH);
			) {
				final byte[] pades = AOUtil.getDataFromInputStream(is);
				Assert.assertEquals("La firma no ha generado un error o genera uno distinto al esperado.", SignValidity.VALIDITY_ERROR.MODIFIED_DOCUMENT, new ValidatePdfSignature().validate(pades, false).getError()); //$NON-NLS-1$
			}
	}
}
