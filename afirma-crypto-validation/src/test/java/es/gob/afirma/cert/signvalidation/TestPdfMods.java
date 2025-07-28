package es.gob.afirma.cert.signvalidation;

import java.io.InputStream;
import java.util.List;

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
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_FORM_MOD_PATH)) {

				boolean errorFound = false;

				final byte[] pades = AOUtil.getDataFromInputStream(is);
				final List<SignValidity> results = new ValidatePdfSignature().validate(pades, false);
				for (int i = 0; i < results.size(); i++) {
					final SignValidity result = results.get(i);
					if (result.getError() == SignValidity.VALIDITY_ERROR.MODIFIED_FORM) {
						errorFound = true;
					}
				}
				Assert.assertTrue("La firma no ha generado un error o no ha generado el esperado: " + SignValidity.VALIDITY_ERROR.MODIFIED_FORM, errorFound); //$NON-NLS-1$
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
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_FORM_PLAIN_MOD_PATH)) {

			boolean errorFound = false;

			final byte[] pades = AOUtil.getDataFromInputStream(is);
			final List<SignValidity> results = new ValidatePdfSignature().validate(pades, false);
			for (int i = 0; i < results.size(); i++) {
				final SignValidity result = results.get(i);
				if (result.getError() == SignValidity.VALIDITY_ERROR.MODIFIED_FORM) {
					errorFound = true;
				}
			}
			Assert.assertTrue("La firma no ha generado un error o no ha generado el esperado: " + SignValidity.VALIDITY_ERROR.MODIFIED_FORM, errorFound); //$NON-NLS-1$
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
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PDF_SHADOW_ATTACK_PATH)) {

			boolean errorFound = false;

			final byte[] pades = AOUtil.getDataFromInputStream(is);
			final List<SignValidity> results = new ValidatePdfSignature().validate(pades, false);
			for (int i = 0; i < results.size(); i++) {
				final SignValidity result = results.get(i);
				System.out.println(result);
				if (result.getError() == SignValidity.VALIDITY_ERROR.MODIFIED_DOCUMENT) {
					errorFound = true;
				}
			}
			Assert.assertTrue("La firma no ha generado un error o no ha generado el esperado: " + SignValidity.VALIDITY_ERROR.MODIFIED_DOCUMENT, errorFound); //$NON-NLS-1$
		}
	}
}
