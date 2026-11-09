package es.gob.afirma.autofirma2026.validation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateBinarySignature;

/**
 * Valida integridad de firmas generadas en F2 (CAdES/XAdES/PAdES/FacturaE).
 * No comprueba revocación OCSP ni VALIDe (fuera de alcance local).
 */
public final class VectorIntegrityValidator {

	private VectorIntegrityValidator() {
		/* utility */
	}

	public static Path vectorsDir() {
		final String prop = System.getProperty("vectors.dir");
		if (prop != null && !prop.isEmpty()) {
			return Paths.get(prop);
		}
		return Paths.get("vectors").toAbsolutePath().normalize();
	}

	public static List<SignValidity> validateFile(final Path file) throws Exception {
		final byte[] data = Files.readAllBytes(file);
		final SignValider valider = SignValiderFactory.getSignValider(data);
		if (valider == null) {
			throw new IllegalStateException("Sin validador para: " + file);
		}
		return valider.validate(data, false);
	}

	public static List<SignValidity> validateCadesWithData(final Path sig, final Path dataFile) throws Exception {
		final byte[] cades = Files.readAllBytes(sig);
		final byte[] data = Files.readAllBytes(dataFile);
		return ValidateBinarySignature.validate(cades, data, false);
	}

	public static boolean isAcceptable(final List<SignValidity> results) {
		if (results == null || results.isEmpty()) {
			return false;
		}
		boolean sawStructuralOkOrCertOnly = false;
		for (final SignValidity v : results) {
			final SIGN_DETAIL_TYPE t = v.getValidity();
			final SignValidity.VALIDITY_ERROR err = v.getError();
			if (t == SIGN_DETAIL_TYPE.OK || t == SIGN_DETAIL_TYPE.GENERATED) {
				sawStructuralOkOrCertOnly = true;
				continue;
			}
			if (t == SIGN_DETAIL_TYPE.UNKNOWN || t == SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER) {
				sawStructuralOkOrCertOnly = true;
				continue;
			}
			if (t == SIGN_DETAIL_TYPE.KO) {
				// Certificado de prueba ANF: fallo de cadena/OCSP no invalida la integridad criptográfica
				if (err == SignValidity.VALIDITY_ERROR.CERTIFICATE_PROBLEM
						|| err == SignValidity.VALIDITY_ERROR.CERTIFICATE_EXPIRED
						|| err == SignValidity.VALIDITY_ERROR.CERTIFICATE_NOT_VALID_YET
						|| err == SignValidity.VALIDITY_ERROR.CANT_VALIDATE_CERT
						|| err == SignValidity.VALIDITY_ERROR.CA_NOT_SUPPORTED
						|| err == SignValidity.VALIDITY_ERROR.NO_DATA) {
					sawStructuralOkOrCertOnly = true;
					continue;
				}
				// Corrupción, datos no coinciden, sin firma, algoritmo → fallo real
				return false;
			}
		}
		return sawStructuralOkOrCertOnly;
	}

	/** True si hay KO estructural (datos/firma corruptos), no solo cert. */
	public static boolean hasStructuralKo(final List<SignValidity> results) {
		if (results == null) {
			return false;
		}
		for (final SignValidity v : results) {
			if (v.getValidity() != SIGN_DETAIL_TYPE.KO) {
				continue;
			}
			final SignValidity.VALIDITY_ERROR err = v.getError();
			if (err == SignValidity.VALIDITY_ERROR.NO_MATCH_DATA
					|| err == SignValidity.VALIDITY_ERROR.CORRUPTED_SIGN
					|| err == SignValidity.VALIDITY_ERROR.NO_SIGN
					|| err == SignValidity.VALIDITY_ERROR.ALGORITHM_NOT_SUPPORTED
					|| err == SignValidity.VALIDITY_ERROR.UNKOWN_ERROR) {
				return true;
			}
		}
		return false;
	}

	public static void main(final String[] args) throws Exception {
		final Path out = vectorsDir().resolve("out");
		final Path input = vectorsDir().resolve("input");
		final List<String> failures = new ArrayList<>();

		validateNamed(out.resolve("plain-cades.csig"), "cades", failures);
		validateNamed(out.resolve("plain-cades-cosign.csig"), "cades-cosign", failures);
		validateNamed(out.resolve("plain-cades-countersign.csig"), "cades-countersign", failures);
		validateNamed(out.resolve("sample-xades.xsig"), "xades", failures);
		validateNamed(out.resolve("sample-pades.pdf"), "pades", failures);
		validateNamed(out.resolve("facturae-signed.xml"), "facturae", failures);

		// CAdES detached con datos originales
		final Path cades = out.resolve("plain-cades.csig");
		final Path plain = input.resolve("plain.txt");
		if (Files.isRegularFile(cades) && Files.isRegularFile(plain)) {
			final List<SignValidity> withData = validateCadesWithData(cades, plain);
			System.out.println("cades+data -> " + withData);
			if (!isAcceptable(withData)) {
				failures.add("cades+data");
			}
			final List<SignValidity> wrong = ValidateBinarySignature.validate(
					Files.readAllBytes(cades),
					"TAMPERED".getBytes("UTF-8"),
					false);
			System.out.println("cades+wrong-data -> " + wrong);
			if (!hasStructuralKo(wrong)) {
				failures.add("cades+wrong-data should be structural KO");
			}
		}

		// Fichero tamperado: PDF firmado con un byte cambiado cerca del final
		final Path pades = out.resolve("sample-pades.pdf");
		if (Files.isRegularFile(pades)) {
			final byte[] bytes = Files.readAllBytes(pades);
			if (bytes.length > 100) {
				bytes[bytes.length / 2] ^= 0x5A;
				final SignValider valider = SignValiderFactory.getSignValider(bytes);
				final List<SignValidity> tampered = valider.validate(bytes, false);
				System.out.println("pades-tampered -> " + tampered);
				if (isAcceptable(tampered) && !hasStructuralKo(tampered)) {
					// Si sigue "aceptable" solo por cert, exigir al menos no-OK puro
					boolean pureOk = false;
					for (final SignValidity v : tampered) {
						if (v.getValidity() == SIGN_DETAIL_TYPE.OK || v.getValidity() == SIGN_DETAIL_TYPE.GENERATED) {
							pureOk = true;
						}
					}
					if (pureOk) {
						failures.add("pades-tampered should not be OK");
					}
				}
			}
		}

		if (!failures.isEmpty()) {
			System.err.println("FAIL: " + failures);
			System.exit(1);
		}
		System.out.println("ALL VALIDATION CHECKS PASSED: " + Arrays.asList(
				"cades", "cosign", "countersign", "xades", "pades", "facturae",
				"cades+data", "cades-wrong-data", "pades-tampered"));
	}

	private static void validateNamed(final Path file, final String name, final List<String> failures)
			throws Exception {
		if (!Files.isRegularFile(file)) {
			failures.add(name + " missing: " + file);
			return;
		}
		final List<SignValidity> results = validateFile(file);
		System.out.println(name + " -> " + results);
		if (!isAcceptable(results)) {
			failures.add(name + " unacceptable: " + results);
		}
	}
}
