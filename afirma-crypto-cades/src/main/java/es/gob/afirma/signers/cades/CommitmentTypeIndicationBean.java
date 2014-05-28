package es.gob.afirma.signers.cades;

import java.util.ArrayList;
import java.util.List;

/** Indicaci&oacute;n sobre el tipo de compromiso adquirido con la firma (<i>CommitmentTypeIndication</i>).
 * <pre>
 * CommitmentTypeIndication ::= SEQUENCE {
 *   commitmentTypeId           CommitmentTypeIdentifier,
 *   commitmentTypeQualifier    SEQUENCE SIZE (1..MAX) OF CommitmentTypeQualifier OPTIONAL
 * }
 *
 * CommitmentTypeIdentifier ::= OBJECT IDENTIFIER
 *
 * CommitmentTypeQualifier ::= SEQUENCE {
 *   commitmentTypeIdentifier   CommitmentTypeIdentifier,
 *   qualifier                  ANY DEFINED BY commitmentTypeIdentifier
 * }
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class CommitmentTypeIndicationBean {

	private final String identifier;
	private final List<String> qualifiers;

	CommitmentTypeIndicationBean(final String id, final List<String> quals) {
		if (id == null) {
			throw new IllegalArgumentException(
				"El obligatorio proporcionar un identificador no nulo" //$NON-NLS-1$
			);
		}
		this.identifier = id;
		this.qualifiers = quals;
	}

	String getIdentifier() {
		return this.identifier;
	}

	List<String> getQualifiers() {
		if (this.qualifiers == null) {
			return new ArrayList<String>(0);
		}
		return this.qualifiers;
	}

}