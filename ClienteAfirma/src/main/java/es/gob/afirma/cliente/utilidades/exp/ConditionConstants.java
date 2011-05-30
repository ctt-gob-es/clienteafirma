package es.gob.afirma.cliente.utilidades.exp;

import java.util.regex.Pattern;

/**
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface ConditionConstants {
	static final String SEQ = "([^\"]|[^\\{\\}])*";

	static final String VALUE_PATTERN = "\\{\"" + SEQ + "\"\\}()";

	static final String HASH_ALG_PATTERN = "(MD5|SHA1)";

	static final String NEXO_PATTERN = "(\\&\\&|\\|\\|)";

	static final String OPERATOR_PATTERN = "(\\=|\\#MATCHES\\#|\\#NOT_MATCHES\\#)";

	static final String FIELD_PATTERN = "((ISSUER\\.|SUBJECT\\.)(DN|SN|SERIALNUMBER|FP\\("
			+ HASH_ALG_PATTERN + "\\)))";

	static final String SIMPLE_CONDITION_PATTERN = FIELD_PATTERN
			+ OPERATOR_PATTERN + VALUE_PATTERN;

	static final String COMPOUND_CONDITION_PATTERN = "\\{"
			+ SIMPLE_CONDITION_PATTERN + "(" + NEXO_PATTERN
			+ SIMPLE_CONDITION_PATTERN + ")*\\}";

	static final String COMPLEX_CONDITION_PATTERN = COMPOUND_CONDITION_PATTERN
			+ "(" + NEXO_PATTERN + COMPOUND_CONDITION_PATTERN + ")*";

	static final Pattern complexConditionPattern = Pattern
			.compile(COMPLEX_CONDITION_PATTERN);

	static final Pattern compoundConditionPattern = Pattern
			.compile(COMPOUND_CONDITION_PATTERN);

	static final Pattern simpleConditionPattern = Pattern
			.compile(SIMPLE_CONDITION_PATTERN);

	static final Pattern nexusConditionPattern = Pattern.compile(NEXO_PATTERN);

	static final Pattern fieldConditionPattern = Pattern.compile(FIELD_PATTERN);

	static final Pattern valueConditionPattern = Pattern.compile(VALUE_PATTERN);

	static final Pattern hashAlgPattern = Pattern.compile(HASH_ALG_PATTERN);

	static final Pattern operatorPattern = Pattern.compile(OPERATOR_PATTERN);

	static final HexHelper HEX_HELPER = HexHelper.getInstance();

}
