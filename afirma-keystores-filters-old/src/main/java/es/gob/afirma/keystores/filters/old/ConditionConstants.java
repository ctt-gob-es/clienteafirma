package es.gob.afirma.keystores.filters.old;

import java.util.regex.Pattern;


/** @deprecated */
@Deprecated
interface ConditionConstants {
    static final String SEQ = "([^\"]|[^\\{\\}])*"; //$NON-NLS-1$

    static final String VALUE_PATTERN = "\\{\"" + SEQ + "\"\\}()"; //$NON-NLS-1$ //$NON-NLS-2$

    static final String HASH_ALG_PATTERN = "(MD5|SHA1)"; //$NON-NLS-1$

    static final String NEXO_PATTERN = "(\\&\\&|\\|\\|)"; //$NON-NLS-1$

    static final String OPERATOR_PATTERN = "(\\=|\\#MATCHES\\#|\\#NOT_MATCHES\\#)"; //$NON-NLS-1$

    static final String FIELD_PATTERN = "((ISSUER\\.|SUBJECT\\.)(DN|SN|SERIALNUMBER|FP\\(" + HASH_ALG_PATTERN + "\\)))"; //$NON-NLS-1$ //$NON-NLS-2$

    static final String SIMPLE_CONDITION_PATTERN = FIELD_PATTERN + OPERATOR_PATTERN + VALUE_PATTERN;

    static final String COMPOUND_CONDITION_PATTERN = "\\{" + SIMPLE_CONDITION_PATTERN + "(" + NEXO_PATTERN + SIMPLE_CONDITION_PATTERN + ")*\\}"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    static final String COMPLEX_CONDITION_PATTERN = COMPOUND_CONDITION_PATTERN + "(" + NEXO_PATTERN + COMPOUND_CONDITION_PATTERN + ")*"; //$NON-NLS-1$ //$NON-NLS-2$

    static final Pattern complexConditionPattern = Pattern.compile(COMPLEX_CONDITION_PATTERN);

    static final Pattern compoundConditionPattern = Pattern.compile(COMPOUND_CONDITION_PATTERN);

    static final Pattern simpleConditionPattern = Pattern.compile(SIMPLE_CONDITION_PATTERN);

    static final Pattern nexusConditionPattern = Pattern.compile(NEXO_PATTERN);

    static final Pattern fieldConditionPattern = Pattern.compile(FIELD_PATTERN);

    static final Pattern valueConditionPattern = Pattern.compile(VALUE_PATTERN);

    static final Pattern hashAlgPattern = Pattern.compile(HASH_ALG_PATTERN);

    static final Pattern operatorPattern = Pattern.compile(OPERATOR_PATTERN);

    static final HexHelper HEX_HELPER = HexHelper.getInstance();

}
