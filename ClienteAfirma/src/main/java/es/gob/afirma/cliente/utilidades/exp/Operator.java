package es.gob.afirma.cliente.utilidades.exp;

import java.util.regex.Pattern;

/**
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface Operator {
	public static Operator EQ = new Operator() {
		public boolean eval(Object o1, Object o2) {
			return o1.equals(o2);
		}
	};

	public static Operator MATCHES = new Operator() {
		public boolean eval(Object str, Object pattern) {
			boolean matches;
			matches = Pattern.matches((String) pattern, (String) str);
			return matches;
		}
	};

	public static Operator NOT_MATCHES = new Operator() {
		public boolean eval(Object str, Object pattern) {
			boolean matches;
			matches = Pattern.matches((String) pattern, (String) str);
			return !matches;
		}
	};

	public boolean eval(Object o1, Object o2);
}
