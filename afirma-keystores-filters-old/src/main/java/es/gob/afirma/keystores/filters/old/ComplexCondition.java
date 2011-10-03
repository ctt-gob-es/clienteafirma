/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters.old;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;

import es.gob.afirma.core.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
final class ComplexCondition extends AClause implements ConditionConstants {
	
	/**
	 * @param str Texto de la expresi&oacute;n
	 * @throws AOException cuando ocurre cualquier problema
	 * @deprecated Usar filtros compatibles RFC2254
	 */
    @Deprecated
	public ComplexCondition(final String str) throws AOException {
        Matcher compoundConditionMatcher = compoundConditionPattern.matcher(str);
        Matcher nexusConditionMatcher = nexusConditionPattern.matcher(str);

        Collection<Clause> cClauses = new ArrayList<Clause>();
        Collection<String> cNexus = new ArrayList<String>();

        int pos = 0;

        if(nexusConditionMatcher.find()) {
            compoundConditionMatcher.find(0);
            int ini = compoundConditionMatcher.start();
            int fin = compoundConditionMatcher.end();
            String clause = str.substring(ini, fin);
            pos = fin;
            cClauses.add(new CompoundCondition(clause));
            while (pos < str.length() - 1 && pos > -1) {
                if (nexusConditionMatcher.find(pos)) {
                    ini = nexusConditionMatcher.start();
                    fin = nexusConditionMatcher.end();
                    String myNexus = str.substring(ini, fin);
                    pos = fin;
                    cNexus.add(myNexus);
                }
                else {
                    throw new AOException("Error de sintaxis en el filtro en la posicion " + pos + ": " + str); //$NON-NLS-1$ //$NON-NLS-2$
                }
  
                if (compoundConditionMatcher.find(pos)) {
                    ini = compoundConditionMatcher.start();
                    fin = compoundConditionMatcher.end();
                    clause = str.substring(ini, fin);
                    pos = fin;
                    cClauses.add(new CompoundCondition(clause));
                }
                else {
                    throw new AOException("Error de sintaxis, pos=" + pos + ": " + str); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
            this.clauses = cClauses.toArray(new Clause[cClauses.size()]);
            this.nexus = cNexus.toArray(new Nexus[cNexus.size()]);
        }
        else {
            this.clauses= new Clause[]{new SimpleCondition(str.substring(1, str.length()-1))};
            this.nexus= new Nexus[0];
        }

    }
}
