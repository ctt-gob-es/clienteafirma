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
final class CompoundCondition extends AClause implements ConditionConstants {

    CompoundCondition(String str) throws AOException {
        // Matcher simpleConditionMatcher = COMPILED_SIMPLE_CONDITION_PATERN.matcher(str);
        Matcher nexusConditionMatcher = COMPILED_NEXUS_CONDITION_PATTERN.matcher(str);

        Collection<Clause> cClauses = new ArrayList<Clause>();
        Collection<Nexus> cNexus = new ArrayList<Nexus>();

        if (nexusConditionMatcher.find()) {
            int pos = 1;
            int ini = 0, fin = 0;

            String clause;
            while (nexusConditionMatcher.find(pos)) {
                ini = nexusConditionMatcher.start();
                fin = nexusConditionMatcher.end();
                String myNexus = str.substring(ini, fin);
                cNexus.add(myNexus.equals("&&") ? Nexus.AND : Nexus.OR); //$NON-NLS-1$

                clause = str.substring(pos, ini);
                cClauses.add(new SimpleCondition(clause));
                pos = fin;
            }
            clause = str.substring(fin, str.length() - 1);
            cClauses.add(new SimpleCondition(clause));
        }
        else {
            String clause = str.substring(1, str.length() - 1);
            cClauses.add(new SimpleCondition(clause));
        }

        setClauses(cClauses.toArray(new Clause[cClauses.size()]));
        setNexus(cNexus.toArray(new Nexus[cNexus.size()]));
    }
}
