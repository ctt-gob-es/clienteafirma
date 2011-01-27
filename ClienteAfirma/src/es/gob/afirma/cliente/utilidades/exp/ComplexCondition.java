/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.cliente.utilidades.exp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;

import es.gob.afirma.exceptions.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
public final class ComplexCondition extends AClause implements ConditionConstants {
	
	/**
	 * @deprecated Usar filtros compatibles RFC2254
	 */
    @Deprecated
	public ComplexCondition(final String str) throws AOException {
        Matcher compoundConditionMatcher = compoundConditionPattern.matcher(str);
        Matcher nexusConditionMatcher = nexusConditionPattern.matcher(str);

        Collection<Clause> cClauses = new ArrayList<Clause>();
        Collection<String> cNexus = new ArrayList<String>();

        int pos = 0;

        if(nexusConditionMatcher.find())
        {
            compoundConditionMatcher.find(0);
            int ini = compoundConditionMatcher.start();
            int fin = compoundConditionMatcher.end();
            String clause = str.substring(ini, fin);
            pos = fin;
            cClauses.add(new CompoundCondition(clause));
            while (pos < str.length() - 1 && pos > -1)
            {
                if (nexusConditionMatcher.find(pos))
                {
                    ini = nexusConditionMatcher.start();
                    fin = nexusConditionMatcher.end();
                    String myNexus = str.substring(ini, fin);
                    pos = fin;
                    cNexus.add(myNexus);
                }
                else
                {
                    throw new AOException("Error de sintaxis, pos=" + pos + ": " + str);
                }
  
                if (compoundConditionMatcher.find(pos))
                {
                    ini = compoundConditionMatcher.start();
                    fin = compoundConditionMatcher.end();
                    clause = str.substring(ini, fin);
                    pos = fin;
                    cClauses.add(new CompoundCondition(clause));
                }
                else
                {
                    throw new AOException("Error de sintaxis, pos=" + pos + ": " + str);
                }
            }
            clauses = cClauses.toArray(new Clause[cClauses.size()]);
            nexus = cNexus.toArray(new Nexus[cNexus.size()]);
        }
        else
        {
            clauses= new Clause[]{new SimpleCondition(str.substring(1, str.length()-1))};
            nexus= new Nexus[0];
        }

    }
}
