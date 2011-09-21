package es.gob.afirma.keystores.filters.old;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.core.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
final class SimpleCondition extends AClause implements ConditionConstants {
	
    private String field;
    private Operator operator;
    private String value;
    private Object aux = null;
    private String hashAlg;

    SimpleCondition(final String str) throws AOException {
        Matcher fieldMatcher = fieldConditionPattern.matcher(str);
        Matcher valueMatcher = valueConditionPattern.matcher(str);
        Matcher operatorMatcher = operatorPattern.matcher(str);

        int ini, fin;
        if (fieldMatcher.find()) {
            ini = fieldMatcher.start();
            fin = fieldMatcher.end();
            this.field = str.substring(ini, fin);
        }
        else throw new AOException("Error de sintaxis: " + str); //$NON-NLS-1$

        if (operatorMatcher.find()) {
            ini = operatorMatcher.start();
            fin = operatorMatcher.end();
            String strOperator = str.substring(ini, fin);
            
            if (strOperator.equals("="))				 this.operator = Operator.EQ; //$NON-NLS-1$
	        else if (strOperator.equals("#MATCHES#"))	 this.operator = Operator.MATCHES; //$NON-NLS-1$
            else if (strOperator.equals("#NOT_MATCHES#"))this.operator = Operator.NOT_MATCHES; //$NON-NLS-1$
            else throw new AOException("Operador desconocido: " + str); //$NON-NLS-1$
        }
        else throw new AOException("Error de sintaxis: " + str); //$NON-NLS-1$

        if (valueMatcher.find()) {
            ini = valueMatcher.start();
            fin = valueMatcher.end();
            this.value = str.substring(ini + 2, fin - 2);
        }
        else throw new AOException("Error de sintaxis: " + str); //$NON-NLS-1$
    }

    @Override
	public boolean eval(X509Certificate cert) throws AOException {
    	if (cert==null) return false;
        try {
            Object o1=null, o2;
            // Ver java.util.Pattern para lo de "?i"
            //if (aux == null && (operator == Operator.MATCHES || operator == Operator.NOT_MATCHES)) aux = "(?i)" + value; 

            if (this.field.equalsIgnoreCase("ISSUER.DN")) { //$NON-NLS-1$
                this.aux = this.value;
                o1 = rewriteLdapName(cert.getIssuerDN().getName());
            }
            else if (this.field.equalsIgnoreCase("SUBJECT.SERIALNUMBER")) { //$NON-NLS-1$
            	if (this.aux == null) this.aux = this.value;
            	String principal = cert.getSubjectX500Principal().getName();
            	List<Rdn> rdns = new LdapName(principal).getRdns();
        		if (rdns != null && (!rdns.isEmpty())) {
        			for (int j=0; j<rdns.size(); j++) {
        				if (rdns.get(j).toString().startsWith("serialnumber=") || rdns.get(j).toString().startsWith("SERIALNUMBER=")) { //$NON-NLS-1$ //$NON-NLS-2$
        					o1 = asciiHexToString(rdns.get(j).toString().substring(13));
        				}
        				else if (rdns.get(j).toString().startsWith("2.5.4.5=")) { //$NON-NLS-1$
        				    o1 = asciiHexToString(rdns.get(j).toString().substring(8));
        				}
        			}
        			if (o1==null) return false;
        		} 
        		else return false;
            }
            else if (this.field.equalsIgnoreCase("SUBJECT.DN")) { //$NON-NLS-1$
                this.aux = this.value;
                o1 = rewriteLdapName(cert.getSubjectDN().getName());
            }
            else if (this.field.equalsIgnoreCase("SUBJECT.SN")) { //$NON-NLS-1$
                if (this.aux == null) this.aux = new BigInteger(this.value).toString();
                o1 = cert.getSerialNumber().toString();
            }
            else if (this.field.toUpperCase().startsWith("SUBJECT.FP(")) { //$NON-NLS-1$
                if (this.hashAlg == null) {
                    Matcher matcher = hashAlgPattern.matcher(this.field);
                    matcher.find();
                    this.hashAlg = this.field.substring(matcher.start(), matcher.end());
                }
                if (this.aux == null) this.aux = this.value;
                o1 = HEX_HELPER.toString(MessageDigest.getInstance(this.hashAlg).digest(cert.getEncoded()));
            }
            else throw new AOException("Campo desconocido:" + this.field); //$NON-NLS-1$
            o2 = this.aux;
            
            Logger.getLogger("es.gob.afirma").info("\nValor extraido del certificado: " + o1+"\nValor patron del filtro: " + o2); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            
            return this.operator.eval(o1, o2);
        }
        catch (final Exception e) {
        	throw new AOException("Error evaluando la expresion", e); //$NON-NLS-1$
        }
    } 			

    private String asciiHexToString(String ah) {
        if(!ah.startsWith("#")) { //$NON-NLS-1$
            return ah;
        }
        
        ah = ah.replaceAll("#", ""); //$NON-NLS-1$ //$NON-NLS-2$
        String tmpStr;
        StringBuilder outStr = new StringBuilder();
        for (int i=0;i<ah.length()/2;i++) {
            tmpStr = ah.substring(i*2, i*2+2);
            tmpStr = tmpStr.trim();
            
            if      (tmpStr.equalsIgnoreCase("30")) outStr.append("0"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("31")) outStr.append("1"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("32")) outStr.append("2"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("33")) outStr.append("3"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("34")) outStr.append("4"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("35")) outStr.append("5"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("36")) outStr.append("6"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("37")) outStr.append("7"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("38")) outStr.append("8"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("39")) outStr.append("9"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("41")) outStr.append("A"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("42")) outStr.append("B"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("43")) outStr.append("C"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("44")) outStr.append("D"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("45")) outStr.append("E"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("46")) outStr.append("F"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("47")) outStr.append("G"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("48")) outStr.append("H"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("49")) outStr.append("I"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4a")) outStr.append("J"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4b")) outStr.append("K"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4c")) outStr.append("L"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4d")) outStr.append("M"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4e")) outStr.append("N"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("4f")) outStr.append("O"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("50")) outStr.append("P"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("51")) outStr.append("Q"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("52")) outStr.append("R"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("53")) outStr.append("S"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("54")) outStr.append("T"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("55")) outStr.append("U"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("56")) outStr.append("V"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("57")) outStr.append("W"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("58")) outStr.append("X"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("59")) outStr.append("Y"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("5a")) outStr.append("Z"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("61")) outStr.append("a");  //$NON-NLS-1$//$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("62")) outStr.append("b"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("63")) outStr.append("c"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("64")) outStr.append("d"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("65")) outStr.append("e"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("66")) outStr.append("f"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("67")) outStr.append("g"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("68")) outStr.append("h"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("69")) outStr.append("i"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6a")) outStr.append("j"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6b")) outStr.append("k"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6c")) outStr.append("l"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6d")) outStr.append("m"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6e")) outStr.append("n"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("6f")) outStr.append("o"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("70")) outStr.append("p"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("71")) outStr.append("q"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("72")) outStr.append("r"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("73")) outStr.append("s"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("74")) outStr.append("t"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("75")) outStr.append("u"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("76")) outStr.append("v"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("77")) outStr.append("w"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("78")) outStr.append("x"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("79")) outStr.append("y"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("7a")) outStr.append("z"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("2d")) outStr.append("-"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("5f")) outStr.append("_"); //$NON-NLS-1$ //$NON-NLS-2$
            else if (tmpStr.equalsIgnoreCase("20")) outStr.append(" "); //$NON-NLS-1$ //$NON-NLS-2$
        }
        
        return outStr.toString();
    }
    
    private String rewriteLdapName(String ln) {
        try {
            List<Rdn> rdns = new LdapName(ln).getRdns();
            StringBuilder out = new StringBuilder();
            if (rdns != null && (!rdns.isEmpty())) {
                for (int j=rdns.size()-1; j>=0; j--) {
                    String keyValue = rdns.get(j).toString();
                    if(keyValue.startsWith("2.5.4.5=") || keyValue.startsWith("serialnumber=") || keyValue.startsWith("SERIALNUMBER=")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        out.append("SERIALNUMBER="); //$NON-NLS-1$
                        out.append(asciiHexToString(keyValue.substring(keyValue.indexOf('=')+1)));
                        out.append(","); //$NON-NLS-1$
                    } else {
                        out.append(keyValue);     
                        out.append(","); //$NON-NLS-1$
                    }
                }
                String outStr = out.toString();
                if (outStr.endsWith(",")) outStr = outStr.substring(0,outStr.length()-1); //$NON-NLS-1$
                return outStr;
            }
            return ln;
        }
        catch(Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha tratado el nombre X400 antes de aplicar un filtro: " + ln); //$NON-NLS-1$ //$NON-NLS-2$
            Logger.getLogger("es.gob.afirma").warning("Excepcion: "+e); //$NON-NLS-1$ //$NON-NLS-2$
            return ln;
        }
        
    }
    
}
