package es.gob.afirma.cliente.utilidades.exp;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface Nexus
{
    public static final Nexus AND = new Nexus()
    {
        public boolean eval(boolean b1, boolean b2)
        {
            return b1 && b2;
        }
    };

    public static final Nexus OR = new Nexus()
    {
        public boolean eval(boolean b1, boolean b2)
        {
            return b1 || b2;
        }
    };

    public boolean eval(boolean b1, boolean b2);
}
