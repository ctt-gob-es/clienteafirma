package net.java.xades.security.xml;

/**
 *
 * @author miro
 */
public enum ValidateResult
{
    VALID("Valid"),
    INCOMPLETE("Incomplete validation"),
    INVALID("Invalid");

    private ValidateResult(String resultName)
    {
        this.resultName = resultName;
    }

    public String getResultName()
    {
        return resultName;
    }

    public String toString()
    {
        return getResultName();
    }

    private String resultName;
}
