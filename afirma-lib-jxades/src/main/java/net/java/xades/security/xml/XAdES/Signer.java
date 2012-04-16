package net.java.xades.security.xml.XAdES;

/**
 *
 * @author miro
 */
public interface Signer
{
    public String getUserId();
    public void setUserId(String userId);

    public String getUsername();
    public void setUsername(String username);

    public String getPersonName();
    public void setPersonName(String personName);

    public RoleType getRoleType();
    public void setRoleType(RoleType roleType);

}
