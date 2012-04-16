package net.java.xades.security.xml.XAdES;

/**
 *
 * @author miro
 */
class SignerImpl
    implements Signer
{
    private String userId;
    private String username;
    private String personName;
    private RoleType roleType;
    
    
    public SignerImpl()
    {
    }

    public String getUserId()
    {
        return userId;
    }

    public void setUserId(String userId)
    {
        this.userId = userId;
    }

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getPersonName()
    {
        return personName;
    }

    public void setPersonName(String personName)
    {
        this.personName = personName;
    }

    public RoleType getRoleType()
    {
        return roleType;
    }

    public void setRoleType(RoleType roleType)
    {
        this.roleType = roleType;
    }

}
