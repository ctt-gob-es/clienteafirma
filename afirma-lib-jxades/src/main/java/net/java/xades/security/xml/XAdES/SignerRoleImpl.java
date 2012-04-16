package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

public class SignerRoleImpl implements SignerRole
{
	private ArrayList<String> claimedRole;
	private ArrayList<String> certifiedRole;

	public SignerRoleImpl()
	{
		this.claimedRole = new ArrayList<String>();
		this.certifiedRole = new ArrayList<String>();
	}

	public ArrayList<String> getCertifiedRole()
	{
		return certifiedRole;
	}

	public void setCertifiedRole(ArrayList<String> certifiedRole)
	{
		this.certifiedRole = certifiedRole;
	}

	public ArrayList<String> getClaimedRole()
	{
		return claimedRole;
	}

	public void setClaimedRole(ArrayList<String> claimedRole)
	{
		this.claimedRole = claimedRole;
	}

	public void addClaimedRole(String role)
	{
		this.claimedRole.add(role);
	}

	public void addCertifiedRole(String role)
	{
		this.certifiedRole.add(role);
	}
}