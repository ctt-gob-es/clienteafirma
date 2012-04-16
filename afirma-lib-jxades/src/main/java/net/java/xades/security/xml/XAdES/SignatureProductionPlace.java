package net.java.xades.security.xml.XAdES;

public interface SignatureProductionPlace 
{
	public void setCity(String city);
	public void setStateOrProvince(String stateOrProvince);
	public void setPostalCode(String postalCode);
	public void setCountryName(String country);
	
	public String getCity();
	public String getStateOrProvince();
	public String getPostalCode();
	public String getCountryName();
}
