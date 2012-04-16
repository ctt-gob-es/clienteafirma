package net.java.xades.security.xml.XAdES;

public class SignatureProductionPlaceImpl implements SignatureProductionPlace
{
	private String city;
	private String stateOrProvince;
	private String postalCode;
	private String countryName;

	public SignatureProductionPlaceImpl() 
	{
	}

	public SignatureProductionPlaceImpl(String city, String stateOrProvince, String postalCode, String countryName) 
	{
		this.city = city;
		this.stateOrProvince = stateOrProvince;
		this.postalCode = postalCode;
		this.countryName = countryName;
	}
	
	public String getCity() 
	{
		return city;
	}

	public void setCity(String city) 
	{
		this.city = city;
	}

	public String getStateOrProvince() 
	{
		return stateOrProvince;
	}

	public void setStateOrProvince(String stateOrProvince) 
	{
		this.stateOrProvince = stateOrProvince;
	}

	public String getPostalCode() 
	{
		return postalCode;
	}

	public void setPostalCode(String postalCode) 
	{
		this.postalCode = postalCode;
	}

	public String getCountryName() 
	{
		return countryName;
	}

	public void setCountryName(String countryName) 
	{
		this.countryName = countryName;
	}
}