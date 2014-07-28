package modules;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Holds the standard format of bizhub addresses.
 * @author mike
 */
public final class Address {
	private String address;
	private String city;
	private String postcode;
	private String state;
	private String country;

	public Address() {
		// nothing to see here
	}

	public Address(String address, String city, String postcode, String state, String country) {
		this.address = address;
		this.city = city;
		this.postcode = postcode;
		this.state = state;
		this.country = country;
	}
	
	public String getAddress() {
		return address;
	}
	public void setAddress(String address) {
		this.address = address;
	}

	public String getCity() {
		return city;
	}
	public void setCity(String city) {
		this.city = city;
	}

	public String getPostcode() {
		return postcode;
	}
	public void setPostcode(String postcode) {
		this.postcode = postcode;
	}

	public String getState() {
		return state;
	}
	public void setState(String state) {
		this.state = state;
	}

	public String getCountry() {
		return (country==null?null:country.toUpperCase());
	}
	public void setCountry(String country) {
		this.country = (country==null?null:country.toUpperCase());
	}
	
	public String getAddressLine1() {
		return getAddress();
	}
	
	public String getAddressLine2() {
		StringBuilder result = new StringBuilder(128);
		
		if (city != null) {
			result.append(city);
		}
		if (state != null) {
			if (result.length() > 0) {
				result.append(' ');
			}
			result.append(state);
		}
		if (postcode != null) {
			if (result.length() > 0) {
				result.append(' ');
			}
			result.append(postcode);
		}

		return result.toString();
	}
	
	public String getAddressLine3() {
		return getCountry();
	}
	
	/**
	 * returns a compiled address string for an address
	 * 
	 * @param c
	 * @return
	 */
	public String getCompiledAddress(){
		StringBuilder cA = new StringBuilder();
		cA.append(ModulesUtil.coalesce(address,"").trim());
		cA.append("\n").append(ModulesUtil.coalesce(city,"").toUpperCase());
		cA.append(" ").append(ModulesUtil.coalesce(state,"").toUpperCase());
		if(!"AUSTRALIA".equals(ModulesUtil.coalesce(country,"").toUpperCase())){
			cA.append(", ").append(ModulesUtil.coalesce(country,""));
		}
		cA.append(" ").append(ModulesUtil.coalesce(postcode,"").toUpperCase());

		return cA.toString();
	}
	
	@Override
	public String toString(){
		return ModulesUtil.concatWithDelim(" ",address, city, state, postcode, country);
	}
	
	/**
	 * Gets location of an address
	 * 
	 * @param address
	 * @return
	 * @throws Exception
	 */
	public Point getLocationPoint()
	throws Exception {
		Point result = null;
		StringBuilder sb = new StringBuilder(128);
		sb.append("http://maps.google.com/maps/api/geocode/xml?address=").append(this.toString()).append("&sensor=false"); 
		String url = sb.toString().replaceAll("\\s"," "); // replace all sorts of whitespace (eg '\n') with a space
		
		DocumentBuilderFactory df = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = df.newDocumentBuilder();
		Document dom = db.parse(url);
		Element document = dom.getDocumentElement();
		NodeList resultList = document.getElementsByTagName("result");
		if ((resultList != null) && (resultList.getLength() > 0)) {
			Element firstResult = (Element) resultList.item(0);
			NodeList latList = firstResult.getElementsByTagName("lat");
			if ((latList != null) && (latList.getLength() > 0)) {
				NodeList lngList = firstResult.getElementsByTagName("lng");
				if ((lngList != null) && (lngList.getLength() > 0)) {
					double lat = Double.parseDouble(latList.item(0).getTextContent());
					double lng = Double.parseDouble(lngList.item(0).getTextContent());
					GeometryFactory gf = new GeometryFactory();
					result = gf.createPoint(new Coordinate(lng, lat));
				}
			}
		}
		
		return result;
	}
	
}