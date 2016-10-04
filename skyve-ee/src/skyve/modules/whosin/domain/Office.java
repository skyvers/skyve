package modules.whosin.domain;

import com.vividsolutions.jts.geom.Geometry;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;

/**
 * An official location where staff operate.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Office extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "whosin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Office";

	/** @hidden */
	public static final String levelUnitPropertyName = "levelUnit";
	/** @hidden */
	public static final String buildingNamePropertyName = "buildingName";
	/** @hidden */
	public static final String streetAddressPropertyName = "streetAddress";
	/** @hidden */
	public static final String suburbPropertyName = "suburb";
	/** @hidden */
	public static final String postCodePropertyName = "postCode";
	/** @hidden */
	public static final String phonePropertyName = "phone";
	/** @hidden */
	public static final String boundaryPropertyName = "boundary";
	/** @hidden */
	public static final String demoDataPropertyName = "demoData";

	private String levelUnit;
	private String buildingName;
	private String streetAddress;
	private String suburb;
	private String postCode;
	private String phone;
	/**
	 * The boundary around the office.
	 **/
	private Geometry boundary;
	/**
	 * If this is set, the data was created by the <em>demo data job</em> and can safely be deleted.
	 **/
	private Boolean demoData;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Office.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Office.DOCUMENT_NAME;
	}

	public static Office newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{buildingName} {streetAddress} {suburb}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Office) && 
					this.getBizId().equals(((Office) o).getBizId()));
	}

	/**
	 * {@link #levelUnit} accessor.
	 **/
	public String getLevelUnit() {
		return levelUnit;
	}

	/**
	 * {@link #levelUnit} mutator.
	 * 
	 * @param levelUnit	The new value to set.
	 **/
	@XmlElement
	public void setLevelUnit(String levelUnit) {
		preset(levelUnitPropertyName, levelUnit);
		this.levelUnit = levelUnit;
	}

	/**
	 * {@link #buildingName} accessor.
	 **/
	public String getBuildingName() {
		return buildingName;
	}

	/**
	 * {@link #buildingName} mutator.
	 * 
	 * @param buildingName	The new value to set.
	 **/
	@XmlElement
	public void setBuildingName(String buildingName) {
		preset(buildingNamePropertyName, buildingName);
		this.buildingName = buildingName;
	}

	/**
	 * {@link #streetAddress} accessor.
	 **/
	public String getStreetAddress() {
		return streetAddress;
	}

	/**
	 * {@link #streetAddress} mutator.
	 * 
	 * @param streetAddress	The new value to set.
	 **/
	@XmlElement
	public void setStreetAddress(String streetAddress) {
		preset(streetAddressPropertyName, streetAddress);
		this.streetAddress = streetAddress;
	}

	/**
	 * {@link #suburb} accessor.
	 **/
	public String getSuburb() {
		return suburb;
	}

	/**
	 * {@link #suburb} mutator.
	 * 
	 * @param suburb	The new value to set.
	 **/
	@XmlElement
	public void setSuburb(String suburb) {
		preset(suburbPropertyName, suburb);
		this.suburb = suburb;
	}

	/**
	 * {@link #postCode} accessor.
	 **/
	public String getPostCode() {
		return postCode;
	}

	/**
	 * {@link #postCode} mutator.
	 * 
	 * @param postCode	The new value to set.
	 **/
	@XmlElement
	public void setPostCode(String postCode) {
		preset(postCodePropertyName, postCode);
		this.postCode = postCode;
	}

	/**
	 * {@link #phone} accessor.
	 **/
	public String getPhone() {
		return phone;
	}

	/**
	 * {@link #phone} mutator.
	 * 
	 * @param phone	The new value to set.
	 **/
	@XmlElement
	public void setPhone(String phone) {
		preset(phonePropertyName, phone);
		this.phone = phone;
	}

	/**
	 * {@link #boundary} accessor.
	 **/
	public Geometry getBoundary() {
		return boundary;
	}

	/**
	 * {@link #boundary} mutator.
	 * 
	 * @param boundary	The new value to set.
	 **/
	@XmlJavaTypeAdapter(GeometryMapper.class)
	@XmlElement
	public void setBoundary(Geometry boundary) {
		preset(boundaryPropertyName, boundary);
		this.boundary = boundary;
	}

	/**
	 * {@link #demoData} accessor.
	 **/
	public Boolean getDemoData() {
		return demoData;
	}

	/**
	 * {@link #demoData} mutator.
	 * 
	 * @param demoData	The new value to set.
	 **/
	@XmlElement
	public void setDemoData(Boolean demoData) {
		preset(demoDataPropertyName, demoData);
		this.demoData = demoData;
	}
}
