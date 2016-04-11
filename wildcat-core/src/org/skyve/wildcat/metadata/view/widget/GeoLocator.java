package org.skyve.wildcat.metadata.view.widget;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"latitudeBinding", 
							"longitudeBinding", 
							"descriptionBinding", 
							"addressBinding",
							"cityBinding",
							"stateBinding",
							"postcodeBinding",
							"countryBinding"})
public class GeoLocator implements MetaData, Disableable, Invisible {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7302870279072305100L;

	private String latitudeBinding;
	private String longitudeBinding;
	private String descriptionBinding;
	private String addressBinding;
	private String cityBinding;
	private String stateBinding;
	private String postcodeBinding;
	private String countryBinding;
	private String disabledConditionName;
	private String invisibleConditionName;

	public String getLatitudeBinding() {
		return latitudeBinding;
	}
	@XmlAttribute(required = false)
	public void setLatitudeBinding(String latitudeBinding) {
		this.latitudeBinding = latitudeBinding;
	}

	public String getLongitudeBinding() {
		return longitudeBinding;
	}
	@XmlAttribute(required = false)
	public void setLongitudeBinding(String longitudeBinding) {
		this.longitudeBinding = longitudeBinding;
	}

	public String getDescriptionBinding() {
		return descriptionBinding;
	}
	@XmlAttribute(required = false)
	public void setDescriptionBinding(String descriptionBinding) {
		this.descriptionBinding = descriptionBinding;
	}

	public String getAddressBinding() {
		return addressBinding;
	}
	@XmlAttribute(required = false)
	public void setAddressBinding(String addressBinding) {
		this.addressBinding = addressBinding;
	}

	public String getCityBinding() {
		return cityBinding;
	}
	@XmlAttribute(required = false)
	public void setCityBinding(String cityBinding) {
		this.cityBinding = cityBinding;
	}

	public String getStateBinding() {
		return stateBinding;
	}
	@XmlAttribute(required = false)
	public void setStateBinding(String stateBinding) {
		this.stateBinding = stateBinding;
	}

	public String getPostcodeBinding() {
		return postcodeBinding;
	}
	@XmlAttribute(required = false)
	public void setPostcodeBinding(String postcodeBinding) {
		this.postcodeBinding = postcodeBinding;
	}

	public String getCountryBinding() {
		return countryBinding;
	}
	@XmlAttribute(required = false)
	public void setCountryBinding(String countryBinding) {
		this.countryBinding = countryBinding;
	}

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
