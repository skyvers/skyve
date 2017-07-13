package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"latitudeBinding", 
							"longitudeBinding", 
							"descriptionBinding", 
							"addressBinding",
							"cityBinding",
							"stateBinding",
							"postcodeBinding",
							"countryBinding",
							"properties"})
public class GeoLocator implements MetaData, Disableable, Invisible, FormItemWidget {
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

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
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

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
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

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
