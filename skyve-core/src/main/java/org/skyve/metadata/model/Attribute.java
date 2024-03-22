package org.skyve.metadata.model;

import java.util.List;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
public interface Attribute extends NamedMetaData {
	/**
	 * The Skyve type of the attribute.
	 * This also encapsulates the implementation type.
	 */
	@XmlType
	public enum AttributeType {
		text(String.class), 
		date(DateOnly.class), 
		time(TimeOnly.class), 
		dateTime(DateTime.class), 
		timestamp(Timestamp.class), 
		integer(Integer.class), 
		longInteger(Long.class), 
		decimal2(Decimal2.class), 
		decimal5(Decimal5.class), 
		decimal10(Decimal10.class), 
		bool(Boolean.class),
		enumeration(Enum.class),
		memo(String.class), 
		markup(String.class),
		colour(String.class), 
		content(String.class), 
		image(String.class),
		association(Bean.class), 
		collection(List.class),
		inverseOne(Bean.class),
		inverseMany(List.class),
		geometry(Geometry.class),
		id(String.class);

		private Class<?> implementingType;

		/**
		 * @param implementingType	The java class that implements the Skyve type.
		 */
		private AttributeType(Class<?> implementingType) {
			this.implementingType = implementingType;
		}

		/**
		 * @return	the java class that implements the Skyve type.
		 */
		public Class<?> getImplementingType() {
			return implementingType;
		}
	}
	
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum UsageType {
		domain, view, both
	}
	
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum SensitivityType {	
		/**
		 * Data is freely available and does not require any special security measures. 
		 * This data can be openly shared with anyone without the need for additional precautions.
		 */
		none,
		
		/**
		 * Internal data is only intended for use within an organisation, and can include things
		 * like the employee handbook, company policies, and certain company-wide communications.
		 * Though it should remain private, if this type of information were to be public, the
		 * repercussions would be minimal.
		 */
		internal, 
		
		/**
		 * Confidential data must be kept within the organisation and should only be accessed by
		 * authorised personnel. It can include information like pricing details, promotional
		 * materials, or contact information. If this type of data were to be disclosed, it could
		 * damage the company or brand.
		 */
		confidential,
		
		/**
		 * Restricted data requires the highest level of protection and access must be limited to
		 * necessary personnel. Often protected by a Non-Disclosure-Agreement (NDA), restricted
		 * data can include trade secrets and medical records - which is especially important in
		 * the context of privacy.
		 */
		restricted, 
		
		/**
		 * `Personally Identifiable Information' (PII) means any information relating to an identified
		 * or identifiable natural person. Under this definition, personal data includes phone number,
		 * physical address, driver's license number, license plate number, social security number,
		 * IP address, bank account, location data, utility records, work hours/performance, biometric
		 * data. Personal data can include information like someone's name (a direct identifier) or
		 * physical characteristics (an indirect identifier). Ultimately, personal data is any
		 * information that can identify an individual - whether it is used independantly or in tandem
		 * with other data.
		 */
		personal,
		
		/**
		 * Applies when compromise might reasonably cause serious injury e.g., passwords, credit card
		 * details.
		 */
		secret
	}
	
	/**
	 * 
	 * @return
	 */
	String getDisplayName();
	
	default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}
	
	/**
	 * 
	 * @return
	 */
	String getDescription();
	
	default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}

	/**
	 * 
	 * @return
	 */
	AttributeType getAttributeType();
	
	/**
	 * Fields are scalar (a single value), Relations are not.
	 * @return	whether scalar.
	 */
	boolean isScalar();

	/**
	 * Informs the Skyve framework when to include and exclude the attributes.
	 * @return	the usage.
	 */
	UsageType getUsage();
	
	/**
	 * Informs the Skyve framework when to redact information when performing backups.
	 * @return	the data sensitivity.
	 */
	SensitivityType getSensitivity();
	
	/**
	 * 
	 * @return
	 */
	boolean isPersistent();
	
	/**
	 * 
	 * @return
	 */
	boolean isRequired();
	
	/**
	 * 
	 * @return
	 */
	DomainType getDomainType();

	/**
	 * 
	 * @return
	 */
	boolean isDeprecated();
	
	/**
	 * Should mutations to this attribute be tracked and make the object "changed".
	 * 
	 * @return
	 */
	boolean isTrackChanges();

	/**
	 * Should this attribute be audited.
	 * 
	 * @return
	 */
	boolean isAudited();

	/**
	 * Should the attribute have the transient java modifier added to its definition.
	 * This affects Java Serialization and thus its inclusion in the conversation, EJB remoting etc. 
	 * 
	 * @return
	 */
	boolean isTransient();

	/**
	 * 
	 * @return
	 */
	InputWidget getDefaultInputWidget();
	
	/**
	 * 
	 * @return
	 */
	String getDocumentation();
}
