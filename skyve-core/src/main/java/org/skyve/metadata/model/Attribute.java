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
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Describes a single typed field or relation on a Skyve document.
 *
 * <p>Attributes are the building blocks of the domain model. Every field, scalar value,
 * association, and collection declared in a document XML produces one {@code Attribute}
 * instance. The {@link AttributeType} enum enumerates every supported Skyve type and
 * maps each to its Java implementation class.
 *
 * <p>Attributes carry metadata governing how the framework treats them:
 * <ul>
 *   <li>Persistence ({@link #isPersistent()}) — whether the value is persisted to the database.</li>
 *   <li>Usage ({@link #getUsage()}) — whether the attribute applies to the domain, the view, or both.</li>
 *   <li>Sensitivity ({@link #getSensitivity()}) — data classification for backup redaction.</li>
 *   <li>Change tracking ({@link #isTrackChanges()}) — whether mutations mark the bean as dirty.</li>
 *   <li>Auditing ({@link #isAudited()}) — whether value changes are written to the audit log.</li>
 * </ul>
 *
 * @see Model#getAttributes()
 * @see AttributeType
 */
public interface Attribute extends NamedMetaData, DecoratedMetaData {
	/**
	 * The Skyve type system for document attributes.
	 *
	 * <p>Each constant maps a Skyve type name to the Java class used to represent
	 * values of that type in the domain model. The mapping is available at runtime
	 * via {@link #getImplementingType()} / {@link Attribute#getImplementingType()}.
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
		private AttributeType(@Nonnull Class<?> implementingType) {
			this.implementingType = implementingType;
		}
	}
	
	/**
	 * Governs when the attribute is included in generated or resolved metadata.
	 *
	 * <ul>
	 *   <li>{@code domain} — the attribute is only present in the domain model (not in views).</li>
	 *   <li>{@code view} — the attribute is only present in views (not persisted to domain).</li>
	 *   <li>{@code both} — the attribute is present in both domain and view contexts.</li>
	 * </ul>
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum UsageType {
		domain, view, both
	}
	
	/**
	 * Defines the Sensitivity enumeration.
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum Sensitivity {	
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
		 * information that can identify an individual - whether it is used independently or in tandem
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
	 * Returns the i18n resource key (or literal string) for the human-readable attribute label.
	 *
	 * @return the display name key; may be {@code null}
	 * @see #getLocalisedDisplayName()
	 */
	String getDisplayName();
	
	default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}
	
	/**
	 * Returns the i18n resource key (or literal string) for a tooltip or help description.
	 *
	 * @return the description key; may be {@code null}
	 * @see #getLocalisedDescription()
	 */
	String getDescription();
	
	default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}

	/**
	 * Returns the Skyve type of this attribute.
	 *
	 * @return the attribute type; never {@code null}
	 */
	AttributeType getAttributeType();
	
	/**
	 * This is mapped through the attribute type except for enumerations which have a Java enum generated
	 * @return	the java class that implements the Skyve type.
	 */
	default @Nonnull Class<?> getImplementingType() {
		return getAttributeType().implementingType;
	}

	/**
	 * Returns whether this attribute holds a single value (as opposed to a collection or inverse relation).
	 *
	 * @return {@code true} for scalar types (text, integer, date, association, etc.);
	 *         {@code false} for collection and inverse-many types
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
	Sensitivity getSensitivity();
	
	/**
	 * Returns whether this attribute is persisted to the database.
	 *
	 * <p>Transient attributes (e.g. view-only calculated fields) return {@code false}.
	 *
	 * @return {@code true} if the attribute is stored in the database
	 */
	boolean isPersistent();
	
	/**
	 * Returns whether this attribute must have a non-null, non-empty value before save.
	 *
	 * @return {@code true} if the attribute is mandatory
	 */
	boolean isRequired();
	
	String getRequiredMessage();
	
	default String getLocalisedRequiredMessage() {
		return Util.i18n(getRequiredMessage());
	}
	
	/**
	 * Returns the domain type that governs what values are valid for this attribute.
	 *
	 * @return the domain type, or {@code null} if there are no domain value constraints
	 */
	DomainType getDomainType();

	/**
	 * Returns whether this attribute is marked as deprecated.
	 *
	 * <p>Deprecated attributes are hidden from generated UIs and may be removed in a
	 * future version of the module.
	 *
	 * @return {@code true} if deprecated
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
	 * Returns the default widget to use when this attribute is rendered in a generated view.
	 *
	 * @return the default input widget; may be {@code null} if not configured
	 */
	InputWidget getDefaultInputWidget();
	
	/**
	 * Returns extended documentation for this attribute (HTML or plain text).
	 *
	 * @return the documentation; may be {@code null}
	 */
	String getDocumentation();
}
