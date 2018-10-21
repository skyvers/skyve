package org.skyve.metadata.model;

import java.util.List;

import javax.xml.bind.annotation.XmlType;

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

import com.vividsolutions.jts.geom.Geometry;

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
	
	/**
	 * 
	 * @return
	 */
	public String getDisplayName();
	
	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	/**
	 * 
	 * @return
	 */
	public AttributeType getAttributeType();

	/**
	 * Informs the Skyve framework when to include and exclude the attributes.
	 * @return	the usage.
	 */
	public UsageType getUsage();
	
	/**
	 * 
	 * @return
	 */
	public boolean isPersistent();
	
	/**
	 * 
	 * @return
	 */
	public boolean isRequired();
	
	/**
	 * 
	 * @return
	 */
	public DomainType getDomainType();

	/**
	 * 
	 * @return
	 */
	public boolean isDeprecated();
	
	/**
	 * Should mutations to this attribute be tracked and make the object "changed".
	 * 
	 * @return
	 */
	public boolean isTrackChanges();

	/**
	 * Should this attribute be audited.
	 * 
	 * @return
	 */
	public boolean isAudited();

	/**
	 * Should the attribute have the transient java modifier added to its definition.
	 * This affects Java Serialization and thus its inclusion in the conversation, EJB remoting etc. 
	 * 
	 * @return
	 */
	public boolean isTransient();

	/**
	 * 
	 * @return
	 */
	public InputWidget getDefaultInputWidget();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
