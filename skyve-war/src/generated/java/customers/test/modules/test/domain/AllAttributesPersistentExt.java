package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.domain.AllAttributesPersistent;

/**
 * All Persistent
 * <br/>
 * All persistent attributes.
 * 
 * @depend - - - Enum3
 * @navhas n aggregatedCollection 0..n AllAttributesPersistent
 * @navcomposed n composedCollection 0..n AllAttributesPersistent
 * @navcomposed n composedAssociation 0..1 AllAttributesPersistent
 * @navhas n aggregatedAssociation 0..1 AllAttributesPersistent
 * @navcomposed n embeddedAssociation 0..1 AllAttributesEmbedded
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class AllAttributesPersistentExt extends AllAttributesPersistent {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * test
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTest() {
		return (false);
	}

	/**
	 * {@link #isTest} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTest() {
		return (! isTest());
	}
}
