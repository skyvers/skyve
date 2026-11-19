package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.OverrideNarrowedExtended.OverrideNarrowedExtendedExtension;

/**
 * Override Narrowed Extended
 * <br/>
 * The test customer's override of OverrideNarrowedExtended, leaving out dropped.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideNarrowedExtendedOverride extends OverrideNarrowedExtendedExtension {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

}
