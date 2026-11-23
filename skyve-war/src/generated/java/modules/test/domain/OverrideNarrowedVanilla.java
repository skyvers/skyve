package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Override Narrowed
 * <br/>
 * Customer override fixture: the test customer's override leaves out dropped and status, and the test2 customer's keeps them and adds a value to status. OverrideEnumReferrer references status.
 * 
 * @depend - - - Status
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideNarrowedVanilla extends OverrideNarrowed {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String droppedPropertyName = "dropped";

	/** @hidden */
	public static final String statusPropertyName = "status";

	/**
	 * Status
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Status implements Enumeration {
		open("open", "open"),
		closed("closed", "closed");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Status::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Status(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.nullSafeI18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Status fromCode(String code) {
			Status result = null;

			for (Status value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Status fromLocalisedDescription(String description) {
			Status result = null;

			for (Status value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Dropped
	 **/
	private String dropped;

	/**
	 * Status
	 **/
	private Status status;

	/**
	 * {@link #dropped} accessor.
	 * @return	The value.
	 **/
	public String getDropped() {
		return dropped;
	}

	/**
	 * {@link #dropped} mutator.
	 * @param dropped	The new value.
	 **/
	@XmlElement
	public void setDropped(String dropped) {
		preset(droppedPropertyName, dropped);
		this.dropped = dropped;
	}

	/**
	 * {@link #status} accessor.
	 * @return	The value.
	 **/
	public Status getStatus() {
		return status;
	}

	/**
	 * {@link #status} mutator.
	 * @param status	The new value.
	 **/
	@XmlElement
	public void setStatus(Status status) {
		preset(statusPropertyName, status);
		this.status = status;
	}
}
