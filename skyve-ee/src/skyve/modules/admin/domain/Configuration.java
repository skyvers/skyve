package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Setup
 * 
 * @depend - - - PasswordComplexityModel
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Configuration extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Configuration";

	/** @hidden */
	public static final String passwordComplexityModelPropertyName = "passwordComplexityModel";

	/**
	 * Password Complexity
	 * <br/>
	 * The security level/complexity model for user passwords
	 **/
	@XmlEnum
	public static enum PasswordComplexityModel implements Enumeration {
		minimumMin6Chars("MINIMUM", "Minimum - min 6 chars"),
		mediumMin6CharsUpperLowerAndNumeric("MEDIUM", "Medium - min 6 chars, upper, lower and numeric"),
		maximumMin8CharsUpperLowerNumericAndPunctuation("MAXIMUM", "Maximum - min 8 chars, upper, lower, numeric and punctuation");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private PasswordComplexityModel(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static PasswordComplexityModel fromCode(String code) {
			PasswordComplexityModel result = null;

			for (PasswordComplexityModel value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static PasswordComplexityModel fromDescription(String description) {
			PasswordComplexityModel result = null;

			for (PasswordComplexityModel value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				PasswordComplexityModel[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (PasswordComplexityModel value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Password Complexity
	 * <br/>
	 * The security level/complexity model for user passwords
	 **/
	private PasswordComplexityModel passwordComplexityModel;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Configuration.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Configuration.DOCUMENT_NAME;
	}

	public static Configuration newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"Admin Setup",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Configuration) && 
					this.getBizId().equals(((Configuration) o).getBizId()));
	}

	/**
	 * {@link #passwordComplexityModel} accessor.
	 * @return	The value.
	 **/
	public PasswordComplexityModel getPasswordComplexityModel() {
		return passwordComplexityModel;
	}

	/**
	 * {@link #passwordComplexityModel} mutator.
	 * @param passwordComplexityModel	The new value.
	 **/
	@XmlElement
	public void setPasswordComplexityModel(PasswordComplexityModel passwordComplexityModel) {
		preset(passwordComplexityModelPropertyName, passwordComplexityModel);
		this.passwordComplexityModel = passwordComplexityModel;
	}
}
