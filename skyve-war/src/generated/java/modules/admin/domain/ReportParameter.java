package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;

/**
 * Report Parameter
 * <br/>
 * <p>Report Parameters are used to define a parameter which is injected into a ReportDataset
			when a report is executed. Report Paramters sit at the template level, so that a single
			parameter can be reused for multiple datasets, e.g. 'dateFrom'.</p>
			<p>The test values are used when testing a report from a Report Template, and when running 
			a report from a Report Configuration, the default value or input values are used.</p>
 * 
 * @depend - - - Type
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ReportParameter extends AbstractPersistentBean implements ChildBean<ReportTemplateExtension>, org.skyve.domain.app.admin.ReportParameter {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ReportParameter";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String descriptionPropertyName = "description";

	/** @hidden */
	public static final String typePropertyName = "type";

	/** @hidden */
	public static final String requiredPropertyName = "required";

	/** @hidden */
	public static final String dateDefaultValuePropertyName = "dateDefaultValue";

	/** @hidden */
	public static final String numericalDefaultValuePropertyName = "numericalDefaultValue";

	/** @hidden */
	public static final String textDefaultValuePropertyName = "textDefaultValue";

	/** @hidden */
	public static final String dateTestValuePropertyName = "dateTestValue";

	/** @hidden */
	public static final String numericalTestValuePropertyName = "numericalTestValue";

	/** @hidden */
	public static final String textTestValuePropertyName = "textTestValue";

	/** @hidden */
	public static final String defaultValueStringPropertyName = "defaultValueString";

	/** @hidden */
	public static final String testValueStringPropertyName = "testValueString";

	/** @hidden */
	public static final String reportInputValuePropertyName = "reportInputValue";

	/** @hidden */
	public static final String formattedInputValuePropertyName = "formattedInputValue";

	/**
	 * Parameter Name
	 * <br/>
	 * The name of this parameter.
	 **/
	private String name;

	/**
	 * Description
	 * <br/>
	 * A description for this parameter which may be presented to the user when 
				they run the report to assist them in providing a value.
	 **/
	private String description;

	/**
	 * Parameter Type
	 * <br/>
	 * The data type of this parameter
	 **/
	private Type type;

	/**
	 * Required
	 * <br/>
	 * Is this parameter required to run the report?
	 **/
	private Boolean required = Boolean.valueOf(false);

	/**
	 * Default Value
	 * <br/>
	 * The default value which will be used if one is not provided
	 **/
	private DateOnly dateDefaultValue;

	/**
	 * Default Value
	 * <br/>
	 * The default value which will be used if one is not provided
	 **/
	private Long numericalDefaultValue;

	/**
	 * Default Value
	 * <br/>
	 * The default value which will be used if one is not provided
	 **/
	private String textDefaultValue;

	/**
	 * Test Value
	 * <br/>
	 * The value to use when testing this report
	 **/
	private DateOnly dateTestValue;

	/**
	 * Test Value
	 * <br/>
	 * The value to use when testing this report
	 **/
	private Long numericalTestValue;

	/**
	 * Test Value
	 * <br/>
	 * The value to use when testing this report
	 **/
	private String textTestValue;

	/**
	 * Default Value
	 * <br/>
	 * Calculated field to show the default value as a String in the ReportTemplate view
	 **/
	private String defaultValueString;

	/**
	 * Test Value
	 * <br/>
	 * Calculated field to show the test value as a String in the ReportTemplate view
	 **/
	private String testValueString;

	/**
	 * Value
	 * <br/>
	 * The parameter value to use for this report
	 * <br/>
	 * This is the input parameter value passed into the report when it being run from the user interface.
	 **/
	private String reportInputValue;

	/**
	 * Formatted Value
	 * <br/>
	 * Calculated field to use in a report to show the formatted value of the parameter 
				which was supplied, or the default value used if one exists and it was used.
	 **/
	private String formattedInputValue;

	private ReportTemplateExtension parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ReportParameter.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ReportParameter.DOCUMENT_NAME;
	}

	public static ReportParameterExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Parameter - {name}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ReportParameter) && 
					this.getBizId().equals(((ReportParameter) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #type} accessor.
	 * @return	The value.
	 **/
	public Type getType() {
		return type;
	}

	/**
	 * {@link #type} mutator.
	 * @param type	The new value.
	 **/
	@XmlElement
	public void setType(Type type) {
		preset(typePropertyName, type);
		this.type = type;
	}

	/**
	 * {@link #required} accessor.
	 * @return	The value.
	 **/
	public Boolean getRequired() {
		return required;
	}

	/**
	 * {@link #required} mutator.
	 * @param required	The new value.
	 **/
	@XmlElement
	public void setRequired(Boolean required) {
		preset(requiredPropertyName, required);
		this.required = required;
	}

	/**
	 * {@link #dateDefaultValue} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDateDefaultValue() {
		return dateDefaultValue;
	}

	/**
	 * {@link #dateDefaultValue} mutator.
	 * @param dateDefaultValue	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDateDefaultValue(DateOnly dateDefaultValue) {
		preset(dateDefaultValuePropertyName, dateDefaultValue);
		this.dateDefaultValue = dateDefaultValue;
	}

	/**
	 * {@link #numericalDefaultValue} accessor.
	 * @return	The value.
	 **/
	public Long getNumericalDefaultValue() {
		return numericalDefaultValue;
	}

	/**
	 * {@link #numericalDefaultValue} mutator.
	 * @param numericalDefaultValue	The new value.
	 **/
	@XmlElement
	public void setNumericalDefaultValue(Long numericalDefaultValue) {
		preset(numericalDefaultValuePropertyName, numericalDefaultValue);
		this.numericalDefaultValue = numericalDefaultValue;
	}

	/**
	 * {@link #textDefaultValue} accessor.
	 * @return	The value.
	 **/
	public String getTextDefaultValue() {
		return textDefaultValue;
	}

	/**
	 * {@link #textDefaultValue} mutator.
	 * @param textDefaultValue	The new value.
	 **/
	@XmlElement
	public void setTextDefaultValue(String textDefaultValue) {
		preset(textDefaultValuePropertyName, textDefaultValue);
		this.textDefaultValue = textDefaultValue;
	}

	/**
	 * {@link #dateTestValue} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDateTestValue() {
		return dateTestValue;
	}

	/**
	 * {@link #dateTestValue} mutator.
	 * @param dateTestValue	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDateTestValue(DateOnly dateTestValue) {
		preset(dateTestValuePropertyName, dateTestValue);
		this.dateTestValue = dateTestValue;
	}

	/**
	 * {@link #numericalTestValue} accessor.
	 * @return	The value.
	 **/
	public Long getNumericalTestValue() {
		return numericalTestValue;
	}

	/**
	 * {@link #numericalTestValue} mutator.
	 * @param numericalTestValue	The new value.
	 **/
	@XmlElement
	public void setNumericalTestValue(Long numericalTestValue) {
		preset(numericalTestValuePropertyName, numericalTestValue);
		this.numericalTestValue = numericalTestValue;
	}

	/**
	 * {@link #textTestValue} accessor.
	 * @return	The value.
	 **/
	public String getTextTestValue() {
		return textTestValue;
	}

	/**
	 * {@link #textTestValue} mutator.
	 * @param textTestValue	The new value.
	 **/
	@XmlElement
	public void setTextTestValue(String textTestValue) {
		preset(textTestValuePropertyName, textTestValue);
		this.textTestValue = textTestValue;
	}

	/**
	 * {@link #defaultValueString} accessor.
	 * @return	The value.
	 **/
	public String getDefaultValueString() {
		return defaultValueString;
	}

	/**
	 * {@link #defaultValueString} mutator.
	 * @param defaultValueString	The new value.
	 **/
	@XmlElement
	public void setDefaultValueString(String defaultValueString) {
		this.defaultValueString = defaultValueString;
	}

	/**
	 * {@link #testValueString} accessor.
	 * @return	The value.
	 **/
	public String getTestValueString() {
		return testValueString;
	}

	/**
	 * {@link #testValueString} mutator.
	 * @param testValueString	The new value.
	 **/
	@XmlElement
	public void setTestValueString(String testValueString) {
		this.testValueString = testValueString;
	}

	/**
	 * {@link #reportInputValue} accessor.
	 * @return	The value.
	 **/
	public String getReportInputValue() {
		return reportInputValue;
	}

	/**
	 * {@link #reportInputValue} mutator.
	 * @param reportInputValue	The new value.
	 **/
	@XmlElement
	public void setReportInputValue(String reportInputValue) {
		preset(reportInputValuePropertyName, reportInputValue);
		this.reportInputValue = reportInputValue;
	}

	/**
	 * {@link #formattedInputValue} accessor.
	 * @return	The value.
	 **/
	public String getFormattedInputValue() {
		return formattedInputValue;
	}

	/**
	 * {@link #formattedInputValue} mutator.
	 * @param formattedInputValue	The new value.
	 **/
	@XmlElement
	public void setFormattedInputValue(String formattedInputValue) {
		preset(formattedInputValuePropertyName, formattedInputValue);
		this.formattedInputValue = formattedInputValue;
	}

	/**
	 * True when the parameter type is date
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isDateValue() {
		return (Type.date == getType());
	}

	/**
	 * {@link #isDateValue} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotDateValue() {
		return (! isDateValue());
	}

	/**
	 * True when the parameter type is number
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isNumericalValue() {
		return (Type.integer == getType() || Type.longInteger == getType());
	}

	/**
	 * {@link #isNumericalValue} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotNumericalValue() {
		return (! isNumericalValue());
	}

	/**
	 * True when the parameter type is text
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTextValue() {
		return (Type.text == getType());
	}

	/**
	 * {@link #isTextValue} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTextValue() {
		return (! isTextValue());
	}

	@Override
	public ReportTemplateExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(ReportTemplateExtension parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
